/* global exports:true, module:true, require:true, define:true, global:true */

((root, name, factory) => {
	// Used to determine if values are of the language type `Object`
	const objectTypes = {
		function: true,
		object: true,
	};
	// Detect free variable `exports`
	const freeExports =
		objectTypes[typeof exports] && exports && !exports.nodeType && exports;
	// Detect free variable `module`
	const freeModule =
		objectTypes[typeof module] && module && !module.nodeType && module;
	// Detect free variable `global`, from Node.js or Browserified code, and
	// use it as `window`
	const freeGlobal =
		freeExports && freeModule && typeof global === "object" && global;
	// Detect the popular CommonJS extension `module.exports`
	const moduleExports =
		freeModule && freeModule.exports === freeExports && freeExports;

	/* istanbul ignore else */
	if (
		freeGlobal &&
		(freeGlobal.global === freeGlobal ||
			/* istanbul ignore next */ freeGlobal.window === freeGlobal ||
			/* istanbul ignore next */ freeGlobal.self === freeGlobal)
	) {
		root = freeGlobal;
	}

	// Some AMD build optimizers, like r.js, check for specific condition
	// patterns like the following:
	/* istanbul ignore if */
	if (
		typeof define === "function" &&
		/* istanbul ignore next */ typeof define.amd === "object" &&
		/* istanbul ignore next */ define.amd
	) {
		// defined as an anonymous module.
		define(["exports"], factory);
		// In case the source has been processed and wrapped in a define module use
		// the supplied `exports` object.
		if (freeExports && moduleExports) factory(freeModule.exports);
	}
	// check for `exports` after `define` in case a build optimizer adds an
	// `exports` object
	/* istanbul ignore else */
	else if (freeExports && freeModule) {
		// in Node.js or RingoJS v0.8.0+
		/* istanbul ignore else */
		if (moduleExports) factory(freeModule.exports);
		// in RingoJS v0.7.0-
		else factory(freeExports);
	}
	// in a browser or Rhino
	else {
		// biome-ignore lint/suspicious/noAssignInExpressions: <explanation>
		factory((root[name] = {}));
	}
})(this, "luaparse", (exports) => {
	exports.version = "0.3.1";

	let input;
	let options;
	let length;
	let features;
	let encodingMode;

	// Options can be set either globally on the parser object through
	// defaultOptions, or during the parse call.
	// biome-ignore lint/suspicious/noAssignInExpressions: <explanation>
	const defaultOptions = (exports.defaultOptions = {
		// Explicitly tell the parser when the input ends.
		wait: false,
		// Store comments as an array in the chunk object.
		comments: true,
		// Track identifier scopes by adding an isLocal attribute to each
		// identifier-node.
		scope: false,
		// Store location information on each syntax node as
		// `loc: { start: { line, column }, end: { line, column } }`.
		locations: false,
		// Store the start and end character locations on each syntax node as
		// `range: [start, end]`.
		ranges: false,
		// A callback which will be invoked when a syntax node has been completed.
		// The node which has been created will be passed as the only parameter.
		onCreateNode: null,
		// A callback which will be invoked when a new scope is created.
		onCreateScope: null,
		// A callback which will be invoked when the current scope is destroyed.
		onDestroyScope: null,
		// A callback which will be invoked when a local variable is declared in the current scope.
		// The variable's name will be passed as the only parameter
		onLocalDeclaration: null,
		// The version of Lua targeted by the parser (string; allowed values are
		// '5.1', '5.2', '5.3').
		luaVersion: "5.1",
		// Encoding mode: how to interpret code units higher than U+007F in input
		encodingMode: "none",
	});

	function encodeUTF8(codepoint, highMask) {
		const _highMask = highMask || 0;

		if (codepoint < 0x80) {
			return String.fromCharCode(codepoint);
		}
		if (codepoint < 0x800) {
			return String.fromCharCode(
				_highMask | 0xc0 | (codepoint >> 6),
				_highMask | 0x80 | (codepoint & 0x3f),
			);
		}
		if (codepoint < 0x10000) {
			return String.fromCharCode(
				_highMask | 0xe0 | (codepoint >> 12),
				_highMask | 0x80 | ((codepoint >> 6) & 0x3f),
				_highMask | 0x80 | (codepoint & 0x3f),
			);
		}
		if (codepoint < 0x200000) {
			return String.fromCharCode(
				_highMask | 0xf0 | (codepoint >> 18),
				_highMask | 0x80 | ((codepoint >> 12) & 0x3f),
				_highMask | 0x80 | ((codepoint >> 6) & 0x3f),
				_highMask | 0x80 | (codepoint & 0x3f),
			);
		}
		if (codepoint < 0x4000000) {
			return String.fromCharCode(
				_highMask | 0xf8 | (codepoint >> 24),
				_highMask | 0x80 | ((codepoint >> 18) & 0x3f),
				_highMask | 0x80 | ((codepoint >> 12) & 0x3f),
				_highMask | 0x80 | ((codepoint >> 6) & 0x3f),
				_highMask | 0x80 | (codepoint & 0x3f),
			);
		}
		if (codepoint <= 0x7fffffff) {
			return String.fromCharCode(
				_highMask | 0xfc | (codepoint >> 30),
				_highMask | 0x80 | ((codepoint >> 24) & 0x3f),
				_highMask | 0x80 | ((codepoint >> 18) & 0x3f),
				_highMask | 0x80 | ((codepoint >> 12) & 0x3f),
				_highMask | 0x80 | ((codepoint >> 6) & 0x3f),
				_highMask | 0x80 | (codepoint & 0x3f),
			);
		}
		throw new Error("Should not happen");
	}

	function toHex(num, digits) {
		let result = num.toString(16);
		while (result.length < digits) result = `0${result}`;
		return result;
	}

	function checkChars(rx) {
		return (s) => {
			const m = rx.exec(s);
			if (!m) return s;
			raise(
				null,
				errors.invalidCodeUnit,
				toHex(m[0].charCodeAt(0), 4).toUpperCase(),
			);
		};
	}

	const encodingModes = {
		// `pseudo-latin1` encoding mode: assume the input was decoded with the latin1 encoding
		// WARNING: latin1 does **NOT** mean cp1252 here like in the bone-headed WHATWG standard;
		// it means true ISO/IEC 8859-1 identity-mapped to Basic Latin and Latin-1 Supplement blocks
		"pseudo-latin1": {
			// biome-ignore lint/suspicious/noControlCharactersInRegex: <explanation>
			fixup: checkChars(/[^\x00-\xff]/),
			encodeByte: (value) => {
				if (value === null) return "";
				return String.fromCharCode(value);
			},
			encodeUTF8: (codepoint) => encodeUTF8(codepoint),
		},
		"utf-8": {
			fixup: (s) => s,
			encodeByte: (value) => {
				if (value === null) return "";
				return String.fromCharCode(value);
			},
			encodeUTF8: (codepoint) => encodeUTF8(codepoint),
		},
		// `x-user-defined` encoding mode: assume the input was decoded with the WHATWG `x-user-defined` encoding
		"x-user-defined": {
			// biome-ignore lint/suspicious/noControlCharactersInRegex: <explanation>
			fixup: checkChars(/[^\x00-\x7f\uf780-\uf7ff]/),
			encodeByte: (value) => {
				if (value === null) return "";
				if (value >= 0x80) return String.fromCharCode(value | 0xf700);
				return String.fromCharCode(value);
			},
			encodeUTF8: (codepoint) => encodeUTF8(codepoint, 0xf700),
		},

		// `none` encoding mode: disregard intrepretation of string literals, leave identifiers as-is
		none: {
			discardStrings: true,
			fixup: (s) => s,
			encodeByte: (value) => "",
			encodeUTF8: (codepoint) => "",
		},
	};

	// The available tokens expressed as enum flags so they can be checked with
	// bitwise operations.

	const EOF = 1;
	const StringLiteral = 2;
	const Keyword = 4;
	const Identifier = 8;
	const NumericLiteral = 16;
	const Punctuator = 32;
	const BooleanLiteral = 64;
	const NilLiteral = 128;
	const VarargLiteral = 256;

	exports.tokenTypes = {
		EOF: EOF,
		StringLiteral: StringLiteral,
		Keyword: Keyword,
		Identifier: Identifier,
		NumericLiteral: NumericLiteral,
		Punctuator: Punctuator,
		BooleanLiteral: BooleanLiteral,
		NilLiteral: NilLiteral,
		VarargLiteral: VarargLiteral,
	};

	// As this parser is a bit different from luas own, the error messages
	// will be different in some situations.

	// biome-ignore lint/suspicious/noAssignInExpressions: <explanation>
	const errors = (exports.errors = {
		unexpected: "unexpected %1 '%2' near '%3'",
		unexpectedEOF: "unexpected symbol near '<eof>'",
		expected: "'%1' expected near '%2'",
		expectedToken: "%1 expected near '%2'",
		unfinishedString: "unfinished string near '%1'",
		malformedNumber: "malformed number near '%1'",
		decimalEscapeTooLarge: "decimal escape too large near '%1'",
		invalidEscape: "invalid escape sequence near '%1'",
		hexadecimalDigitExpected: "hexadecimal digit expected near '%1'",
		braceExpected: "missing '%1' near '%2'",
		tooLargeCodepoint: "UTF-8 value too large near '%1'",
		unfinishedLongString:
			"unfinished long string (starting at line %1) near '%2'",
		unfinishedLongComment:
			"unfinished long comment (starting at line %1) near '%2'",
		ambiguousSyntax:
			"ambiguous syntax (function call x new statement) near '%1'",
		noLoopToBreak: "no loop to break near '%1'",
		labelAlreadyDefined: "label '%1' already defined on line %2",
		labelNotVisible: "no visible label '%1' for <goto>",
		gotoJumpInLocalScope: "<goto %1> jumps into the scope of local '%2'",
		cannotUseVararg: "cannot use '...' outside a vararg function near '%1'",
		invalidCodeUnit:
			"code unit U+%1 is not allowed in the current encoding mode",
	});

	// ### Abstract Syntax Tree
	//
	// The default AST structure is inspired by the Mozilla Parser API but can
	// easily be customized by overriding these functions.

	// biome-ignore lint/suspicious/noAssignInExpressions: <explanation>
	const ast = (exports.ast = {
		labelStatement: (label) => ({
			type: "LabelStatement",
			label: label,
		}),
		breakStatement: () => ({
			type: "BreakStatement",
		}),
		gotoStatement: (label) => ({
			type: "GotoStatement",
			label: label,
		}),
		returnStatement: (args) => ({
			type: "ReturnStatement",
			arguments: args,
		}),
		ifStatement: (clauses) => ({
			type: "IfStatement",
			clauses: clauses,
		}),
		ifClause: (condition, body) => ({
			type: "IfClause",
			condition: condition,
			body: body,
		}),
		elseifClause: (condition, body) => ({
			type: "ElseifClause",
			condition: condition,
			body: body,
		}),
		elseClause: (body) => ({
			type: "ElseClause",
			body: body,
		}),
		whileStatement: (condition, body) => ({
			type: "WhileStatement",
			condition: condition,
			body: body,
		}),
		doStatement: (body) => ({
			type: "DoStatement",
			body: body,
		}),
		repeatStatement: (condition, body) => ({
			type: "RepeatStatement",
			condition: condition,
			body: body,
		}),
		localStatement: (variables, init) => ({
			type: "LocalStatement",
			variables: variables,
			init: init,
		}),
		assignmentStatement: (variables, init) => ({
			type: "AssignmentStatement",
			variables: variables,
			init: init,
		}),
		compoundAssignmentStatement: (variables, init, operator) => ({
			type: "CompoundAssignmentStatement",
			operator: operator,
			variables: variables,
			init: init,
		}),
		callStatement: (expression) => ({
			type: "CallStatement",
			expression: expression,
		}),
		functionStatement: (identifier, parameters, isLocal, body) => ({
			type: "FunctionDeclaration",
			identifier: identifier,
			isLocal: isLocal,
			parameters: parameters,
			body: body,
		}),
		forNumericStatement: (variable, start, end, step, body) => ({
			type: "ForNumericStatement",
			variable: variable,
			start: start,
			end: end,
			step: step,
			body: body,
		}),
		forGenericStatement: (variables, iterators, body) => ({
			type: "ForGenericStatement",
			variables: variables,
			iterators: iterators,
			body: body,
		}),
		chunk: (body) => ({
			type: "Chunk",
			body: body,
		}),
		identifier: (name) => ({
			type: "Identifier",
			name: name,
		}),
		literal: (type, value, raw) => {
			const t =
				type === StringLiteral
					? "StringLiteral"
					: type === NumericLiteral
						? "NumericLiteral"
						: type === BooleanLiteral
							? "BooleanLiteral"
							: type === NilLiteral
								? "NilLiteral"
								: "VarargLiteral";

			return {
				type: t,
				value: value,
				raw: raw,
			};
		},
		tableKey: (key, value) => ({
			type: "TableKey",
			key: key,
			value: value,
		}),
		tableKeyString: (key, value) => ({
			type: "TableKeyString",
			key: key,
			value: value,
		}),
		tableValue: (value) => ({
			type: "TableValue",
			value: value,
		}),
		tableConstructorExpression: (fields) => ({
			type: "TableConstructorExpression",
			fields: fields,
		}),
		binaryExpression: (operator, left, right) => {
			const type =
				"and" === operator || "or" === operator
					? "LogicalExpression"
					: "BinaryExpression";

			return {
				type: type,
				operator: operator,
				left: left,
				right: right,
			};
		},
		unaryExpression: (operator, argument) => ({
			type: "UnaryExpression",
			operator: operator,
			argument: argument,
		}),
		memberExpression: (base, indexer, identifier) => ({
			type: "MemberExpression",
			indexer: indexer,
			identifier: identifier,
			base: base,
		}),
		indexExpression: (base, index) => ({
			type: "IndexExpression",
			base: base,
			index: index,
		}),
		optionalIndexExpression: (base, index) => ({
			type: "OptionalIndexExpression",
			base: base,
			index: index,
		}),
		callExpression: (base, args) => ({
			type: "CallExpression",
			base: base,
			arguments: args,
		}),
		tableCallExpression: (base, args) => ({
			type: "TableCallExpression",
			base: base,
			arguments: args,
			argument: args,
		}),
		stringCallExpression: (base, argument) => ({
			type: "StringCallExpression",
			base: base,
			argument: argument,
		}),
		comment: (value, raw) => ({
			type: "Comment",
			value: value,
			raw: raw,
		}),
	});

	// Wrap up the node object.

	function finishNode(node) {
		// Pop a `Marker` off the location-array and attach its location data.
		if (trackLocations) {
			const location = locations.pop();
			location.complete();
			location.bless(node);
		}
		if (options.onCreateNode) options.onCreateNode(node);
		return node;
	}

	// Helpers
	// -------

	const slice = Array.prototype.slice;

	let indexOf = /* istanbul ignore next */ (array, element) => {
		for (let i = 0, length = array.length; i < length; ++i) {
			if (array[i] === element) return i;
		}
		return -1;
	};

	/* istanbul ignore else */
	if (Array.prototype.indexOf)
		indexOf = (array, element) => array.indexOf(element);

	// Iterate through an array of objects and return the index of an object
	// with a matching property.

	function indexOfObject(array, property, element) {
		for (let i = 0, length = array.length; i < length; ++i) {
			if (array[i][property] === element) return i;
		}
		return -1;
	}

	// A sprintf implementation using %index (beginning at 1) to input
	// arguments in the format string.
	//
	// Example:
	//
	//     // Unexpected function in token
	//     sprintf('Unexpected %2 in %1.', 'token', 'function');

	function sprintf(format) {
		// biome-ignore lint/style/noArguments: <explanation>
		const args = slice.call(arguments, 1);
		format = format.replace(
			/%(\d)/g,
			(match, index) => `${args[index - 1]}` || /* istanbul ignore next */ "",
		);
		return format;
	}

	// Polyfill for `Object.assign`.

	let assign = /* istanbul ignore next */ (dest) => {
		// biome-ignore lint/style/noArguments: <explanation>
		const args = slice.call(arguments, 1);
		let src;
		let prop;

		for (let i = 0, length = args.length; i < length; ++i) {
			src = args[i];
			/* istanbul ignore else */
			for (prop in src)
				if (Object.prototype.hasOwnProperty.call(src, prop)) {
					dest[prop] = src[prop];
				}
		}

		return dest;
	};

	/* istanbul ignore else */
	if (Object.assign) assign = Object.assign;

	// ### Error functions

	exports.SyntaxError = SyntaxError;

	// XXX: Eliminate this function and change the error type to be different from SyntaxError.
	// This will unfortunately be a breaking change, because some downstream users depend
	// on the error thrown being an instance of SyntaxError. For example, the Ace editor:
	// <https://github.com/ajaxorg/ace/blob/4c7e5eb3f5d5ca9434847be51834a4e41661b852/lib/ace/mode/lua_worker.js#L55>

	function fixupError(e) {
		/* istanbul ignore if */
		if (!Object.create) return e;
		return Object.create(e, {
			line: { writable: true, value: e.line },
			index: { writable: true, value: e.index },
			column: { writable: true, value: e.column },
		});
	}

	// #### Raise an exception.
	//
	// Raise an exception by passing a token, a string format and its paramters.
	//
	// The passed tokens location will automatically be added to the error
	// message if it exists, if not it will default to the lexers current
	// position.
	//
	// Example:
	//
	//     // [1:0] expected [ near (
	//     raise(token, "expected %1 near %2", '[', token.value);

	function raise(token) {
		// biome-ignore lint/style/noArguments: <explanation>
		const message = sprintf.apply(null, slice.call(arguments, 1));
		let error;
		let col;

		if (token === null || typeof token.line === "undefined") {
			col = index - lineStart + 1;
			error = fixupError(
				new SyntaxError(sprintf("[%1:%2] %3", line, col, message)),
			);
			error.index = index;
			error.line = line;
			error.column = col;
		} else {
			col = token.range[0] - token.lineStart;
			error = fixupError(
				new SyntaxError(sprintf("[%1:%2] %3", token.line, col, message)),
			);
			error.line = token.line;
			error.index = token.range[0];
			error.column = col;
		}
		throw error;
	}

	function tokenValue(token) {
		const raw = input.slice(token.range[0], token.range[1]);
		if (raw) return raw;
		return token.value;
	}

	// #### Raise an unexpected token error.
	//
	// Example:
	//
	//     // expected <name> near '0'
	//     raiseUnexpectedToken('<name>', token);

	function raiseUnexpectedToken(type, token) {
		raise(token, errors.expectedToken, type, tokenValue(token));
	}

	// #### Raise a general unexpected error
	//
	// Usage should pass either a token object or a symbol string which was
	// expected. We can also specify a nearby token such as <eof>, this will
	// default to the currently active token.
	//
	// Example:
	//
	//     // Unexpected symbol 'end' near '<eof>'
	//     unexpected(token);
	//
	// If there's no token in the buffer it means we have reached <eof>.

	function unexpected(found) {
		const near = tokenValue(lookahead);
		if ("undefined" !== typeof found.type) {
			let type;
			switch (found.type) {
				case StringLiteral:
					type = "string";
					break;
				case Keyword:
					type = "keyword";
					break;
				case Identifier:
					type = "identifier";
					break;
				case NumericLiteral:
					type = "number";
					break;
				case Punctuator:
					type = "symbol";
					break;
				case BooleanLiteral:
					type = "boolean";
					break;
				case NilLiteral:
					return raise(found, errors.unexpected, "symbol", "nil", near);
				case EOF:
					return raise(found, errors.unexpectedEOF);
			}
			return raise(found, errors.unexpected, type, tokenValue(found), near);
		}
		return raise(found, errors.unexpected, "symbol", found, near);
	}

	// Lexer
	// -----
	//
	// The lexer, or the tokenizer reads the input string character by character
	// and derives a token left-right. To be as efficient as possible the lexer
	// prioritizes the common cases such as identifiers. It also works with
	// character codes instead of characters as string comparisons was the
	// biggest bottleneck of the parser.
	//
	// If `options.comments` is enabled, all comments encountered will be stored
	// in an array which later will be appended to the chunk object. If disabled,
	// they will simply be disregarded.
	//
	// When the lexer has derived a valid token, it will be returned as an object
	// containing its value and as well as its position in the input string (this
	// is always enabled to provide proper debug messages).
	//
	// `lex()` starts lexing and returns the following token in the stream.

	let index;
	let token;
	let previousToken;
	let lookahead;
	let comments;
	let tokenStart;
	let line;
	let lineStart;

	exports.lex = lex;

	function lex() {
		skipWhiteSpace();

		// Skip comments beginning with --
		while (
			45 === input.charCodeAt(index) &&
			45 === input.charCodeAt(index + 1)
		) {
			scanComment();
			skipWhiteSpace();
		}
		if (index >= length)
			return {
				type: EOF,
				value: "<eof>",
				line: line,
				lineStart: lineStart,
				range: [index, index],
			};

		const charCode = input.charCodeAt(index);
		const next = input.charCodeAt(index + 1);

		// Memorize the range index where the token begins.
		tokenStart = index;
		if (isIdentifierStart(charCode)) return scanIdentifierOrKeyword();

		switch (charCode) {
			case 39:
			case 34:
			case 96: // '"
				return scanStringLiteral();

			case 48:
			case 49:
			case 50:
			case 51:
			case 52:
			case 53:
			case 54:
			case 55:
			case 56:
			case 57: // 0-9
				return scanNumericLiteral();

			case 46: // .
				// If the dot is followed by a digit it's a float.
				if (isDecDigit(next)) return scanNumericLiteral();
				if (46 === next) {
					if (46 === input.charCodeAt(index + 2)) return scanVarargLiteral();
					return scanPunctuator("..");
				}
				return scanPunctuator(".");

			case 61: // =
				if (61 === next) return scanPunctuator("==");

				return scanPunctuator("=");

			case 62: // >
				if (features.bitwiseOperators)
					if (62 === next) return scanPunctuator(">>");
				if (61 === next) return scanPunctuator(">=");
				return scanPunctuator(">");

			case 60: // <
				if (features.bitwiseOperators)
					if (60 === next) return scanPunctuator("<<");
				if (61 === next) return scanPunctuator("<=");
				return scanPunctuator("<");

			case 126: // ~
				if (61 === next) return scanPunctuator("~=");
				if (!features.bitwiseOperators) break;
				return scanPunctuator("~");

			case 58: // :
				if (features.labels) if (58 === next) return scanPunctuator("::");
				return scanPunctuator(":");

			case 91: // [
				// Check for a multiline string, they begin with [= or [[
				if (91 === next || 61 === next) return scanLongStringLiteral();
				return scanPunctuator("[");

			case 47: // /
				// Check for integer division op (//)
				if (features.compoundAssignments) {
					//todo added
					if (61 === next) return scanPunctuator("/=");
				}
				if (features.integerDivision)
					if (47 === next) return scanPunctuator("//");
				return scanPunctuator("/");

			case 63:
				if (features.optionalChaining) {
					if (46 === next) return scanPunctuator("?.");
					if (91 === next) return scanPunctuator("?[");
				}
				break;

			/* fall through */
			case 43:
				if (features.compoundAssignments) {
					if (61 === next) return scanPunctuator("+=");
				}
			case 45:
				if (features.compoundAssignments) {
					if (61 === next) return scanPunctuator("-=");
				}
			case 42:
				if (features.compoundAssignments) {
					if (61 === next) return scanPunctuator("*=");
				}
			case 38:
			case 124: // & |
				if (!features.bitwiseOperators) break;
			case 94:
			case 37:
			case 44:
			case 123:
			case 125:
			case 93:
			case 40:
			case 41:
			case 59:
			case 35:
				// * ^ % , { } ] ( ) ; # - +

				return scanPunctuator(input.charAt(index));
		}

		return unexpected(input.charAt(index));
	}

	// Whitespace has no semantic meaning in lua so simply skip ahead while
	// tracking the encounted newlines. Any kind of eol sequence is counted as a
	// single line.

	function consumeEOL() {
		const charCode = input.charCodeAt(index);
		const peekCharCode = input.charCodeAt(index + 1);

		if (isLineTerminator(charCode)) {
			// Count \n\r and \r\n as one newline.
			if (10 === charCode && 13 === peekCharCode) ++index;
			if (13 === charCode && 10 === peekCharCode) ++index;
			++line;
			lineStart = ++index;

			return true;
		}
		return false;
	}

	function skipWhiteSpace() {
		while (index < length) {
			const charCode = input.charCodeAt(index);
			if (isWhiteSpace(charCode)) {
				++index;
			} else if (!consumeEOL()) {
				break;
			}
		}
	}

	// Identifiers, keywords, booleans and nil all look the same syntax wise. We
	// simply go through them one by one and defaulting to an identifier if no
	// previous case matched.

	function scanIdentifierOrKeyword() {
		let value;
		let type;

		// Slicing the input string is prefered before string concatenation in a
		// loop for performance reasons.
		while (isIdentifierPart(input.charCodeAt(++index)));
		value = encodingMode.fixup(input.slice(tokenStart, index));

		// Decide on the token type and possibly cast the value.
		if (isKeyword(value)) {
			type = Keyword;
		} else if ("true" === value || "false" === value) {
			type = BooleanLiteral;
			value = "true" === value;
		} else if ("nil" === value) {
			type = NilLiteral;
			value = null;
		} else {
			type = Identifier;
		}

		return {
			type: type,
			value: value,
			line: line,
			lineStart: lineStart,
			range: [tokenStart, index],
		};
	}

	// Once a punctuator reaches this function it should already have been
	// validated so we simply return it as a token.

	function scanPunctuator(value) {
		index += value.length;
		return {
			type: Punctuator,
			value: value,
			line: line,
			lineStart: lineStart,
			range: [tokenStart, index],
		};
	}

	// A vararg literal consists of three dots.

	function scanVarargLiteral() {
		index += 3;
		return {
			type: VarargLiteral,
			value: "...",
			line: line,
			lineStart: lineStart,
			range: [tokenStart, index],
		};
	}

	// Find the string literal by matching the delimiter marks used.

	function scanStringLiteral() {
		const delimiter = input.charCodeAt(index++);
		const beginLine = line;
		const beginLineStart = lineStart;
		let stringStart = index;
		let string = encodingMode.discardStrings ? null : "";
		let charCode;

		for (;;) {
			charCode = input.charCodeAt(index++);
			if (delimiter === charCode) break;
			// EOF or `\n` terminates a string literal. If we haven't found the
			// ending delimiter by now, raise an exception.
			if (index > length || isLineTerminator(charCode)) {
				string += input.slice(stringStart, index - 1);
				raise(
					null,
					errors.unfinishedString,
					input.slice(tokenStart, index - 1),
				);
			}
			if (92 === charCode) {
				// backslash
				if (!encodingMode.discardStrings) {
					const beforeEscape = input.slice(stringStart, index - 1);
					string += encodingMode.fixup(beforeEscape);
				}
				const escapeValue = readEscapeSequence();
				if (!encodingMode.discardStrings) string += escapeValue;
				stringStart = index;
			}
		}
		if (!encodingMode.discardStrings) {
			string += encodingMode.encodeByte(null);
			string += encodingMode.fixup(input.slice(stringStart, index - 1));

			if (features.joatHashes) {
				if (delimiter === 96) {
					string = joaat(string);
				}
			}
		}

		return {
			type: StringLiteral,
			value: string,
			line: beginLine,
			lineStart: beginLineStart,
			lastLine: line,
			lastLineStart: lineStart,
			range: [tokenStart, index],
		};
	}
	function joaat(key) {
		let hash = 0;
		for (let i = 0; i < key.length; ++i) {
			hash += key.charCodeAt(i);
			hash += hash << 10;
			hash ^= hash >>> 6;
		}
		hash += hash << 3;
		hash ^= hash >>> 11;
		hash += hash << 15;
		return hash >>> 0;
	}
	// Expect a multiline string literal and return it as a regular string
	// literal, if it doesn't validate into a valid multiline string, throw an
	// exception.

	function scanLongStringLiteral() {
		const beginLine = line;
		const beginLineStart = lineStart;
		const string = readLongString(false);
		// Fail if it's not a multiline literal.
		if (false === string) raise(token, errors.expected, "[", tokenValue(token));

		return {
			type: StringLiteral,
			value: encodingMode.discardStrings ? null : encodingMode.fixup(string),
			line: beginLine,
			lineStart: beginLineStart,
			lastLine: line,
			lastLineStart: lineStart,
			range: [tokenStart, index],
		};
	}

	// Numeric literals will be returned as floating-point numbers instead of
	// strings. The raw value should be retrieved from slicing the input string
	// later on in the process.
	//
	// If a hexadecimal number is encountered, it will be converted.

	function scanNumericLiteral() {
		const character = input.charAt(index);
		const next = input.charAt(index + 1);

		const literal =
			"0" === character && "xX".indexOf(next || null) >= 0
				? readHexLiteral()
				: readDecLiteral();

		const foundImaginaryUnit = readImaginaryUnitSuffix();
		const foundInt64Suffix = readInt64Suffix();

		if (foundInt64Suffix && (foundImaginaryUnit || literal.hasFractionPart)) {
			raise(null, errors.malformedNumber, input.slice(tokenStart, index));
		}

		return {
			type: NumericLiteral,
			value: literal.value,
			line: line,
			lineStart: lineStart,
			range: [tokenStart, index],
		};
	}

	function readImaginaryUnitSuffix() {
		if (!features.imaginaryNumbers) return;

		// Imaginary unit number suffix is optional.
		// See http://luajit.org/ext_ffi_api.html#literals
		if ("iI".indexOf(input.charAt(index) || null) >= 0) {
			++index;
			return true;
		}
		return false;
	}

	function readInt64Suffix() {
		if (!features.integerSuffixes) return;

		// Int64/uint64 number suffix is optional.
		// See http://luajit.org/ext_ffi_api.html#literals

		if ("uU".indexOf(input.charAt(index) || null) >= 0) {
			++index;
			if ("lL".indexOf(input.charAt(index) || null) >= 0) {
				++index;
				if ("lL".indexOf(input.charAt(index) || null) >= 0) {
					++index;
					return "ULL";
				}
				// UL but no L
				raise(null, errors.malformedNumber, input.slice(tokenStart, index));
			} else {
				// U but no L
				raise(null, errors.malformedNumber, input.slice(tokenStart, index));
			}
		} else if ("lL".indexOf(input.charAt(index) || null) >= 0) {
			++index;
			if ("lL".indexOf(input.charAt(index) || null) >= 0) {
				++index;
				return "LL";
			}
			// First L but no second L
			raise(null, errors.malformedNumber, input.slice(tokenStart, index));
		}
	}

	// Lua hexadecimals have an optional fraction part and an optional binary
	// exoponent part. These are not included in JavaScript so we will compute
	// all three parts separately and then sum them up at the end of the function
	// with the following algorithm.
	//
	//     Digit := toDec(digit)
	//     Fraction := toDec(fraction) / 16 ^ fractionCount
	//     BinaryExp := 2 ^ binaryExp
	//     Number := ( Digit + Fraction ) * BinaryExp

	function readHexLiteral() {
		let fraction = 0; // defaults to 0 as it gets summed
		let binaryExponent = 1; // defaults to 1 as it gets multiplied
		let binarySign = 1; // positive
		let digit;
		let fractionStart;
		let exponentStart;
		let digitStart;

		digitStart = index += 2; // Skip 0x part

		// A minimum of one hex digit is required.
		if (!isHexDigit(input.charCodeAt(index)))
			raise(null, errors.malformedNumber, input.slice(tokenStart, index));

		while (isHexDigit(input.charCodeAt(index))) ++index;
		// Convert the hexadecimal digit to base 10.
		digit = Number.parseInt(input.slice(digitStart, index), 16);

		// Fraction part is optional.
		let foundFraction = false;
		if ("." === input.charAt(index)) {
			foundFraction = true;
			fractionStart = ++index;

			while (isHexDigit(input.charCodeAt(index))) ++index;
			fraction = input.slice(fractionStart, index);

			// Empty fraction parts should default to 0, others should be converted
			// 0.x form so we can use summation at the end.
			fraction =
				fractionStart === index
					? 0
					: Number.parseInt(fraction, 16) / 16 ** (index - fractionStart);
		}

		// Binary exponents are optional
		let foundBinaryExponent = false;
		if ("pP".indexOf(input.charAt(index) || null) >= 0) {
			foundBinaryExponent = true;
			++index;

			// Sign part is optional and defaults to 1 (positive).
			if ("+-".indexOf(input.charAt(index) || null) >= 0)
				binarySign = "+" === input.charAt(index++) ? 1 : -1;

			exponentStart = index;

			// The binary exponent sign requires a decimal digit.
			if (!isDecDigit(input.charCodeAt(index)))
				raise(null, errors.malformedNumber, input.slice(tokenStart, index));

			while (isDecDigit(input.charCodeAt(index))) ++index;
			binaryExponent = input.slice(exponentStart, index);

			// Calculate the binary exponent of the number.
			binaryExponent = 2 ** (binaryExponent * binarySign);
		}

		return {
			value: (digit + fraction) * binaryExponent,
			hasFractionPart: foundFraction || foundBinaryExponent,
		};
	}

	// Decimal numbers are exactly the same in Lua and in JavaScript, because of
	// this we check where the token ends and then parse it with native
	// functions.

	function readDecLiteral() {
		while (isDecDigit(input.charCodeAt(index))) ++index;
		// Fraction part is optional
		let foundFraction = false;
		if ("." === input.charAt(index)) {
			foundFraction = true;
			++index;
			// Fraction part defaults to 0
			while (isDecDigit(input.charCodeAt(index))) ++index;
		}

		// Exponent part is optional.
		let foundExponent = false;
		if ("eE".indexOf(input.charAt(index) || null) >= 0) {
			foundExponent = true;
			++index;
			// Sign part is optional.
			if ("+-".indexOf(input.charAt(index) || null) >= 0) ++index;
			// An exponent is required to contain at least one decimal digit.
			if (!isDecDigit(input.charCodeAt(index)))
				raise(null, errors.malformedNumber, input.slice(tokenStart, index));

			while (isDecDigit(input.charCodeAt(index))) ++index;
		}

		return {
			value: Number.parseFloat(input.slice(tokenStart, index)),
			hasFractionPart: foundFraction || foundExponent,
		};
	}

	function readUnicodeEscapeSequence() {
		const sequenceStart = index++;

		if (input.charAt(index++) !== "{")
			raise(
				null,
				errors.braceExpected,
				"{",
				`\\${input.slice(sequenceStart, index)}`,
			);
		if (!isHexDigit(input.charCodeAt(index)))
			raise(
				null,
				errors.hexadecimalDigitExpected,
				`\\${input.slice(sequenceStart, index)}`,
			);

		while (input.charCodeAt(index) === 0x30) ++index;
		const escStart = index;

		while (isHexDigit(input.charCodeAt(index))) {
			++index;
			if (index - escStart > (features.relaxedUTF8 ? 8 : 6))
				raise(
					null,
					errors.tooLargeCodepoint,
					`\\${input.slice(sequenceStart, index)}`,
				);
		}

		const b = input.charAt(index++);
		if (b !== "}") {
			if (b === '"' || b === "'")
				raise(
					null,
					errors.braceExpected,
					"}",
					`\\${input.slice(sequenceStart, index--)}`,
				);
			else
				raise(
					null,
					errors.hexadecimalDigitExpected,
					`\\${input.slice(sequenceStart, index)}`,
				);
		}

		const codepoint = Number.parseInt(
			input.slice(escStart, index - 1) || "0",
			16,
		);
		const frag = `\\${input.slice(sequenceStart, index)}`;

		if (codepoint > (features.relaxedUTF8 ? 0x7fffffff : 0x10ffff)) {
			raise(null, errors.tooLargeCodepoint, frag);
		}

		return encodingMode.encodeUTF8(codepoint, frag);
	}

	// Translate escape sequences to the actual characters.
	function readEscapeSequence() {
		const sequenceStart = index;
		switch (input.charAt(index)) {
			// Lua allow the following escape sequences.
			case "a":
				++index;
				return "\x07";
			case "n":
				++index;
				return "\n";
			case "r":
				++index;
				return "\r";
			case "t":
				++index;
				return "\t";
			case "v":
				++index;
				return "\x0b";
			case "b":
				++index;
				return "\b";
			case "f":
				++index;
				return "\f";

			// Backslash at the end of the line. We treat all line endings as equivalent,
			// and as representing the [LF] character (code 10). Lua 5.1 through 5.3
			// have been verified to behave the same way.
			case "\r":
			case "\n":
				consumeEOL();
				return "\n";

			case "0":
			case "1":
			case "2":
			case "3":
			case "4":
			case "5":
			case "6":
			case "7":
			case "8":
			case "9": {
				// \ddd, where ddd is a sequence of up to three decimal digits.
				while (isDecDigit(input.charCodeAt(index)) && index - sequenceStart < 3)
					++index;

				const frag = input.slice(sequenceStart, index);
				const ddd = Number.parseInt(frag, 10);
				if (ddd > 255) {
					raise(null, errors.decimalEscapeTooLarge, `\\${ddd}`);
				}
				return encodingMode.encodeByte(ddd, `\\${frag}`);
			}

			case "z":
				if (features.skipWhitespaceEscape) {
					++index;
					skipWhiteSpace();
					return "";
				}
				break;

			case "x":
				if (features.hexEscapes) {
					// \xXX, where XX is a sequence of exactly two hexadecimal digits
					if (
						isHexDigit(input.charCodeAt(index + 1)) &&
						isHexDigit(input.charCodeAt(index + 2))
					) {
						index += 3;
						return encodingMode.encodeByte(
							Number.parseInt(input.slice(sequenceStart + 1, index), 16),
							`\\${input.slice(sequenceStart, index)}`,
						);
					}
					raise(
						null,
						errors.hexadecimalDigitExpected,
						`\\${input.slice(sequenceStart, index + 2)}`,
					);
				}
				break;

			case "u":
				if (features.unicodeEscapes) return readUnicodeEscapeSequence();
				break;

			case "\\":
			case '"':
			case "'":
				return input.charAt(index++);
		}

		if (features.strictEscapes)
			raise(
				null,
				errors.invalidEscape,
				`\\${input.slice(sequenceStart, index + 1)}`,
			);
		return input.charAt(index++);
	}

	// Comments begin with -- after which it will be decided if they are
	// multiline comments or not.
	//
	// The multiline functionality works the exact same way as with string
	// literals so we reuse the functionality.

	function scanComment() {
		tokenStart = index;
		index += 2; // --

		const character = input.charAt(index);
		let content = "";
		let isLong = false;
		const commentStart = index;
		const lineStartComment = lineStart;
		const lineComment = line;

		if ("[" === character) {
			content = readLongString(true);
			// This wasn't a multiline comment after all.
			if (false === content) content = character;
			else isLong = true;
		}
		// Scan until next line as long as it's not a multiline comment.
		if (!isLong) {
			while (index < length) {
				if (isLineTerminator(input.charCodeAt(index))) break;
				++index;
			}
			if (options.comments) content = input.slice(commentStart, index);
		}

		if (options.comments) {
			const node = ast.comment(content, input.slice(tokenStart, index));

			// `Marker`s depend on tokens available in the parser and as comments are
			// intercepted in the lexer all location data is set manually.
			if (options.locations) {
				node.loc = {
					start: { line: lineComment, column: tokenStart - lineStartComment },
					end: { line: line, column: index - lineStart },
				};
			}
			if (options.ranges) {
				node.range = [tokenStart, index];
			}
			if (options.onCreateNode) options.onCreateNode(node);
			comments.push(node);
		}
	}

	// Read a multiline string by calculating the depth of `=` characters and
	// then appending until an equal depth is found.

	function readLongString(isComment) {
		let level = 0;
		let content = "";
		let terminator = false;
		let character;
		let stringStart;
		const firstLine = line;

		++index; // [

		// Calculate the depth of the comment.
		while ("=" === input.charAt(index + level)) ++level;
		// Exit, this is not a long string afterall.
		if ("[" !== input.charAt(index + level)) return false;

		index += level + 1;

		// If the first character is a newline, ignore it and begin on next line.
		if (isLineTerminator(input.charCodeAt(index))) consumeEOL();

		stringStart = index;
		while (index < length) {
			// To keep track of line numbers run the `consumeEOL()` which increments
			// its counter.
			while (isLineTerminator(input.charCodeAt(index))) consumeEOL();

			character = input.charAt(index++);

			// Once the delimiter is found, iterate through the depth count and see
			// if it matches.
			if ("]" === character) {
				terminator = true;
				for (let i = 0; i < level; ++i) {
					if ("=" !== input.charAt(index + i)) terminator = false;
				}
				if ("]" !== input.charAt(index + level)) terminator = false;
			}

			// We reached the end of the multiline string. Get out now.
			if (terminator) {
				content += input.slice(stringStart, index - 1);
				index += level + 1;
				return content;
			}
		}

		raise(
			null,
			isComment ? errors.unfinishedLongComment : errors.unfinishedLongString,
			firstLine,
			"<eof>",
		);
	}

	// ## Lex functions and helpers.

	// Read the next token.
	//
	// This is actually done by setting the current token to the lookahead and
	// reading in the new lookahead token.

	function next() {
		previousToken = token;
		token = lookahead;
		lookahead = lex();
	}

	// Consume a token if its value matches. Once consumed or not, return the
	// success of the operation.

	function consume(value) {
		if (value === token.value) {
			next();
			return true;
		}
		return false;
	}

	// Expect the next token value to match. If not, throw an exception.

	function expect(value) {
		if (value === token.value) next();
		else raise(token, errors.expected, value, tokenValue(token));
	}

	// ### Validation functions

	function isWhiteSpace(charCode) {
		return (
			9 === charCode || 32 === charCode || 0xb === charCode || 0xc === charCode
		);
	}

	function isLineTerminator(charCode) {
		return 10 === charCode || 13 === charCode;
	}

	function isDecDigit(charCode) {
		return charCode >= 48 && charCode <= 57;
	}

	function isHexDigit(charCode) {
		return (
			(charCode >= 48 && charCode <= 57) ||
			(charCode >= 97 && charCode <= 102) ||
			(charCode >= 65 && charCode <= 70)
		);
	}

	// From [Lua 5.2](http://www.lua.org/manual/5.2/manual.html#8.1) onwards
	// identifiers cannot use 'locale-dependent' letters (i.e. dependent on the C locale).
	// On the other hand, LuaJIT allows arbitrary octets ≥ 128 in identifiers.

	function isIdentifierStart(charCode) {
		if (
			(charCode >= 65 && charCode <= 90) ||
			(charCode >= 97 && charCode <= 122) ||
			95 === charCode
		)
			return true;
		if (features.extendedIdentifiers && charCode >= 128) return true;
		return false;
	}

	function isIdentifierPart(charCode) {
		if (
			(charCode >= 65 && charCode <= 90) ||
			(charCode >= 97 && charCode <= 122) ||
			95 === charCode ||
			(charCode >= 48 && charCode <= 57)
		)
			return true;
		if (features.extendedIdentifiers && charCode >= 128) return true;
		return false;
	}

	// [3.1 Lexical Conventions](http://www.lua.org/manual/5.2/manual.html#3.1)
	//
	// `true`, `false` and `nil` will not be considered keywords, but literals.

	function isKeyword(id) {
		switch (id.length) {
			case 2:
				return "do" === id || "if" === id || "in" === id || "or" === id;
			case 3:
				return "and" === id || "end" === id || "for" === id || "not" === id;
			case 4:
				if ("else" === id || "then" === id) return true;
				if (features.labels && !features.contextualGoto) return "goto" === id;
				return false;
			case 5:
				return (
					"break" === id || "local" === id || "until" === id || "while" === id
				);
			case 6:
				return "elseif" === id || "repeat" === id || "return" === id;
			case 8:
				return "function" === id;
		}
		return false;
	}

	function isUnary(token) {
		if (Punctuator === token.type) return "#-~".indexOf(token.value) >= 0;
		if (Keyword === token.type) return "not" === token.value;
		return false;
	}

	// Check if the token syntactically closes a block.

	function isBlockFollow(token) {
		if (EOF === token.type) return true;
		if (Keyword !== token.type) return false;
		switch (token.value) {
			case "else":
			case "elseif":
			case "end":
			case "until":
				return true;
			default:
				return false;
		}
	}

	// Scope
	// -----

	// Store each block scope as a an array of identifier names. Each scope is
	// stored in an FILO-array.
	let scopes;
	// The current scope index
	let scopeDepth;
	// A list of all global identifier nodes.
	let globals;

	// Create a new scope inheriting all declarations from the previous scope.
	function createScope() {
		const scope = scopes[scopeDepth++].slice();
		scopes.push(scope);
		if (options.onCreateScope) options.onCreateScope();
	}

	// Exit and remove the current scope.
	function destroyScope() {
		const scope = scopes.pop();
		--scopeDepth;
		if (options.onDestroyScope) options.onDestroyScope();
	}

	// Add identifier name to the current scope if it doesnt already exist.
	function scopeIdentifierName(name) {
		if (options.onLocalDeclaration) options.onLocalDeclaration(name);
		if (-1 !== indexOf(scopes[scopeDepth], name)) return;
		scopes[scopeDepth].push(name);
	}

	// Add identifier to the current scope
	function scopeIdentifier(node) {
		scopeIdentifierName(node.name);
		attachScope(node, true);
	}

	// Attach scope information to node. If the node is global, store it in the
	// globals array so we can return the information to the user.
	function attachScope(node, isLocal) {
		if (!isLocal && -1 === indexOfObject(globals, "name", node.name))
			globals.push(node);

		node.isLocal = isLocal;
	}

	// Is the identifier name available in this scope.
	function scopeHasName(name) {
		return -1 !== indexOf(scopes[scopeDepth], name);
	}

	// Location tracking
	// -----------------
	//
	// Locations are stored in FILO-array as a `Marker` object consisting of both
	// `loc` and `range` data. Once a `Marker` is popped off the list an end
	// location is added and the data is attached to a syntax node.

	let locations = [];
	let trackLocations;

	function createLocationMarker() {
		return new Marker(token);
	}

	function Marker(token) {
		if (options.locations) {
			this.loc = {
				start: {
					line: token.line,
					column: token.range[0] - token.lineStart,
				},
				end: {
					line: 0,
					column: 0,
				},
			};
		}
		if (options.ranges) this.range = [token.range[0], 0];
	}

	// Complete the location data stored in the `Marker` by adding the location
	// of the *previous token* as an end location.
	Marker.prototype.complete = function () {
		if (options.locations) {
			this.loc.end.line = previousToken.lastLine || previousToken.line;
			this.loc.end.column =
				previousToken.range[1] -
				(previousToken.lastLineStart || previousToken.lineStart);
		}
		if (options.ranges) {
			this.range[1] = previousToken.range[1];
		}
	};

	Marker.prototype.bless = function (node) {
		if (this.loc) {
			const loc = this.loc;
			node.loc = {
				start: {
					line: loc.start.line,
					column: loc.start.column,
				},
				end: {
					line: loc.end.line,
					column: loc.end.column,
				},
			};
		}
		if (this.range) {
			node.range = [this.range[0], this.range[1]];
		}
	};

	// Create a new `Marker` and add it to the FILO-array.
	function markLocation() {
		if (trackLocations) locations.push(createLocationMarker());
	}

	// Push an arbitrary `Marker` object onto the FILO-array.
	function pushLocation(marker) {
		if (trackLocations) locations.push(marker);
	}

	// Control flow tracking
	// ---------------------
	// A context object that validates loop breaks and `goto`-based control flow.

	function FullFlowContext() {
		this.scopes = [];
		this.pendingGotos = [];
	}

	FullFlowContext.prototype.isInLoop = function () {
		let i = this.scopes.length;
		while (i-- > 0) {
			if (this.scopes[i].isLoop) return true;
		}
		return false;
	};

	FullFlowContext.prototype.pushScope = function (isLoop) {
		const scope = {
			labels: {},
			locals: [],
			deferredGotos: [],
			isLoop: !!isLoop,
		};
		this.scopes.push(scope);
	};

	FullFlowContext.prototype.popScope = function () {
		for (let i = 0; i < this.pendingGotos.length; ++i) {
			const theGoto = this.pendingGotos[i];
			if (theGoto.maxDepth >= this.scopes.length)
				if (--theGoto.maxDepth <= 0)
					raise(theGoto.token, errors.labelNotVisible, theGoto.target);
		}

		this.scopes.pop();
	};

	FullFlowContext.prototype.addGoto = function (target, token) {
		const localCounts = [];

		for (let i = 0; i < this.scopes.length; ++i) {
			const scope = this.scopes[i];
			localCounts.push(scope.locals.length);
			if (Object.prototype.hasOwnProperty.call(scope.labels, target)) return;
		}

		this.pendingGotos.push({
			maxDepth: this.scopes.length,
			target: target,
			token: token,
			localCounts: localCounts,
		});
	};

	FullFlowContext.prototype.addLabel = function (name, token) {
		const scope = this.currentScope();

		if (Object.prototype.hasOwnProperty.call(scope.labels, name)) {
			raise(token, errors.labelAlreadyDefined, name, scope.labels[name].line);
		} else {
			const newGotos = [];

			for (let i = 0; i < this.pendingGotos.length; ++i) {
				const theGoto = this.pendingGotos[i];

				if (theGoto.maxDepth >= this.scopes.length && theGoto.target === name) {
					if (
						theGoto.localCounts[this.scopes.length - 1] < scope.locals.length
					) {
						scope.deferredGotos.push(theGoto);
					}
					continue;
				}

				newGotos.push(theGoto);
			}

			this.pendingGotos = newGotos;
		}

		scope.labels[name] = {
			localCount: scope.locals.length,
			line: token.line,
		};
	};

	FullFlowContext.prototype.addLocal = function (name, token) {
		this.currentScope().locals.push({
			name: name,
			token: token,
		});
	};

	FullFlowContext.prototype.currentScope = function () {
		return this.scopes[this.scopes.length - 1];
	};

	FullFlowContext.prototype.raiseDeferredErrors = function () {
		const scope = this.currentScope();
		const bads = scope.deferredGotos;
		for (let i = 0; i < bads.length; ++i) {
			const theGoto = bads[i];
			raise(
				theGoto.token,
				errors.gotoJumpInLocalScope,
				theGoto.target,
				scope.locals[theGoto.localCounts[this.scopes.length - 1]].name,
			);
		}
		// Would be dead code currently, but may be useful later
		// if (bads.length)
		//   scope.deferredGotos = [];
	};

	// Simplified context that only checks the validity of loop breaks.

	function LoopFlowContext() {
		this.level = 0;
		this.loopLevels = [];
	}

	LoopFlowContext.prototype.isInLoop = function () {
		return !!this.loopLevels.length;
	};

	LoopFlowContext.prototype.pushScope = function (isLoop) {
		++this.level;
		if (isLoop) this.loopLevels.push(this.level);
	};

	LoopFlowContext.prototype.popScope = function () {
		const levels = this.loopLevels;
		const levlen = levels.length;
		if (levlen) {
			if (levels[levlen - 1] === this.level) levels.pop();
		}
		--this.level;
	};

	LoopFlowContext.prototype.addGoto = LoopFlowContext.prototype.addLabel =
		/* istanbul ignore next */
		() => {
			throw new Error("This should never happen");
		};

	LoopFlowContext.prototype.addLocal =
		LoopFlowContext.prototype.raiseDeferredErrors = () => {};

	function makeFlowContext() {
		return features.labels ? new FullFlowContext() : new LoopFlowContext();
	}

	// Parse functions
	// ---------------

	// Chunk is the main program object. Syntactically it's the same as a block.
	//
	//     chunk ::= block

	function parseChunk() {
		next();
		markLocation();
		if (options.scope) createScope();
		const flowContext = makeFlowContext();
		flowContext.allowVararg = true;
		flowContext.pushScope();
		const body = parseBlock(flowContext);
		flowContext.popScope();
		if (options.scope) destroyScope();
		if (EOF !== token.type) unexpected(token);
		// If the body is empty no previousToken exists when finishNode runs.
		if (trackLocations && !body.length) previousToken = token;
		return finishNode(ast.chunk(body));
	}

	// A block contains a list of statements with an optional return statement
	// as its last statement.
	//
	//     block ::= {stat} [retstat]

	function parseBlock(flowContext) {
		const block = [];
		let statement;

		while (!isBlockFollow(token)) {
			// Return has to be the last statement in a block.
			// Likewise 'break' in Lua older than 5.2
			if (
				"return" === token.value ||
				(!features.relaxedBreak && "break" === token.value)
			) {
				block.push(parseStatement(flowContext));
				break;
			}
			statement = parseStatement(flowContext);
			consume(";");
			// Statements are only added if they are returned, this allows us to
			// ignore some statements, such as EmptyStatement.
			if (statement) block.push(statement);
		}

		// Doesn't really need an ast node
		return block;
	}

	// There are two types of statements, simple and compound.
	//
	//     statement ::= break | goto | do | while | repeat | return
	//          | if | for | function | local | label | assignment
	//          | functioncall | ';'

	function parseStatement(flowContext) {
		markLocation();

		if (Punctuator === token.type) {
			if (consume("::")) return parseLabelStatement(flowContext);
		}

		// When a `;` is encounted, simply eat it without storing it.
		if (features.emptyStatement) {
			if (consume(";")) {
				if (trackLocations) locations.pop();
				return;
			}
		}

		flowContext.raiseDeferredErrors();

		if (Keyword === token.type) {
			switch (token.value) {
				case "local":
					next();
					return parseLocalStatement(flowContext);
				case "if":
					next();
					return parseIfStatement(flowContext);
				case "return":
					next();
					return parseReturnStatement(flowContext);
				case "function": {
					next();
					const name = parseFunctionName();
					return parseFunctionDeclaration(name);
				}
				case "while":
					next();
					return parseWhileStatement(flowContext);
				case "for":
					next();
					return parseForStatement(flowContext);
				case "repeat":
					next();
					return parseRepeatStatement(flowContext);
				case "break":
					next();
					if (!flowContext.isInLoop())
						raise(token, errors.noLoopToBreak, token.value);
					return parseBreakStatement();
				case "do":
					next();
					return parseDoStatement(flowContext);
				case "goto":
					next();
					return parseGotoStatement(flowContext);
			}
		}

		if (
			features.contextualGoto &&
			token.type === Identifier &&
			token.value === "goto" &&
			lookahead.type === Identifier &&
			lookahead.value !== "goto"
		) {
			next();
			return parseGotoStatement(flowContext);
		}

		// Assignments memorizes the location and pushes it manually for wrapper nodes.
		if (trackLocations) locations.pop();

		return parseAssignmentOrCallStatement(flowContext);
	}

	// ## Statements

	//     label ::= '::' Name '::'

	function parseLabelStatement(flowContext) {
		const nameToken = token;
		const label = parseIdentifier();

		if (options.scope) {
			scopeIdentifierName(`::${nameToken.value}::`);
			attachScope(label, true);
		}

		expect("::");

		flowContext.addLabel(nameToken.value, nameToken);
		return finishNode(ast.labelStatement(label));
	}

	//     break ::= 'break'

	function parseBreakStatement() {
		consume(";");
		return finishNode(ast.breakStatement());
	}

	//     goto ::= 'goto' Name

	function parseGotoStatement(flowContext) {
		const name = token.value;
		const gotoToken = previousToken;
		const label = parseIdentifier();

		flowContext.addGoto(name, gotoToken);
		return finishNode(ast.gotoStatement(label));
	}

	//     do ::= 'do' block 'end'

	function parseDoStatement(flowContext) {
		if (options.scope) createScope();
		flowContext.pushScope();
		const body = parseBlock(flowContext);
		flowContext.popScope();
		if (options.scope) destroyScope();
		expect("end");
		return finishNode(ast.doStatement(body));
	}

	//     while ::= 'while' exp 'do' block 'end'

	function parseWhileStatement(flowContext) {
		const condition = parseExpectedExpression(flowContext);
		expect("do");
		if (options.scope) createScope();
		flowContext.pushScope(true);
		const body = parseBlock(flowContext);
		flowContext.popScope();
		if (options.scope) destroyScope();
		expect("end");
		return finishNode(ast.whileStatement(condition, body));
	}

	//     repeat ::= 'repeat' block 'until' exp

	function parseRepeatStatement(flowContext) {
		if (options.scope) createScope();
		flowContext.pushScope(true);
		const body = parseBlock(flowContext);
		expect("until");
		flowContext.raiseDeferredErrors();
		const condition = parseExpectedExpression(flowContext);
		flowContext.popScope();
		if (options.scope) destroyScope();
		return finishNode(ast.repeatStatement(condition, body));
	}

	//     retstat ::= 'return' [exp {',' exp}] [';']

	function parseReturnStatement(flowContext) {
		const expressions = [];

		if ("end" !== token.value) {
			let expression = parseExpression(flowContext);
			if (null != expression) {
				expressions.push(expression);
				while (consume(",")) {
					expression = parseExpectedExpression(flowContext);
					expressions.push(expression);
				}
			}
			consume(";"); // grammar tells us ; is optional here.
		}
		return finishNode(ast.returnStatement(expressions));
	}

	//     if ::= 'if' exp 'then' block {elif} ['else' block] 'end'
	//     elif ::= 'elseif' exp 'then' block

	function parseIfStatement(flowContext) {
		const clauses = [];
		let condition;
		let body;
		let marker;

		// IfClauses begin at the same location as the parent IfStatement.
		// It ends at the start of `end`, `else`, or `elseif`.
		if (trackLocations) {
			marker = locations[locations.length - 1];
			locations.push(marker);
		}
		condition = parseExpectedExpression(flowContext);
		expect("then");
		if (options.scope) createScope();
		flowContext.pushScope();
		body = parseBlock(flowContext);
		flowContext.popScope();
		if (options.scope) destroyScope();
		clauses.push(finishNode(ast.ifClause(condition, body)));

		if (trackLocations) marker = createLocationMarker();
		while (consume("elseif")) {
			pushLocation(marker);
			condition = parseExpectedExpression(flowContext);
			expect("then");
			if (options.scope) createScope();
			flowContext.pushScope();
			body = parseBlock(flowContext);
			flowContext.popScope();
			if (options.scope) destroyScope();
			clauses.push(finishNode(ast.elseifClause(condition, body)));
			if (trackLocations) marker = createLocationMarker();
		}

		if (consume("else")) {
			// Include the `else` in the location of ElseClause.
			if (trackLocations) {
				marker = new Marker(previousToken);
				locations.push(marker);
			}
			if (options.scope) createScope();
			flowContext.pushScope();
			body = parseBlock(flowContext);
			flowContext.popScope();
			if (options.scope) destroyScope();
			clauses.push(finishNode(ast.elseClause(body)));
		}

		expect("end");
		return finishNode(ast.ifStatement(clauses));
	}

	// There are two types of for statements, generic and numeric.
	//
	//     for ::= Name '=' exp ',' exp [',' exp] 'do' block 'end'
	//     for ::= namelist 'in' explist 'do' block 'end'
	//     namelist ::= Name {',' Name}
	//     explist ::= exp {',' exp}

	function parseForStatement(flowContext) {
		let variable = parseIdentifier();
		let body;

		// The start-identifier is local.

		if (options.scope) {
			createScope();
			scopeIdentifier(variable);
		}

		// If the first expression is followed by a `=` punctuator, this is a
		// Numeric For Statement.
		if (consume("=")) {
			// Start expression
			const start = parseExpectedExpression(flowContext);
			expect(",");
			// End expression
			const end = parseExpectedExpression(flowContext);
			// Optional step expression
			const step = consume(",") ? parseExpectedExpression(flowContext) : null;

			expect("do");
			flowContext.pushScope(true);
			body = parseBlock(flowContext);
			flowContext.popScope();
			expect("end");
			if (options.scope) destroyScope();

			return finishNode(
				ast.forNumericStatement(variable, start, end, step, body),
			);
		}
		// If not, it's a Generic For Statement

		// The namelist can contain one or more identifiers.
		const variables = [variable];
		while (consume(",")) {
			variable = parseIdentifier();
			// Each variable in the namelist is locally scoped.
			if (options.scope) scopeIdentifier(variable);
			variables.push(variable);
		}
		expect("in");
		const iterators = [];

		// One or more expressions in the explist.
		do {
			const expression = parseExpectedExpression(flowContext);
			iterators.push(expression);
		} while (consume(","));

		expect("do");
		flowContext.pushScope(true);
		body = parseBlock(flowContext);
		flowContext.popScope();
		expect("end");
		if (options.scope) destroyScope();

		return finishNode(ast.forGenericStatement(variables, iterators, body));
	}

	// Local statements can either be variable assignments or function
	// definitions. If a function definition is found, it will be delegated to
	// `parseFunctionDeclaration()` with the isLocal flag.
	//
	// This AST structure might change into a local assignment with a function
	// child.
	//
	//     local ::= 'local' 'function' Name funcdecl
	//        | 'local' Name {',' Name} ['=' exp {',' exp}]

	function parseLocalStatement(flowContext) {
		let name;
		const declToken = previousToken;

		if (Identifier === token.type) {
			const variables = [];
			const init = [];

			do {
				name = parseIdentifier();

				variables.push(name);
				flowContext.addLocal(name.name, declToken);
			} while (consume(","));

			if (consume("=")) {
				do {
					const expression = parseExpectedExpression(flowContext);
					init.push(expression);
				} while (consume(","));
			}

			// Declarations doesn't exist before the statement has been evaluated.
			// Therefore assignments can't use their declarator. And the identifiers
			// shouldn't be added to the scope until the statement is complete.
			if (options.scope) {
				for (let i = 0, l = variables.length; i < l; ++i) {
					scopeIdentifier(variables[i]);
				}
			}

			return finishNode(ast.localStatement(variables, init));
		}
		if (consume("function")) {
			name = parseIdentifier();
			flowContext.addLocal(name.name, declToken);

			if (options.scope) {
				scopeIdentifier(name);
				createScope();
			}

			// MemberExpressions are not allowed in local function statements.
			return parseFunctionDeclaration(name, true);
		}
		raiseUnexpectedToken("<name>", token);
	}

	//     assignment ::= varlist '=' explist
	//     var ::= Name | prefixexp '[' exp ']' | prefixexp '.' Name
	//     varlist ::= var {',' var}
	//     explist ::= exp {',' exp}
	//
	//     call ::= callexp
	//     callexp ::= prefixexp args | prefixexp ':' Name args

	function parseAssignmentOrCallStatement(flowContext) {
		// Keep a reference to the previous token for better error messages in case
		// of invalid statement
		const previous = token;
		let marker;
		let startMarker;
		let lvalue;
		let base;
		let name;

		const targets = [];

		if (trackLocations) startMarker = createLocationMarker();

		do {
			if (trackLocations) marker = createLocationMarker();

			if (Identifier === token.type) {
				name = token.value;
				base = parseIdentifier();
				// Set the parent scope.
				if (options.scope) attachScope(base, scopeHasName(name));
				lvalue = true;
			} else if ("(" === token.value) {
				next();
				base = parseExpectedExpression(flowContext);
				expect(")");
				lvalue = false;
			} else {
				return unexpected(token);
			}

			both: for (;;) {
				switch (StringLiteral === token.type ? '"' : token.value) {
					case ".":
					case "[":
						lvalue = true;
						break;
					case ":":
					case "(":
					case "{":
					case '"':
						lvalue = null;
						break;
					default:
						break both;
				}

				base = parsePrefixExpressionPart(base, marker, flowContext);
			}

			targets.push(base);

			if ("," !== token.value) break;

			if (!lvalue) {
				return unexpected(token);
			}

			next();
		} while (true);

		if (targets.length === 1 && lvalue === null) {
			pushLocation(marker);
			return finishNode(ast.callStatement(targets[0]));
		}
		if (!lvalue) {
			return unexpected(token);
		}
		const values = [];

		if (
			token.value === "+=" ||
			token.value === "-=" ||
			token.value === "*=" ||
			token.value === "/="
		) {
			const operator = token.value;
			next();
			do {
				values.push(parseExpectedExpression(flowContext));
			} while (consume(","));
			pushLocation(startMarker);
			return finishNode(
				ast.compoundAssignmentStatement(targets, values, operator),
			);
		}

		expect("=");

		do {
			values.push(parseExpectedExpression(flowContext));
		} while (consume(","));

		pushLocation(startMarker);
		return finishNode(ast.assignmentStatement(targets, values));
	}

	// ### Non-statements

	//     Identifier ::= Name

	function parseIdentifier() {
		markLocation();
		const identifier = token.value;
		if (Identifier !== token.type) raiseUnexpectedToken("<name>", token);
		next();
		return finishNode(ast.identifier(identifier));
	}

	// Parse the functions parameters and body block. The name should already
	// have been parsed and passed to this declaration function. By separating
	// this we allow for anonymous functions in expressions.
	//
	// For local functions there's a boolean parameter which needs to be set
	// when parsing the declaration.
	//
	//     funcdecl ::= '(' [parlist] ')' block 'end'
	//     parlist ::= Name {',' Name} | [',' '...'] | '...'

	function parseFunctionDeclaration(name, isLocal) {
		const flowContext = makeFlowContext();
		flowContext.pushScope();

		const parameters = [];
		expect("(");

		// The declaration has arguments
		if (!consume(")")) {
			// Arguments are a comma separated list of identifiers, optionally ending
			// with a vararg.
			while (true) {
				if (Identifier === token.type) {
					const parameter = parseIdentifier();
					// Function parameters are local.
					if (options.scope) scopeIdentifier(parameter);

					parameters.push(parameter);

					if (consume(",")) continue;
				}
				// No arguments are allowed after a vararg.
				else if (VarargLiteral === token.type) {
					flowContext.allowVararg = true;
					parameters.push(parsePrimaryExpression(flowContext));
				} else {
					raiseUnexpectedToken("<name> or '...'", token);
				}
				expect(")");
				break;
			}
		}

		const body = parseBlock(flowContext);
		flowContext.popScope();
		expect("end");
		if (options.scope) destroyScope();

		isLocal = isLocal || false;
		return finishNode(ast.functionStatement(name, parameters, isLocal, body));
	}

	// Parse the function name as identifiers and member expressions.
	//
	//     Name {'.' Name} [':' Name]

	function parseFunctionName() {
		let base;
		let name;
		let marker;

		if (trackLocations) marker = createLocationMarker();
		base = parseIdentifier();

		if (options.scope) {
			attachScope(base, scopeHasName(base.name));
			createScope();
		}

		while (consume(".")) {
			pushLocation(marker);
			name = parseIdentifier();
			base = finishNode(ast.memberExpression(base, ".", name));
		}

		if (consume(":")) {
			pushLocation(marker);
			name = parseIdentifier();
			base = finishNode(ast.memberExpression(base, ":", name));
			if (options.scope) scopeIdentifierName("self");
		}

		return base;
	}

	//     tableconstructor ::= '{' [fieldlist] '}'
	//     fieldlist ::= field {fieldsep field} fieldsep
	//     field ::= '[' exp ']' '=' exp | Name = 'exp' | exp
	//
	//     fieldsep ::= ',' | ';'

	function parseTableConstructor(flowContext) {
		const fields = [];
		let key;
		let value;

		while (true) {
			markLocation();
			if (Punctuator === token.type && consume("[")) {
				key = parseExpectedExpression(flowContext);
				expect("]");
				expect("=");
				value = parseExpectedExpression(flowContext);
				fields.push(finishNode(ast.tableKey(key, value)));
			} else if (Identifier === token.type) {
				if ("=" === lookahead.value) {
					key = parseIdentifier();
					next();
					value = parseExpectedExpression(flowContext);
					fields.push(finishNode(ast.tableKeyString(key, value)));
				} else {
					value = parseExpectedExpression(flowContext);
					fields.push(finishNode(ast.tableValue(value)));
				}
			} else {
				if (null == (value = parseExpression(flowContext))) {
					locations.pop();
					break;
				}
				fields.push(finishNode(ast.tableValue(value)));
			}
			if (",;".indexOf(token.value) >= 0) {
				next();
				continue;
			}
			break;
		}
		expect("}");
		return finishNode(ast.tableConstructorExpression(fields));
	}

	// Expression parser
	// -----------------
	//
	// Expressions are evaluated and always return a value. If nothing is
	// matched null will be returned.
	//
	//     exp ::= (unop exp | primary | prefixexp ) { binop exp }
	//
	//     primary ::= nil | false | true | Number | String | '...'
	//          | functiondef | tableconstructor
	//
	//     prefixexp ::= (Name | '(' exp ')' ) { '[' exp ']'
	//          | '.' Name | ':' Name args | args }
	//

	function parseExpression(flowContext) {
		const expression = parseSubExpression(0, flowContext);
		return expression;
	}

	// Parse an expression expecting it to be valid.

	function parseExpectedExpression(flowContext) {
		const expression = parseExpression(flowContext);
		if (null == expression) raiseUnexpectedToken("<expression>", token);
		else return expression;
	}

	// Return the precedence priority of the operator.
	//
	// As unary `-` can't be distinguished from binary `-`, unary precedence
	// isn't described in this table but in `parseSubExpression()` itself.
	//
	// As this function gets hit on every expression it's been optimized due to
	// the expensive CompareICStub which took ~8% of the parse time.

	function binaryPrecedence(operator) {
		const charCode = operator.charCodeAt(0);
		const length = operator.length;

		if (1 === length) {
			switch (charCode) {
				case 94:
					return 12; // ^
				case 42:
				case 47:
				case 37:
					return 10; // * / %
				case 43:
				case 45:
					return 9; // + -
				case 38:
					return 6; // &
				case 126:
					return 5; // ~
				case 124:
					return 4; // |
				case 60:
				case 62:
					return 3; // < >
			}
		}
		if (2 === length) {
			switch (charCode) {
				case 47:
					return 10; // //
				case 46:
					return 8; // ..
				case 60:
				case 62:
					if ("<<" === operator || ">>" === operator) return 7; // << >>
					return 3; // <= >=
				case 61:
				case 126:
					return 3; // == ~=
				case 111:
					return 1; // or
			}
		}
		if (97 === charCode && "and" === operator) return 2;
		return 0;
	}

	// Implement an operator-precedence parser to handle binary operator
	// precedence.
	//
	// We use this algorithm because it's compact, it's fast and Lua core uses
	// the same so we can be sure our expressions are parsed in the same manner
	// without excessive amounts of tests.
	//
	//     exp ::= (unop exp | primary | prefixexp ) { binop exp }

	function parseSubExpression(minPrecedence, flowContext) {
		let operator = token.value;
		// The left-hand side in binary operations.
		let expression;
		let marker;

		if (trackLocations) marker = createLocationMarker();

		// UnaryExpression
		if (isUnary(token)) {
			markLocation();
			next();
			const argument = parseSubExpression(10, flowContext);
			if (argument == null) raiseUnexpectedToken("<expression>", token);
			expression = finishNode(ast.unaryExpression(operator, argument));
		}
		if (null == expression) {
			// PrimaryExpression
			expression = parsePrimaryExpression(flowContext);

			// PrefixExpression
			if (null == expression) {
				expression = parsePrefixExpression(flowContext);
			}
		}
		// This is not a valid left hand expression.
		if (null == expression) return null;

		let precedence;
		while (true) {
			operator = token.value;

			precedence =
				Punctuator === token.type || Keyword === token.type
					? binaryPrecedence(operator)
					: 0;

			if (precedence === 0 || precedence <= minPrecedence) break;
			// Right-hand precedence operators
			if ("^" === operator || ".." === operator) --precedence;
			next();
			const right = parseSubExpression(precedence, flowContext);
			if (null == right) raiseUnexpectedToken("<expression>", token);
			// Push in the marker created before the loop to wrap its entirety.
			if (trackLocations) locations.push(marker);
			expression = finishNode(
				ast.binaryExpression(operator, expression, right),
			);
		}
		return expression;
	}

	//     prefixexp ::= prefix {suffix}
	//     prefix ::= Name | '(' exp ')'
	//     suffix ::= '[' exp ']' | '.' Name | ':' Name args | args
	//
	//     args ::= '(' [explist] ')' | tableconstructor | String

	function parsePrefixExpressionPart(base, marker, flowContext) {
		let expression;
		let identifier;

		if (Punctuator === token.type) {
			switch (token.value) {
				case "[":
					pushLocation(marker);
					next();
					expression = parseExpectedExpression(flowContext);
					expect("]");
					return finishNode(ast.indexExpression(base, expression));
				case ".":
					pushLocation(marker);
					next();
					identifier = parseIdentifier();
					return finishNode(ast.memberExpression(base, ".", identifier));
				case "?.":
					pushLocation(marker);
					next();
					identifier = parseIdentifier();
					return finishNode(ast.memberExpression(base, "?.", identifier));
				case "?[":
					pushLocation(marker);
					next();
					expression = parseExpectedExpression(flowContext);
					expect("]");
					return finishNode(ast.optionalIndexExpression(base, expression));
				case ":":
					pushLocation(marker);
					next();
					identifier = parseIdentifier();
					base = finishNode(ast.memberExpression(base, ":", identifier));
					// Once a : is found, this has to be a CallExpression, otherwise
					// throw an error.
					pushLocation(marker);
					return parseCallExpression(base, flowContext);
				case "(":
				case "{": // args
					pushLocation(marker);
					return parseCallExpression(base, flowContext);
			}
		}
		if (StringLiteral === token.type) {
			pushLocation(marker);
			return parseCallExpression(base, flowContext);
		}

		return null;
	}

	function parsePrefixExpression(flowContext) {
		let base;
		let name;
		let marker;

		if (trackLocations) marker = createLocationMarker();

		// The prefix
		if (Identifier === token.type) {
			name = token.value;
			base = parseIdentifier();
			// Set the parent scope.
			if (options.scope) attachScope(base, scopeHasName(name));
		} else if (consume("(")) {
			base = parseExpectedExpression(flowContext);
			expect(")");
		} else {
			return null;
		}

		// The suffix
		for (;;) {
			const newBase = parsePrefixExpressionPart(base, marker, flowContext);
			if (newBase === null) break;
			base = newBase;
		}

		return base;
	}

	//     args ::= '(' [explist] ')' | tableconstructor | String

	function parseCallExpression(base, flowContext) {
		if (Punctuator === token.type) {
			switch (token.value) {
				case "(": {
					if (!features.emptyStatement) {
						if (token.line !== previousToken.line)
							raise(null, errors.ambiguousSyntax, token.value);
					}
					next();

					// List of expressions
					const expressions = [];
					let expression = parseExpression(flowContext);
					if (null != expression) {
						expressions.push(expression);
						while (consume(",")) {
							expression = parseExpectedExpression(flowContext);
							expressions.push(expression);
						}
					}

					expect(")");
					return finishNode(ast.callExpression(base, expressions));
				}

				case "{": {
					markLocation();
					next();
					const table = parseTableConstructor(flowContext);
					return finishNode(ast.tableCallExpression(base, table));
				}
			}
		}
		if (StringLiteral === token.type) {
			return finishNode(
				ast.stringCallExpression(base, parsePrimaryExpression(flowContext)),
			);
		}

		raiseUnexpectedToken("function arguments", token);
	}

	//     primary ::= String | Numeric | nil | true | false
	//          | functiondef | tableconstructor | '...'

	function parsePrimaryExpression(flowContext) {
		const literals =
			StringLiteral |
			NumericLiteral |
			BooleanLiteral |
			NilLiteral |
			VarargLiteral;
		const value = token.value;
		const type = token.type;
		let marker;

		if (trackLocations) marker = createLocationMarker();

		if (type === VarargLiteral && !flowContext.allowVararg) {
			raise(token, errors.cannotUseVararg, token.value);
		}

		if (type & literals) {
			pushLocation(marker);
			const raw = input.slice(token.range[0], token.range[1]);
			next();
			return finishNode(ast.literal(type, value, raw));
		}
		if (Keyword === type && "function" === value) {
			pushLocation(marker);
			next();
			if (options.scope) createScope();
			return parseFunctionDeclaration(null);
		}
		if (consume("{")) {
			pushLocation(marker);
			return parseTableConstructor(flowContext);
		}
	}

	// Parser
	// ------

	// Export the main parser.
	//
	//   - `wait` Hold parsing until end() is called. Defaults to false
	//   - `comments` Store comments. Defaults to true.
	//   - `scope` Track identifier scope. Defaults to false.
	//   - `locations` Store location information. Defaults to false.
	//   - `ranges` Store the start and end character locations. Defaults to
	//     false.
	//   - `onCreateNode` Callback which will be invoked when a syntax node is
	//     created.
	//   - `onCreateScope` Callback which will be invoked when a new scope is
	//     created.
	//   - `onDestroyScope` Callback which will be invoked when the current scope
	//     is destroyed.
	//
	// Example:
	//
	//     var parser = require('luaparser');
	//     parser.parse('i = 0');

	exports.parse = parse;

	const versionFeatures = {
		5.1: {},
		5.2: {
			labels: true,
			emptyStatement: true,
			hexEscapes: true,
			skipWhitespaceEscape: true,
			strictEscapes: true,
			relaxedBreak: true,
		},
		5.3: {
			labels: true,
			emptyStatement: true,
			hexEscapes: true,
			skipWhitespaceEscape: true,
			strictEscapes: true,
			unicodeEscapes: true,
			bitwiseOperators: true,
			integerDivision: true,
			relaxedBreak: true,
		},
		5.4: {
			labels: true,
			emptyStatement: true,
			hexEscapes: true,
			skipWhitespaceEscape: true,
			strictEscapes: true,
			unicodeEscapes: true,
			bitwiseOperators: true,
			integerDivision: true,
			relaxedBreak: true,
			compoundAssignments: true,
			optionalChaining: true,
			joatHashes: true,
			relaxedUTF8: true,
		},

		LuaJIT: {
			// XXX: LuaJIT language features may depend on compilation options; may need to
			// rethink how to handle this. Specifically, there is a LUAJIT_ENABLE_LUA52COMPAT
			// that removes contextual goto. Maybe add 'LuaJIT-5.2compat' as well?
			labels: true,
			contextualGoto: true,
			hexEscapes: true,
			skipWhitespaceEscape: true,
			strictEscapes: true,
			unicodeEscapes: true,
			imaginaryNumbers: true,
			integerSuffixes: true,
		},
	};

	function parse(_input, _options) {
		if ("undefined" === typeof _options && "object" === typeof _input) {
			_options = _input;
			_input = undefined;
		}
		if (!_options) _options = {};

		input = _input || "";
		options = assign({}, defaultOptions, _options);

		// Rewind the lexer
		index = 0;
		line = 1;
		lineStart = 0;
		length = input.length;
		// When tracking identifier scope, initialize with an empty scope.
		scopes = [[]];
		scopeDepth = 0;
		globals = [];
		locations = [];

		if (
			!Object.prototype.hasOwnProperty.call(versionFeatures, options.luaVersion)
		) {
			throw new Error(
				sprintf("Lua version '%1' not supported", options.luaVersion),
			);
		}

		features = assign({}, versionFeatures[options.luaVersion]);
		if (options.extendedIdentifiers !== void 0)
			features.extendedIdentifiers = !!options.extendedIdentifiers;

		if (
			!Object.prototype.hasOwnProperty.call(encodingModes, options.encodingMode)
		) {
			throw new Error(
				sprintf("Encoding mode '%1' not supported", options.encodingMode),
			);
		}

		encodingMode = encodingModes[options.encodingMode];

		if (options.comments) comments = [];
		if (!options.wait) return end();
		return exports;
	}

	// Write to the source code buffer without beginning the parse.
	exports.write = write;

	function write(_input) {
		input += String(_input);
		length = input.length;
		return exports;
	}

	// Send an EOF and begin parsing.
	exports.end = end;

	function end(_input) {
		if ("undefined" !== typeof _input) write(_input);

		// Ignore shebangs.
		if (input && input.substr(0, 2) === "#!")
			input = input.replace(/^.*/, (line) => line.replace(/./g, " "));

		length = input.length;
		trackLocations = options.locations || options.ranges;
		// Initialize with a lookahead token.
		lookahead = lex();

		const chunk = parseChunk();
		if (options.comments) chunk.comments = comments;
		if (options.scope) chunk.globals = globals;

		/* istanbul ignore if */
		if (locations.length > 0)
			throw new Error(
				"Location tracking failed. This is most likely a bug in luaparse",
			);

		return chunk;
	}
});
/* vim: set sw=2 ts=2 et tw=79 : */
