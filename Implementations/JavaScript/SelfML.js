var SFNode = (function() {
	"use strict";

	function SFNode() {
		this.children = [];
	};

	SFNode.prototype.stringValue = null;
	SFNode.prototype.parent = null;

	SFNode.createFromString = function(sourceString) {
		var node = new SFNode();
		parseRoot(String(sourceString), node);
		return node;
	};

	SFNode.prototype.addString = function(str) {
		var node = new SFStringNode(str);
		this.children.push(node);
	};

	SFNode.prototype.toString = function(indentation) {
		var out;

		indentation = Number(indentation);
		if (isNaN(indentation)) {
			indentation = 0;
		}

		out = "";
		var head = this.stringValue;
		if (head) {
			out += "(" + head;
		}

		var isScalarOnly = true;
		if (!head) {
			isScalarOnly = false;
		} else {
			this.children.forEach(function(child) {
				if (child instanceof SFNode) {
					isScalarOnly = false;
				}
			});
		}

		this.children.forEach(function(child, first) {
			var newindent = 0;

			first = first === 0;
			if (!head) {
				if (!first) {
					out += "\n\n";
				}
				newindent = 0;
			} else {
				if (isScalarOnly) {
					out += " ";
					newindent = 0;
				} else {
					out += "\n";
					newindent = indentation + 1;
					for (var i = 0; i < newindent; i++) {
						out += "    ";
					}
				}
			}
			out += child.toString(newindent);
		});

		if (head) {
			out += ")";
		}

		return out;
	};

	function SFStringNode(stringValue) {
		this.stringValue = stringValue;
	}

	SFStringNode.prototype.toString = function() {
		var str = this.stringValue;
		if (str === null) {
			return "";
		}

		if (!str.length) {
			return "[]";
		}

		// Find out if scannerStrval can be written as a verbatim string or bracketed string
		var isVerbatimString = true;
		var isBracketedString = true;

		var bracketedStringNestingLevel = 0;
		if (/[\s#`()\[\]{}]/.test(str)) {
			isVerbatimString = false;
		}

		if ((str.match(/\[/g)||[]).length !== (str.match(/\]/g)||[]).length) {
			isBracketedString = false;
		}

		if (isVerbatimString) {
			return str;
		} else if (isBracketedString) {
			return "[" + str + "]";
		} else {
			return "`" + str.replace(/`/g, "``") + "`";
		}
	};


	// root := nodes
	function parseRoot(sourceString, rootNode) {
		//`offset` is a cursor for sourceString while we're parsing
		//Functions take a pointer to it, then increment or decrement it
		//It can be a little tricky to figure out what's happening, but there's a general rule of thumb:
		//  Functions *take* `offset` on the character they want to parse (so a line comment takes `offset` pointing to the '#')
		//  Functions *leave* `offset` on the last character they parsed (so a line comment leaves `offset` pointing to the 'LF')
		var offset = {value: 0};

		//Parse a list of nodes
		parseNodes(sourceString, sourceString.length, rootNode, offset);
	}

	// nodes := <node>*
	function parseNodes(sourceString, length, node, offset) {
		var str;
		// Loop continuously unless we run out of characters to parse
		// We will exit early
		while (offset.value < length) {
			// Check `c` to see what kind of thing we'll be parsing
			var c = sourceString.charCodeAt(offset.value);
			switch (c) {
			case 9:  // \t
			case 10: // \n
			case 11: // \v
			case 12: // \f
			case 13: // \r
			case 32: // space
				// whitespace is ignored
				break;
			case 93:  // ']'
			case 125: // '}'
				// ] and } are ERRORS
				throw new Error("Unexpected " + String.fromCharCode(c) + " in input at offset " + offset.value);
			case 123: // '{'
				// an opening { is an ERROR unless it is immediately followed by a #, in which case it's a block comment
				var next_c = sourceString.charCodeAt(offset.value + 1);
				if (next_c === 35) {
					parseBlockComment(sourceString, length, node, offset);
				} else {
					throw new Error("Unexpected { in input at offset " + offset.value);
				}
				break;
			case 35:  // '#'
				// # denotes a line comment
				parseLineComment(sourceString, length, node, offset);
				break;
			case 91:  // '['
				// [ denotes a string literal
				str = parseBracketString(sourceString, length, node, offset);
				node.addString(str);
				break;
			case 96:  // '`'
				// ` denotes a string literal
				str = parseBacktickString(sourceString, length, node, offset);
				node.addString(str);
				break;
			case 40:  // '('
				// ( denotes a subnode
				// parse a new subnode until it returns control to us
				parseList(sourceString, length, node, offset);
				break;
			case 41:  // '('
				// ) denotes the end of this node
				// return control to the parent
				return;
			default:
				// any other characters start a verbatim string
				str = parseVerbatimString(sourceString, length, node, offset);
				node.addString(str);
			}
			offset.value++;
		}

		//The rule is to leave on the last character we parsed
		offset.value--;
	}

	// head := BACKTICK_STRING | BRACKETED_STRING | VERBATIM_STRING
	function parseHead(sourceString, length, node, offset) {
		var str;

		while (offset.value < length) {
			var c = sourceString.charCodeAt(offset.value);
			switch (c) {
			case 9:  // \t
			case 10: // \n
			case 11: // \v
			case 12: // \f
			case 13: // \r
			case 32: // space
				// whitespace is ignored
				break;
			case 93:  // ']'
			case 125: // '}'
				// ] and } are ERRORS
				throw new Error("Unexpected " + String.fromCharCode(c) + " in input at offset " + offset.value);
			case 123: // '{'
				// an opening { is an ERROR unless it is immediately followed by a #, in which case it's a block comment
				var next_c = sourceString.charCodeAt(offset.value + 1);
				if (next_c === 35) {
					parseBlockComment(sourceString, length, node, offset);
				} else {
					throw new Error("Unexpected { in input at offset " + offset.value);
				}
				break;
			case 35:  // '#'
				// # denotes a line comment
				parseLineComment(sourceString, length, node, offset);
				break;
			case 91:  // '['
				// [ denotes a string literal
				str = parseBracketString(sourceString, length, node, offset);
				offset.value++;
				return str;
			case 96:  // '`'
				// ` denotes a string literal
				str = parseBacktickString(sourceString, length, node, offset);
				offset.value++;
				return str;
			default:
				// any other characters start a verbatim string
				str = parseVerbatimString(sourceString, length, node, offset);
				offset.value++;
				return str;
			}
			offset.value++;
		}

		return null;
	}

	// blockComment := '{#' (blockComment | any)+ '#}'
	function parseBlockComment(sourceString, length, node, offset) {
		if (offset.value >= length) {
			return;
		}

		offset.value += 2;

		while (offset.value < length) {
			var c = sourceString.charCodeAt(offset.value);
			var next_c = sourceString.charCodeAt(offset.value + 1);
			if (c === 123 && next_c === 35) {
				parseBlockComment(sourceString, length, node, offset);
			} else if (c === 35 && next_c === 125) {
				break;
			}
			offset.value++;
		}

		offset.value++;

	}

	// lineComment := '#' any+ (CR | LF)
	function parseLineComment(sourceString, length, node, offset) {
		if (offset.value >= length) {
			return;
		}

		for(offset.value++; offset.value < length; offset.value++) {
			var c = sourceString.charCodeAt(offset.value);
			if (c === 0x0A || c === 0x0D) {
				return;
			}
		}

		offset.value--;
	}

	// curlyBracketBlock := '{' (curlyBracketBlock | any)+ '}'
	function parseCurlyBracketBlock(sourceString, length, node, offset) {
		if (offset.value >= length) {
			return;
		}

		for(offset.value++; offset.value < length; offset.value++) {
			var c = sourceString.charCodeAt(offset.value);
			if (c === 135) {
				parseCurlyBracketBlock(sourceString, length, node, offset);
			} else if (c === 135) {
				return;
			}
		}

		offset.value--;
	}

	// verbatim := [^\s()[\]{}]+
	function parseVerbatimString(sourceString, length, node, offset) {
		if (offset.value >= length) {
			return null;
		}

		var buffer = "";

		out:
		for(; offset.value < length; offset.value++) {
			var c = sourceString.charCodeAt(offset.value);
			switch (c) {
			case 9: case 10: case 11: case 12: case 13: case 32: // whitespace
			case 40: case 41: // ()
			case 91: case 93: // []
			case 123: case 125: // {}
				break out;
			default:
				buffer += sourceString[offset.value];
			}
		}

		offset.value--;
		return buffer;
	}

	// bracketString := '[' (bracketString | any)+ ']'
	function parseBracketString(sourceString, length, node, offset) {
		if (offset.value >= length) {
			return null;
		}

		var buffer = "";
		var nestingLevel = 1;

		offset.value++;

		while (offset.value < length) {
			var c = sourceString.charCodeAt(offset.value);
			if (c === 91) {
				nestingLevel++;
			} else if (c === 93) {
				nestingLevel--;
				if (nestingLevel === 0) {
					break;
				}
			}

			buffer += sourceString[offset.value];
			offset.value++;
		}

		return buffer;
	}

	// backtickString := '`' ('``' | any)+ '`'
	function parseBacktickString(sourceString, length, node, offset) {
		if (offset.value >= length) {
			return null;
		}

		var buffer = "";

		offset.value++;
		while (offset.value < length) {
			var c = sourceString.charCodeAt(offset.value);
			var next_c = sourceString.charCodeAt(offset.value + 1);

			if (c === 96) {
				var next_c = sourceString.charCodeAt(offset.value + 1);
				if (next_c === 96) {
					buffer += "`";
					offset.value += 2;
					continue;
				} else {
					break;
				}
			}

			buffer += sourceString[offset.value];
			offset.value++;
		}

		return buffer;
	}

	// list := '(' head nodes ')'
	function parseList(sourceString, length, parentNode, offset) {
		if (offset.value >= length) {
			return;
		}

		offset.value++;

		// Create the node
		var node = new SFNode();

		// Head
		var head = parseHead(sourceString, length, parentNode, offset);

		// If no head can be found, do a parse anyway to sort up any loose ends, but don't use the result
		if (!head) {
			parseNodes(sourceString, length, node, offset);
			return;
		}

		node.stringValue = head;

		//Add to its parent
		node.parent = parentNode;
		parentNode.children.push(node);

		//Children
		parseNodes(sourceString, length, node, offset);
	}

	return SFNode;

}());

if (typeof module === "object") {
	module.exports = SFNode;
}
