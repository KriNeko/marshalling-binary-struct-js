/**
cstring_utf8[sizemax];

int8le int16le int32le int64le uint8le uint16le uint32le uint64le
int8be int16be int32be int64be uint8be uint16be uint32be uint64be

/// alias le 
int8 int16 int32 int64 uint8 uint16 uint32 uint64
int8 int16 int32 int64 uint8 uint16 uint32 uint64

/// alias int32le
int
*/



const EncoderDecoderUTF8 = require("./helpers/EncoderDecoderUTF8.js");


const T_SYMBOL  = 0b1;
const T_COMMENT = 0b10;
const T_WORD    = 0b100;
const T_INTEGER = 0b1000;
const T_SPACE   = 0b10000;

const TokensRules = [
	{pattern: /\/\*[^]*?\*\//         , token: T_COMMENT, delete: true},
	{pattern: /\/\/[^\r\n]*/          , token: T_COMMENT, delete: true},
	{pattern: /[a-zA-Z_][a-zA-Z0-9_]*/, token: T_WORD},
	{pattern: /\d+/                   , token: T_INTEGER, filter: n => parseInt(n)},
	{pattern: /[{}\[\]\.\;\,]/         , token: T_SYMBOL},
	{pattern: /\s+/                   , token: T_SPACE, delete: true},
];
class Lexer {
	constructor(tokensRules) {
		this.tokensRules = tokensRules.map(r => {
			const r2 = {...r};
			if ( r2.pattern )
				r2.pattern = new RegExp(`^(${r2.pattern.source})`);
			return r2;
		});
	}

	parse(code) {
		const tokens = [];
		
		next:
		while(code.length) {
			for(const rule of this.tokensRules) {
				const m = code.match(rule.pattern);
				if ( m ) {
					const token = {
						token: rule.token,
						value: m[0],
						text : m[0]
					};
					if ( rule.filter )
						token.value = rule.filter(token.value);
					
					if ( !rule.delete )
						tokens.push(token);

					code = code.substr(token.text.length);
					continue next;
				}
			}
			
			throw new SyntaxError(`LexerError: char '${code[0]}' unexpected`);
		}

		return tokens;
	}
}


class TypeName {
	constructor(name, array, type) {
		this.name = name;
		this.array = array;
		this.type = type;
	}
}
class Struct {
	constructor(structName, props, type) {
		this.structName = structName;
		this.props = props;
		this.type = type;
	}
}
class Namespace {}
class Pack {
	constructor(stmts) {
		this.stmts = stmts;
	}
}

const ParserRules = {
	tokens: {
		T_SYMBOL, T_COMMENT, T_WORD, T_INTEGER, T_SPACE
	},
	rules: {
		nameCm: [",", "@T_WORD", ($0, name) => name],
		nameList: ["@T_WORD", "@nameCm*", (name, names) => [name, ...names]],
		
		parseTypeArray: ["[", "@T_INTEGER", "]", ($0, length) => length],
		
		parseTypeStructProp: ["@parseTypeName", ";", (typeName) => typeName],
		parseTypeStruct: ["struct", "@T_WORD?", "{", "@parseTypeStructProp+", "}", ($0, structName, $2, props) => new Struct(structName, props)],
		parseType: [
			["@parseTypeStruct"], 
			["@T_WORD"],
			([val]) => val
		],
		parseTypeName: ["@parseType", "@T_WORD", "@parseTypeArray*", (type, names, array) => new TypeName(names, array, type)],
		
		parseTypedef: ["typedef", "@parseTypeName", ";", ($0, tn) => tn],
		
		parseNamespaceNameDot: [".", "@T_WORD", ($0, name) => name],
		parseNamespaceNames: ["@T_WORD", "@parseNamespaceNameDot*", (name, names) => [name, ...names]],
		parseNamespace: ["namespace", "@parseNamespaceNames", "{", "@parseStmt*", "}", ($0, names, $2, stmts) => new Namespace(names, stmts)],
		
		parsePack: ["pack", "{", "@parseStmt*", "}", ($0, $1, stmts) => new Pack(stmts)],

		parseStmt: [
			["@parseNamespace"],
			["@parseTypedef"],
			["@parsePack"],
			([v]) => v,
		],
		
		entrypoint: ["@parseStmt*", (s) => s],
		
		
		//test: ["sup", "@T_WORD", () => new Pack()],
		
		//entrypoint: ["@test+", (s) => s],
	},
	
	rules2: {
		array: ["[", "@T_INTEGER", "]", ($0, length) => length],
		typeName: ["@type", "@T_WORD", "@array*", (type, name, array) => new TypeName(name, array, type)],
		type: [
			["struct", "@T_WORD?", "{", "@typeName*", "}", ($0, structName, $1, typeNameList) => new Struct(structName, typeNameList)],
			["@T_WORD", (type) => type],
		],
		
		typeName: [`T_WORD`],
		typeStruct: [`"struct"! T_WORD? "{" typeDesc* "}"`],
		
		type: [`typeStruct | typeName`],
		typeDesc: [`type T_WORD array* ";"`],
		
		typedef: [`"typedef"! typeDesc`, () => {}],
		pack: [`"pack"! "{" stmt* "}"`, () => {}],
		namespace: [`"namespace"! namesSplitDot "{" stmt* "}"`, () => {}],
		
		stmt: [`typedef | pack | namespace`, () => {}],
		
		entrypoint: [`stmt`],
		
		stmt: {or: ["@typedef", "@pack", "@namespace"]},
		
		stmt: "typedef | pack | namespace",
		stmt: [
			["typedef", "@typeName", ($0, typeName) => typeName],
			["pack", "{", "stmt*", "}", ($0, $1, stmtList) => new Pack(stmtList)],
			["namespace", "@namesSplitDot", "{", "stmt*", "}", ($0, names, $2, stmtList) => new Namespace(names, stmtList)],
		],
		
		entrypoint: ["@parseStmt*"]
	}

};

class EachControl {
	constructor(array, index = 0) {
		this.array = array;
		this.index = index;
	}

	get(offset, move = 1) {
		const ret = this.array[this.index + offset];
		this.index += move;
		return ret;
	}
	
	clone() {
		return new this.constructor(this.array, this.index);
	}
}
class Parser {
	constructor(rules) {
		this.rules = {
			tokens: {...rules.tokens},
			rules: {...rules.rules},
		};
	}

	parseRule(rule) {
		let flag = "";
		if ( rule[0] === "@" ) {
			if ( ["*", "+", "?"].includes(rule[rule.length-1]) )
				flag = rule[rule.length-1];
			
			const name = rule.slice(1, flag.length ? -flag.length : undefined);
			if ( this.rules.tokens.hasOwnProperty(name) )
				return {type: "token", value: this.rules.tokens[name], flag};
			
			if ( this.rules.rules.hasOwnProperty(name) )
				return {type: "rule", value: this.rules.rules[name], flag};
		}

		return {type: "value", value: rule, flag};
	}

	_parse(rules, et) {
		const array = [];
		//console.log('rules: ', rules)
		for(const rule of rules) {
			//console.log(' rule>: ', rule);
			
			let tok = et.get(0, 0);
			if ( typeof rule === "function" )
				return rule(...array);
			
			if ( Array.isArray(rule) ) {
				try {
					const et2 = et.clone();
					const ret = this._parse(rule, et2);
					et.index = et2.index;
					const f = rules.find(v => typeof v === "function");
					if ( f )
						return f(ret);
					break;
				} catch(e) {
					continue;
				}
			}
			
			const r = this.parseRule(rule);
			const isArray = ["+", "*"].includes(r.flag);
			let isNoError = ["?", "*"].includes(r.flag);
			const rArray = [];

			while(1) {
				tok = et.get(0, 0);
				
				try {
					if ( r.type === "rule" ) {
						const et2 = et.clone();
						rArray.push( this._parse(r.value, et2) );
						et.index = et2.index;
					} else {
						if ( tok[r.type] !== r.value )
							throw new SyntaxError(`ParserError: unexpected token '${tok.value}', expected '${r.value}'`);
					
						rArray.push(tok.value);
						et.get(0,1);
					}
				} catch(e) {
					if ( isNoError )
						break;
					
					throw e;
				}
				
				if ( !isArray )
					break;
				
				if (["+"].includes(r.flag) )
					isNoError = true;
			}
			
			if ( isArray )
				array.push(rArray);
			else
				array.push(rArray.length ? rArray[0] : null);
		}
		
		return array;
	}
	parse(tokens) {
		const ctx = new Context();
		const et = new EachControl(tokens);
		
		const {rules} = this.rules;
		
		const ast = this._parse(rules.entrypoint, et);
		
		const tok = et.get(0, 0);
		if ( tok )
			throw new SyntaxError(`ParserError: unexpected token '${tok.value}', expected 'end of text'`);
		
		return ast;
	}
}

class Context {
	constructor() {
		this.names = new Map();
		this.packStack = [];
	}
}

const MarshallingTypeArrayMap = {
	Uint8: "UINT8ARRAY",
	Uint16: "UINT16ARRAY",
	Uint32: "UINT32ARRAY",
	Int8: "INT8ARRAY",
	Int16: "INT16ARRAY",
	Int32: "INT32ARRAY",
	Float32: "FLOAT32ARRAY",
	Float64: "FLOAT64ARRAY",
};

class Type {
	constructor(obj) {
		this.size;
		this.alignSize;
	}
	
	clone() {}
}
class TypeBase {
	constructor(type, bits, endian = "le") {
		this.type = type;
		this.bits = bits;
		this.endian = endian;
		
		const nativeTypeName = this.type[0].toUpperCase() + this.type.slice(1);
		this.dataViewGetFnName = `get${ nativeTypeName }${ this.bits }`;
		this.dataViewSetFnName = `set${ nativeTypeName }${ this.bits }`;
		
		this.typeArrayAlignFnName = MarshallingTypeArrayMap[`${nativeTypeName}${ this.bits }`];
		
		///////////////
		this.size = this.bits >> 3;
		this.alignSize = this.size;
	}
	
	clone() {
		return new this.constructor(this.type, this.bits, this.endian);
	}
}
class TypeArray {
	constructor(itemType, length) {
		this.itemType = itemType.clone();
		this.length = length;

		///////////////
		this.alignSize = this.itemType.alignSize;
		this.size = this.itemType.size * this.length;
	}

	clone() {
		return new this.constructor(this.itemType.clone(), this.length);
	}
}
class TypeStruct {
	constructor(props, pack = false) {
		this.pack = pack;
		this.props = [];
	
		let offset = 0;
		for(const prop of props) {
			offset = this._align(offset, prop.type.alignSize);
			
			this.props.push({
				name  : prop.name,
				type  : prop.type,
				offset: offset,
			});
			
			offset += prop.type.size;
		}
		
		this.alignSize = Math.max(...props.map(p => p.type.alignSize));
		this.size = this._align(offset, this.alignSize);
	}
	
	_align(offset, size) {
		return this.pack ? offset : Math.ceil(offset / size) * size;
	}
	
	clone() {
		return new this.constructor(this.props.map(p => ({
			name: p.name,
			type: p.type.clone()
		})), this.pack);
	}
}

const baseTypes = [];
for(const endian of ["le", "be"]) {
	for(const type of ["int", "uint"]) {
		for(const bits of [8,16,32]) {
			baseTypes.push([`${type}${bits}${endian}`, new TypeBase(type, bits, endian)]);
			if ( endian === "le" )
				baseTypes.push([`${type}${bits}`, new TypeBase(type, bits, endian)]);
		}
	}

	for(const type of ["float"]) {
		for(const bits of [32,64]) {
			baseTypes.push([`${type}${bits}${endian}`, new TypeBase(type, bits, endian)]);
			if ( endian === "le" )
				baseTypes.push([`${type}${bits}`, new TypeBase(type, bits, endian)]);
		}
	}
}
baseTypes.push([`cstring_utf8`, new TypeBase("cstring_utf8", 8, "le")]);


const baseTypesMap = new Map(baseTypes);

class Build {
	constructor(baseTypesMap) {
		this.baseTypesMap = baseTypesMap;
	}

	_parseTypeRaw(rawType) {
		if ( typeof rawType === "string" ) {
			if ( this.ctx.names.has(rawType) )
				return this.ctx.names.get(rawType);
			
			if ( this.baseTypesMap.has(rawType) )
				return this.baseTypesMap.get(rawType);
			
			throw new Error(`CompileError: name '${rawType}' not found`)
		}
		
		if ( rawType instanceof Struct ) {
			const props = rawType.props.map(p => ({name: p.name, type: this.parseType(p.type, p.array)}));
			return new TypeStruct(props, this.ctx.packStack.slice(-1)[0]);
		}
	}
	parseType(rawType, array = []) {
		let type = this._parseTypeRaw(rawType);
		
		for(let length; (length = array.shift()) !== undefined; )
			type = new TypeArray(type, length);
		
		return type;
	}
	parseStmts(stmts) {
		const array = [];
		for(const stmt of stmts) {
			if ( stmt instanceof Pack ) {
				this.ctx.packStack.push(true);
					this.parseStmts(stmt.stmts);
				this.ctx.packStack.pop();
				continue;
			}
			
			if ( stmt instanceof TypeName ) {
				if ( this.ctx.names.has(stmt.name) )
					throw new Error(`CompileError: name '${stmt.name}' already def.`)
				
				this.ctx.names.set(stmt.name, this.parseType(stmt.type, stmt.array));
			}
		}
	}
	
	build(stmts) {
		this.ctx = new Context();
		
		this.parseStmts(stmts);
		return this.ctx.names;
	}
}


class MarshallingBaseClassCode {
	constructor(vnClass, size, align) {
		this.vnClass = vnClass;
		
		this.codeHeader = `
			static get _size() { return ${size} }
			static get _align() { return ${align} }

			constructor(_offset_) {
				this._offset = _offset_
			}
			
			get _offset() { return this._offset_ }
			set _offset(value) { this._offset_ = value | 0 }

			get _size() { return ${size} }
			get _align() { return ${align} }
			
			_fill(char) {
				char |= 0;
				for(let i = 0; i < ${size}; i++)
					UINT8ARRAY[this._offset + i] = char;
				return this
			}
			
			_inc(count = 1) { this._offset += count * ${size}; return this }
			_dec(count = 1) { this._offset -= count * ${size}; return this }
			_next(count = 1) { return new this.constructor(this._offset + count * ${size}) }
			_prev(count = 1) { return new this.constructor(this._offset - count * ${size}) }
			${
				Object.values(MarshallingTypeArrayMap).map(name => `
			get _${name}() { return ${name} }`).join("")
			}
		`;
		
		this.codeFooter = ``;
		
		this.codeBody = ``;
	}
	
	add(...args) {
		this.codeBody += args.length === 1 ? args[0] : String.raw(...args);
	}
	
	get hash() {
		return this.codeHeader + this.codeBody + this.codeFooter;
	}

	get code() {
		return `class ${this.vnClass} { ${ this.codeHeader + this.codeBody + this.codeFooter }}
		`;
	}
}
class CompileMarshalling {
	constructor() {
		this.code = "";

		this.classMap = new Map();

		this.vnIteratorClass = this.getRandName();
		this.code += `
		class ${this.vnIteratorClass} {
			constructor(array) {
				this.array = array;
				this.index = 0;
			}

			next() {
				if ( this.index >= this.array.length )
					return {done: true};
			
				const value = this.array.get(this.index);
				this.index++;
				return {done: false, value};
			}
		}
		`;
	}
	getRandName() {
		return "_" + Math.random().toString(36).substr(2);
	}
	
	classPrepare(mshClass) {
		if ( this.classMap.has(mshClass.hash) )
			return this.classMap.get(mshClass.hash);

		this.classMap.set(mshClass.hash, mshClass.vnClass);
		this.code += mshClass.code;
		return mshClass.vnClass;
	}
	
	_makeReadWriteCode_BaseType(type, offset, value) {
		const map = {
			8: 0,
			16: 1,
			32: 2,
			64: 3,
		};
		
		if ( type.endian === "le" ) {
			const expr = `${type.typeArrayAlignFnName}[(${offset}) >> ${map[type.bits]}]`;
			return {
				readExpr: `${ expr }`,
				writeStmt: `${ expr } = ${ value }`
			};
		}
		
		return {
			readExpr: `DATAVIEW.${ type.dataViewGetFnName }(${offset}, ${type.endian === "le"})`,
			writeStmt: `DATAVIEW.${ type.dataViewSetFnName }(${offset}, ${value}, ${type.endian === "le"})`,
		}
	}
	_makeReadWriteCode(type, offset, value) {
		if ( type instanceof TypeBase )
			return this._makeReadWriteCode_BaseType(type, offset, value);
		
		if ( type instanceof TypeArray &&
				type.itemType instanceof TypeBase &&
					type.itemType.type === "cstring_utf8" ) {

			const selStr = `UINT8ARRAY.subarray(${offset}, (${offset}) + ${type.length})`;
			return {
				readExpr: `MODULES.EncoderDecoderUTF8.decode( ${selStr} ).replace(/\\x00[^]*/, "")`,
				writeStmt: `${selStr}.set(MODULES.EncoderDecoderUTF8.encode(${value} + "\\x00").subarray(0, ${type.length}))`
			};
		}

		const itemTypeVarName = this._makeType(type);
		return {
			readExpr: `new ${itemTypeVarName}(${offset})`,
			writeStmt: `
				const srcOffset = ${offset}
				for(let i = 0; i < ${type.size}; i++)
					UINT8ARRAY[srcOffset + i] = ${value}._UINT8ARRAY[${value}._offset + i];
			`,
		}
	}
	
	_makeArrayCode(type) {
		const varName = this.getRandName();
		
		const offsetCode = `this._offset + index*${type.itemType.size}`;
		const subCode = this._makeReadWriteCode(type.itemType, offsetCode, "value");
		
		const mCode = new MarshallingBaseClassCode(varName, type.length * type.itemType.size, type.align);
		mCode.add`
			get(index) {
				return ${subCode.readExpr}
			}
			set(index, value) {
				${subCode.writeStmt}
			}
			
			[Symbol.iterator]() { return new ${this.vnIteratorClass}(this) }
			static get length() {return ${type.length}}
			get length() {return ${type.length}}
		`;
		
		return this.classPrepare(mCode);
	}
	_makeStructCode(type) {
		const varName = this.getRandName();
		
		const mCode = new MarshallingBaseClassCode(varName, type.size, type.align);
		
		let propIndex = 0;
		for(const prop of type.props) {
			const offsetCode = `this._offset + ${prop.offset}`;
			const subCode = this._makeReadWriteCode(prop.type, offsetCode, "value");
			mCode.add`
			get ${prop.name}() { return ${subCode.readExpr} }
			set ${prop.name}(value) { ${subCode.writeStmt} }
			get [${propIndex}]() { return ${subCode.readExpr} }
			set [${propIndex}](value) { ${subCode.writeStmt} }
			`;

			propIndex++;
		}
		
		return this.classPrepare(mCode);
	}

	_makeType(type) {
		if ( type instanceof TypeBase ) {
		}
		
		if ( type instanceof TypeStruct )
			return this._makeStructCode(type);
		
		if ( type instanceof TypeArray )
			return this._makeArrayCode(type);
	}

	getHelpers() {
		
``
		
	}

	compileHeaders(namesMap) {
		let code = "";
		for(const [name, type] of namesMap) {
			code += `
class ${name}_HEADER {
	static get _size() { return ${type.size} }
	get _size() { return ${type.size} }
`;

		if ( type instanceof TypeArray ) {
			code += `
	static get _length() {return ${type.length}}
	get _length() {return ${type.length}}
	static get length() {return ${type.length}}
	get length() {return ${type.length}}
			`;
		}
		
		code += `
}
`;

		}
		
		return code;
	}

	compile(namesMap) {
		const obj = {};
		for(const [name, type] of namesMap) {
			const varName = this._makeType(type);
			if ( varName )
				obj[name] = varName;
		}
		
		const firstCode = `
		const DATAVIEW     = new DataView(ARRAYBUFFER);
		const UINT8ARRAY   = new Uint8Array(ARRAYBUFFER);
		const UINT16ARRAY  = new Uint16Array(ARRAYBUFFER);
		const UINT32ARRAY  = new Uint32Array(ARRAYBUFFER);
		const INT8ARRAY    = new Int8Array(ARRAYBUFFER);
		const INT16ARRAY   = new Int16Array(ARRAYBUFFER);
		const INT32ARRAY   = new Int32Array(ARRAYBUFFER);
		const FLOAT32ARRAY = new Float32Array(ARRAYBUFFER);
		const FLOAT64ARRAY = new Float64Array(ARRAYBUFFER);
		`;
		const retCode = `\n`+
			Object.entries(obj)
			.map(([name, varName]) => `		this.${name} = ${varName};`)
			.join("\n") +
		`\n`;

		const headers = this.compileHeaders(namesMap);
		
		const headersInline = Object.keys(obj)
			.map(name => `
	static get ${name}() { return ${name}_HEADER }`)
			.join("");
		
		let code = `
		${headers}
class Marshalling {
	${headersInline}
	constructor(ARRAYBUFFER) {
		${firstCode + this.code}
		${retCode}
	}
}

return Marshalling;
`;
		
		const modules = { EncoderDecoderUTF8 };
		
		this.fullCode = code;
		//require("fs").writeFileSync("./tmpjsgencode-marshalling.js", code);
		return new Function(["MODULES"], code)(modules);
	}
}

class StructJS {
	constructor() {
		this.codeList = [];
	}
	
	add(...args) {
		const code = ( args.length === 1 ) ? args[0] : String.raw(...args);
		this.codeList.push(code);
		return this;
	}
	
	compile() {
		const code = this.codeList.join("\n");
		
		const lexer = new Lexer(TokensRules);
		const parser = new Parser(ParserRules);
		const build = new Build(baseTypesMap);

		const tokens = lexer.parse(code);
		const ast = parser.parse(tokens);
		const data = build.build(ast);

		const compileMarshalling = new CompileMarshalling();
		this.marshalling = compileMarshalling.compile(data);
		this.code = compileMarshalling.fullCode;
		
		return this.marshalling;
	}
}

module.exports = StructJS;
