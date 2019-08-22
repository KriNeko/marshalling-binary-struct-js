# Marshalling-binary-struct-js

Allows projecting native structures onto structures of a typed array.
  - Status: pre alpha
  
# Install
`npm i marshalling-binary-struct-js`

# Simple performance test
https://jsperf.com/marshalling-test-v2-0-1

# Examples
```javascript
const StructJS = require("marshalling-binary-struct-js");


const structJS = new StructJS();
structJS.add`
	typedef int32le i32;
	typedef struct {
		cstring_utf8 myString[256];
		i32 index;
		i32 flags;
		i32 flags2;
		struct {
			float32le x;
			float32le y;
			float32le z;
		} pos;
	} tmp_t;

	typedef tmp_t array_of_tmp_t[1024];
`;

const Marshalling = structJS.compile();
const ab = new ArrayBuffer(Marshalling.array_of_tmp_t._size);
const marshal = new Marshalling(ab);
const arrayTmp = new marshal.array_of_tmp_t(0);

arrayTmp._fill(0);

let i = 0;
for(const tmp of arrayTmp) {
	tmp.myString = `tmp string for ${i}`;
	tmp.index = i;
	tmp.flags |= 1;
	tmp.flags2 |= 0xF5890;
	tmp.pos.x = 2;
	tmp.pos.y = 2;
	tmp.pos.z = 2;
	i++;
}
console.log( arrayTmp.get(100).myString );
console.log( arrayTmp.get(100).index );
```
 
 # Base types
 ```javascript
 cstring_utf8[sizemax];

int8le int16le int32le int64le uint8le uint16le uint32le uint64le
int8be int16be int32be int64be uint8be uint16be uint32be uint64be

/// alias le 
int8 int16 int32 int64 uint8 uint16 uint32 uint64
int8 int16 int32 int64 uint8 uint16 uint32 uint64

/// alias int32le
int
```

Author: RainNeko