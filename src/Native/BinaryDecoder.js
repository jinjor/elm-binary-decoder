var _jinjor$binary_decoder$Native_BinaryDecoder = function() {

function decodeInt(option, context)
{
	var offset = context.position;
	if(offset + option.length > context.source.length) {
		return _elm_lang$core$Result$Err('offset exceeded source length: ' + offset);
	}
	try {
		var value = context.source[option.method](offset, option.littleEndian);
		var newContext = {
			source: context.source,
			position: offset + option.length
		};
		return _elm_lang$core$Result$Ok(
			_elm_lang$core$Native_Utils.Tuple2(newContext, value)
		);
	} catch (e) {
		return _elm_lang$core$Result$Err(e.toString());
	}
}

var uint8 = {
	method: 'getUint8',
	length: 1
};

function uint16(littleEndian) {
	return {
		method: 'getUint16',
		length: 2,
		littleEndian: littleEndian
	};
}

function uint32(littleEndian) {
	return {
		method: 'getUint32',
		length: 4,
		littleEndian: littleEndian
	};
}

var int8 = {
	method: 'getInt8',
	length: 1
};

function int16(littleEndian) {
	return {
		method: 'getInt16',
		length: 2,
		littleEndian: littleEndian
	};
}

function int32(littleEndian) {
	return {
		method: 'getInt32',
		length: 4,
		littleEndian: littleEndian
	};
}

return {
	decodeInt: F2(decodeInt),
	uint8: uint8,
	uint16: uint16,
	uint32: uint32,
	int8: int8,
	int16: int16,
	int32: int32
}

}();
