var _jinjor$binary_decoder$Native_BinaryDecoder = function() {

function toFileList(value) {
  if(value instanceof FileList) {
    return _elm_lang$core$Result$Ok(value);
  } else {
    var str = (typeof value == 'undefined') ? 'undefined' : JSON.stringify(value);
    var message = 'Expected FileList but instead got: ' + str;
    return _elm_lang$core$Result$Err(message);
  }
}

function getAt(index, fileList) {
  var file = fileList[index];
  if(file) {
    return _elm_lang$core$Maybe$Just(file);
  } else {
    return _elm_lang$core$Maybe$Nothing;
  }
}

function readFileAsArrayBuffer(file) {
  var reader = new FileReader();
  return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
  {
    reader.addEventListener('load', function() {
      callback(_elm_lang$core$Native_Scheduler.succeed(reader.result));
    });
    // reader.addEventListener('progress', function(e) {
    //   if(e.lengthComputable) {
    //     _elm_lang$core$Native_Scheduler.rawSpawn({
    //       bytes: e.loaded,
    //       bytesExpected: e.total
    //     });
    //   }
    // });
    reader.addEventListener('error', function() {
      callback(_elm_lang$core$Native_Scheduler.fail(
        _jinjor$binary_decoder$Native_BinaryDecoder$Error
      ));
    });
    reader.readAsArrayBuffer(file);
    return function() {
      reader.abort();
    };
  });
}

function fetchArrayBuffer(url) {
  return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback){
    var xhr = new XMLHttpRequest();
    xhr.responseType = 'arraybuffer';
    xhr.open('GET', url, true);
    xhr.onload = function() {
      callback(_elm_lang$core$Native_Scheduler.succeed(this.response));
    };
    xhr.onError = function(e) {
      callback(_elm_lang$core$Native_Scheduler.fail(e.toString()));
    };
    xhr.send();
  });
}

function toDataView(buffer) {
  return new DataView(buffer);
}

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
  toFileList: toFileList,
  getAt: F2(getAt),
  readFileAsArrayBuffer: readFileAsArrayBuffer,
  fetchArrayBuffer: fetchArrayBuffer,
	toDataView: toDataView,
	decodeInt: F2(decodeInt),
	uint8: uint8,
	uint16: uint16,
	uint32: uint32,
	int8: int8,
	int16: int16,
	int32: int32
}

}();
