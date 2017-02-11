var _jinjor$binary_decoder$Native_TestData = function() {

function fromList(uints) {
  var uints = _elm_lang$core$Native_List.toArray(uints);
  var buffer = new ArrayBuffer(uints.length);
	var dataView = new DataView(buffer);
  for(var i = 0; i < uints.length; i++) {
    dataView.setUint8(i, uints[i]);
  }
  return buffer;
}

function variousUint()
{
  var buffer = new ArrayBuffer(26);
	var dataView = new DataView(buffer);
  dataView.setUint8(0, 1);
  dataView.setUint16(1, 2);
  dataView.setUint32(3, 3);
  dataView.setUint16(7, 4, true);
  dataView.setUint32(9, 5, true);
  dataView.setInt8(13, -1);
  dataView.setInt16(14, -2);
  dataView.setInt32(16, -3);
  dataView.setInt16(20, -4, true);
  dataView.setInt32(22, -5, true);
  return buffer;
}

function log(label, a) {
  console.log(label, a);
  return a;
}

return {
  fromList: fromList,
  variousUint: variousUint()
}

}();
