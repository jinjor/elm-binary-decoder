var _jinjor$binary_decoder$Native_TestData = function() {

function fromList(uints) {
  var uints = _elm_lang$core$Native_List.toArray(uints);
  var buffer = new ArrayBuffer(uints.length);
	var dataView = new DataView(buffer);
  for(var i = 0; i < uints.length; i++) {
    dataView.setUint8(i, uints[i]);
  }
  return dataView;
}

function empty() {
  var buffer = new ArrayBuffer(0);
	var dataView = new DataView(buffer);
  return dataView;
}

function variousUint()
{
  var buffer = new ArrayBuffer(13);
	var dataView = new DataView(buffer);
  dataView.setUint8(0, 1);
  dataView.setUint16(1, 2);
  dataView.setUint32(3, 3);
  dataView.setUint16(7, 4, true);
  dataView.setUint32(9, 5, true);
  return dataView;
}

function log(label, a) {
  console.log(label, a);
  return a;
}

return {
	empty: empty(),
  fromList: fromList,
  variousUint: variousUint()
}

}();
