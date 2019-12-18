"use string";

exports.decodeURI = function(uri) {
  try {
    return decodeURI(uri);
  } catch(error) {
    return uri;
  }
};

exports.decodeURIComponent = function(uriComponent) {
  try {
    return decodeURIComponent(uriComponent);
  } catch (error) {
    return uriComponent;
  }
};

exports.encodeBase64 = function(msg) {
  return Buffer.from(msg).toString("base64");
};
