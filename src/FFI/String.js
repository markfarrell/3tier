"use strict";

exports.decodeURI = function(uri) {
  try {
    return decodeURI(uri);
  } catch (error) {
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

exports.escapeImpl = function(msg) {
  try {
    return escape(msg);
  } catch (error) {
    return msg;
  }
};

exports.encodeURIComponent = function(uriComponent) {
  try {
    return encodeURIComponent(uriComponent);
  } catch (error) {
    return uriComponent;
  }
};

exports.encodeURI = function(uri) {
  try {
    return encodeURI(uri);
  } catch (error) {
    return uri;
  }
};
