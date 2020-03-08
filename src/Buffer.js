"use strict";

exports.from = function(data) {
  return function() {
    return Buffer.from(data);
  };
};

exports.readInt16BEImpl = function(buffer) {
  return function() {
    return buffer.readInt16BE();
  };
};

exports.readInt32BEImpl = function(buffer) {
  return function() {
    return buffer.readInt32BE(0, 4);
  };
};
