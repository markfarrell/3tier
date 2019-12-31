"use strict";

exports.pipe = function(readableStream) {
  return function(writableStream) {
    return function() {
      return readableStream.pipe(writableStream);
    };
  };
};
