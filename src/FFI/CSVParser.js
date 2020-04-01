"use strict";

var parser = require("csv-parser");

exports.createWritable = function(options) {
  return function() {
    return parser(options);
  };
};

exports.onRow = function(emit) {
  return function(writableStream) {
    return function() {
      writableStream.on("data", function(row) {
        emit(row)();
      });
    };
  };
};
