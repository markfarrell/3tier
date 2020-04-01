"use strict";

var readline = require("readline");

exports.createInterface = function(readableStream) {
  return function(writableStream) {
    return function(terminalOption) {
      return function() {
        return readline.createInterface({
          input : readableStream,
          output : writableStream,
          terminal : terminalOption
        });
      };
    };
  };
};

exports.onLine = function(emit) {
  return function(readlineInterface) {
    return function() {
      readlineInterface.on("line", function(line) {
        emit(line)();
      });
    };
  };
};
