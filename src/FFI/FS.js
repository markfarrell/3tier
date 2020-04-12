"use strict";

var fs = require("fs");

exports.unlinkImpl = function(path) {
  return function(onError, onSuccess) {
    fs.unlink(path, function(err) {
      if (err) {
        onError(err);
      } else {
        onSuccess();
      }
    });
    return function(cancelError, onCancelerError, onCancelerSuccess) {
    };
  };
};
