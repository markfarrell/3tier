"use strict";

exports.stringify = JSON.stringify;

exports.parseImpl = function(left) {
  return function(right) {
    return function(input) {
      try {
        var obj = JSON.parse(input);
        return right(obj);
      } catch (err) {
        return left(err);
      }
    };
  };
};
