"use strict";

exports.fromArrayImpl = function(fst) {
  return function(snd) {
    return function (array) {
      var obj = {};
      array.forEach(function(elem, index) {
        var name = fst(elem);
        var value = snd(elem);
        obj[name] = value;
      });
      return obj;
    };
  };
};
