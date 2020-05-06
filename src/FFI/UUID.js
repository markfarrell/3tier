"use strict";

exports.uuidv1 = function() {
  return require("uuid/v1")();
};

exports.uuidv5 = function(input) {
  return function(namespaceUUID) {
    return function() {
      return require("uuid/v5")(input, namespaceUUID);
    };
  };
};
