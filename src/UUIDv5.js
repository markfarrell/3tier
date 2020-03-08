"use strict";

var uuidv5 = require("uuid/v5");

exports.namespaceUUID = function(namespace) {
  return function(name) {
    return uuidv5(name, namespace);
  };
};
