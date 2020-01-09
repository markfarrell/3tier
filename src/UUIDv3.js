"use strict";

var uuidv3 = require("uuid/v3");

exports.namespaceUUID = function(namespace) {
  return function(name) {
    return uuidv3(name, namespace);
  };
};
