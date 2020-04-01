"use strict";

var uuidv1 = require("uuid/v1");

var defaultUUID = uuidv1();

exports.defaultUUID = defaultUUID;

exports.createUUID = function() {
  return uuidv1();
};
