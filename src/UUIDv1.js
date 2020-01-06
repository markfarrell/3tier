"use strict";

var uuidv1 = require("uuid/v1");

exports.createUUID = function() {
  return uuidv1();
};
