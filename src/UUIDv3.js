"use strict";

var uuidv3 = require("uuid/v3");

exports.url = function(str) {
  return uuidv3(str, uuidv3.URL);
};
