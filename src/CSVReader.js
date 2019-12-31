"use strict";

var reader = require("csv-reader");

exports.writable = function(options) {
  return function() {
    return reader(options);
  };
};
