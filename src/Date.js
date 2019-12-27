"use strict";

exports.current = function() {
  var date = new Date();
  return date;
};

exports.toISOString = function(date) {
  return date.toISOString();
};
