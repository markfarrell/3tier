"use strict";

var epoch = new Date("1970-01-01T00:00:00.000Z")

exports.current = function() {
  var date = new Date();
  return date;
};

exports.toISOString = function(date) {
  return date.toISOString();
};

exports.getMilliseconds = function(date) {
  return date.getMilliseconds();
};

exports.getYear = function(date) {
  return date.getYear();
};

exports.getTime = function(date) {
  return date.getTime();
};

exports.isValid = function(dateString) {
  return !(isNaN(Date.parse(dateString)));
};

exports.parseImpl = function(dateString) {
  var date = new Date(dateString);
  return date;
};

exports.epoch = epoch;
