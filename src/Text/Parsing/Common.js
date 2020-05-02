"use strict";

exports.parseInt = parseInt; 

exports.parseFloat = parseFloat;

exports.indexOf = function(x) {
  return function(y) {
    return x.indexOf(y);
  };
};

exports.substringImpl = function(x) {
  return function(y) {
    return function(z) {
      return x.substring(y, z); 
    };
  };
};
