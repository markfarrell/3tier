"use strict";

var NodeRSA = require("node-rsa");

var rsa = new NodeRSA();

exports.generateKeyPair = function(bits) {
  return function(publicExponent) {
    return function() {
      return rsa.generateKeyPair(bits, publicExponent);
    };
  };
};

exports.sign = function(key) {
  return function(str) {
    return key.sign(str, "hex", "utf8");
  };
};

exports.verify = function(key) {
  return function(signature) {
    return function(str) {
      return key.verify(str, signature, "utf8", "hex");
    };
  };
};
