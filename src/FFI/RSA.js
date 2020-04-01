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
  return function(data) {
    return key.sign(data, "hex", "utf8");
  };
};

exports.verify = function(key) {
  return function(signature) {
    return function(data) {
      return key.verify(data, signature, "utf8", "hex");
    };
  };
};
