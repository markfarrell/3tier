"use strict";

var NodeRSA = require("node-rsa");

var rsa = new NodeRSA();

var defaultBits = 2048;
var defaultPublicExponent = 65537;

var defaultKey = rsa.generateKeyPair(defaultBits, defaultPublicExponent);

exports.defaultEncrypt = function(str) {
  return defaultKey.encryptPrivate(str, "base64", "utf8");
};

exports.defaultDecrypt = function(str) {
  return defaultKey.decryptPublic(str, "utf8", "base64");
};

exports.defaultSign = function(str) {
  return defaultKey.sign(str, "base64", "utf8");
};

exports.defaultVerify = function(str) {
  return function(signature) {
    return defaultKey.verify(str, signature, "utf8", "base64");
  };
};
