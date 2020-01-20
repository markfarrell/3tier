"use strict";

var NodeRSA = require("node-rsa");

var rsa = new NodeRSA();

var defaultBits = 2048;
var defaultPublicExponent = 65537;

var defaultKey = rsa.generateKeyPair(defaultBits, defaultPublicExponent);

exports.defaultEncrypt = function(str) {
  return defaultKey.encryptPrivate(str, "base64");
};

exports.defaultDecrypt= function(str) {
  return defaultKey.decryptPublic(str, "utf8", "base64");
};
