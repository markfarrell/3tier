"use strict";

var udp = require("dgram");

exports.createSocket = function() {
  return udp.createSocket("udp4");
};

exports.bind = function(server) {
  return function(port) {
    return function() {
      server.bind(port);
    };
  };
};

exports.onMessage = function(listener) {
  return function(server) {
    return function() {
      server.on("message", function(msg) {
        listener(msg.toString("utf8"))();
      });
    };
  };
};

exports.send = function(client) {
  return function(address) {
    return function(port) {
      return function(msg) {
        return function() {
          client.send(msg, port);
        };
      };
    };
  };
};
