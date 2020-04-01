"use strict";

exports.remoteAddress = function(socket) {
  return socket.remoteAddress;
};

exports.remotePort = function(socket) {
  return socket.remotePort;
};
