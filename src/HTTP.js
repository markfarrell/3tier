"use strict";

var http = require("http");

exports.createServer = function() {
  return http.createServer();
};

exports.listen = function(port) {
  return function(httpServer) {
    return function() {
      httpServer.listen(port);
    };
  };
};

exports.messageMethod = function(incomingMessage) {
  return incomingMessage.method;
};

exports.messageURL = function(incomingMessage) {
  return incomingMessage.url;
};

exports.host = function(incomingMessage) {
  return incomingMessage.socket.remoteAddress + ":" + incomingMessage.socket.remotePort;
};

exports.messageHeaders = function(incomingMessage) {
  return incomingMessage.headers;
};

exports.showMessageHeadersImpl = function(messageHeaders) {
  return JSON.stringify(messageHeaders, null, 2);
};

exports.end = function(serverResponse) {
  return function() {
    serverResponse.end();
  };
};

exports.writeHead = function(statusCode) {
  return function(serverResponse) {
    return function() {
      serverResponse.writeHead(statusCode);
    };
  };
};

exports.setHeader = function(name) {
  return function(value) {
    return function(serverResponse) {
      return function() {
        serverResponse.setHeader(name, value);
      };
    };
  };
}

exports.onRequest = function(requestListener) {
  return function(httpServer) {
    return function() {
      httpServer.on("request", function(req, res) {
        requestListener(req)(res)();
      });
    };
  };
};

exports.write = function(chunk) {
  return function(serverResponse) {
    return function() {
      serverResponse.write(chunk);
    };
  };
};
