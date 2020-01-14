"use strict";

var http = require("http");
var url = require("url");

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

exports.close = function(server) {
  return function() {
    server.close();
  };
};

exports.messageMethod = function(incomingMessage) {
  return incomingMessage.method;
};

exports.messageURL = function(incomingMessage) {
  return incomingMessage.url;
};

exports.socket = function(incomingMessage) {
  return incomingMessage.socket;
};

exports.messageHeaders = function(incomingMessage) {
  return incomingMessage.headers;
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

exports.createRequestImpl = function(method) {
  return function(requestURL) {
    return function() {
      return http.request(requestURL);
    };
  };
};

exports.setRequestHeaderImpl = function(headerName) {
  return function(headerValue) {
    return function(req) {
      return function() {
        req.setHeader(headerName, headerValue);
      };
    };
  };
};

exports.endRequestImpl = function(respond) {
  return function(req) {
    return function(onError, onSuccess) {
      var result = "";
      req.on("response", function(res) {
        res.setEncoding("utf8");
        res.on("data", function(data) {
          result += data;
        });
        res.on("end", function() {
          onSuccess(respond(result)(res));
        });
      });
      req.on("error", function(error) {
        onError(error);
      });
      req.end();
      return function(cancelError, cancelerError, cancelerSuccess) {
        req.on("error", function() {
          cancelerError(cancelError);
        });
        req.on("close", function() {
          cancelerSuccess();
        });
        req.abort();
      };
    };
  };
};
