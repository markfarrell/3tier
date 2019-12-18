"use strict";

var url = require("url");

exports.runMatchImpl = function(pathname) {
  return function(parameter) {
    return function(path) {
      return function() {
        var result = url.parse(path, true);
        if(result.pathname === pathname) {
          if(result.query) {
            if(typeof result.query[parameter] === "string") {
              return result.query[parameter];
            }
          }
        }
        throw new Error("Invalid URL parameter.");
      };
    };
  };
};
