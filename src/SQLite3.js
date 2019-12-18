"use strict";

var sqlite3 = require("sqlite3");

exports.connectImpl = function(filename) {
  return function(mode) {
    return function(onError, onSuccess) {
      var db = new sqlite3.Database(filename, mode, function(e) {
        if (e === null) {
          onSuccess(db);
        } else {
          onError(e);
        }
      });
      return function(cancelError, onCancelerError, onCancelerSuccess) {
        onCancelerError(cancelError);
      };
    };
  };
};

exports.closeImpl = function(db) {
  return function(onError, onSuccess) {
    db.close(function(e) {
      if (e === null) {
        onSuccess();
      } else {
        onError(e);
      }
    });
    return function(cancelError, onCancelerError, onCancelerSuccess) {
      onCancelerError(cancelError);
    };
  };
};

exports.allImpl = function(query) {
  return function(db) {
    return function(onError, onSuccess) {
      db.all(query, function(err, rows) {
        if(err) {
          onError(err);
        } else {
          onSuccess(rows);
        }
      });
      return function(cancelError, onCancelerError, onCancelerSuccess) {
        onCancelerError(cancelError);
      };
    };
  };
};

exports.showRowImpl = function(row) {
  return JSON.stringify(row);
};
