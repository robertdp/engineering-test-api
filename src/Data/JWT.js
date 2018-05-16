var jwt = require("jsonwebtoken");

exports.signJWT_ = function(key, payload) {
  return function() {
    return jwt.sign(payload, key);
  };
};

exports.verifyJWT_ = function(error, success, key, token) {
  return function() {
    try {
      return success(jwt.verify(token, key));
    }
    catch (e) {
      return error;
    }
  };
};
