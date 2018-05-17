var jwt = require("jsonwebtoken");

exports.signJWT_ = function(key, payload) {
  return jwt.sign(payload, key);
};

exports.verifyJWT_ = function(error, success, key, token) {
  try {
    var token = jwt.verify(token, key);
    return success(token);
  }
  catch (e) {
    return error;
  }
};
