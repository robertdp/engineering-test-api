var uuid = require("uuid");

exports.generateUUIDImpl = function() {
  return uuid.v4();
};
