var bodyParser = require("body-parser");

exports.jsonBodyParser = bodyParser.json();

exports.urlencodedBodyParser = bodyParser.urlencoded({ extended: true });
