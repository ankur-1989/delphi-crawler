var esprima = require('esprima')
var fs = require("fs"),
    path = require("path"),
    util = require("util");
var file = fs.readFileSync(process.argv[2], "utf8");
//console.log(file);
var result = esprima.parseScript(file, {comment: true})
console.log(JSON.stringify(result));
