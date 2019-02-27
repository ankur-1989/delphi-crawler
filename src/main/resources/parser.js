var esprima = require('esprima')
var fs = require("fs"),
    path = require("path"),
    util = require("util");
var file = fs.readFileSync(process.argv[2], "utf8");
file = file.replace(/^#!(.*\n)/, '')
var locValue = false
if (process.argv[3] === "true") locValue = true
var result = esprima.parseScript(file, {loc: locValue, comment: true})
console.log(JSON.stringify(result))