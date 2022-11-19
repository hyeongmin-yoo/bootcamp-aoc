// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("rescript/lib/js/curry.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Belt_Result = require("rescript/lib/js/belt_Result.js");
var Caml_option = require("rescript/lib/js/caml_option.js");

function tap(arr, fn) {
  return Belt_Array.map(arr, (function (it) {
                Curry._1(fn, it);
                return it;
              }));
}

var $$Array = {
  tap: tap
};

function toArray(input) {
  return input.trim().split("\n");
}

var Input = {
  toArray: toArray
};

function toArray$1(str) {
  return Array.from(str);
}

function divide(str, anchor) {
  var first = str.slice(0, anchor);
  var last = str.slice(anchor);
  return [
          first,
          last
        ];
}

var $$String = {
  toArray: toArray$1,
  divide: divide
};

function toOption(result) {
  if (result.TAG === /* Ok */0) {
    return Caml_option.some(result._0);
  }
  
}

function traverse(results) {
  return Belt_Array.reduce(results, {
              TAG: /* Ok */0,
              _0: []
            }, (function (r, it) {
                return Belt_Result.flatMap(r, (function (prev) {
                              return Belt_Result.map(it, (function (val) {
                                            return Belt_Array.concat(prev, [val]);
                                          }));
                            }));
              }));
}

function fold(result, err, ok) {
  if (result.TAG === /* Ok */0) {
    return Curry._1(ok, result._0);
  } else {
    return Curry._1(err, result._0);
  }
}

var Result = {
  toOption: toOption,
  traverse: traverse,
  fold: fold
};

exports.$$Array = $$Array;
exports.Input = Input;
exports.$$String = $$String;
exports.Result = Result;
/* No side effect */
