// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Belt_HashMapString = require("rescript/lib/js/belt_HashMapString.js");

function make(data) {
  var results_0 = Belt_HashMapString.get(data, "byr");
  var results_1 = Belt_HashMapString.get(data, "iyr");
  var results_2 = Belt_HashMapString.get(data, "eyr");
  var results_3 = Belt_HashMapString.get(data, "hgt");
  var results_4 = Belt_HashMapString.get(data, "hcl");
  var results_5 = Belt_HashMapString.get(data, "ecl");
  var results_6 = Belt_HashMapString.get(data, "pid");
  var results_7 = Belt_HashMapString.get(data, "cid");
  var byr = results_0;
  if (byr === undefined) {
    return {
            TAG: /* Error */1,
            _0: "fields are not fullfiled"
          };
  }
  var iyr = results_1;
  if (iyr === undefined) {
    return {
            TAG: /* Error */1,
            _0: "fields are not fullfiled"
          };
  }
  var eyr = results_2;
  if (eyr === undefined) {
    return {
            TAG: /* Error */1,
            _0: "fields are not fullfiled"
          };
  }
  var hgt = results_3;
  if (hgt === undefined) {
    return {
            TAG: /* Error */1,
            _0: "fields are not fullfiled"
          };
  }
  var hcl = results_4;
  if (hcl === undefined) {
    return {
            TAG: /* Error */1,
            _0: "fields are not fullfiled"
          };
  }
  var ecl = results_5;
  if (ecl === undefined) {
    return {
            TAG: /* Error */1,
            _0: "fields are not fullfiled"
          };
  }
  var pid = results_6;
  if (pid !== undefined) {
    return {
            TAG: /* Ok */0,
            _0: {
              byr: byr,
              iyr: iyr,
              eyr: eyr,
              hgt: hgt,
              hcl: hcl,
              ecl: ecl,
              pid: pid,
              cid: results_7
            }
          };
  } else {
    return {
            TAG: /* Error */1,
            _0: "fields are not fullfiled"
          };
  }
}

exports.make = make;
/* No side effect */