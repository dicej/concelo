"use strict";

exports.unsafeGet = function (array) {
  return function (index) {
    return array[index]
  }
}

exports.length = function (array) {
  return array.length
}

exports.packJS = function (array) {
  return new Uint8Array(array)
}
