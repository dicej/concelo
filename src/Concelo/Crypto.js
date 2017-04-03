"use strict";

var nacl = (typeof window !== 'undefined') ? window.nacl : require('../' + (process.env.NACL_SRC || 'nacl.min.js'))

function concatenate(buffers) {
  if (buffers.length == 1) {
    return buffers[0];
  }

  var total = 0
  for (var i = 0; i < buffers.length; ++i) {
    total += buffers[i].length
  }

  var result = new Uint8Array(total)
  var offset = 0
  for (var i = 0; i < buffers.length; ++i) {
    result.set(buffers[i], offset)
    offset += buffers[i].length
  }

  return result
}

exports.seedLength = nacl.sign.seedLength

exports.derivePrivateJS = function(seed) {
  return nacl.sign.keyPair.fromSeed(seed).secretKey
}

exports.derivePublicJS = function(privateKey) {
  return nacl.sign.keyPair.fromSecretKey(privateKey).publicKey
}

exports.signJS = function(privateKey) {
  return function(buffers) {
    return nacl.sign.detached(concatenate(buffers), privateKey);
  }
}

exports.verifyJS = function(signature) {
  return function(publicKey) {
    return function(buffers) {
      return nacl.sign.detached.verify(concatenate(buffers), signature, publicKey)
    }
  }
}

exports.hashJS = function(buffers) {
  return nacl.hash(concatenate(buffers))
}
