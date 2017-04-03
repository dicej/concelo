module Concelo.Crypto
       ( Hash()
       , Seed()
       , Signature()
       , PrivateKey()
       , PublicKey()
       , hash
       , class ToUint8Array
       , toUint8Array
       , first48
       , maybeSeed
       , sign
       , verify ) where

import Prelude (($), (==), (+), (*), (>>>), map, id)
import Data.Int53 as Int53
import Data.Maybe (Maybe(Just, Nothing))

import Concelo.Uint8Array (Uint8Array())
import Concelo.Uint8Array as Uint8Array

newtype Hash = Hash Uint8Array

newtype Seed = Seed Uint8Array

newtype Signature = Signature Uint8Array

newtype PrivateKey = PrivateKey Uint8Array

newtype PublicKey = PublicKey Uint8Array

class ToUint8Array b where
  toUint8Array :: b -> Uint8Array

instance toUint8ArrayUint8Array :: ToUint8Array Uint8Array where
  toUint8Array = id

instance toUint8ArrayHash :: ToUint8Array Hash where
  toUint8Array (Hash buffer) = buffer

first48 (Hash buffer) =
  (get 0 `shiftLeft` 40) +
  (get 1 `shiftLeft` 32) +
  (get 2 `shiftLeft` 24) +
  (get 3 `shiftLeft` 16) +
  (get 4 `shiftLeft`  8) +
  (get 5               )
  where
    get = Uint8Array.unsafeGet buffer >>> Int53.fromInt
    shiftLeft n count = n * Int53.pow (Int53.fromInt 2) (Int53.fromInt count)

foreign import seedLength :: Int

maybeSeed buffer = if Uint8Array.length buffer == seedLength then
                     Just $ Seed buffer
                   else
                     Nothing

foreign import derivePrivateJS :: Uint8Array -> Uint8Array

derivePrivate (Seed seed) = PrivateKey $ derivePrivateJS seed

foreign import derivePublicJS :: Uint8Array -> Uint8Array

derivePublic (PrivateKey privateKey) = PublicKey $ derivePublicJS privateKey

foreign import signJS :: Uint8Array -> Array Uint8Array -> Uint8Array

sign (PrivateKey privateKey) =
  map toUint8Array >>> signJS privateKey >>> Signature

foreign import verifyJS :: Uint8Array -> Uint8Array -> Array Uint8Array -> Boolean

verify (Signature signature) (PublicKey publicKey) =
  map toUint8Array >>> verifyJS signature publicKey

foreign import hashJS :: Array Uint8Array -> Uint8Array

hash = map toUint8Array >>> hashJS >>> Hash
