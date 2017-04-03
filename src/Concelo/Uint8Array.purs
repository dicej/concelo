module Concelo.Uint8Array
       ( Uint8Array()
       , unsafeGet
       , length
       , pack ) where

import Prelude ((>>>))
import Data.Array as Array

foreign import data Uint8Array :: *

foreign import unsafeGet :: Uint8Array -> Int -> Int

foreign import length :: Uint8Array -> Int

foreign import packJS :: Array Int -> Uint8Array

pack = Array.fromFoldable >>> packJS
