module Concelo.Interval
       ( Interval()
       , make
       , leftHalf
       , rightHalf
       , midpoint ) where

import Prelude ((+), (-), (/))

newtype Interval a
  = Interval { start :: a
             , length :: a }

make start end = Interval { start: start
                          , length: end - start }

leftHalf (Interval interval) =
  Interval { start: interval.start
           , length: interval.length / 2 }

rightHalf (Interval interval) =
  Interval { start: interval.start + (interval.length / 2)
           , length: interval.length / 2 }

midpoint (Interval interval) = interval.start + (interval.length / 2)
