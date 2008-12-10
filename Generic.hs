-- Module containing generic functions
module Generic (toLowerHead, toUpperHead, fst3, snd3, thd3, mapInd)

where

import Data.Char (toUpper,toLower)

toUpperHead []    = []
toUpperHead (h:t) = (toUpper h):t
toLowerHead []    = []
toLowerHead(h:t)  = (toLower h):t

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

thd3 :: (a,b,c) -> c
thd3 (_,_,c) = c

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [1..]
