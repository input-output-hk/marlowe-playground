{-# LANGUAGE OverloadedStrings #-}
module Example where

import Language.Marlowe.Extended.V1

main :: IO ()
main = printJSON example


{- Define a contract, Close is the simplest contract which just ends the contract straight away
-}

example :: Contract
example = Close
