{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main,
    )
    where

import qualified ContractForDifferences as ContractForDifferences
import qualified ContractForDifferencesWithOracle as ContractForDifferencesWithOracle
import qualified CouponBondGuaranteed as CouponBondGuaranteed
import qualified Escrow as Escrow
import qualified EscrowWithCollateral as EscrowWithCollateral
import Marlowe.Mermaid (toMermaid)
import qualified Swap as Swap
import qualified ZeroCouponBond as ZeroCouponBond

main :: IO ()
main = do
    putStrLn "===== SIMPLE ESCROW ====="
    putStrLn $ toMermaid Escrow.escrow
    putStrLn "===== ESCROW WITH COLLATERAL ====="
    putStrLn $ toMermaid EscrowWithCollateral.escrowC
    putStrLn "===== ZERO COUPON BOND ====="
    putStrLn $ toMermaid ZeroCouponBond.zcb
    putStrLn "===== COUPON BOND GUARANTEED ====="
    putStrLn $ toMermaid CouponBondGuaranteed.cbg
    putStrLn "===== SWAP OF ADA AND DOLLAR TOKENS ====="
    putStrLn $ toMermaid Swap.swap
    putStrLn "===== CONTRACT FOR DIFFERENCES ====="
    putStrLn $ toMermaid ContractForDifferences.cfd
    putStrLn "===== CONTRACT FOR DIFFERENCES WITH ORACLE ====="
    putStrLn $ toMermaid ContractForDifferencesWithOracle.cfd
