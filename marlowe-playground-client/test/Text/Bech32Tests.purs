module Test.Text.Bech32Tests
  ( all
  ) where

import Prologue

import Data.Foldable (traverse_)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldNotSatisfy, shouldSatisfy)
import Text.Bech32 (validPaymentShelleyAddress)

all :: Spec Unit
all =
  describe "Shelley address check" do
    mainnetPaymentKeyAddresses
    invalidPaymentKeyAddresses
    mainnetScriptKeyAddresses
    mainnetStakeAddresses
    testnetPaymentKeyAddresses
    testnetScriptKeyAddresses
    testnetStakeAddresses

mainnetPaymentKeyAddresses :: Spec Unit
mainnetPaymentKeyAddresses =
  it "Mainnet payment key addresses" do
    traverse_ (\addr -> shouldSatisfy addr validPaymentShelleyAddress)
      [ "addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgse35a3x" -- type-00
      , "addr1yx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzerkr0vd4msrxnuwnccdxlhdjar77j6lg0wypcc9uar5d2shs2z78ve" -- type-02
      , "addr1gx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer5pnz75xxcrzqf96k" -- type-04
      , "addr1vx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzers66hrl8" -- type-06
      ]

invalidPaymentKeyAddresses :: Spec Unit
invalidPaymentKeyAddresses =
  it "Invalid addresses" do
    traverse_ (\addr -> shouldNotSatisfy addr validPaymentShelleyAddress)
      [ "addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3nqd3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgse35a3x" -- type-00 (bad checksum)
      , "addr1yx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzerkr0vd4msrxnuwnccdxlhdjar77j6lg0wypcc9ulr5d2shs2z78ve" -- type-02 (bad checksum)
      , "addr_test1gx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer5pnz75xxcrzqf96k" -- type-04 (bad network label/bit)
      , "addr1vx2fxv2umyhttkxyxp1x0dlpdt3k6cwng5pxj3jhsydzers66hrl8" -- type-06 (bad label)
      ]

mainnetScriptKeyAddresses :: Spec Unit
mainnetScriptKeyAddresses =
  it "Mainnet script addresses" do
    traverse_ (\addr -> shouldNotSatisfy addr validPaymentShelleyAddress)
      [ "addr1z8phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gten0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgs9yc0hh" -- type-01
      , "addr1x8phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gt7r0vd4msrxnuwnccdxlhdjar77j6lg0wypcc9uar5d2shskhj42g" -- type-03
      , "addr128phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtupnz75xxcrtw79hu" -- type-05
      , "addr1w8phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtcyjy7wx" -- type-07
      ]

mainnetStakeAddresses :: Spec Unit
mainnetStakeAddresses =
  it "Mainnet stake addresses" do
    traverse_ (\addr -> shouldNotSatisfy addr validPaymentShelleyAddress)
      [ "stake1uyehkck0lajq8gr28t9uxnuvgcqrc6070x3k9r8048z8y5gh6ffgw" -- type-14
      , "stake178phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtcccycj5" -- type-15
      ]

testnetPaymentKeyAddresses :: Spec Unit
testnetPaymentKeyAddresses =
  it "Testnet payment key addresses" do
    traverse_ (\addr -> shouldSatisfy addr validPaymentShelleyAddress)
      [ "addr_test1qz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgs68faae" -- type-00
      , "addr_test1yz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzerkr0vd4msrxnuwnccdxlhdjar77j6lg0wypcc9uar5d2shsf5r8qx" -- type-02
      , "addr_test1gz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer5pnz75xxcrdw5vky" -- type-04
      , "addr_test1vz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzerspjrlsz" -- type-06
      ]

testnetScriptKeyAddresses :: Spec Unit
testnetScriptKeyAddresses =
  it "Testnet script addresses" do
    traverse_ (\addr -> shouldNotSatisfy addr validPaymentShelleyAddress)
      [ "addr_test1zrphkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gten0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgsxj90mg" -- type-01
      , "addr_test1xrphkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gt7r0vd4msrxnuwnccdxlhdjar77j6lg0wypcc9uar5d2shs4p04xh" -- type-03
      , "addr_test12rphkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtupnz75xxcryqrvmw" -- type-05
      , "addr_test1wrphkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtcl6szpr" -- type-07
      ]

testnetStakeAddresses :: Spec Unit
testnetStakeAddresses =
  it "Testnet stake addresses" do
    traverse_ (\addr -> shouldNotSatisfy addr validPaymentShelleyAddress)
      [ "stake_test1uqehkck0lajq8gr28t9uxnuvgcqrc6070x3k9r8048z8y5gssrtvn" -- type-14
      , "stake_test17rphkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtcljw6kf" -- type-15
      ]
