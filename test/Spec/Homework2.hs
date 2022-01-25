{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Spec.Homework2
  (
    main,
    tests,
    runMyTrace  ) where

import Control.Lens
import Control.Monad hiding (fmap)
import Data.Default (Default (..))
import qualified Data.Map as Map
import Data.Monoid (Last (..))
import Data.Text (Text)
import Ledger
import Ledger.Ada as Ada
import Ledger.CardanoWallet as CW
import Ledger.TimeSlot
import Ledger.Value as Value
import Plutus.Contract as Contract
import Plutus.Trace (callEndpoint)
import qualified Plutus.Trace as Emulator
import Plutus.Trace.Emulator as Emulator
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import qualified Wallet.Emulator as Wallet
import Wallet.Emulator.Wallet
import Prelude (IO, Semigroup (..), Show (..))
import PlutusTx.Prelude (trace)
import Type.Reflection (typeOf)
import Plutus.Contract.Test
import Plutus.Trace.Emulator.Types
import Test.Tasty

import Week02.Homework2

wallets :: [Wallet]
wallets = toMockWallet <$> CW.knownMockWallets

testsGoodRedeemer :: TestTree
testsGoodRedeemer = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "give and grab"
    (     walletFundsChange (wallets !! 1) (Ada.lovelaceValueOf  (- 5_000_000))
     .&&. walletFundsChange (wallets !! 2) (Ada.lovelaceValueOf 5_000_000)
    )
    myTrace
      where 
        v :: Value
        v = Ada.lovelaceValueOf 10_000_000
        owMap :: Map.Map Wallet Value
        owMap = Map.fromList $ (,v) <$> tail wallets
        emCfg :: EmulatorConfig
        emCfg = EmulatorConfig (Left owMap) def def

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Redeemer testing"
    [ testsGoodRedeemer
      , testInvalidRedeemer
    ]

runMyTrace :: IO ()
runMyTrace = runEmulatorTraceIO' def emCfg myTraceInvalidRedeemer
  where
    v :: Value
    v = Ada.lovelaceValueOf 10_000_000

    owMap :: Map.Map Wallet Value
    owMap = Map.fromList $ (,v) <$> tail wallets

    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig (Left owMap) def def


myTrace :: EmulatorTrace ()
myTrace = do
  h1 <- activateContractWallet (wallets !! 1) endpoints
  h2 <- activateContractWallet (wallets !! 2) endpoints

  callEndpoint @"give" h1 5_000_000

  void $ Emulator.waitNSlots 1

  callEndpoint @"grab" h2 $
      Week02.Homework2.MyRedeemer
      { 
        flag1 = True,
        flag2 = True
      }

  void $ Emulator.waitNSlots 1


testInvalidRedeemer :: TestTree
testInvalidRedeemer = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "give and grab invalid redeemer"
    (     walletFundsChange (wallets !! 1) (Ada.lovelaceValueOf  (- 5_000_000))
     .&&. walletFundsChange (wallets !! 2) (Ada.lovelaceValueOf 0_000_000)
    )
    myTraceInvalidRedeemer
      where 
        v :: Value
        v = Ada.lovelaceValueOf 10_000_000
        owMap :: Map.Map Wallet Value
        owMap = Map.fromList $ (,v) <$> tail wallets
        emCfg :: EmulatorConfig
        emCfg = EmulatorConfig (Left owMap) def def

myTraceInvalidRedeemer :: EmulatorTrace ()
myTraceInvalidRedeemer = do
  h1 <- activateContractWallet (wallets !! 1) endpoints
  h2 <- activateContractWallet (wallets !! 2) endpoints

  callEndpoint @"give" h1 5_000_000

  void $ Emulator.waitNSlots 1

  callEndpoint @"grab" h2 $
      Week02.Homework2.MyRedeemer
      { 
        flag1 = True,
        flag2 = False
      }

  void $ Emulator.waitNSlots 1