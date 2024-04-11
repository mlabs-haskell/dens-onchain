{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Onchain.Types (
  DensKey (..),
  DensValue (..),
  Protocol (..),
  RecordDatum (..),
  SetDatum (..),
  SetInsert (..),
  PMaybe (..),
  ptryFromData,
) where

import qualified GHC.Generics
import Plutarch.Internal (Term, pdelay, pforce)

import qualified Plutarch
import Plutarch.Api.V2 (
  PCurrencySymbol,
  PMaybeData,
  PPubKeyHash,
  PScriptHash,
 )
import Plutarch.Builtin (
  PAsData,
  PBuiltinList (..),
  PData,
  PIsData,
  pdata,
  pforgetData,
  pfromData,
 )
import Plutarch.Internal.PlutusType (
  PlutusType (pcon', pmatch'),
  pcon,
  pmatch,
 )
import Plutarch.Prelude (
  PByteString,
  PEq,
  PInteger,
  PMaybe (..),
  PTryFrom,
  PlutusType (..),
  ptraceError,
  ptryFrom,
 )

ptryFromData :: forall x s. (PTryFrom PData (PAsData x), PIsData x) => Term s PData -> Term s x
ptryFromData t = pfromData $ ptryFrom @(PAsData x) t fst

data DensKey (s :: Plutarch.S)
  = DensKey
      (Plutarch.Term s (PAsData PByteString))
      (Plutarch.Term s (PAsData PInteger))
  deriving stock (GHC.Generics.Generic)
  deriving anyclass (PEq)

instance PlutusType DensKey where
  type PInner DensKey = PBuiltinList PData
  pcon' (DensKey t1 t2) = pcon $ PCons (pforgetData t1) $ pcon $ PCons (pforgetData t2) $ pcon PNil
  pmatch' xs f = pforce $ pmatch xs \case
    PCons bs rest -> pmatch rest \case
      PCons i _ -> pdelay $ f $ DensKey (ptryFrom bs fst) (ptryFrom i fst)
      _ -> pdelay $ ptraceError "PMatch DensKey Fail"
    _ -> pdelay $ ptraceError "PMatch DensKey Fail"

instance PIsData DensKey
instance PTryFrom PData (PAsData DensKey)

newtype DensValue (s :: Plutarch.S) = DensValue (Plutarch.Term s (PAsData (PMaybeData (PAsData PByteString))))
  deriving stock (GHC.Generics.Generic)
  deriving anyclass (PEq)
instance PIsData DensValue
instance PTryFrom PData (PAsData DensValue)

instance PlutusType DensValue where
  type PInner DensValue = PBuiltinList PData
  pcon' (DensValue mbs) = pcon $ PCons (pforgetData mbs) $ pcon PNil
  pmatch' xs f = pforce $ pmatch xs $ \case
    PCons md _ -> pdelay $ f $ DensValue (pdata $ ptryFrom md fst)
    _ -> pdelay $ ptraceError "PMatch DensValue fail"

pbfromList :: [Term s PData] -> Term s (PBuiltinList PData)
pbfromList = foldr go (pcon PNil)
  where
    go :: Term s PData -> Term s (PBuiltinList PData) -> Term s (PBuiltinList PData)
    go x xs = pcon $ PCons x xs

data Protocol (s :: Plutarch.S)
  = Protocol
      (Plutarch.Term s (PAsData PScriptHash))
      (Plutarch.Term s (PAsData PScriptHash))
      (Plutarch.Term s (PAsData PScriptHash))
      (Plutarch.Term s (PAsData PScriptHash))
  deriving stock (GHC.Generics.Generic)

instance PIsData Protocol
instance PTryFrom PData (PAsData Protocol)

instance PlutusType Protocol where
  type PInner Protocol = PBuiltinList PData
  pcon' (Protocol a b c d) = pbfromList $ pforgetData <$> [a, b, c, d]
  pmatch' xs f = pforce $ pmatch xs \case
    PCons a rest -> pmatch rest \case
      PCons b rest' -> pmatch rest' \case
        PCons c rest'' -> pmatch rest'' \case
          PCons d _ ->
            pdelay $
              f $
                Protocol
                  (ptryFrom a fst)
                  (ptryFrom b fst)
                  (ptryFrom c fst)
                  (ptryFrom d fst)
          _ -> pdelay $ ptraceError "Pmatch Protocol fail"
        _ -> pdelay $ ptraceError "Pmatch Protocol fail"
      _ -> pdelay $ ptraceError "Pmatch Protocol fail"
    _ -> pdelay $ ptraceError "Pmatch Protocol fail"

data RecordDatum (s :: Plutarch.S)
  = RecordDatum
      (Plutarch.Term s (PAsData PInteger))
      (Plutarch.Term s (PAsData PByteString))
      (Plutarch.Term s (PAsData DensValue))
      (Plutarch.Term s (PAsData PPubKeyHash))
  deriving stock (GHC.Generics.Generic)
  deriving anyclass (PEq)

instance PIsData RecordDatum
instance PTryFrom PData (PAsData RecordDatum)

cons :: Term s (PAsData x) -> Term s (PBuiltinList PData) -> Term s (PBuiltinList PData)
cons x xs = pcon $ PCons (pforgetData x) xs

nil :: Term s (PBuiltinList PData)
nil = pcon PNil

instance PlutusType RecordDatum where
  type PInner RecordDatum = PBuiltinList PData
  pcon' (RecordDatum a b c d) = cons a $ cons b $ cons c $ cons d nil
  pmatch' xs f = pforce $ pmatch xs $ \case
    PCons a rest -> pmatch rest \case
      PCons b rest' -> pmatch rest' \case
        PCons c rest'' -> pmatch rest'' \case
          PCons d _ ->
            pdelay $
              f $
                RecordDatum
                  (ptryFrom a fst)
                  (ptryFrom b fst)
                  (ptryFrom c fst) -- TODO write tryfrom instance
                  (ptryFrom d fst)
          _ -> pdelay $ ptraceError "Pmatch RecordDatum fail"
        _ -> pdelay $ ptraceError "Pmatch RecordDatum fail"
      _ -> pdelay $ ptraceError "Pmatch RecordDatum fail"
    _ -> pdelay $ ptraceError "Pmatch RecordDatum fail"

data SetDatum (s :: Plutarch.S)
  = SetDatum
      (Plutarch.Term s (PAsData DensKey))
      (Plutarch.Term s (PAsData DensKey))
      (Plutarch.Term s (PAsData PCurrencySymbol))
  deriving stock (GHC.Generics.Generic)

instance PIsData SetDatum
instance PTryFrom PData (PAsData SetDatum)

instance PlutusType SetDatum where
  type PInner SetDatum = PBuiltinList PData
  pcon' (SetDatum a b c) = cons a $ cons b $ cons c nil
  pmatch' xs f = pforce $ pmatch xs $ \case
    PCons a rest -> pmatch rest $ \case
      PCons b rest' -> pmatch rest' $ \case
        PCons c _ ->
          pdelay $
            f $
              SetDatum
                (ptryFrom a fst) -- TODO write tryfrom instance
                (ptryFrom b fst) -- TODO write tryfrom instance
                (ptryFrom c fst) -- TODO write tryfrom instance
        _ -> pdelay $ ptraceError "Pmatch SetDatum fail"
      _ -> pdelay $ ptraceError "Pmatch SetDatum fail"
    _ -> pdelay $ ptraceError "Pmatch SetDatum fail"

data SetInsert (s :: Plutarch.S) = SetInsert'Insert (Plutarch.Term s (PAsData DensKey))
  deriving stock (GHC.Generics.Generic)
  deriving anyclass (PIsData, PEq)

instance PTryFrom PData (PAsData SetInsert)

instance PlutusType SetInsert where
  type PInner SetInsert = PBuiltinList PData
  pcon' (SetInsert'Insert a) = cons a nil
  pmatch' xs f = pforce $ pmatch xs $ \case
    PCons a _ -> pdelay $ f $ SetInsert'Insert (ptryFrom a fst)
    _ -> pdelay $ ptraceError "Pmatch SetInsert fail"
