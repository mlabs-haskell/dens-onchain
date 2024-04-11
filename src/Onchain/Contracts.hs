{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Onchain.Contracts (DeNSScripts (..), mkDeNSScripts) where

import Onchain.ElemID qualified as ElemID
import Onchain.PSMTypes (RecordDatum, SetInsert)
import Onchain.Protocol qualified as Protocol
import Onchain.Records qualified as Records
import Onchain.SetElem qualified as SetElem
import Onchain.Types qualified as T
import Plutarch (
  ClosedTerm,
  Config (..),
  PType,
  Term,
  TracingMode (..),
  pcon,
  phoistAcyclic,
  plam,
  popaque,
  (#),
  type (:-->),
 )

import PlutusLedgerApi.V2 (
  CurrencySymbol,
  TxOutRef,
 )

import Plutarch.Api.V2 (PMintingPolicy, PValidator)
import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Prelude (PData, PUnit (..), pconstant)
import Plutarch.TryFrom (PTryFrom, ptryFrom)
import Plutus.Model.V2 (TypedPolicy, TypedValidator, mkTypedPolicyPlutarch, mkTypedValidatorPlutarch, scriptCurrencySymbol)

mkSetValidator :: CurrencySymbol -> ClosedTerm PValidator
mkSetValidator pcs = phoistAcyclic $ plam $ \_ sInsert cxt ->
  popaque $ SetElem.mkSetValidator # pconstant pcs # pcon PUnit # T.ptryFromData @T.SetInsert sInsert # cxt

mkSetElemMintingPolicy :: CurrencySymbol -> ClosedTerm PMintingPolicy
mkSetElemMintingPolicy pcs = phoistAcyclic $ plam $ \sInsert cxt ->
  popaque $ SetElem.mkSetElemMintingPolicy # pconstant pcs # T.ptryFromData sInsert # cxt

mkElemIDMintingPolicy :: CurrencySymbol -> ClosedTerm PMintingPolicy
mkElemIDMintingPolicy pcs = phoistAcyclic $ plam $ \_ cxt ->
  popaque $ ElemID.mkElemIDMintingPolicy # pconstant pcs # pcon PUnit # cxt

mkProtocolMintingPolicy :: TxOutRef -> ClosedTerm PMintingPolicy
mkProtocolMintingPolicy outref = phoistAcyclic $ plam $ \_ cxt ->
  popaque $ Protocol.mkProtocolMintingPolicy # pconstant outref # pcon PUnit # cxt

mkRecordValidator :: CurrencySymbol -> ClosedTerm PValidator
mkRecordValidator pcs = phoistAcyclic $ plam $ \recDatum _ cxt ->
  popaque $ Records.mkRecordValidator # pconstant pcs # T.ptryFromData recDatum # pcon PUnit # cxt

data DeNSScripts = DeNSScripts
  { setValidator :: TypedValidator () SetInsert
  , recordValidator :: TypedValidator RecordDatum ()
  , setElemMintingPolicy :: TypedPolicy SetInsert
  , elemIDMintingPolicy :: TypedPolicy ()
  , protocolMintingPolicy :: TypedPolicy ()
  }

myConfig :: Config
myConfig = Config DoTracing

mkDeNSScripts :: TxOutRef -> DeNSScripts
mkDeNSScripts outRef =
  DeNSScripts
    { setValidator = setValidator'
    , recordValidator = recordValidator'
    , setElemMintingPolicy = setElemMintingPolicy'
    , elemIDMintingPolicy = elemIDMintingPolicy'
    , protocolMintingPolicy = protocolMintingPolicy'
    }
  where
    unsafeFromRight :: forall l r. (Show l) => Either l r -> r
    unsafeFromRight = \case
      Left x -> error ("unsafeFromRight: " <> show x)
      Right res -> res

    makeV :: forall d r. ClosedTerm PValidator -> TypedValidator d r
    makeV v = unsafeFromRight $ mkTypedValidatorPlutarch @d @r myConfig v

    makeP :: forall t. ClosedTerm PMintingPolicy -> TypedPolicy t
    makeP p = unsafeFromRight $ mkTypedPolicyPlutarch @t myConfig p

    protocolMintingPolicy' = makeP $ mkProtocolMintingPolicy outRef
    protocolCS = scriptCurrencySymbol $ protocolMintingPolicy'
    setValidator' = makeV $ mkSetValidator protocolCS
    recordValidator' = makeV $ mkRecordValidator protocolCS
    setElemMintingPolicy' = makeP $ mkSetElemMintingPolicy protocolCS
    elemIDMintingPolicy' = makeP $ mkElemIDMintingPolicy protocolCS
