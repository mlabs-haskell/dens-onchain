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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Onchain.Contracts (DeNSScripts (..), mkDeNSScripts) where

import Data.Default (Default (def))

import Onchain.ElemID qualified as ElemID
import Onchain.PSMTypes (RecordDatum, SetInsert)
import Onchain.Protocol qualified as Protocol
import Onchain.Records qualified as Records
import Onchain.SetElem qualified as SetElem
import Plutarch (
  ClosedTerm,
  PType,
  Term,
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

class PTryFromWithoutStupidSubtypeBullshit (b :: PType) where
  ptryFrom_ :: forall s. Term s PData -> Term s b

instance PTryFromWithoutStupidSubtypeBullshit PUnit where
  ptryFrom_ _ = pcon PUnit

instance {-# OVERLAPS #-} PTryFrom PData b => PTryFromWithoutStupidSubtypeBullshit b where
  ptryFrom_ d = ptryFrom d fst

type C x = PTryFromWithoutStupidSubtypeBullshit x

plutarchValidator ::
  forall (a :: PType) (b :: PType) (c :: PType).
  (C a, C b) =>
  ClosedTerm (a :--> b :--> PScriptContext :--> c) ->
  ClosedTerm PValidator
plutarchValidator v = phoistAcyclic $ plam $ \aData bData cxt ->
  popaque $ v # ptryFrom_ aData # ptryFrom_ bData # cxt

plutarchMintingPolicy ::
  forall (a :: PType) (c :: PType).
  C a =>
  ClosedTerm (a :--> PScriptContext :--> c) ->
  ClosedTerm PMintingPolicy
plutarchMintingPolicy v = phoistAcyclic $ plam $ \aData cxt ->
  popaque $ v # ptryFrom_ aData # cxt

mkSetValidator :: CurrencySymbol -> ClosedTerm PValidator
mkSetValidator pcs = plutarchValidator $ SetElem.mkSetValidator # pconstant pcs

mkSetElemMintingPolicy :: CurrencySymbol -> ClosedTerm PMintingPolicy
mkSetElemMintingPolicy pcs =
  plutarchMintingPolicy $
    SetElem.mkSetElemMintingPolicy # pconstant pcs

mkElemIDMintingPolicy :: CurrencySymbol -> ClosedTerm PMintingPolicy
mkElemIDMintingPolicy pcs =
  plutarchMintingPolicy $
    ElemID.mkElemIDMintingPolicy # pconstant pcs

mkProtocolMintingPolicy :: TxOutRef -> ClosedTerm PMintingPolicy
mkProtocolMintingPolicy outref =
  plutarchMintingPolicy $
    Protocol.mkProtocolMintingPolicy # pconstant outref

mkRecordValidator :: CurrencySymbol -> ClosedTerm PValidator
mkRecordValidator pcs =
  plutarchValidator $
    Records.mkRecordValidator # pconstant pcs

data DeNSScripts = DeNSScripts
  { setValidator :: TypedValidator () SetInsert
  , recordValidator :: TypedValidator RecordDatum ()
  , setElemMintingPolicy :: TypedPolicy SetInsert
  , elemIDMintingPolicy :: TypedPolicy ()
  , protocolMintingPolicy :: TypedPolicy ()
  }

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
    makeV v = unsafeFromRight $ mkTypedValidatorPlutarch @d @r def v

    makeP :: forall t. ClosedTerm PMintingPolicy -> TypedPolicy t
    makeP p = unsafeFromRight $ mkTypedPolicyPlutarch @t def p

    protocolMintingPolicy' = makeP $ mkProtocolMintingPolicy outRef
    protocolCS = scriptCurrencySymbol $ protocolMintingPolicy'
    setValidator' = makeV $ mkSetValidator protocolCS
    recordValidator' = makeV $ mkRecordValidator protocolCS
    setElemMintingPolicy' = makeP $ mkSetElemMintingPolicy protocolCS
    elemIDMintingPolicy' = makeP $ mkElemIDMintingPolicy protocolCS
