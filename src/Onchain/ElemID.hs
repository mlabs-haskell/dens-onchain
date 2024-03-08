{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- for hls to not yell at me
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Onchain.ElemID where

import Onchain.Types (
  Protocol (..),
 )

-- import LambdaBuffers.Plutus.V1.Plutarch (Bytes)
-- import LambdaBuffers.Prelude.Plutarch qualified as Lb.Plutarch
-- import LambdaBuffers.Runtime.Plutarch (PList (PList))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude (
  ClosedTerm,
  PEq ((#==)),
  PUnit (..),
  pcon,
  pfield,
  pfromData,
  phoistAcyclic,
  plam,
  plet,
  pletFields,
  pmap,
  pmatch,
  (#),
  (#$),
  (:-->),
 )

import Plutarch.Api.V1 (PCurrencySymbol (..))
import Plutarch.Api.V1.Value (passertPositive)
import Plutarch.Api.V2 (
  PScriptContext,
  PScriptPurpose (..),
 )

import Onchain.Utils (
  emptyTN,
  findUnique,
  hasCS,
  mintsExactly,
  pguardM,
  resolved,
  scriptHashToCS,
  txOutDatum,
 )

-- NOTE: We can assume the protocol datum to be in the reference inputs here (if it's in the outputs, no ElemIDs should get minted)
mkElemIDMintingPolicy :: ClosedTerm (PCurrencySymbol :--> PUnit :--> PScriptContext :--> PUnit)
mkElemIDMintingPolicy = phoistAcyclic $ plam $ \protocolCS _ cxt -> P.do
  PMinting elemIDCSRec <- pmatch . pfromData $ pfield @"purpose" # cxt
  elemIDCS' <- plet . pfromData $ pfield @"_0" # elemIDCSRec

  txInfo <- plet $ pfield @"txInfo" # cxt

  fields <- pletFields @["inputs", "referenceInputs", "outputs", "mint"] txInfo

  refInputsResolved <- plet $ pmap # resolved # fields.referenceInputs

  Protocol elemIDMP setElemMP _ _ <- pmatch $ txOutDatum @Protocol #$ findUnique # (hasCS # protocolCS) # refInputsResolved

  elemIDCS <- plet $ scriptHashToCS # pfromData elemIDMP
  -- It just makes me feel better to verify it!
  pguardM "Own CS doesn't match protocol ElemID CS" $ elemIDCS #== elemIDCS'

  mint <- plet $ passertPositive # pfromData fields.mint

  setElemCS <- plet $ scriptHashToCS # pfromData setElemMP
  -- One SetElem token is minted in the same tx
  pguardM "SetElemID token minted" $ mintsExactly # 1 # setElemCS # emptyTN # mint

  -- TODO: Do we need to check the outputs? If someone mistakenly pays the ElemID token to the validator, is there any way to fix that?

  pcon PUnit
