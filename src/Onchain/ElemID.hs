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
import Plutarch.TermCont

import Plutarch.Api.V1 (PCurrencySymbol (..))
import Plutarch.Api.V1.Value (passertPositive)
import Plutarch.Api.V2 (
  PScriptContext,
  PScriptPurpose (..),
 )

import Onchain.Utils

-- NOTE: We can assume the protocol datum to be in the reference inputs here (if it's in the outputs, no ElemIDs should get minted)
mkElemIDMintingPolicy :: ClosedTerm (PCurrencySymbol :--> PUnit :--> PScriptContext :--> PUnit)
mkElemIDMintingPolicy = phoistAcyclic $ plam $ \protocolCS _ cxt -> unTermCont $ do
  PMinting elemIDCSRec <- pmatchC . pfromData $ pfield @"purpose" # cxt
  elemIDCS' <- pletC . pfromData $ pfield @"_0" # elemIDCSRec

  txInfo <- pletC $ pfield @"txInfo" # cxt

  fields <- pletFieldsC @["inputs", "referenceInputs", "outputs", "mint"] txInfo

  refInputsResolved <- pletC $ pmap # resolved # fields.referenceInputs

  Protocol elemIDMP setElemMP _ _ <- pmatchC $ txOutDatum @Protocol #$ findUnique # (hasCS # protocolCS) # refInputsResolved

  elemIDCS <- pletC $ scriptHashToCS # pfromData elemIDMP
  -- It just makes me feel better to verify it!
  pguardC "Own CS doesn't match protocol ElemID CS" $ elemIDCS #== elemIDCS'

  mint <- pletC $ pfromData fields.mint

  setElemCS <- pletC $ scriptHashToCS # pfromData setElemMP
  -- One SetElem token is minted in the same tx
  pguardC "SetElemID token minted" $ mintsExactly # 1 # setElemCS # emptyTN # mint

  -- TODO: Do we need to check the outputs? If someone mistakenly pays the ElemID token to the validator, is there any way to fix that?

  pure $ pcon PUnit
