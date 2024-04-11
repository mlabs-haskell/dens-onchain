-- for hls to not yell at me
{-# LANGUAGE BlockArguments #-}
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
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Onchain.Protocol where

import Plutarch.Prelude (
  ClosedTerm,
  PBuiltinList,
  PEq ((#==)),
  PUnit (..),
  pany,
  pcon,
  pfield,
  pfromData,
  phoistAcyclic,
  plam,
  (#),
  (:-->),
 )
import Plutarch.TermCont

import Plutarch.Api.V2 (
  PScriptContext,
  PScriptPurpose (..),
 )
import Plutarch.Api.V2.Tx (
  PTxOutRef,
 )

import Onchain.Utils (
  emptyTN,
  mintsExactly,
  pguardC,
  pletC,
  pletFieldsC,
  pmatchC,
 )

mkProtocolMintingPolicy :: ClosedTerm (PTxOutRef :--> PUnit :--> PScriptContext :--> PUnit)
mkProtocolMintingPolicy = phoistAcyclic $ plam $ \outRef _ cxt -> unTermCont $ do
  PMinting protocolCSRec <- pmatchC . pfromData $ pfield @"purpose" # cxt
  protocolCS <- pletC . pfromData $ pfield @"_0" # protocolCSRec

  info <- pletC $ pfield @"txInfo" # cxt

  fields <- pletFieldsC @["inputs", "mint"] info

  -- check that we have an input w/ the appropriate outref
  inputExistsWithOutRef <-
    pletC $
      pany @PBuiltinList
        # plam (\out -> pfromData (pfield @"outRef" # out) #== outRef)
        # fields.inputs
  pguardC "Tx has input with known outref (for one-shot MP)" inputExistsWithOutRef

  -- check that exactly one protocol token is minted
  pguardC "Exactly one protocol NFT Minted" $ mintsExactly # 1 # protocolCS # emptyTN # fields.mint

  -- TODO: Do we need to check *here* for the Protocol Datum in the outputs? Doesn't seem necessary given setElem MP logic
  pure $ pcon PUnit
