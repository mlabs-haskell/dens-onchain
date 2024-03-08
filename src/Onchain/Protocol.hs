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

-- import LambdaBuffers.Plutus.V1.Plutarch (Bytes)
-- import LambdaBuffers.Prelude.Plutarch qualified as Lb.Plutarch
-- import LambdaBuffers.Runtime.Plutarch (PList (PList))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude (
  ClosedTerm,
  PBuiltinList,
  PEq ((#==)),
  POpaque,
  PUnit (..),
  pany,
  pcon,
  pfield,
  pfromData,
  phoistAcyclic,
  plam,
  plet,
  pletFields,
  pmatch,
  (#),
  (:-->),
 )

import Plutarch.Api.V2 (
  PScriptContext,
  PScriptPurpose (..),
 )
import Plutarch.Api.V2.Tx (
  PTxOutRef,
 )

import Onchain.Utils

mkProtocolMintingPolicy :: ClosedTerm (PTxOutRef :--> PUnit :--> PScriptContext :--> PUnit)
mkProtocolMintingPolicy = phoistAcyclic $ plam $ \outRef _ cxt -> P.do
  PMinting protocolCSRec <- pmatch . pfromData $ pfield @"purpose" # cxt
  protocolCS <- plet . pfromData $ pfield @"_0" # protocolCSRec

  info <- plet $ pfield @"txInfo" # cxt

  fields <- pletFields @["inputs", "mint"] info

  -- check that we have an input w/ the appropriate outref
  inputExistsWithOutRef <-
    plet $
      pany @PBuiltinList
        # plam (\out -> pfromData (pfield @"outRef" # out) #== outRef)
        # fields.inputs
  pguardM "Tx has input with known outref (for one-shot MP)" inputExistsWithOutRef

  -- check that exactly one protocol token is minted
  pguardM "Exactly one protocol NFT Minted" $ mintsExactly # 1 # protocolCS # emptyTN # fields.mint

  -- TODO: Do we need to check *here* for the Protocol Datum in the outputs? Doesn't seem necessary given setElem MP logic
  pcon PUnit
