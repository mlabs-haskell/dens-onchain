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

module Onchain.Records where

import Onchain.Types (
  DensKey (..),
  Protocol (..),
  RecordDatum (..),
 )
import Plutarch.Api.V1 (PCurrencySymbol (..), PTokenName (..))
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V2 (
  PScriptContext,
 )

import Plutarch.Builtin (PIsData (pdataImpl), pserialiseData)
import Plutarch.Crypto (pblake2b_256)
import Plutarch.List (pany, pelem)
import Plutarch.Prelude (
  ClosedTerm,
  PEq ((#==)),
  PUnit (..),
  pcon,
  pfield,
  pfromData,
  phoistAcyclic,
  plam,
  pmap,
  (#),
  (#$),
  (:-->),
 )
import Plutarch.TermCont

import Onchain.Utils (
  findUnique,
  hasCS,
  paysTo,
  pguardC,
  pletC,
  pletFieldsC,
  pmatchC,
  pscriptHashAddress,
  resolved,
  scriptHashToCS,
  txOutDatum,
  txOutValue,
 )

mkRecordValidator :: ClosedTerm (PCurrencySymbol :--> RecordDatum :--> PUnit :--> PScriptContext :--> PUnit)
mkRecordValidator = phoistAcyclic $ plam $ \protocolCS recordDatum _ cxt -> unTermCont $ do
  txInfo <- pletC $ pfield @"txInfo" # cxt

  fields <- pletFieldsC @'["referenceInputs", "outputs", "mint", "inputs", "signatories"] txInfo

  referenceInputsResolved <- pletC $ pmap # resolved # pfromData fields.referenceInputs

  Protocol elemIdMP _ _ recordsValidator <-
    pmatchC $
      txOutDatum @Protocol
        #$ findUnique
        # (hasCS # protocolCS)
        # referenceInputsResolved

  -- INPUTS: UTxO w/ ElemID Token

  elemIdCS <- pletC $ scriptHashToCS # pfromData elemIdMP

  RecordDatum dClass dName _ dOwner <- pmatchC recordDatum

  densKey <- pletC . pcon $ DensKey dName dClass

  recTokName <- pletC $ pcon $ PTokenName (pblake2b_256 #$ pserialiseData # pdataImpl densKey)

  inputsResolved <- pletC $ pmap # resolved # pfromData fields.inputs

  authTokenInInputs <- pletC $ pany # plam (\x -> pvalueOf # (txOutValue # x) # elemIdCS # recTokName #== 1) # inputsResolved

  pguardC "Auth token in inputs" authTokenInInputs

  -- OUTPUTS: RecordDatum paid to RecordsValidator

  outs <- pletC $ pfromData fields.outputs

  recValidatorAddr <- pletC $ pscriptHashAddress # recordsValidator

  -- REVIEW: I'm assuming that there's only ever one output paid to the record validator. Not 100% sure that this should always be the case?
  --         If not, we need to find some other way to identify the TxOut that's supposed to contain the datum
  --         (`PTryFrom` doesn't give us a `PMaybe` wrapped result, but just crashes, so we can't just try to convert arbitrary
  --          datums in the outputs to find the "right" one)
  outRecDatum <- pletC $ txOutDatum @RecordDatum #$ findUnique # (paysTo # recValidatorAddr) # outs

  pguardC "Output record datum == input record datum" $ outRecDatum #== recordDatum

  -- SIGNATURES: Tx is signed by owner
  pguardC "Signed by owner" $ pelem # dOwner # fields.signatories

  -- REVIEW: Do we need a check that nothing gets minted? Doesn't seem to matter.

  pure $ pcon PUnit
