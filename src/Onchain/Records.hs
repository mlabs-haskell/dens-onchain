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

import Onchain.Utils (
  findUnique,
  hasCS,
  paysTo,
  pguardM,
  pscriptHashAddress,
  resolved,
  scriptHashToCS,
  txOutDatum,
  txOutValue,
 )

{-
data RecordDatum = RecordDatum
    { class :: Word16
    , name  :: ByteString
    , reference :: Maybe ByteString
        -- ^ an Arweave address
    , owner :: PubKeyHash
}

-}

mkRecordValidator :: ClosedTerm (PCurrencySymbol :--> RecordDatum :--> PUnit :--> PScriptContext :--> PUnit)
mkRecordValidator = phoistAcyclic $ plam $ \protocolCS recordDatum _ cxt -> P.do
  txInfo <- plet $ pfield @"txInfo" # cxt

  fields <- pletFields @'["referenceInputs", "outputs", "mint", "inputs", "signatories"] txInfo

  referenceInputsResolved <- plet $ pmap # resolved # pfromData fields.referenceInputs

  Protocol elemIdMP setElemMP setValidator recordsValidator <-
    pmatch $
      txOutDatum @Protocol
        #$ findUnique
        # (hasCS # protocolCS)
        # referenceInputsResolved

  -- INPUTS: UTxO w/ ElemID Token

  elemIdCS <- plet $ scriptHashToCS # pfromData elemIdMP

  RecordDatum dClass dName dRef dOwner <- pmatch recordDatum

  densKey <- plet . pcon $ DensKey dName dClass

  recTokName <- plet $ pcon $ PTokenName (pblake2b_256 #$ pserialiseData # pdataImpl densKey)

  inputsResolved <- plet $ pmap # resolved # pfromData fields.inputs

  authTokenInInputs <- plet $ pany # plam (\x -> pvalueOf # (txOutValue # x) # elemIdCS # recTokName #== 1) # inputsResolved

  pguardM "Auth token in inputs" authTokenInInputs

  -- OUTPUTS: RecordDatum paid to RecordsValidator

  outs <- plet $ pfromData fields.outputs

  recValidatorAddr <- plet $ pscriptHashAddress # recordsValidator

  -- REVIEW: I'm assuming that there's only ever one output paid to the record validator. Not 100% sure that this should always be the case?
  --         If not, we need to find some other way to identify the TxOut that's supposed to contain the datum
  --         (`PTryFrom` doesn't give us a `PMaybe` wrapped result, but just crashes, so we can't just try to convert arbitrary
  --          datums in the outputs to find the "right" one)
  outRecDatum <- plet $ txOutDatum @RecordDatum #$ findUnique # (paysTo # recValidatorAddr) # outs

  pguardM "Output record datum == input record datum" $ outRecDatum #== recordDatum

  -- SIGNATURES: Tx is signed by owner
  pguardM "Signed by owner" $ pelem # dOwner # fields.signatories

  -- REVIEW: Do we need a check that nothing gets minted? Doesn't seem to matter.

  pcon PUnit
