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

module Onchain.SetElem (mkSetValidator, mkSetElemMintingPolicy, densNameMin', densNameMax') where

import Onchain.Types (
  DensKey (..),
  Protocol (..),
  SetDatum (..),
  SetInsert (..),
 )

-- import LambdaBuffers.Plutus.V1.Plutarch (Bytes)
-- import LambdaBuffers.Prelude.Plutarch qualified as Lb.Plutarch
-- import LambdaBuffers.Runtime.Plutarch (PList (PList))
import Plutarch (Term)
import Plutarch.Prelude (
  ClosedTerm,
  PBool (..),
  PBuiltinList,
  PByteString,
  PEq ((#==)),
  PMaybe (..),
  PPartialOrd ((#<)),
  PUnit (..),
  pcon,
  pconstant,
  pfield,
  pfilter,
  pfromData,
  phoistAcyclic,
  pif,
  plam,
  plength,
  pmap,
  (#),
  (#$),
  (#&&),
  (:-->),
 )

import Plutarch.Api.V1 (PCurrencySymbol (..), PTokenName (..))
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V2 (
  PScriptContext,
  PTxOut (..),
 )
import Plutarch.Builtin (PIsData (pdataImpl), pserialiseData)
import Plutarch.Crypto (pblake2b_256)
import Plutarch.List (PListLike (..), pall)

import Plutarch.TermCont

import Onchain.Utils (
  emptyTN,
  extractDatum,
  extractDatumSuchThat,
  findOutWithCS,
  findUnique,
  hasCS,
  hasLR,
  lacksCS,
  mintsExactly,
  pguardC,
  pletC,
  pletFieldsC,
  pmatchC,
  pownCurrencySymbol,
  pscriptHashAddress,
  ptraceC,
  resolved,
  scriptHashToCS,
  totalMint,
  txOutDatum,
  valLacksCS,
 )
import Plutarch.Maybe (pfromJust)

import Data.ByteString

densNameMin :: ClosedTerm PByteString
densNameMin = pconstant densNameMin'

densNameMin' :: ByteString
densNameMin' = ""

-- placeholder
densNameMax :: ClosedTerm PByteString
densNameMax = pconstant densNameMax'

densNameMax' :: ByteString
densNameMax' = "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"

-- ignores the currency symbol for the extension for now
isInitialSetDatum :: ClosedTerm (SetDatum :--> PBool)
isInitialSetDatum = phoistAcyclic $ plam $ \setDatum -> unTermCont $ do
  SetDatum l r _ <- pmatchC setDatum
  DensKey lname lclass <- pmatchC . pfromData $ l
  DensKey rname rclass <- pmatchC . pfromData $ r
  leftIsMin <- pletC $ pfromData lname #== densNameMin
  rightIsMax <- pletC $ pfromData rname #== densNameMax
  classesMatch <- pletC $ lclass #== rclass
  pure $ leftIsMin #&& rightIsMax #&& classesMatch

mkSetValidator ::
  Term
    s
    ( PCurrencySymbol
        :--> PUnit
        :--> SetInsert
        :--> PScriptContext
        :--> PUnit
    )
mkSetValidator = phoistAcyclic $ plam $ \protocolCS _ _ cxt -> unTermCont $ do
  ptraceC "mkSetValidator a"
  -- We only need to check whether the setElem token is minted, all the real validation logic is in that MP
  txInfo <- pletC $ pfield @"txInfo" # cxt
  ptraceC "mkSetValidator b"
  fields <- pletFieldsC @'["referenceInputs", "outputs", "mint"] txInfo
  ptraceC "mkSetValidator c"
  resolvedOutputs <- pletC $ pfromData fields.outputs
  ptraceC "mkSetValidator d"
  resolvedRefInputs <- pletC $ pmap # resolved # pfromData fields.referenceInputs
  ptraceC "mkSetValidator e"
  -- NOTE/FIXME/TODO: I am NOT 100% sure that this is safe, but I do not know another way to get the setElemID
  --                  Someone needs to carefully review. It *seems* safe if the protocol NFT MP is a one shot policy
  Protocol _ setElemMP _ _ <- pmatchC . unTermCont $ do
    inOutputs <- pmatchC $ findOutWithCS # protocolCS # resolvedOutputs
    case inOutputs of
      PNothing ->
        pure $
          txOutDatum @Protocol
            #$ pfromJust
            #$ findOutWithCS
            # protocolCS
            # resolvedRefInputs
      PJust outRef -> pure $ txOutDatum @Protocol # outRef
  ptraceC "mkSetValidator f"
  setElemIdCS <- pletC $ scriptHashToCS # pfromData setElemMP
  ptraceC "mkSetValidator g"
  pguardC "SetElemID token minted" ((pvalueOf # fields.mint # setElemIdCS # emptyTN) #== 1)
  ptraceC "mkSetValidator h"
  pure $ pcon PUnit

mkSetElemMintingPolicy ::
  Term
    s
    ( PCurrencySymbol -- Protocol NFT (assume empty token name for now)
        :--> SetInsert
        :--> PScriptContext
        :--> PUnit
    )
mkSetElemMintingPolicy = phoistAcyclic $ plam $ \protocolSymb setInsert cxt -> unTermCont $ do
  ptraceC "setElemA"
  SetInsert'Insert keyToInsert <- pmatchC setInsert
  ptraceC "setElemB"
  txInfo <- pletC $ pfield @"txInfo" # cxt
  ptraceC "setElemC"
  fields <- pletFieldsC @'["inputs", "referenceInputs", "outputs", "mint"] txInfo
  ptraceC "setElemD"
  setElemCS <- pletC $ pownCurrencySymbol # cxt
  ptraceC "setElemE"
  outputs <- pletC $ pfromData fields.outputs
  ptraceC "setElemF"
  mint <- pletC $ pfromData fields.mint
  ptraceC "setElemG"
  isInitalization <- pletC $ initialSetDatumInOutputs # setElemCS # outputs
  ptraceC "setElemH"
  result <-
    pletC $
      pif
        isInitalization
        ( unTermCont $ do
            ptraceC "setElemN"
            -- Checks for an initialize transaction. We already know a SetDatum w/ the initialization properties is in an output w/ a SetElem NFT

            -- Is the protocol NFT minted in this TX? NOTE: Protocol MP must be one shot!
            pguardC "Protocol NFT Minted" (pvalueOf # mint # protocolSymb # emptyTN #== 1)
            ptraceC "setElemO"
            -- Is there a protocol datum in the output that contains the protocol NFT? (also we need the fields here)
            Protocol elemIdMP setElemMP _ _ <- pmatchC $ findProtocolInOutputs # protocolSymb # outputs
            ptraceC "setElemP"
            -- Probably redundant, but it can't hurt to make sure that this minting policy is the one indicated in the Protocol datum
            pguardC "Own CS doesn't match protocol SetElem CS" $ (scriptHashToCS # pfromData setElemMP) #== setElemCS
            ptraceC "setElemQ"
             -- Are no element id tokens minted? (We check the output and mints for redundancy, maybe not necessary)
            elemIdCS <- pletC $ scriptHashToCS # pfromData elemIdMP
            ptraceC "setElemR"
            noElemIDInOutputs <- pletC $ pall # (lacksCS # elemIdCS) # outputs
            ptraceC "setElemS"
            noElemIDInMint <- pletC $ valLacksCS # elemIdCS # mint
            ptraceC "setElemT"
            pguardC "No ElemID NFT minted or in output" $ noElemIDInMint #&& noElemIDInOutputs
            ptraceC "setElemU"
            pure $ pcon PUnit
        )
        ( unTermCont $ do
            ptraceC "setElemI"
            -- 0) REFERENCE INPUTS: Get the protocol datum (reference input, paid to protocol validator)

            Protocol elemIdMP _ setValidator _ <-
              pmatchC $
                txOutDatum @(Protocol)
                  #$ findUnique
                  # (hasCS # protocolSymb)
                  #$ pmap
                  # resolved
                  # fields.referenceInputs
            ptraceC "setElemJ"
            -- 1) INPUTS: Check for k < densKey < nxt input that pays to the set validator & holds a setDatum
            resolvedInputs <- pletC $ pmap # resolved # pfromData fields.inputs
            ptraceC "setElemK"
            setValidatorAddress <- pletC $ pscriptHashAddress # setValidator
            ptraceC "setElemL"
            setDatum <- pletC $ extractDatum @SetDatum # setValidatorAddress # setElemCS #$ resolvedInputs -- ignoring OwnerApproval for now
            ptraceC "setElemM"
            SetDatum l r _ <- pmatchC setDatum
            ptraceC "setElemM.2"
            pguardC "Validate set insert" $ validateSetInsert # setDatum # pfromData keyToInsert
            ptraceC "setElemU"

            -- 2) OUTPUTS: Check for a. SD(k, densKey) b. SD(densKey,nxt)

            l' <- pletC $ pfromData l
            r' <- pletC $ pfromData r
            k <- pletC $ pfromData keyToInsert
            ptraceC "setElemV"
            checkOutput <- pletC $ plam $ \p -> extractDatumSuchThat @SetDatum # p # setValidatorAddress # setElemCS # outputs
            ptraceC "setElemW"
            SetDatum {} <- pmatchC $ checkOutput # (hasLR # l' # k)
            SetDatum {} <- pmatchC $ checkOutput # (hasLR # k # r')
            ptraceC "setElemX"
            -- 3) MINTS: Check for the presence of a SetElemID NFT & ElementID NFT

            -- This, plus the two subsequent checks, implies that *only* the SetElem and ElemID tokens are minted
            pguardC "Only two CS minted " $ totalMint # mint #== 2

            checkMintsOne <- pletC $ plam $ \currSym tokName -> mintsExactly # 1 # currSym # tokName # mint
            ptraceC "setElemY"
            pguardC "Mints one SetElemID token" $ checkMintsOne # setElemCS # emptyTN

            elemIdCS <- pletC $ scriptHashToCS # pfromData elemIdMP
            kTokName <- pletC $ pcon $ PTokenName (pblake2b_256 #$ pserialiseData # pdataImpl k)
            ptraceC "setElemZ"
            pguardC "Mints one ElementID NFT with a token name == blake2b_256(densKey)" $ checkMintsOne # elemIdCS # kTokName

            pure $ pcon PUnit
        )
  pure result
  where
    -- TODO: pletC these funs with the correct arguments pre-applied. These bindings
    --       are a temporary thing to improve code readability until I'm sure the logic is solid

    -- If it's an initialization tx, there should be *one* SetDatum in the outputs, and that set datum should match
    -- the schema for an initialization datum
    initialSetDatumInOutputs :: ClosedTerm (PCurrencySymbol :--> PBuiltinList PTxOut :--> PBool)
    initialSetDatumInOutputs = phoistAcyclic $ plam $ \setElemCS outputs -> unTermCont $ do
      withSetElemCS <- pletC $ pfilter # (hasCS # setElemCS) # outputs
      ptraceC "HERE!"
      pure $
        pif
          (plength # withSetElemCS #== 1)
          (isInitialSetDatum #$ txOutDatum #$ phead # withSetElemCS)
          (pcon PFalse)

    -- The protocol MP should be a one shot, and a set initialization tx should mint it
    findProtocolInOutputs :: ClosedTerm (PCurrencySymbol :--> PBuiltinList PTxOut :--> Protocol)
    findProtocolInOutputs = phoistAcyclic $ plam $ \protocolCS outputs -> unTermCont $ do
      withProtocolCS <- pletC $ pfilter # (hasCS # protocolCS) # outputs
      pguardC "Exactly 1 output with a Protocol CS" $ plength # withProtocolCS #== 1
      pure $ txOutDatum # (phead # withProtocolCS)

    validateSetInsert :: ClosedTerm (SetDatum :--> DensKey :--> PBool)
    validateSetInsert = phoistAcyclic $ plam $ \setDatum toInsert -> unTermCont $ do
      SetDatum l r _ <- pmatchC setDatum
      DensKey lName lClass <- pmatchC $ pfromData l
      DensKey rName rClass <- pmatchC $ pfromData r
      DensKey xName xClass <- pmatchC toInsert
      pguardC "All keys have same class" (lClass #== rClass #&& lClass #== xClass)
      pguardC "l < x" (pfromData lName #< pfromData xName)
      pguardC "x < r" (pfromData xName #< pfromData rName)
      pure $ pcon PTrue
