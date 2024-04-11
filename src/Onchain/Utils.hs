{-# LANGUAGE AllowAmbiguousTypes #-}
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
-- for hls to not yell at me
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Onchain.Utils where

import Onchain.Types (
  DensKey (..),
  SetDatum (..),
  ptryFromData,
 )

-- import LambdaBuffers.Plutus.V1.Plutarch (Bytes)
-- import LambdaBuffers.Prelude.Plutarch qualified as Lb.Plutarch
-- import LambdaBuffers.Runtime.Plutarch (PList (PList))

import Plutarch (PlutusType, Term)

import Plutarch.Prelude (
  ClosedTerm,
  PAsData,
  PBool (..),
  PBuiltinList,
  PBuiltinPair,
  PData,
  PEq ((#==)),
  PInteger,
  PIsData,
  PMaybe (..),
  PString,
  PTryFrom,
  pcon,
  pconstant,
  pdata,
  pdcons,
  pdnil,
  pfilter,
  pfind,
  pfromData,
  phoistAcyclic,
  pif,
  plam,
  plength,
  plet,
  pmap,
  pmatch,
  psndBuiltin,
  ptrace,
  ptraceError,
  (#),
  (#$),
  (#&&),
  (:-->),
 )

import Plutarch.Api.V1 (KeyGuarantees (Sorted), PCredential (..), PCurrencySymbol (..), PTokenName (..))
import Plutarch.Api.V1.AssocMap (plookup)
import Plutarch.Api.V1.Maybe (PMaybeData (PDNothing))
import Plutarch.Api.V1.Scripts (PScriptHash (..))
import Plutarch.Api.V1.Value (pforgetPositive, pvalueOf)
import Plutarch.Api.V2 (
  AmountGuarantees (..),
  PAddress (..),
  PDatum (PDatum),
  PMap (..),
  POutputDatum (POutputDatum),
  PScriptContext,
  PTxInInfo (..),
  PTxOut (..),
  PValue (..),
 )
import Plutarch.Api.V2.Contexts (PScriptPurpose (PMinting))
import Plutarch.List (PList, PListLike (..), pconvertLists, pfoldl)

import Plutarch.DataRepr.Internal.Field
import Plutarch.TermCont

{-
    UTILITIES (TODO: Break out into separate module)
-}

-- stupid plutarch stuff

-- traceIfFalse for unTermCont $ do
-- pguardM :: Term s PString -> Term s PBool ->  Term s PBool

-- pguardM :: Term s PString -> Term s PBool -> Term s b  -> Term s b

pletC :: Term s a -> TermCont s (Term s a)
pletC = tcont . plet

pguardC :: Term s PString -> Term s PBool -> TermCont s ()
pguardC s cond = tcont $ \f -> pif cond (f ()) $ ptraceError s

ptraceC :: Term s PString -> TermCont s ()
ptraceC s = tcont $ \f -> ptrace s (f ())

pletFieldsC ::
  forall fs a s b ps bs.
  ( PDataFields a
  , ps ~ PFields a
  , bs ~ Bindings ps fs
  , BindFields ps bs
  ) =>
  Term s a ->
  TermCont @b s (HRec (BoundTerms ps bs s))
pletFieldsC x = tcont $ pletFields @fs x

pmatchC :: PlutusType a => Term s a -> TermCont s (a s)
pmatchC = tcont . pmatch

-- empty token name constant
emptyTN :: Term s PTokenName
emptyTN = pcon (PTokenName $ pconstant "")

mintsExactly :: ClosedTerm (PInteger :--> PCurrencySymbol :--> PTokenName :--> PValue _ _ :--> PBool)
mintsExactly = phoistAcyclic $ plam $ \i cs tn val -> (pvalueOf # val # cs # tn) #== i

scriptHashToCS :: Term s (PScriptHash :--> PCurrencySymbol)
scriptHashToCS = phoistAcyclic $ plam $ \shash -> unTermCont $ do
  PScriptHash bs <- pmatchC shash
  pure $ pcon (PCurrencySymbol bs)

{- Gets the total quantity of all minted tokens in a value.
   This is needed for a "doesn't mint anything else" check,
   which is strictly necessary to conform with the specification.
-}
totalMint :: ClosedTerm (PValue 'Sorted 'NoGuarantees :--> PInteger)
totalMint = phoistAcyclic $ plam $ \val -> unTermCont $ do
  PValue innerV <- pmatchC val
  PMap innerM1 <- pmatchC innerV
  pure $ pfoldl # go # 0 # innerM1
  where
    go ::
      ClosedTerm
        ( PInteger
            :--> PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger))
            :--> PInteger
        )
    go = phoistAcyclic $ plam $ \acc bipair -> unTermCont $ do
      PMap innerM2 <- pmatchC . pfromData $ psndBuiltin # bipair
      recList <- pletC $ plam $ \inneracc innerpair -> unTermCont $ do
        q <- pletC $ pfromData $ psndBuiltin # innerpair
        pure $ inneracc + q
      x <- pletC $ pfoldl # recList # 0 # innerM2
      pure $ acc + x

{- -go :: ClosedTerm (
  PList (PPair PCurrencySymbol (PPair PTokenName PInteger))
  :--> PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger))
     ) -}
-- i hate plutarch
pscriptHashAddress :: Term s (PAsData PScriptHash :--> PAddress)
pscriptHashAddress = plam $ \datahash ->
  let credential = pcon (PScriptCredential (pdcons @"_0" # datahash #$ pdnil))
      nothing = pdata $ pcon (PDNothing pdnil)
      inner = pdcons @"credential" # pdata credential #$ pdcons @"stakingCredential" # nothing #$ pdnil
   in pcon (PAddress inner)

-- SetDatum predicate builder
hasLR :: Term s (DensKey :--> DensKey :--> SetDatum :--> PBool)
hasLR = phoistAcyclic $ plam $ \l r setDatum -> unTermCont $ do
  SetDatum l' r' _ <- pmatchC setDatum
  pure $ (l #== pfromData l') #&& (r #== pfromData r')

resolved :: ClosedTerm (PTxInInfo :--> PTxOut)
resolved = phoistAcyclic $ plam $ \out -> unTermCont $ do
  PTxInInfo inRec <- pmatchC out
  pure $ pfield @"resolved" # inRec

{- Finds a datum in a list of TxOuts where:
   - The TxOut pays to the supplied address
   - The matching TxOut value contains an empty token name at the provided currency symbol
   - The datum satisfies the provided predicate

   May require a type application if the type cannot be inferred (thought it usually should be inferable)

   Is this really a closed term? :P

   NOTE: Rework if the token name matters
   TODO: Optimize so we only traverse the list once
-}
extractDatumSuchThat ::
  forall t.
  (PTryFrom PData (PAsData t), PIsData t) =>
  ClosedTerm
    ( (t :--> PBool)
        :--> PAddress -- output addr
        :--> PCurrencySymbol
        :--> PBuiltinList PTxOut
        :--> t
    )
extractDatumSuchThat = phoistAcyclic $ plam $ \p addr cs outs' -> unTermCont $ do
  outs <- pletC $ pconvertLists @PBuiltinList @PList @PTxOut @_ # outs' -- missing PLift for SetDatum if we don't convert. Maybe write instance?
  xs <- pletC (pfilter # (matchAddrCS # addr # cs) # outs)
  x <- pmatchC $ pfind # p #$ pmap # plam (\x -> txOutDatum # x) # xs
  case x of
    PJust datum -> pure datum
    PNothing -> pure $ ptraceError "Could not find tx out with supplied addr/cs/setdatum pred" -- todo: better error!

matchAddrCS :: ClosedTerm (PAddress :--> PCurrencySymbol :--> PTxOut :--> PBool)
matchAddrCS = plam $ \addr cs txout -> unTermCont $ do
  PTxOut outRec <- pmatchC txout
  outFields <- pletFieldsC @'["address", "value", "datum"] outRec
  addrsMatch <- pletC $ outFields.address #== addr
  hasNFT <- pletC $ (pvalueOf # outFields.value # cs # emptyTN) #== 1
  pure $ addrsMatch #&& hasNFT

txOutValue :: ClosedTerm (PTxOut :--> PValue 'Sorted 'Positive)
txOutValue = phoistAcyclic $ plam $ \txOut -> unTermCont $ do
  PTxOut outRec <- pmatchC txOut
  pure $ pfield @"value" # outRec

hasCS :: ClosedTerm (PCurrencySymbol :--> PTxOut :--> PBool)
hasCS = phoistAcyclic $ plam $ \cs out -> unTermCont $ do
  PTxOut outRec <- pmatchC out
  val <- pletC $ pfield @"value" # outRec
  pure $
    pvalueOf
      # val
      # cs
      # emptyTN
      #== 1

valHasCS :: ClosedTerm (PCurrencySymbol :--> PValue 'Sorted 'Positive :--> PBool)
valHasCS = phoistAcyclic $ plam $ \cs val -> unTermCont $ do
  PValue valInner <- pmatchC val
  res <- pmatchC $ plookup # cs # valInner
  case res of
    PJust _ -> pure $ pcon PTrue
    PNothing -> pure $ pcon PFalse

lacksCS :: ClosedTerm (PCurrencySymbol :--> PTxOut :--> PBool)
lacksCS = phoistAcyclic $ plam $ \cs out -> unTermCont $ do
  PTxOut outRec <- pmatchC out
  pure $ valLacksCS # cs #$ pforgetPositive (pfield @"value" # outRec)

valLacksCS :: ClosedTerm (PCurrencySymbol :--> PValue 'Sorted 'NoGuarantees :--> PBool)
valLacksCS = phoistAcyclic $ plam $ \cs val -> unTermCont $ do
  PValue valInner <- pmatchC val
  pure $ (plookup # cs # valInner) #== pcon PNothing

paysTo :: ClosedTerm (PAddress :--> PTxOut :--> PBool)
paysTo = phoistAcyclic $ plam $ \addr out -> unTermCont $ do
  PTxOut outRec <- pmatchC out
  outAddr <- pletC . pfromData $ pfield @"address" # outRec
  pure $ outAddr #== addr

findUnique ::
  (PListLike l, PElemConstraint l a) =>
  Term
    s
    ( (a :--> PBool)
        :--> l a
        :--> a
    )
findUnique = phoistAcyclic $ plam $ \p list -> unTermCont $ do
  xs <- pletC $ pfilter # p # list
  pguardC "findUnique: List does not have exactly one matching element" ((plength # xs) #== 1)
  pure $ phead # xs

pownCurrencySymbol :: ClosedTerm (PScriptContext :--> PCurrencySymbol)
pownCurrencySymbol = phoistAcyclic $ plam $ \cxt -> unTermCont $ do
  purpose <- pmatchC (pfromData $ pfield @"purpose" # cxt)
  case purpose of
    PMinting csRec -> pure $ pfield @"_0" # csRec
    _ -> pure $ ptraceError "Purpose is not minting"

txOutDatum :: forall t. (PTryFrom PData (PAsData t), PIsData t) => ClosedTerm (PTxOut :--> t)
txOutDatum = phoistAcyclic $ plam $ \txOut -> unTermCont $ do
  outDatum <- pmatchC $ pfield @"datum" # txOut
  case outDatum of
    POutputDatum outDatumRec -> do
      PDatum inner <- pmatchC $ pfield @"outputDatum" # outDatumRec
      pure $ ptryFromData inner
    _ -> pure $ ptraceError "not a set datum"

-- extractDatumSuchThat where you don't care about the suchThat
extractDatum :: forall t. (PTryFrom PData (PAsData t), PIsData t) => ClosedTerm (PAddress :--> PCurrencySymbol :--> PBuiltinList PTxOut :--> t)
extractDatum = extractDatumSuchThat #$ plam (\_ -> pcon PTrue)

findOutWithCS ::
  (PListLike l, PElemConstraint l PTxOut) =>
  Term s (PCurrencySymbol :--> l PTxOut :--> PMaybe PTxOut)
findOutWithCS = phoistAcyclic $ plam $ \cs list ->
  pfind # (hasCS # cs) # list
