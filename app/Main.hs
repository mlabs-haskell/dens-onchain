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
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Plutarch
import Plutarch.Api.V2
import Ply.Plutarch
import Plutarch.Prelude (PData, PUnit (..), pconstant)


import Onchain.Protocol qualified as Protocol
import Onchain.Records qualified as Records
import Onchain.SetElem qualified as SetElem
import Onchain.ElemID qualified as ElemID
import Onchain.Types qualified as T



mkSetValidator :: ClosedTerm (PCurrencySymbol :--> PValidator)
mkSetValidator = phoistAcyclic $ plam $ \pcs _ sInsert cxt ->
  popaque $ SetElem.mkSetValidator # pcs # pcon PUnit # T.ptryFromData @T.SetInsert sInsert # cxt

mkRecordValidator :: ClosedTerm (PCurrencySymbol :--> PValidator)
mkRecordValidator = phoistAcyclic $ plam $ \pcs recDatum _ cxt ->
  popaque $ Records.mkRecordValidator # pcs # T.ptryFromData recDatum # pcon PUnit # cxt

mkSetElemMintingPolicy :: ClosedTerm (PCurrencySymbol :--> PMintingPolicy)
mkSetElemMintingPolicy = phoistAcyclic $ plam $ \pcs sInsert cxt ->
  popaque $ SetElem.mkSetElemMintingPolicy # pcs # T.ptryFromData sInsert # cxt

mkElemIDMintingPolicy :: ClosedTerm (PCurrencySymbol :--> PMintingPolicy)
mkElemIDMintingPolicy = phoistAcyclic $ plam $ \pcs _ cxt ->
  popaque $ ElemID.mkElemIDMintingPolicy # pcs # pcon PUnit # cxt

mkProtocolMintingPolicy :: ClosedTerm (PTxOutRef :--> PMintingPolicy)
mkProtocolMintingPolicy = phoistAcyclic $ plam $ \outref _ cxt ->
  popaque $ Protocol.mkProtocolMintingPolicy # outref # pcon PUnit # cxt


main :: IO ()
main = do
  let cfg = Config DoTracing
  writeTypedScript cfg "mkSetValidator" "./scripts/mkSetValidator.plutus" mkSetValidator
  writeTypedScript cfg "mkRecordValidator" "./scripts/mkRecordValidator.plutus" mkRecordValidator
  writeTypedScript cfg "mkSetElemMintingPolicy" "./scripts/mkSetElemMintingPolicy.plutus" mkSetElemMintingPolicy
  writeTypedScript cfg "mkElemIDMintingPolicy" "./scripts/mkElemIDMintingPolicy.plutus" mkElemIDMintingPolicy
  writeTypedScript cfg "mkProtocolMintingPolicy" "./scripts/mkProtocolMintingPolicy" mkProtocolMintingPolicy
