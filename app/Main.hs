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
import Plutarch.Prelude (PData, PUnit (..), pconstant, PAsData)


import Onchain.Protocol qualified as Protocol
import Onchain.Records qualified as Records
import Onchain.SetElem qualified as SetElem
import Onchain.ElemID qualified as ElemID
import Onchain.Types qualified as T
import Plutarch.TryFrom (ptryFrom)



mkSetValidator :: ClosedTerm (PData  :--> PValidator)
mkSetValidator = phoistAcyclic $ plam $ \pcs _ sInsert cxt ->
  popaque $ SetElem.mkSetValidator # T.ptryFromData pcs # pcon PUnit # T.ptryFromData @T.SetInsert sInsert # cxt

mkRecordValidator :: ClosedTerm (PData :--> PValidator)
mkRecordValidator = phoistAcyclic $ plam $ \pcs recDatum _ cxt ->
  popaque $ Records.mkRecordValidator # T.ptryFromData pcs # T.ptryFromData recDatum # pcon PUnit # cxt

mkSetElemMintingPolicy :: ClosedTerm (PData :--> PMintingPolicy)
mkSetElemMintingPolicy = phoistAcyclic $ plam $ \pcs sInsert cxt ->
  popaque $ SetElem.mkSetElemMintingPolicy # T.ptryFromData pcs # T.ptryFromData sInsert # cxt

mkElemIDMintingPolicy :: ClosedTerm (PData  :--> PMintingPolicy)
mkElemIDMintingPolicy = phoistAcyclic $ plam $ \pcs _ cxt ->
  popaque $ ElemID.mkElemIDMintingPolicy # T.ptryFromData pcs # pcon PUnit # cxt

mkProtocolMintingPolicy :: ClosedTerm (PData :--> PMintingPolicy)
mkProtocolMintingPolicy = phoistAcyclic $ plam $ \outref _ cxt ->
  popaque $ Protocol.mkProtocolMintingPolicy # T.ptryFromData outref # pcon PUnit # cxt


main :: IO ()
main = do
  let cfg = Config DoTracing
  writeTypedScript cfg "mkSetValidator" "./scripts/mkSetValidator.json" mkSetValidator
  writeTypedScript cfg "mkRecordValidator" "./scripts/mkRecordValidator.json" mkRecordValidator
  writeTypedScript cfg "mkSetElemMintingPolicy" "./scripts/mkSetElemMintingPolicy.json" mkSetElemMintingPolicy
  writeTypedScript cfg "mkElemIDMintingPolicy" "./scripts/mkElemIDMintingPolicy.json" mkElemIDMintingPolicy
  writeTypedScript cfg "mkProtocolMintingPolicy" "./scripts/mkProtocolMintingPolicy.json" mkProtocolMintingPolicy
