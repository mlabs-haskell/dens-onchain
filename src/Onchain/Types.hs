{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Onchain.Types (
  DensKey (..),
  DensValue (..),
  Protocol (..),
  RecordDatum (..),
  SetDatum (..),
  SetInsert (..),
) where

-- import qualified LambdaBuffers.Plutus.V1.Plutarch
-- import qualified LambdaBuffers.Plutus.V2.Plutarch
-- import qualified LambdaBuffers.Prelude.Plutarch
import qualified Data.Functor.Const
import qualified GHC.Generics

-- import qualified LambdaBuffers.Runtime.Plutarch
-- import qualified LambdaBuffers.Runtime.Plutarch.LamVal

import Data.Functor.Const (Const)
import qualified Plutarch
import Plutarch.Api.V2
import qualified Plutarch.Bool
import Plutarch.Builtin
import qualified Plutarch.Internal.PlutusType
import Plutarch.Prelude hiding (PJust, PMaybe, PNothing)
import qualified Plutarch.Prelude hiding (PJust, PMaybe, PNothing)
import Plutarch.Reducible (Reduce)
import qualified Plutarch.Show
import Plutarch.TryFrom (PTryFrom (..))
import qualified Plutarch.Unsafe

data DensKey (s :: Plutarch.S)
  = DensKey
      (Plutarch.Term s (PAsData PByteString))
      (Plutarch.Term s (PAsData PInteger))
  deriving stock (GHC.Generics.Generic)
  deriving anyclass (Plutarch.Show.PShow)

newtype DensValue (s :: Plutarch.S) = DensValue (Plutarch.Term s (PAsData (PMaybe PByteString)))
  deriving stock (GHC.Generics.Generic)
  deriving anyclass (Plutarch.Show.PShow)

data Protocol (s :: Plutarch.S)
  = Protocol
      (Plutarch.Term s (PAsData PScriptHash))
      (Plutarch.Term s (PAsData PScriptHash))
      (Plutarch.Term s (PAsData PScriptHash))
      (Plutarch.Term s (PAsData PScriptHash))
  deriving stock (GHC.Generics.Generic)
  deriving anyclass (Plutarch.Show.PShow)

data RecordDatum (s :: Plutarch.S)
  = RecordDatum
      (Plutarch.Term s (PAsData PInteger))
      (Plutarch.Term s (PAsData PByteString))
      (Plutarch.Term s (PAsData DensValue))
      (Plutarch.Term s (PAsData PPubKeyHash))
  deriving stock (GHC.Generics.Generic)
  deriving anyclass (Plutarch.Show.PShow)

data SetDatum (s :: Plutarch.S)
  = SetDatum
      (Plutarch.Term s (PAsData DensKey))
      (Plutarch.Term s (PAsData DensKey))
      (Plutarch.Term s (PAsData PCurrencySymbol))
  deriving stock (GHC.Generics.Generic)
  deriving anyclass (Plutarch.Show.PShow)

data SetInsert (s :: Plutarch.S) = SetInsert'Insert (Plutarch.Term s (PAsData DensKey))
  deriving stock (GHC.Generics.Generic)
  deriving anyclass (Plutarch.Show.PShow)

instance Plutarch.Bool.PEq Protocol where
  (#==) = \l r -> ((Plutarch.Bool.#==)) (Plutarch.Builtin.pdata l) (Plutarch.Builtin.pdata r)

instance Plutarch.Builtin.PIsData Protocol where
  pdataImpl = Plutarch.Unsafe.punsafeCoerce
  pfromDataImpl = Plutarch.Unsafe.punsafeCoerce

instance Plutarch.Internal.PlutusType.PlutusType Protocol where
  type PInner Protocol = Plutarch.Builtin.PData
  pcon' =
    ( \x0 ->
        let Protocol x1 x2 x3 x4 = x0
         in listData
              ( [ toPlutusData (x1)
                , toPlutusData (x2)
                , toPlutusData (x3)
                , toPlutusData (x4)
                ]
              )
    )
  pmatch' =
    ( \pd f ->
        ((Plutarch.Prelude.#))
          ( Plutarch.Prelude.plam
              ( \x0 ->
                  (Plutarch.Prelude.#)
                    ( (Plutarch.Prelude.#)
                        ( (Plutarch.Prelude.#)
                            ( (Plutarch.Prelude.#)
                                ((Plutarch.Prelude.#) (pcasePlutusData) (Plutarch.Prelude.plam (\x1 -> Plutarch.Prelude.plam (\x2 -> pfailParse))))
                                ( Plutarch.Prelude.plam
                                    ( \x3 ->
                                        Plutarch.Prelude.pmatch
                                          x3
                                          ( \x4 -> case x4 of
                                              Plutarch.Prelude.PNil -> pfailParse
                                              Plutarch.Prelude.PCons x5 x6 ->
                                                Plutarch.Prelude.pmatch
                                                  x6
                                                  ( \x7 -> case x7 of
                                                      Plutarch.Prelude.PNil -> pfailParse
                                                      Plutarch.Prelude.PCons x8 x9 ->
                                                        Plutarch.Prelude.pmatch
                                                          x9
                                                          ( \x10 -> case x10 of
                                                              Plutarch.Prelude.PNil -> pfailParse
                                                              Plutarch.Prelude.PCons x11 x12 ->
                                                                Plutarch.Prelude.pmatch
                                                                  x12
                                                                  ( \x13 -> case x13 of
                                                                      Plutarch.Prelude.PNil -> pfailParse
                                                                      Plutarch.Prelude.PCons x14 x15 ->
                                                                        Plutarch.Prelude.pmatch
                                                                          x15
                                                                          ( \x16 -> case x16 of
                                                                              Plutarch.Prelude.PNil -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPlutusType) (x5))) (Plutarch.Prelude.plam (\x19 -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPlutusType) (x8))) (Plutarch.Prelude.plam (\x20 -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPlutusType) (x11))) (Plutarch.Prelude.plam (\x21 -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPlutusType) (x14))) (Plutarch.Prelude.plam (\x22 -> (Plutarch.Prelude.#) (psucceedParse) (f (Protocol (x19) (x20) (x21) (x22)))))))))))
                                                                              Plutarch.Prelude.PCons x17 x18 -> pfailParse
                                                                          )
                                                                  )
                                                          )
                                                  )
                                          )
                                    )
                                )
                            )
                            (Plutarch.Prelude.plam (\x23 -> pfailParse))
                        )
                        (Plutarch.Prelude.plam (\x24 -> pfailParse))
                    )
                    (x0)
              )
          )
          pd
    )

instance Plutarch.TryFrom.PTryFrom Plutarch.Builtin.PData Protocol where
  type PTryFromExcess Plutarch.Builtin.PData Protocol = Data.Functor.Const.Const ()
  ptryFrom' = ptryFromPAsData
instance Plutarch.TryFrom.PTryFrom Plutarch.Builtin.PData (PAsData Protocol) where
  type PTryFromExcess Plutarch.Builtin.PData (PAsData Protocol) = Data.Functor.Const.Const ()
  ptryFrom' =
    ( \pd f ->
        f
          ( ((Plutarch.Prelude.#))
              ( Plutarch.Prelude.plam
                  ( \x0 ->
                      (Plutarch.Prelude.#)
                        ( (Plutarch.Prelude.#)
                            ( (Plutarch.Prelude.#)
                                ( (Plutarch.Prelude.#)
                                    ((Plutarch.Prelude.#) (pcasePlutusData) (Plutarch.Prelude.plam (\x1 -> Plutarch.Prelude.plam (\x2 -> pfailParse))))
                                    ( Plutarch.Prelude.plam
                                        ( \x3 ->
                                            Plutarch.Prelude.pmatch
                                              x3
                                              ( \x4 -> case x4 of
                                                  Plutarch.Prelude.PNil -> pfailParse
                                                  Plutarch.Prelude.PCons x5 x6 ->
                                                    Plutarch.Prelude.pmatch
                                                      x6
                                                      ( \x7 -> case x7 of
                                                          Plutarch.Prelude.PNil -> pfailParse
                                                          Plutarch.Prelude.PCons x8 x9 ->
                                                            Plutarch.Prelude.pmatch
                                                              x9
                                                              ( \x10 -> case x10 of
                                                                  Plutarch.Prelude.PNil -> pfailParse
                                                                  Plutarch.Prelude.PCons x11 x12 ->
                                                                    Plutarch.Prelude.pmatch
                                                                      x12
                                                                      ( \x13 -> case x13 of
                                                                          Plutarch.Prelude.PNil -> pfailParse
                                                                          Plutarch.Prelude.PCons x14 x15 ->
                                                                            Plutarch.Prelude.pmatch
                                                                              x15
                                                                              ( \x16 -> case x16 of
                                                                                  Plutarch.Prelude.PNil -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPTryFrom) (x5))) (Plutarch.Prelude.plam (\x19 -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPTryFrom) (x8))) (Plutarch.Prelude.plam (\x20 -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPTryFrom) (x11))) (Plutarch.Prelude.plam (\x21 -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPTryFrom) (x14))) (Plutarch.Prelude.plam (\x22 -> (Plutarch.Prelude.#) (psucceedParse) (pdata . pcon $ (Protocol (x19) (x20) (x21) (x22)))))))))))
                                                                                  Plutarch.Prelude.PCons x17 x18 -> pfailParse
                                                                              )
                                                                      )
                                                              )
                                                      )
                                              )
                                        )
                                    )
                                )
                                (Plutarch.Prelude.plam (\x23 -> pfailParse))
                            )
                            (Plutarch.Prelude.plam (\x24 -> pfailParse))
                        )
                        (x0)
                  )
              )
              pd
          , ()
          )
    )

instance Plutarch.Bool.PEq DensKey where
  (#==) = \l r -> ((Plutarch.Bool.#==)) (Plutarch.Builtin.pdata l) (Plutarch.Builtin.pdata r)

instance Plutarch.Builtin.PIsData DensKey where
  pdataImpl = Plutarch.Unsafe.punsafeCoerce
  pfromDataImpl = Plutarch.Unsafe.punsafeCoerce

instance Plutarch.Internal.PlutusType.PlutusType DensKey where
  type PInner DensKey = Plutarch.Builtin.PData
  pcon' =
    ( \x0 ->
        let DensKey x1 x2 = x0
         in listData
              ( [ toPlutusData (x1)
                , toPlutusData (x2)
                ]
              )
    )
  pmatch' =
    ( \pd f ->
        ((Plutarch.Prelude.#))
          ( Plutarch.Prelude.plam
              ( \x0 ->
                  (Plutarch.Prelude.#)
                    ( (Plutarch.Prelude.#)
                        ( (Plutarch.Prelude.#)
                            ( (Plutarch.Prelude.#)
                                ((Plutarch.Prelude.#) (pcasePlutusData) (Plutarch.Prelude.plam (\x1 -> Plutarch.Prelude.plam (\x2 -> pfailParse))))
                                ( Plutarch.Prelude.plam
                                    ( \x3 ->
                                        Plutarch.Prelude.pmatch
                                          x3
                                          ( \x4 -> case x4 of
                                              Plutarch.Prelude.PNil -> pfailParse
                                              Plutarch.Prelude.PCons x5 x6 ->
                                                Plutarch.Prelude.pmatch
                                                  x6
                                                  ( \x7 -> case x7 of
                                                      Plutarch.Prelude.PNil -> pfailParse
                                                      Plutarch.Prelude.PCons x8 x9 ->
                                                        Plutarch.Prelude.pmatch
                                                          x9
                                                          ( \x10 -> case x10 of
                                                              Plutarch.Prelude.PNil -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPlutusType) (x5))) (Plutarch.Prelude.plam (\x13 -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPlutusType) (x8))) (Plutarch.Prelude.plam (\x14 -> (Plutarch.Prelude.#) (psucceedParse) (f (DensKey (x13) (x14)))))))
                                                              Plutarch.Prelude.PCons x11 x12 -> pfailParse
                                                          )
                                                  )
                                          )
                                    )
                                )
                            )
                            (Plutarch.Prelude.plam (\x15 -> pfailParse))
                        )
                        (Plutarch.Prelude.plam (\x16 -> pfailParse))
                    )
                    (x0)
              )
          )
          pd
    )

instance Plutarch.TryFrom.PTryFrom Plutarch.Builtin.PData DensKey where
  type PTryFromExcess Plutarch.Builtin.PData DensKey = Data.Functor.Const.Const ()
  ptryFrom' = ptryFromPAsData
instance Plutarch.TryFrom.PTryFrom Plutarch.Builtin.PData (PAsData DensKey) where
  type PTryFromExcess Plutarch.Builtin.PData (PAsData DensKey) = Data.Functor.Const.Const ()
  ptryFrom' =
    ( \pd f ->
        f
          ( ((Plutarch.Prelude.#))
              ( Plutarch.Prelude.plam
                  ( \x0 ->
                      (Plutarch.Prelude.#)
                        ( (Plutarch.Prelude.#)
                            ( (Plutarch.Prelude.#)
                                ( (Plutarch.Prelude.#)
                                    ((Plutarch.Prelude.#) (pcasePlutusData) (Plutarch.Prelude.plam (\x1 -> Plutarch.Prelude.plam (\x2 -> pfailParse))))
                                    ( Plutarch.Prelude.plam
                                        ( \x3 ->
                                            Plutarch.Prelude.pmatch
                                              x3
                                              ( \x4 -> case x4 of
                                                  Plutarch.Prelude.PNil -> pfailParse
                                                  Plutarch.Prelude.PCons x5 x6 ->
                                                    Plutarch.Prelude.pmatch
                                                      x6
                                                      ( \x7 -> case x7 of
                                                          Plutarch.Prelude.PNil -> pfailParse
                                                          Plutarch.Prelude.PCons x8 x9 ->
                                                            Plutarch.Prelude.pmatch
                                                              x9
                                                              ( \x10 -> case x10 of
                                                                  Plutarch.Prelude.PNil -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPTryFrom) (x5))) (Plutarch.Prelude.plam (\x13 -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPTryFrom) (x8))) (Plutarch.Prelude.plam (\x14 -> (Plutarch.Prelude.#) (psucceedParse) (pdata . pcon $ (DensKey (x13) (x14)))))))
                                                                  Plutarch.Prelude.PCons x11 x12 -> pfailParse
                                                              )
                                                      )
                                              )
                                        )
                                    )
                                )
                                (Plutarch.Prelude.plam (\x15 -> pfailParse))
                            )
                            (Plutarch.Prelude.plam (\x16 -> pfailParse))
                        )
                        (x0)
                  )
              )
              pd
          , ()
          )
    )

instance Plutarch.Bool.PEq DensValue where
  (#==) = \l r -> ((Plutarch.Bool.#==)) (Plutarch.Builtin.pdata l) (Plutarch.Builtin.pdata r)

instance Plutarch.Builtin.PIsData DensValue where
  pdataImpl = Plutarch.Unsafe.punsafeCoerce
  pfromDataImpl = Plutarch.Unsafe.punsafeCoerce

instance Plutarch.Internal.PlutusType.PlutusType DensValue where
  type PInner DensValue = Plutarch.Builtin.PData
  pcon' = (\x0 -> let DensValue x1 = x0 in toPlutusData (x1))
  pmatch' = (\pd f -> ((Plutarch.Prelude.#)) (Plutarch.Prelude.plam (\x0 -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPlutusType) (x0))) (Plutarch.Prelude.plam (\x1 -> (Plutarch.Prelude.#) (psucceedParse) (f (DensValue (x1))))))) pd)

instance Plutarch.TryFrom.PTryFrom Plutarch.Builtin.PData DensValue where
  type PTryFromExcess Plutarch.Builtin.PData DensValue = Data.Functor.Const.Const ()
  ptryFrom' = ptryFromPAsData
instance Plutarch.TryFrom.PTryFrom Plutarch.Builtin.PData (PAsData DensValue) where
  type PTryFromExcess Plutarch.Builtin.PData (PAsData DensValue) = Data.Functor.Const.Const ()
  ptryFrom' = (\pd f -> f (((Plutarch.Prelude.#)) (Plutarch.Prelude.plam (\x0 -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPTryFrom) (x0))) (Plutarch.Prelude.plam (\x1 -> (Plutarch.Prelude.#) (psucceedParse) (pdata . pcon $ (DensValue (x1))))))) pd, ()))

instance Plutarch.Bool.PEq SetDatum where
  (#==) = \l r -> ((Plutarch.Bool.#==)) (Plutarch.Builtin.pdata l) (Plutarch.Builtin.pdata r)

instance Plutarch.Builtin.PIsData SetDatum where
  pdataImpl = Plutarch.Unsafe.punsafeCoerce
  pfromDataImpl = Plutarch.Unsafe.punsafeCoerce

instance Plutarch.Internal.PlutusType.PlutusType SetDatum where
  type PInner SetDatum = Plutarch.Builtin.PData
  pcon' =
    ( \x0 ->
        let SetDatum x1 x2 x3 = x0
         in listData
              ( [ toPlutusData (x1)
                , toPlutusData (x2)
                , toPlutusData (x3)
                ]
              )
    )
  pmatch' =
    ( \pd f ->
        ((Plutarch.Prelude.#))
          ( Plutarch.Prelude.plam
              ( \x0 ->
                  (Plutarch.Prelude.#)
                    ( (Plutarch.Prelude.#)
                        ( (Plutarch.Prelude.#)
                            ( (Plutarch.Prelude.#)
                                ((Plutarch.Prelude.#) (pcasePlutusData) (Plutarch.Prelude.plam (\x1 -> Plutarch.Prelude.plam (\x2 -> pfailParse))))
                                ( Plutarch.Prelude.plam
                                    ( \x3 ->
                                        Plutarch.Prelude.pmatch
                                          x3
                                          ( \x4 -> case x4 of
                                              Plutarch.Prelude.PNil -> pfailParse
                                              Plutarch.Prelude.PCons x5 x6 ->
                                                Plutarch.Prelude.pmatch
                                                  x6
                                                  ( \x7 -> case x7 of
                                                      Plutarch.Prelude.PNil -> pfailParse
                                                      Plutarch.Prelude.PCons x8 x9 ->
                                                        Plutarch.Prelude.pmatch
                                                          x9
                                                          ( \x10 -> case x10 of
                                                              Plutarch.Prelude.PNil -> pfailParse
                                                              Plutarch.Prelude.PCons x11 x12 ->
                                                                Plutarch.Prelude.pmatch
                                                                  x12
                                                                  ( \x13 -> case x13 of
                                                                      Plutarch.Prelude.PNil -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPlutusType) (x5))) (Plutarch.Prelude.plam (\x16 -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPlutusType) (x8))) (Plutarch.Prelude.plam (\x17 -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPlutusType) (x11))) (Plutarch.Prelude.plam (\x18 -> (Plutarch.Prelude.#) (psucceedParse) (f (SetDatum (x16) (x17) (x18)))))))))
                                                                      Plutarch.Prelude.PCons x14 x15 -> pfailParse
                                                                  )
                                                          )
                                                  )
                                          )
                                    )
                                )
                            )
                            (Plutarch.Prelude.plam (\x19 -> pfailParse))
                        )
                        (Plutarch.Prelude.plam (\x20 -> pfailParse))
                    )
                    (x0)
              )
          )
          pd
    )

instance Plutarch.TryFrom.PTryFrom Plutarch.Builtin.PData SetDatum where
  type PTryFromExcess Plutarch.Builtin.PData SetDatum = Data.Functor.Const.Const ()
  ptryFrom' = ptryFromPAsData
instance Plutarch.TryFrom.PTryFrom Plutarch.Builtin.PData (PAsData SetDatum) where
  type PTryFromExcess Plutarch.Builtin.PData (PAsData SetDatum) = Data.Functor.Const.Const ()
  ptryFrom' =
    ( \pd f ->
        f
          ( ((Plutarch.Prelude.#))
              ( Plutarch.Prelude.plam
                  ( \x0 ->
                      (Plutarch.Prelude.#)
                        ( (Plutarch.Prelude.#)
                            ( (Plutarch.Prelude.#)
                                ( (Plutarch.Prelude.#)
                                    ((Plutarch.Prelude.#) (pcasePlutusData) (Plutarch.Prelude.plam (\x1 -> Plutarch.Prelude.plam (\x2 -> pfailParse))))
                                    ( Plutarch.Prelude.plam
                                        ( \x3 ->
                                            Plutarch.Prelude.pmatch
                                              x3
                                              ( \x4 -> case x4 of
                                                  Plutarch.Prelude.PNil -> pfailParse
                                                  Plutarch.Prelude.PCons x5 x6 ->
                                                    Plutarch.Prelude.pmatch
                                                      x6
                                                      ( \x7 -> case x7 of
                                                          Plutarch.Prelude.PNil -> pfailParse
                                                          Plutarch.Prelude.PCons x8 x9 ->
                                                            Plutarch.Prelude.pmatch
                                                              x9
                                                              ( \x10 -> case x10 of
                                                                  Plutarch.Prelude.PNil -> pfailParse
                                                                  Plutarch.Prelude.PCons x11 x12 ->
                                                                    Plutarch.Prelude.pmatch
                                                                      x12
                                                                      ( \x13 -> case x13 of
                                                                          Plutarch.Prelude.PNil -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPTryFrom) (x5))) (Plutarch.Prelude.plam (\x16 -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPTryFrom) (x8))) (Plutarch.Prelude.plam (\x17 -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPTryFrom) (x11))) (Plutarch.Prelude.plam (\x18 -> (Plutarch.Prelude.#) (psucceedParse) (pdata . pcon $ (SetDatum (x16) (x17) (x18)))))))))
                                                                          Plutarch.Prelude.PCons x14 x15 -> pfailParse
                                                                      )
                                                              )
                                                      )
                                              )
                                        )
                                    )
                                )
                                (Plutarch.Prelude.plam (\x19 -> pfailParse))
                            )
                            (Plutarch.Prelude.plam (\x20 -> pfailParse))
                        )
                        (x0)
                  )
              )
              pd
          , ()
          )
    )

instance Plutarch.Bool.PEq SetInsert where
  (#==) = \l r -> ((Plutarch.Bool.#==)) (Plutarch.Builtin.pdata l) (Plutarch.Builtin.pdata r)

instance Plutarch.Builtin.PIsData SetInsert where
  pdataImpl = Plutarch.Unsafe.punsafeCoerce
  pfromDataImpl = Plutarch.Unsafe.punsafeCoerce

instance Plutarch.Internal.PlutusType.PlutusType SetInsert where
  type PInner SetInsert = Plutarch.Builtin.PData
  pcon' =
    ( \x0 -> case x0 of
        SetInsert'Insert x1 -> constrData (0) ([toPlutusData (x1)])
    )
  pmatch' =
    ( \pd f ->
        ((Plutarch.Prelude.#))
          ( Plutarch.Prelude.plam
              ( \x0 ->
                  (Plutarch.Prelude.#)
                    ( (Plutarch.Prelude.#)
                        ( (Plutarch.Prelude.#)
                            ( (Plutarch.Prelude.#)
                                ( (Plutarch.Prelude.#)
                                    (pcasePlutusData)
                                    ( Plutarch.Prelude.plam
                                        ( \x1 ->
                                            Plutarch.Prelude.plam
                                              ( \x2 ->
                                                  Plutarch.Prelude.pif
                                                    ((Plutarch.Prelude.#==) (x1) (Plutarch.Prelude.pconstant 0))
                                                    ( Plutarch.Prelude.pmatch
                                                        x2
                                                        ( \x3 -> case x3 of
                                                            Plutarch.Prelude.PNil -> pfailParse
                                                            Plutarch.Prelude.PCons x4 x5 ->
                                                              Plutarch.Prelude.pmatch
                                                                x5
                                                                ( \x6 -> case x6 of
                                                                    Plutarch.Prelude.PNil -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPlutusType) (x4))) (Plutarch.Prelude.plam (\x9 -> (Plutarch.Prelude.#) (psucceedParse) (f (SetInsert'Insert (x9)))))
                                                                    Plutarch.Prelude.PCons x7 x8 -> pfailParse
                                                                )
                                                        )
                                                    )
                                                    (pfailParse)
                                              )
                                        )
                                    )
                                )
                                (Plutarch.Prelude.plam (\x10 -> pfailParse))
                            )
                            (Plutarch.Prelude.plam (\x11 -> pfailParse))
                        )
                        (Plutarch.Prelude.plam (\x12 -> pfailParse))
                    )
                    (x0)
              )
          )
          pd
    )

instance Plutarch.TryFrom.PTryFrom Plutarch.Builtin.PData SetInsert where
  type PTryFromExcess Plutarch.Builtin.PData SetInsert = Data.Functor.Const.Const ()
  ptryFrom' = ptryFromPAsData
instance Plutarch.TryFrom.PTryFrom Plutarch.Builtin.PData (PAsData SetInsert) where
  type PTryFromExcess Plutarch.Builtin.PData (PAsData SetInsert) = Data.Functor.Const.Const ()
  ptryFrom' =
    ( \pd f ->
        f
          ( ((Plutarch.Prelude.#))
              ( Plutarch.Prelude.plam
                  ( \x0 ->
                      (Plutarch.Prelude.#)
                        ( (Plutarch.Prelude.#)
                            ( (Plutarch.Prelude.#)
                                ( (Plutarch.Prelude.#)
                                    ( (Plutarch.Prelude.#)
                                        (pcasePlutusData)
                                        ( Plutarch.Prelude.plam
                                            ( \x1 ->
                                                Plutarch.Prelude.plam
                                                  ( \x2 ->
                                                      Plutarch.Prelude.pif
                                                        ((Plutarch.Prelude.#==) (x1) (Plutarch.Prelude.pconstant 0))
                                                        ( Plutarch.Prelude.pmatch
                                                            x2
                                                            ( \x3 -> case x3 of
                                                                Plutarch.Prelude.PNil -> pfailParse
                                                                Plutarch.Prelude.PCons x4 x5 ->
                                                                  Plutarch.Prelude.pmatch
                                                                    x5
                                                                    ( \x6 -> case x6 of
                                                                        Plutarch.Prelude.PNil -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPTryFrom) (x4))) (Plutarch.Prelude.plam (\x9 -> (Plutarch.Prelude.#) (psucceedParse) (pdata . pcon $ (SetInsert'Insert (x9)))))
                                                                        Plutarch.Prelude.PCons x7 x8 -> pfailParse
                                                                    )
                                                            )
                                                        )
                                                        (pfailParse)
                                                  )
                                            )
                                        )
                                    )
                                    (Plutarch.Prelude.plam (\x10 -> pfailParse))
                                )
                                (Plutarch.Prelude.plam (\x11 -> pfailParse))
                            )
                            (Plutarch.Prelude.plam (\x12 -> pfailParse))
                        )
                        (x0)
                  )
              )
              pd
          , ()
          )
    )

instance Plutarch.Bool.PEq RecordDatum where
  (#==) = \l r -> ((Plutarch.Bool.#==)) (Plutarch.Builtin.pdata l) (Plutarch.Builtin.pdata r)

instance Plutarch.Builtin.PIsData RecordDatum where
  pdataImpl = Plutarch.Unsafe.punsafeCoerce
  pfromDataImpl = Plutarch.Unsafe.punsafeCoerce

instance Plutarch.Internal.PlutusType.PlutusType RecordDatum where
  type PInner RecordDatum = Plutarch.Builtin.PData
  pcon' =
    ( \x0 ->
        let RecordDatum x1 x2 x3 x4 = x0
         in listData
              ( [ toPlutusData (x1)
                , toPlutusData (x2)
                , toPlutusData (x3)
                , toPlutusData (x4)
                ]
              )
    )
  pmatch' =
    ( \pd f ->
        ((Plutarch.Prelude.#))
          ( Plutarch.Prelude.plam
              ( \x0 ->
                  (Plutarch.Prelude.#)
                    ( (Plutarch.Prelude.#)
                        ( (Plutarch.Prelude.#)
                            ( (Plutarch.Prelude.#)
                                ((Plutarch.Prelude.#) (pcasePlutusData) (Plutarch.Prelude.plam (\x1 -> Plutarch.Prelude.plam (\x2 -> pfailParse))))
                                ( Plutarch.Prelude.plam
                                    ( \x3 ->
                                        Plutarch.Prelude.pmatch
                                          x3
                                          ( \x4 -> case x4 of
                                              Plutarch.Prelude.PNil -> pfailParse
                                              Plutarch.Prelude.PCons x5 x6 ->
                                                Plutarch.Prelude.pmatch
                                                  x6
                                                  ( \x7 -> case x7 of
                                                      Plutarch.Prelude.PNil -> pfailParse
                                                      Plutarch.Prelude.PCons x8 x9 ->
                                                        Plutarch.Prelude.pmatch
                                                          x9
                                                          ( \x10 -> case x10 of
                                                              Plutarch.Prelude.PNil -> pfailParse
                                                              Plutarch.Prelude.PCons x11 x12 ->
                                                                Plutarch.Prelude.pmatch
                                                                  x12
                                                                  ( \x13 -> case x13 of
                                                                      Plutarch.Prelude.PNil -> pfailParse
                                                                      Plutarch.Prelude.PCons x14 x15 ->
                                                                        Plutarch.Prelude.pmatch
                                                                          x15
                                                                          ( \x16 -> case x16 of
                                                                              Plutarch.Prelude.PNil -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPlutusType) (x5))) (Plutarch.Prelude.plam (\x19 -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPlutusType) (x8))) (Plutarch.Prelude.plam (\x20 -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPlutusType) (x11))) (Plutarch.Prelude.plam (\x21 -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPlutusType) (x14))) (Plutarch.Prelude.plam (\x22 -> (Plutarch.Prelude.#) (psucceedParse) (f (RecordDatum (x19) (x20) (x21) (x22)))))))))))
                                                                              Plutarch.Prelude.PCons x17 x18 -> pfailParse
                                                                          )
                                                                  )
                                                          )
                                                  )
                                          )
                                    )
                                )
                            )
                            (Plutarch.Prelude.plam (\x23 -> pfailParse))
                        )
                        (Plutarch.Prelude.plam (\x24 -> pfailParse))
                    )
                    (x0)
              )
          )
          pd
    )

instance Plutarch.TryFrom.PTryFrom Plutarch.Builtin.PData RecordDatum where
  type PTryFromExcess Plutarch.Builtin.PData RecordDatum = Data.Functor.Const.Const ()
  ptryFrom' = ptryFromPAsData
instance Plutarch.TryFrom.PTryFrom Plutarch.Builtin.PData (PAsData RecordDatum) where
  type PTryFromExcess Plutarch.Builtin.PData (PAsData RecordDatum) = Data.Functor.Const.Const ()
  ptryFrom' =
    ( \pd f ->
        f
          ( ((Plutarch.Prelude.#))
              ( Plutarch.Prelude.plam
                  ( \x0 ->
                      (Plutarch.Prelude.#)
                        ( (Plutarch.Prelude.#)
                            ( (Plutarch.Prelude.#)
                                ( (Plutarch.Prelude.#)
                                    ( (Plutarch.Prelude.#)
                                        (pcasePlutusData)
                                        ( Plutarch.Prelude.plam
                                            ( \x1 ->
                                                Plutarch.Prelude.plam
                                                  (\x2 -> pfailParse)
                                            )
                                        )
                                    )
                                    ( Plutarch.Prelude.plam
                                        ( \x3 ->
                                            Plutarch.Prelude.pmatch
                                              x3
                                              ( \x4 -> case x4 of
                                                  Plutarch.Prelude.PNil -> pfailParse
                                                  Plutarch.Prelude.PCons x5 x6 ->
                                                    Plutarch.Prelude.pmatch
                                                      x6
                                                      ( \x7 -> case x7 of
                                                          Plutarch.Prelude.PNil -> pfailParse
                                                          Plutarch.Prelude.PCons x8 x9 ->
                                                            Plutarch.Prelude.pmatch
                                                              x9
                                                              ( \x10 -> case x10 of
                                                                  Plutarch.Prelude.PNil -> pfailParse
                                                                  Plutarch.Prelude.PCons x11 x12 ->
                                                                    Plutarch.Prelude.pmatch
                                                                      x12
                                                                      ( \x13 -> case x13 of
                                                                          Plutarch.Prelude.PNil -> pfailParse
                                                                          Plutarch.Prelude.PCons x14 x15 ->
                                                                            Plutarch.Prelude.pmatch
                                                                              x15
                                                                              ( \x16 -> case x16 of
                                                                                  Plutarch.Prelude.PNil -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPTryFrom) (x5))) (Plutarch.Prelude.plam (\x19 -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPTryFrom) (x8))) (Plutarch.Prelude.plam (\x20 -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPTryFrom) (x11))) (Plutarch.Prelude.plam (\x21 -> (Plutarch.Prelude.#) ((Plutarch.Prelude.#) (pbindParse) ((Plutarch.Prelude.#) (pfromPlutusDataPTryFrom) (x14))) (plam (\x22 -> (Plutarch.Prelude.#) (psucceedParse) (pdata . pcon $ (RecordDatum (x19) (x20) (x21) (x22)))))))))))
                                                                                  Plutarch.Prelude.PCons x17 x18 -> pfailParse
                                                                              )
                                                                      )
                                                              )
                                                      )
                                              )
                                        )
                                    )
                                )
                                (Plutarch.Prelude.plam (\x23 -> pfailParse))
                            )
                            (Plutarch.Prelude.plam (\x24 -> pfailParse))
                        )
                        (x0)
                  )
              )
              pd
          , ()
          )
    )
ptryFromPAsData :: forall a s r. (PTryFrom PData (PAsData a), PIsData a) => Term s PData -> ((Term s a, Reduce (PTryFromExcess PData (PAsData a) s)) -> Term s r) -> Term s r
ptryFromPAsData (pd :: Term s PData) f = ptryFrom @(PAsData a) pd (\(x, exc) -> f (pfromData x, exc))

-- | Plutarch `toPlutusData :: a -> PlutusData`
ptoPlutusData :: ClosedTerm (PAsData a :--> PData)
ptoPlutusData = plam toPlutusData

-- | Haskell `toPlutusData :: a -> PlutusData`
toPlutusData :: Term s (PAsData a) -> Term s PData
toPlutusData = pforgetData

-- | Plutarch PlutusType `fromPlutusData :: PlutusData -> Parser a`
pfromPlutusDataPlutusType :: ClosedTerm (PData :--> PAsData a)
pfromPlutusDataPlutusType = plam Plutarch.Unsafe.punsafeCoerce

-- | Plutarch PTryFrom `fromPlutusData :: PlutusData -> Parser a`
pfromPlutusDataPTryFrom :: (PTryFrom PData (PAsData a)) => ClosedTerm (PData :--> PAsData a)
pfromPlutusDataPTryFrom = phoistAcyclic $ plam ptryFromData
  where
    ptryFromData :: forall a s. (PTryFrom PData (PAsData a)) => Term s PData -> Term s (PAsData a)
    ptryFromData pd = ptryFrom @(PAsData a) pd fst

-- | Plutarch `constrData :: IntE -> ListE PlutusData -> PlutusData`
pconstrData :: ClosedTerm (PInteger :--> PBuiltinList PData :--> PData)
pconstrData = phoistAcyclic $ plam $ \ix args -> pforgetData $ pconstrBuiltin # ix # args

-- | Haskell `constrData :: IntE -> ListE PlutusData -> PlutusData`
constrData :: Term s PInteger -> [Term s PData] -> Term s PData
constrData ix args = pforgetData $ pconstrBuiltin # ix # toBuiltinList args

-- | Plutarch `integerData :: IntE -> PlutusData`
pintegerData :: ClosedTerm (PInteger :--> PData)
pintegerData = phoistAcyclic $ plam $ \i -> ptoPlutusData # pdata i

-- | Haskell `integerData :: IntE -> PlutusData`
integerData :: Term s PInteger -> Term s PData
integerData = toPlutusData . pdata

-- | Plutarch `listData :: ListE PlutusData -> PlutusData`
plistData :: ClosedTerm (PBuiltinList PData :--> PData)
plistData = phoistAcyclic $ plam $ pforgetData . pdata

-- | Haskell `listData :: ListE PlutusData -> PlutusData`
listData :: [Term s PData] -> Term s PData
listData = pforgetData . pdata . toBuiltinList

toBuiltinList :: [Term s PData] -> Term s (PBuiltinList PData)
toBuiltinList [] = pcon PNil
toBuiltinList (x : xs) = pcon (PCons x (toBuiltinList xs))

-- | Plutarch `casePlutusData :: (Int -> [PlutusData] -> a) -> ([PlutusData] -> a) -> (Int -> a) -> (PlutusData -> a) -> PlutusData -> a`
pcasePlutusData ::
  ClosedTerm ((PInteger :--> PBuiltinList PData :--> a) :--> (PBuiltinList PData :--> a) :--> (PInteger :--> a) :--> (PData :--> a) :--> PData :--> a)
pcasePlutusData = phoistAcyclic $ plam $ \handleConstr handleList handleInt handleOther pd ->
  pforce $
    pchooseData
      # pd
      # pdelay (plet (pasConstr # pd) $ \pair -> handleConstr # (pfstBuiltin # pair) # (psndBuiltin # pair))
      # pdelay (ptrace "Got a PlutusData Map" (handleOther # pd))
      # pdelay (handleList # (pasList # pd))
      # pdelay (handleInt # (pasInt # pd))
      # pdelay (ptrace "Got PlutusData Bytes" (handleOther # pd))

-- | Haskell `casePlutusData :: (Int -> [PlutusData] -> a) -> ([PlutusData] -> a) -> (Int -> a) -> (PlutusData -> a) -> PlutusData -> a`
casePlutusData ::
  (Term s PInteger -> Term s (PBuiltinList PData) -> Term s a) ->
  (Term s (PBuiltinList PData) -> Term s a) ->
  (Term s PInteger -> Term s a) ->
  (Term s PData -> Term s a) ->
  Term s PData ->
  Term s a
casePlutusData handleConstr handleList handleInt handleOther pd = pcasePlutusData # plam handleConstr # plam handleList # plam handleInt # plam handleOther # pd

-- | Plutarch `succeedParse :: a -> Parser a`
psucceedParse :: ClosedTerm (a :--> a)
psucceedParse = plam id

-- | Plutarch `failParse :: Parser a`
pfailParse :: ClosedTerm a
pfailParse = perror

-- | Plutarch `bindParse :: Parser a -> (a -> Parser b) -> Parser b`
pbindParse :: ClosedTerm (a :--> (a :--> b) :--> b)
pbindParse = phoistAcyclic $ plam (flip (#))

-- | PMaybe messed up in Plutarch so redefining here.
data PMaybe (a :: PType) (s :: S)
  = PJust (Term s (PAsData a))
  | PNothing
  deriving stock (Generic)
  deriving anyclass (PShow)

instance PlutusType (PMaybe a) where
  type PInner (PMaybe a) = PData
  pcon' (PJust x) = constrData 0 [toPlutusData x]
  pcon' PNothing = constrData 1 []
  pmatch' pd f =
    casePlutusData
      ( \ix args ->
          pif
            (ix #== 0)
            ( pmatch args \case
                PNil -> perror
                PCons h t -> pif (t #== (pcon PNil)) (f $ PJust (pfromPlutusDataPlutusType # h)) perror
            )
            ( pif
                (ix #== 1)
                ( pmatch args \case
                    PNil -> f PNothing
                    PCons _h _t -> perror
                )
                perror
            )
      )
      (const perror)
      (const perror)
      (const perror)
      pd

instance PTryFrom PData (PAsData a) => PTryFrom PData (PMaybe a) where
  type PTryFromExcess PData (PMaybe a) = Const ()
  ptryFrom' = ptryFromPAsData

instance PTryFrom PData (PAsData a) => PTryFrom PData (PAsData (PMaybe a)) where
  type PTryFromExcess PData (PAsData (PMaybe a)) = Const ()
  ptryFrom' pd f =
    f
      ( casePlutusData
          ( \ix args ->
              pif
                (ix #== 0)
                ( pmatch args \case
                    PNil -> perror
                    PCons h t ->
                      pif
                        (t #== pcon PNil)
                        (pdata . pcon $ PJust (pfromPlutusDataPTryFrom # h))
                        perror
                )
                ( pif
                    (ix #== 1)
                    ( pmatch args \case
                        PNil -> pdata . pcon $ PNothing
                        PCons _h _t -> perror
                    )
                    perror
                )
          )
          (const perror)
          (const perror)
          (const perror)
          pd
      , ()
      )

instance PIsData (PMaybe a) where
  pdataImpl = Plutarch.Unsafe.punsafeCoerce
  pfromDataImpl = Plutarch.Unsafe.punsafeCoerce
