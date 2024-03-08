{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Onchain.PSMTypes where

import PlutusCore.Data (Data (..))
import PlutusLedgerApi.V2 (CurrencySymbol, PubKeyHash)
import PlutusTx.Builtins.Internal (BuiltinData (..))
import PlutusTx.IsData
import PlutusTx.Prelude

data DensKey = DensKey BuiltinByteString Integer

data DensValue = DensValue (Maybe BuiltinByteString)

data RecordDatum = RecordDatum Integer BuiltinByteString DensValue PubKeyHash

data SetDatum = SetDatum DensKey DensKey CurrencySymbol

data SetInsert = SetInsert'Insert DensKey

instance ToData DensKey where
  toBuiltinData (DensKey bs i) = BuiltinData $ Constr 0 [toData bs, toData i]

instance FromData DensKey where
  fromBuiltinData (BuiltinData dInner) = case dInner of
    Constr 0 [bs, i] -> DensKey <$> fromData bs <*> fromData i
    _ -> Nothing

instance ToData DensValue where
  toBuiltinData (DensValue mbs) = BuiltinData $ Constr 0 [toData mbs]

instance FromData DensValue where
  fromBuiltinData (BuiltinData dInner) = case dInner of
    Constr 0 [mbs] -> DensValue <$> fromData mbs
    _ -> Nothing

instance ToData RecordDatum where
  toBuiltinData (RecordDatum i bs dval pkh) =
    BuiltinData $ Constr 0 [toData i, toData bs, toData dval, toData pkh]

instance FromData RecordDatum where
  fromBuiltinData (BuiltinData dInner) = case dInner of
    Constr 0 [i, bs, dval, pkh] ->
      RecordDatum <$> fromData i <*> fromData bs <*> fromData dval <*> fromData pkh
    _ -> Nothing

instance ToData SetDatum where
  toBuiltinData (SetDatum k1 k2 cs) = BuiltinData $ Constr 0 [toData k1, toData k2, toData cs]

instance FromData SetDatum where
  fromBuiltinData (BuiltinData dInner) = case dInner of
    Constr 0 [k1, k2, cs] -> SetDatum <$> fromData k1 <*> fromData k2 <*> fromData cs
    _ -> Nothing

instance ToData SetInsert where
  toBuiltinData (SetInsert'Insert dKey) = BuiltinData $ Constr 0 [toData dKey]

instance FromData SetInsert where
  fromBuiltinData (BuiltinData dInner) = case dInner of
    Constr 0 [dKey] -> SetInsert'Insert <$> fromData dKey
    _ -> Nothing
