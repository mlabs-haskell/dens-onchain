{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Onchain.PSMTypes where

import PlutusLedgerApi.V2 (BuiltinData (BuiltinData), CurrencySymbol, Data (List), PubKeyHash, ScriptHash)

import PlutusTx.IsData (
  FromData (..),
  ToData (..),
  fromData,
  toData,
 )
import PlutusTx.Prelude (
  Applicative ((<*>)),
  BuiltinByteString,
  Integer,
  Maybe (Nothing),
  ($),
  (<$>),
 )
import Prelude (Show)

data DensKey = DensKey BuiltinByteString Integer deriving (Show)

data DensValue = DensValue (Maybe BuiltinByteString)

data RecordDatum = RecordDatum Integer BuiltinByteString DensValue PubKeyHash

data SetDatum = SetDatum DensKey DensKey CurrencySymbol deriving (Show)

data SetInsert = SetInsert'Insert DensKey

data ProtocolDatum = ProtocolDatum
  { pdElemIDMP :: ScriptHash
  , pdSetElemMP :: ScriptHash
  , pdSetVal :: ScriptHash
  , pdRecVal :: ScriptHash
  }

instance ToData ProtocolDatum where
  toBuiltinData (ProtocolDatum a b c d) = BuiltinData $ List [toData a, toData b, toData c, toData d]

instance FromData ProtocolDatum where
  fromBuiltinData (BuiltinData dInner) = case dInner of
    List [a, b, c, d] -> ProtocolDatum <$> fromData a <*> fromData b <*> fromData c <*> fromData d
    _ -> Nothing

instance ToData DensKey where
  toBuiltinData (DensKey bs i) = BuiltinData $ List [toData bs, toData i]

instance FromData DensKey where
  fromBuiltinData (BuiltinData dInner) = case dInner of
    List [bs, i] -> DensKey <$> fromData bs <*> fromData i
    _ -> Nothing

instance ToData DensValue where
  toBuiltinData (DensValue mbs) = BuiltinData $ List [toData mbs]

instance FromData DensValue where
  fromBuiltinData (BuiltinData dInner) = case dInner of
    List [mbs] -> DensValue <$> fromData mbs
    _ -> Nothing

instance ToData RecordDatum where
  toBuiltinData (RecordDatum i bs dval pkh) =
    BuiltinData $ List [toData i, toData bs, toData dval, toData pkh]

instance FromData RecordDatum where
  fromBuiltinData (BuiltinData dInner) = case dInner of
    List [i, bs, dval, pkh] ->
      RecordDatum <$> fromData i <*> fromData bs <*> fromData dval <*> fromData pkh
    _ -> Nothing

instance ToData SetDatum where
  toBuiltinData (SetDatum k1 k2 cs) = BuiltinData $ List [toData k1, toData k2, toData cs]

instance FromData SetDatum where
  fromBuiltinData (BuiltinData dInner) = case dInner of
    List [k1, k2, cs] -> SetDatum <$> fromData k1 <*> fromData k2 <*> fromData cs
    _ -> Nothing

instance ToData SetInsert where
  toBuiltinData (SetInsert'Insert dKey) = BuiltinData $ List [toData dKey]

instance FromData SetInsert where
  fromBuiltinData (BuiltinData dInner) = case dInner of
    List [dKey] -> SetInsert'Insert <$> fromData dKey
    _ -> Nothing
