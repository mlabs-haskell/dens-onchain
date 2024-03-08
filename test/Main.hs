{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Onchain.Contracts
import Onchain.PSMTypes
import Plutus.Model

import qualified Cardano.Simple.Ledger.Tx as CSL
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import PlutusLedgerApi.V2 (CurrencySymbol (..), Datum (..), OutputDatum (..), PubKeyHash, ToData (toBuiltinData), TxOut (TxOut), TxOutRef, Value, singleton)
import PlutusTx.IsData (ToData)
import qualified PlutusTx.Prelude as P

data TestState = TestState
  { tsScripts :: Maybe DeNSScripts
  , tsUsers :: Map String PubKeyHash
  , tsProtocolOutRef :: Maybe TxOutRef
  }

type TestM = StateT TestState Run

main :: IO ()
main = putStrLn "Test suite not yet implemented."

initialSetDatum :: SetDatum
initialSetDatum = SetDatum l h (CurrencySymbol "") -- might break? how long are these supposed to be?
  where
    l = DensKey densMin 0
    h = DensKey densMax 0
    densMin = ""
    densMax = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

initializeScriptsAndUser :: Integer -> TestM ()
initializeScriptsAndUser amt = do
  u1 <- lift $ newUser $ adaValue amt
  ref <- getHeadRef <$> lift (spend u1 (adaValue amt)) -- maybe like amt-1?
  let newScripts = mkDeNSScripts ref
  modify' $ \(TestState _ users _) -> TestState (Just newScripts) (M.insert "user1" u1 users) (Just ref)

-- unsafe
user :: String -> TestM PubKeyHash
user name =
  gets tsUsers >>= \users -> case M.lookup name users of
    Nothing -> lift $ fail ("No user named " <> name)
    Just u -> pure u

-- unsafe
scripts :: TestM DeNSScripts
scripts =
  gets tsScripts >>= \case
    Nothing -> lift $ fail ("Scripts not initialized yet!")
    Just s -> pure s

-- unsafe
protocolOutRef :: TestM TxOutRef
protocolOutRef =
  gets tsProtocolOutRef
    >>= maybe
      (lift $ fail "Protocol OutRef not initialized")
      pure

withScriptCS ::
  forall a r.
  (DeNSScripts -> TypedPolicy a) ->
  (CurrencySymbol -> r) ->
  TestM r
withScriptCS f g = g . scriptCurrencySymbol . f <$> scripts

singleton' :: CurrencySymbol -> Value
singleton' cs = singleton cs "" 1

mpSingleton :: (DeNSScripts -> TypedPolicy a) -> TestM Value
mpSingleton x = withScriptCS x singleton'

-- inline
unsafePayToScript ::
  (HasAddress script, ToData a) =>
  script ->
  a ->
  Value ->
  Tx
unsafePayToScript script dat val =
  toExtra $
    mempty
      { CSL.txOutputs = [TxOut (toAddress script) val outDatum Nothing]
      , CSL.txData = datumMap
      }
  where
    datum = Datum $ toBuiltinData dat
    dh = datumHash datum
    (outDatum, datumMap) = (OutputDatum datum, M.singleton dh datum)

setElemTest :: Integer -> TestM ()
setElemTest amt = do
  initializeScriptsAndUser amt
  u1 <- user "user1"
  pRef <- protocolOutRef

  protocolPolicy <- protocolMintingPolicy <$> scripts
  oneProtocolNFT <- mpSingleton protocolMintingPolicy

  setVal <- setValidator <$> scripts

  let mintProtocol = mintValue protocolPolicy () oneProtocolNFT
      paySetDatumToScript = unsafePayToScript setVal initialSetDatum (adaValue 100)
      spendPRef = spendPubKey pRef
      refundExcess = payToKey u1 (adaValue $ amt - 100)

      tx = mintProtocol <> paySetDatumToScript <> spendPRef <> refundExcess
  pure ()
