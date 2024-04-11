{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Onchain.Contracts
import Onchain.PSMTypes
import Plutus.Model

import qualified Cardano.Simple.Ledger.Tx as CSL
import Control.Monad.State
import Data.Foldable (foldl')
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import PlutusLedgerApi.V1.Value (valueOf)
import PlutusLedgerApi.V2 (BuiltinByteString, CurrencySymbol (..), Datum (..), FromData (fromBuiltinData), OutputDatum (..), PubKeyHash, ToData (toBuiltinData), TokenName (TokenName), TxOutRef, Value, singleton)
import PlutusLedgerApi.V2.Contexts (TxOut (..))
import PlutusTx.Builtins (blake2b_256, serialiseData)
import Test.Tasty (defaultMain)
import Text.Pretty.Simple

data TestState = TestState
  { tsScripts :: Maybe DeNSScripts
  , tsUsers :: Map String PubKeyHash
  , tsProtocolOutRef :: Maybe TxOutRef
  }

type TestM = StateT TestState Run

runTestIO :: String -> TestM a -> IO ()
runTestIO msg test =
  defaultMain $
    testNoErrorsTrace
      (adaValue 100000)
      defaultBabbageV2
      msg
      (runTest test)

runTest :: TestM a -> Run a
runTest test = evalStateT test initTestState
  where
    initTestState = TestState Nothing M.empty Nothing

main :: IO ()
main = runTestIO "setElemTest" $ do
  setElemInitTest 1000
  setElemMintTest
  recordValidatorTest

initialSetDatum :: SetDatum
initialSetDatum = SetDatum l h (CurrencySymbol "") -- might break? how long are these supposed to be?
  where
    l = DensKey densMin 0
    h = DensKey densMax 0
    densMin = ""
    densMax = "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"

initialSetInsert :: SetInsert
initialSetInsert = SetInsert'Insert (DensKey "" 0)

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

setElemInitTest :: Integer -> TestM ()
setElemInitTest amt = do
  lift $ logBalanceSheet
  initializeScriptsAndUser amt
  u1 <- user "user1"
  pRef <- protocolOutRef

  protocolPolicy <- protocolMintingPolicy <$> scripts
  oneProtocolNFT <- mpSingleton protocolMintingPolicy

  setVal <- setValidator <$> scripts

  setElemPolicy <- setElemMintingPolicy <$> scripts
  oneSetElemNFT <- mpSingleton setElemMintingPolicy

  DeNSScripts {..} <- scripts

  let protocolDatum =
        ProtocolDatum
          (scriptHash elemIDMintingPolicy)
          (scriptHash setElemMintingPolicy)
          (scriptHash setValidator)
          (scriptHash recordValidator)
      mintProtocol = mintValue protocolPolicy () oneProtocolNFT
      mintSetElem = mintValue setElemPolicy (initialSetInsert) oneSetElemNFT
      paySetDatumToScript = unsafePayToScript setVal initialSetDatum (oneSetElemNFT)
      spendPRef = spendPubKey pRef
      -- I guess we should probably send it to an alwaysFail validator to ensure it's never spent?
      sendProtocolDatumToValidator = unsafePayToScript setVal protocolDatum (oneProtocolNFT)
      refundExcess =
        payToKey u1 (adaValue amt)

      tx =
        mintProtocol
          <> mintSetElem
          <> paySetDatumToScript
          <> spendPRef
          <> refundExcess
          <> sendProtocolDatumToValidator

  lift $ do
    logInfo $ T.unpack (pShow tx)
    submitTx u1 tx
    logBalanceSheet
  pure ()

setElemMintTest :: TestM ()
setElemMintTest = do
  u1 <- user "user1"

  setElemSymbol <- withScriptCS setElemMintingPolicy $ T.unpack . pShow

  lift $ logInfo $ "SetElemSymbol:\n" <> setElemSymbol
  protocolRef <- findProtocolRefInput
  (setElemRef, SetDatum k nxt _) <- findOldSetDatum "www.google.com"

  oneSetElemNFT <- mpSingleton setElemMintingPolicy

  let setInsert = SetInsert'Insert (DensKey "www.google.com" 0)
      setInsertHash = blake2b_256 (serialiseData $ toBuiltinData (DensKey "www.google.com" 0))

  myElemIDNFT <- withScriptCS elemIDMintingPolicy $ \cs ->
    singleton cs (TokenName setInsertHash) 1

  setElemPolicy <- setElemMintingPolicy <$> scripts
  elemIDPolicy <- elemIDMintingPolicy <$> scripts
  setValidator <- setValidator <$> scripts

  let newSDL = SetDatum k (DensKey "www.google.com" 0) (CurrencySymbol "")
      newSDR = SetDatum (DensKey "www.google.com" 0) nxt (CurrencySymbol "")

      referToProtocol = refInputInline protocolRef

      mintNewSetElem = mintValue setElemPolicy setInsert oneSetElemNFT
      mintNewElemID = mintValue elemIDPolicy () myElemIDNFT

      spendOldSetDatum = spendScript setValidator setElemRef setInsert ()
      spendOldSetDatumUser = spendPubKey setElemRef
      sendNewSetElemLToScript = unsafePayToScript setValidator newSDL (oneSetElemNFT)
      sendNewSetElemRToScript = unsafePayToScript setValidator newSDR (oneSetElemNFT)

      collectElemID = payToKey u1 myElemIDNFT

      tx =
        spendOldSetDatumUser
          <> referToProtocol -- ref input, no mint/spend
          <> mintNewSetElem -- setElem +1
          <> mintNewElemID -- elemID +1
          <> spendOldSetDatum -- setElem +1
          <> sendNewSetElemLToScript -- setElem -1
          <> sendNewSetElemRToScript -- setElem -1
          <> collectElemID -- elemID -1
  lift $ do
    logInfo $ T.unpack (pShow tx)
    submitTx u1 tx
    logBalanceSheet
  where
    {- Find the utxo locked at the set validator w/ a setElemNFT and SetDatum such that
       k < new < nxt
    -}
    findOldSetDatum :: BuiltinByteString -> StateT TestState Run (TxOutRef, SetDatum)
    findOldSetDatum target = do
      setElemCS <- withScriptCS setElemMintingPolicy id
      DeNSScripts {..} <- scripts
      utxos <- lift $ utxoAt setValidator
      let result = foldl' go (Left ["No Match"]) utxos

          go :: Either [String] (TxOutRef, SetDatum) -> (TxOutRef, TxOut) -> Either [String] (TxOutRef, SetDatum)
          go (Right res) _ = Right res
          go (Left errs) (ref, TxOut {..})
            | valueOf txOutValue setElemCS (TokenName "") == 1 = case txOutDatum of
                OutputDatum (Datum inner) -> case fromBuiltinData @SetDatum inner of
                  Just sd@(SetDatum (DensKey l _) (DensKey r _) _)
                    | (l < target) && (target < r) -> Right (ref, sd)
                    | otherwise ->
                        Left $
                          prefixRef
                            ( "SetDatum: \n "
                                <> show sd
                                <> "\n doesn't match\n"
                                <> "(l < target)="
                                <> (show (l < target))
                                <> "(target < r)="
                                <> show (target < r)
                            )
                            : errs
                  _ -> Left $ prefixRef "Failed to fromData datum to a SetDatum" : errs
                _ -> Left $ prefixRef "Not an inline datum" : errs
            | otherwise = Left $ prefixRef "Doesn't contain exactly 1 setElemCS" : errs
            where
              prefixRef x = show ref <> " " <> x

      case result of
        Left es ->
          error $
            "Couldn't find a UTXO locked at validator with a SetDatum l < target < r in:\n"
              <> "SetElemCS: "
              <> (T.unpack $ pShow setElemCS)
              <> T.unpack (pShow utxos)
              <> "\n"
              <> T.unpack (pShow es)
        Right it -> pure it

{- Find the utxo locked at the set validator w/ a protocol NFT
-}
findProtocolRefInput :: StateT TestState Run TxOutRef
findProtocolRefInput = do
  lift $ logBalanceSheet
  protocolCS <- withScriptCS protocolMintingPolicy id
  DeNSScripts {..} <- scripts
  utxos <- lift $ utxoAt setValidator
  let result = find (\(_, TxOut {..}) -> valueOf txOutValue protocolCS (TokenName "") == 1) utxos
  case result of
    Nothing -> error $ "Couldn't find a protocol NFT at the validator. UTXOS at validator: " <> T.unpack (pShow utxos)
    Just (ref, _) -> pure ref

recordValidatorTest :: TestM ()
recordValidatorTest = do
  u1 <- user "user1"

  elemIDOutRef <- findElemIDOutRef

  let myDomain = blake2b_256 (serialiseData $ toBuiltinData (DensKey "www.google.com" 0))

  recValidator <- recordValidator <$> scripts

  myElemIDToken <- withScriptCS elemIDMintingPolicy $ \cs ->
    singleton cs (TokenName myDomain) 1

  protocolRef <- findProtocolRefInput

  let myRecordDatum = RecordDatum 0 myDomain (DensValue Nothing) u1

      referToProtocol = refInputInline protocolRef

      spendElemID = spendPubKey elemIDOutRef

      payValidator = payToScript recValidator (InlineDatum myRecordDatum) (adaValue 0)

      refundElemID = payToKey u1 myElemIDToken

      tx =
        referToProtocol
          <> spendElemID
          <> payValidator
          <> refundElemID

  lift $ do
    logInfo $ T.unpack (pShow tx)
    submitTx u1 tx
    logBalanceSheet
  where
    findElemIDOutRef :: TestM (TxOutRef)
    findElemIDOutRef = do
      u1 <- user "user1"
      let myDomain = blake2b_256 (serialiseData $ toBuiltinData (DensKey "www.google.com" 0))
      elemIDCS <- withScriptCS elemIDMintingPolicy id
      myUTXOs <- lift $ utxoAt u1
      let result = find (\(_, TxOut {..}) -> valueOf txOutValue elemIDCS (TokenName myDomain) == 1) myUTXOs
      case result of
        Nothing -> error $ "Couldn't find a matching elemID token in user UTXOs"
        Just res -> pure $ fst res
