module Main (main) where

import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import GHC.IO.Unsafe (unsafePerformIO)
import Control.Exception (bracket)
import qualified Data.ByteString.Lazy as LB
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.IO (hGetLine)
import Test.Hspec
  ( hspec
  , shouldBe
  , it
  )

import Database.SQLServer
  ( ConnectionInfo(..)
  , Connection
  , Only(..)
  , FieldParser
  , query
  , queryWith
  , execute
  , withConnection
  , mkQuery_
  , rawBytes
  , parseFieldWith
  , parseField
  )
import Util.Docker (DockerCommand(..), withDocker)
import Util.Delay (deadline, seconds)

main :: IO ()
main =
  withMicrosoftSQLServer $ \cinfo ->
  hspec $ do
    it "connects" $ do
      withConnection cinfo $ \_ -> return ()

    it "executes command" $
      withConnection cinfo $ \conn -> do
      execute conn $ mkQuery_ "CREATE TABLE tests_execute (id INT, value TEXT)"
      execute conn $ mkQuery_ "DROP TABLE tests_execute"

    it "queries" $
      withConnection cinfo $ \conn ->
      withTable conn (SQLType "INT") $ \(Table table) -> do
      execute conn $ mkQuery_ $ unwords
        [ "INSERT INTO ", table, " (value) VALUES (1), (2), (3)"]
      rs <- query conn $ mkQuery_ $ unwords [ "SELECT value from", table ]
      rs `shouldBe` ([Only 1, Only 2, Only 3] :: [Only Int])

    it "queries with parser" $
      withConnection cinfo $ \conn ->
      withTable conn (SQLType "TEXT") $ \(Table table) -> do
      execute conn $ mkQuery_ $ unwords
        [ "INSERT INTO ", table, " (value) VALUES ('a'), ('b'), ('c')"]
      let parser = Only <$> parseFieldWith parseText
      rs <- queryWith parser conn $ mkQuery_ $ unwords [ "SELECT value from", table ]
      rs `shouldBe` ([Only "a", Only "b", Only "c"] :: [Only Text])

    it "structured type parsing" $
      withConnection cinfo $ \conn ->
      withTable conn (SQLType "TEXT") $ \(Table table) -> do
      execute conn $ mkQuery_ $ unwords
        [ "INSERT INTO ", table, " (value) VALUES ('a'), ('b'), ('c')"]
      let parser = (,) <$> parseField <*> parseFieldWith parseText
      rs <- queryWith parser conn $ mkQuery_ $ unwords [ "SELECT * from", table ]
      rs `shouldBe` ([(1, "a"), (2, "b"), (3, "c")] :: [(Int, Text)])

parseText :: FieldParser Text
parseText = Text.decodeUtf8 . LB.toStrict <$> rawBytes

-- | Binary data saved in MySQL.
-- Use it to read and write binary data. Does not perform base64 conversion.
newtype SQLType = SQLType String

data Table = Table String

withTable :: Connection -> SQLType -> (Table -> IO a) -> IO a
withTable conn (SQLType ty) f =
  withSQLServerTable conn schema $ \name -> f (Table name)
  where
  schema = unwords
      [ "( id    INT IDENTITY(1,1) PRIMARY KEY"
      , ", value " <> ty
      , ")"
      ]

type Schema = String

{-# NOINLINE tableNumber #-}
tableNumber :: MVar Int
tableNumber = unsafePerformIO (newMVar 0)

mkTableName :: IO String
mkTableName = do
  number <- modifyMVar tableNumber $ \n -> return (n + 1, n)
  return $ "table_" <> show number

-- | Creates a new table on every invocation.
withSQLServerTable :: Connection -> Schema -> (String -> IO a) -> IO a
withSQLServerTable conn schema f = bracket create destroy f
  where
  create = do
    name <- mkTableName
    execute conn $ mkQuery_ $ unwords [ "CREATE TABLE", name, schema ]
    return name

  destroy name =
    execute conn $ mkQuery_ $ "DROP TABLE " <> name

-- | Use MicrosoftSQLServer from Docker.
withMicrosoftSQLServer :: (ConnectionInfo -> IO a) -> IO a
withMicrosoftSQLServer f = do
  let cmd = DockerRun
        { run_image = "mcr.microsoft.com/azure-sql-edge:latest"
        , run_args =
          [ "--env", "ACCEPT_EULA=" <> "Y"
          , "--env", "MSSQL_SA_PASSWORD=" <> Text.unpack conn_password
          , "--env", "MSSQL_AGENT_ENABLED=TRUE"
          , "--env", "ClientTransportType=AMQP_TCP_Only"
          , "--env", "MSSQL_PID=Premium"
          , "--publish",  show conn_port <> ":1433"
          ]
        }
  r <- withDocker False "MicrosoftSQLServer" cmd $ \h -> do
    waitTillReady h
    f creds
  return r
  where
  waitTillReady h = do
    putStrLn "Waiting for MicrosoftSQLServer docker..."
    deadline (seconds 60) $ do
      whileM $ do
        line <- hGetLine h
        return $ not $ isReadyNotice line
    putStrLn "MicrosoftSQLServer docker is ready."

  whileM m = do
    r <- m
    if r then whileM m else return ()

  isReadyNotice str =
    "EdgeTelemetry starting up" `isInfixOf` str

  creds@ConnectionInfo{..} = ConnectionInfo
    { conn_database = "master"
    , conn_username = "sa"
    , conn_password = "TestPass1234"
    , conn_host =  "0.0.0.0"
    , conn_port = 6666
    }

