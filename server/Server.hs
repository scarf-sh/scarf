{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lib
import qualified Models                     as DB

import qualified Data.ByteString            as BS
import           Data.Text                  (Text)
import           Data.Time.Clock
import           Database.Beam
import           Database.Beam.Postgres
import           Database.PostgreSQL.Simple (Connection)

connString = "postgres://avipress@127.0.0.1:5432/udb?sslmode=disable"

main = do
  conn <- connectPostgreSQL connString
  now <- getCurrentTime
  runBeamPostgres conn $ do
    runInsert $
      insert (DB._repoUsers DB.repoDb) $
      insertValues [DB.User 0 "mail@avi.press" "aviaviavi" Nothing "api_token" now Nothing]
  putStrLn "asdf"

