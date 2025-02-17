{-# LANGUAGE OverloadedStrings #-}

import Database.PostgreSQL.Simple
import qualified Data.ByteString as BS
import System.Environment (getArgs, getEnv)
import System.Exit (die)

-- Function to fetch DB connection info from environment
getConnInfo :: IO ConnectInfo
getConnInfo = do
  password <- getEnv "PGPASSWORD"  -- Read password from environment variable
  return defaultConnectInfo
    { connectHost = "45.124.54.206"
    , connectDatabase = "postgres"
    , connectUser = "postgres"
    , connectPassword = password
    }

readBinaryData :: FilePath -> IO ()
readBinaryData filePath = do
  connInfo <- getConnInfo
  conn <- connect connInfo
  results <- query_ conn "SELECT content FROM qgis_projects WHERE name = 'farm' LIMIT 1" :: IO [Only BS.ByteString]
  case results of
    (Only binaryData : _) -> BS.writeFile filePath binaryData
    [] -> putStrLn "No data found"
  close conn

writeBinaryData :: FilePath -> IO ()
writeBinaryData filePath = do
    connInfo <- getConnInfo
    conn <- connect connInfo
    newBinaryData <- BS.readFile filePath
    putStrLn $ "Writing data from file " <> show filePath <> " with length " <> show (BS.length newBinaryData)
    withTransaction conn $ do
        rowsAffected <- execute conn "UPDATE qgis_projects SET content = ? WHERE name = ?" (Binary newBinaryData, "farm" :: String)
        if rowsAffected > 0
            then putStrLn "Binary data written successfully."
            else putStrLn "No rows updated."
    close conn

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["read", filePath] -> readBinaryData filePath
        ["write", filePath] -> writeBinaryData filePath
        _ -> die "Usage: program (read|write) <file_path>"
