{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Config where
  import           Data.ByteString (ByteString)
  import qualified Data.ByteString.Char8 as B (pack)
  import           Control.Exception (SomeException(..), catch)
  import           System.Environment (getEnv, getArgs)

  data Config = Config { port :: Int
                       } deriving (Read, Show)

  getConfig :: String -> IO Config
  getConfig configFile = catch (read <$> readFile configFile) $ \(SomeException _) -> do
    args <- getArgs
    let port = case args of
                (a:_) -> read $ a
                _     -> 0
    putStrLn $ "port: " ++ (show port)
    return $ Config port