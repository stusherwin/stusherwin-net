{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App where
 
import qualified Data.ByteString as B (ByteString)
import           Control.Monad.IO.Class (liftIO)
import           Data.Text (Text)
import qualified Data.Text as T (unpack)
import qualified Data.Text.Lazy as TL (toStrict)
import qualified Data.Text.Lazy.Encoding as TLE (decodeUtf8)
import           Network.HTTP.Types (hContentType, status200)
import           Network.Wai (responseFile)
import           Network.Wai.Middleware.Static (staticPolicy, addBase)
import           Servant.Multipart (generalOptions, defaultMultipartOptions, Mem)
import           Network.Wai.Parse (clearMaxHeaderLines, clearMaxHeaderLineLength, defaultParseRequestBodyOptions)
import           Servant

import Data.Maybe (listToMaybe, fromMaybe)
import Data.Aeson

import Api
import Config

app :: Config -> Application
app config = serveWithContext fullApi ctxt $ server config
  where ctxt = multipartOpts :. EmptyContext
        multipartOpts = (defaultMultipartOptions (Proxy :: Proxy Mem))
          { generalOptions = clearMaxHeaderLines $
                             clearMaxHeaderLineLength $
                             defaultParseRequestBodyOptions 
          }
         
server :: Config -> Server Api
server config = 
  appServer

appServer :: Server Raw
appServer = Tagged (staticPolicy (addBase "dist") indexPage)
  where
    indexPage :: Application
    indexPage _ respond = respond $ 
      responseFile status200
                   [(hContentType, "text/html")]
                   "dist/index.html"
               Nothing