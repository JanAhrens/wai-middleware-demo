{-# LANGUAGE OverloadedStrings #-}

module CancelRequestOnMissingParameter(main) where

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.IO.Class (liftIO)
import Network.Wai (Request(..), Response(..), responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Parse ( getRequestBodyType, sinkRequestBody
                         , lbsBackEnd, fileName, Param, File)
import Network.HTTP.Types (statusOK, statusPreconditionFailed)

main :: IO ()
main = run 3000 (requireParameter ("version", "1") application)
  where
    application _ =return $
      responseLBS statusOK [("Content-Type", "text/plain")] "Hello World"

requireParameter requiredParameter app env = do
  body       <- extractBody
  postParams <- extractPostParams env body
  getParams  <- extractGetParams  env

  case parameterPresent requiredParameter (getParams ++ postParams) of
    Just _  -> app env { requestBody = CL.sourceList body }
    Nothing -> return $
      responseLBS statusPreconditionFailed
        [("Content-Type", "application/json")]
        "{\"code\": \"PARAMETER_MISSING\"}"
  where
    parameterPresent (paramName, paramValue) params = do
      result <- lookup paramName params
      if result == paramValue
        then Just result
        else Nothing
    extractBody = requestBody env C.$$ CL.consume

extractPostParams env body =
  if requestMethod env `elem` ["GET", "HEAD"]
    then return []
    else do postParams <- liftIO allPostParams
            return $ collectPostParams postParams
  where
    allPostParams = case getRequestBodyType env of
      Nothing  -> return ([], [])
      Just rbt -> C.runResourceT $ CL.sourceList body C.$$ sinkRequestBody lbsBackEnd rbt
    collectPostParams (postParams, files) = postParams ++
      map (\(k,v) -> (k, BS.append "FILE: " (fileName v))) files

extractGetParams env = return $ map emptyGetParams $ queryString env
  where
    emptyGetParams (k, Just v)  = (k, v)
    emptyGetParams (k, Nothing) = (k, "")
