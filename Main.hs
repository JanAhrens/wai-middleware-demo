{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)

main = run 3000 (addResponseHeaders headers application)
  where
    headers = [("X-Message", "Hello from the WAI middleware")]

application _ = return $
  responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

addResponseHeaders header app env = do
  res <- app env
  let (s, hs, b) = responseSource res
  return $ ResponseSource s (header ++ hs) b
