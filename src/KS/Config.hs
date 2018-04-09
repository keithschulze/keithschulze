{-# LANGUAGE OverloadedStrings #-}
module KS.Config where

import Hakyll

data EngineConfiguration = EngineConfiguration
    { lessCommand :: String
    , lessFiles :: [Identifier]
    , lessOptions :: [String]
    }

defaultEngineConfig :: EngineConfiguration
defaultEngineConfig = EngineConfiguration
  { lessCommand = "node_modules/less/bin/lessc"
  , lessFiles = [ "css/default.less" ]
  , lessOptions = []
  }
