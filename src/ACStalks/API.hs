module ACStalks.API (
   startServer 
) where

import ACStalks.API.RouteUser
import ACStalks.API.RouteMarket
import ACStalks.API.RouteHotItem
import ACStalks.API.RouteAdmin
import qualified Data.Text as T
import Network.Wai.Handler.Warp
import Servant
import Servant.Server.StaticFiles
import WaiAppStatic.Types
import WaiAppStatic.Storage.Filesystem

type API = "api" :> "user"    :> UserApi
      :<|> "api" :> "market"  :> MarketApi
      :<|> "api" :> "hotitem" :> HotItemApi
      :<|> "api" :> "admin" :> AdminApi
      :<|> Raw

api :: Proxy API 
api = Proxy

extendedSettings =  
    (defaultFileServerSettings "frontend/_site/") {
        ssListing = Nothing
    }

startServer dbc port = run port. serve api 
                     $ userServer dbc
                  :<|> marketServer dbc
                  :<|> hotItemServer dbc
                  :<|> adminServer dbc
                  :<|> serveDirectoryWith extendedSettings
