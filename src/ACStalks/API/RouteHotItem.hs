module ACStalks.API.RouteHotItem (
    HotItemApi,
    hotItemServer, 
) where

import ACStalks.Database
import ACStalks.Schema
import Control.Monad
import Control.Monad.IO.Class
import Crypto.BCrypt
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Maybe
import GHC.Generics
import Servant
import Text.Read (readEither)

type HotItemApi = "rand"
               :> QueryParam "n" Int
               :> Get '[JSON] [HotItem]
             :<|> "search"
               :> QueryParam "q" T.Text 
               :> Get '[JSON] [HotItem]
             :<|> "user"
               :> QueryParam "id" Int
               :> Get '[JSON] (Maybe HotItem)

hotItemServer :: DatabaseConnection -> Server HotItemApi
hotItemServer dbc = randomItems :<|> searchItems :<|> getByUser
    where
        randomItems :: Maybe Int -> Handler [HotItem] 
        randomItems n = liftIO $ getNRandomHotItems dbc (fromMaybe 10 n)

        searchItems :: Maybe T.Text -> Handler [HotItem]
        searchItems str = liftIO $ searchHotItems dbc (fromMaybe (T.pack "") str)

        getByUser :: Maybe Int -> Handler (Maybe HotItem)
        getByUser uid = liftIO $ getHotItemByUid dbc (fromMaybe (-1) uid)
