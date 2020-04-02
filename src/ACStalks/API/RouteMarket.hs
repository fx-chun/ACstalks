module ACStalks.API.RouteMarket (
    MarketApi,
    marketServer, 
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

type MarketApi = "top"
                  :> QueryParam "sort" PriceSortOrder 
                  :> QueryParam "n" Int
                  :> Get '[JSON] [Price]
            :<|> "user"
                  :> QueryParam "id" Int
                  :> Get '[JSON] (Maybe Price)

marketServer :: DatabaseConnection -> Server MarketApi
marketServer dbc = topPrices :<|> byUser
    where
        topPrices :: Maybe PriceSortOrder -> Maybe Int -> Handler [Price] 
        topPrices o n = liftIO $ getNPrices dbc (fromMaybe 10 n) (fromMaybe PriceDescending o)

        byUser :: Maybe Int -> Handler (Maybe Price) 
        byUser (Just uid) = liftIO $ (getPriceByUid dbc uid)
        byUser (Nothing)  = return Nothing 
            
instance FromHttpApiData PriceSortOrder where
    parseQueryParam = parseQueryParam' . T.unpack  
        where
            parseQueryParam' "asc"  = (Right PriceAscending)
            parseQueryParam' "desc" = (Right PriceDescending)
            parseQueryParam' _      = (Left $ T.pack "RouteMarket: query param is not asc or desc")
            
