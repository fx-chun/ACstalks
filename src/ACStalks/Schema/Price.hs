module ACStalks.Schema.Price (
    Price(..)
) where

import ACStalks.Schema.Utils
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Time as Time
import GHC.Generics

data Price = Price { priceId :: Int,
                     price :: Int,
                     priceTime :: Time.UTCTime,
                     priceTimezone :: Time.TimeZone,
                     priceUserId :: Int }
    deriving Generic

instance ToJSON Price

