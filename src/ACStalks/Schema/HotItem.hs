module ACStalks.Schema.HotItem (
    HotItem(..)
) where

import ACStalks.Schema.Utils
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Time as Time
import GHC.Generics

data HotItem = HotItem { hotItemId :: Int,
                         hotItem :: T.Text,
                         hotItemTime :: Time.UTCTime,
                         hotItemTimezone :: Time.TimeZone,
                         hotItemUserId :: Int
                         }
    deriving Generic

instance ToJSON HotItem

