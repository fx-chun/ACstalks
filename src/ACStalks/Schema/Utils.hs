module ACStalks.Schema.Utils (

) where

import Data.Aeson
import Data.Scientific (toBoundedInteger)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Time as Time

instance ToJSON Time.TimeZone where
    toJSON t = toJSON (Time.timeZoneMinutes t)

instance FromJSON Time.TimeZone where
    parseJSON = withScientific "TimeZone" f
        where
            f t = return (g t)
            g t = Time.minutesToTimeZone $ ((fromJust $ toBoundedInteger t) :: Int)


