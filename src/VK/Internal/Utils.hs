{-# LANGUAGE TypeFamilies #-}
-- |

module VK.Internal.Utils where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Types  (Parser)
import           Data.Char         (isLower)
import qualified Data.Text         as T

dropLPrefix :: String -> String
dropLPrefix = dropWhile isLower

aesonPhotoCase :: String -> String
aesonPhotoCase "Photo50" = "photo_50"
aesonPhotoCase "Photo100" = "photo_100"
aesonPhotoCase "Photo200" = "photo_200"
aesonPhotoCase "Photo200Orig" = "photo_200_orig"
aesonPhotoCase "Photo130" = "photo_130"
aesonPhotoCase "Photo160" = "photo_160"
aesonPhotoCase "Photo320" = "photo_320"
aesonPhotoCase "Photo400" = "photo_400"
aesonPhotoCase "Photo400Orig" = "photo_400_orig"
aesonPhotoCase "Photo640" = "photo_640"
aesonPhotoCase v = snakeCase v

tshow :: Show a => a -> T.Text
tshow = T.pack . show

intEnumFromJSON :: String -> [a] -> (Value -> Parser a)
intEnumFromJSON name vs =
  withScientific name
    (\v ->
       case (round v :: Int) of
       idx | idx >= length vs || idx < 0 ->
               fail $ (show idx) ++ " not in enum " ++ name
       idx ->
         pure $ vs !! idx
    )

