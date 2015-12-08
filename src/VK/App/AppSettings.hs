-- |

module VK.App.AppSettings where

import           VK.API

appId :: Integer
appId = 5082615

appScope :: [AuthPermissions]
appScope = [Audio, Status]
