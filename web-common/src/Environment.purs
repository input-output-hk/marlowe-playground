module Environment where

import Prologue

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)

data Environment
  = Browser
  | NodeJs

derive instance Eq Environment
derive instance Ord Environment
derive instance Generic Environment _

instance Show Environment where
  show = genericShow

foreign import detectEnvironment
  :: Environment -> Environment -> Effect Environment

currentEnvironment :: Effect Environment
currentEnvironment = detectEnvironment Browser NodeJs
