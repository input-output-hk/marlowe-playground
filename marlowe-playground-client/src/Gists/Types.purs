module Gists.Types
  ( GistAction(..)
  , parseGistUrl
  ) where

import Component.ErrorPane (ErrorPaneAction)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either, note)
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (ignoreCase)
import Gists.Extra (GistId(..))
import Prelude (bind, ($), (<$>))

data GistAction
  = PublishOrUpdateGist
  | SetGistUrl String
  | LoadGist
  | ErrorPaneAction ErrorPaneAction

gistIdInLinkRegex :: Either String Regex
gistIdInLinkRegex = regex "^(.*/)?([0-9a-f]{32})$" ignoreCase

parseGistUrl :: String -> Either String GistId
parseGistUrl str = do
  gistIdInLink <- gistIdInLinkRegex
  note "Could not parse Gist Url"
    $ do
        matches <- match gistIdInLink str
        match <- NonEmptyArray.index matches 2
        GistId <$> match
