{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Gist
    ( API
    , GistAPI
    , getGists
    , createNewGist
    , getGist
    , updateGist
    , Owner(..)
    , GistId(..)
    , Gist(..)
    , GistFile(..)
    , NewGist(..)
    , NewGistFile(..)
    , PatchGist(..)
    , PatchGistFile(..)
    ) where

import Auth.Types (Token, TokenProvider (Github))
import Data.Aeson (FromJSON, GFromJSON, KeyValue, ToJSON, Value, Zero, genericParseJSON, object, parseJSON, toJSON,
                   withObject, (.!=), (.:), (.:?), (.=))
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (Parser)
import Data.Bifunctor (Bifunctor (first), bimap)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic, Rep)
import Servant.API (Capture, FromHttpApiData (parseQueryParam), Get, Header, JSON, Patch, PostCreated, ReqBody,
                    ToHttpApiData (toQueryParam), (:<|>), (:>))
import Servant.Client (ClientM, client)
import qualified Servant.Extra
import Text.Read (readEither)

type API = Header "Authorization" (Token 'Github) :> "gists" :> GistAPI

type GistAPI
       -- See https://docs.github.com/en/rest/gists/gists?apiVersion=2022-11-28#list-gists-for-the-authenticated-user
     = Get '[ JSON] [Gist]
       -- See https://docs.github.com/en/rest/gists/gists?apiVersion=2022-11-28#create-a-gist
       :<|> ReqBody '[ JSON] NewGist :> PostCreated '[ JSON] Gist
       -- See https://docs.github.com/en/rest/gists/gists?apiVersion=2022-11-28#get-a-gist
       :<|> Capture "GistId" GistId :> Get '[ JSON] Gist
       -- See https://docs.github.com/en/rest/gists/gists?apiVersion=2022-11-28#update-a-gist
       :<|> Capture "GistId" GistId :> ReqBody '[ JSON] PatchGist :> Patch '[ JSON] Gist

apiClient ::
       Maybe (Token 'Github)
    -> ClientM [Gist]
       :<|> (NewGist -> ClientM Gist)
       :<|> (GistId -> ClientM Gist)
       :<|> (GistId -> PatchGist -> ClientM Gist)
apiClient = client (Proxy @API)

getGists :: Maybe (Token 'Github) -> ClientM [Gist]
getGists = Servant.Extra.left . apiClient

createNewGist :: Maybe (Token 'Github) -> NewGist -> ClientM Gist
createNewGist = Servant.Extra.left . Servant.Extra.right . apiClient

getGist :: Maybe (Token 'Github) -> GistId -> ClientM Gist
getGist =
    Servant.Extra.left . Servant.Extra.right . Servant.Extra.right . apiClient

updateGist :: Maybe (Token 'Github) -> GistId -> PatchGist -> ClientM Gist
updateGist =
    Servant.Extra.right . Servant.Extra.right . Servant.Extra.right . apiClient

------------------------------------------------------------
data Owner =
    Owner
        { _ownerLogin   :: !Text
        , _ownerHtmlUrl :: !Text
        }
    deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Owner where
    parseJSON = githubParseJSON

data NewGist =
    NewGist
        { _newGistDescription :: !Text
        , _newGistPublic      :: !Bool
        , _newGistFiles       :: ![NewGistFile]
        }
    deriving (Show, Eq, Generic, FromJSON)

instance ToJSON NewGist where
    toJSON NewGist {..} =
        object
            [ "description" .= _newGistDescription
            , "public" .= _newGistPublic
            , "files" .= object (first Key.fromText . toPair <$> _newGistFiles)
            ]
      where
        toPair NewGistFile {..} =
            (_newGistFilename, object ["content" .= _newGistFileContent])

data NewGistFile =
    NewGistFile
        { _newGistFilename    :: !Text
        , _newGistFileContent :: !Text
        }
    deriving (Show, Eq, Generic, FromJSON)

data PatchGist =
    PatchGist
        { _patchGistDescription :: !(Maybe Text)
        , _patchGistFiles       :: !(Maybe (Map Text PatchGistFile))
        }
    deriving (Show, Eq, Generic, FromJSON)

data PatchGistFile =
    PatchGistFile
        { _patchGistFilename    :: !(Maybe Text)
        , _patchGistFileContent :: !(Maybe Text)
        }
    deriving (Show, Eq, Generic, FromJSON)

(?=) :: (KeyValue b, ToJSON v) => Key.Key -> Maybe v -> Maybe b
k ?= mv = (k .=) <$> mv

instance ToJSON PatchGist where
    toJSON PatchGist {..} =
        object $ catMaybes
            [ "description" ?= _patchGistDescription
            , "files" ?= _patchGistFiles
            ]

instance ToJSON PatchGistFile where
    toJSON PatchGistFile {..} =
        object $ catMaybes
            [ "content" ?= _patchGistFileContent
            , "filename" ?= _patchGistFilename
            ]

newtype GistId =
    GistId Text
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

instance ToHttpApiData GistId where
    toQueryParam (GistId gistId) = gistId

instance FromHttpApiData GistId where
    parseQueryParam = bimap T.pack GistId . readEither . T.unpack

data Gist =
    Gist
        { _gistId          :: !GistId
        , _gistGitPushUrl  :: !Text
        , _gistHtmlUrl     :: !Text
        , _gistOwner       :: !Owner
        , _gistFiles       :: !(Map String GistFile)
        , _gistTruncated   :: !Bool
        , _gistCreatedAt   :: !String
        , _gistUpdatedAt   :: !String
        , _gistDescription :: !String
        }
    deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Gist where
    parseJSON =
        withObject "gist" $ \o -> do
            _gistId <- o .: "id"
            _gistGitPushUrl <- o .: "git_push_url"
            _gistHtmlUrl <- o .: "html_url"
            _gistOwner <- o .: "owner"
            _gistFiles <- o .: "files"
            _gistTruncated <- o .: "truncated"
            _gistCreatedAt <- o .: "created_at"
            _gistUpdatedAt <- o .: "updated_at"
            -- playground gists will always have a description but to avoid breaking with non-playground gists we change null to empty string
            _gistDescription <- o .:? "description" .!= ""
            pure Gist {..}

data GistFile =
    GistFile
        { _gistFileFilename  :: !Text
        , _gistFileLanguage  :: !(Maybe Text)
        , _gistFileType      :: !Text
        , _gistFileTruncated :: !(Maybe Bool)
        , _gistFileContent   :: !(Maybe Text)
        }
    deriving (Show, Eq, Generic, ToJSON)

instance FromJSON GistFile where
    parseJSON =
        withObject "gistfile" $ \o -> do
            _gistFileFilename <- o .: "filename"
            _gistFileLanguage <- o .:? "language"
            _gistFileType <- o .: "type"
            _gistFileTruncated <- o .:? "truncated"
            _gistFileContent <- o .:? "content"
            pure GistFile {..}

------------------------------------------------------------
githubParseJSON :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
githubParseJSON = genericParseJSON $ aesonPrefix snakeCase
