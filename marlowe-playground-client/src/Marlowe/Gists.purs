module Marlowe.Gists
  ( mkNewGist
  , mkPatchGist
  , isPlaygroundGist
  , playgroundFiles
  , filenames
  , fileExists
  , PlaygroundFiles
  ) where

import Prologue

import Blockly.Internal (XML)
import Data.Array (catMaybes)
import Data.Lens (has, view)
import Data.Lens.Index (ix)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap, wrap)
import Gist
  ( Gist
  , NewGist(..)
  , NewGistFile(..)
  , PatchGist(..)
  , PatchGistFile(..)
  , gistFileContent
  , gistFiles
  )

mkNewGist :: String -> PlaygroundFiles -> NewGist
mkNewGist description files =
  NewGist
    { _newGistDescription: description
    , _newGistPublic: true
    , _newGistFiles: playgroundFilesToNewGistFiles files
    }

mkNewGistFile :: String -> String -> NewGistFile
mkNewGistFile _newGistFilename _newGistFileContent =
  NewGistFile
    { _newGistFilename
    , _newGistFileContent
    }

mkPatchGist :: String -> PlaygroundFiles -> PatchGist
mkPatchGist description files =
  PatchGist
    { _patchGistDescription: Just description
    , _patchGistFiles: Just $ playgroundFilesToPatchGistFiles files
    }

mkPatchGistFile :: String -> String -> Tuple String PatchGistFile
mkPatchGistFile filename content = Tuple filename $
  PatchGistFile
    { _patchGistFilename: Just filename
    , _patchGistFileContent: Just content
    }

type PlaygroundFiles =
  { playground :: String
  , marlowe :: Maybe String
  , haskell :: Maybe String
  , blockly :: Maybe String
  , javascript :: Maybe String
  , actus :: Maybe XML
  , metadata :: Maybe String
  }

playgroundFilesToNewGistFiles :: PlaygroundFiles -> Array NewGistFile
playgroundFilesToNewGistFiles
  { playground, marlowe, haskell, blockly, javascript, actus, metadata } =
  [ mkNewGistFile filenames.playground playground
  ]
    <> catMaybes
      [ mkNewGistFile filenames.marlowe <$> marlowe
      , mkNewGistFile filenames.haskell <$> haskell
      , mkNewGistFile filenames.blockly <$> blockly
      , mkNewGistFile filenames.javascript <$> javascript
      , mkNewGistFile filenames.actus <<< unwrap <$> actus
      , mkNewGistFile filenames.metadata <$> metadata
      ]

playgroundFilesToPatchGistFiles :: PlaygroundFiles -> Map String PatchGistFile
playgroundFilesToPatchGistFiles
  { playground, marlowe, haskell, blockly, javascript, actus, metadata } =
  Map.fromFoldable $
    [ mkPatchGistFile filenames.playground playground
    ]
      <> catMaybes
        [ mkPatchGistFile filenames.marlowe <$> marlowe
        , mkPatchGistFile filenames.haskell <$> haskell
        , mkPatchGistFile filenames.blockly <$> blockly
        , mkPatchGistFile filenames.javascript <$> javascript
        , mkPatchGistFile filenames.actus <<< unwrap <$> actus
        , mkPatchGistFile filenames.metadata <$> metadata
        ]

filenames
  :: { playground :: String
     , marlowe :: String
     , haskell :: String
     , blockly :: String
     , javascript :: String
     , actus :: String
     , metadata :: String
     }
filenames =
  { playground: "playground.marlowe.json"
  , marlowe: "playground.marlowe"
  , haskell: "Main.hs"
  , blockly: "playground.marlowe"
  , javascript: "playground.js"
  , actus: "actus.xml"
  , metadata: "metadata.json"
  }

isPlaygroundGist :: Gist -> Boolean
isPlaygroundGist = has (gistFiles <<< ix filenames.playground)

playgroundFiles :: Gist -> PlaygroundFiles
playgroundFiles gist =
  { playground: fromMaybe "{}" $ getFile filenames.playground
  , marlowe: getFile filenames.marlowe
  , haskell: getFile filenames.haskell
  , blockly: getFile filenames.blockly
  , javascript: getFile filenames.javascript
  , actus: wrap <$> getFile filenames.actus
  , metadata: getFile filenames.metadata
  }
  where
  getFile name = view (gistFiles <<< ix name <<< gistFileContent) gist

fileExists :: String -> Gist -> Boolean
fileExists name gist = has (gistFiles <<< ix name) gist
