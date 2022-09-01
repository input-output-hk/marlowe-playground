module Component.Popper
  ( module Component.Popper.Types
  , module ExportedInternal
  , defaultModifiers
  , defaultPreventOverflow
  , defaultFlip
  ) where

import Prologue hiding (Either(..))

import Component.Popper.Internal
  ( applyStyles
  , arrow
  , computeStyles
  , createPopper
  , destroyPopper
  , eventListeners
  , flipPlacement
  , forceUpdate
  , offset
  , popperOffsets
  , preventOverflow
  ) as ExportedInternal
import Component.Popper.Types
  ( Boundary(..)
  , CalculateFromBoundingBox
  , ComputeStyleOptions
  , EventListenerOptions
  , FlipOptions
  , Modifier
  , Offset
  , OffsetOption(..)
  , Options
  , Padding(..)
  , PaddingOption(..)
  , Placement(..)
  , PopperInstance
  , PositioningStrategy(..)
  , PreventOverflowOptions
  , Rect
  , RootBoundary(..)
  , TetherOffsetOption(..)
  , pAll
  , pBottom
  , pLeft
  , pRight
  , pTop
  )
import Effect (Effect)

defaultModifiers :: Effect (Array Modifier)
defaultModifiers =
  ( \applyStyles popperOffsets ->
      [ ExportedInternal.computeStyles
          { gpuAcceleration: true
          , adaptive: true
          , roundOffsets: true
          }
      , applyStyles
      , ExportedInternal.eventListeners
          { scroll: true
          , resize: true
          }
      , popperOffsets
      ]
  ) <$> ExportedInternal.applyStyles <*> ExportedInternal.popperOffsets

defaultPreventOverflow :: PreventOverflowOptions
defaultPreventOverflow =
  { mainAxis: true
  , altAxis: false
  , padding: pAll 0.0
  , boundary: ClippinParents
  , altBoundary: false
  , rootBoundary: ViewportBoundary
  , tether: true
  , tetherOffset: TetherOffset 0.0
  }

defaultFlip :: FlipOptions
defaultFlip =
  { padding: pAll 0.0
  , boundary: ClippinParents
  , rootBoundary: ViewportBoundary
  , flipVariations: true
  , allowedAutoPlacements:
      [ Top
      , TopStart
      , TopEnd
      , Bottom
      , BottomStart
      , BottomEnd
      , Right
      , RightStart
      , RightEnd
      , Left
      , LeftStart
      , LeftEnd
      ]
  }
