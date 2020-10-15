module Lib.Reflex.Clicks
  ( clickEvent
  )
where

import           Protolude
import           Reflex.Dom

-- | Get the click event on an element
--
-- Use as:
--   clickEvent $ el' "a" ...
clickEvent
  :: (DomBuilder t m, HasDomEvent t target 'ClickTag)
  => m target
  -> m (Event t ())
clickEvent = fmap (void . domEvent Click)
