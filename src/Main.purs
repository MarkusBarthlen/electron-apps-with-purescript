module Main where

import Prelude
import React as R
import ReactDOM as RDOM
import Thermite as T
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (try)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import Data.Either (either)
import Data.Maybe (fromJust)
import Data.Nullable (toMaybe)
import Node.FS (FS)
import Node.FS.Sync (readdir)
import Partial.Unsafe (unsafePartial)
import React.DOM (text, li', ul', input)
import React.DOM.Props as RP
import Unsafe.Coerce (unsafeCoerce)

type State = {dir :: String, names :: Array String}

data Action
  = SetEditText String

-- dirListingComponent :: forall eff. T.Spec eff Unit State Unit
-- dirListingComponent :: T.Spec _ State _ Action

render :: T.Render State _ _
render perform props state _ =
  [
    input [RP.placeholder "directory",
             RP.onChange \e -> perform (SetEditText (unsafeCoerce e).target.value)   ] [],
    ul' (map (\file -> li' [text file]) props.names)
  ]

performAction :: T.PerformAction _ State _ Action
performAction (SetEditText s)           _ _ = void $ T.cotransform $ _ { dir = s }

dirListingComponent :: T.Spec _ State _ Action
dirListingComponent = T.simpleSpec performAction render

main :: Eff (fs :: FS, dom :: DOM) Unit
main = void do
  let dir = "/home/markus"
  fileNames <- either (const []) id <$> try (readdir dir)
  let state = {dir: dir, names: fileNames}
  let component = T.createClass dirListingComponent state
  document <- DOM.window >>= DOM.document
  container <-
    unsafePartial
    (fromJust <<< toMaybe
    <$> DOM.querySelector "body"
    (DOM.htmlDocumentToParentNode document))
  RDOM.render
    (R.createFactory component (state))
    container
