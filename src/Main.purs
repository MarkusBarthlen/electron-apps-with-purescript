module Main where

import Prelude
import React as R
import React.DOM.Props as RP
import ReactDOM as RDOM
import Thermite as T
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (try)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.HTMLInputElement (files)
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
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.Trans (lift)

type State = {dir :: String, names :: Array String}

data Action
  = SetEditText String |
  UpdateFiles String

render :: T.Render State _ Action
render perform props state _ =
  let handleKeyPress :: Int -> String -> _
      handleKeyPress 13 text = perform $ UpdateFiles text
      handleKeyPress 27 _    = perform $ SetEditText ""
      handleKeyPress _  _    = pure unit
  in
    [
      input [RP.placeholder "directory",
               RP.value state.dir,
               RP.onChange \e -> perform (SetEditText (unsafeCoerce e).target.value),
               RP.onKeyUp \e -> handleKeyPress (unsafeCoerce e).keyCode (unsafeCoerce e).target.value
               ] [],
      ul' (map (\file -> li' [text file]) props.names)
    ]


performAction :: T.PerformAction _ State _ Action
performAction (SetEditText s)           _ _ = void do
  T.cotransform $ _ { dir = s }
performAction (UpdateFiles s)           _ _ = void do
   filenames <- (either (const []) id <$> try (readdir s))
   T.cotransform $ _ { names = filenames }
  -- T.cotransform $ _ { dir = ""}


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
