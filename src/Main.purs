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
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import Data.Either (either)
import Data.Maybe (fromJust)
import Data.Nullable (toMaybe)
import Node.FS (FS)
import Node.FS.Sync (readdir)
import Partial.Unsafe (unsafePartial)
import React.DOM (text, li', ul', input, div)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.Trans (lift)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Lens (over)
import Data.Foldable (fold)



type State = {dir :: String, names :: Array String}

data Action
  = SetEditText String |
  UpdateFiles String

render :: T.Render State _ Action
render perform props state _ =
  let handleKeyPress :: Int -> String -> _
      handleKeyPress 13 s = perform $ UpdateFiles s
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


performAction :: forall e. T.PerformAction (fs :: FS, console :: CONSOLE | e) State _ Action
performAction (SetEditText s)           _ _ = void do
  T.cotransform $ _ { dir = s }
performAction _                     _ _ = pure unit


-- dirListingComponent :: T.Spec _ State _ Action
dirListingComponent :: forall e. T.Spec (fs :: FS, console :: CONSOLE | e) State _ Action
dirListingComponent = T.simpleSpec performAction render

performActionParent :: forall e. T.PerformAction (fs :: FS, console :: CONSOLE | e) State _ Action
performActionParent (UpdateFiles s)           _ _ = do
   filenames <- lift ( liftEff (either (const []) id <$> try (readdir s)))
   lift (liftEff $ log s)
   lift (liftEff $ log (show filenames))
   void $ T.cotransform $ _ { names = filenames }
performActionParent _                     _ _ = pure unit

-- renderParent :: forall props e. T.Render State _ Action
-- renderParent dispatch props state _ = container $ fold
parent :: forall eff. T.Spec (fs :: FS, console :: CONSOLE | eff) State _ Action
parent = container $ fold
  [  dirListingComponent , parentActions
    ]
  where
  container :: forall state action. T.Spec (fs :: FS, console :: CONSOLE | eff) state _ action -> T.Spec (fs :: FS, console :: CONSOLE | eff) state _ action
  container = over T._render \renderX d p s c ->
      [ div [ RP.className "container" ] (renderX d p s c) ]
  parentActions :: T.Spec (fs :: FS, console :: CONSOLE | eff) State _ Action
  parentActions = T.simpleSpec performActionParent T.defaultRender

-- parent :: T.Spec _ State _ Action
-- parent = T.simpleSpec performActionParent renderParent

main :: Eff (fs :: FS, dom :: DOM) Unit
main = void do
  let dir = "/home/markus"
  fileNames <- either (const []) id <$> try (readdir dir)
  let state = {dir: dir, names: fileNames}
  let component = T.createClass parent state
  document <- DOM.window >>= DOM.document
  container <-
    unsafePartial
    (fromJust <<< toMaybe
    <$> DOM.querySelector "body"
    (DOM.htmlDocumentToParentNode document))
  RDOM.render
    (R.createFactory component (state))
    container
