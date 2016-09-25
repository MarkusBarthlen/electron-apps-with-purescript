module Main where

import Prelude
import React as R
import React.DOM.Props as RP
import ReactDOM as RDOM
import Thermite as T
import Control.Monad.Aff (liftEff')
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (try)
import Control.Monad.Trans (lift)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.HTMLAnchorElement (download)
import DOM.HTML.HTMLInputElement (files)
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import Data.Either (either, Either)
import Data.Maybe (fromJust)
import Data.Nullable (toMaybe)
import Node.FS (FS)
import Node.FS.Sync (readdir)
import Partial.Unsafe (unsafePartial)
import React.DOM (text, li', ul', input, button)
import Unsafe.Coerce (unsafeCoerce)
import Data.Tuple
import Data.Lens


type InputState = String

data InputAction = SetEditText String

type State = Tuple InputState FilesState

renderInput :: T.Render InputState _ InputAction
renderInput perform props state _ =
  let handleKeyPress :: Int -> String -> _
      handleKeyPress 27 _    = perform $ SetEditText ""
      handleKeyPress _  _    = pure unit
  in
    [
      input [RP.placeholder "directory",
               RP.value state,
               RP.onChange \e -> perform (SetEditText (unsafeCoerce e).target.value),
               RP.onKeyUp \e -> handleKeyPress (unsafeCoerce e).keyCode (unsafeCoerce e).target.value
               ] []
    ]

performInputAction :: forall e. T.PerformAction _ InputState _ InputAction
performInputAction (SetEditText s)           _ _ = void do
  T.cotransform \state -> s

inputSpec :: T.Spec _ InputState _ InputAction
inputSpec = T.simpleSpec performInputAction renderInput

type FilesState = Array String

data FilesAction = Update

renderFiles :: T.Render FilesState _ FilesAction
renderFiles perform props state _ =
      [
      button [RP.onClick \_ -> perform Update
               ] [],
      ul' (map (\file -> li' [text file]) props.names)
    ]

performFilesAction :: forall e. T.PerformAction (fs :: FS, console :: CONSOLE | e) FilesState _ FilesAction
performFilesAction (Update)           _ s = do
   filenames <- lift ( liftEff (either (const []) id <$> try (readdir s.dir)))
   lift (liftEff $ log s)
   lift (liftEff $ log (show filenames))
   void $ T.cotransform $ _ { names = filenames }

filesSpec :: T.Spec _ FilesState _ FilesAction
filesSpec = T.simpleSpec performInputAction renderInput



type Action = Either InputAction FilesAction

dirListingComponent :: T.Spec _ State _ Action
dirListingComponent = T.focus _1 _Left inputSpec <> T.focus _2 _Right filesSpec

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
    <$> DOM.querySelector "#container"
    (DOM.htmlDocumentToParentNode document))
  RDOM.render
    (R.createFactory component (state))
    container
