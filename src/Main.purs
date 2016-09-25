module Main where

import Prelude

import Control.Monad.Eff.Exception (try, EXCEPTION)
import Control.Monad.Eff (Eff)

import Signal.Channel (CHANNEL)

import Pux (renderToDOM, fromSimple, start)
import Pux.Html (Html, text, button, span, div, input, ul, li, form)
import Pux.Html.Events (onClick, onChange, onKeyUp, onSubmit, FormEvent)
import Pux.Html.Attributes (type_, value, name)

import Data.Int (toNumber)

import Control.Monad.Trans (lift)
import Control.Monad.Eff.Class (liftEff)
import Data.Either (either)

import Node.FS (FS)
import Node.FS.Sync (readdir)

type EffModel state action eff =
  { state :: state
  , effects :: Array (Eff eff action)
  }

data Action
  =
  UpdateDir FormEvent | UpdateFiles (Array String)

type State = {dir :: String, names :: Array String}

noEffects :: forall state action eff. state -> EffModel state action eff
noEffects state = { state: state, effects: [] }

update :: Action -> State -> EffModel State Action (fs :: FS)
update (UpdateDir ev) state =
  { state: state { dir = ev.target.value }
  , effects: [ do
      filenames <- (either (const []) id <$> try (readdir ev.target.value))
      pure $ UpdateFiles filenames
    ]
  }
update (UpdateFiles files) state =
  noEffects $ state


{--
update (UpdateFiles ev) state =  do
   let dirX = "/" -- ev.target.value
   -- state {dir = ev.target.value}
   filenames <- lift ( liftEff (either (const []) id <$> try (readdir dirX)))
   pure state { names = filenames , dir = dirX}
--}

view state = form
  [ name "signin"

  ]
  [ input [ type_ "text", value state.dir, onChange UpdateDir ] []
  , button [ type_ "submit" ] [ text "Sign In" ]
  ]


{--
view :: State -> Html Action
view state = div
    []
     [
      input [  type_ "text", value state.dir,
               onChange (const UpdateFiles)
               ] [],
      div [] (map (\file -> (div [][text file])) state.names)
    ]
--}

main :: forall e. Eff (fs :: FS, err :: EXCEPTION, channel :: CHANNEL | e) Unit
main = do
  let init = { dir : "/home/markus" , names :  [] }
  app <- start
    { initialState: init
    , update: update
    , view: view
    , inputs: []
    }

  renderToDOM "#container" app.html
