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
import Node.FS.Sync (readdir)


data Action
  =
  UpdateFiles FormEvent

type State = {dir :: String, names :: Array String}

update :: Action -> State -> State
update (UpdateFiles ev) state = -- do
   -- let dirX = ev.target.value
   state {dir = ev.target.value}
   -- filenames <- lift ( liftEff (either (const []) id <$> try (readdir dirX)))
   -- state { names = filenames , dir = dirX}


view state = form
  [ name "signin"

  ]
  [ input [ type_ "text", value state.dir, onChange UpdateFiles ] []
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

main :: forall e. Eff (err :: EXCEPTION, channel :: CHANNEL | e) Unit
main = do
  let init = { dir : "/home/markus" , names :  [] }
  app <- start
    { initialState: init
    , update: fromSimple update
    , view: view
    , inputs: []
    }

  renderToDOM "#container" app.html
