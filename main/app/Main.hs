{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

import qualified GI.Gtk as Gtk
import Data.GI.Base

main :: IO ()
main = do
  _ <- Gtk.init Nothing

  win <- new Gtk.Window [ #title := "Checkers" ]

  board <- Gtk.imageNewFromFile "./resources/board.png"
  pawn <- Gtk.imageNewFromFile "./resources/b_piece.png"

  -- on win #destroy Gtk.mainQuit
  -- _ <- Gtk.onWidgetDestroy win Gtk.mainQuit

  button <- new Gtk.Button [ #label := "Click me" ]

  on button #clicked 
    (set button 
      [ #sensitive := False
      , #label := "Thanks for clicking me" ])


  #add win board
  #add win button

  #showAll win

  Gtk.main