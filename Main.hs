{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified Data.Text as T
import System.IO
import Foo

defaultWidth = 1440
defaultHeight = 900

orientation = Gtk.OrientationVertical
spacing = 10
padding = 10

editable = False
cursorVisible = False
monospace = True
wrapMode = Gtk.WrapModeWord

placeholderText = "Enter here"
truncateMultiline = True

labelIpt = "Input:"
labelBtn = "Click to Compute"
labelOpt = "Output:"

main :: IO ()
main = do
  _ <- Gtk.init Nothing

  win <- new Gtk.Window [ #title := T.pack title
                        , #defaultWidth := defaultWidth
                        , #defaultHeight := defaultHeight
                        ]
  _ <- on win #destroy Gtk.mainQuit

  box <- new Gtk.Box [ #orientation := orientation
                     , #spacing := spacing
                     ]
  #add win box

  handle <- openFile "ReadMe.md" ReadMode  
  contents <- hGetContents handle
  usageBuffer <- new Gtk.TextBuffer [ #text := T.pack contents ]
  usage <- new Gtk.TextView [ #buffer := usageBuffer
                            , #editable := editable
                            , #cursorVisible := cursorVisible
                            , #monospace := monospace
                            , #wrapMode := wrapMode
                            ]
  #packStart box usage False False padding

  lbIpt <- new Gtk.Label [ #label := labelIpt ]
  #packStart box lbIpt False False padding

  txt <- new Gtk.Entry [ #placeholderText := placeholderText
                       , #truncateMultiline := truncateMultiline
                       ]
  #packStart box txt False False padding

  btn <- new Gtk.Button [ #label := labelBtn ]
  #packStart box btn False False padding

  lbOpt <- new Gtk.Label [ #label := labelOpt ]
  #packStart box lbOpt False False padding
  
  resultBuffer <- new Gtk.TextBuffer [ #text := "" ]
  result <- new Gtk.TextView [ #buffer := resultBuffer
                             , #editable := editable
                             , #cursorVisible := cursorVisible
                             , #monospace := monospace
                             , #wrapMode := wrapMode
                             ]
  #packStart box result False False padding
  _ <- on btn #clicked (do input <- Gtk.entryGetText txt
                           set resultBuffer [ #text := T.pack $ foo $ T.unpack $ input])

  #showAll win

  Gtk.main