{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Elm.JohnnyFive where

import Elm.Wrapper
import Elm.Wrapper.Quote

johnny5 :: Module -> Module
johnny5 mdl = mdl
  { mdlInit = 
      [js|var j5    = require("johnny-five");
         |var board = new j5.Board();
         |
         |board.on("ready", function() {
         |#{indent $ toJS $ mdlInit mdl}
         |  });
         |]
  , mdlExports = ("j5",[js|j5|]) : ("board",[js|board|]) : mdlExports mdl
  }

dropbox :: Module
dropbox = Module "Native.Dropbox"
  [js|#{importAs "Signal" "Native.Signal"}
     |]
  [ "client" @@ ["apiKey"] :-> [jsFile|scripts/Dropbox.js|]
  ]
  [("client",[js|client|])]

