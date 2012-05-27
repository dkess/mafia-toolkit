module Main where

import Graphics.UI.WX

main :: IO ()
main
  = start gui

spinnerW :: Int
spinnerW = 40

gui :: IO ()
gui
  = do f <- frame [text := "Mafia Toolkit"]
       leftLayout <- panel f []
       sMafiaNum <- spinCtrl leftLayout 1 99 [selection := 2]
       sMaxPlayers <- spinCtrl leftLayout 1 99 [selection := 7]
       sMafiaKP <- spinCtrl leftLayout 0 99 [selection := 1
                      ,tooltip := "Mafia KP-- 0 means ceil(maf#/2)"]

       defaultSize <- get sMafiaKP size 
       
       cDayNight <- choice leftLayout [items := ["Day","Night"], selection := 0]
       sCycle <- spinCtrl leftLayout 0 99 [selection := 1]

       set f [layout := container leftLayout $
                        margin 10 $
                        column 5 [boxed "Game Setup"
                        (grid 5 5 [[label "Mafia #",minsize (defaultSize {sizeW = spinnerW}) (widget sMafiaNum)]
                                  ,[label "Max Players",minsize (defaultSize {sizeW = spinnerW}) (widget sMaxPlayers)]
                                  ,[label "Mafia KP",minsize (defaultSize {sizeW = spinnerW}) (widget sMafiaKP)]
                                  ,[widget cDayNight,minsize (defaultSize {sizeW = spinnerW}) (widget sCycle)]])]]
       return ()
