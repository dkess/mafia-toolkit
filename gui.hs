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
       p <- panel f []

       -- The game setup widgets
       sMafiaNum <- spinCtrl p 1 99 [selection := 2]
       sMaxPlayers <- spinCtrl p 1 99 [selection := 7]
       sMafiaKP <- spinCtrl p 0 99 [selection := 1
                      ,tooltip := "Mafia KP-- 0 means ceil(maf#/2)"]

       defaultSize <- get sMafiaKP size
       
       cDayNight <- choice p [items := ["Day","Night"], selection := 0]
       sCycle <- spinCtrl p 0 99 [selection := 1]

       -- The notebook containing more tools
       nb <- notebook p []

       -- Role Setup page
       p1 <- panel nb []
       -- stuff goes here
       
       -- Simulation page
       p2 <- panel nb []

       set f [layout := container p $
                        margin 10 $
                        column 5 [boxed "Game Setup"
                        (grid 5 5 [[label "Mafia #",minsize (defaultSize {sizeW = spinnerW}) (widget sMafiaNum)]
                                  ,[label "Max Players",minsize (defaultSize {sizeW = spinnerW}) (widget sMaxPlayers)]
                                  ,[label "Mafia KP",minsize (defaultSize {sizeW = spinnerW}) (widget sMafiaKP)]
                                  ,[widget cDayNight,minsize (defaultSize {sizeW = spinnerW}) (widget sCycle)]])
                        , glue
                        ,row 5 [tabs nb
                          [tab "Role Setup" $ container p1 $ margin 10 $ column 5 [label "page 1"]
                          ,tab "Simulation" $ container p2 $ margin 10 $ column 5 [label "page 2"]]]]]
       return ()
