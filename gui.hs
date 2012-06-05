module Main where

import Graphics.UI.WX
import Control.Monad
import Data.List
import Data.Maybe
import MafiaRole
import Data.List (transpose)

main :: IO ()
main
  = start gui

spinnerW :: Int
spinnerW = 40

-- We use this to ensure that the roles marked
-- for "fill" are calculate seperately
listFromBools :: [a] -> [Bool] -> [a]
listFromBools x y = [fst a | a <- zip x y, snd a == True]

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

       checkBoxes <- forM roleList (\a -> do
                                           cbox <- checkBox p1 [text := (name a), checked := (defRole a)]
                                           return cbox)
       minSpinners <- forM roleList(\a -> do
                                           spinner <- spinCtrl p1 0 99 [selection := minAmount a]
                                           return spinner)
       maxSpinners <- forM roleList(\a -> do
                                           spinner <- spinCtrl p1 0 99 [selection := maxAmount a]
                                           return spinner)

       fillBoxes <- forM roleList(\a -> do
                                       cbox <- checkBox p1 [text := "Fill"]
                                       set cbox [checked := fillRole a]
                                       return cbox)

       refButton <- button p [text := "Print Output", on command :=
          do
              fillVals <- getCheckValues fillBoxes
              enabledVals <- getCheckValues checkBoxes

              let usedRoles = zipWith (&&) (map not fillVals) enabledVals

              minVal <- getSpinValues minSpinners
              maxVal <- getSpinValues maxSpinners
              mafiaNum <- get sMafiaNum selection
              maxPlayers <- get sMaxPlayers selection

              print $ [i ++ (maybe [] (\y -> replicate (maxPlayers - mafiaNum - length [z | z <- i, MafiaRole.color z /= Red]) (fst y))
                         (listToMaybe [x | x <- listFromBools (zip roleList minVal) (zipWith (&&) fillVals enabledVals)
                                         , (MafiaRole.color (fst x)) == Green]))
                         ++ (maybe [] (\y -> replicate (mafiaNum - length [x | x <- i, MafiaRole.color x == Red]) (fst y))
                         (listToMaybe [x | x <- listFromBools (zip roleList minVal) (zipWith (&&) fillVals enabledVals)
                                         , (MafiaRole.color (fst x)) == Red]))
                         | i <- minMaxList $ listFromBools (zip3 roleList minVal maxVal) usedRoles]

              return ()]

       let roleSettings = transpose [[widget cbox | cbox <- checkBoxes]
                                    ,[minsize (defaultSize {sizeW = spinnerW}) (widget spinner) | spinner <- minSpinners]
                                    ,[label "-" | a <- roleList]
                                    ,[minsize (defaultSize {sizeW = spinnerW}) (widget spinner) | spinner <- maxSpinners]
                                    ,[widget cbox | cbox <- fillBoxes]
                                    ]

       -- Simulation page
       p2 <- panel nb []

       set f [layout := container p $
                        margin 10 $
                        row 5 [boxed "Game Setup"
                        (grid 5 5 [[label "Mafia #",minsize (defaultSize {sizeW = spinnerW}) (widget sMafiaNum)]
                                  ,[label "Max Players",minsize (defaultSize {sizeW = spinnerW}) (widget sMaxPlayers)]
                                  ,[label "Mafia KP",minsize (defaultSize {sizeW = spinnerW}) (widget sMafiaKP)]
                                  ,[widget cDayNight,minsize (defaultSize {sizeW = spinnerW}) (widget sCycle)]])
                        ,row 5 [tabs nb
                          [tab "Role Setup" $ container p1 $ margin 10 $ row 5 [grid 5 5
                            roleSettings]
                          ,tab "Simulation" $ container p2 $ margin 10 $ column 5 [label "page 2"]]]]]
       return ()

getSpinValues i
  = do maxvals <- forM i (\a -> do
           t <- get a selection
           return t)
       return maxvals

getCheckValues i
  = do checkedNums <- forM i (\a -> do
        t <- get a checked
        return t)
       return checkedNums

