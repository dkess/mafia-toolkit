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

-- megaDelete returns an empty list if the item
-- to be deleted does not exist
megaDelete :: Eq a => a -> [a] -> [a]
megaDelete x y
 | length afterDel < length y = afterDel
 | otherwise = []
  where afterDel = delete x y

multiApply :: [(a -> a)] -> a -> a
multiApply (f:fs) x = multiApply fs (f x)
multiApply _ x = x

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
                                       cbox <- checkBox p1 []
                                       set cbox [checked := fillRole a]
                                       return cbox)
       
       deadSpinners <- forM roleList(\a -> do
                                            spinner <- spinCtrl p1 0 99 [selection := 0]
                                            return spinner)

       let getRolePossibilities = do
           fillVals <- getCheckValues fillBoxes
           enabledVals <- getCheckValues checkBoxes

           let usedRoles = zipWith (&&) (map not fillVals) enabledVals
 
           minVal <- getSpinValues minSpinners
           maxVal <- getSpinValues maxSpinners
           mafiaNum <- get sMafiaNum selection
           maxPlayers <- get sMaxPlayers selection
           deadPlayers <- getSpinValues deadSpinners

           return $ [multiApply (concat [replicate (fst q) (megaDelete (snd q)) | q <- zip deadPlayers roleList])
                    (i ++ (maybe [] (\y -> replicate (maxPlayers - mafiaNum - length [z | z <- i, MafiaRole.color z /= Red]) (fst y))
                      (listToMaybe [x | x <- listFromBools (zip roleList minVal) (zipWith (&&) fillVals enabledVals)
                                      , (MafiaRole.color (fst x)) == Green]))
                      ++ (maybe [] (\y -> replicate (mafiaNum - length [x | x <- i, MafiaRole.color x == Red]) (fst y))
                      (listToMaybe [x | x <- listFromBools (zip roleList minVal) (zipWith (&&) fillVals enabledVals)
                                      , (MafiaRole.color (fst x)) == Red])))
                      | i <- minMaxList $ listFromBools (zip3 roleList minVal maxVal) usedRoles]

       refButton <- button p [text := "Print Output", on command := do
           rolePossibilities <- getRolePossibilities
           print rolePossibilities
           return ()] 
 

       let roleSettings = transpose [label "Role Name" : [widget cbox | cbox <- checkBoxes]
                                    ,label "Min" : [minsize (defaultSize {sizeW = spinnerW}) (widget spinner) | spinner <- minSpinners]
                                    ,glue : [label "-" | a <- roleList]
                                    ,label "Max" : [minsize (defaultSize {sizeW = spinnerW}) (widget spinner) | spinner <- maxSpinners]
                                    ,label "Fill?" : [widget cbox | cbox <- fillBoxes]
                                    ,label "# Dead" : [minsize (defaultSize {sizeW = spinnerW}) (widget spinner) | spinner <- deadSpinners]
                                    ]

       -- Simulation page
       p2 <- panel nb []

       -- Detective simulator page
       p3 <- panel nb []
       cUseOtherDead    <- checkBox p3 [text := "Use dead players from Setup page"]
       
       sDeadTown        <- spinCtrl p3  0 99 []
       sDeadMafia       <- spinCtrl p3 0 99 []
       sTownChecks      <- spinCtrl p3 0 99 []
       sMafiaChecks     <- spinCtrl p3 0 99 []
       -- TODO: maybe make this a list of sanities instead of hard-coding them?
       probSane         <- staticText p3 [text := "00%"]
       probInsane       <- staticText p3 [text := "00%"]
       probNaive        <- staticText p3 [text := "00%"]
       probParanoid     <- staticText p3 [text := "00%"]
       updateProbs      <- button p3 [text := "Update"]

       set cUseOtherDead [on command := do
                             isChecked <- get cUseOtherDead checked
                             set sDeadTown [enabled := not isChecked]
                             set sDeadMafia [enabled := not isChecked]
                             return ()]

       -- layout
       set f [layout := container p $
                        margin 10 $
                        row 5 [boxed "Game Setup"
                        (grid 5 5 [[label "Mafia #",minsize (defaultSize {sizeW = spinnerW}) (widget sMafiaNum)]
                                  ,[label "Max Players",minsize (defaultSize {sizeW = spinnerW}) (widget sMaxPlayers)]
                                  ,[label "Mafia KP",minsize (defaultSize {sizeW = spinnerW}) (widget sMafiaKP)]
                                  ,[widget cDayNight,minsize (defaultSize {sizeW = spinnerW}) (widget sCycle)]
                                  ,[widget refButton, glue]])
                        ,row 5 [tabs nb
                          [tab "Role Setup" $ container p1 $ margin 10 $ row 5 [grid 5 5
                            roleSettings]
                          ,tab "Simulation"     $ container p2 $ margin 10 $ column 5 [label "page 2"]
                          ,tab "DT Simulator"   $ container p3 $ margin 10 $ column 5
                             [ widget cUseOtherDead
                             , grid 5 5
                               [ [label "Dead Town" ,            widget sDeadTown]
                               , [label "Dead Mafia",            widget sDeadMafia]
                               , [label "Town Checks" ,          widget sTownChecks]
                               , [label "Mafia Checks",          widget sMafiaChecks]
                               , [label "Sane Probability:",     widget probSane]
                               , [label "Insane Probability:",   widget probInsane]
                               , [label "Naive Probability:",    widget probNaive]
                               , [label "Paranoid Probability:", widget probParanoid]
                               ]
                             , widget updateProbs
                             ]
                          ]
                        ]
                        ]]
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

