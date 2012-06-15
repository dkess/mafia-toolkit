module Main where

import Graphics.UI.WX
import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe
import MafiaRole
import MafiaCalc
import Data.List (transpose)
import Data.Ratio

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
       tProbSane         <- staticText p3 [text := "00%"]
       tProbInsane       <- staticText p3 [text := "00%"]
       tProbNaive        <- staticText p3 [text := "00%"]
       tProbParanoid     <- staticText p3 [text := "00%"]
       cTownFramer       <- checkBox p3 [text := "T -> M framer"
                                       ,tooltip := "Framer can make town look like mafia"]
       cMafiaFramer      <- checkBox p3 [text := "M -> T framer"
                                       ,tooltip := "Framer can make mafia look like town"]
       cUnsureFramer     <- checkBox p3 [text := "Unsure of Framer's presence (50/50)"]

       let updateUnsureFramer = do
           townFramer <- get cTownFramer checked
           mafiaFramer <- get cMafiaFramer checked
           if (townFramer || mafiaFramer)
               then set cUnsureFramer [enabled := True]
               else set cUnsureFramer [enabled := False, checked := False]
           return ()
       
       updateUnsureFramer

       let updateProbs = do
           -- get all the values. he have to use fromIntegral on all of them because Int isn't good enough for us
           mafiaNum <- fromIntegral <$> get sMafiaNum selection
           maxPlayers <- fromIntegral <$> get sMaxPlayers selection
           deadTown <- fromIntegral <$> get sDeadTown selection
           deadMafia <- fromIntegral <$> get sDeadMafia selection
           townChecks <- fromIntegral <$> get sTownChecks selection
           mafiaChecks <- fromIntegral <$> get sMafiaChecks selection
           townFramer <- get cTownFramer checked
           mafiaFramer <- get cMafiaFramer checked
           unsureFramer <- get cUnsureFramer checked
           -- We use Data.Ratio for ratio calculations
           -- The ratio is town % mafia
           let saneRatio = if unsureFramer
                              then ((origNum * 2) + (if townFramer then 1 else 0) - (if mafiaFramer then 1 else 0))
                                 % ((origDen * 2) - (if townFramer then 1 else 0) + (if mafiaFramer then 1 else 0))
                              else origNum % origDen
                 where origNum = (maxPlayers - mafiaNum - (if townFramer then 1 else 0) + (if mafiaFramer then 1 else 0) - deadTown)
                       origDen = (mafiaNum + (if townFramer then 1 else 0) - (if mafiaFramer then 1 else 0) - deadMafia)
            
           let insaneRatio = recip saneRatio
           
           -- First we make sure that the number of checks don't exceed
           -- the number of people of that alignment
           let probSane = if (townChecks <= (maxPlayers - mafiaNum) || townFramer) && (mafiaChecks <= mafiaNum || mafiaFramer)
                          then (((numerator saneRatio) % ((numerator saneRatio) + (denominator saneRatio)))^townChecks)
                             * (((denominator saneRatio) % ((numerator saneRatio) + (denominator saneRatio)))^mafiaChecks)
                          else 0 % 1
           let probInsane = if (townChecks <= mafiaNum || townFramer) && (mafiaChecks <= (maxPlayers - mafiaNum) || mafiaFramer)
                            then (((numerator insaneRatio) % ((numerator insaneRatio) + (denominator insaneRatio)))^townChecks)
                               * (((denominator insaneRatio) % ((numerator insaneRatio) + (denominator insaneRatio)))^mafiaChecks)
                            else 0 % 1
           let probNaive = if mafiaChecks > 0
                           then 0 % 1
                           else 1 % 1
           let probParanoid = if townChecks > 0
                              then 0 % 1
                              else 1 % 1
           let allProbs = probSane + probInsane + probNaive + probParanoid

           -- Now we actually set the label's values
           set tProbSane [text := (show $ fromRational $ probSane/allProbs*100) ++ "%"]
           set tProbInsane [text := (show $ fromRational $ probInsane/allProbs*100) ++ "%"]
           set tProbNaive [text := (show $ fromRational $ probNaive/allProbs*100) ++ "%"]
           set tProbParanoid [text := (show $ fromRational $ probParanoid/allProbs*100) ++ "%"]
           return ()
       
       set sDeadTown [on select := updateProbs]
       set sDeadMafia [on select := updateProbs]
       set sTownChecks [on select := updateProbs]
       set sMafiaChecks [on select := updateProbs]
       set cTownFramer [on command := do updateProbs
                                         updateUnsureFramer
                                         return ()]
       set cMafiaFramer [on command := do updateProbs
                                          updateUnsureFramer
                                          return ()]
       set sMafiaNum [on select := updateProbs]
       set cUnsureFramer [on command := updateProbs]
       set sMaxPlayers [on select := updateProbs]
       set cUseOtherDead [on command := do
                             isChecked <- get cUseOtherDead checked
                             set sDeadTown [enabled := not isChecked]
                             set sDeadMafia [enabled := not isChecked]
                             updateProbs
                             return ()]

       -- Misc. Calculator page
       p4 <- panel nb []
       sRemainingTown <- spinCtrl p4 0 99 []
       sRemainingMafia <- spinCtrl p4 0 99 []
       mislynchesText <- staticText p4 []

       let updateMLText = do
           remainingTown <- get sRemainingTown selection
           remainingMafia <- get sRemainingMafia selection
           mafiaKP <- get sMafiaKP selection
           dayNight <- get cDayNight selection
           let boolDayNight = if dayNight < 1 then True else False
           set mislynchesText [text :=
               "Town loses in " ++
               show (mislynchCycles remainingTown remainingMafia mafiaKP boolDayNight) ++
               " cycles with " ++
               show (mislynches remainingTown remainingMafia mafiaKP boolDayNight) ++
               " mislynches."]
           return ()
       set sRemainingTown [on select := updateMLText]
       set sRemainingMafia [on select := updateMLText]
       set sMafiaKP [on select := updateMLText]
       set cDayNight [on select := updateMLText]

       -- layout
       set f [layout := container p $
                        margin 10 $
                        row 5 [
                        column 5
                        [ boxed "Game Setup"
                        (grid 5 5 [[label "Mafia #",minsize (defaultSize {sizeW = spinnerW}) (widget sMafiaNum)]
                                  ,[label "Max Players",minsize (defaultSize {sizeW = spinnerW}) (widget sMaxPlayers)]
                                  ,[label "Mafia KP",minsize (defaultSize {sizeW = spinnerW}) (widget sMafiaKP)]
                                  ,[widget cDayNight,minsize (defaultSize {sizeW = spinnerW}) (widget sCycle)]])
                        ,widget refButton]
                        ,tabs nb
                          [tab "Roles" $ container p1 $ margin 10 $ row 5 [grid 5 5
                            roleSettings]
                          ,tab "Simulation"     $ container p2 $ margin 10 $ column 5 [label "page 2"]
                          ,tab "DT Calc"   $ container p3 $ margin 10 $ column 5
                             [ widget cUseOtherDead
                             , grid 5 5
                               [ [label "Dead Town",                widget sDeadTown]
                               , [label "Dead Mafia",               widget sDeadMafia]
                               , [label "Town Checks" ,             widget sTownChecks]
                               , [label "Mafia Checks",             widget sMafiaChecks]
                               , [widget cTownFramer,               widget cMafiaFramer]
                               ]
                             , widget cUnsureFramer
                             , grid 5 5
                               [ [label "Sane Probability:",        widget tProbSane]
                               , [label "Insane Probability:",      widget tProbInsane]
                               , [label "Naive Probability:",       widget tProbNaive]
                               , [label "Paranoid Probability:",    widget tProbParanoid]
                               ]
                             ]
                          ,tab "Misc. Calc" $ container p4 $ margin 10 $ column 5
                             [ grid 5 5
                               [ [label "Remaining Town", widget sRemainingTown]
                               , [label "Remaining Mafia", widget sRemainingMafia]
                               ]
                             , widget mislynchesText
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

