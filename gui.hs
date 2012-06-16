module Main where

import Graphics.UI.WX
import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe
import MafiaCalc
import Data.List (transpose)
import Data.Ratio

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

       -- Detective simulator page
       p1 <- panel nb []
       cUseOtherDead    <- checkBox p1 [text := "Use dead players from Setup page"]
       
       sDeadTown        <- spinCtrl p1  0 99 []
       sDeadMafia       <- spinCtrl p1 0 99 []
       sTownChecks      <- spinCtrl p1 0 99 []
       sMafiaChecks     <- spinCtrl p1 0 99 []
       -- TODO: maybe make this a list of sanities instead of hard-coding them?
       tProbSane         <- staticText p1 [text := "00%"]
       tProbInsane       <- staticText p1 [text := "00%"]
       tProbNaive        <- staticText p1 [text := "00%"]
       tProbParanoid     <- staticText p1 [text := "00%"]
       cTownFramer       <- checkBox p1 [text := "T -> M framer"
                                       ,tooltip := "Framer can make town look like mafia"]
       cMafiaFramer      <- checkBox p1 [text := "M -> T framer"
                                       ,tooltip := "Framer can make mafia look like town"]
       cUnsureFramer     <- checkBox p1 [text := "Unsure of Framer's presence (50/50)"]

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
       p2 <- panel nb []
       sRemainingTown <- spinCtrl p2 0 99 []
       sRemainingMafia <- spinCtrl p2 0 99 []
       mislynchesText <- staticText p2 []

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
                        ]
                        ,tabs nb
                          [tab "DT Calc"   $ container p1 $ margin 10 $ column 5
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
                          ,tab "Misc. Calc" $ container p2 $ margin 10 $ column 5
                             [ grid 5 5
                               [ [label "Remaining Town", widget sRemainingTown]
                               , [label "Remaining Mafia", widget sRemainingMafia]
                               ]
                             , widget mislynchesText
                             ]
                          ]
                        ]]
       return ()
