module MafiaCalc (mislynches, mislynchCycles) where

mislynches :: Int -> Int -> Int -> Bool -> Int
mislynches t m kp d = if d
                        then if t > m
                                then 1 + (mislynches (t-1) m kp False)
                                else 0
                        else if (t-kp) < m
                                then (-1) -- sometimes mafia will win even if one of them is lynched
                                else if (t > m)
                                        then mislynches (t-(if kp<1 then ceiling ((fromIntegral m)/2) else kp)) m kp True
                                        else 0

mislynchCycles :: Int -> Int -> Int -> Bool -> Int
mislynchCycles t m kp d = (if d then (-1) else 0) + (mislynches t m kp d)*2
--mislynchCycles t m kp d = (mislynches t m kp d)*2
