module MafiaRole( Role (..),roleList, minMaxList) where

import Data.List
import Control.Applicative

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not.null) . map (take n) . iterate (drop n)

-- Massive thanks to dmwit in #haskell for figuring this out
select :: [a] -> [([a], [a])]
select xs = zip (inits xs) (tails xs)

-- I might change this to faction names instead of colors
data Color = Red | Blue | Green | Black | None

instance Show Color where
    show Red = "Red"        -- primary scumteam
    show Blue = "Blue"      -- secondary scumteam (NOT town)
    show Green = "Green"    -- town
    -- coming soon, sleeper cell and other factions
    show None = "None"      -- should not be used

data RoleFlags = RoleFlags {    -- RoleFlags are more dynamic than normal attributes
    bullets  :: Int,    -- bullets, for a vigilante 
    dtCheck  :: Role,   -- what this player returns as a dt check
    lives    :: Int,    -- lives carry over multiple nights
    willAct  :: Bool,   -- if player has an optional action
    sanity   :: Sanities
}

data Sanities = Sane    -- gets normal checks
              | Insane  -- opposite of correct check
              | Paranoid --Always returns red
              | Naive   -- Always returns green
              deriving (Show, Eq)

defaultFlags = RoleFlags {
    bullets = 0,
    dtCheck = defaultRole {
        name = "NORMAL" -- detective should use real role
    },
    lives = 0,
    willAct = False,
    sanity = Sane
}

data Role = Role {
    name    :: String,
    health  :: Int,
    color   :: Color,
    action  :: [Role] -> [[Role]],
    startDay:: [Role -> Role],  -- actions done at the beginning
    rflags  :: RoleFlags,
    maxAmount::Int,
    minAmount::Int,
    fillRole:: Bool
}

instance Show Role where
    show role = name role ++ "(" ++ show (health role) ++ ")"

defaultRole :: Role     -- The default role
defaultRole = Role {
    name = "unnamed",
    health = 1,
    color = None,
    action = noAction,
    startDay = [],
    rflags = defaultFlags,
    maxAmount = 2,
    minAmount = 0,
    fillRole = False
} where noAction x = [[]]

cVigilante :: Role
cVigilante = defaultRole {
    name = "Comp. Vigilante",
    color = Green,
    action = modHP (-1),
    rflags = defaultFlags {
        willAct = True
    },
    maxAmount = 1
}

vanilla :: Role
vanilla = defaultRole {
    name = "Vanilla",
    color = Green,
    maxAmount = 4,
    minAmount = 3,
    fillRole = True
}

doctor :: Role
doctor = defaultRole {
    name = "Doctor",
    color = Green,
    action = modHP(1)
}

roleCop :: Role     -- role, not alignment
roleCop = defaultRole {
    name = "Role Cop",
    color = Green
    -- action to be implemented later
}

aCop :: Role    -- Alignment cop
aCop = defaultRole {
    name = " Cop",
    color = Green
    -- action should take sanity as parameter
}

detective :: Role -- Role and alignment
detective = defaultRole {
    name = "Detective",
    color = Green
}

goon :: Role
goon = defaultRole {
    name = "Goon",
    color = Red,
    fillRole = True
}

roleblocker :: Role
roleblocker = defaultRole {
    name = "Roleblocker",
    color = Red,
    maxAmount = 1
}

-- Change health (s in sMod stands for single,
-- because it operates on a single role)
sModHP :: Int -> Role -> Role
sModHP num role = role {health = (health role) + num}

-- modHP takes a value and applies it to everyone in a
-- different possibility. A value of 1 heals, -1 is KP
modHP :: Int -> [Role] -> [[Role]]
modHP i r = listDist (sModHP i) r

-- listDist takes an action that modifies a role,
-- and gives the possibilities of it being acted on everyone
-- shoutout to dmwit in #haskell for figuring this out
listDist :: (a -> a) -> [a] -> [[a]]
listDist f r = [b ++ f m:e | (b,m:e) <- select r]

-- If willAct in the flags is set to False, do not give an action.
-- Otherwise perform the action
maybeAct :: RoleFlags -> ([Role] -> [[Role]]) -> [Role] -> [[Role]]
maybeAct flags f r = if (willAct flags) then
    f r
else
    [r]

{-
-- Functions to generate lists based off the min/max values specified in the GUI
minMaxList :: [(a,Int,Int)] -> [[a]]
minMaxList i = filter notnull $ map quantityList $ concatMap  permutations $ transpose $ [take (wideLCM [t | t <- map length (map expandMinMax i), t /= 0]) (cycle a) | a <- map expandMinMax i]
-}

minMaxList :: [(a,Int,Int)] -> [[a]]
minMaxList i = 
  map quantityList $
  sequence $
  --map concat $
  --map permutations $
  [take (wideLCM (map length ei)) (cycle a) | a <- ei]
     where ei = [expandMinMax (a,b,c) | (a,b,c) <- i, c /= 0]

expandMinMax :: (a,Int,Int) -> [(a,Int)]
expandMinMax (a,min,max) =  zip (repeat a) [min..max]

testList :: [(Integer,Int,Int)]
testList = [(1,3,4),(2,1,3),(3,3,5)]

-- yfeldblum on StackOverflow for this one
wideLCM :: [Int] -> Int
wideLCM = foldl lcm 1

quantityList :: [(a,Int)] -> [a]
quantityList [] = []
quantityList ((a,num):xs)
  | num <= 0 = quantityList xs
  | otherwise= (replicate num a) ++ (quantityList xs)

roleList :: [Role]
roleList = [vanilla,cVigilante,doctor,roleCop,aCop,detective,goon,roleblocker]

-- for testing purposes
testRoles :: [Role]
testRoles = [aCop, doctor, vanilla, vanilla, vanilla, goon, goon]
