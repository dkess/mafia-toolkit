module MafiaRole( Role (..),roleList, minMaxList, Color (..)) where

import Data.List
import Control.Applicative

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not.null) . map (take n) . iterate (drop n)

-- Massive thanks to dmwit in #haskell for figuring this out
select :: [a] -> [([a], [a])]
select xs = zip (inits xs) (tails xs)

-- I might change this to faction names instead of colors
data Color = Red | Blue | Green | Black | None deriving (Show, Eq)

data RoleFlags = RoleFlags {    -- RoleFlags are more dynamic than normal attributes
    bullets  :: Int,    -- bullets, for a vigilante 
    dtCheck  :: Role,   -- what this player returns as a dt check
    lives    :: Int,    -- lives carry over multiple nights
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
    sanity = Sane
}

data Role = Role {
    name    :: String,
    health  :: Int,
    color   :: Color,
    action  :: [Role] -> [[Role]],
    willAct :: Bool,
    startDay:: [Role -> Role],  -- actions done at the beginning
    rflags  :: RoleFlags,       -- if player has an optional action
    maxAmount::Int,
    minAmount::Int,
    defRole :: Bool, -- Decides if the role should be enabled by default
    fillRole:: Bool
}

--instance (Eq m) => Eq (Role m) where
instance Eq Role where
    x == y = name x == name y
    _ == _ = False

instance Show Role where
    --show role = name role ++ "(" ++ show (health role) ++ ")"
    show role = [head $ name role]

defaultRole :: Role     -- The default role
defaultRole = Role {
    name = "unnamed",
    health = 1,
    color = None,
    action = noAction,
    willAct = True,
    startDay = [],
    rflags = defaultFlags,
    maxAmount = 2,
    minAmount = 0,
    defRole = False,
    fillRole = False
} where noAction x = [x]

vigilante :: Role
vigilante = defaultRole {
    name = "Vigilante",
    color = Green,
    action = modHP (-1),
    willAct = False,
    maxAmount = 1
}

vanilla :: Role
vanilla = defaultRole {
    name = "Vanilla",
    color = Green,
    maxAmount = 4,
    minAmount = 3,
    defRole = True,
    fillRole = True
}

doctor :: Role
doctor = defaultRole {
    name = "Doctor",
    color = Green,
    action = modHP(1),
    defRole = True
}

roleCop :: Role     -- role, not alignment
roleCop = defaultRole {
    name = "Role Cop",
    color = Green
    -- action to be implemented later
}

aCop :: Role    -- Alignment cop
aCop = defaultRole {
    name = "Cop",
    color = Green,
    defRole = True
    -- action should take sanity as parameter
}

detective :: Role -- Role and alignment
detective = defaultRole {
    name = "Detective",
    color = Green
}

veteran :: Role
veteran = defaultRole {
    name = "Veteran",
    color = Green,
    rflags = defaultFlags {
        lives = 1
    }
}

watcher :: Role
watcher = defaultRole {
    name = "Watcher",
    color = Green
}

tracker :: Role
tracker = defaultRole {
    name = "Tracker",
    color = Green
}

goon :: Role
goon = defaultRole {
    name = "Goon",
    color = Red,
    defRole = True,
    fillRole = True,
    maxAmount = 0
}

godfather :: Role
godfather = defaultRole {
    name = "Godfather",
    color = Red,
    defRole = True,
    rflags = defaultFlags {
        dtCheck = defaultRole {
            name = "FakeGF",
            color = Green
        }
    },
    minAmount = 1,
    maxAmount = 1
}

roleblocker :: Role
roleblocker = defaultRole {
    name = "Roleblocker",
    color = Red,
    maxAmount = 1,
    action = listDist (\a -> a {willAct = False})
}

serialKiller :: Role
serialKiller = defaultRole {
    name = "Serial Killer",
    color = Black,
    maxAmount = 1,
    action = modHP (-1)
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
maybeAct :: Role -> ([Role] -> [[Role]]) -> [Role] -> [[Role]]
maybeAct q f r = if (willAct q) then
    f r
else
    [r]

minMaxList :: [(a,Int,Int)] -> [[a]]
minMaxList i = 
  map quantityList $
  sequence $
  [take (foldl lcm 1 (map length ei)) (cycle a) | a <- ei]
     where ei = [expandMinMax (a,b,c) | (a,b,c) <- i, c /= 0]

expandMinMax :: (a,Int,Int) -> [(a,Int)]
expandMinMax (a,min,max) =  zip (repeat a) [min..max]

testList :: [(Integer,Int,Int)]
testList = [(1,3,4),(2,1,3),(3,3,5)]

quantityList :: [(a,Int)] -> [a]
quantityList [] = []
quantityList ((a,num):xs)
  | num <= 0 = quantityList xs
  | otherwise= (replicate num a) ++ (quantityList xs)

roleList :: [Role]
roleList = [vanilla,vigilante,doctor,roleCop,aCop,detective,veteran,watcher,tracker,godfather,goon,roleblocker,serialKiller]

-- for testing purposes
testRoles :: [Role]
testRoles = [aCop, doctor, vanilla, vanilla, vanilla, goon, goon]
