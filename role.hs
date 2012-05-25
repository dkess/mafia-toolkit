import Data.List

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
    rflags  :: RoleFlags
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
    rflags = defaultFlags
} where noAction x = [[]]

cVigilante :: Role
cVigilante = defaultRole {
    name = "Comp. Vigilante",
    color = Green,
    action = modHP (-1),
    rflags = defaultFlags {
        willAct = True
    }
}

vanilla :: Role
vanilla = defaultRole {
    name = "Vanilla",
    color = Green
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
    color = Red
}

roleblocker :: Role
roleblocker = defaultRole {
    name = "Roleblocker",
    color = Red
}

-- Change health (s in sMod stands for single,
-- because it operates on a single role)
sModHP :: Int -> Role -> Role
sModHP num role = role {health = (health role) + num}

-- modHP takes a value and applies it to everyone in a
-- different possibility. A value of 1 heals, -1 is KP
modHP :: Int -> [Role] -> [[Role]]
modHP i r = actionDist (sModHP i) r

-- actionDist takes an action that modifies a role,
-- and gives the possibilities of it being acted on everyone
-- shoutout to dmwit in #haskell for figuring this out
actionDist :: (Role -> Role) -> [Role] -> [[Role]]
actionDist f r = [b ++ f m:e | (b,m:e) <- select r]

-- If willAct in the flags is set to False, do not give an action.
-- Otherwise perform the action
maybeAct :: RoleFlags -> ([Role] -> [[Role]]) -> [Role] -> [[Role]]
maybeAct flags f r = if (willAct flags) then
    f r
else
    [r]

-- for testing purposes
testRoles :: [Role]
testRoles = [aCop, doctor, vanilla, vanilla, vanilla, goon, goon]
