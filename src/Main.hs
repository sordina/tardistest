module Main where

  -- http://hackage.haskell.org/package/tardis-0.3.0.0/docs/Control-Monad-Tardis.html

import Control.Monad.Tardis
import Data.Maybe

main :: IO ()
main = timeParadox

newtype Person       = Person { unPerson :: String } deriving (Eq, Show)
newtype Instruction  = Kill   { who      :: Person } deriving (Eq, Show)

type Instructions    = [Instruction]
type Living          = [Person]
type Paradoxically a = Tardis Instructions Living a

timeParadox :: IO ()
timeParadox = print $ evalTardis paradox ([], [june,pete,jane,joe])

june, pete, jane, joe :: Person
june = Person "Grandma June"
pete = Person "Grandpa Pete"
jane = Person "Grandma Jane"
joe  = Person "Grandpa Joe"

paradox :: Paradoxically Bool
paradox = do
  patGrams  <- getPerson june
  patGramps <- getPerson pete
  matGrams  <- getPerson jane
  matGramps <- getPerson joe

  inst <- getInstructions
  -- respondToInstructions inst
  ---- Ignore instruction and kill pete
  killPete

  dad       <- breed patGrams patGramps
  mom       <- breed matGrams matGramps
  maybeMe   <- breed dad mom

  if isJust maybeMe then sendInstructionBackThroughTime (Kill pete)
                    else return ()

  amIAlive

respondToInstructions :: [Instruction] -> Paradoxically ()
respondToInstructions ls = mapM_ respondToInstruction ls

respondToInstruction :: Instruction -> Paradoxically ()
respondToInstruction (Kill person) | person == pete = killPete
respondToInstruction (Kill _     )                  = return ()

breed :: Maybe Person -> Maybe Person -> Paradoxically (Maybe Person)
breed (Just x) (Just y) = do
  px <- getPerson x
  py <- getPerson y
  breedDumb px py
breed _ _ = return Nothing

breedDumb :: Maybe Person -> Maybe Person -> Paradoxically (Maybe Person)
breedDumb (Just x) (Just y) = do
  let newPerson = namePair x y
  personIsBorn newPerson
  return $ Just $ newPerson
breedDumb _ _ = return Nothing

namePair :: Person -> Person -> Person
namePair (Person x) (Person y) = Person $ concat ["(", x, " + ", y, ")"]

personIsBorn :: Person -> Paradoxically ()
personIsBorn person = modifyForwards (person : )

getPerson :: Person -> Paradoxically (Maybe Person)
getPerson person = do
  people <- getPast
  if elem person people
     then return (Just person)
     else return Nothing

killPete :: Paradoxically ()
killPete = killPerson pete

getInstructions :: Paradoxically Instructions
getInstructions = getFuture

sendInstructionBackThroughTime :: Instruction -> Paradoxically ()
sendInstructionBackThroughTime i = modifyBackwards (i : )

killPerson :: Person -> Paradoxically ()
killPerson person = modifyForwards (filter (/= person))

amIAlive :: Paradoxically Bool
amIAlive = do
  people <- getPast
  return $ elem (namePair (namePair june pete) (namePair jane joe)) people

{-
  patGrams  <- getPerson june
  patGramps <- getPerson pete
  matGrams  <- getPerson jane
  matGramps <- getPerson joe
  dad       <- breed patGrams patGramps
  mom       <- breed matGrams matGramps
  maybeMe   <- breed dad mom
  -}
