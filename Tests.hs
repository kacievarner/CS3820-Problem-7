module Tests where

-- GHC
import System.Exit
import System.Environment
import System.IO.Silently

-- External
import Test.HUnit

import Data.Char (isSpace)
import Control.Monad
import Control.Monad.State

-- Lib
import Problem7

--------------------------------------------------------------------------------
-- Part 1
--------------------------------------------------------------------------------

captureCount :: String -> Int -> String
captureCount = evalState . addCount

p1 :: Test
p1 = test [
  captureCount "foobar" 0
    @?= "foobar" ++ (replicate 71 ' ') ++ "(6)",
  captureCount "1" 0
    @?= "1" ++ (replicate 76 ' ') ++ "(1)",
  captureCount "1" 80
    @?= "1" ++ (replicate 75 ' ') ++ "(81)",
  captureCount "foobar" 40
    @?= "foobar" ++ (replicate 70 ' ') ++ "(46)",
  let s = (concat (replicate 15 "five ")) in
    captureCount s 0 @?= s ++ " (75)",
  captureCount
  (filter (not . isSpace) "The smallest positive integer not definable in under sixty letters") 0
    @?= "Thesmallestpositiveintegernotdefinableinundersixtyletters" ++ (replicate 19 ' ')
    ++ "(57)"
  ]

--------------------------------------------------------------------------------
-- Part 2
--------------------------------------------------------------------------------

captureCounts :: [String] -> Int -> [String]
captureCounts = evalState . addCounts

p2 :: Test
p2 = test [
  captureCounts [] 0 @?= [],
  captureCounts [replicate 76 '0'] 0 @?= [replicate 76 '0' ++ "(76)"],
  captureCounts ["foobar", "or", "not", "to", "foobar"] 0
    @?= [
      "foobar" ++ (replicate 71 ' ') ++ "(6)",
      "or" ++ (replicate 75 ' ') ++ "(8)",
      "not" ++ (replicate 73 ' ') ++ "(11)",
      "to" ++ (replicate 74 ' ') ++ "(13)",
      "foobar" ++ (replicate 70 ' ') ++ "(19)"
      ],
    captureCounts [replicate 75 '0', replicate 75 '1', replicate 75 '2'] 100
      @?= [
      replicate 75 '0' ++ "(175)",
      replicate 75 '1' ++ "(250)",
      replicate 75 '2' ++ "(325)"
      ]
    
  ]

--------------------------------------------------------------------------------
-- Part 3
--------------------------------------------------------------------------------
sample1   = "foobar"
expected1 = "foobar" ++ (replicate 71 ' ') ++ "(6)\n"


sample2 = "What a waste it is to lose one's mind. Or not\n\
          \to have a mind is being very wasteful.\n\
          \How true that is."

expected2 =
  "What a waste it is to lose one's mind. Or not                               (45)\n\
  \to have a mind is being very wasteful.                                      (83)\n\
  \How true that is.                                                          (100)\n"

sample3 =
  "Hawaii has always been a very pivotal role in the Pacific.\n\
  \It is in the Pacific.\n\
  \It is a part of the United States that is an island\n\
  \that is right there."

expected3 =
  "Hawaii has always been a very pivotal role in the Pacific.                  (58)\n\
  \It is in the Pacific.                                                       (79)\n\
  \It is a part of the United States that is an island                        (130)\n\
  \that is right there.                                                       (150)\n"

sample4 =
  "This\n\
   \\n\n\n\n\n\n\
  \has too many\n\
  \blank\n\
  \lines"

expected4 =
  "This                                                                         (4)\n\
  \\n\n\n\n\n\n\                                                                               
  \has too many                                                                (16)\n\
  \blank                                                                       (21)\n\
  \lines                                                                       (26)\n"


testIO :: String -> String -> IO Test
testIO s expected = do
  (actual, _) <- capture . printLines $ s
  return . test $ (actual @?= expected)

p3 :: IO [Test]
p3 = sequence [
  testIO sample1 expected1,
  testIO sample2 expected2,
  testIO sample3 expected3,
  testIO sample4 expected4
  ]
  
--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

argMap :: Int -> IO Test
argMap 1 = return p1
argMap 2 = return p2
argMap 3 = do
  tests <- p3
  return . test $ tests
argMap _ = do
  p3' <- p3
  return . test $ [p1, p2, test p3']

hd :: [a] -> Maybe a
hd (x : _) = Just x
hd []       = Nothing

main :: IO ()
main = do
  args <- getArgs
  tests <- case read <$> (hd args) of
                Just x -> argMap x
                Nothing -> argMap 42
  results <- runTestTT tests
  if (errors results + failures results == 0)
    then
      exitWith ExitSuccess
    else
      exitWith (ExitFailure 1)
