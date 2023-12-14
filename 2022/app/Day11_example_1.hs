{-# LANGUAGE ScopedTypeVariables #-}
module Day11_example_1 (Monkey(..), getExampleMonkeys, getInputMonkeys) where

type WorryLevel = Int

data Monkey = Monkey {
  monkeyIndex :: Int,
  startingItems :: [WorryLevel],
  operation :: (Int -> Int),
  test :: (Int -> Bool),
  throwToIfTrue :: Int,
  throwToIfFalse :: Int,
  inspections :: Int
}

getExampleMonkeys :: [Monkey]
getExampleMonkeys = [
  Monkey {
    monkeyIndex = 0,
    startingItems = [79, 98],
    operation = (* 19),
    test = (\x -> x `mod` 23 == 0),
    throwToIfTrue = 2,
    throwToIfFalse = 3,
    inspections = 0
  },
  Monkey {
    monkeyIndex = 1,
    startingItems = [54, 65, 75, 74],
    operation = (+ 6),
    test = (\x -> x `mod` 19 == 0),
    throwToIfTrue = 2,
    throwToIfFalse = 0,
    inspections = 0
  },
  Monkey {
    monkeyIndex = 2,
    startingItems = [79, 60, 97],
    operation = (\x -> x * x),
    test = (\x -> x `mod` 13 == 0),
    throwToIfTrue = 1,
    throwToIfFalse = 3,
    inspections = 0
  },
  Monkey {
    monkeyIndex = 3,
    startingItems = [74],
    operation = (+ 3),
    test = (\x -> x `mod` 17 == 0),
    throwToIfTrue = 0,
    throwToIfFalse = 1,
    inspections = 0
  }
  ]

getInputMonkeys :: [Monkey]
getInputMonkeys = [
    Monkey {
      monkeyIndex = 0,
      startingItems = [52, 60, 85, 69, 75, 75],
      operation = (* 17),
      test = (\x -> x `mod` 13 == 0),
      throwToIfTrue = 6,
      throwToIfFalse = 7,
      inspections = 0
    },
    Monkey {
      monkeyIndex = 1,
      startingItems = [96, 82, 61, 99, 82, 84, 85],
      operation = (+ 8),
      test = (\x -> x `mod` 7 == 0),
      throwToIfTrue = 0,
      throwToIfFalse = 7,
      inspections = 0
    },
    Monkey {
      monkeyIndex = 2,
      startingItems = [95, 79],
      operation = (+ 6),
      test = (\x -> x `mod` 19 == 0),
      throwToIfTrue = 5,
      throwToIfFalse = 3,
      inspections = 0
    },
    Monkey {
      monkeyIndex = 3,
      startingItems = [88, 50, 82, 65, 77],
      operation = (* 19),
      test = (\x -> x `mod` 2 == 0),
      throwToIfTrue = 4,
      throwToIfFalse = 1,
      inspections = 0
    },
    Monkey {
      monkeyIndex = 4,
      startingItems = [66, 90, 59, 90, 87, 63, 53, 88],
      operation = (+ 7),
      test = (\x -> x `mod` 5 == 0),
      throwToIfTrue = 1,
      throwToIfFalse = 0,
      inspections = 0
    },
    Monkey {
      monkeyIndex = 5,
      startingItems = [92, 75, 62],
      operation = (\x -> x * x),
      test = (\x -> x `mod` 3 == 0),
      throwToIfTrue = 3,
      throwToIfFalse = 4,
      inspections = 0
    },
    Monkey {
      monkeyIndex = 6,
      startingItems = [94, 86, 76, 67],
      operation = (+ 1),
      test = (\x -> x `mod` 11 == 0),
      throwToIfTrue = 5,
      throwToIfFalse = 2,
      inspections = 0
    },
    Monkey {
      monkeyIndex = 7,
      startingItems = [57],
      operation = (+ 2),
      test = (\x -> x `mod` 17 == 0),
      throwToIfTrue = 6,
      throwToIfFalse = 2,
      inspections = 0
    }
    ]
