--a list of all actions we can do
{--
1. fil jug 1
2. fill jug 2
3. move jug 1 -> jug 2
4. move jug 2 -> jug 1
5. discard jug 1 
6. discard jug 2

--}
-- 1.
fillJug1 :: (Integer,Integer) -> Integer-> (Integer,Integer)
fillJug1 jugs sizeOfJug = if fst jugs == sizeOfJug then jugs else (sizeOfJug, snd jugs)
-- 2.
fillJug2 :: (Integer,Integer) -> Integer-> (Integer,Integer)
fillJug2 jugs sizeOfJug = if snd jugs == sizeOfJug then jugs else (fst jugs ,sizeOfJug) 
-- 3.
move1to2 :: (Integer,Integer) ->(Integer,Integer)-> (Integer,Integer)
move1to2 jugs jugSizes = if fst jugs == 0 || snd jugs == snd jugSizes then jugs else move1to2 (fst jugs - 1,snd jugs +1) jugSizes
-- 4.
move2to1 :: (Integer,Integer) ->(Integer,Integer)-> (Integer,Integer)
move2to1 jugs jugSizes = if snd jugs == 0 || fst jugs == fst jugSizes then jugs else move1to2 (fst jugs + 1,snd jugs -1) jugSizes
-- 4.
discardJug1 :: (Integer,Integer) -> (Integer,Integer)
discardJug1 jugs = (0 ,snd jugs)
-- 5.
discardJug2 :: (Integer,Integer) -> (Integer,Integer)
discardJug2 jugs = (fst jugs ,0)