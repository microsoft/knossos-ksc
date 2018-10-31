{-# LANGUAGE LambdaCase #-}

module Expression where

data Expression = Constant Int
                | Variable String
                | Expression :+ Expression
                | Expression :- Expression
                | Expression :* Expression
                | Expression :/ Expression
                deriving (Show, Eq)

data PartialExpression = (:<+) Expression
                       | (:>+) Expression
                       | (:>-) Expression
                       | (:<-) Expression
                       | (:>*) Expression
                       | (:<*) Expression
                       | (:>/) Expression
                       | (:</) Expression
  deriving (Show, Eq)

data ExpressionZ = ExpressionZ
  { focus :: Expression
  , rest  :: [PartialExpression]
  }
  deriving (Show, Eq)

numLeaves :: Expression -> Int
numLeaves = \case
  Constant _ -> 1
  Variable _ -> 1
  e1 :+ e2 -> numLeaves e1 + numLeaves e2
  e1 :- e2 -> numLeaves e1 + numLeaves e2
  e1 :* e2 -> numLeaves e1 + numLeaves e2
  e1 :/ e2 -> numLeaves e1 + numLeaves e2

numLeavesPartial :: PartialExpression -> Int
numLeavesPartial = \case
  (:<+) e -> numLeaves e
  (:>+) e -> numLeaves e
  (:<-) e -> numLeaves e
  (:>-) e -> numLeaves e
  (:<*) e -> numLeaves e
  (:>*) e -> numLeaves e
  (:</) e -> numLeaves e
  (:>/) e -> numLeaves e

numLeavesZ :: ExpressionZ -> Int
numLeavesZ e = numLeaves (focus e) + sum (map numLeavesPartial (rest e))

energy :: Floating a => ExpressionZ -> a
energy = fromIntegral . numLeavesZ

focusNeigbours :: ExpressionZ -> [ExpressionZ]
focusNeigbours e = up ++ down
  where up = case rest e of
          []        -> []
          (e':es) -> return $ case e' of
            (:<+) e'' -> ExpressionZ (focus e :+ e'') es
            (:<-) e'' -> ExpressionZ (focus e :- e'') es
            (:<*) e'' -> ExpressionZ (focus e :* e'') es
            (:</) e'' -> ExpressionZ (focus e :/ e'') es
        
            (:>+) e'' -> ExpressionZ (e'' :+ focus e) es
            (:>-) e'' -> ExpressionZ (e'' :- focus e) es
            (:>*) e'' -> ExpressionZ (e'' :* focus e) es
            (:>/) e'' -> ExpressionZ (e'' :/ focus e) es

        down = case focus e of
          Constant _ -> []
          Variable _ -> []
          e1 :+ e2   -> [ ExpressionZ e1 ((:<+) e2 : rest e)
                        , ExpressionZ e2 ((:>+) e1 : rest e)
                        ]
          e1 :- e2   -> [ ExpressionZ e1 ((:<-) e2 : rest e)
                        , ExpressionZ e2 ((:>-) e1 : rest e)
                        ]
          e1 :* e2   -> [ ExpressionZ e1 ((:<*) e2 : rest e)
                        , ExpressionZ e2 ((:>*) e1 : rest e)
                        ]
          e1 :/ e2   -> [ ExpressionZ e1 ((:</) e2 : rest e)
                        , ExpressionZ e2 ((:>/) e1 : rest e)
                        ]
  
transformNeighbours :: ExpressionZ -> [ExpressionZ]
transformNeighbours e = map (\focus' -> e { focus = focus' }) (transforms (focus e))

neighbours :: ExpressionZ -> [ExpressionZ]
neighbours e = focusNeigbours e ++ transformNeighbours e

transforms :: Expression -> [Expression]
transforms e = concat [ mult1
                      , unmult1
                      , pushDivide
--                      , unpushDivide
                      , pushMult
--                      , unpushMult
                      , distribute
--                      , undistribute
                      , aOvera
                      ]
  where mult1 = [ e :* Constant 1
                , Constant 1 :* e ]
        unmult1 = case e of
               e' :* Constant 1 -> [e']
               Constant 1 :* e' -> [e']
               _ -> []
        pushDivide = case e of
          (a :/ b) :/ c -> [a :/ (b :* c)]
          _ -> []
        unpushDivide = case e of
          a :/ (b :* c) -> [(a :/ b) :/ c]
          _ -> []
        pushMult = case e of
          a :* (b :/ c) -> [(a :* b) :/ c]
          _ -> []
        unpushMult = case e of
          (a :* b) :/ c -> [a :* (b :/ c)]
          _ -> []
        distribute = case e of
          a :* (b :+ c) -> [(a :* b) :+ (a :* c)]
          _ -> []
        undistribute = case e of
          (a :* b) :+ (a' :* c) ->
            if a == a'
            then [a :* (b :+ c)]
            else []
          _ -> []
        aOvera = case e of
          a :/ a' -> if a == a'
                     then [Constant 1]
                     else []
          _ -> []
        
andrew'sFavourite :: Expression
andrew'sFavourite = (one :/ x) :/ (one :+ (one :/ x))
  where x = Variable "x"
        one = Constant 1

hope :: [ExpressionZ]
hope = [
    ExpressionZ andrew'sFavourite []
  , ExpressionZ (one :/ (x :* (one :+ (one :/ x)))) []
  , ExpressionZ (x :* (one :+ (one :/ x))) [(:>/) one]
  , ExpressionZ ((x :* one) :+ (x :* (one :/ x))) [(:>/) one]
  , ExpressionZ (x :* one) [(:<+) (x :* (one :/ x)), (:>/) one]
  , ExpressionZ x [(:<+) (x :* (one :/ x)), (:>/) one]
  , ExpressionZ (x :+ (x :* (one :/ x))) [(:>/) one]
  , ExpressionZ (x :* (one :/ x)) [(:>+) x, (:>/) one]
  , ExpressionZ ((x :* one) :/ x) [(:>+) x, (:>/) one]
  , ExpressionZ (x :* one) [(:</) x, (:>+) x, (:>/) one]
  , ExpressionZ x [(:</) x, (:>+) x, (:>/) one]
  , ExpressionZ (x :/ x) [(:>+) x, (:>/) one]
  , ExpressionZ (Constant 1) [(:>+) x, (:>/) one]
  ]
  where x = Variable "x"
        one = Constant 1

check = mapM_ (\(from, to) -> print from >> print to >> putStrLn "") wrong
  where pairs = zip hope (tail hope)
        wrong = filter (\(from, to) -> not (to `elem` neighbours from)) pairs

hopeEnergy = map energy hope
