module Operation where 

data Operation = Plus | Minus | Times | Div | Power deriving (Eq, Show)

data Bracket = Open | Closed deriving (Eq, Show)

priority :: Operation -> Int
priority op = 
  case op of
    Plus  -> 1
    Minus -> 1
    Times -> 2
    Div   -> 2
    Power -> 3

apply :: Integer -> Integer -> Operation -> Maybe Integer
apply e1 e2 op = case op of
  Plus  -> Just $ e1 + e2
  Minus -> Just $ e1 - e2
  Times -> Just $ e1 * e2
  Div   -> if e2 == 0 then Nothing else Just $ div e1 e2
  Power -> if e2 >= 0 then Just $ e1 ^ e2 else Nothing

isLeftAssociative :: Operation -> Bool
isLeftAssociative op = op /= Power

opFromChar :: Char -> Operation
opFromChar c = case c of
  '+' -> Plus
  '-' -> Minus
  '*' -> Times
  '/' -> Div
  '^' -> Power

brackFromChar :: Char -> Bracket
brackFromChar c = case c of 
  '(' -> Open
  ')' -> Closed
