module ShuntingYard(evaluateTokenList) where
import Queue ( concat, empty, fromList, pop, push, Queue )
import Parser ( Token(..) )
import Operation
    ( isLeftAssociative, priority, Bracket(Closed, Open), apply )
import Data.Maybe ( listToMaybe, mapMaybe )

evaluateTokenList :: [Token] -> Maybe Integer
evaluateTokenList ls = evaluateRPN =<< shuntingYard ls

shuntingYard :: [Token] -> Maybe (Queue Token)
shuntingYard ls = internalShuntingYard ls [] Queue.empty

internalShuntingYard :: [Token] -> [Token] -> Queue Token -> Maybe (Queue Token)
internalShuntingYard inp mid out
  | null inp && null mid = Just out
  | null inp = Just $ Queue.concat out $ Queue.fromList mid
  | otherwise = case head inp of
    In _      ->
      internalShuntingYard (tail inp) mid $ push (head inp) out
    Br Open   ->
      internalShuntingYard (tail inp) (head inp : mid) out
    Br Closed ->
      if null mid then
        Nothing
      else if head mid == Br Open then
        internalShuntingYard (tail inp) (tail mid) out
      else
        internalShuntingYard inp (tail mid) $ push (head mid) out
    Op ophead ->
      case listToMaybe mid of
        Just (Op i) | priority i > priority ophead || (priority i == priority ophead && isLeftAssociative ophead) ->
          internalShuntingYard inp (tail mid) $ push (head mid) out
        _ ->
          internalShuntingYard (tail inp) (head inp : mid) out

evaluateRPN :: Queue Token -> Maybe Integer
evaluateRPN q = internalEvaluateRPN q []

internalEvaluateRPN :: Queue Token -> [Token] -> Maybe Integer
internalEvaluateRPN q s =
    let (tok, rest) = pop q in case tok of
      Nothing -> finishedStack s
      Just (Op bin) -> case s of
        (In e1 : In e2 : stackLeft) -> 
          (\c -> internalEvaluateRPN rest $ In c : stackLeft) =<< apply e2 e1 bin
        _ -> 
          Nothing
      Just smt -> internalEvaluateRPN rest (smt : s)

finishedStack :: [Token] -> Maybe Integer
finishedStack s = case listToMaybe s of
  Just (In res) -> Just res
  _ -> Nothing
