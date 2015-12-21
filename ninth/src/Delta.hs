module Delta where

delta :: Expression -> Expression
delta e = if (e == calc e) then e else (delta (calc e))

-- Разбирает выражение и производит δ-редукции
calc :: Expression -> Expression
calc e@(Application (Application (Function f) (Constant a)) (Constant b)) = if (canCalc f) then (Constant ((getFunc f) a b)) else e
calc (Application e1 e2)                                                  = Application (calc e1) (calc e2)
calc (Lambda s e)                                                         = Lambda s (calc e)
calc (Let ctx e)                                                          = Let (zip s newExp) (calc e)
                                                                            where (s, oldExp) = unzip ctx
                                                                                  newExp = map (calc) oldExp
calc (If p t e)                                                           = If (calc p) (calc t) (calc e)
calc e                                                                    = e

-- Проверяет следует ли пытаться вычислить функцию
canCalc f  = f == "+" || f == "-" || f == "*"

-- Возвращает функцию для применения к константам
getFunc f = case (f) of
            "+" -> (+)
            "-" -> (-)
            "*" -> (*)

data Expression = Constant Integer | Variable String | Function String
                | If Expression Expression Expression
                | Application Expression Expression
                | Lambda String Expression
                | Let [(String, Expression)] Expression
                deriving (Eq, Show)
