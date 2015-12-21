module Simplify where

type Context = [(String, Expression)]

data Expression = Constant Integer | Variable String | Function String
                | If Expression Expression Expression
                | Application Expression Expression
                | Lambda String Expression
                | Let Context Expression
                deriving (Eq, Show)

-- Пока выражение меняется повторяем упрощение
simplify :: Expression -> Expression
simplify exp
    | exp == simplify' exp  = exp
    | otherwise             = simplify $ simplify' exp

-- Функция упрощения
simplify' :: Expression -> Expression
simplify' e@(Constant _)                                            = e
simplify' e@(Variable _)                                            = e
simplify' e@(Function _)                                            = e
simplify' (If p t e)                                                = (If (simplify' p) (simplify' t) (simplify' e))
simplify' (Application (Application (Function "+") (Constant 0)) e) = simplify' e
simplify' (Application (Application (Function "+") e) (Constant 0)) = simplify' e
simplify' (Application (Application (Function "-") e) (Constant 0)) = simplify' e
simplify' (Application (Application (Function "*") (Constant 1)) e) = simplify' e
simplify' (Application (Application (Function "*") e) (Constant 1)) = simplify' e
simplify' (Application (Application (Function "*") (Constant 0)) e) = (Constant 0)
simplify' (Application (Application (Function "*") e) (Constant 0)) = (Constant 0)
simplify' (Application e1 e2)                                       = (Application (simplify' e1) (simplify' e2))
simplify' (Lambda s e)                                              = (Lambda s (simplify' e))
simplify' (Let ctx e)                                               = (Let (map (\x -> (fst x, simplify' $ snd x)) ctx) (simplify' e))
