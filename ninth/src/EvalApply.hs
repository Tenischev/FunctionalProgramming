module EvalApply where

-- интерпретация
interpreter :: Expression -> Expression
interpreter = eval []

type Context = [(String, Expression)]

-- Ассоциируем значение из контекста с именем переменной
assoc :: String -> Context -> Expression
assoc x ((y,e):ctx) | x == y     =  e
                    | otherwise  = assoc x ctx

-- "Function - это символ примитивной функции, например, "<=", "sin"", '<=' и 'sin' имея только Integer?
data Expression = Constant Integer | Variable String | Function String
                | If Expression Expression Expression
                | Application Expression Expression
                | Lambda String Expression
                | Let Context Expression
                | Closure String Expression Context    -- до определяю замыкание
                | Oper Int String [Expression]         -- до определяю сечение
                deriving (Eq, Show)

-- вычисление значения выражения в контексте (приведение к СЗНФ):
eval :: Context -> Expression -> Expression
eval _   e@(Constant _)     = e
eval ctx (Variable x)       = assoc x ctx
eval _   (Function f)       = Oper (arity f) f []
eval ctx (If p t e)         = eval ctx (if (eval ctx p) /= (Constant 0) then t else e)
eval ctx (Application f a)  = apply (eval ctx f) (eval ctx a)
eval ctx (Lambda x e)       = Closure x e ctx
eval ctx (Let args body)    = eval newCtx body
            where newCtx = (map (\(x,arg) -> (x, eval newCtx arg)) args) ++ ctx
eval _   e@(Closure _ _ _)  = e
eval _   e@(Oper _ _ _)     = e

-- вычисление результата применения функции к аргументу
apply :: Expression -> Expression -> Expression
apply (Closure x body ctx) arg     = eval nc body
            where nc = (x, arg) : ctx
apply (Oper n f la) a | n == 1     = intrinsic f newListArgs
                      | otherwise  = Oper (n-1) f newListArgs
            where newListArgs = la ++ [a]

intrinsic "+" [Constant(a), Constant(b)] = Constant (a + b)
intrinsic "-" [Constant(a), Constant(b)] = Constant (a - b)
intrinsic "*" [Constant(a), Constant(b)] = Constant (a * b)

arity "+" = 2
arity "-" = 2
arity "*" = 2

