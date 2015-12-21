module FindConflict where
import Data.List(find)
import Data.Maybe

findConflict :: Expression -> Maybe String
findConflict = deepLearn

data Expression = Constant Integer | Variable String | Function String
                | Application Expression Expression
                | Lambda String Expression
                deriving (Eq, Show)

-- Разбираю выражение проходя вглубь лямбда-выражений и проверяя применения
deepLearn :: Expression -> Maybe String
deepLearn (Lambda s e)                   = deepLearn e
deepLearn (Application l@(Lambda s e) a) = case (checkConflict l a, deepLearn e, deepLearn a) of -- проверяем на конфликт
                                            (Just s, _, _)     -> Just s
                                            (_, Just s, _)     -> Just s
                                            (_, _, Just s)     -> Just s
                                            (_, _, _)          -> Nothing
deepLearn (Application e1 e2)            = case (deepLearn e1, deepLearn e2) of
                                            (Just s, _)     -> Just s
                                            (_, Just s)     -> Just s
                                            (_, _)          -> Nothing
deepLearn _                              = Nothing

-- Проверет есть ли пересечения между именами связанных переменных из лямбда-выражения и применяемого выражения
checkConflict :: Expression -> Expression -> Maybe String
checkConflict lambd exp = fromMaybe Nothing (find (isJust) $ map (\x -> find (== x) linkedVars) (findFreeVariable exp))
                        where linkedVars = findLinkedVariable lambd

-- Собирает имена переменных связанных лямбда-выражением
findLinkedVariable :: Expression -> [String]
findLinkedVariable (Lambda s e)         = s : (findLinkedVariable e)
findLinkedVariable (Application e1 e2)  = (findLinkedVariable e1) ++ (findLinkedVariable e2)
findLinkedVariable _                    = []

-- Собирает все имена переменных в Expression
findFreeVariable :: Expression -> [String]
findFreeVariable (Variable s)           = [s]
findFreeVariable (Application e1 e2)    = (findFreeVariable e1) ++ (findFreeVariable e2)
findFreeVariable (Lambda s e)           = filter (/= s) $ findFreeVariable e -- убираем связанную переменную, оставляя свободные
findFreeVariable _                      = []