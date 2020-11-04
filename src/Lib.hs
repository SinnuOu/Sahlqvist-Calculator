module Lib
( sahlqvistCalculator
) where

import qualified Data.Set as Set
import Text.Read (readMaybe)
import Foreign.C.String
foreign import ccall "parseM" c_parseM :: CString -> IO CString

sahlqvistCalculator :: String -> IO String
sahlqvistCalculator s = do
    c_s <- withCString s c_parseM
    s' <- peekCString c_s
    return $ output $ do
        wffM <- parseM s'
        wff1 <- mTo1 wffM
        simplify1 wff1

data Variable = Variable String deriving (Eq, Ord)
data Predicate = Predicate String Word deriving (Eq, Ord)
data Term = TermV Variable deriving (Eq, Ord)
data Wff1 = Wff1Top
          | Wff1Bottom
          | Wff1P Predicate [Term]
          | Wff1E Term Term
          | Wff1And Wff1 Wff1
          | Wff1Or Wff1 Wff1
          | Wff1Not Wff1
          | Wff1To Wff1 Wff1
          | Wff1Forall Variable Wff1
          | Wff1Exists Variable Wff1
            deriving (Eq, Ord)
data Box = Box String Word deriving (Eq, Ord)
data Diamond = Diamond Box deriving (Eq, Ord)
data WffM = WffMTop
          | WffMBottom
          | WffMP Predicate
          | WffMAnd WffM WffM
          | WffMOr WffM WffM
          | WffMNot WffM
          | WffMTo WffM WffM
          | WffMBox Box [WffM]
          | WffMDiamond Diamond [WffM]
            deriving (Eq, Ord)

countStart :: Word
countStart = 0
tempVariable :: String
tempVariable = "x"
freeVariable :: Variable
freeVariable = Variable $ tempVariable ++ show countStart
tempVariable' :: String
tempVariable' = "y"
blank :: String
blank = " "
lp :: String
lp = "("
rp :: String
rp = ")"
showList' :: Show a => [a] -> Word -> String
showList' [] 0 = ""
showList' [] _ = error "showList'!"
showList' (t:ts) w = blank ++ show t ++ showList' ts (w-1)
instance Show Variable where
    show (Variable s) = s
instance Show Predicate where
    show (Predicate s 1) = s
    show _ = error "show Predicate!"
instance Show Term where
    show (TermV v) = show v
instance Show Wff1 where
    show Wff1Top = "Top"
    show Wff1Bottom = "Bottom"
    show (Wff1P (Predicate s w) ts) = "{" ++ s ++ blank ++ show w ++ "}" ++ showList' ts w
    show (Wff1E t1 t2) = "Equal" ++ blank ++ show t1 ++ blank ++ show t2
    show (Wff1And f1 f2) = "And" ++ blank ++ lp ++ show f1 ++ rp ++ blank ++ lp ++ show f2 ++ rp
    show (Wff1Or f1 f2) = "Or" ++ blank ++ lp ++ show f1 ++ rp ++ blank ++ lp ++ show f2 ++ rp
    show (Wff1Not f) = "Not" ++ blank ++ lp ++ show f ++ rp
    show (Wff1To f1 f2) = "To" ++ blank ++ lp ++ show f1 ++ rp ++ blank ++ lp ++ show f2 ++ rp
    show (Wff1Forall v f) = "Forall" ++ blank ++ show v ++ blank ++ lp ++ show f ++ rp
    show (Wff1Exists v f) = "Exists" ++ blank ++ show v ++ blank ++ lp ++ show f ++ rp
instance Show Box where
    show (Box s w) = "[" ++ s ++ blank ++ show w ++ "]"
instance Show Diamond where
    show (Diamond (Box s w)) = "<" ++ s ++ blank ++ show w ++ ">"
instance Show WffM where
    show WffMTop = "Top"
    show WffMBottom = "Bottom"
    show (WffMP p) = show p
    show (WffMAnd f1 f2) = lp ++ "And" ++ blank ++ show f1 ++ blank ++ show f2 ++ rp
    show (WffMOr f1 f2) = lp ++ "Or" ++ blank ++ show f1 ++ blank ++ show f2 ++ rp
    show (WffMNot f) = lp ++ "Not" ++ blank ++ show f ++ rp
    show (WffMTo f1 f2) = lp ++ "To" ++ blank ++ show f1 ++ blank ++ show f2 ++ rp
    show (WffMBox b@(Box _ w) fs) = lp ++ show b ++ showList' fs w ++ rp
    show (WffMDiamond d@(Diamond (Box _ w)) fs) = lp ++ show d ++ showList' fs w ++ rp
output :: Show a => (Either String a) -> String
output (Left s) = s
output (Right x) = show x
simplify1' :: Wff1 -> Wff1
simplify1' Wff1Top = Wff1Top
simplify1' Wff1Bottom = Wff1Bottom
simplify1' f@(Wff1P _ _) = f
simplify1' f@(Wff1E t1 t2) | t1 == t2 = Wff1Top
                           | otherwise = f
simplify1' (Wff1And (Wff1Not f1) (Wff1Not f2)) = Wff1Not $ Wff1Or (simplify1' f1) (simplify1' f2)
simplify1' (Wff1And f1 f2)
    | f1 == Wff1Top = simplify1' f2
    | f2 == Wff1Top = simplify1' f1
    | f1 == Wff1Bottom = Wff1Bottom
    | f2 == Wff1Bottom = Wff1Bottom
    | f1 == f2 = f1
    | otherwise = Wff1And (simplify1' f1) (simplify1' f2)
simplify1' (Wff1Or (Wff1Not f1) (Wff1Not f2)) = Wff1Not $ Wff1And (simplify1' f1) (simplify1' f2)
simplify1' (Wff1Or f1 f2)
    | f1 == Wff1Bottom = simplify1' f2
    | f2 == Wff1Bottom = simplify1' f1
    | f1 == Wff1Top = Wff1Top
    | f2 == Wff1Top = Wff1Top
    | f1 == f2 = f1
    | otherwise = Wff1Or (simplify1' f1) (simplify1' f2)
simplify1' (Wff1Not (Wff1Not f)) = simplify1' f
simplify1' (Wff1Not (Wff1Forall v (Wff1Not f))) = Wff1Exists v $ simplify1' f
simplify1' (Wff1Not (Wff1Exists v (Wff1Not f))) = Wff1Forall v $ simplify1' f
simplify1' (Wff1Not f)
    | f == Wff1Top = Wff1Bottom
    | f == Wff1Bottom = Wff1Top
    | otherwise = Wff1Not $ simplify1' f
simplify1' (Wff1To (Wff1Not f1) (Wff1Not f2)) = simplify1' $ Wff1To f2 f1
simplify1' (Wff1To (Wff1Not f1) f2) = simplify1' $ Wff1Or f1 f2
simplify1' (Wff1To f1 (Wff1Not f2)) = simplify1' $ Wff1Not $ Wff1And f1 f2
simplify1' (Wff1To f1 f2)
    | f1 == Wff1Top = simplify1' f2
    | f2 == Wff1Top = Wff1Top
    | f1 == Wff1Bottom = Wff1Top
    | f2 == Wff1Bottom = simplify1' $ Wff1Not f1
    | f1 == f2 = Wff1Top
    | otherwise = Wff1To (simplify1' f1) (simplify1' f2)
simplify1' (Wff1Forall v f) | Set.member v $ getFreeVariables1 f = Wff1Forall v $ simplify1' f
                            | otherwise = simplify1' f
simplify1' (Wff1Exists v0 (Wff1And (Wff1P p [TermV v1, TermV v2]) (Wff1E (TermV v3) (TermV v4))))
    | v0 == v3 && v0 /= v4 = simplify1' $ Wff1P p [TermV $ if v0 == v1 then v4 else v1, TermV $ if v0 == v2 then v4 else v2]
    | v0 == v4 && v0 /= v3 = simplify1' $ Wff1P p [TermV $ if v0 == v1 then v3 else v1, TermV $ if v0 == v2 then v3 else v2]
simplify1' (Wff1Exists v0 (Wff1And (Wff1E (TermV v3) (TermV v4)) (Wff1P p [TermV v1, TermV v2])))
    | v0 == v3 && v0 /= v4 = simplify1' $ Wff1P p [TermV $ if v0 == v1 then v4 else v1, TermV $ if v0 == v2 then v4 else v2]
    | v0 == v4 && v0 /= v3 = simplify1' $ Wff1P p [TermV $ if v0 == v1 then v3 else v1, TermV $ if v0 == v2 then v3 else v2]
simplify1' (Wff1Exists v f) | Set.member v $ getFreeVariables1 f = Wff1Exists v $ simplify1' f
                            | otherwise = simplify1' f
fix' :: Eq a => (a -> a) -> a -> a
fix' f = f1
    where f2 x y | x == y = x
                 | otherwise = f2 y $ f y
          f1 x = f2 x $ f x
simplify1 :: Wff1 -> Either String Wff1
simplify1 = Right . fix' simplify1'
parseError :: String
parseError = "Syntax error!"

parseBlank :: String -> Either String (String, String)
parseBlank "" = Left parseError
parseBlank (' ':_) = Left parseError
parseBlank s = parseBlank' ("", s, 0, False, False)

parseBlank' :: (String, String, Word, Bool, Bool) -> Either String (String, String)
parseBlank' (s, "", 0, False, False) = Right (s, "")
parseBlank' (_, "", _, _, _) = Left parseError
parseBlank' (_, c:_, _, True, _) | c == '(' ||
                                   c == ')' ||
                                   c == '[' ||
                                   c == '<' ||
                                   c == '>' = Left parseError
parseBlank' (_, c:_, _, _, True) | c == '(' ||
                                   c == ')' ||
                                   c == '[' ||
                                   c == ']' ||
                                   c == '<' = Left parseError
parseBlank' (h, '[':t, w, False, False) = parseBlank' (h ++ "[", t, w, True, False)
parseBlank' (h, '<':t, w, False, False) = parseBlank' (h ++ "<", t, w, False, True)
parseBlank' (_, ']':_, _, False, _) = Left parseError
parseBlank' (h, ']':t, w, True, False) = parseBlank' (h ++ "]", t, w, False, False)
parseBlank' (_, '>':_, _, _, False) = Left parseError
parseBlank' (h, '>':t, w, False, True) = parseBlank' (h ++ ">", t, w, False, False)
parseBlank' (h, '(':t, w, False, False) = parseBlank' (h ++ "(", t, w+1, False, False)
parseBlank' (h, ')':t, w, False, False) | w == 0 = Left parseError
                                        | otherwise = parseBlank' (h ++ ")", t, w-1, False, False)
parseBlank' (h, ' ':t, 0, False, False) = Right (h, t)
parseBlank' (h, c:t, w, b1, b2) = parseBlank' (h ++ [c], t, w, b1, b2)

parseM :: String -> Either String WffM
parseM "" = Left parseError
parseM s@(c:t) | c /= '(' = Right $ case s of "Top" -> WffMTop
                                              "Bottom" -> WffMBottom
                                              _ -> WffMP (Predicate s 1)
               | null t || last t /= ')' = Left parseError
               | otherwise = do
                   parse1 <- parseBlank $ init t
                   parse1h <- Right $ fst parse1
                   parse1t <- Right $ snd parse1
                   case parse1h of '[':_ -> parseModality parse1h parse1t 
                                   '<':_ -> parseModality parse1h parse1t
                                   "Not" -> do
                                       f <- parseM parse1t
                                       Right $ WffMNot f
                                   _ -> do
                                       parse2 <- parseBlank parse1t
                                       f1 <- parseM $ fst parse2
                                       f2 <- parseM $ snd parse2
                                       case parse1h of "And" -> Right $ WffMAnd f1 f2
                                                       "Or" -> Right $ WffMOr f1 f2
                                                       "To" -> Right $ WffMTo f1 f2
                                                       _ -> Left parseError

parseMs :: Word -> String -> Either String [WffM]
parseMs 0 "" = Right []
parseMs 0 _ = Left parseError
parseMs w s = do
    parse1 <- parseBlank s
    f <- parseM $ fst parse1
    fs <- parseMs (w-1) $ snd parse1
    Right $ f:fs

parseModality :: String -> String -> Either String WffM
parseModality modality@(c:_) s = do
    parse1 <- parseBlank $ init $ tail $ modality
    name <- Right $ fst parse1
    arity <- case (readMaybe $ snd parse1) :: Maybe Word of Just w -> Right w
                                                            Nothing -> Left parseError
    fs <- parseMs arity s
    Right $ case c of '[' -> WffMBox (Box name arity) fs
                      '<' -> WffMDiamond (Diamond $ Box name arity) fs

mTo1 :: WffM -> Either String Wff1
mTo1 f | sahlqvistFormula f = Right $ sahlqvistFormulaTo1 f
       | otherwise = Left "Not a Sahlqvist formula!"

sahlqvistFormulaTo1 :: WffM -> Wff1
sahlqvistFormulaTo1 (WffMBox (Box s w) fs) = forallClause
    where ys = [ Variable $ tempVariable' ++ show c | (_, c) <- zip fs [countStart..] ]
          subs = [ sibstitution1 (sahlqvistFormulaTo1 f, freeVariable) y | (f, y) <- zip fs ys ]
          orClause = foldl Wff1Or Wff1Bottom subs
          toClause = Wff1To (Wff1P (Predicate s (w+1)) $ map TermV (freeVariable:ys)) orClause
          forallClause = foldl (flip Wff1Forall) toClause ys
sahlqvistFormulaTo1 (WffMAnd f1 f2) = Wff1And (sahlqvistFormulaTo1 f1) (sahlqvistFormulaTo1 f2)
sahlqvistFormulaTo1 (WffMOr f1 f2) = Wff1Or (sahlqvistFormulaTo1 f1) (sahlqvistFormulaTo1 f2)
sahlqvistFormulaTo1 f = shalqvistImplicationTo1 f

shalqvistImplicationTo1 :: WffM -> Wff1
shalqvistImplicationTo1 f = shalqvistImplicationTo1' $ Wff1To (Wff1And f1 $ Wff1And Wff1Top Wff1Top) f2
    where (Wff1To f1 f2, _) = standardTranslationTo1 (f, freeVariable, countStart+1)

shalqvistImplicationTo1' :: Wff1 -> Wff1
shalqvistImplicationTo1' (Wff1To (Wff1And (Wff1And f1 f2) (Wff1And f3 f3')) f4) =
    shalqvistImplicationTo1' $ Wff1To (Wff1And f1 $ Wff1And f3 $ Wff1And f2 f3') f4
shalqvistImplicationTo1' (Wff1To (Wff1And (Wff1Or f1 f2) f3) f4) =
    Wff1Or (shalqvistImplicationTo1' $ Wff1To (Wff1And f1 f3) f4)
           (shalqvistImplicationTo1' $ Wff1To (Wff1And f2 f3) f4)
shalqvistImplicationTo1' (Wff1To (Wff1And (Wff1Exists v f1) f2) f3) =
    Wff1Forall v $ shalqvistImplicationTo1' $ Wff1To (Wff1And f1 f2) f3
shalqvistImplicationTo1' (Wff1To (Wff1And Wff1Top (Wff1And f3 f3')) f4) =
    case f3' of Wff1Top -> assignment $ Wff1To f3 f4
                Wff1And f5 f6 -> shalqvistImplicationTo1' (Wff1To (Wff1And f5 (Wff1And f3 f6)) f4)
shalqvistImplicationTo1' (Wff1To (Wff1And f1 (Wff1And f3 f3')) f4)
    | negative1 f1 = shalqvistImplicationTo1' $ Wff1To (Wff1And Wff1Top $ Wff1And f3 f3') $ Wff1Or f4 $ Wff1Not f1
    | otherwise = shalqvistImplicationTo1' $ Wff1To (Wff1And Wff1Top $ Wff1And (Wff1And f1 f3) f3') $ f4

instantiation :: Wff1 -> (Predicate, Term -> Wff1)
instantiation (Wff1P p [t]) = (p, \x -> Wff1E x t)
instantiation (Wff1Forall v (Wff1To rel (Wff1Or Wff1Bottom f))) = (p', Wff1Exists v . Wff1And rel . fun)
    where (p', fun) = instantiation f

instantiations :: Wff1 -> [(Predicate, Term -> Wff1)]
instantiations Wff1Top = []
instantiations (Wff1And f fs) = instantiation f : instantiations fs

assignment :: Wff1 -> Wff1
assignment (Wff1To f1' f2') = map' subs $ Wff1To (filter' (not . boxed) f1') f2'
    where boxed (Wff1P (Predicate _ w) _) | w > 1 = False
          boxed _ = True
          filter' _ Wff1Top = Wff1Top
          filter' fun (Wff1And f fs) | fun f = Wff1And f $ filter' fun fs
                                     | otherwise = filter' fun fs
          subs = instantiations $ filter' boxed f1'
          map' l (Wff1P p@(Predicate _ 1) [t]) = foldl Wff1Or Wff1Bottom [ f t | (p', f) <- l, p' == p ]
          map' l (Wff1And f1 f2) = Wff1And (map' l f1) (map' l f2)
          map' l (Wff1Or f1 f2) = Wff1Or (map' l f1) (map' l f2)
          map' l (Wff1Not f) = Wff1Not $ map' l f
          map' l (Wff1To f1 f2) = Wff1To (map' l f1) (map' l f2)
          map' l (Wff1Forall v f) = Wff1Forall v $ map' l f
          map' l (Wff1Exists v f) = Wff1Exists v $ map' l f
          map' _ f = f

standardTranslationTo1' :: Word -> [(WffM, Variable)] -> (Word, [Wff1])
standardTranslationTo1' w [] = (w,[])
standardTranslationTo1' w ((f, v):fvs) = (w2, f1:fs)
    where (f1, w1) = standardTranslationTo1 (f, v, w)
          (w2, fs) = standardTranslationTo1' w1 fvs

standardTranslationTo1 :: (WffM, Variable, Word) -> (Wff1, Word)
standardTranslationTo1 (WffMTop, _, w) = (Wff1Top, w)
standardTranslationTo1 (WffMBottom, _, w) = (Wff1Bottom, w)
standardTranslationTo1 (WffMP p, v, w) = (Wff1P p [TermV v], w)
standardTranslationTo1 (WffMAnd f1 f2, v, w) = (Wff1And f1' f2', w')
    where (w', [f1',f2']) = standardTranslationTo1' w [(f1, v),(f2, v)]
standardTranslationTo1 (WffMOr f1 f2, v, w) = (Wff1Or f1' f2', w')
    where (w', [f1',f2']) = standardTranslationTo1' w [(f1, v),(f2, v)]
standardTranslationTo1 (WffMNot f, v, w) = (Wff1Not f', w')
    where (f', w') = standardTranslationTo1 (f, v, w)
standardTranslationTo1 (WffMTo f1 f2, v, w) = (Wff1To f1' f2', w')
    where (w', [f1',f2']) = standardTranslationTo1' w [(f1, v),(f2, v)]
standardTranslationTo1 (WffMBox (Box s w0) fs0, v, w1) = (forallClause, w2)
    where vs = [ Variable $ tempVariable ++ show w | (_, w) <- zip fs0 [w1..] ]
          (w2, fs1) = standardTranslationTo1' (w1+w0) $ zip fs0 vs
          orClause = foldl Wff1Or Wff1Bottom fs1
          toClause = Wff1To (Wff1P (Predicate s (w0+1)) $ map TermV (v:vs)) orClause
          forallClause = foldl (flip Wff1Forall) toClause vs
standardTranslationTo1 (WffMDiamond (Diamond (Box s w0)) fs0, v, w1) = (existsClause, w2)
    where vs = [ Variable $ tempVariable ++ show w | (_, w) <- zip fs0 [w1..] ]
          (w2, fs1) = standardTranslationTo1' (w1+w0) $ zip fs0 vs
          andClause = foldl Wff1And (Wff1P (Predicate s (w0+1)) $ map TermV (v:vs)) fs1
          existsClause = foldl (flip Wff1Exists) andClause vs

substitutionT :: (Term, Variable) -> Variable -> Term
substitutionT (TermV v, x) y | v == x = TermV y
                             | otherwise = TermV v

sibstitution1 :: (Wff1, Variable) -> Variable -> Wff1
sibstitution1 (Wff1Top, _) _ = Wff1Top
sibstitution1 (Wff1Bottom, _) _ = Wff1Bottom
sibstitution1 (Wff1P p ts, x) y = Wff1P p [ substitutionT (t, x) y | t <- ts ]
sibstitution1 (Wff1E t1 t2, x) y = Wff1E (substitutionT (t1, x) y) (substitutionT (t2, x) y)
sibstitution1 (Wff1And f1 f2, x) y = Wff1And (sibstitution1 (f1, x) y) (sibstitution1 (f2, x) y)
sibstitution1 (Wff1Or f1 f2, x) y = Wff1Or (sibstitution1 (f1, x) y) (sibstitution1 (f2, x) y)
sibstitution1 (Wff1Not f, x) y = Wff1Not $ sibstitution1 (f, x) y
sibstitution1 (Wff1To f1 f2, x) y = Wff1To (sibstitution1 (f1, x) y) (sibstitution1 (f2, x) y)
sibstitution1 (Wff1Forall v f, x) y | v == x = Wff1Forall x f
                                    | otherwise = Wff1Forall v $ sibstitution1 (f, x) y
sibstitution1 (Wff1Exists v f, x) y | v == x = Wff1Exists x f
                                    | otherwise = Wff1Exists v $ sibstitution1 (f, x) y

getFreeVariablesT :: Term -> Set.Set Variable
getFreeVariablesT (TermV v) = Set.fromList [v]

getFreeVariables1 :: Wff1 -> Set.Set Variable
getFreeVariables1 Wff1Top = Set.empty
getFreeVariables1 Wff1Bottom = Set.empty
getFreeVariables1 (Wff1P _ ts) = foldl Set.union Set.empty $ map getFreeVariablesT ts
getFreeVariables1 (Wff1E t1 t2) = foldl Set.union Set.empty $ map getFreeVariablesT [t1, t2]
getFreeVariables1 (Wff1And f1 f2) = Set.union (getFreeVariables1 f1) (getFreeVariables1 f2)
getFreeVariables1 (Wff1Or f1 f2) = Set.union (getFreeVariables1 f1) (getFreeVariables1 f2)
getFreeVariables1 (Wff1Not f) = getFreeVariables1 f
getFreeVariables1 (Wff1To f1 f2) = Set.union (getFreeVariables1 f1) (getFreeVariables1 f2)
getFreeVariables1 (Wff1Forall v f) = Set.delete v (getFreeVariables1 f)
getFreeVariables1 (Wff1Exists v f) = Set.delete v (getFreeVariables1 f)

getAtoms :: WffM -> Set.Set Predicate
getAtoms WffMTop = Set.empty
getAtoms WffMBottom = Set.empty
getAtoms (WffMP p) = Set.fromList [p]
getAtoms (WffMAnd f1 f2) = Set.union (getAtoms f1) (getAtoms f2)
getAtoms (WffMOr f1 f2) = Set.union (getAtoms f1) (getAtoms f2)
getAtoms (WffMNot f) = getAtoms f
getAtoms (WffMTo f1 f2) = Set.union (getAtoms f1) (getAtoms f2)
getAtoms (WffMBox _ fs) = foldl Set.union Set.empty $ map getAtoms fs
getAtoms (WffMDiamond _ fs) = foldl Set.union Set.empty $ map getAtoms fs

boxedAtom :: WffM -> Bool
boxedAtom (WffMP _) = True
boxedAtom (WffMBox (Box _ 1) [f]) = boxedAtom f
boxedAtom _ = False

positive1 :: Wff1 -> Bool
positive1 Wff1Top = True
positive1 Wff1Bottom = True
positive1 (Wff1P _ _) = True
positive1 (Wff1E _ _) = True
positive1 (Wff1And f1 f2) = positive1 f1 && positive1 f2
positive1 (Wff1Or f1 f2) = positive1 f1 && positive1 f2
positive1 (Wff1Not f) = negative1 f
positive1 (Wff1To f1 f2) = negative1 f1 && positive1 f2
positive1 (Wff1Forall _ f) = positive1 f
positive1 (Wff1Exists _ f) = positive1 f

positiveM :: WffM -> Bool
positiveM WffMTop = True
positiveM WffMBottom = True
positiveM (WffMP _) = True
positiveM (WffMAnd f1 f2) = positiveM f1 && positiveM f2
positiveM (WffMOr f1 f2) = positiveM f1 && positiveM f2
positiveM (WffMNot f) = negativeM f
positiveM (WffMTo f1 f2) = negativeM f1 && positiveM f2
positiveM (WffMBox _ fs) = foldl (&&) True $ map positiveM fs
positiveM (WffMDiamond _ fs) = foldl (&&) True $ map positiveM fs

negative1 :: Wff1 -> Bool
negative1 Wff1Top = True
negative1 Wff1Bottom = True
negative1 (Wff1P (Predicate _ 0) _) = True
negative1 (Wff1P _ _) = False
negative1 (Wff1E _ _) = True
negative1 (Wff1And f1 f2) = negative1 f1 && negative1 f2
negative1 (Wff1Or f1 f2) = negative1 f1 && negative1 f2
negative1 (Wff1Not f) = positive1 f
negative1 (Wff1To f1 f2) = positive1 f1 && negative1 f2
negative1 (Wff1Forall _ f) = negative1 f
negative1 (Wff1Exists _ f) = negative1 f

negativeM :: WffM -> Bool
negativeM WffMTop = True
negativeM WffMBottom = True
negativeM (WffMP _) = False
negativeM (WffMAnd f1 f2) = negativeM f1 && negativeM f2
negativeM (WffMOr f1 f2) = negativeM f1 && negativeM f2
negativeM (WffMNot f) = positiveM f
negativeM (WffMTo f1 f2) = positiveM f1 && negativeM f2
negativeM (WffMBox _ fs) = foldl (&&) True $ map negativeM fs
negativeM (WffMDiamond _ fs) = foldl (&&) True $ map negativeM fs

sahlqvistAntecedent' :: WffM -> Bool
sahlqvistAntecedent' WffMTop = True
sahlqvistAntecedent' WffMBottom = True
sahlqvistAntecedent' f = boxedAtom f || negativeM f

sahlqvistAntecedent :: WffM -> Bool
sahlqvistAntecedent (WffMAnd f1 f2) = sahlqvistAntecedent f1 && sahlqvistAntecedent f2
sahlqvistAntecedent (WffMOr f1 f2) = sahlqvistAntecedent f1 && sahlqvistAntecedent f2
sahlqvistAntecedent (WffMDiamond _ fs) = foldl (&&) True $ map sahlqvistAntecedent fs
sahlqvistAntecedent f = sahlqvistAntecedent' f

shalqvistImplication :: WffM -> Bool
shalqvistImplication (WffMTo f1 f2) = sahlqvistAntecedent f1 && positiveM f2
shalqvistImplication _ = False

sahlqvistFormula :: WffM -> Bool
sahlqvistFormula (WffMBox _ fs) = foldl (&&) True $ map sahlqvistFormula fs
sahlqvistFormula (WffMAnd f1 f2) = sahlqvistFormula f1 && sahlqvistFormula f2
sahlqvistFormula (WffMOr f1 f2) | Set.null $ Set.intersection (getAtoms f1) (getAtoms f2) = sahlqvistFormula f1 && sahlqvistFormula f2
                                | otherwise = False
sahlqvistFormula f = shalqvistImplication f