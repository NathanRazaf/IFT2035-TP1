-- Nathan Razafindrakoto 20254813 
-- Yasmine Ben Youssef 20237210

-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}
--
-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Pretty printer
-- - Implantation du langage

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Bibliothèque d'analyse syntaxique.
import Data.Char                -- Conversion de Chars de/vers Int et autres.
import System.IO                -- Pour stdout, hPutStr
---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          | Snode Sexp [Sexp]           -- Une liste non vide
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3) ==> Snode (Ssym "+")
--                   [Snum 2, Snum 3]
--
-- (/ (* (- 68 32) 5) 9)
--     ==>
-- Snode (Ssym "/")
--       [Snode (Ssym "*")
--              [Snode (Ssym "-")
--                     [Snum 68, Snum 32],
--               Snum 5],
--        Snum 9]

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> c /= '\n'));
                pChar '\n' <|> eof; return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment);
               return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (== '-')
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + digitToInt c)
                       <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuations.
pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (`elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 pSymchar;
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(quote E)"
pQuote :: Parser Sexp
pQuote = do { pChar '\''; pSpaces; e <- pSexp;
              return (Snode (Ssym "quote") [e]) }

-- Une liste est de la forme:  ( {e} [. e] )
pList :: Parser Sexp
pList  = do { pChar '('; pSpaces;
              ses <- pTail;
                    return (case ses of [] -> Snil
                                        se : ses' -> Snode se ses')
            }
pTail :: Parser [Sexp]
pTail  = do { pChar ')'; return [] }
     -- <|> do { pChar '.'; pSpaces; e <- pSexp; pSpaces;
     --          pChar ')' <|> error ("Missing ')' after: " ++ show e);
     --          return e }
     <|> do { e <- pSexp; pSpaces; es <- pTail; return (e : es) }

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { Just <$> anyChar ; } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pSpaces;
                pList <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _p s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = shows n
showSexp' (Ssym s) = showString s
showSexp' (Snode h t) =
    let showTail [] = showChar ')'
        showTail (e : es) =
            showChar ' ' . showSexp' e . showTail es
    in showChar '(' . showSexp' h . showTail t

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.



-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs/GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire Lexp                                     --
---------------------------------------------------------------------------

type Var = String

data Lexp = Lnum Int             -- Constante entière.
          | Lbool Bool           -- Constante Booléenne.
          | Lvar Var             -- Référence à une variable.
          | Ltest Lexp Lexp Lexp -- Expression conditionelle.
          | Lfob [Var] Lexp      -- Construction de fobjet.
          | Lsend Lexp [Lexp]    -- Appel de fobjet.
          | Llet Var Lexp Lexp   -- Déclaration non-récursive.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Lfix [(Var, Lexp)] Lexp
          deriving (Show, Eq)

-- Première passe simple qui analyse une Sexp et construit une Lexp équivalente

s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym s) = Lvar s

-- Déclaration de variable
s2l (Snode (Ssym var) [exp1]) = Lsend (s2l (Ssym var)) [s2l exp1] 
-- Construction de fobjet
s2l (Snode (Ssym "fob") [vars, expr]) =
  case s2l expr of
    Lfob vars2 expr2 -> Lfob (extractSnodeVars vars ++ vars2) expr2
    _ -> Lfob (extractSnodeVars vars) (s2l expr)
-- Conditionnelle
s2l (Snode (Ssym "if") [exp1 , exp2, exp3]) = 
  Ltest (s2l exp1) (s2l exp2) (s2l exp3)  
-- Déclaration locale non-récursive
s2l (Snode (Ssym "let") [Ssym var, exp1, exp2]) = 
  Llet var (s2l exp1) (s2l exp2) 
-- Déclaration de fonctions récursives
s2l (Snode (Ssym "fix") [declasNode, expr]) = 
  Lfix (extractDeclasFromSnodeAll declasNode) (s2l expr)  
-- Appel de fonction
s2l (Snode (Ssym funcName) params) = 
  Lsend (Lvar funcName) (map s2l params)
-- Appel par défaut pour les imbrications 
s2l (Snode exp0 vars) =
  case s2l exp0 of
    Lsend innerFn innerArgs -> Lsend innerFn (innerArgs ++ map s2l vars)
    otherExp -> Lsend otherExp (map s2l vars)
s2l se = error ("Expression Psil inconnue: " ++ showSexp se)

extractVar :: Sexp -> Var
extractVar (Ssym v) = v
extractVar _ = error "Symbole attendu"

-- Fonction qui extrait toutes les déclarations d'un nœud
extractDeclasFromSnodeAll :: Sexp -> [(Var, Lexp)]
extractDeclasFromSnodeAll (Snode snode snodes) = 
  extractDeclasFromSnode snode : extractDeclasFromSnodeList snodes
extractDeclasFromSnodeAll _ = error "Snode avec Ssym attendu"

-- Fonction qui extrait une déclaration individuelle
extractDeclasFromSnode :: Sexp -> (Var, Lexp)
-- Cas d'une déclaration simple de variable
extractDeclasFromSnode (Snode (Ssym var) [exp1]) = (var, s2l exp1)
-- Cas d'une déclaration de fonction (avec paramètres et corps)
extractDeclasFromSnode (Snode (Snode (Ssym funcName) vars) [expr]) = 
  (funcName, Lfob (extractVars vars) (s2l expr))
extractDeclasFromSnode _ = 
  error "Snode valide pour une déclaration attendu"

-- Fonction qui extrait une liste de déclarations
extractDeclasFromSnodeList :: [Sexp] -> [(Var, Lexp)]
extractDeclasFromSnodeList = map extractDeclasFromSnode


-- Fonction pour extraire une liste de Var à partir d'un Snode
extractSnodeVars :: Sexp -> [Var]
extractSnodeVars (Snode (Ssym funcName) syms) = funcName : extractVars syms
extractSnodeVars _ = error "Snode avec Ssym attendu"

-- Helper pour extraire la liste de Ssym dans les paramètres
extractVars :: [Sexp] -> [Var]
extractVars [] = []
extractVars (Ssym s : rest) = s : extractVars rest
extractVars _ = error "Liste de Ssym attendue"

-- Vérifie qu'un Sexp est un symbole (Ssym)
isSsym :: Sexp -> Bool
isSsym (Ssym _) = True
isSsym _        = False

---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

-- Type des valeurs manipulées à l'exécution.
data Value = Vnum Int
           | Vbool Bool
           | Vbuiltin ([Value] -> Value)
           | Vfob VEnv [Var] Lexp

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vbool b) = showsPrec p b
    showsPrec _ (Vbuiltin _) = showString "<primitive>"
    showsPrec _ (Vfob {}) = showString "<fobjet>"

type VEnv = [(Var, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: VEnv
env0 = let binop f op =
              Vbuiltin (\vs -> case vs of
                         [Vnum n1, Vnum n2] -> f (n1 `op` n2)
                         [_, _] -> error "Pas un nombre"
                         _ -> error "Nombre d'arguments incorrect")

          in [("+", binop Vnum (+)),
              ("*", binop Vnum (*)),
              ("/", binop Vnum div),
              ("-", binop Vnum (-)),
              ("<", binop Vbool (<)),
              (">", binop Vbool (>)),
              ("≤", binop Vbool (<=)),
              ("≥", binop Vbool (>=)),
              ("=", binop Vbool (==)),
              ("true",  Vbool True),
              ("false", Vbool False)]

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

eval :: VEnv -> Lexp -> Value
-- ¡¡ COMPLETER !!
eval _ (Lnum n) = Vnum n
eval _ (Lbool b) = Vbool b

eval env (Llet var val expr) = -- déclaration locale
  let val' = eval env val
      env' = (var, val') : env
  in eval env' expr

eval env (Lvar var) = case lookup var env of
                        Just v -> v
                        Nothing -> error ("Variable non définie: " ++ var)

eval env (Ltest cond e1 e2) =
  case eval env cond of
    Vbool True  -> eval env e1
    Vbool False -> eval env e2
    _           -> error "La condition doit retourner un booléen"

eval env (Lfob vars body) = Vfob env vars body

eval env (Lsend func params) = -- Apparemment j'ai réussi du premier coup
  case func of
    Lvar f -> -- Appel de fonction définie dans l'environnement
      case lookup f env of
        Just (Vfob env' vars body) ->
          let params' = map (eval env) params
              env'' = zip vars params' ++ env'
          in eval env'' body
        Just (Vbuiltin bfn) ->
          let params' = map (eval env) params
          in bfn params' -- Appel de la fonction built-in avec les paramètres
        _ -> error ("Fonction " ++ f ++ " indéfinie")
    Lfob vars body -> -- Appel de fonction définie localement
      let params' = map (eval env) params 
          env' = zip vars params' ++ env 
      in eval env' body

    _ -> eval env func


eval env (Lfix bindings body) =
  let -- Évaluer chaque binding récursivement
      -- On crée un environnement récursif où chaque variable 
      -- peut faire référence aux autres 
      recursiveEnv = map 
        (\(varName, lexp) -> (varName, eval (recursiveEnv ++ env) lexp)) 
        bindings
      -- Ajouter les bindings évalués à l'environnement courant
      env' = recursiveEnv ++ env
  in eval env' body


---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> Value
evalSexp = eval env0 . s2l

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do inputHandle <- openFile filename ReadMode
       hSetEncoding inputHandle utf8
       s <- hGetContents inputHandle
       (putStr . show)
           (let sexps s' = case parse pSexps filename s' of
                             Left _ -> [Ssym "#<parse-error>"]
                             Right es -> es
            in map evalSexp (sexps s))
       hClose inputHandle

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

valOf :: String -> Value
valOf = evalSexp . sexpOf
