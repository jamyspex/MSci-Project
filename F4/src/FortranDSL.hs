module FortranDSL where

import           Language.Fortran
import           LanguageFortranTools

-- import           Utils
--      for = for' lessThanEq
for
     -- (Expr Anno -> Expr Anno -> Expr Anno)
 -- ->
 ::
     String -> Int -> Expr Anno -> Fortran Anno -> Fortran Anno
-- comparision
for loopVar initial lhs =
  For
    nullAnno
    nullSrcSpan
    (varName loopVar)
    (con initial)
    lhs -- (comparision (var loopVar) lhs)
    (con 1)

-- forLT = for' lessThan
nullUseBlock = UseBlock (UseNil nullAnno) NoSrcLoc

buildAstSeq :: (a -> a -> a) -> a -> [a] -> a
buildAstSeq _ nullNode [] = nullNode
buildAstSeq _ _ [statement] = statement
buildAstSeq constructor nullNode (statement:statements) =
  constructor statement (buildAstSeq constructor nullNode statements)

-- combines multiple conditions produced by buildLoopGuard with .and.
combineWithAnd :: [Expr Anno] -> Expr Anno
combineWithAnd =
  buildAstSeq
    (Bin nullAnno nullSrcSpan (And nullAnno))
    (NullExpr nullAnno nullSrcSpan)

argNode :: [String] -> Arg Anno
argNode names =
  Arg
    nullAnno
    (buildAstSeq (ASeq nullAnno) (NullArg nullAnno) argNames)
    nullSrcSpan
  where
    argNames = map (ArgName nullAnno) names

eq = Bin nullAnno nullSrcSpan (RelEQ nullAnno)

plus = Bin nullAnno nullSrcSpan (Plus nullAnno)

minus = Bin nullAnno nullSrcSpan (Minus nullAnno)

divide = Bin nullAnno nullSrcSpan (Div nullAnno)

modulo = Mod nullAnno nullSrcSpan

lessThanEq = Bin nullAnno nullSrcSpan (RelLE nullAnno)

lessThan = Bin nullAnno nullSrcSpan (RelLT nullAnno)

greaterThanEq = Bin nullAnno nullSrcSpan (RelGE nullAnno)

assign = Assg nullAnno nullSrcSpan

varName = VarName nullAnno

argName = ArgName nullAnno

block = buildAstSeq (FSeq nullAnno nullSrcSpan) (NullStmt nullAnno nullSrcSpan)

fortranModule name =
  Module
    nullAnno
    nullSrcSpan
    (SubName nullAnno name)
    (UseNil nullAnno)
    (ImplicitNull nullAnno)

sub name decls body args =
  Sub
    nullAnno
    nullSrcSpan
    Nothing
    (SubName nullAnno name)
    (if null args
       then Arg nullAnno (NullArg nullAnno) nullSrcSpan
       else Arg
              nullAnno
              (buildAstSeq (ASeq nullAnno) (NullArg nullAnno) args)
              nullSrcSpan)
    block
  where
    block =
      Block nullAnno nullUseBlock (ImplicitNull nullAnno) nullSrcSpan decls body

argList args =
  ArgList nullAnno $
  buildAstSeq
    (ESeq nullAnno nullSrcSpan)
    (NullExpr nullAnno nullSrcSpan)
    argsAsVars
  where
    argsAsVars = map var args

ifLTCon variableName compareTo = ifLT variableName (con compareTo)

ifLT variableName compareTo body =
  If
    nullAnno
    nullSrcSpan
    (lessThan (var variableName) compareTo)
    body
    []
    Nothing

ifLE variableName compareTo body =
  If
    nullAnno
    nullSrcSpan
    (lessThanEq (var variableName) compareTo)
    body
    []
    Nothing

ifGECon variableName compareTo = ifGE variableName (con compareTo)

ifGE variableName compareTo body =
  If
    nullAnno
    nullSrcSpan
    (greaterThanEq (var variableName) compareTo)
    body
    []
    Nothing

call name args = Call nullAnno nullSrcSpan (var name) (argList args)

comment text = TextStmt nullAnno nullSrcSpan ("! F4 Comment: " ++ text)

pragma text = TextStmt nullAnno nullSrcSpan ("!$PRAGMA " ++ text)

declNode = buildAstSeq (DSeq nullAnno) (NullDecl nullAnno nullSrcSpan)

bufferDecl name bounds valueType =
  Decl nullAnno nullSrcSpan [(var name, nullExpr, Nothing)] bufType
  where
    bufType =
      BaseType
        nullAnno
        valueType
        [Dimension nullAnno (map (\(lwb, upb) -> (con lwb, con upb)) bounds)]
        nullExpr
        nullExpr

typedDecl name valueType =
  Decl nullAnno nullSrcSpan [(var name, nullExpr, Nothing)] declType
  where
    declType = BaseType nullAnno valueType [] nullExpr nullExpr

intDecl name =
  Decl
    nullAnno
    nullSrcSpan
    [(var name, nullExpr, Nothing)]
    (BaseType nullAnno (Integer nullAnno) [] nullExpr nullExpr)

intParam name val =
  Decl
    nullAnno
    nullSrcSpan
    [(var name, con val, Nothing)]
    (BaseType nullAnno (Integer nullAnno) [Parameter nullAnno] nullExpr nullExpr)

varC name = var [name]

var name = arrayVar name []

writePipe = call "write_pipe"

readPipe = call "read_pipe"

arrayVar name indices = Var nullAnno nullSrcSpan [(varName name, indices)]

con val = Con nullAnno nullSrcSpan (show val)

nullExpr = NullExpr nullAnno nullSrcSpan
