module FortranDSL where

import           Language.Fortran
import           LanguageFortranTools
import           Utils

for loopVar initial lhs =
  For
    nullAnno
    nullSrcSpan
    (varName loopVar)
    (con initial)
    (lessThan (var loopVar) lhs)
    (con 1)

plus = Bin nullAnno nullSrcSpan (Plus nullAnno)

minus = Bin nullAnno nullSrcSpan (Minus nullAnno)

lessThan = Bin nullAnno nullSrcSpan (RelLT nullAnno)

greaterThanEq = Bin nullAnno nullSrcSpan (RelGE nullAnno)

assign = Assg nullAnno nullSrcSpan

varName = VarName nullAnno

block = buildAstSeq (FSeq nullAnno nullSrcSpan) (NullStmt nullAnno nullSrcSpan)

sub name decls body =
  Sub
    nullAnno
    nullSrcSpan
    Nothing
    (SubName nullAnno name)
    (Arg nullAnno (NullArg nullAnno) nullSrcSpan)
    block
  where
    block =
      Block
        nullAnno
        (UseBlock (UseNil nullAnno) NoSrcLoc)
        (ImplicitNull nullAnno)
        nullSrcSpan
        decls
        body

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

var name = arrayVar name []

arrayVar name indices = Var nullAnno nullSrcSpan [(varName name, indices)]

con val = Con nullAnno nullSrcSpan (show val)

nullExpr = NullExpr nullAnno nullSrcSpan