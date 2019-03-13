module FortranDSL where

import           Language.Fortran
import           Utils
import           LanguageFortranTools

for loopVar initial limit = For nullAnno
                                nullSrcSpan
                                (varName loopVar)
                                (con initial)
                                (lessThan (var loopVar) (con limit))
                                (con 1)

plus = Bin nullAnno nullSrcSpan (Plus nullAnno)

lessThan = Bin nullAnno nullSrcSpan (RelLT nullAnno)

assign = Assg nullAnno nullSrcSpan

varName = VarName nullAnno

argList args = ArgList nullAnno $ buildAstSeq (ESeq nullAnno nullSrcSpan)
                                              (NullExpr nullAnno nullSrcSpan)
                                              argsAsVars
  where argsAsVars = map var args

call name args = Call nullAnno nullSrcSpan (var name) (argList args)

comment text = TextStmt nullAnno nullSrcSpan ("! F4 Comment: " ++ text)

pragma text = TextStmt nullAnno nullSrcSpan ("!$PRAGMA " ++ text)

intDecl name = Decl
  nullAnno
  nullSrcSpan
  [(var name, nullExpr, Nothing)]
  (BaseType nullAnno (Integer nullAnno) [] nullExpr nullExpr)

intParam name val = Decl
  nullAnno
  nullSrcSpan
  [(var name, con val, Nothing)]
  (BaseType nullAnno (Integer nullAnno) [Parameter nullAnno] nullExpr nullExpr)

var name = arrayVar name []

arrayVar name indices = Var nullAnno nullSrcSpan [(varName name, indices)]

con val = Con nullAnno nullSrcSpan (show val)

nullExpr = NullExpr nullAnno nullSrcSpan
