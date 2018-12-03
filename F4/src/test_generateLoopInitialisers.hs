module Main where
import Language.Fortran 
import LanguageFortranTools
import FortranGenerator hiding (generateLoopInitialisers)
import FortranSynthesiser ( produceCode_fortran )

{-
      j_range = ((ny + 1) - 0)
      j_rel = (global_id / k_range)
      j = (j_rel + 0)
      k_range = (nx - 1)
      k_rel = (global_id - (j_rel * k_range))
      k = (k_rel + 1)
-}

generateLoopInitialisers :: [(VarName Anno, Expr Anno, Expr Anno, Expr Anno)] -> Expr Anno -> Maybe(Expr Anno) -> [Fortran Anno]
generateLoopInitialisers ((var, start, end, step):[]) iterator Nothing 
			= 	[
                 Assg nullAnno nullSrcSpan (generateRelVar var) iterator,
				generateLoopStartAddition var start
            ] 
generateLoopInitialisers ((var, start, end, step):[]) iterator (Just offset) 
			= 	[
--                 generateRangeExpr var start end,
--                Assg nullAnno nullSrcSpan one zero,                                                
				Assg nullAnno nullSrcSpan (generateRelVar var) (offset),
				generateLoopStartAddition var start
            ]
-- Here is where we start. 
-- 1. range expr
-- 2. assign rel var to division of iterator by range vars
-- 3. assign loop iterator                          
generateLoopInitialisers xxs@((var, start, end, step):xs) iterator Nothing 
			= 	(map (\(var, start, end, _) -> generateRangeExpr var start end) xxs ) ++
                [
--                 generateRangeExpr var start end,
				Assg nullAnno nullSrcSpan (generateRelVar var) (Bin nullAnno nullSrcSpan (Div nullAnno)  iterator multipliedExprs),
				generateLoopStartAddition var start
            ]
				++
				generateLoopInitialisers xs iterator (Just nextOffset)
    where
	    followingRangeExprs = map (\(v,_,_,_) -> generateRangeVar v) xs -- this is k_range
	    nextOffset = generateSubtractionExpr_list ([iterator] ++ [generateProductExpr_list ([generateRelVar var] ++ followingRangeExprs)])
	    multipliedExprs = generateProductExpr_list followingRangeExprs 
                                                     
generateLoopInitialisers ((var, start, end, step):xs) iterator (Just offset) 
			= 	[
--                 generateRangeExpr var start end,
				Assg nullAnno nullSrcSpan (generateRelVar var) (Bin nullAnno nullSrcSpan (Div nullAnno) offset multipliedExprs),
				generateLoopStartAddition var start
            ]
				++
				generateLoopInitialisers xs iterator (Just nextOffset)
					where
						nextOffset = generateSubtractionExpr_list ([offset] ++ [generateProductExpr_list ([generateRelVar var] ++ followingRangeExprs)])
						followingRangeExprs = map (\(v,_,_,_) -> generateRangeVar v) xs
						multipliedExprs = generateProductExpr_list followingRangeExprs     

--  generateLoopInitialisers loopvars globalIdVar Nothing and loopvars is [(j,0,ny+1,1),(k,0,nx+1)] or  [(j,1,ny,1),(k,1,nx)]
--  generateLoopInitialisers :: [(VarName , Expr , Expr , Expr )] -> Expr  -> (Maybe Expr) -> [Fortran ]
--
globalIdVar = generateVar (VarName nullAnno "global_id")
zero = Con nullAnno nullSrcSpan "0"
one = Con nullAnno nullSrcSpan "1"

n_var = VarName nullAnno "n"
n_from = zero
n_to = one

i_var = VarName nullAnno "i"
i_from = zero
i_to = generateVar (VarName nullAnno "nz")

j_var = VarName nullAnno "j"
j_from = zero
j_to = Bin 
    nullAnno 
    nullSrcSpan  
    (Plus nullAnno) 
    (generateVar (VarName nullAnno "ny"))
    one
    
k_var = VarName nullAnno "k"
k_from = one 
k_to = generateVar (VarName nullAnno "nx")
loopvars = [ (n_var,n_from,n_to,one),(i_var, i_from,i_to,one), (j_var,j_from, j_to, one), (k_var,k_from,k_to,one) ]

loopInitialisers = generateLoopInitialisers loopvars globalIdVar Nothing
loopInitialiserCode = case loopInitialisers of
    [] -> error "synthesiseOpenCLMap: loopInitialiserCode - empty list"
    _ -> foldl1 (\accum item -> appendFortran_recursive item accum) loopInitialisers
tabs = "      "
originalLines = []
programInfo = ([],"dummy")

code = produceCode_fortran programInfo tabs originalLines loopInitialiserCode

main = do
    mapM (putStrLn . show ) loopInitialisers
    putStrLn "\n"
--    putStrLn $ show $ loopInitialiserCode
--    putStrLn "\n"
    putStrLn $ code
    

