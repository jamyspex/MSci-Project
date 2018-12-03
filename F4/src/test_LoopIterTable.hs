-- module Main where

-- import Language.Fortran.Parser
-- import Language.Fortran
-- import Data.Char
-- import PreProcessor
-- import LanguageFortranTools
-- import VarDependencyAnalysis ( constructLoopIterTable )
-- import TupleTable
-- import qualified Data.Map as DMap


-- main :: IO ()
-- -- main = return ()
-- main = do
--     ast <- parseTest "test.f95"
--     let
--         loops = extractLoops ast
--         tup = constructLoopIterTable  (Just(Empty)) DMap.empty [] (head loops)
--     mapM print loops -- putStrLn $ show $ head loops
--     print ""
--     print "Iterator variables"
--     putStrLn $ show $ map (\(VarName _ v) -> v) ((\(x,y,z) -> y) tup)
--     print "Iterator steps"
--     putStrLn $ show $ ((\(x,y,z) -> z) tup)
--     print "Iterator ranges"
--     putStrLn $ show $ (\(Just x)->x) ( (\(x,y,z) -> x) tup)


-- parseTest s = do f <- readFile s
--                  return $ parse f

-- -- (loopIterTable_maybe, loopVars, loopStepTable) = constructLoopIterTable (Just(Empty)) DMap.empty [] codeSeg
-- --
-- --
-- -- Main
-- -- (fromList [])
-- -- ({<unknown>, line = 1, col = -1},{<unknown>, line = 8, col = 0})
-- -- (SubName (fromList []) "main")
-- -- (Arg (fromList []) (NullArg (fromList [])) ({<unknown>, line = 1, col = 11},{<unknown>, line = 1, col = 11}))
-- -- (Block
--         -- (fromList [])
--         -- (UseBlock (UseNil (fromList [])) {<unknown>, line = 2, col = 3})
--         -- (ImplicitNull (fromList []))
--         -- ({<unknown>, line = 2, col = 3},{<unknown>, line = 8, col = 0})
--         -- (NullDecl (fromList []) ({<unknown>, line = 2, col = 3},{<unknown>, line = 2, col = 3}))
--         -- (For
--             -- (fromList [])
--             -- ({<unknown>, line = 2, col = 3},{<unknown>, line = 6, col = 9})
--             -- (VarName (fromList []) "i")
--             -- (Con (fromList []) ({<unknown>, line = 2, col = 8},{<unknown>, line = 2, col = 9}) "1")
--             -- (Con (fromList []) ({<unknown>, line = 2, col = 10},{<unknown>, line = 2, col = 11}) "5")
--             -- (Con (fromList []) ({<unknown>, line = 2, col = 11},{<unknown>, line = 2, col = 11}) "1")
--             -- (FSeq
--                 -- (fromList [])
--                 -- ({<unknown>, line = 3, col = 6},{<unknown>, line = 6, col = 9})
--                 -- (For (fromList []) ({<unknown>, line = 3, col = 6},{<unknown>, line = 5, col = 12}) (VarName (fromList []) "k") (Con (fromList []) ({<unknown>, line = 3, col = 11},{<unknown>, line = 3, col = 12}) "1") (Con (fromList []) ({<unknown>, line = 3, col = 13},{<unknown>, line = 3, col = 15}) "10") (Con (fromList []) ({<unknown>, line = 3, col = 15},{<unknown>, line = 3, col = 15}) "1") (FSeq (fromList []) ({<unknown>, line = 4, col = 6},{<unknown>, line = 5, col = 12}) (Assg (fromList []) ({<unknown>, line = 4, col = 6},{<unknown>, line = 4, col = 25}) (Var (fromList []) ({<unknown>, line = 4, col = 6},{<unknown>, line = 4, col = 25}) [(VarName (fromList []) "v",[Var (fromList []) ({<unknown>, line = 4, col = 8},{<unknown>, line = 4, col = 9}) [(VarName (fromList []) "i",[])],Con (fromList []) ({<unknown>, line = 4, col = 10},{<unknown>, line = 4, col = 11}) "0",Var (fromList []) ({<unknown>, line = 4, col = 12},{<unknown>, line = 4, col = 13}) [(VarName (fromList []) "k",[])]])]) (Var (fromList []) ({<unknown>, line = 4, col = 17},{<unknown>, line = 4, col = 25}) [(VarName (fromList []) "v",[Var (fromList []) ({<unknown>, line = 4, col = 19},{<unknown>, line = 4, col = 20}) [(VarName (fromList []) "i",[])],Con (fromList []) ({<unknown>, line = 4, col = 21},{<unknown>, line = 4, col = 22}) "1",Var (fromList []) ({<unknown>, line = 4, col = 23},{<unknown>, line = 4, col = 24}) [(VarName (fromList []) "k",[])]])])) (NullStmt (fromList []) ({<unknown>, line = 5, col = 12},{<unknown>, line = 5, col = 12})))) (NullStmt (fromList []) ({<unknown>, line = 6, col = 9},{<unknown>, line = 6, col = 9}))))) []
-- --
-- --
-- --
