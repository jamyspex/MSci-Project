module MiniPP (
    miniPPF,
    miniPP,
    miniPPO,
    miniPPD,
    miniPPProgram,
    miniPPProgUnit,
    miniPPP,
    showVarLst,
    showSubName
    )
where

import           Data.Char
import           Data.List
import           Language.Fortran
import           LanguageFortranTools

{-
WV: I know Language.Fortran provides a pretty-printer but getting it to work on the recent version of ghc is too much work so I wrote my own, workmanlike, no fancy features at all.
-}



{- TODO!
data ProgUnit  p = Main      p SrcSpan                      (SubName p)  (Arg p)                      (Block p) [ProgUnit p]
                | Sub        p SrcSpan (Maybe (BaseType p)) (SubName p)  (Arg p)                      (Block p)
                | Function   p SrcSpan (Maybe (BaseType p)) (SubName p)  (Arg p)  (Maybe (VarName p)) (Block p)
                | Module     p SrcSpan                      (SubName p)  (Uses p) (Implicit p) (Decl p) [ProgUnit p]
                | BlockData  p SrcSpan                      (SubName p)  (Uses p) (Implicit p) (Decl p)
                | PSeq       p SrcSpan (ProgUnit p) (ProgUnit p)   -- sequence of programs
                | Prog       p SrcSpan (ProgUnit p)                -- useful for {#p: #q : program ... }
                | NullProg   p SrcSpan                             -- null
                | IncludeProg p SrcSpan (Decl p) (Maybe (Fortran p))
                deriving (Show, Functor, Typeable, Data, Eq)

-- | Fortran subroutine names
data SubName p  = SubName p String
                 | NullSubName p
                 deriving (Show, Functor, Typeable, Data, Eq)

data VarName  p = VarName p Variable
                  deriving (Show, Functor, Typeable, Data, Eq, Read, Ord)

data ArgName  p = ArgName p String
                | ASeq p (ArgName p) (ArgName p)
                | NullArg p
                 deriving (Show, Functor, Typeable, Data, Eq)

-- | The src span denotes the end of the arg list before ')'
data Arg      p = Arg p (ArgName p) SrcSpan
                  deriving (Show, Functor, Typeable, Data, Eq)

data ArgList  p = ArgList p (Expr p)
                  deriving (Show, Functor, Typeable, Data, Eq)

-}

showSubName (SubName _ s)   = s
showSubName (NullSubName _) = ""

showArg (Arg _ argname _) = let
        arg_str = showArgName argname
    in
        if arg_str == ""  then  "" else "("++arg_str++")"

showArgName argName = case argName of
                ArgName _ argname -> argname
                NullArg _         -> ""
                ASeq _ (NullArg _) a2 -> showArgName a2
                ASeq _ a1 (NullArg _) -> showArgName a1
                ASeq _ a1 a2      -> showArgName a1 ++ "," ++ showArgName a2

-- showArgName  (NullArg _) = ""
-- showArgName  (ASeq _ (NullArg _) (NullArg _)) = ""
-- showArgName  (ASeq _ a1 a2) = (showArgName a1)++", "++(showArgName a2)


miniPPP (Main _ _ subname args block ps) = "! Generated code\n"++"program "++(showSubName subname)++" "++(showArg args)++"\n"++(miniPPB block) ++ (unlines (map miniPPP ps))++"\nend program "++(showSubName subname)++"\n"    -- TODO
miniPPP progunit = show progunit
-- Decl     p = Decl           p SrcSpan [(Expr p, Expr p, Maybe Int)] (Type p)      -- declaration stmt
miniPPB (Block _ useblock implicit _ decl fortran) = (miniPPD decl) ++ "\n" ++ (miniPPF fortran) -- TODO

miniPPAttr attr = case attr of
    Parameter _ -> "parameter"
    Dimension _ dim_exp_tups -> "dimension("++ (intercalate "," (map (\(b,e)-> (if (miniPP b == "") then "" else ((miniPP b) ++":"))++(miniPP e)) dim_exp_tups )) ++")" -- [(Expr p, Expr p)]
    Intent _ intent_attr -> "intent(" ++ (case   intent_attr of
            In _    -> "In"
            Out _   -> "Out"
            InOut _ -> "InOut"
        ) ++ ")"
    _ -> showAttr attr

showAttr attr = let
        attr_show = map toLower (show attr)
        attr_str = head $ words attr_show
    in
        attr_str

showType bt e1 e2 = let
        bt_show = map toLower (show bt)
        ty_str = head $ words bt_show
        kind_str = if (miniPP e1) == "" then "" else "("++(miniPP e1)++")"
        size_str = if (miniPP e2) == "" then "" else "*"++(miniPP e2)
    in
        ty_str++kind_str++size_str

miniPPD :: Decl Anno -> String
miniPPD decl  = case decl of
         -- Decl _ _ [(Expr p, Expr p, Maybe Int)] (Type p)
        Decl _ _ ttups ty ->
            let
                ty_str = case ty of
                    BaseType _ bt attrs e1 e2 ->(intercalate ", "  ([showType bt e1 e2]++(map miniPPAttr attrs)))
                    ArrayT   _ expr_tups bt attrs e1 e2  -> (intercalate ", "  ([showType bt e1 e2]++(map miniPPAttr attrs))) ++ "<<"++ (show expr_tups) ++ ">>"
                ttups_str = intercalate "," (
                                map (\(e1,e2,mi) -> (miniPP e1)++( if (miniPP e2)=="" then "" else " = "++(miniPP e2)++" ") ++
                                    (case mi of
                                        Nothing -> ""
                                        Just ii -> " ! " ++(show ii)
                                    ))
                                    ttups
                                        )
            in
              "      "++ty_str ++ " :: " ++ ttups_str -- indent is ad hoc!
        DSeq _ d1 d2 -> (miniPPD d1)++"\n"++(miniPPD d2)
        _ ->  "! UNSUPPORTED in miniPPD! "++(show decl)

miniPPF :: Fortran Anno -> String
miniPPF stmt = miniPPFT stmt "    "

blockToFortran :: Block Anno -> Fortran Anno
blockToFortran (Block _ _ _ _ _ (f)) = f


-- subname (SubName _ name) =

-- getFortranFromProgUnit :: ProgUnit Anno -> [Fortran Anno]
-- getFortranFromProgUnit (Main _ _ subname _ b p) = [blockToFortran b] ++ concatMap getFortranFromProgUnit p
-- getFortranFromProgUnit (Sub _ _ _ _ _ b) = [blockToFortran b]
-- getFortranFromProgUnit (Function _ _ _ _ _ _ b) = [blockToFortran b]
-- getFortranFromProgUnit (Module _ _ _ _ _ _ p) = concatMap getFortranFromProgUnit p
-- getFortranFromProgUnit (BlockData _ _ _ _ _ _) = []
-- getFortranFromProgUnit (PSeq _ _ p1 p2) = getFortranFromProgUnit p1 ++ getFortranFromProgUnit p2
-- getFortranFromProgUnit (Prog _ _ p) = getFortranFromProgUnit p
-- getFortranFromProgUnit (NullProg _ _) = []
-- getFortranFromProgUnit (IncludeProg _ _ _ (Just f)) = [f]
-- getFortranFromProgUnit (IncludeProg _ _ _ (Nothing)) = []

miniPPProgram :: Program Anno -> String
miniPPProgram prog = concatMap miniPPProgUnit prog

miniPPProgUnit :: ProgUnit Anno -> String
miniPPProgUnit prog = case prog of
                    (Main _ _ (SubName _ subname) args b p) ->
                        "program " ++ subname ++ "\n" ++ showArg args ++ printBlock b ++ "\n" ++ concatMap miniPPProgUnit p ++ "\nend program " ++ subname
                    (Sub _ _ _ (SubName _ subname) args b) -> "subroutine " ++ subname ++ showArg args ++ "\n" ++ printBlock b ++ "\nend subroutine " ++ subname ++ "\n"
                    -- (Function _ _ _ _ _ _ b) -> [blockToFortran b]
                    (Module _ _ (SubName _ moduleName) _ _ _ p) ->
                        "module " ++ moduleName ++ "\ncontains\n" ++ concatMap miniPPProgUnit p ++ "\nend module " ++ moduleName
                    _ -> "program anno unsuppported"
                    -- (BlockData _ _ _ _ _ _) -> []
                    -- (PSeq _ _ p1 p2) -> getFortranFromProgUnit p1 ++ getFortranFromProgUnit p2
                    -- (Prog _ _ p) -> getFortranFromProgUnit p
                    -- (NullProg _ _) -> []
                    -- (IncludeProg _ _ _ (Just f)) -> [f]
                    -- (IncludeProg _ _ _ (Nothing)) -> []

printBlock (Block _ _ _ _ decls fortran) =
    miniPPD decls ++ "\n" ++ miniPPF fortran

-- printArgName argName = case argName of
--                     ArgName _ argname -> argname
--                     NullArg _ -> ""
--                     ASeq _ a1 a2 -> printArgName a1 ++ "," ++ printArgName a2


miniPPFT :: Fortran Anno -> String -> String
miniPPFT stmt tab = case stmt of
                 (Assg _ _ expr1 expr2)  -> tab++ (miniPP expr1)++" = "++(miniPP expr2)
                 For  _ _ (VarName _ v) expr1 expr2 expr3 stmt1 -> tab++"do "++v++" = "++ (miniPP expr1)++ ", "++(miniPP expr2)++", "++ (miniPP expr3)++"\n"++(miniPPFT stmt1 (tab++tab))++"\n"++tab++"end do"
                 DoWhile  _ _ expr stmt1-> tab++ "do while ("++(miniPP expr)++ ") "++"\n"++(miniPPFT stmt1 (tab++tab))++"\n"++tab++"end do"
                 FSeq _ _ (NullStmt _ _) stmt2 -> (miniPPFT stmt2 tab)
                 FSeq _ _ stmt1 (NullStmt _ _) -> (miniPPFT stmt1 tab)
                 FSeq _ _ stmt1 stmt2 -> (miniPPFT stmt1 tab)++"\n"++(miniPPFT stmt2 tab)
                 If _ _ expr1 stmt1 exprs_stmts m_stmt -> tab++"if ("++ (miniPP expr1)++") then\n"++(miniPPFT stmt1 (tab++tab))++"\n"++ (unlines (map (\(expr,stmt) -> (  tab++"else if ("++ (miniPP expr)++") then\n"++(miniPPFT stmt (tab++tab)) )) exprs_stmts) ) ++ ""++ (
                    case m_stmt of
                        Just (NullStmt _ _) -> "" -- tab++"else"
                        Just stmt -> tab++"else\n"++(miniPPFT stmt (tab++tab))++"\n"
                        Nothing -> ""
                        )++tab++"end if"
                 Call _ _ expr (ArgList _ es) -> tab++"call "++(miniPP expr)++"("++(miniPP es)++")"
                 Allocate _ _ expr1 expr2 -> tab++"allocate "++ (show (expr1,expr2))
                 Goto _ _ lbl -> tab++"goto "++lbl
                 Label _ _ lbl stmt1 -> lbl++tab++(miniPPFT stmt1 tab)
                 Print _ _ expr exprs -> tab++ "print *, "++(miniPP expr)++(intercalate "," (map miniPP exprs))
                 NullStmt _ _ -> "" -- "! NullStmt"
                 Stop _ _ e1 -> tab++"stop "++(miniPP e1)
                 Continue _ _ -> tab++"continue"
                 OpenCLMap _ _ vrs  vws lvars ilvars stmt1  -> "! OpenCLMap ( "++(showVarLst vrs)++","++(  showVarLst vws)++","++( showLoopVarLst lvars)++","++( showVarLst ilvars)++") {\n"++(miniPPFT stmt1 tab)++"\n"++tab++"}" -- WV20170426
                 OpenCLReduce _ _ vrs vws lvars ilvars rvarexprs stmt1 -> "! OpenCLReduce ( "++(showVarLst vrs)++","++(  showVarLst vws)++","++( showLoopVarLst lvars)++","++( showVarLst ilvars)++","++ (showReductionVarLst rvarexprs)++") {\n"++(miniPPFT stmt1 tab) ++"\n"++tab++"}" -- WV20170426
                 OpenCLStencil _ _ stencils stmt1 -> "! OpenCLStencil (\n" ++ showStencils tab stencils ++ "\n" ++ tab ++ "){\n" ++ miniPPFT stmt1 tab ++ tab ++ "\n}"
                 OpenCLBufferWrite _ _ (VarName _ v) -> tab++"oclWriteBuffer("++v++")" -- FIXME! Should have type info etc oclWrite3DFloatArrayBuffer(p_buf,p_sz,p) This requires a lookup in the context!
                 OpenCLBufferRead _ _ (VarName _ v) -> tab++"oclWriteBuffer("++v++")" -- FIXME! Should have type info etc
                 Return _ _ expr -> tab ++ "return "++(miniPP expr)
                 Open _ _ specs -> tab ++ "open(" ++ miniPPSpecs specs tab ++ ")"
                 Close _ _ specs -> tab ++ "close(" ++ miniPPSpecs specs tab ++ ")"
                 Write _ _ specs exprs -> tab ++ "write(" ++ miniPPSpecs specs tab ++ ")(" ++ (intercalate ", " (map miniPP exprs)) ++ ")"
                 _ -> "! UNSUPPORTED in miniPPF ! "++(show stmt)

showStencils tab [(Stencil _ dimen points coords (VarName _ name))]
    = tab ++ tab ++ (show points) ++ " point stencil on " ++ (show dimen) ++ "D array " ++ name ++ ": " ++ (show coords)
showStencils tab (s:ss) = showStencils tab [s] ++ "\n" ++ showStencils tab ss

miniPPSpecs :: [Spec Anno] -> String -> String
miniPPSpecs specs tab = (intercalate ", " $ map (miniPPSpec tab) specs)

miniPPSpec :: String -> Spec Anno -> String
miniPPSpec tab spec = case spec of
                        Access       _ e     -> miniPP e
                        Action       _ e     -> miniPP e
                        Advance      _ e     -> miniPP e
                        Blank        _ e     -> miniPP e
                        Delim        _ e     -> miniPP e
                        Direct       _ e     -> miniPP e
                        End          _ e     -> miniPP e
                        Err          _ e     -> miniPP e
                        ExFile       _ e     -> miniPP e
                        Exist        _ e     -> miniPP e
                        Eor          _ e     -> miniPP e
                        File         _ e     -> "file=" ++ miniPP e
                        FMT          _ e     -> miniPP e
                        Form         _ e     -> "form=" ++ miniPP e
                        Formatted    _ e     -> "formatted=" ++ miniPP e
                        Unformatted  _ e     -> miniPP e
                        IOLength     _ e     -> miniPP e
                        IOStat       _ e     -> miniPP e
                        Name         _ e     -> miniPP e
                        Named        _ e     -> miniPP e
                        NoSpec       _ e     -> miniPP e
                        Number       _ e     -> miniPP e
                        Floating     _ e1 e2 -> miniPP e1 ++ "\n" ++ miniPP e2
                        NextRec      _ e     -> miniPP e
                        NML          _ e     -> miniPP e
                        Opened       _ e     -> miniPP e
                        Pad          _ e     -> miniPP e
                        Position     _ e     -> miniPP e
                        Read         _ e     -> miniPP e
                        ReadWrite    _ e     -> miniPP e
                        Rec          _ e     -> miniPP e
                        Recl         _ e     -> miniPP e
                        Sequential   _ e     -> miniPP e
                        Size         _ e     -> miniPP e
                        Status       _ e     -> miniPP e
                        StringLit    _ s     -> s
                        Unit         _ e     -> miniPP e
                        WriteSp      _ e     -> miniPP e
                        Delimiter    _       -> ","

showVar (VarName _ v) = v
showVarLst lst = show $ map showVar lst
showLoopVar (VarName _ v, e1, e2,e3 ) = "("++v++","++(miniPP e1)++","++(miniPP e2)++","++(miniPP e3)++")"
-- L for loop
showLoopVarLst lst = show $ map showLoopVar lst
-- R for reduction
showReductionVar (VarName _ v, e1) = "("++v++","++(miniPP e1)++")"
showReductionVarLst lst = show $ map showReductionVar lst


{-
                | OpenCLMap p SrcSpan                   -- Node to represent the data needed for an OpenCL map kernel
                  [VarName p]                           -- List of arguments to kernel that are READ
                  [VarName p]                           -- List of arguments to kernel that are WRITTEN
                  [(VarName p, Expr p, Expr p, Expr p)] -- Loop variables of nested maps
                  (Fortran p)                           -- Body of kernel code
                | OpenCLReduce p SrcSpan
                  [VarName p]                           -- List of arguments to kernel that are READ
                  [VarName p]                           -- List of arguments to kernel that are WRITTEN
                  [(VarName p, Expr p, Expr p, Expr p)] -- Loop variables of nested reductions
                  [(VarName p, Expr p)]                 -- List of variables that are considered 'reduction variables' along with their initial values
                  (Fortran p)                           -- Body of kernel code
                | OpenCLSeq p SrcSpan
                  [VarName p]                           -- List of arguments to kernel that are READ
                  [VarName p]                           -- List of arguments to kernel that are WRITTEN
                  (Fortran p)                           -- Body of kernel code
                | OpenCLBufferRead p SrcSpan
                  (VarName p)                           -- Name of var that buffer is read to
                | OpenCLBufferWrite p SrcSpan
                  (VarName p)                           -- Name of var that buffer is written to
                  deriving (Show, Functor, Typeable, Data, Eq)
-}


miniPP expr = case expr of
    (Con _ _ s) -> s
    (ConL p _ _ s) -> s
    (ConS _ _ s) -> s  -- String representing a constant
    (Var _ _  [((VarName _ v), es)] ) -> v++( if length es == 0 then "" else "("++(intercalate "," (map miniPP es))++")")
    (Bin _ _  op e1 e2) -> (miniPP e1)++(miniPPO op)++(miniPP e2)
    (Unary _ _ minus e) -> "-"++(miniPP e)
    (NullExpr _ _) -> "" -- "! NullExpr"
    (Null _ _) -> "" -- "! Null"
    (ESeq _ _ e1 e2) -> (miniPP e1)++", "++(miniPP e2)
    (Bound _ _ e1 e2) -> (miniPP e1)++" , "++(miniPP e2)
    (ArrayCon _ _ es) -> "("++(intercalate "," (map miniPP es))++")"
    (Sqrt _ _ expr1) -> "sqrt("++(miniPP expr1)++")" -- WV: This is silly, should be handled either for all intrinsics or none
    (AssgExpr _ _ name e1) -> name++" = "++(miniPP e1)
    (ParenthesizedExpr _ _ innerExpr) -> "(" ++ miniPP innerExpr ++ ")"
    otherwise -> "! UNSUPPORTED in miniPP ! "++(show expr)

-- miniPPO binop
miniPPO (Plus _)   = "+"
miniPPO (Minus _)  = "-"
miniPPO (Mul _)    = "*"
miniPPO (Div _)    = "/"
miniPPO (Or _)     = " .or. "
miniPPO (And _)    = " .and. "
miniPPO (Concat _) = "//"
miniPPO (Power _)  = "**"
miniPPO (RelEQ _)  = "=="
miniPPO (RelNE _)  = "/="
miniPPO (RelLT _)  = "<"
miniPPO (RelLE _)  = "<="
miniPPO (RelGT _)  = ">"
miniPPO (RelGE _)  = ">="

