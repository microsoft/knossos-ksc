module Main where

import GHC.Stack;
import Data.Hashable

import Lang
import Parse (runParser, pDecls, parseF )
import Rules
import Annotate
import AD
import Opt
import CSE

import ANF
import Cgen (cppF, runM, cgenDef, cgenDefs)
import KMonad
import Data.List( partition )


-------------------------------------
--  The demo driver
-------------------------------------

demoF :: String -> IO ()
-- Read source code from specified input file, optimise,
-- differentiate, optimise, and display results of each step
demoF file
  = do { cts <- readFile file
       ; case runParser pDecls cts of
            Left err   -> putStrLn ("Failed parse: " ++ show err)
            Right defs -> runKM (demoN defs) }

demo :: Decl -> IO ()
demo d = runKM (demoN [d])

demoN :: [Decl] -> KM ()
demoN decls
  = do { banner "Original declarations"
       ; displayN decls

       ; banner "Typechecked declarations"
       ; let (env, tc_decls) = annotDecls emptyGblST decls
       ; let (rules, defs)   = partitionDecls tc_decls
             rulebase        = mkRuleBase rules

       ; displayN $! tc_decls

       ; banner "Optimised original definition"
       ; let (env1, opt_defs) = optDefs rulebase env defs
       ; displayN opt_defs

       ; banner "Anf-ised original definition"
       ; anf_defs <- anfDefs opt_defs
       ; displayN anf_defs

       ; banner "The full Jacobian (unoptimised)"
       ; let grad_defs = gradDefs anf_defs
             env2      = env1 `extendGblST` grad_defs
       ; displayN grad_defs

       ; banner "The full Jacobian (optimised)"
       ; let (env3, opt_grad_defs) = optDefs rulebase env2 grad_defs
       ; displayN opt_grad_defs

       ; banner "Forward derivative (unoptimised)"
       ; let der_fwd = map applyD opt_grad_defs
       ; displayN der_fwd

       ; banner "Forward-mode derivative (optimised)"
       ; let (env4, opt_der_fwd) = optDefs rulebase env3 der_fwd
       ; displayN opt_der_fwd

       ; banner "Forward-mode derivative (CSE'd)"
       ; (env5, cse_fwd) <- cseDefs rulebase env4 opt_der_fwd
       ; displayN cse_fwd

       ; banner "Transposed Jacobian"
       ; let trans_grad_defs = map transposeD opt_grad_defs
       ; displayN trans_grad_defs

       ; banner "Optimised transposed Jacobian"
       ; let (env6, opt_trans_grad_defs) = optDefs rulebase env5 trans_grad_defs
       ; displayN opt_trans_grad_defs

       ; banner "Reverse-mode derivative (unoptimised)"
       ; let der_rev = map applyD opt_trans_grad_defs
       ; displayN der_rev

       ; banner "Reverse-mode derivative (optimised)"
       ; let (env7, opt_der_rev) = optDefs rulebase env6 der_rev
       ; displayN opt_der_rev

       ; (env8, cse_rev) <- cseDefs rulebase env7 opt_der_rev
       ; banner "Reverse-mode derivative (CSE'd)"
       ; displayN cse_rev
       }

-------------------------------------
-- GMM derivatives
-------------------------------------

moveMain :: [Decl]
         -> ( [Decl]    -- Singleton 'main' decl, or empty
            , [Decl])   -- All the rest
moveMain = partition isMain
  where
    isMain (DefDecl (DefX (Fun (UserFun "main")) _ _)) = True
    isMain _ = False

doall :: HasCallStack => Int -> String -> IO ()
doall verbosity file =
  let dd defs = liftIO $ putStrLn ("...\n" ++ (pps $ take verbosity $! defs))
      ddx :: Pretty p => [p] -> KM ()
      ddx = displayN in
  runKM $
  do { decls0 <- liftIO (parseF (file ++ ".ks"))
  ;  liftIO $ putStrLn "read decls"

  ;  let (main, decls)    = moveMain decls0

  ;  banner "annotated defs"
  ;  let (env, ann_decls) = annotDecls emptyGblST decls
  ;  dd ann_decls

  ;  let (rules, defs) = partitionDecls ann_decls
         rulebase      = mkRuleBase rules

  ;  banner "main"
  ;  dd main

  ;  banner "defs"
  ;  dd defs

  ;  let grad = gradDefs defs
  ;  banner "grad"
  ;  dd grad

  ;  let (env1, optgrad) = optDefs rulebase env grad
  ;  banner "optgrad"
  ;  dd optgrad

  ;  let fwd  = applyDefs optgrad
  ;  banner "fwd"
  ;  dd fwd

  ;  let (env2, optfwd) = optDefs rulebase env1 fwd
  ;  banner "optfwd"
  ;  dd optfwd

  ;  let annot_main = map (\ (DefDecl x) -> x) $ snd $ annotDecls env2 main

  ;  let alldefs = defs ++ optgrad ++ optfwd ++ annot_main
  ;  (env3, cse) <- cseDefs rulebase env2 alldefs
  ;  dd cse

  ;  let ann2 =  cse
  ;  banner "all"
  ;  dd ann2

  ;  liftIO (cppF ("obj\\" ++ file) ann2)
  }

gmm :: IO ()
gmm = doall 400 "test\\ksc\\gmm"

main :: IO ()
main = gmm
