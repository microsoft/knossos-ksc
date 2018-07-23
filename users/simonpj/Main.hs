module Main where

import Lang
import Prim
import AD
import Opt

ex1 :: Def
-- f x = let y = x*x in x + y
ex1 = Def (Fun "f") [sx] $
      Let sy (pMul (Var sx) (Var sx)) $
      pAdd (Var sx) (Var sy)

-- f' x dx = dx + 2*x*dx
-- f` x dr =

ex2 :: Def
-- g x = let y = x*x in
--       let z = x + y
--       in y*z
ex2 = Def (Fun "g") [sx] $
      Let sy (pMul (Var sx) (Var sx)) $
      Let sz (pAdd (Var sx) (Var sy))  $
      pMul (Var sy) (Var sz)

ex3 :: Def
-- h (x,y) = let z = x + y
--           in y*z
ex3 = Def (Fun "g") [sp] $
      Let sx (pFst (Var sp))    $
      Let sy (pSnd (Var sp))    $
      Let sz (pAdd (Var sx) (Var sy))  $
      pMul (Var sy) (Var sz)


sx, sy, sz :: Var
sx = Simple "x"
sy = Simple "y"
sz = Simple "z"
sp = Simple "p"


demo :: Def -> IO ()
demo def
  = do { banner "Original definition"
       ; display def

       ; banner "The full Jacobian"
       ; let grad_def = gradD def
       ; display grad_def

       ; banner "Forward derivative (unoptimised)"
       ; let der_fwd = applyD grad_def
       ; display der_fwd

       ; banner "Forward-mode derivative (optimised)"
       ; let opt_der_fwd = optD der_fwd
       ; display opt_der_fwd

       ; banner "Transposed Jacobian"
       ; let trans_grad_def = transD grad_def
       ; display trans_grad_def

       ; banner "Optimised transposed Jacobian"
       ; let opt_trans_grad_def = optD trans_grad_def
       ; display opt_trans_grad_def

       ; banner "Reverse-mode derivative (unoptimised)"
       ; let der_rev = applyD opt_trans_grad_def
       ; display der_rev

       ; banner "Reverse-mode derivative (optimised)"
       ; let opt_der_rev = optD der_rev
       ; display opt_der_rev
       }


banner :: String -> IO ()
banner s
  = do { putStrLn "\n----------------------------"
       ; putStrLn s
       ; putStrLn "----------------------------\n" }


main :: IO ()
main = return ()  -- To keep GHC quiet

{-
------ Driver ---------

process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ex -> case eval ex of
      Nothing -> putStrLn "Cannot evaluate"
      Just result -> putStrLn $ ppexpr result

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Arith> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop
-}