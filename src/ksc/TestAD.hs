import Opt
import AD
import Lang
import Annotate
import Parse

-- For when TODO Lam is done
--toLam (Def f vs body) = Lam vs body
-- cmpDef a b = cmpExpr (toLam a) (toLam b)

main:: IO ()
main = do
    let d = runParserOrPanic pDef "(def f ((x : Float)) (* x 2.0))"
    putStrLn $ pps d
    let (env,ad) = annotDef d
    putStrLn $ pps ad
    let dd = applyD $ gradDef emptyST ad
    putStrLn $ pps dd
    let oo = optDef env dd
    putStrLn $ pps $ stripAnnot oo
    let expected = runParserOrPanic pDef "(def f' ((x : Float) (dx : Float)) (* 2.0 dx))"
    putStrLn $ pps expected
    -- putStrLn $ show $ cmpDef (stripAnnot oo) expected
