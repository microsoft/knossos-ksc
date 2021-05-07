-- | Contains the rules for rewriting 'Lang.SUFFwdPass' and
-- 'Lang.SUFRevPass' derived functions of 'Lang.PrimFun's to
-- expressions that don't contain any derived functions.  This saves
-- us from having to implement the derived functions in the backend.

{-# LANGUAGE DataKinds #-}

module Ksc.SUF.Rewrite (rewriteSUFFwdPass, rewriteSUFRevPass) where

import Lang
import Prim

rewriteSUFFwdPass :: PrimFun -> TExpr -> Maybe TExpr
rewriteSUFFwdPass (P_SelFun i n) arg
  = Just $ Tuple [ pSel i n arg, mkTangentZero arg ]

-- FIXME augment the dups
rewriteSUFFwdPass (P_dup 2) arg
  = Just $ Tuple [ Tuple [ arg, arg ], Tuple [] ]

rewriteSUFFwdPass P_elim arg
  = Just $ Tuple [ Tuple [], mkTangentZero arg ]

rewriteSUFFwdPass P_index i_v
  -- FIXME: Avoid duplicating i_v
  = Just $ Tuple [pIndex i v, Tuple [i, shape]]
  where shape = mkTangentZero v
        i = pFst i_v
        v = pSnd i_v

rewriteSUFFwdPass P_size v
  = Just $ Tuple [pSize v, shape]
  where shape = mkTangentZero v

rewriteSUFFwdPass P_sum v
  = Just $ Tuple [pSum v, shape]
  where shape = pSize v

rewriteSUFFwdPass P_eq t
  = Just $ Tuple [mkPrimCall1 P_eq t, mkTangentZero t]

rewriteSUFFwdPass P_ne t
  = Just $ Tuple [mkPrimCall1 P_ne t, mkTangentZero t]

rewriteSUFFwdPass P_constVec t
  = Just $ Tuple [mkPrimCall1 P_constVec t, Tuple []]

rewriteSUFFwdPass P_deltaVec t
  = Just $ Tuple [mkPrimCall1 P_deltaVec t, snd3 t]
  where snd3 = pSel 2 3

rewriteSUFFwdPass P_ts_add t
  = Just $ Tuple [mkPrimCall1 P_ts_add t, Tuple []]

rewriteSUFFwdPass P_ts_dot t
  = Just $ Tuple [mkPrimCall1 P_ts_dot t, t]

rewriteSUFFwdPass P_ts_scale t
  = Just $ Tuple [mkPrimCall1 P_ts_scale t, t]

rewriteSUFFwdPass _ _ = Nothing


rewriteSUFRevPass :: PrimFun -> ExprX 'Typed -> Maybe TExpr
rewriteSUFRevPass (P_SelFun i n) (Tuple [ dt, bog ])
  = Just $ Tuple (map project_or_dt [1..n])
  where project_or_dt j = if i == j then dt else pSel j n bog

rewriteSUFRevPass (P_dup 2) (Tuple [arg, _])
  = Just $ pAdd1 arg

rewriteSUFRevPass P_elim (Tuple [_, arg])
  = Just arg

-- FIXME: avoid duplicating i_shape
rewriteSUFRevPass P_index (Tuple [d_da, i_shape])
  = Just $ Tuple [
           unitOfIndexType,
           pBuild (pSize shape)
                  (Lam ii (If (pEqual (Var ii) i)
                            d_da
                            (pIndex (Var ii) shape)))
           ]
  where ii = TVar (typeof i) $ Simple "primDindex$i"
        i = pFst i_shape
        shape = pSnd i_shape
        unitOfIndexType = case typeof i of
          TypeTuple ts -> Tuple (map (const (Tuple [])) ts)
          _ -> Tuple []

rewriteSUFRevPass P_size unit_shape
  = Just shape
  where shape = pSnd unit_shape

rewriteSUFRevPass P_sum d_da_size
  = Just $ pConstVec size d_da
  where d_da = pFst d_da_size
        size = pSnd d_da_size

rewriteSUFRevPass P_eq (Tuple [_, tangentZero])
  = Just tangentZero

rewriteSUFRevPass P_ne (Tuple [_, tangentZero])
  = Just tangentZero

rewriteSUFRevPass P_constVec t
  | TypeTuple [TypeTensor n _, _] <- typeof t
  , let unitOfIndexType = mkTangentZero (zeroIndexForDimension n)
  = Just $ Tuple [unitOfIndexType, pSum (pFst t)]

rewriteSUFRevPass P_deltaVec (Tuple [v, i])
  | TypeTensor n _ <- typeof v
  , let unitOfIndexType = mkTangentZero (zeroIndexForDimension n)
  = Just $ Tuple [unitOfIndexType, unitOfIndexType, pIndex i v]

rewriteSUFRevPass P_ts_add (Tuple [ddr, Tuple []])
  -- FIXME: This is bad because it duplicates the expression ddr.  CSE
  -- will probably resolve this problem, but we shouldn't create it in
  -- the first place.
  -- TODO: Can we just "let tmp = ddr in Tuple [tmp, tmp]"?  No need to get a fresh name.
  = Just $ Tuple [ddr, ddr]

rewriteSUFRevPass P_ts_dot (Tuple [ddr, dadb])
  = Just $ Tuple [pScale ddr db, pScale ddr da]
  where da = pFst dadb
        db = pSnd dadb

rewriteSUFRevPass P_ts_scale (Tuple [ddr, t])
  = Just $ Tuple [pDot x ddr, pScale lambda ddr]
  where lambda = pFst t
        x = pSnd t

rewriteSUFRevPass _ _ = Nothing
