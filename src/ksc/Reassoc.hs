-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
module Reassoc where

{-
   let x1 = e1
   ...
   in let xn = en
   in let y = e2
   in body[y,x3, x7]

===>

   let t = let xi=ei
           in (e2, x3, x7)
   in let y = sel_1_3 t
          x3 = sel_2_3 t
          x7 = sel_2_3 t
   in body

-}

push binds (Let x ex body) = push ((x,ex) : binds) body
push 