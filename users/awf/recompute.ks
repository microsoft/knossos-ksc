(def h0 T (x : S)
   (let (a1  (f1 x))
    (let (a2  (f2 a1)) ; a2 is large
     (let (a3  (f3 x a2))
      (let (a4  (f4 a3))
       (let (o3  (g3 a4 a3))
        (let (o2  (g2 o3 a2)) ; a2 used again
         (let (o1  (g1 o2)) 
          (g0 o1)  ; return value
         ) ; o1 gets freed here
        ) ; free o2
       )
      )     
     )
    ) ; free a2
   )
)

(def h0 T (x : S)
   (let (a1  (f1 x))
     (let (a3 (let (a2  (f2 a1)) 
                 (f3 x a2))) ; free a2
      (let (a4  (f4 a3))
       (let (o3  (g3 a4 a3))
        (let (o2 (let (a2'  (f2 a1)) ; f2(a1) recomputed  
                     (g2 o3 a2'))) ; a2' freed
         (let (o1  (g1 o2)) 
          (g0 o1)  ; return value
         ) ; o1 gets freed here
        ) ; free o2
       )
      )     
     )
    ) ; free a2
   )
)

