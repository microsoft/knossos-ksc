(def f ((x : Float))
    (* x x)
)

(def main() 
    (pr (f 9.0)
        (D$f 9.0)
        (fwd$f 9.0 1.0))
    )
