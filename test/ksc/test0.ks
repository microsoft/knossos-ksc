(def f ((x : Float))
    ($trace * x x)
)
        
(def test_inline (x : Vec Float)
  (let (n (size x))
    (build 4 (lam (x : Integer) n))))

(def test_inline2 (x : Vec Float)
  (let (n (size x))
    (let (x 4) n)))
    
(def main() 
    (pr (D$f 9.0)
        (f 9.0)
        (fwd$f 9.0 1.0)
        )
    )
