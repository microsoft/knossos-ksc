(def f Float ((x : Float))
    ($trace * x x)
)

(def test_inline (Vec Integer) (x : Vec Float)
  (let (n (size x))
    (build 4 (lam (i : Integer) n))))

(def test_inline2 Integer (x : Vec Float)
  (let (n (size x))
    (let (x 4) n)))

(def main Integer ()
    (pr -- See https://github.com/awf/knossos/issues/281 (D$f 9.0)
        (f 9.0)
        (fwd$f 9.0 1.0)
        )
    )
