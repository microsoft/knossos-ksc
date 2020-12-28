import numpy as np
import os
from ksc.utils import translate_and_import

def test_conv1d():
    # we need parentheses around Vec after ':' to parse correctly
    ks_str = """
(edef add Integer (Integer Integer))
(edef sub Integer (Integer Integer))
(edef mul Integer (Integer Integer))
(edef div Integer (Integer Integer))
(edef lt Bool (Integer Integer))
(edef gte Bool (Integer Integer))
(edef or Bool (Bool Bool))

(def conv1d (Vec (Vec Float)) ((kernels : (Vec (Vec (Vec Float))))
                               (image : (Vec (Vec Float))))
  (let (k (size kernels))
  (let (kernels_elt (index 0 kernels))
  (let (kn (size (index 0 kernels_elt)))
  (let (l  (size image))
  (let (n  (size (index 0 image)))
    (build k (lam (ki : Integer)
      (build n (lam (ni : Integer)
        (sumbuild kn (lam (kni : Integer)
          (sumbuild l  (lam (li  : Integer)
            (let (knc (div kn 2))
            (let (noi (sub (add ni knc) kni))
            (let (outside_image (or (lt noi 0) (gte noi n)))
            (let (image_noi (if outside_image 0.0 (index noi (index li image))))
              (mul image_noi (index kni (index li (index ki kernels)))))
            )))))))))))))))))
        """
    py_out = translate_and_import(__file__, ks_str, "common")
    image = [
      np.random.normal(0, 1, (100,))
    ]
    kernel = [
         [[-0.5, 0, 0.5]],
         [[0.333, 0.333, 0.333]]
         ]

    expected_output = np.vstack((
      np.convolve(image[0], kernel[0][0], 'same'),
      np.convolve(image[0], kernel[1][0], 'same')))
    output = py_out.conv1d(kernel, image)
    assert np.allclose(expected_output, output)

def test_conv1d_tensors():
    # we need parentheses around Vec after ':' to parse correctly
    ks_str = """
(edef add Integer (Integer Integer))
(edef sub Integer (Integer Integer))
(edef mul Integer (Integer Integer))
(edef div Integer (Integer Integer))
(edef lt Bool (Integer Integer))
(edef gte Bool (Integer Integer))
(edef or Bool (Bool Bool))

(def conv1d (Tensor 2 Float) ((kernels : (Vec (Tensor 2 Float)))
                               (image : (Tensor 2 Float)))
  (let (k (size kernels))
  (let (kernels_elt (index 0 kernels))
  (let ((kl kn) (size kernels_elt))
  (let ((l n) (size image))
    (build (tuple k n) (lam (t_ : Tuple Integer Integer)
        (let ((ki ni) t_)
        (sumbuild kn (lam (kni : Integer)
          (sumbuild l (lam (li  : Integer)
            (let (knc (div kn 2))
            (let (noi (sub (add ni knc) kni))
            (let (outside_image (or (lt noi 0) (gte noi n)))
            (let (image_noi (if outside_image 0.0 (index noi (index li image))))
              (mul image_noi (index kni (index li (index ki kernels)))))
            )))))))))))))))
        """
    py_out = translate_and_import(__file__, ks_str, "common")
    image = np.random.normal(0, 1, (1, 11))
    kernel = [
      np.array([[-0.5, 0, 0.5]]),
      np.array([[0.333, 0.333, 0.333]])
    ]
    assert kernel[0].shape == (1, 3)
    expected_output = np.vstack((
      np.convolve(image[0], kernel[0][0], 'same'),
      np.convolve(image[0], kernel[1][0], 'same')))
    output = py_out.conv1d(kernel, image)
    assert np.allclose(expected_output, output)
