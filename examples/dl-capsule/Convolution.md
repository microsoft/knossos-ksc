
Convolutions
------------

A very natural thing to want to support is 2D convolution, and then the natural 
generalization to capsules

Here's what I think is a natural expression of convolution, 
with for loops for the outer accumulation and comprehensions for the inner one.
```python
@knossos.register(generate_lm=True)
def conv2(a : torch.Tensor, b : torch.Tensor) -> torch.Tensor:
  ma,na = a.size()
  mb,nb = b.size()
  out = torch.empty(ma-mb+1,na-nb+1)
  for ia in range(ma-mb+1):
    for ja in range(na-nb+1):
      out[ia, ja] = sum(
          a[ia+ib,ja+jb] * b[ib,jb]
          for ib in range(ma)
          for jb in range(na)
        )
  return out 
```

That's not supported in PyTorch at the time of writing, but this is close enough:
```python
@knossos.register(generate_lm=True)
def conv2(a : torch.Tensor, b : torch.Tensor) -> torch.Tensor:
  ma,na = a.size()
  mb,nb = b.size()
  out = torch.empty(ma-mb+1,na-nb+1)
  for ia in range(ma-mb+1):
    for ja in range(na-nb+1):
      out[ia, ja] = sum(
          a[ia+ib,ja+jb] * b[ib,jb]
          for ib in range(ma)
          for jb in range(na)
        )
  return out 
```

