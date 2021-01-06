#include "knossos.h"
#include "knossos-prelude.h"
#include "prelude-aten.cpp"

/*
Gradient code from python input:

    def foo(x : torch.Tensor):
        y = torch.mean(x)
        if y < 0:
            t = -0.125*x
        else:
            t = 1/2 * x ** 2
        return torch.mean(torch.sin(t)*t)

*/

double foo$aT2f(ks::allocator *$alloc, tensor<2, double> _x$o1)
{
  ks::alloc_mark_t c12 = $alloc->mark();
  /* Let */ ty$at::mean$aT2ff c11;
  {
    auto c1 = at::mean($alloc, _x$o1, 0.0);
    auto c2 = at::lt($alloc, c1, 0.0);
    auto c3 = at::Bool($alloc, c2);
    auto c0;
    if (c3)
    {
      auto c4 = at::mul($alloc, _x$o1, -0.125);
      c0 = (c4);
    }
    else
    {
      tensor<2, double> c5 = at::pow($alloc, _x$o1, 2);
      auto c6 = at::mul($alloc, c5, 0.5);
      c0 = (c6);
    }
    auto _t = c0;
    ks::alloc_mark_t c10 = $alloc->mark();
    auto c7 = at::sin($alloc, _t);
    auto c8 = at::mul($alloc, c7, _t);
    auto c9 = at::mean($alloc, c8, 0.0);
    $alloc->reset(c10);
    c11 = c9;
  }
  $alloc->reset(c12);
  return (c11);
}

typedef tensor<2, double> 
rev$foo_s_aT2ff(ks::allocator *$alloc, tensor<2, double> _t1arg1, double _t1arg2)
{
  tensor<2, double> c56;
  {
    tuple<tensor<2, double>, double> _t1 = std::make_tuple(_t1arg1, _t1arg2);
    tensor<2, double> c55;
    {
      tensor<2, double> c0 = ks::get<0>(_t1);
      tensor<2, double> _x_s_o1 = c0;
      tensor<2, double> c54;
      {
        auto c1 = at::mean(_s_alloc, _x_s_o1, 0.0);
        auto c2 = at::lt(_s_alloc, c1, 0.0);
        auto c3 = at::Bool(_s_alloc, c2);
        auto v7 = c3;
        tensor<2, double> c53;
        {
          auto c4;
          if (_7)
          {
            auto c5 = at::mul(_s_alloc, _x_s_o1, -0.125);
            c4 = (c5);
          }
          else
          {
            tensor<2, double> c6 = at::pow(_s_alloc, _x_s_o1, 2);
            auto c7 = at::mul(_s_alloc, c6, 0.5);
            c4 = (c7);
          }
          auto vt = c4;
          tensor<2, double> c52;
          {
            auto c8 = at::sin(_s_alloc, _t);
            auto v24 = c8;
            tensor<2, double> c9;
            if (_7)
            {
              auto c10 = at::mul(_s_alloc, _24, _t);
              double c11 = ks::get<1>(_t1);
              auto c12 = at::rev$mean(_s_alloc, std::make_tuple(c10, 0.0), c11);
              tensor<2, double> c13 = ks::get<0>(c12);
              auto c14 = at::rev$mul(_s_alloc, std::make_tuple(_24, _t), c13);
              tensor<2, double> c15 = ks::get<0>(c14);
              auto c16 = at::rev$sin(_s_alloc, _t, c15);
              auto c17 = at::rev$mul(_s_alloc, std::make_tuple(_x_s_o1, -0.125), c16);
              tensor<2, double> c18 = ks::get<0>(c17);
              c9 = (c18);
            }
            else
            {
              tensor<2, double> c19 = at::pow(_s_alloc, _x_s_o1, 2);
              auto c20 = at::mul(_s_alloc, _24, _t);
              double c21 = ks::get<1>(_t1);
              auto c22 = at::rev$mean(_s_alloc, std::make_tuple(c20, 0.0), c21);
              tensor<2, double> c23 = ks::get<0>(c22);
              auto c24 = at::rev$mul(_s_alloc, std::make_tuple(_24, _t), c23);
              tensor<2, double> c25 = ks::get<0>(c24);
              auto c26 = at::rev$sin(_s_alloc, _t, c25);
              auto c27 = at::rev$mul(_s_alloc, std::make_tuple(c19, 0.5), c26);
              tensor<2, double> c28 = ks::get<0>(c27);
              auto c29 = at::rev$pow(_s_alloc, std::make_tuple(_x_s_o1, 2), c28);
              tensor<2, double> c30 = ks::get<0>(c29);
              c9 = (c30);
            }
            tensor<2, double> c31;
            if (_7)
            {
              auto c32 = at::mul(_s_alloc, _24, _t);
              double c33 = ks::get<1>(_t1);
              auto c34 = at::rev$mean(_s_alloc, std::make_tuple(c32, 0.0), c33);
              tensor<2, double> c35 = ks::get<0>(c34);
              auto c36 = at::rev$mul(_s_alloc, std::make_tuple(_24, _t), c35);
              tensor<2, double> c37 = ks::get<1>(c36);
              auto c38 = at::rev$mul(_s_alloc, std::make_tuple(_x_s_o1, -0.125), c37);
              tensor<2, double> c39 = ks::get<0>(c38);
              c31 = (c39);
            }
            else
            {
              tensor<2, double> c40 = at::pow(_s_alloc, _x_s_o1, 2);
              auto c41 = at::mul(_s_alloc, _24, _t);
              double c42 = ks::get<1>(_t1);
              auto c43 = at::rev$mean(_s_alloc, std::make_tuple(c41, 0.0), c42);
              tensor<2, double> c44 = ks::get<0>(c43);
              auto c45 = at::rev$mul(_s_alloc, std::make_tuple(_24, _t), c44);
              tensor<2, double> c46 = ks::get<1>(c45);
              auto c47 = at::rev$mul(_s_alloc, std::make_tuple(c40, 0.5), c46);
              tensor<2, double> c48 = ks::get<0>(c47);
              auto c49 = at::rev$pow(_s_alloc, std::make_tuple(_x_s_o1, 2), c48);
              tensor<2, double> c50 = ks::get<0>(c49);
              c31 = (c50);
            }
            tensor<2, double> c51 = ts_add(_s_alloc, c9, c31);
            c52 = c51;
          }
          c53 = c52;
        }
        c54 = c53;
      }
      c55 = c54;
    }
    c56 = c55;
  }
  return (c56);
}
