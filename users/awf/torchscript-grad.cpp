typedef tensor<2, double> ty$rev$foo$aT2ff;
ty$rev$foo$aT2ff rev$foo$aT2ff(ks::allocator * $alloc, tensor<2, double> _t1arg1, double _t1arg2) {
  /* Let */tensor<2, double> c$56;
  {
  tuple<tensor<2, double>,double> _t1 = std::make_tuple(_t1arg1,_t1arg2);
  /* Let */tensor<2, double> c$55;
  {
  tensor<2, double> c$0 = ks::get<0>(_t1);
  tensor<2, double> _x$o1 = c$0;
  /* Let */tensor<2, double> c$54;
  {
  auto c1 = at::mean($alloc, _x$o1, 0.0);
  auto c2 = at::lt($alloc, c$1, 0.0);
  auto c3 = at::Bool($alloc, c$2);
  auto v7 = c$3;
  /* Let */tensor<2, double> c$53;
  {
  auto c4;
  if (_7) {
    auto c5 = at::mul($alloc, _x$o1, -0.125);
    c$4 = (c$5);
  } else {
    tensor<2, double> c$6 = at::pow($alloc, _x$o1, 2);
    auto c7 = at::mul($alloc, c$6, 0.5);
    c$4 = (c$7);
  }
  auto vt = c$4;
  /* Let */tensor<2, double> c$52;
  {
  auto c8 = at::sin($alloc, _t);
  auto v24 = c$8;
  tensor<2, double> c$9;
  if (_7) {
    auto c10 = at::mul($alloc, _24, _t);
    double c$11 = ks::get<1>(_t1);
    auto c12 = rev$at::mean($alloc, std::make_tuple(c$10,0.0), c$11);
    tensor<2, double> c$13 = ks::get<0>(c$12);
    auto c14 = rev$at::mul($alloc, std::make_tuple(_24,_t), c$13);
    tensor<2, double> c$15 = ks::get<0>(c$14);
    auto c16 = rev$at::sin($alloc, _t, c$15);
    auto c17 = rev$at::mul($alloc, std::make_tuple(_x$o1,-0.125), c$16);
    tensor<2, double> c$18 = ks::get<0>(c$17);
    c$9 = (c$18);
  } else {
    tensor<2, double> c$19 = at::pow($alloc, _x$o1, 2);
    auto c20 = at::mul($alloc, _24, _t);
    double c$21 = ks::get<1>(_t1);
    auto c22 = rev$at::mean($alloc, std::make_tuple(c$20,0.0), c$21);
    tensor<2, double> c$23 = ks::get<0>(c$22);
    auto c24 = rev$at::mul($alloc, std::make_tuple(_24,_t), c$23);
    tensor<2, double> c$25 = ks::get<0>(c$24);
    auto c26 = rev$at::sin($alloc, _t, c$25);
    auto c27 = rev$at::mul($alloc, std::make_tuple(c$19,0.5), c$26);
    tensor<2, double> c$28 = ks::get<0>(c$27);
    auto c29 = rev$at::pow($alloc, std::make_tuple(_x$o1,2), c$28);
    tensor<2, double> c$30 = ks::get<0>(c$29);
    c$9 = (c$30);
  }
  tensor<2, double> c$31;
  if (_7) {
    auto c32 = at::mul($alloc, _24, _t);
    double c$33 = ks::get<1>(_t1);
    auto c34 = rev$at::mean($alloc, std::make_tuple(c$32,0.0), c$33);
    tensor<2, double> c$35 = ks::get<0>(c$34);
    auto c36 = rev$at::mul($alloc, std::make_tuple(_24,_t), c$35);
    tensor<2, double> c$37 = ks::get<1>(c$36);
    auto c38 = rev$at::mul($alloc, std::make_tuple(_x$o1,-0.125), c$37);
    tensor<2, double> c$39 = ks::get<0>(c$38);
    c$31 = (c$39);
  } else {
    tensor<2, double> c$40 = at::pow($alloc, _x$o1, 2);
    auto c41 = at::mul($alloc, _24, _t);
    double c$42 = ks::get<1>(_t1);
    auto c43 = rev$at::mean($alloc, std::make_tuple(c$41,0.0), c$42);
    tensor<2, double> c$44 = ks::get<0>(c$43);
    auto c45 = rev$at::mul($alloc, std::make_tuple(_24,_t), c$44);
    tensor<2, double> c$46 = ks::get<1>(c$45);
    auto c47 = rev$at::mul($alloc, std::make_tuple(c$40,0.5), c$46);
    tensor<2, double> c$48 = ks::get<0>(c$47);
    auto c49 = rev$at::pow($alloc, std::make_tuple(_x$o1,2), c$48);
    tensor<2, double> c$50 = ks::get<0>(c$49);
    c$31 = (c$50);
  }
  tensor<2, double> c$51 = ts_add($alloc, c$9, c$31);
  c$52 = c$51;
  }
  c$53 = c$52;
  }
  c$54 = c$53;
  }
  c$55 = c$54;
  }
  c$56 = c$55;
  }
  return (c$56);
}
