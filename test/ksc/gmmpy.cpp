
#include "knossos-pybind.h"

#include "gmm.cpp"

template<typename T>
void declare_vec(py::module &m, std::string typestr) {
  using Class = ks::vec<T>;
  std::string pyclass_name = std::string("vec_") + typestr;
  py::class_<Class>(m, pyclass_name.c_str())
    .def(py::init<>())
    .def(py::init([](std::vector<T> const& v) { return ks::vec<T>(&ks::entry_points::g_alloc, v); }))
    .def("__getitem__", [](const ks::vec<T> &a, const int &b) {
	return a[b];
      })
    .def("__len__", [](const ks::vec<T> &a) { return a.size(); });
}

// In the future it might make more sense to move the vec type
// definitions to a general Knossos CPP types Python module.
//
// I don't know how to make a single Python type that works for vecs
// of many different sorts of contents.  It seems like it must be
// possible because Python tuples map to std::tuples regardless of
// their contents.  I'll look into it later.  For now I'll just have a
// bunch of verbose replication.
PYBIND11_MODULE(PYTHON_MODULE_NAME, m) {
  using ks::entry_points::with_ks_allocator;
  declare_vec<ks::Float>(m, std::string("Float"));
  declare_vec<ks::Tuple<ks::vec<ks::Float>, ks::vec<ks::Float>, ks::vec<ks::Float>, ks::vec<ks::Float>, ks::vec<ks::Float>, ks::vec<ks::Float>, ks::vec<ks::Float>, ks::vec<ks::Float>, ks::vec<ks::Float>, ks::vec<ks::Float>>>(m, std::string("tuple_vec10"));
  declare_vec<ks::Tuple<ks::vec<ks::Float>, ks::vec<ks::Float>>>(m, std::string("tuple_vec2"));
  declare_vec<ks::vec<ks::Float> >(m, std::string("vec_Float"));
  declare_vec<ks::vec<ks::vec<ks::Float> > >(m, std::string("vec_vec_Float"));
  declare_vec<ks::vec<ks::vec<ks::vec<ks::Float> > > >(m, std::string("vec_vec_vec_Float"));
  declare_vec<ks::vec<ks::vec<ks::vec<ks::vec<ks::Float> > > > >(m, std::string("vec_vec_vec_vec_Float"));
  m.def("gmm_knossos_gmm_objective", with_ks_allocator("gmm_knossos_gmm_objective", &ks::gmm_knossos_gmm_objective$aT1T1fT1fT1T1fT1T1fT1T1f$dfi$b));
  m.def("rev_gmm_knossos_gmm_objective", with_ks_allocator("rev_gmm_knossos_gmm_objective", &ks::rev$gmm_knossos_gmm_objective$aT1T1fT1fT1T1fT1T1fT1T1f$dfi$b));
}

#include "knossos-entry-points.cpp"
