/* There's a lot of duplication between this and mnistcnnpy.cpp, but
 * we will follow the Rule of Three
 *
 *    https://en.wikipedia.org/wiki/Rule_of_three_(computer_programming)
 */

#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <pybind11/operators.h>

namespace py = pybind11;

#include "gmm.cpp"

template<typename T>
void declare_vec(py::module &m, std::string typestr) {
  using Class = ks::vec<T>;
  std::string pyclass_name = std::string("vec_") + typestr;
  py::class_<Class>(m, pyclass_name.c_str())
    .def(py::init<>())
    .def(py::init<std::vector<T> const&>())
    .def("is_zero",     &Class::is_zero)
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
  declare_vec<double>(m, std::string("double"));
  declare_vec<std::tuple<ks::vec<double>, ks::vec<double>, ks::vec<double>, ks::vec<double>, ks::vec<double>, ks::vec<double>, ks::vec<double>, ks::vec<double>, ks::vec<double>, ks::vec<double>>>(m, std::string("tuple_vec10"));
  declare_vec<std::tuple<ks::vec<double>, ks::vec<double>>>(m, std::string("tuple_vec2"));
  declare_vec<ks::vec<double> >(m, std::string("vec_double"));
  declare_vec<ks::vec<ks::vec<double> > >(m, std::string("vec_vec_double"));
  declare_vec<ks::vec<ks::vec<ks::vec<double> > > >(m, std::string("vec_vec_vec_double"));
  declare_vec<ks::vec<ks::vec<ks::vec<ks::vec<double> > > > >(m, std::string("vec_vec_vec_vec_double"));
  m.def("gmm_knossos_gmm_objective", &ks::gmm_knossos_gmm_objective);
  m.def("rev_gmm_knossos_gmm_objective", &ks::rev$gmm_knossos_gmm_objective);
}
