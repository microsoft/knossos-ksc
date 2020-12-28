#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <pybind11/operators.h>

namespace py = pybind11;

#include "mnistcnn.cpp"

ks::allocator g_alloc{ 1'000'000'000 };

template<typename T>
void declare_vec(py::module &m, std::string typestr) {
  using Class = ks::vec<T>;
  std::string pyclass_name = std::string("vec_") + typestr;
  py::class_<Class>(m, pyclass_name.c_str())
    .def(py::init<>())
    .def(py::init([](std::vector<T> const& v) { return ks::vec<T>(&g_alloc, v); }))
    .def("__getitem__", [](const ks::vec<T> &a, const int &b) {
	return a[b];
      })
    .def("__len__", [](const ks::vec<T> &a) { return a.size(); });
}

template<typename RetType, typename... ParamTypes>
auto withGlobalAllocator(RetType(*f)(ks::allocator*, ParamTypes...)) {
  return [f](ParamTypes... params) { return f(&g_alloc, params...); };
}

// In the future it might make more sense to move the vec type
// definitions to a general Knossos CPP types Python module.
PYBIND11_MODULE(PYTHON_MODULE_NAME, m) {
  declare_vec<double>(m, std::string("double"));
  declare_vec<ks::vec<double> >(m, std::string("vec_double"));
  declare_vec<ks::vec<ks::vec<double> > >(m, std::string("vec_vec_double"));
  declare_vec<ks::vec<ks::vec<ks::vec<double> > > >(m, std::string("vec_vec_vec_double"));
  declare_vec<ks::vec<ks::vec<ks::vec<ks::vec<double> > > > >(m, std::string("vec_vec_vec_vec_double"));
  m.def("conv2d", withGlobalAllocator(&ks::conv2d$aT1T1T1T1fT1fT1T1T1f));
  m.def("mnist", withGlobalAllocator(&ks::mnist$aT1T1T1fT1T1T1T1fT1fT1T1T1T1fT1fT1T1T1T1fT1fT1T1fT1f));
}
