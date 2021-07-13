
#include "knossos-pybind.h"

#include "mnistcnn.cpp"

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
PYBIND11_MODULE(PYTHON_MODULE_NAME, m) {
  using ks::entry_points::with_ks_allocator;
  declare_vec<ks::Float>(m, std::string("Float"));
  declare_vec<ks::vec<ks::Float> >(m, std::string("vec_Float"));
  declare_vec<ks::vec<ks::vec<ks::Float> > >(m, std::string("vec_vec_Float"));
  declare_vec<ks::vec<ks::vec<ks::vec<ks::Float> > > >(m, std::string("vec_vec_vec_Float"));
  declare_vec<ks::vec<ks::vec<ks::vec<ks::vec<ks::Float> > > > >(m, std::string("vec_vec_vec_vec_Float"));
  m.def("conv2d", with_ks_allocator("conv2d", &ks::conv2d$aT1T1T1T1fT1fT1T1T1f));
  m.def("mnist", with_ks_allocator("mnist", &ks::mnist$aT1T1T1fT1T1T1T1fT1fT1T1T1T1fT1fT1T1T1T1fT1fT1T1fT1f));
}

#include "knossos-entry-points.cpp"

