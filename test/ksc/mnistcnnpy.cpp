
#include "knossos-entry-points-python.h"

#include "mnistcnn.cpp"

#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

// In the future it might make more sense to move the vec type
// definitions to a general Knossos CPP types Python module.
PYBIND11_MODULE(PYTHON_MODULE_NAME, m) {
  using ks::entry_points::python_entry_point;
  m.def("conv2d", python_entry_point(ks::conv2d$aT1T1T1T1fT1fT1T1T1f));
  m.def("mnist", python_entry_point(ks::mnist$aT1T1T1fT1T1T1T1fT1fT1T1T1T1fT1fT1T1T1T1fT1fT1T1fT1f));
}

#include "knossos-entry-points.cpp"

