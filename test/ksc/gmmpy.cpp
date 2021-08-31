
#include "knossos-entry-points-python.h"

#include "gmm.cpp"

#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

PYBIND11_MODULE(PYTHON_MODULE_NAME, m) {
  using ks::entry_points::python_entry_point;
  m.def("gmm_knossos_gmm_objective", python_entry_point(ks::gmm_knossos_gmm_objective$aT1T1fT1fT1T1fT1T1fT1T1f$dfi$b));
  m.def("rev_gmm_knossos_gmm_objective", python_entry_point(ks::rev$gmm_knossos_gmm_objective$aT1T1fT1fT1T1fT1T1fT1T1f$dfi$b));
}

#include "knossos-entry-points.cpp"
