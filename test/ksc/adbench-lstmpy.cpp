
#include "knossos-entry-points-python.h"

#include "adbench-lstm.cpp"

#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

PYBIND11_MODULE(PYTHON_MODULE_NAME, m) {
  using ks::entry_points::python_entry_point;
  m.def("sigmoid", python_entry_point(ks::sigmoid$af));
  m.def("logsumexp", python_entry_point(ks::logsumexp$aT1f));
  m.def("lstm_model", python_entry_point(ks::lstm_model$aT1fT1fT1fT1fT1fT1fT1fT1fT1fT1fT1f));
  m.def("lstm_predict", python_entry_point(ks::lstm_predict$aT1$dT1fT1fT1fT1fT1fT1fT1fT1fT1fT1f$bT1fT1fT1fT1f));
  m.def("lstm_objective", python_entry_point(ks::lstm_objective$aT1$dT1fT1fT1fT1fT1fT1fT1fT1fT1fT1f$bT1fT1fT1fT1$dT1fT1f$b));
}

#include "knossos-entry-points.cpp"
