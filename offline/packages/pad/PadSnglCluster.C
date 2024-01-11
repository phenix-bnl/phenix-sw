#include <cmath>
#include <iostream>

#include "PadSnglCluster.h"

ClassImp(PadSnglCluster)

float PadSnglCluster::get_dxyz(const int i) const
{
  virtual_warning("get_dxyz(const int i)");
  return NAN;
}

float PadSnglCluster::get_xyz(const int i) const
{
  virtual_warning("get_xyz(const int i)");
  return NAN;
}

void PadSnglCluster::virtual_warning(const char *funcname) const
{
  std::cout << "PadSnglCluster: using virtual function " << funcname << std::endl;
  return;
}

