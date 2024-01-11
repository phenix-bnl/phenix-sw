#include <cmath>
#include <iostream>

#include "TofHit.h"

using namespace std;

ClassImp(TofHit)

float TofHit::get_eloss() const
{
  virtual_warning("get_eloss()");
  return NAN;
}

float TofHit::get_eloss_err() const
{
  virtual_warning("get_eloss_err()");
  return NAN;
}

float TofHit::get_tof() const
{
  virtual_warning("get_tof()");
  return NAN;
}

float TofHit::get_tof_err() const
{
  virtual_warning("get_tof_err()");
  return NAN;
}

float TofHit::get_xtof(const short ival) const
{
  virtual_warning("get_xtof(const short ival)");
  return NAN;
}

float TofHit::get_xtof_err(const short ival) const
{
  virtual_warning("get_xtof_err(const short ival)");
  return NAN;
}

void TofHit::virtual_warning(const char *funcname) const
{
  cout << "TofHit: " << funcname << " is virtual" << endl;
  return;
}

float TofHit::get_tdiff() const
{
  virtual_warning("get_tdiff()");
  return NAN;
}
