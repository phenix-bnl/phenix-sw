#include <cmath>
#include <iostream>
#include <MpcOut.h>

ClassImp(MpcOut)

const int MPC_INVALID_SHORT = -999;

MpcOut::MpcOut()
{
}

short MpcOut::get_ntow(const short nMpc) const
{
  virtual_warning("get_ntow(const short nMpc)");
  return MPC_INVALID_SHORT;
}

Float_t MpcOut::get_esum(const short nMpc) const
{
  virtual_warning("get_esum(const short nMpc)");
  return NAN;
}

Float_t MpcOut::get_time(const short nMpc) const
{
  virtual_warning("get_time(const short nMpc)");
  return NAN;
}

Float_t MpcOut::get_z() const
{
  virtual_warning("get_z()");
  return NAN;
}

Float_t MpcOut::get_t0() const
{
  virtual_warning("get_t0()");
  return NAN;
}

Float_t MpcOut::get_dz() const
{
  virtual_warning("get_dz()");
  return NAN;
}

Float_t MpcOut::get_dt0() const
{
  virtual_warning("get_dt0()");
  return NAN;
}

void MpcOut::set_ntow(const Short_t s_esum, const Short_t n_esum)
{
  virtual_warning("set_ntow(const Short_t s_esum, const Short_t n_esum)");
}

void MpcOut::set_esum(const Float_t s_esum, const Float_t n_esum)
{
  virtual_warning("set_esum(const Float_t s_esum, const Float_t n_esum)");
}

void MpcOut::set_time(const Float_t s_time, const Float_t n_time)
{
  virtual_warning("set_time(const Float_t s_time, const Float_t n_time)");
}

void MpcOut::set_vtxt0(const Float_t vtx, const Float_t t0,
                       const Float_t vtxerr, const Float_t t0err)
{
  virtual_warning("set_vtxt0(const Float_t vtx, const Float_t t0, const Float_t vtxerr, const Float_t t0err)");
  return ;
}

void MpcOut::virtual_warning(const char *funcsname) const
{
  std::cout << "MpcOut::" << funcsname << " is virtual, doing nothing" << std::endl;
  return ;
}

void MpcOut::identify(std::ostream& os) const
{
  os << "virtual MpcOut object";
  return ;
}

void MpcOut::Reset()
{
  std::cout << "ERROR MpcOut: Reset() not implemented by daughter class" << std::endl;
  return ;
}

int MpcOut::isValid() const
{
  virtual_warning("isValid()");
  return 0;
}

