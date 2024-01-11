#include "BbcReturncodes.h"
#include "BbcOut.h"

#include <cmath>
#include <iostream>

ClassImp(BbcOut)

void BbcOut::identify(std::ostream& os) const
{
  os << "virtual BbcOut object";
  return ;
}

void BbcOut::Reset()
{
  std::cout << "ERROR BbcOut: Reset() not implemented by daughter class" << std::endl;
  return ;
}

int BbcOut::isValid() const
{
  virtual_warning("isValid()");
  return 0;
}

float BbcOut::get_VertexPoint() const
{
  virtual_warning("get_VertexPoint()");
  return NAN;
}

float BbcOut::get_dVertexPoint() const
{
  virtual_warning("get_dVertexPoint()");
  return NAN;
}

float BbcOut::get_TimeZero() const
{
  virtual_warning("get_TimeZero()");
  return NAN;
}

//__________________________________________
float BbcOut::get_dTimeZero() const
{
  virtual_warning("get_dTimeZero()");
  return NAN;
}

//__________________________________________
void BbcOut::set_TimeZero(const float, const float)
{
  virtual_warning("set_TimeZero(const float t0, const float t0err)");
  return ;
}

//__________________________________________
void BbcOut::set_Vertex( const float, const float )
{
  virtual_warning("set_Vertex(const float vtx, const float vtxerr)");
  return ;
}

//__________________________________________
void BbcOut::set_dZVertex(const float vtxerr)
{
  virtual_warning("set_dZVertex(const float vtxerr)");
  return ;
}

//________________________________________________________________
void BbcOut::AddBbcNS(const short npmt, const float energy,
                      const float timing, const short nBbc)
{
  virtual_warning("AddBbcNS(const short npmt, const float energy, const float timing, const short nBbc)");
  return ;
}

void BbcOut::AddBbcNS(const short npmt, const float energy, const short nBbc)
{
  virtual_warning("AddBbcNS(const short npmt, const float energy, const short nBbc)");
  return ;
}

short BbcOut::get_nPmt(const short nBbc) const
{
  virtual_warning("get_nPmt(const short nBbc)");
  return BBC_INVALID_SHORT;
}

float BbcOut::get_ChargeSum(const short nBbc) const
{
  virtual_warning("get_ChargeSum(const short nBbc)");
  return NAN;
}

float BbcOut::get_Timing(const short nBbc) const
{
  virtual_warning("get_Timing(const short nBbc)");
  return NAN;
}

void BbcOut::virtual_warning(const char *funcsname) const
{
  std::cout << "BbcOut::" << funcsname << " is virtual, doing nothing" << std::endl;
  return ;
}

void BbcOut::FillFromClass(const BbcOut& old)
{
  for(int iarm = 0; iarm < 2; iarm++)
  {
    AddBbcNS(
             old.get_nPmt(iarm),
             old.get_ChargeSum(iarm),
             old.get_Timing(iarm),
             iarm
             );
  }
  set_TimeVertex(
                 old.get_TimeZero(),
                 old.get_dTimeZero(),
                 old.get_VertexPoint(),
                 old.get_dVertexPoint()
                 );

}
