#include <VtxSnglVtx.h>

#include <cmath>
#include <iostream>

ClassImp(VtxSnglVtx)

using namespace std;

float VtxSnglVtx::Vtx(const short index) const
{
  return NAN;
}

float VtxSnglVtx::VtxErr(const short index) const
{
  return NAN;
}

VtxSnglVtx *VtxSnglVtx::clone() const
{
  cout << "VtxSnglVtx::clone() not implemented by daughter class" << endl;
  return NULL;
}

VtxSnglVtx& VtxSnglVtx::operator=(const VtxSnglVtx& source)
{
  if (this != &source)
    {
      for (short int i=0; i< 3; i++)
	{
	  Vtx(i,source.Vtx(i));
	  VtxErr(i,source.VtxErr(i));
	}
      Name(source.Name());
      SubSystem(source.SubSystem());
    }
  return *this;
}
