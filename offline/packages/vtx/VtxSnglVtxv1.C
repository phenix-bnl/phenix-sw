#include "VtxSnglVtxv1.h"

#include <cmath>

ClassImp(VtxSnglVtxv1)

using namespace std;

VtxSnglVtxv1::VtxSnglVtxv1():
  Subsystem(-9999),
  VtxName("DUMMY")
{
  for (short int i=0; i<3;i++)
    {
      vtx[i] = NAN;
      vtxerr[i] = NAN;
    }
}

VtxSnglVtxv1::VtxSnglVtxv1(const VtxSnglVtxv1& source)
{
  VtxName = source.VtxName;
  Subsystem = source.Subsystem;
  for ( int i = 0; i < 3; ++i )
    {
      vtx[i] = source.vtx[i];
      vtxerr[i] = source.vtxerr[i];
    }
}

VtxSnglVtxv1::VtxSnglVtxv1(const char *name, const float *vertex, const float *vertexerr, const short int subsystem)
{
  VtxName = name;
  Subsystem = subsystem;
  for (short i=0;i<3;i++)
    {
      vtx[i] = *vertex++;
      vtxerr[i] = *vertexerr++;
    }
  return;
}
