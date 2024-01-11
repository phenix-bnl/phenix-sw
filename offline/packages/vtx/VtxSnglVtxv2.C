#include "VtxSnglVtxv2.h"

#include <cmath>
#include <string>

ClassImp(VtxSnglVtxv2)

using namespace std;

VtxSnglVtxv2::VtxSnglVtxv2():
  Subsystem(-9999),
  VtxName("DUMMY")
{
  for (short int i=0; i<3;i++)
    {
      vtx[i] = NAN;
      vtxerr[i] = NAN;
    }
}

VtxSnglVtxv2::VtxSnglVtxv2(const VtxSnglVtxv2& source):
  Subsystem(source.Subsystem),
  VtxName(source.VtxName)
{
  for ( int i = 0; i < 3; ++i )
    {
      vtx[i] = source.vtx[i];
      vtxerr[i] = source.vtxerr[i];
    }
}

VtxSnglVtxv2::VtxSnglVtxv2(const char *name, const float *vertex, const float *vertexerr, const short int subsystem):
  Subsystem(subsystem),
  VtxName(name)
{
  for (short i=0;i<3;i++)
    {
      vtx[i] = *vertex++;
      vtxerr[i] = *vertexerr++;
    }
  return;
}

void VtxSnglVtxv2::identify(ostream &out) const
{
  out << "VtxSnglVtxv2 Object: " << VtxName << endl;
  return;
}
