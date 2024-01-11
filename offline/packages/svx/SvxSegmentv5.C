// ====================
// FILE: SvxSegmentv5.C
// ====================

#include "SvxSegmentv5.h"

ClassImp(SvxSegmentv5)

using namespace std;

SvxSegmentv5::SvxSegmentv5()
{
  Reset();
}

void SvxSegmentv5::Reset()
{
  id = -9999;
  memset(position, 0, sizeof(position));
  memset(mom3, 0, sizeof(mom3));
  memset(vertex_mom3, 0, sizeof(vertex_mom3));
  memset(nhits, 0, sizeof(nhits));
  for ( int i = 0; i < 3; i++ )
    {
      closest_approach[i] = -9999.;
    }
  for ( int i = 0; i < 4; i++ )
    {
      for ( int j = 0; j < 2; j++ )
        {
          clusterID[i][j] = -1;
        }
    }
  for ( int i = 0; i < 3; i++ )
    {
      scatter[i] = -9999.;
    }
  charge    = false;
  isprimary = false;
  quality   = -9999.;
  chisq     = -9999.;
  ndf       = -9999;
  dchindex  = -9999;
  DCA       = -9999.;
  DCA2D     = -9999.;
  dEdX[0]   = -9999.;
  dEdX[1]   = -9999.;
  recomode  = true;
}

int SvxSegmentv5::isValid() const
{
  return 1;
}

void SvxSegmentv5::identify(ostream& os) const
{
  os << "identify yourself: SvxSegmentv5 Object\n" << std::endl;
  return;
}
