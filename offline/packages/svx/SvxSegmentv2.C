// ====================
// FILE: SvxSegmentv2.C
// ====================

#include "SvxSegmentv2.h"

ClassImp(SvxSegmentv2)

  using namespace std;

SvxSegmentv2::SvxSegmentv2()
{
  Reset();
}

void SvxSegmentv2::Reset()
{
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
  dchindex  = -9999;
  DCA       = -9999.;
  DCA2D     = -9999.;
  dEdX[0]   = -9999.;
  dEdX[1]   = -9999.;
}

int SvxSegmentv2::isValid() const
{
  return 1;
}

void SvxSegmentv2::identify(ostream& os) const
{
  os << "identify yourself: SvxSegmentv2 Object\n" << std::endl;
  return;
}
