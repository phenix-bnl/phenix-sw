// ====================
// FILE: SvxSegmentv1.C
// ====================

#include "SvxSegmentv1.h"

#include <cmath>

ClassImp(SvxSegmentv1)

  using namespace std;

SvxSegmentv1::SvxSegmentv1()
{
  Reset();
}


void SvxSegmentv1::identify(ostream& os) const
{
  os << "identify yourself: SvxSegmentv1 Object\n" << std::endl;
  return;
}

void SvxSegmentv1::Reset()
{

  for ( int i = 0; i < 4; i++ )
    {
      for ( int j = 0; j < 2; j++ )
        {
          clusterID[i][j] = -1;
        }
    }
  memset(position, 0, sizeof(position));
  memset(nhits, 0, sizeof(nhits));
  memset(mom3, 0, sizeof(mom3));
  memset(vertex_mom3, 0, sizeof(vertex_mom3));
  charge    = false;
  isprimary = false;
  quality   = -9999.;
  dchindex  = -9999;
  dEdX[0]   = -9999.;
  dEdX[1]   = -9999.;
  DCA       = -9999.;
  DCA2D     = -9999.;
  for ( int i = 0; i < 3; i++ )
    {
      scatter[i] = -9999.;
    }

}

int SvxSegmentv1::isValid() const
{
  return 1;
}


float SvxSegmentv1::getMomentum() const
{
  return sqrt(mom3[0]*mom3[0] + mom3[1]*mom3[1] + mom3[2]*mom3[2]);
}

void SvxSegmentv1::set3Momentum(const float px, const float py, const float pz)
{
  mom3[0] = px;
  mom3[1] = py;
  mom3[2] = pz;
}

void SvxSegmentv1::set3MomentumAtPrimaryVertex(const float px, const float py, const float pz)
{
  vertex_mom3[0] = px;
  vertex_mom3[1] = py;
  vertex_mom3[2] = pz;
}

void SvxSegmentv1::setProjectedPosition(const int layer, const float x, const float y, const float z, const short hit)
{
  position[layer][hit][0] = x;
  position[layer][hit][1] = y;
  position[layer][hit][2] = z;
}


float SvxSegmentv1::getInnerMostProjectedPosition(const int coor) const
{
  int layer = -1;
  for (int i = 0; i < 4; i++)
    {
      if (nhits[i] > 0)
        {
          layer = i;
          break;
        }
    }
  if (layer == -1)
    {
      return -9999.;
    }
  return position[layer][0][coor];
}

void SvxSegmentv1::setInnerMostProjectedPosition(const int coor, const float xyz)
{
  int layer = 0;
  position[layer][0][coor] = xyz;
}



