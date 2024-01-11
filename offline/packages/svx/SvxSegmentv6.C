// ====================
// FILE: SvxSegmentv6.C
// ====================

#include "SvxSegmentv6.h"

ClassImp(SvxSegmentv6)

using namespace std;

SvxSegmentv6::SvxSegmentv6()
{
  Reset();
}

void SvxSegmentv6::Reset()
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
  segScore = -9999.;
  segQuality = -9999.;
  for ( int i = 0; i < 4; i++ )
    {
      livePercentage[i] = -9999.;
    }
  for ( int i = 0; i < 4; i++ )
    for (int j = 0; j < 2; j++)
      {
	clusGoodFrac[i][j] = -9999.;
      }

}

int SvxSegmentv6::isValid() const
{
  return 1;
}

void SvxSegmentv6::identify(ostream& os) const
{
  os << "identify yourself: SvxSegmentv6 Object\n" << std::endl;
  return;
}

bool SvxSegmentv6::isValidLayer(const int layer) const
{
  if (layer >= 0 && layer < 4)
    return true;
  else
    return false;
}

bool SvxSegmentv6::isValidCoor(const int coor) const
{
  if (coor >= 0 && coor < 3)
    return true;
  else
    return false;
}

void SvxSegmentv6::setClusterID (const int layer, const int hit, const int idx) 
{
  if (isValidLayer(layer) && hit < 2 && hit >= 0)
    clusterID[layer][hit] = idx;
  else
    std::cout << PHWHERE << "Arguments outside of array bounds, clusterID will not be set!" 
	      << " layer=" << layer << " hit=" << hit << " idx=" << idx << std::endl;
}
  
int SvxSegmentv6::getClusterID(const int layer, const int hit) const
{
  if (isValidLayer(layer) && hit < 2 && hit >= 0)
    return clusterID[layer][hit];
  else
    {
      std::cout << PHWHERE << "Arguments outside of array bounds!"
		<< " layer=" << layer << " hit=" << hit << std::endl;
      return -9999;
    }
}

void SvxSegmentv6::setProjectedPosition(const int layer, const float x, const float y, const float z, const short hit) 
{
  if (isValidLayer(layer) && hit < 2 && hit >= 0)
    {
      position[layer][hit][0] = x;
      position[layer][hit][1] = y;
      position[layer][hit][2] = z;
    }
  else
    std::cout << PHWHERE << "Arguments outside of array bounds, projectedPosition will not be set!" 
	      << " layer=" << layer << " hit=" << hit << " x=" << x << " y=" << y << " z=" << z << std::endl;
}

float SvxSegmentv6::getProjectedPosition(const int layer, const int coor, const short hit) const
{ 
  if (isValidLayer(layer) && isValidCoor(coor) && hit < 2 && hit >= 0)
    return position[layer][hit][coor]; 
  else
    {
      std::cout << PHWHERE << "Arguments outside of array bounds"
		<< " layer=" << layer << " hit=" << hit << " coor=" << coor << std::endl;
      return -9999.;
    }
}

void SvxSegmentv6::setClosestApproach(const float cax, const float cay, const float caz) 
{
  closest_approach[0] = cax;
  closest_approach[1] = cay;
  closest_approach[2] = caz;
}

float SvxSegmentv6::getClosestApproach(const int coor) const
{ 
  if (isValidCoor(coor))
    return closest_approach[coor]; 
  else
    {
      std::cout << PHWHERE << "Arguments outside of array bounds!" 
		<< " coor=" << coor << std::endl;
      return -9999.;
    }
}

void SvxSegmentv6::set3Momentum(const float px, const float py, const float pz) 
{
  mom3[0] = px;
  mom3[1] = py;
  mom3[2] = pz;
}

float SvxSegmentv6::get3Momentum(const int coor) const
{ 
  if (isValidCoor(coor))
    return mom3[coor]; 
  else
    {
      std::cout << PHWHERE << "Arguments outside of array bounds!" 
		<< " coor=" << coor << std::endl;
      return -9999.;
    }
}

void SvxSegmentv6::set3MomentumAtPrimaryVertex(const float px0, const float py0, const float pz0) 
{
  vertex_mom3[0] = px0;
  vertex_mom3[1] = py0;
  vertex_mom3[2] = pz0;
}

float SvxSegmentv6::get3MomentumAtPrimaryVertex(const int coor) const
{ 
  if (isValidCoor(coor))
    return vertex_mom3[coor]; 
  else
    {
      std::cout << PHWHERE << "Arguments outside of array bounds!" 
		<< " coor=" << coor << std::endl;
      return -9999.;
    }
}

void SvxSegmentv6::setInnerMostProjectedPosition(const int coor, const float xyz) 
{ 
  if (isValidCoor(coor))
    position[0][0][coor] = xyz;
  else
    {
      std::cout << PHWHERE << "Arguments outside of array bounds. InnerMostProjectedPosition will not be set!" 
		<< " coor=" << coor << " xyz=" << xyz << std::endl;
    }
}

float SvxSegmentv6::getInnerMostProjectedPosition(const int coor) const
{ 
  if (isValidCoor(coor))
    return position[0][0][coor]; 
  else
    {
      std::cout << PHWHERE << "Arguments outside of array bounds!" 
		<< " coor=" << coor << std::endl;
      return -9999.;
    }
}

float SvxSegmentv6::getScatter(const int layer) const
{ 
  if (isValidLayer(layer))
    return scatter[layer]; 
  else
    {
      std::cout << PHWHERE << "Arguments outside of array bounds!" 
		<< " layer=" << layer << std::endl;
      return -9999.;
    }
}

void SvxSegmentv6::setNhits(const int layer, const short n) 
{ 
  if (isValidLayer(layer))
    nhits[layer] = n; 
  else
    {
      std::cout << PHWHERE << "Arguments outside of array bounds. Nhits will not be set!" 
		<< " layer=" << layer << " n=" << n << std::endl;
    }
}

short SvxSegmentv6::getNhits(const int layer) const
{ 
  if (isValidLayer(layer))
    return nhits[layer]; 
  else
    {
      std::cout << PHWHERE << "Arguments outside of array bounds!" 
		<< " layer=" << layer << std::endl;
      return -1;
    }
}

void SvxSegmentv6::setLivePercentage(const int layer, const float perc) 
{ 
  if (isValidLayer(layer))
    livePercentage[layer] = perc; 
  else
    {
      std::cout << PHWHERE << "Arguments outside of array bounds. livePercentage will not be set!" 
		<< " layer=" << layer << " perc=" << perc << std::endl;
    }
}

float SvxSegmentv6::getLivePercentage(const int layer) const
{ 
  if (isValidLayer(layer))
    return livePercentage[layer];
  else
    {
      std::cout << PHWHERE << "Arguments outside of array bounds!" 
		<< " layer=" << layer << std::endl;
      return -9999.;
    }
}

void SvxSegmentv6::setClusterGoodFraction(const int layer, const int hit, const float frac)
{
 if (isValidLayer(layer) && hit < 2)
   clusGoodFrac[layer][hit] = frac;
  else
    {
      std::cout << PHWHERE << "Arguments outside of array bounds. clusterGoodFraction will not be set!" 
		<< " layer=" << layer << " hit=" << hit << " frac=" << frac << std::endl;
    }
}

float SvxSegmentv6::getClusterGoodFraction(const int layer, const int hit) const
{
  if (isValidLayer(layer) && hit < 2)
    return clusGoodFrac[layer][hit];
  else
    {
      std::cout << PHWHERE << "Arguments outside of array bounds!" 
		<< " layer=" << layer << " hit=" << hit << std::endl;
      return -9999.;
    }
}
