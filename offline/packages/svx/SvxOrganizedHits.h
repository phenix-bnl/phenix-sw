#ifndef SVX_ORGANIZED_HITS
#define SVX_ORGANIZED_HITS

#include "TMath.h"

#include <vector>
#include <iostream>


class SvxOrganizedHits
{
  public:
    SvxOrganizedHits() : nphibins(64), nthetabins(64) {}
    ~SvxOrganizedHits(){}
    void setHitPosition(int phibin, int thetabin, int layer, int hit, int coor, float val);
    
    int getNHits(int phibin, int thetabin, int layer) const;
    
    int phiBin(float x, float y);
    
    int thetaBin(float x, float y, float z) const;
    
    int zBin(float z) const;
    
    void addHit(int layer, float x, float y, float z, bool thta, float vx, float vy, float vz, int ladder=0, int clusterid=0)
;    
    void setUsed(int phibin, int thetabin, int layer, int hit, short isused);
    
    short getUsed(int phibin, int thetabin, int layer, int hit);
    
    int getClusterID(int phibin, int thetabin, int layer, int hit);
    
    int getLadder(int phibin, int thetabin, int layer, int hit);
    
    void reset();
    
    float getHitPosition(int phibin, int thetabin, int layer, int hit, int coor) const;
    
    float getChannelPosition(int layer, int ladder, int sensor, int section, int channel, int coor);
    
    int getNPhiBins() const;
    int getNThetaBins() const;
    
  private:
    int nphibins, nthetabins;
    float phi;
    std::vector<float> tempvec;
    
    std::vector<std::vector<float> > hits[64][64][4];
    std::vector<int> cluster_id[64][64][4];
    std::vector<int> ladders[64][64][4];
    std::vector<short> used[64][64][4];
};




inline int SvxOrganizedHits::getNHits(int phibin, int thetabin, int layer) const
{
  return hits[phibin][thetabin][layer-1].size();
}

inline int SvxOrganizedHits::phiBin(float x, float y)
{
  float twopi = 6.28318530717958623;
  phi = atan2(y, x);
  if(phi<0.)
  {
    phi+=(twopi);
  }
  int bin = (int)(floor(nphibins*phi/(twopi)));
  if(bin<0){bin=0;}
  else if(bin>=nphibins){bin=(nphibins-1);}
  return bin;
}


inline int SvxOrganizedHits::thetaBin(float x, float y, float z) const
{
  float pi_o_2 = 1.57079632679489656;
  float pi = 3.14159265358979312;
  float theta=atan2(z, sqrt(x*x + y*y));
  int bin = (int)(floor(nthetabins*(theta + pi_o_2)/(pi)));
  if(bin<0){bin=0;}
  else if(bin>=nthetabins){bin=(nthetabins-1);}
  return bin;
}


inline int SvxOrganizedHits::zBin(float z) const
{
  int bin = (int)(floor(nthetabins*(z + 15.)/(30.)));
  if(bin<0){bin=0;}
  else if(bin>=nthetabins){bin=(nthetabins-1);}
  return bin;
}


inline void SvxOrganizedHits::addHit(int layer, float x, float y, float z, bool thta, float vx, float vy, float vz, int ladder, int clusterid)
{
  tempvec.clear();
  int thetabin=0;
  if(thta==true){thetabin=thetaBin(x, y, (z-vz));}
  else{thetabin = zBin(z);}
  int phibin=phiBin(x-vx, y-vy);
  hits[phibin][thetabin][layer-1].push_back(tempvec);
  hits[phibin][thetabin][layer-1].back().push_back(x);
  hits[phibin][thetabin][layer-1].back().push_back(y);
  hits[phibin][thetabin][layer-1].back().push_back(z);
  hits[phibin][thetabin][layer-1].back().push_back(phi);
  
  cluster_id[phibin][thetabin][layer-1].push_back(clusterid);
  
  ladders[phibin][thetabin][layer-1].push_back(ladder);
  used[phibin][thetabin][layer-1].push_back(0);
}

inline void SvxOrganizedHits::setUsed(int phibin, int thetabin, int layer, int hit, short isused)
{
  used[phibin][thetabin][layer-1][hit-1]=isused;
}

inline short SvxOrganizedHits::getUsed(int phibin, int thetabin, int layer, int hit)
{
  return used[phibin][thetabin][layer-1][hit-1];
}

inline void SvxOrganizedHits::reset()
{
  for(int i=0;i<64;i++)
  {
    for(int j=0;j<64;j++)
    {
      for(int k=0;k<4;k++)
      {
        hits[i][j][k].clear();
        cluster_id[i][j][k].clear();
        ladders[i][j][k].clear();
        used[i][j][k].clear();
      }
    }
  }
}

inline float SvxOrganizedHits::getHitPosition(int phibin, int thetabin, int layer, int hit, int coor) const
{
  return hits[phibin][thetabin][layer-1][hit-1][coor];
}


inline void SvxOrganizedHits::setHitPosition(int phibin, int thetabin, int layer, int hit, int coor, float val)
{ 
  hits[phibin][thetabin][layer-1][hit-1][coor]=val;
}

inline int SvxOrganizedHits::getNPhiBins() const
{
  return nphibins;
}


inline int SvxOrganizedHits::getNThetaBins() const
{
  return nthetabins;
}

inline int SvxOrganizedHits::getClusterID(int phibin, int thetabin, int layer, int hit)
{
  return cluster_id[phibin][thetabin][layer-1][hit-1];
}

inline int SvxOrganizedHits::getLadder(int phibin, int thetabin, int layer, int hit)
{
  return ladders[phibin][thetabin][layer-1][hit-1];
}








#endif
