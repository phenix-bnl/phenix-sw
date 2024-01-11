#ifndef __SVXTRACKMAPENTRY_H_
#define __SVXTRACKMAPENTRY_H_

#include <iostream>

class SvxTrackMapEntry
{
 public:
  SvxTrackMapEntry();
  virtual ~SvxTrackMapEntry() {}

  virtual void identify(std::ostream &os=std::cout) const;

  void set_charge(const short int val) 	{charge = val;}
  void set_nhits(const short int val) 	{nhits = val;}
  void set_quality(const float val) 	{quality = val;}
  void set_dca2d(const float val) 	{dca2d = val;}
  void set_dca3d(const float val) 	{dca3d = val;}
  void set_x(const float val) 		{x = val;}
  void set_y(const float val) 		{y = val;}
  void set_z(const float val) 		{z = val;}
  void set_px(const float val) 		{px = val;}
  void set_py(const float val) 		{py = val;}
  void set_pz(const float val) 		{pz = val;}
  void set_livePercentage(const int layer, const float val) { 
         if(0<=layer&&layer<4) { livePercentage[layer] = val;} 
         else {std::cout<<__FUNCTION__<<" out of range, layer:"<<layer<<std::endl;}
       }
  void set_segmentQuality(const float val) { segmentQuality = val;}
  void set_segmentScore(const float val)   { segmentScore   = val;}
  void set_dEdX1(const float val)        {dedx[0] = val;}
  void set_dEdX2(const float val)        {dedx[1] = val;}
  void set_clusterGoodFraction(const int layer, const int hit, const float val) {
         if( (0<=layer&&layer<4) && (0<=hit&&hit<2) ) { clusGoodFrac[layer][hit] = val;} 
         else {std::cout<<__FUNCTION__<<" out of range, layer,hit:"<<layer<<","<<hit<<std::endl;}
       }

  short int get_charge()	const {return charge;}
  short int get_nhits()	const {return nhits;}
  float get_quality() 	const {return quality;}
  float get_dca2d() 	const {return dca2d;}
  float get_dca3d() 	const {return dca3d;}
  float get_x() 	const {return x;}
  float get_y() 	const {return y;}
  float get_z() 	const {return z;}
  float get_px() 	const {return px;}
  float get_py() 	const {return py;}
  float get_pz() 	const {return pz;}
  float get_livePercentage(const int layer) const { 
          if(0<=layer&&layer<4) { return livePercentage[layer];} 
          else {std::cout<<__FUNCTION__<<" out of range, layer:"<<layer<<std::endl; return -9999;}
        }
  float get_segmentQuality() const { return segmentQuality;}
  float get_segmentScore()   const { return segmentScore  ;}
  float get_dEdX1() 	     const { return dedx[0];}
  float get_dEdX2() 	     const { return dedx[1];}
  float get_clusterGoodFraction(const int layer, const int hit) const { 
         if( (0<=layer&&layer<4) && (0<=hit&&hit<2) ) { return clusGoodFrac[layer][hit];} 
         else {std::cout<<__FUNCTION__<<" out of range, layer,hit:"<<layer<<","<<hit<<std::endl; return -9999;}
       }

 protected:
  short int charge;
  short int nhits;
  float quality;
  float dca2d;
  float dca3d;
  float x;
  float y;
  float z;
  float px;
  float py;
  float pz;

  // add Oct 11 2013
  float livePercentage[4];
  float segmentQuality;
  float segmentScore;

  // add Nov 24 2013
  float dedx[2];
  float clusGoodFrac[4][2];
};

#endif 
