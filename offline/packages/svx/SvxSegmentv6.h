// ====================
// FILE: SvxSegmentv6.h
// ====================
//
// v6 implemented by D. McGlinchey 10/7/2013
//    added information from SvxComponentGeom:
//    1) segScore: (0-100) based on # of 
//       clusters and dead areas
//    2) segQuality: (0-) based on
//       linkScore and chi2/ndf
//    3) livePercentage[4]: the fraction of 
//       good pixels in tiles associated
//       with the segment at each layer
//    Also added checks on array range validity
//    for functions which access arrays.
//
#ifndef __SVXSEGMENTV6_H_
#define __SVXSEGMENTV6_H_

#include "SvxSegment.h"
#include <cmath>

class SvxSegmentv6 : public SvxSegment
{

 public:

  /**
   * V6 implementation of the SVX(VTX) standalone track.
   * In this class, the "layer" argument starts at 0
   * in each layer there can be up to 2 hits.
   * getNhits(int layer) const  tells how many hits are on the layer
   * the "hit" argument should be either 0 or 1, for the first
   * or second hits, if they exist
   **/
  SvxSegmentv6();
  virtual ~SvxSegmentv6() {};

  // The "standard PHObject response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;
  

  ///
  /// Set and get member variables
  ///

  /// index of the segment itself
  int getSegmentID() const  { return id; }
  void setSegmentID(const int ID) { id = ID; }


  /// index of cluster in the SvxClusterList
  void setClusterID (const int layer, const int hit, const int idx); 
  int getClusterID(const int layer, const int hit=0) const; 


  /// expected hit position calculated by track fitting.
  /// coor 0,1,2 == x,y,z
  void setProjectedPosition(const int layer, const float x, const float y, const float z, const short hit=0); 
  float getProjectedPosition(const int layer, const int coor, const short hit=0) const; 


  /// closest approach 
  /// coor 0,1,2 == x,y,z
  void setClosestApproach(const float cax, const float cay, const float caz);
  float getClosestApproach(const int coor) const; 


  /// whether the track could be from the primary vertex
  void setPrimary   (const bool prim) { isprimary = prim; }
  bool getPrimary() const { return isprimary; }


  /// momentum vector at inner most hit
  /// coor 0,1,2 == x,y,z
  void set3Momentum(const float px, const float py, const float pz); 
  float get3Momentum(const int coor) const; 


  /// magnitude of 3-momentum
  float getMomentum() const 
  { return sqrt(mom3[0]*mom3[0] + mom3[1]*mom3[1] + mom3[2]*mom3[2]); }


  /// momentum vector at the closest approach to the primary vertex.
  /// coor 0,1,2 == x,y,z
  void set3MomentumAtPrimaryVertex(const float px0, const float py0, const float pz0);
  float get3MomentumAtPrimaryVertex(const int coor) const; 
  

  /// expected hit position of the inner most hit calculated by the track fitting.
  void setInnerMostProjectedPosition(const int coor, const float xyz);
  float getInnerMostProjectedPosition(const int coor) const; 


  /// scattering angle at a given layer as calculated from the track model
  void setScatter(const int layer, const float sct) { scatter[layer] = sct; }
  float getScatter(const int layer) const; 
  

  /// charge of the track, true=positive
  void setIsPositive(const bool hel) { charge = hel; }
  bool IsPositive() const { return charge; }
  

  /// # hits in a given layer
  void setNhits(const int layer, const short n);
  short getNhits(const int layer) const;
  

  /// track fit quality.
  /// should be a negative value.
  /// the closer to 0 it is, the better the fit
  void setQuality   (const float q) { quality = q; }
  float getQuality() const { return quality; }


  /// segment chi2
  float getChiSq() const  { return chisq; }
  void setChiSq(const float CHISQ) { chisq = CHISQ; }


  /// number of degrees of freedom
  void setNDF(const int NDF) { ndf = NDF; }
  int getNDF() const  { return ndf; }


  /// index of the associated central track in the PHCentralTrack object
  /// (or DCHTrack object), if it exists.
  /// if it does not exit, return -9999.
  void setDchIndex(const int idx) { dchindex = idx; }
  int getDchIndex() const { return dchindex; }
  

  ///distance of closest approach to the primary vertex, from the track model
  void setDCA(const float dca) { DCA = dca; }
  float getDCA() const { return DCA; }
  

  ///distance of the closest approach to the primary vertex in the XY-plane.
  void setDCA2D     (const float dca) { DCA2D = dca; }
  float getDCA2D() const { return DCA2D; }
  

  /// energy loss in strip layers
  void set_dEdX1 (const float val) { dEdX[0] = val; }
  void set_dEdX2 (const float val) { dEdX[1] = val; }
  float get_dEdX1() const { return dEdX[0]; }
  float get_dEdX2() const { return dEdX[1]; }


  /// reconstruction mode
  void set_recomode (const bool mode) { recomode = mode; }
  bool get_recomode() const { return recomode; }


  /// segment score (added in v6)
  void setSegmentScore  (const float score) { segScore = score; }
  float getSegmentScore  () const { return segScore; }


  /// segment quality (added in v6)
  void setSegmentQuality(const float quality) { segQuality = quality; }
  float getSegmentQuality() const { return segQuality; }


  /// live fraction (added in v6)
  void setLivePercentage(const int layer, const float perc);
  float getLivePercentage(const int layer) const;

  /// fraction of good pixels in the clusters associated with this segment
  void setClusterGoodFraction(const int layer, const int hit, const float frac);
  float getClusterGoodFraction(const int layer, const int hit=0) const;


 protected:

  /// Private function to check layer validity range
  bool isValidLayer(const int layer) const;

  /// Private function to check coordinate validity range
  bool isValidCoor(const int coor) const;


  // Data member definition

  int   id; ///< unique segment id/event
  float position[4][2][3];
  float closest_approach[3];
  float mom3[3];
  float vertex_mom3[3];
  int   clusterID[4][2];
  float scatter[3];
  short nhits[4];
  bool  charge;
  bool  isprimary;
  float quality;
  float chisq;
  int   ndf;
  int   dchindex;
  float DCA;
  float DCA2D;
  float dEdX[2];
  bool  recomode;

  //added for v6
  float livePercentage[4];
  float segScore;
  float segQuality;
  float clusGoodFrac[4][2]; //the fraction of good pixels in a cluster associated with this segment

  ClassDef(SvxSegmentv6,1)
};

#endif

