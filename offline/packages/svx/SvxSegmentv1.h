// ====================
// FILE: SvxSegmentv1.h
// ====================

#ifndef __SVXSEGMENTV1_H_
#define __SVXSEGMENTV1_H_

#include "PHObject.h"
#include "SvxSegment.h"


class SvxSegmentv1 : public SvxSegment
{

 public:

  SvxSegmentv1();
  virtual ~SvxSegmentv1() {};

  // The "standard PHObject response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;
  
  
  //in the following, the "layer" argument starts at 0
  //in each layer there can be up to 2 hits.  getNhits(int layer) const  tells how many hits
  //the "hit" argument should be either 0 or 1, for the first or second hits, if they exist
  
  
  
  
  //index of cluster in the SvxClusterList
  int getClusterID(const int layer, const int hit=0) const {return clusterID[layer][hit];}
  
  //hit position from the track model.  for the cluster center position, get the cluster from SvxClusterList
  float getProjectedPosition(const int layer, const int coor, const short hit=0) const { return position[layer][hit][coor]; }  //coor 0,1,2 == x,y,z
  
  //whether the track could be from the primary vertex
  bool getPrimary() const { return isprimary; }
  
  //magnitude of 3-momentum
  float getMomentum() const ;
  
  //momentum vector at inner most hit
  float get3Momentum(const int coor) const { return mom3[coor];}
  
  //momentum vector at primary vertex, if track determined to be possibly primary
  float get3MomentumAtPrimaryVertex(const int coor) const { return vertex_mom3[coor];}
  
  //track model position of the inner most hit.  almost always the first pixel layer
  float getInnerMostProjectedPosition(const int coor) const ;
  
  //scattering angle at a given layer as calculated from the track model
  float getScatter(int layer) const {return scatter[layer];}
  
  //charge of the track, true=positive
  bool IsPositive() const { return charge; }
  
  //# hits in a given layer
  short getNhits(const int layer) const {return nhits[layer];}
  
  //track fit quality.  should be a negative value.  the closer to 0 it is, the better the fit
  float getQuality() const {return quality;}
  
  //index of the associated central track in the PHCentralTrack object (or DCHTrack object), if it exists
  int getDchIndex() const {return dchindex;}
  
  //distance of closest approach to the primary vertex, from the track model
  float getDCA() const {return DCA;}
  
  //distance of closest approach to the primary vertex in the r-phi plane, from the track model
  float getDCA2D() const {return DCA2D;}
  
  // energy loss in strip layers
  float get_dEdX1() const  {return dEdX[0];}
  float get_dEdX2() const  {return dEdX[1];}
  
  void setClusterID(const int layer, const int hit, const int index){clusterID[layer][hit]=index;}
  void setScatter(const int layer, const float sct){scatter[layer]=sct;}
  void setProjectedPosition(const int layer, const float x,const  float y, const float z, const short hit=0);
  void set3MomentumAtPrimaryVertex(const float px, const float py, const float pz);
  void set3Momentum(const float px, const float py, const float pz);
  void setIsPositive(const bool hel) { charge=hel; }
  void setPrimary(const bool prim) { isprimary=prim; }
  void setNhits(const int layer, const short n) {nhits[layer]=n;}
  void setQuality(const float q) {quality=q;}
  void setDchIndex(const int ind){dchindex=ind;}
  void setDCA(const float d){DCA=d;}
  void setDCA2D(const float d){DCA2D=d;}
  void setInnerMostProjectedPosition(const int coor, const float xyz);
  void set_dEdX1(const float val) {dEdX[0]=val;}
  void set_dEdX2(const float val) {dEdX[1]=val;}
  
 protected:


  // Data member definition

  int clusterID[4][2];


  float position[4][2][3];
  short nhits[4];
  float mom3[3];
  float vertex_mom3[3];
  bool charge;
  bool isprimary;
  float quality;
  float dEdX[2];
  
  int dchindex;

  
  float DCA, DCA2D;
  
  float scatter[3];

  ClassDef(SvxSegmentv1,1)

};

#endif

