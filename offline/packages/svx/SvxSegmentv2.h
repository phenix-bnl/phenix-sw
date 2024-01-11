// ====================
// FILE: SvxSegmentv2.h
// ====================

#ifndef __SVXSEGMENTV2_H_
#define __SVXSEGMENTV2_H_

#include "SvxSegment.h"

#include <cmath>

class SvxSegmentv2 : public SvxSegment
{

 public:

  SvxSegmentv2();
  virtual ~SvxSegmentv2() {};

  // The "standard PHObject response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;
  
  
  // in the following, the "layer" argument starts at 0
  // in each layer there can be up to 2 hits.
  // getNhits(int layer) const  tells how many hits are on the layer
  // the "hit" argument should be either 0 or 1, for the first
  // or second hits, if they exist

  
  ///
  /// functions to get member valuables
  ///

  // index of cluster in the SvxClusterList
  int getClusterID(const int layer, const int hit=0) const 
  { return clusterID[layer][hit]; }
  
  // expected hit position calculated by track fitting.
  // coor 0,1,2 == x,y,z
  float getProjectedPosition(const int layer, const int coor, const short hit=0) const 
  { return position[layer][hit][coor]; }

  float getClosestApproach(const int coor) const 
  { return closest_approach[coor]; }

  // whether the track could be from the primary vertex
  bool getPrimary() const 
  { return isprimary; }

  // magnitude of 3-momentum
  float getMomentum() const 
  { return sqrt(mom3[0]*mom3[0] + mom3[1]*mom3[1] + mom3[2]*mom3[2]); }

  // momentum vector at inner most hit
  // coor 0,1,2 == x,y,z
  float get3Momentum(const int coor) const 
  { return mom3[coor]; }

  // momentum vector at the closest approach to the primary vertex.
  // coor 0,1,2 == x,y,z
  float get3MomentumAtPrimaryVertex(const int coor) const 
  { return vertex_mom3[coor]; }

  // expected hit position of the inner most hit calculated by the track fitting.
  float getInnerMostProjectedPosition(const int coor) const 
  { return position[0][0][coor]; }

  // scattering angle at a given layer as calculated from the track model
  float getScatter(const int layer) const 
  { return scatter[layer]; }
  
  // charge of the track, true=positive
  bool IsPositive() const
  { return charge; }
  
  // # hits in a given layer
  short getNhits(const int layer) const 
  { return nhits[layer]; }
  
  // track fit quality.
  // should be a negative value.
  // the closer to 0 it is, the better the fit
  float getQuality() const 
  { return quality; }
  
  // index of the associated central track in the PHCentralTrack object
  // (or DCHTrack object), if it exists.
  // if it does not exit, return -9999.
  int getDchIndex() const 
  { return dchindex; }
  
  //distance of closest approach to the primary vertex, from the track model
  float getDCA() const 
  { return DCA; }
  
  //distance of the closest approach to the primary vertex in the XY-plane.
  float getDCA2D() const 
  { return DCA2D; }
  
  // energy loss in strip layers
  float get_dEdX1() const  { return dEdX[0]; }
  float get_dEdX2() const  { return dEdX[1]; }
  

  ///
  /// functions to set member valuables
  ///

  void setProjectedPosition(const int layer, const float x, const float y, const float z, const short hit=0) {
    position[layer][hit][0] = x;
    position[layer][hit][1] = y;
    position[layer][hit][2] = z;
  }

  void setClosestApproach(const float cax, const float cay, const float caz) {
    closest_approach[0] = cax;
    closest_approach[1] = cay;
    closest_approach[2] = caz;
  }
  void set3Momentum(const float px, const float py, const float pz) {
    mom3[0] = px;
    mom3[1] = py;
    mom3[2] = pz;
  }
  void set3MomentumAtPrimaryVertex(const float px0, const float py0, const float pz0) {
    vertex_mom3[0] = px0;
    vertex_mom3[1] = py0;
    vertex_mom3[2] = pz0;
  }
  void setClusterID (const int layer, const int hit, const int idx) { clusterID[layer][hit] = idx;  }
  void setScatter   (const int layer, const float sct       ) { scatter[layer]        = sct;  }
  void setIsPositive(const bool hel                   ) { charge                = hel;  }
  void setPrimary   (const bool prim                  ) { isprimary             = prim; }
  void setNhits     (const int layer, short n         ) { nhits[layer]          = n;    }
  void setQuality   (const float q                    ) { quality               = q;    }
  void setDchIndex  (const int idx                    ) { dchindex              = idx;  }
  void setDCA       (const float dca                  ) { DCA                   = dca;  }
  void setDCA2D     (const float dca                  ) { DCA2D                 = dca;  }
  void setInnerMostProjectedPosition
                    (const int coor, const float xyz        ) { position[0][0][coor]  = xyz;  }
  void set_dEdX1    (const float val                  ) { dEdX[0]               = val;  }
  void set_dEdX2    (const float val                  ) { dEdX[1]               = val;  }
  
 protected:
  // Data member definition

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
  int   dchindex;
  float DCA;
  float DCA2D;
  float dEdX[2];  


  ClassDef(SvxSegmentv2,1)

};

#endif

