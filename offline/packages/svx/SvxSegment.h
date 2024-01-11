// ==================
// FILE: SvxSegment.h
// ==================

#ifndef __SVXSEGMENT_HH_
#define __SVXSEGMENT_HH_

#include <PHObject.h>
#include <phool.h>

#include <iostream>

/**
 * This is the base class for SVX(VTX) standalone tracks.
 *
 */
class SvxSegment : public PHObject
{

 public:
  virtual ~SvxSegment() {}

/// Index of the standalone track in the SvxSegmentList
  virtual int getSegmentID() const  {PHOOL_VIRTUAL_WARN("SegmentID "); return -9999;}
  virtual void setSegmentID(const int ID) {PHOOL_VIRTUAL_WARN("SegmentID ");}


/// Index of associated cluster in the SvxClusterList.
/// Layer: 0-3, hit: 0-1 (up to 2 hits in each layer), default: -1
  virtual void setClusterID(const int layer, const int hit, const int index){PHOOL_VIRTUAL_WARN("setClusterID ");}
  virtual int getClusterID(const int layer, const int hit=0) const {PHOOL_VIRTUAL_WARN("getClusterID "); return -9999;}

/// Scattering angle at a given layer as calculated from track model
  virtual void setScatter(const int layer, const float sct){PHOOL_VIRTUAL_WARN("setScatter ");}
  virtual float getScatter(const int layer) const {PHOOL_VIRTUAL_WARN("getScatter "); return -9999.;}

  virtual int get_clusterList (const unsigned int ind) const {PHOOL_VIRTUAL_WARN("get_clusterList "); return -9999;}

/// Predicted hit position from the track model
/// layer: 0-3, coor: 0-2 (x,y,z), hit: 0-1 (up to 2 hits per layer)
  virtual void setProjectedPosition(const int layer, const float x, const float y, const float z, const short hit=0) {PHOOL_VIRTUAL_WARN("ProjectedPosition ");}
  virtual float getProjectedPosition(const int layer, const int coor, const short hit=0) const  {PHOOL_VIRTUAL_WARN("ProjectedPosition "); return -9999.;} 

/// Magnitude of momentum vector at innermost hit [GeV/c]  
  virtual float getMomentum() const  {PHOOL_VIRTUAL_WARN("Momentun "); return -9999.;}


/// Momentum vector components at innermost hit [GeV/c]
/// coor: 0-2 (x,y,z)
  virtual void set3Momentum(const float px, const float py, const float pz) {PHOOL_VIRTUAL_WARN("3Momentum ");}
  virtual float get3Momentum(const int coor) const  {PHOOL_VIRTUAL_WARN("3Momentum "); return -9999.;}
   
/// Momentum vector components at closest approach to the primary vertex [GeV/c]
/// coor: 0-2 (x,y,z)
  virtual void set3MomentumAtPrimaryVertex(const float px, const float py, const float pz){PHOOL_VIRTUAL_WARN("set3MomentumAtPrimaryVertex ");}
  virtual float get3MomentumAtPrimaryVertex(const int coor) const  {PHOOL_VIRTUAL_WARN("get3MomentumAtPrimaryVertex "); return -9999.;}

/// Charge of track, true if positive
  virtual void setIsPositive(const bool hel) {PHOOL_VIRTUAL_WARN("Charge ");}
  virtual bool IsPositive() const {PHOOL_VIRTUAL_WARN("Charge "); return false;}

/// True if track can come from primary vertex
  virtual void setPrimary(const bool prim) {PHOOL_VIRTUAL_WARN("Primary ");}
  virtual bool getPrimary() const  {PHOOL_VIRTUAL_WARN("Primary "); return false;}
   
/// Number of associated clusters in a given layer
/// up to 2 clusters in each layer
  virtual void setNhits(const int layer, const short n){PHOOL_VIRTUAL_WARN("setNhits ");}
  virtual short getNhits(const int layer) const {PHOOL_VIRTUAL_WARN("getNhits "); return -9999;}
   
/// Always negative, the closer to 0, the better the fit
  virtual void setQuality(const float q){PHOOL_VIRTUAL_WARN("Quality ");}
  virtual float getQuality() const {PHOOL_VIRTUAL_WARN("Quality "); return -9999.;}
   
/// Fit chi-square
  virtual void setChiSq(const float chisq){PHOOL_VIRTUAL_WARN("ChiSq ");}
  virtual float getChiSq() const {PHOOL_VIRTUAL_WARN("ChiSq "); return -9999.;}

/// NUmber of degrees of freedom for the fit
  virtual void setNDF(const int ndf){PHOOL_VIRTUAL_WARN("NDF ");}
  virtual int getNDF() const {PHOOL_VIRTUAL_WARN("NDF "); return -9999;}

/// Index of the associated PHCentralTrack, negative if no association
  virtual void setDchIndex(const int ind){PHOOL_VIRTUAL_WARN("DchIndex ");}
  virtual int getDchIndex() const {PHOOL_VIRTUAL_WARN("DchIndex "); return -9999;}
   
/// Energy loss in strip layers (currently (Feb2013) not calibrated)
  virtual void set_dEdX1(const float val) {PHOOL_VIRTUAL_WARN("dEdX1 ");}
  virtual void set_dEdX2(const float val) {PHOOL_VIRTUAL_WARN("dEdX2 ");}
  virtual float get_dEdX1() const  {PHOOL_VIRTUAL_WARN("dEdX1 "); return -9999;}
  virtual float get_dEdX2() const  {PHOOL_VIRTUAL_WARN("dEdX2 "); return -9999;}

  virtual void set_recomode(const bool mode) {PHOOL_VIRTUAL_WARN("recomode ");}
  virtual bool get_recomode() const  {PHOOL_VIRTUAL_WARN("recomode "); return false;}
  
  virtual void Reset() {
    std::cout << PHWHERE << "ERROR: Reset() not implemented by daughter function" << std::endl;
    return;
  }

  virtual int isValid() const {
    std::cout << PHWHERE << "isValid() not implemented by daughter function" << std::endl;
    return 0;
  }

  virtual void identify(std::ostream &os=std::cout) const {
    os << "identify yourself: virtual SvxSegment object" << std::endl;
    return;
  }
    
/// Distance of closest approach in 3D
/// d = 0-2 (x,y,z)
  virtual void setDCA(const float d){PHOOL_VIRTUAL_WARN("Primary ");}
  virtual float getDCA() const {PHOOL_VIRTUAL_WARN("Primary ");return -9999.;}

/// Distance of closest approach in X-Y plane
/// d = 0-2 (x,y,z)
  virtual void setDCA2D(const float d){PHOOL_VIRTUAL_WARN("Primary ");}
  virtual float getDCA2D() const {PHOOL_VIRTUAL_WARN("Primary ");return -9999.;}
  
  virtual float getInnerMostProjectedPosition(const int coor) const {PHOOL_VIRTUAL_WARN("Primary ");return -9999.;}
  virtual void setInnerMostProjectedPosition(const int coor, const float xyz){PHOOL_VIRTUAL_WARN("Primary ");}

/// getDiffFromPrimaryVertex and setDiffFromPrimaryVertex are not used any more
  virtual float getDiffFromPrimaryVertex(const int coor) const  
  {  
    std::cout << "getDiffFromPrimaryVertex() const  is not used any more" << std::endl;
    return -9999.9;
  }
  virtual void setDiffFromPrimaryVertex(const float dx0,const  float dy0, const float dz0)
  {
    std::cout << "setDiffFromPrimaryVertex() is not used any more" << std::endl;
    return;
  }

  virtual float getClosestApproach(const int coor) const 
  {
    PHOOL_VIRTUAL_WARN("getClosestApproach") ;
    return -9999.9;
  }
  virtual void setClosestApproach(const float cax, const float cay, const float caz)
  {
    PHOOL_VIRTUAL_WARN("setClosestApproach");
    return;
  }

  virtual void setSegmentScore(const float score)
  {
    PHOOL_VIRTUAL_WARN("setSegmentScore");
    return;
  }
  virtual float getSegmentScore() const
  {
    PHOOL_VIRTUAL_WARN("getSegmentScore");
    return -9999.9;
  }

  virtual void setSegmentQuality(const float quality)
  {
    PHOOL_VIRTUAL_WARN("setSegmentQuality");
    return;
  }
  virtual float getSegmentQuality() const 
  {
    PHOOL_VIRTUAL_WARN("getSegmentQuality");
    return -9999.9;
  }

  virtual void setLivePercentage(const int layer, const float frac)
  {
    PHOOL_VIRTUAL_WARN("setLiveFraction");
    return;
  }
  virtual float getLivePercentage(const int layer) const
  {
    PHOOL_VIRTUAL_WARN("getLiveFraction");
    return -9999.9;
  }

  virtual void setClusterGoodFraction(const int layer, const int hit, const float frac)
  {
    PHOOL_VIRTUAL_WARN("setClusterGoodFraction");
    return;
  }
  virtual float getClusterGoodFraction(const int layer, const int hit=0) const
  {
    PHOOL_VIRTUAL_WARN("getClusterGoodFraction");
    return -9999.;
  }

  ClassDef(SvxSegment, 1);
};
#endif
