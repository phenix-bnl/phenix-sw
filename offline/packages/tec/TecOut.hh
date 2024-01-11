#ifndef TECOUT_h
#define TECOUT_h

#include <iostream>
#include "PHObject.h"
#include "TecTrack.hh"
#include "TClonesArray.h"
/**
This is a basic virtual class for Tec output. It 
contains declarations of virtual functions to be used for Tec data
reading from DSTs.<br>
Detailed documentation: not ready yet.
@author Sasha Lebedev (ISU)
<a href="mailto:lebedev@iastate.edu">lebedev@iastate.edu</a>
@memo Basic TEC Output class
*/

//class TClonesArray;

class TecOut: public PHObject {

 public:
///
  virtual ~TecOut() {}

///
  virtual void Reset();

///
  virtual int isValid() const;

///
  virtual int getHitGlobalIndex(int ihit) const {return -1;}
///
  virtual int getHitIndex(int ihit) const {return -1;}
///
  virtual int getHitSide(int ihit) const {return -1;}
///
  virtual int getHitSector(int ihit) const {return -1;}
///
  virtual int getHitPlane(int ihit) const {return -1;}
///
  virtual int getHitWire(int ihit) const {return -1;}
///
  virtual int getHitTimeBin(int ihit)  const {return -1;}
///
  virtual int getHitADC(int ihit)  const {return -1;}
///
  virtual float getHitCharge(int ihit)  const {return -1.;}
///
  virtual float getHitX(int ihit)  const {return -1.;}
///
  virtual float getHitY(int ihit)  const {return -1.;}
///
  virtual int getHitTrackID(int, int )  const {return -1;}
///
  virtual int getHitTrackID(int ihit)  const {return -1;}
///
  virtual int getTrackID(int itrack)  const {return -1;}
///
  virtual int getTrackIndex(int itrack)  const {return -1;}
///
  virtual int getTrackSector(int itrack)  const {return -1;}
///
  virtual int getTrackSide(int itrack)  const {return -1;}
///
  virtual float getTrackXin(int itrack)  const {return -1.;}
///
  virtual float getTrackXout(int itrack)  const {return -1.;}
///
  virtual float getTrackYin(int itrack)  const {return -1.;}
///
  virtual float getTrackYout(int itrack)  const {return -1.;}
///
  virtual float getTrackXinError(int itrack)  const {return -1.;}
///
  virtual float getTrackXoutError(int itrack)  const {return -1.;}
///
  virtual float getTrackYinError(int itrack)  const {return -1.;}
///
  virtual float getTrackYoutError(int itrack)  const {return -1.;}
///
  virtual float getTrackdEdX(int itrack)  const {return -1.;}
///
  virtual float getTrackTr(int itrack)  const {return -1.;}
///
  virtual float getTrackdEdX(int itrack, int iplane)  const {return -1.;}
///
  virtual float getTrackTr(int itrack, int iplane)  const {return -1.;}
///
  virtual float getTrackLength(int itrack)  const {return -1.;}
///
  virtual int getTrackNdEdXbins(int itrack)  const {return -1;}
///
  virtual int getTrackNdEdXbins(int itrack, int iplane)  const {return -1;}
///
  virtual int getTrackNhits(int itrack)  const {return -1;}
///
  virtual int getTrackNtr(int itrack)  const {return -1;}
///
  virtual int getTrackNhits(int itrack, int iplane)  const {return -1;}
///
  virtual int getTrackNtr(int itrack, int iplane)  const {return -1;}
///
  virtual int getTrackNwires(int itrack, int iplane)  const {return -1;}
///
  virtual float getTrackChi2(int itrack)  const {return -1.;}
///
  virtual float getTrackAlpha(int itrack)  const {return -1.;}
///
  virtual float getTrackPhi(int itrack)  const {return -1.;}
///
  virtual float getTrackCharge(int itrack)  const {return -1.;}
///
  virtual float getTrackPt(int itrack)  const {return -1.;}
///
  virtual float getTrackSlope(int itrack)  const {return -1.;}
///
  virtual float getTrackIntercept(int itrack)  const {return -1.;}
///
  virtual float getTrackWeightedTimeBin(int itrack) const {return -1.;}
///
  virtual float getTrackWeightedTimeBinSq(int itrack) const {return -1.;};
///
  virtual int   getTrackNhits100(int itrack) const {return -1;}
///
  virtual int   getTrackNhits200(int itrack) const {return -1;}
///
  virtual int   getTrackNhits20(int itrack) const {return -1;}
///
  virtual int   getTrackNhits50(int itrack) const {return -1;}
///
  virtual float getTrackdEdX06(int itrack) const {return -1.;}
///
  virtual float getTrackWeightedTimeBin(int itrack, int iplane) const {return -1.;}
///
  virtual int   getTrackNhits100(int itrack, int iplane) const {return -1;}
///
  virtual int   getTrackNhits20(int itrack, int iplane) const {return -1;}
///
  virtual float getTrackdEdX06(int itrack, int iplane) const {return -1.;}
///
  virtual float getTrackDE(int itrack, int iplane) const {return -1.;}
///
  virtual float getTrackDE(int itrack) const {return -1.;}
///
  virtual float getLikelihood(int itrack) const {return -1.;}
///
  virtual int   getTrackPc3Pointer(int itrack) const {return -1;}
///
  virtual float getTrackPc3Distance(int itrack) const {return -1;}
///
  virtual float getTrackPc3Z(int itrack) const {return -1;}
///
  virtual int   getTrackPc3sPointer(int itrack) const {return -1;}
///
  virtual float getTrackPc3sDistance(int itrack) const {return -1;}
///
  virtual int   getTrackPc1Pointer(int itrack) const {return -1;}
///
  virtual float getTrackPc1Distance(int itrack) const {return -1;}
///
  virtual float getTrackPc1Z(int itrack) const {return -1;}
///
  virtual int   getTrackPc1sPointer(int itrack) const {return -1;}
///
  virtual float getTrackPc1sDistance(int itrack) const {return -1;}
///
  virtual int   getTrackEmcPointer(int itrack) const {return -1;}
///
  virtual float getTrackEmcDistance(int itrack) const {return -1;}
///
  virtual float getTrackEmcZ(int itrack) const {return -1;}
///
  virtual float getTrackEmcEcore(int itrack) const {return -1;}
///
  virtual int   getTrackEmcsPointer(int itrack) const {return -1;}
///
  virtual float getTrackEmcsDistance(int itrack) const {return -1;}
///
  virtual float getTrackEmcsEcore(int itrack) const {return -1;}
///
  virtual int   getTrackCrkN0(int itrack) const {return -1;}
///
  virtual float getTrackCrkNpe0(int itrack) const {return -1;}
///
  virtual float getTrackCrkChi2(int itrack) const {return -1;}
///
  virtual int   getTrackCrksN0(int itrack) const {return -1;}
///
  virtual float getTrackCrksNpe0(int itrack) const {return -1;}
///
  virtual float getTrackCrksChi2(int itrack) const {return -1;}
///
  virtual float getTrackCrkDist(int itrack) const {return -1;}
///
  virtual float getTrackCrksDist(int itrack) const {return -1;}
///
  virtual float getTrackLikelihood(int itrack) const {return -1;}
///
  virtual int getRunNumber()  const {return -1;}
///
  virtual void setRunNumber(int) {return;}
///
  virtual void setTrackNhits(int, int) {return;}
  ///
  virtual void setTrackXin(int, int) {return;}
  ///
  virtual void setTrackYin(int, int) {return;}
  ///
  virtual void setTrackXout(int, int) {return;}
  ///
  virtual void setTrackYout(int, int) {return;}
///
  virtual void setTrackNtr(int, int) {return;}
///
  virtual void setTrackIndex(int, int) {return;}
///
  virtual void setTrackNhits(int, int, int) {return;}
///
  virtual void setTrackNtr(int, int, int) {return;}
///
  virtual void setTrackNwires(int, int, int) {return;}
///
  virtual void setTrackdEdX(int itrack, float a) {return;}
///
  virtual void setTrackdEdX(int itrack, int iplane, float a) {return;}
///
  virtual void setTrackTr(int itrack, float a) {return;}
///
  virtual void setTrackTr(int itrack, int iplane, float a) {return;}
///
 virtual void setTrackLength(int itrack, float a) {return;}
///
  virtual void setTrackNdEdXbins(int itrack, int nn) {return;}
///
  virtual void setTrackNdEdXbins(int itrack, int iplane, int nn) {return;}
///
  virtual void setHitADC(int ihit, int nn) {return;}
///
 virtual void setHitCharge(int ihit, float a) {return;}
///
 virtual void setHitX(int ihit, float x) {return;}
///
 virtual void setHitY(int ihit, float a) {return;}
///
 virtual void setHitTrackID(int ihit, int nn, int id) {return;}
///
 virtual void setHitTrackID(int ihit, int i) {return;}
///
  virtual void setTrackPhi(int itrack, float a) {return;}
///
  virtual void setTrackChi2(int itrack, float a) {return;}
///
  virtual void setTrackAlpha(int itrack, float a) {return;}
///
  virtual void setTrackWeightedTimeBin(int itrack, float a) {return;}
///
  virtual void setTrackWeightedTimeBinSq(int itrack, float a) {return;}
///
  virtual void setTrackNhits100(int itrack, int nh) {return;}
///
  virtual void setTrackNhits200(int itrack, int nh) {return;}
///
  virtual void setTrackNhits50(int itrack, int nh) {return;}
///
  virtual void setTrackdEdX06(int itrack, float a) {return;}
///
  virtual void setTrackDE(int itrack, int nn, float a) {return;}
///
  virtual void setTrackWeightedTimeBin(int itrack, int nn, float a) {return;}
///
  virtual void setTrackNhits100(int itrack, int nn, int nh) {return;}
///
  virtual void setTrackNhits20(int itrack, int nn, int nh) {return;}
///
  virtual void setTrackdEdX06(int itrack, int nn, float a) {return;}
///
  virtual void setTrackLikelihood(int itrack, float a) {return;}
///
  virtual TClonesArray *GetTecTracks() const {return 0;}
///
  virtual TClonesArray *GetTecHits() const {return 0;}
///
  virtual int AddTecHit(int iindex, int iwire, int ibin,
                        int adc, float charge, float* xyz, int itrack) {return -1;}
///
  virtual int AddTecTrack(float* xyzin, float* xyzout) {return -1;}
///
  virtual int AddTecTrack(TecTrack &source) {return -1;}
  virtual void AddTecTrack(const unsigned int itrk) {return;}
///
  virtual int getNHits()  const {return -1;}
///
  virtual int getNTracks()  const {return -1;}

///
 virtual void identify(std::ostream& os = std::cout) const;

  virtual int set_TClonesArraySize (const unsigned int ntrk) {return -1;}


  virtual void set_TecNTrack(const unsigned int ntrk) {return;} 
  virtual unsigned int get_TecNTrack() const {return 0;} 

  virtual short get_Pc1Hit(const unsigned int itrk) const {return -1;}
  virtual void set_Pc1Hit(const unsigned int itrk, const short ival) {return;}

  virtual short get_Pc3Hit(const unsigned int itrk) const {return -1;}
  virtual void set_Pc3Hit(const unsigned int itrk, const short ival) {return;}

  virtual short get_TofHit(const unsigned int itrk) const {return -1;}
  virtual void set_TofHit(const unsigned int itrk, const short ival) {return;}

  virtual short get_EmcHit(const unsigned int itrk) const {return -1;}
  virtual void set_EmcHit(const unsigned int itrk, const short ival) {return;}

  virtual int get_index(const unsigned int itrk) const {return -1;}
  virtual void set_index(const unsigned int itrk, const int ival) {return;}

  virtual int get_nhits(const unsigned int itrk) const {return -1;}
  virtual void set_nhits(const unsigned int itrk, const int ival) {return;}

  virtual float get_pt(const unsigned int itrk) const {return -1.;}
  virtual void set_pt(const unsigned int itrk, const float rval) {return;}

  virtual float get_alpha(const unsigned int itrk) const {return -1.;}
  virtual void set_alpha(const unsigned int itrk, const float rval) {return;}

  virtual float get_phi(const unsigned int itrk) const {return -1.;}
  virtual void set_phi(const unsigned int itrk, const float rval) {return;}

  virtual float get_xin(const unsigned int itrk) const {return -1.;}
  virtual void set_xin(const unsigned int itrk, const float rval) {return;}

  virtual float get_xout(const unsigned int itrk) const {return -1.;}
  virtual void set_xout(const unsigned int itrk, const float rval) {return;}

  virtual float get_yin(const unsigned int itrk) const {return -1.;}
  virtual void set_yin(const unsigned int itrk, const float rval) {return;}

  virtual float get_yout(const unsigned int itrk) const {return -1.;}
  virtual void set_yout(const unsigned int itrk, const float rval) {return;}

  virtual float get_dEdx1(const unsigned int itrk) const {return -1.;}
  virtual void set_dEdx1(const unsigned int itrk, const float rval) {return;}

  virtual float get_dEdx2(const unsigned int itrk) const {return -1.;}
  virtual void set_dEdx2(const unsigned int itrk, const float rval) {return;}


  ClassDef(TecOut,1)

};

#endif


