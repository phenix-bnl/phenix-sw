#ifndef TECOUTV7_h
#define TECOUTV7_h

#include "TecOut.hh"
#include <iostream>

class TClonesArray;
class TecTrackV2;
/**
This class contains all tec output information. It 
has two TClonesArray's of TecShortHit and TecTrackTR objects.<br>
Detailed documentation: not ready yet.
@author Sasha Lebedev (ISU)
<a href="mailto:lebedev@iastate.edu">lebedev@iastate.edu</a>
@memo Tec Output class
*/
class TecOutV7: public TecOut {

public:

  TecOutV7();
  virtual ~TecOutV7();

  void Reset();
  int  isValid() const;
  void Clear(Option_t *option = "");
  void ClearHits(Option_t *option = "");
  void ClearTracks(Option_t *option = "");
  void identify(std::ostream& os = std::cout) const;

  int AddTecHit(int iindex, int iwire, int ibin, int adc, float charge, float* xyz, int itrack);
  int AddTecTrack(float* xyzin, float* xyzout);
  int AddTecTrack(TecTrack &source); 
  int AddTecTrack(TecTrackV2 &source); 

  int   getHitIndex(int ihit) const;
  int   getHitGlobalIndex(int ihit) const;
  int   getHitSector(int ihit) const;
  int   getHitPlane(int ihit) const;
  int   getHitSide(int ihit) const;
  int   getHitWire(int ihit) const;
  int   getHitTimeBin(int ihit) const;
  int   getHitADC(int ihit) const;
  float getHitCharge(int ihit) const;
  float getHitX(int ihit) const;
  float getHitY(int ihit) const;
  int   getHitTrackID(int ihit, int nn) const;
  int   getHitTrackID(int ihit) const;

  int   getTrackIndex(int itrack) const;
  int   getTrackSector(int itrack) const;
  int   getTrackSide(int itrack) const;
  float getTrackXin(int itrack) const;
  float getTrackXout(int itrack) const;
  float getTrackYin(int itrack) const;
  float getTrackYout(int itrack) const;
  float getTrackXinError(int itrack) const;
  float getTrackXoutError(int itrack) const;
  float getTrackYinError(int itrack) const;
  float getTrackYoutError(int itrack) const;
  float getTrackDE(int itrack) const;
  float getTrackDE(int itrack, int iplane) const;
  float getTrackLength(int itrack) const;
  int   getTrackNdEdXbins(int itrack) const;
  int   getTrackNdEdXbins(int itrack, int iplane) const;
  int   getTrackNhits(int itrack) const;
  int   getTrackNhits(int itrack, int iplane) const;
  int   getTrackNwires(int itrack, int iplane) const;
  float getTrackChi2(int itrack) const;
  float getTrackAlpha(int itrack) const;
  float getTrackPhi(int itrack) const;
  float getTrackCharge(int itrack)  const;
  float getTrackPt(int itrack) const;
  float getTrackSlope(int itrack) const;
  float getTrackIntercept(int itrack) const;
  float getTrackWeightedTimeBin(int itrack) const;
  float getTrackWeightedTimeBin(int itrack, int iplane) const;
  int   getTrackNhits100(int itrack) const;
  int   getTrackNhits100(int itrack, int iplane) const;
  int   getTrackNhits20(int itrack) const;
  int   getTrackNhits20(int itrack, int iplane) const;
  int   getTrackNtr(int itrack) const;
  int   getTrackNtr(int itrack, int iplane) const;
  float getTrackTr(int itrack) const;
  float getTrackTr(int itrack,int iplane) const;
  float getTrackdEdX06(int itrack) const;
  float getTrackdEdX06(int itrack, int iplane) const;
  float getTrackLikelihood(int itrack) const;
  int   getTrackPc3Pointer(int itrack) const;
  float getTrackPc3Distance(int itrack) const;
  float getTrackPc3Z(int itrack) const;
  int   getTrackPc3sPointer(int itrack) const;
  float getTrackPc3sDistance(int itrack) const;
  int   getTrackPc1Pointer(int itrack) const;
  float getTrackPc1Distance(int itrack) const;
  float getTrackPc1Z(int itrack) const;
  int   getTrackPc1sPointer(int itrack) const;
  float getTrackPc1sDistance(int itrack) const;
  int   getTrackEmcPointer(int itrack) const;
  float getTrackEmcDistance(int itrack) const;
  float getTrackEmcZ(int itrack) const;
  float getTrackEmcEcore(int itrack) const;
  int   getTrackEmcsPointer(int itrack) const;
  float getTrackEmcsDistance(int itrack) const;
  float getTrackEmcsEcore(int itrack) const;
  int   getTrackCrkN0(int itrack) const;
  float getTrackCrkNpe0(int itrack) const;
  float getTrackCrkChi2(int itrack) const;
  int   getTrackCrksN0(int itrack) const;
  float getTrackCrksNpe0(int itrack) const;
  float getTrackCrksChi2(int itrack) const;
  float getTrackCrkDist(int itrack) const;
  float getTrackCrksDist(int itrack) const;

  int   getNHits() const {return NumberOfHits;}
  int   getNTracks() const {return NumberOfTracks;}
  int   getMaxNHits() const;
  int   getMaxNTracks() const;
  int   getRunNumber() const {return RunNumber;}

  unsigned int get_TecNTrack()  const {return NumberOfTracks;}
  int   get_index(const unsigned int itrk) const;
  int   get_nhits(const unsigned int itrk) const;
  //  short get_Pc1Hit(const unsigned int itrk) const;
  short get_Pc3Hit(const unsigned int itrk) const;
  short get_TofHit(const unsigned int itrk) const;
  short get_EmcHit(const unsigned int itrk) const;
  float get_pt(const unsigned int itrk) const;
  float get_xin(const unsigned int itrk) const;
  float get_xout(const unsigned int itrk) const;
  float get_yin(const unsigned int itrk) const;
  float get_yout(const unsigned int itrk) const;
  float get_phi(const unsigned int itrk) const;
  float get_alpha(const unsigned int itrk) const;
  float get_dEdx1(const unsigned int itrk) const;
  float get_dEdx2(const unsigned int itrk) const;


  void setHitX(int ihit, float x);
  void setHitY(int ihit, float a);
  void setHitADC(int ihit, int a);
  void setHitCharge(int ihit, float a);
  void setHitTrackID(int ihit, int nn, int id);
  void setHitTrackID(int ihit, int i);

  void setTrackIndex(int itrack, int ind);
  void setTrackNhits(int itrack, int i);
  void setTrackChi2(int itrack, float a);
  void setTrackAlpha(int itrack, float a);
  void setTrackPhi(int itrack, float a);
  void setTrackDE(int itrack, int iplane, float a);
  void setTrackLength(int itrack, float a);
  void setTrackNdEdXbins(int itrack, int iplane, int nn);
  void setTrackNhits(int itrack, int iplane, int nh);
  void setTrackNwires(int itrack, int iplane, int iw);
  void setTrackWeightedTimeBin(int itrack, int iplane, float a);
  void setTrackNhits100(int itrack, int iplane, int nh);
  void setTrackNhits20(int itrack, int iplane, int nh);
  void setTrackNtr(int itrack, int iplane, int ntr);
  void setTrackTr(int itrack,int iplane, float tr);
  void setTrackdEdX06(int itrack, int iplane, float a);
  void setTrackLikelihood(int itrack, float a);
  void setTrackXin(int itrack, float a);
  void setTrackXout(int itrack, float a);
  void setTrackYin(int itrack, float a);
  void setTrackYout(int itrack, float a);

  void setRunNumber(int rn) {RunNumber=rn;}

  void set_TecNTrack(const unsigned int ntrk) {NumberOfTracks = ntrk;return;}
  void set_dEdx2(const unsigned int itrk, const float rval);
  void set_dEdx1(const unsigned int itrk, const float rval);
  void set_alpha(const unsigned int itrk, const float rval);
  void set_phi(const unsigned int itrk, const float rval);
  void set_yout(const unsigned int itrk, const float rval);
  void set_yin(const unsigned int itrk, const float rval);
  void set_xout(const unsigned int itrk, const float rval);
  void set_xin(const unsigned int itrk, const float rval);
  void set_pt(const unsigned int itrk, const float rval);
  void set_EmcHit(const unsigned int itrk, const short ival);
  void set_TofHit(const unsigned int itrk, const short ival);
  void set_Pc3Hit(const unsigned int itrk, const short ival);
  void set_Pc1Hit(const unsigned int itrk, const short ival);
  void set_nhits(const unsigned int itrk, const int ival);
  void set_index(const unsigned int itrk, const int ival);

  TClonesArray *GetTecTracks() const {return TecTracks;}
  TClonesArray *GetTecHits() const {return TecHits;}

protected:

  int NumberOfHits;
  int NumberOfTracks;

  TClonesArray *TecHits;
  TClonesArray *TecTracks;

  int RunNumber;

  ClassDef(TecOutV7,1)

};

#endif

