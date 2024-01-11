#ifndef TECOUTV2_h
#define TECOUTV2_h

#include "TecOut.hh"
#include <iostream>

class TClonesArray;

#define TECMAXNUMHITS 100
#define TECMAXNUMTRACKS 1

/**
This class contains all tec output information. It 
has two TClonesArray's of TecShortHits and TecTracks.<br>
Detailed documentation: not ready yet.
@author Sasha Lebedev (ISU)
<a href="mailto:lebedev@iastate.edu">lebedev@iastate.edu</a>
@memo Tec Output class
*/
class TecOutV2: public TecOut {

public:

///
  TecOutV2();
///
  virtual ~TecOutV2();

///
  void Reset();
///
  int isValid() const;
///
  void Clear(Option_t *option = "");
///
  void ClearHits(Option_t *option = "");
///
  void ClearTracks(Option_t *option = "");
///
  void identify(std::ostream& os = std::cout) const;

///
  int getHitIndex(int ihit) const;
///
  int getHitGlobalIndex(int ihit) const;
///
  int getHitSector(int ihit) const;
///
  int getHitPlane(int ihit) const;
///
  int getHitSide(int ihit) const;
///
  int getHitWire(int ihit) const;
///
  int getHitTimeBin(int ihit) const;
///
  int getHitADC(int ihit) const;
///
  float getHitCharge(int ihit) const;
///
  void setHitADC(int ihit, int a);
///
  void setHitCharge(int ihit, float a);
///
  float getHitX(int ihit) const;
///
  void setHitX(int ihit, float x);
///
  float getHitY(int ihit) const;
///
  void setHitY(int ihit, float a);
///
  int getHitTrackID(int ihit, int nn) const;
///
  void setHitTrackID(int ihit, int nn, int id);
///
  int getHitTrackID(int ihit) const;
///
  void setHitTrackID(int ihit, int i);
///
  int getTrackIndex(int itrack) const;
///
  void setTrackIndex(int itrack, int ind);
///
  int getTrackSector(int itrack) const;
///
  int getTrackSide(int itrack) const;
///
  float getTrackXin(int itrack) const;
///
  float getTrackXout(int itrack) const;
///
  float getTrackYin(int itrack) const;
///
  float getTrackYout(int itrack) const;
///
  float getTrackXinError(int itrack) const;
///
  float getTrackXoutError(int itrack) const;
///
  float getTrackYinError(int itrack) const;
///
  float getTrackYoutError(int itrack) const;
///
  float getTrackdEdX(int itrack) const;
///
  float getTrackdEdX(int itrack, int iplane) const;
///
  void setTrackdEdX(int itrack, float a);
///
  float getTrackLength(int itrack) const;
///
  void setTrackLength(int itrack, float a);
///
  int getTrackNdEdXbins(int itrack) const;
///
  void setTrackNdEdXbins(int itrack, int nn);
///
  int getTrackNhits(int itrack) const;
///
  void setTrackNhits(int itrack, int nh);
///
  int getTrackNhits(int itrack, int iplane) const;
///
  void setTrackNhits(int itrack, int iplane, int nh);
///
  int getTrackNwires(int itrack, int iplane) const;
///
  void setTrackNwires(int itrack, int iplane, int iw);
///
  float getTrackAlpha(int itrack) const;
///
  float getTrackPhi(int itrack) const;
///
  float getTrackCharge(int itrack)  const;
///
  float getTrackPt(int itrack) const;
///
  float getTrackSlope(int itrack) const;
///
  float getTrackIntercept(int itrack) const;
///
  void setRunNumber(int rn) {RunNumber=rn;}
///
  int getRunNumber() const {return RunNumber;}


// for compatibility with microDST:

  unsigned int get_TecNTrack()  const {return NumberOfTracks;}
  void set_TecNTrack(const unsigned int ntrk) {NumberOfTracks = ntrk;return;}

  int get_index(const unsigned int itrk) const;
  void set_index(const unsigned int itrk, const int ival);

  int get_nhits(const unsigned int itrk) const;
  void set_nhits(const unsigned int itrk, const int ival);

  short get_Pc1Hit(const unsigned int itrk) const;
  void set_Pc1Hit(const unsigned int itrk, const short ival);

  short get_Pc3Hit(const unsigned int itrk) const;
  void set_Pc3Hit(const unsigned int itrk, const short ival);

  short get_TofHit(const unsigned int itrk) const;
  void set_TofHit(const unsigned int itrk, const short ival);

  short get_EmcHit(const unsigned int itrk) const;
  void set_EmcHit(const unsigned int itrk, const short ival);

  float get_pt(const unsigned int itrk) const;
  void set_pt(const unsigned int itrk, const float rval);

  float get_xin(const unsigned int itrk) const;
  void set_xin(const unsigned int itrk, const float rval);

  float get_xout(const unsigned int itrk) const;
  void set_xout(const unsigned int itrk, const float rval);

  float get_yin(const unsigned int itrk) const;
  void set_yin(const unsigned int itrk, const float rval);

  float get_yout(const unsigned int itrk) const;
  void set_yout(const unsigned int itrk, const float rval);

  float get_phi(const unsigned int itrk) const;
  void set_phi(const unsigned int itrk, const float rval);

  float get_alpha(const unsigned int itrk) const;
  void set_alpha(const unsigned int itrk, const float rval);

  float get_dEdx1(const unsigned int itrk) const;
  void set_dEdx1(const unsigned int itrk, const float rval);

  float get_dEdx2(const unsigned int itrk) const;
  void set_dEdx2(const unsigned int itrk, const float rval);



///
  int AddTecHit(int iindex, int iwire, int ibin,
                int adc, float charge, float* xyz, int itrack);
///
  int AddTecTrack(float* xyzin, float* xyzout);
///
  int AddTecTrack(TecTrack &source);


///
    TClonesArray *GetTecTracks() const {return TecTracks;}
///
    TClonesArray *GetTecHits() const {return TecHits;}
///
    int getNHits() const {return NumberOfHits;}
///
    int getNTracks() const {return NumberOfTracks;}
///
    int getMaxNHits() const;
///
    int getMaxNTracks() const;


protected:

/// Current number of tracks in the object
  int NumberOfHits;
/// Current number of hits in the object
  int NumberOfTracks;
/// List of Tec Hits
  TClonesArray *TecHits;
/// List of Tec Tracks
  TClonesArray *TecTracks;
///
  int RunNumber;

  ClassDef(TecOutV2,1)

};

#endif


