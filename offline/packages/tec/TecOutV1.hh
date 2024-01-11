#ifndef TECOUTV1_h
#define TECOUTV1_h

#include "TecOut.hh"
#include <iostream>

#define TECMAXNUMHITS 100
#define TECMAXNUMTRACKS 1

/**
This class contains all tec output information. It 
has two TClonesArray's of Tec Hits and Tec Tracks.<br>
Detailed documentation: not ready yet.
@author Sasha Lebedev (ISU)
<a href="mailto:lebedev@iastate.edu">lebedev@iastate.edu</a>
@memo Tec Output class
*/
class TecOutV1: public TecOut {

public:

///
  TecOutV1();
///
  virtual ~TecOutV1();

///
  void Reset();
///
  void Clear(Option_t *option = "");
///
  void ClearHits(Option_t *option = "");
///
  void ClearTracks(Option_t *option = "");
///
  void identify(std::ostream& os = std::cout) const;

///
  int getHitGlobalIndex(int ihit) const;
///
  int getHitIndex(int ihit) const;
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
  float getHitX(int ihit) const;
///
  float getHitY(int ihit) const;
///
  int getHitTrackID(int ihit, int nn) const;
///
  int getHitTrackID(int ihit) const;
///
  int getTrackIndex(int itrack) const;
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
  float getTrackLength(int itrack) const;
///
  int getTrackNdEdXbins(int itrack) const;
///
  int getTrackNhits(int itrack) const;
///
  int getTrackNhits(int itrack, int iplane) const;
///
  int getTrackNwires(int itrack, int iplane) const;
///
  float getTrackAlpha(int itrack) const;
///
  float getTrackPhi(int itrack) const;
///
  float getTrackCharge(int itrack) const;
///
  float getTrackPt(int itrack) const;
///
  float getTrackSlope(int itrack) const;
///
  float getTrackIntercept(int itrack) const;
///
  int getRunNumber() const {return RunNumber;}


///
  void setHitADC(int ihit, int a);
///
  void setHitCharge(int ihit, float a);
///
  void setHitX(int ihit, float x);
///
  void setHitY(int ihit, float a);
///
  void setHitTrackID(int ihit, int nn, int id);
///
  void setHitTrackID(int ihit, int i);
///
  void setTrackIndex(int itrack, int ind);
///
  void setTrackXin(int itrack, float a);
///
  void setTrackXout(int itrack, float a);
///
  void setTrackYin(int itrack, float a);
///
  void setTrackYout(int itrack, float a);
///
  void setTrackdEdX(int itrack, float a);
///
  void setTrackLength(int itrack, float a);
///
  void setTrackNdEdXbins(int itrack, int nn);
///
  void setTrackNhits(int itrack, int nh);
///
  void setTrackNhits(int itrack, int iplane, int nh);
///
  void setTrackNwires(int itrack, int iplane, int iw);

///
  void setRunNumber(int rn) {RunNumber=rn;}


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

  ClassDef(TecOutV1,1)

};

#endif


