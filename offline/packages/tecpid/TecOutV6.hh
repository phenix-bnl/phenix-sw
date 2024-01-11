#ifndef TECOUTV6_h
#define TECOUTV6_h

#include "TecOut.hh"
#include <iostream>

class TClonesArray;
class TecTrackTRv3;
/**
This class contains all tec output information. It 
has two TClonesArray's of TecShortHit and TecTrackTR objects.<br>
Detailed documentation: not ready yet.
@author Sasha Lebedev (ISU)
<a href="mailto:lebedev@iastate.edu">lebedev@iastate.edu</a>
@memo Tec Output class
*/
class TecOutV6: public TecOut {

public:

  TecOutV6();
  virtual ~TecOutV6();
  TecOutV6& operator=(const TecOut &);

  void Reset();
  int  isValid() const;
  void Clear(Option_t *option = "");
  void ClearTracks(Option_t *option = "");
  void identify(std::ostream& os = std::cout) const;

  int AddTecTrack(float* xyzin, float* xyzout);
  int AddTecTrack(TecTrack &source); 
  int AddTecTrack(TecTrackTRv3 &source); 

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
  int   getNTracks() const {return NumberOfTracks;}

  unsigned int get_TecNTrack()  const {return NumberOfTracks;}

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

  void set_TecNTrack(const unsigned int ntrk) {NumberOfTracks = ntrk;return;}

  TClonesArray *GetTecTracks() const {return TecTracks;}

protected:

  int NumberOfTracks;

  TClonesArray *TecTracks;

  ClassDef(TecOutV6,1)

};

#endif


