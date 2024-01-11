#ifndef TECTRACKTRV3_H
#define TECTRACKTRV3_H 

#include "TecBasicObject.hh"
#include "TecTrackTR.hh"
#include "TecTrack.hh"

///
class PHCompositeNode;
///
class TecTrackV2;

/** Methods and variables for one single track with TR information. This is stored in CNTs */
class TecTrackTRv3 : public TecTrackTR {
 
 public:
 
  /// Default constructor
  TecTrackTRv3();  
  /// Constructor from a line
  TecTrackTRv3(float xyzin[3], float xyzout[3]);
  /// Constructor from TecTrack object
  TecTrackTRv3(const TecTrack &source);
  /// Copy from TecTrackV2 (stored in DST_TEC)
  TecTrackTRv3& operator=(const TecTrackV2 &);
  /// Copy
  TecTrackTRv3& operator=(const TecTrackTRv3 &);
  ///
  virtual ~TecTrackTRv3() { }

  /// index = sector*2 + side
  int   getIndex()  	const { return Sector*TECMAXSIDE+Side; }
  ///
  int   getSector()  	const { return Sector; }
  ///
  int   getSide()  	const { return Side; }
  ///
  float getPhi()  	const { return Phi; }
  /// 
  float getAlpha()  	const { return Alpha; }
  /// 
  float gettecMomentum() const { return tecMomentum; }
  /// 
  int   getNwires(int i)  const { return Nwires[i]; }
  ///
  int   getNHITS(int i) const { return NHITS[i]; }
  /// N planes with at least 3 hits
  int   getNFplanes() 	const;
  ///
  int   getNFwires() 	const; 
  /// 
  int   getNdEdXbins(int i) 	const { return NdEdXbins[i]; }
  /// 
  float getdEdX06(int i) 	const { return dEdX06[i]; }
  ///
  float getTrackLength() const { return TrackLength; }
  ///
  int   getNhits100(int i)  	const { return Nhits100[i]; }
  /// 
  int   getNhits20(int i)  	const { return Nhits20[i]; }
  ///
  int   getNhits()       const;
  ///
  int   getNhits100()      const;
  ///
  int   getNhits20()       const;
  /// 
  int   getNTR()       const;
  /// 
  float getTR()        const;
  /// 
  float getDE()        const;
  /// 
  float getWeightedTimeBin() const;
  ///
  float getdEdX()      const;
  ///
  float getdEdX06()      const;
  ///
  int   getNTR(int i)        const { return NTR[i]; }
  ///
  float getDE(int i)    const { return DE[i]; }
  ///
  float getTR(int i)    const { return TR[i]; }
  ///
  float getWeightedTimeBin(int i) const { return WeightedTimeBin[i]; }
  ///
  float getLikelihood() const { return hitLikelihood;}

  ///
  void setSector(const int a)  		{Sector=a; return;}
  ///
  void setSide(const int a)  		{Side=a; return;}
  ///
  void setAlpha(const float a);
  ///
  void setPhi(const float a) {Phi = a; return;}
  ///
  void setNwires(const int i, const int a){Nwires[i]=a; return;}
  ///
  void setNHITS(const int i, const int a) {NHITS[i]=a; return;}
  ///
  void setNdEdXbins(const int i, const int a)  	{NdEdXbins[i]=a; return;}
  ///
  void setdEdX06(const int i, const float a)  	{dEdX06[i]=a; return;}
  ///
  void setTrackLength(const float a)  	{TrackLength=a; return;}
  ///
  void setNhits100(const int i, const int a)  	{Nhits100[i]=a; return;}
  ///
  void setNhits20(const int i, const int a)  	{Nhits20[i]=a; return;}
  ///
  void setNTR(const int i, const int a) {NTR[i]=a; return;}
  ///
  void setDE(const int i, const float a)  {DE[i]=a; return;}
  ///
  void setTR(const int i, const float a)  {TR[i]=a; return;}
  ///
  void setWeightedTimeBin(const int i, const float a)  {WeightedTimeBin[i]=a; return;}
  ///
  void setLikelihood(const float a) {hitLikelihood = a; return;}

  ///
  void Initialize(float xyzin[3], float xyzout[3]);

protected:

  ///  (0-4) from the botton sector
  int   Sector;
  /// 0 = south, 1 = north
  int   Side;
  /// PHENIX $\phi$ coordinate on TEC reference circle (450cm)
  float Phi;
  /// angle between track and $\phi$ angle. Alpha = C/momentum
  float Alpha;
  /// Momentum calculated from Tec Alpha angle
  float tecMomentum;
  /// N. wires fired by the track in each plane
  int   Nwires[6];
  /// N. Hits per plane
  int   NHITS[6];
  /// N. TR hits (adc > 4 and timebin > 30) per plane
  int   NTR[6];
  /// N. timebin used in 60% truncation per plane
  int   NdEdXbins[6];
  /// 60 % truncated dE/dX per plane
  float dEdX06[6];
  /// Track Length amoung actived planes
  float TrackLength;
  /// N. hits with charge > 100 (TR electronic range) per plane
  int   Nhits100[6]; 
  /// N. hits with charge > 20 per plane
  int   Nhits20[6]; 
  /// Total charge per plane
  float DE[6];
  /// Total charge amoung TR hits (adc > 4 and timebin > 30) per plane
  float TR[6];
  /// Total weigted time bin charge $wt=\frac{hitcharge*timebin2}}{\sum timebin}$ per plane
  float WeightedTimeBin[6];
  /// Electron likelihood = $log\frac{probability_{electron}}{probability_{other particle}}$
  float hitLikelihood;

  ///
  ClassDef(TecTrackTRv3,1)
};

#endif /* TECTRACKTRV2_H */                                                         

