#ifndef TECTRACKTRV2_H
#define TECTRACKTRV2_H 

#include "TecBasicObject.hh"
#include "TecTrackTR.hh"
#include "TecTrack.hh"

class PHCompositeNode;

/** Represents a single Tec track with added TR stuff */
class TecTrackTRv2 : public TecTrackTR {
 
 public:
 
  TecTrackTRv2();                                                                   
  TecTrackTRv2(float xyzin[3], float xyzout[3]);
  TecTrackTRv2(const TecTrack &source);
  TecTrackTRv2& operator=(const TecTrackTRv2 &);
  virtual ~TecTrackTRv2() { }

  int   getIndex()  	const { return Sector*TECMAXSIDE+Side; }
  int   getSector()  	const { return Sector; }
  int   getSide()  	const { return Side; }
  float getXin() 	const { return XYZin[0]; } 
  float getYin()  	const { return XYZin[1]; }
  float getZin()  	const { return XYZin[2]; }
  float getXinError() 	const { return XYZinError[0]; }
  float getYinError() 	const { return XYZinError[1]; }
  float getZinError() 	const { return XYZinError[2]; }
  float getXout()  	const { return XYZout[0]; }
  float getYout()  	const { return XYZout[1]; }
  float getZout()  	const { return XYZout[2]; }
  float getXoutError() 	const { return XYZoutError[0]; }
  float getYoutError() 	const { return XYZoutError[1]; }
  float getZoutError() 	const { return XYZoutError[2]; }
  float getPhi()  	const { return Phi; }
  float getAlpha()  	const { return Alpha; }
  float getPhiAtDch()  	const { return PhiAtDch; }
  float getAlphaAtDch() const { return AlphaAtDch; }
  float getSlope()  	const { return Slope; }
  float getIntercept()  const { return Intercept; }
  int   getNhits()  	const { return Nhits; }
  float gettecMomentum() const { return tecMomentum; }
  int   gettecCharge()  const { return tecCharge; }
  int   getNwires(int i)  const { return Nwires[i]; }
  int   getNHITS(int i) const { return NHITS[i]; }
  int   getNFplanes() 	const;
  int   getNFwires() 	const; 
  int   getPc3Pointer() const { return Pc3Pointer; }
  float getPc3Distance() const { return Pc3Distance; }
  int   getPc3sPointer() const { return Pc3sPointer; }
  float getPc3sDistance() const { return Pc3sDistance; }
  float getdEdX() 	const { return dEdX; }
  int   getNdEdXbins() 	const { return NdEdXbins; }
  float getdEdX06() 	const { return dEdX06; }
  float getTrackLength() const { return TrackLength; }
  int   getNhits100()  	const { return Nhits100; }
  int   getNhits200()  	const { return Nhits200; }
  int   getNhits50()  	const { return Nhits50; }
  int   getNTR(int i)        const { return NTR[i]; }
  int   getNtr()  	const { return Ntr; }
  float getTr()         const { return Tr; }
  float getDE(int i)    const { return DE[i]; }
  float getTR(int i)    const { return TR[i]; }
  float getWeightedTimeBin() const { return WeightedTimeBin; }
  float getWeightedTimeBinSq() const { return WeightedTimeBinSq; }

  void setSector(const int a)  		{Sector=a; return;}
  void setSide(const int a)  		{Side=a; return;}
  void setXin(const float a) 		{XYZin[0]=a;  return;} 
  void setYin(const float a) 		{XYZin[1]=a;  return;}
  void setZin(const float a)  		{XYZin[2]=a;  return;}
  void setXout(const float a)  		{XYZout[0]=a; return;}
  void setYout(const float a)  		{XYZout[1]=a; return;}
  void setZout(const float a)  		{XYZout[2]=a; return;}
  void setNhits(const int a)  		{Nhits=a; return;}
  void setNwires(const int i, const int a){Nwires[i]=a; return;}
  void setNHITS(const int i, const int a) {NHITS[i]=a; return;}
  void setdEdX(const float a)  		{dEdX=a; return;}
  void setNdEdXbins(const int a)  	{NdEdXbins=a; return;}
  void setdEdX06(const float a)  	{dEdX06=a; return;}
  void setTrackLength(const float a)  	{TrackLength=a; return;}
  void setNhits100(const int a)  	{Nhits100=a; return;}
  void setNhits200(const int a)  	{Nhits200=a; return;}
  void setNhits50(const int a)  	{Nhits50=a; return;}
  void setNTR(const int i, const int a) {NTR[i]=a; return;}
  void setTr(const float a)             {Tr=a; return;}
  void setNtr(const int a)  		{Ntr=a; return;}
  void setDE(const int i, const float a)  {DE[i]=a; return;}
  void setTR(const int i, const float a)  {TR[i]=a; return;}
  void setWeightedTimeBin(const float a)  {WeightedTimeBin=a; return;}
  void setWeightedTimeBinSq(const float a)  {WeightedTimeBinSq=a; return;}

  void Initialize(float xyzin[3], float xyzout[3]);
  void project2PC(PHCompositeNode*);
  void project2InvertedPC(PHCompositeNode*);

protected:

  int   Sector;
  int   Side;
  float XYZin[3];
  float XYZout[3];
  float XYZinError[3];
  float XYZoutError[3];
  float Phi;
  float Alpha;
  float PhiAtDch;
  float AlphaAtDch;
  float Slope;
  float Intercept;
  int   Nhits;
  float tecMomentum;
  int   tecCharge;
  int   Nwires[6];
  int   NHITS[6];
  int   NTR[6];
  int   Pc3Pointer;
  float Pc3Distance;
  int   Pc3sPointer;
  float Pc3sDistance;
  float dEdX;
  int   NdEdXbins;
  float dEdX06;
  float TrackLength;
  int   Nhits100; 
  int   Nhits200; 
  int   Nhits50;
  int   Ntr;
  float Tr;
  float DE[6];
  float TR[6];
  float WeightedTimeBin;
  float WeightedTimeBinSq;

  void calculateXinError();
  void calculateYinError();

  ClassDef(TecTrackTRv2,1)
};

#endif /* TECTRACKTRV2_H */                                                         

