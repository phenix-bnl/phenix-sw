#ifndef TECTRACKV2_H
#define TECTRACKV2_H 

#include "TecBasicObject.hh"
#include "TecTrackTR.hh"
#include "TecTrack.hh"

class PHCompositeNode;
class CrkGeometryObject;
/** Represents a single Tec track with added TR stuff */
class TecTrackV2 : public TecTrackTR {
 
 public:
 
  TecTrackV2();                                                                   
  TecTrackV2(float xyzin[3], float xyzout[3]);
  TecTrackV2(const TecTrack &source);
  TecTrackV2(const TecTrackV2 &source);
  TecTrackV2& operator=(const TecTrack &);
  virtual ~TecTrackV2() { }

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
  float gettecMomentum() const { return tecMomentum; }
  int   gettecCharge()  const { return tecCharge; }
  int   getNwires(int i)  const { return Nwires[i]; }
  int   getNHITS(int i) const { return NHITS[i]; }
  int   getNFplanes() 	const;
  int   getNFwires() 	const; 
  int   getPc3Pointer() const { return Pc3Pointer; }
  float getPc3Distance() const { return Pc3Distance; }
  float getPc3Z() const { return Pc3Z; }
  int   getPc3sPointer() const { return Pc3sPointer; }
  float getPc3sDistance() const { return Pc3sDistance; }
  int   getPc1Pointer() const { return Pc1Pointer; }
  float getPc1Distance() const { return Pc1Distance; }
  float getPc1Z() const { return Pc1Z; }
  int   getPc1sPointer() const { return Pc1sPointer; }
  float getPc1sDistance() const { return Pc1sDistance; }
  int   getEmcPointer() const { return EmcPointer; }
  float getEmcDistance() const { return EmcDistance; }
  float getEmcZ() const { return EmcZ; }
  float getEmcEcore() const { return EmcEcore; }
  int   getEmcsPointer() const { return EmcsPointer; }
  float getEmcsDistance() const { return EmcsDistance; }
  float getEmcsEcore() const { return EmcsEcore; }
  int   getCrkN0() const { return CrkN0; }
  float getCrkNpe0() const { return CrkNpe0; }
  float getCrkChi2() const { return CrkChi2; }
  int   getCrksN0() const { return CrksN0; }
  float getCrksNpe0() const { return CrksNpe0; }
  float getCrksChi2() const { return CrksChi2; }
  float getCrkDist() const { return CrkDist; }
  float getCrksDist() const { return CrksDist; }
  int   getNdEdXbins(int i) 	const { return NdEdXbins[i]; }
  float getdEdX06(int i) 	const { return dEdX06[i]; }
  float getTrackLength() const { return TrackLength; }
  int   getNhits100(int i)  	const { return Nhits100[i]; }
  int   getNhits20(int i)  	const { return Nhits20[i]; }
  int   getNhits()       const { return Nhits; }
  int   getNTR(int i)        const { return NTR[i]; }
  float getChi2()    const { return Chi2; }
  float getDE(int i)    const { return DE[i]; }
  float getTR(int i)    const { return TR[i]; }
  float getWeightedTimeBin(int i) const { return WeightedTimeBin[i]; }
  float getLikelihood() const { return hitLikelihood;}

  void setSector(const int a)  		{Sector=a; return;}
  void setSide(const int a)  		{Side=a; return;}
  void setXin(const float a) 		{XYZin[0]=a;  return;} 
  void setYin(const float a) 		{XYZin[1]=a;  return;}
  void setZin(const float a)  		{XYZin[2]=a;  return;}
  void setXout(const float a)  		{XYZout[0]=a; return;}
  void setYout(const float a)  		{XYZout[1]=a; return;}
  void setZout(const float a)  		{XYZout[2]=a; return;}
  void setNwires(const int i, const int a){Nwires[i]=a; return;}
	void setNhits(const int i)  {Nhits = i; return;}
  void setNHITS(const int i, const int a) {NHITS[i]=a; return;}
  void setNdEdXbins(const int i, const int a)  	{NdEdXbins[i]=a; return;}
  void setdEdX06(const int i, const float a)  	{dEdX06[i]=a; return;}
  void setTrackLength(const float a)  	{TrackLength=a; return;}
  void setNhits100(const int i, const int a)  	{Nhits100[i]=a; return;}
  void setNhits20(const int i, const int a)  	{Nhits20[i]=a; return;}
  void setNTR(const int i, const int a) {NTR[i]=a; return;}
  void setDE(const int i, const float a)  {DE[i]=a; return;}
  void setTR(const int i, const float a)  {TR[i]=a; return;}
  void setWeightedTimeBin(const int i, const float a)  {WeightedTimeBin[i]=a; return;}
  void setLikelihood(const float a) {hitLikelihood = a; return;}

  void Initialize(float xyzin[3], float xyzout[3]);
  void project2PC(PHCompositeNode*);
  void project2ZinvertedPC(PHCompositeNode*);
  void project2EMC(PHCompositeNode*);
  void project2ZinvertedEMC(PHCompositeNode*);
  void project2Crk(PHCompositeNode*, CrkGeometryObject* cgo, float pmt_dist_cut = 20.);
  void project2ZinvertedCrk(PHCompositeNode*, CrkGeometryObject* cgo, float pmt_dist_cut = 20.);

protected:

  int   Sector;
  int   Side;
  float XYZin[3];
  float XYZout[3];
  float XYZinError[3];
  float XYZoutError[3];
  float Chi2;
  float Phi;
  float Alpha;
  float PhiAtDch;
  float AlphaAtDch;
  float Slope;
  float Intercept;
  float tecMomentum;
  int   tecCharge;
  int   Nwires[6];
  int   Nhits;
  int   NHITS[6];
  int   NTR[6];
  int   Pc3Pointer;
  float Pc3Distance;
  int   Pc3sPointer;
  float Pc3sDistance;
  float Pc3Z;
  int   Pc1Pointer;
  float Pc1Distance;
  int   Pc1sPointer;
  float Pc1sDistance;
  float Pc1Z;
  int   EmcPointer;
  float EmcDistance;
  float EmcZ;
  float EmcEcore;
  int   EmcsPointer;
  float EmcsDistance;
  float EmcsEcore;
  float CrkNpe0;
  int   CrkN0;
  float CrkChi2;
  float CrksNpe0;
  int   CrksN0;
  float CrksChi2;
  float CrkDist;
  float CrksDist;
  int   NdEdXbins[6];
  float dEdX06[6];
  float TrackLength;
  int   Nhits100[6]; 
  int   Nhits20[6]; 
  float DE[6];
  float TR[6];
  float WeightedTimeBin[6];
  float hitLikelihood;

  void calculateXinError();
  void calculateYinError();

  ClassDef(TecTrackV2,1)
};

#endif /* TECTRACKTRV2_H */                                                         

