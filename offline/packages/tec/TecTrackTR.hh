#ifndef TECTRACKTR_H
#define TECTRACKTR_H 

#include "PHObject.h"

class PHCompositeNode;

/** Base class for a single Tec track with added TR stuff */
class TecTrackTR : public PHObject {
 
public:
 
  TecTrackTR() {}                                                                  
//  TecTrackTR(float xyzin[3], float xyzout[3]) {}
//  TecTrackTR(const TecTrack &source) {}
  virtual ~TecTrackTR() {}


  virtual int   getIndex()  	const { virtual_warning(" Index "); 	return -999; }
  virtual int   getSector()  	const { virtual_warning(" Sector "); 	return -999; }
  virtual int   getSide()  	const { virtual_warning(" Side "); 	return -999; }
  virtual float getXin() 	const { virtual_warning(" Xin "); 	return -999.; } 
  virtual float getYin()  	const { virtual_warning(" Yin "); 	return -999.; }
  virtual float getZin()  	const { virtual_warning(" Zin "); 	return -999.; }
  virtual float getXinError() 	const { virtual_warning(" XinError "); 	return -999.; }
  virtual float getYinError() 	const { virtual_warning(" YinError "); 	return -999.; }
  virtual float getZinError() 	const { virtual_warning(" ZinError "); 	return -999.; }
  virtual float getXout()  	const { virtual_warning(" Xout "); 	return -999.; }
  virtual float getYout()  	const { virtual_warning(" Xout "); 	return -999.; }
  virtual float getZout()  	const { virtual_warning(" Xout "); 	return -999.; }
  virtual float getXoutError() 	const { virtual_warning(" XoutError "); return -999.; }
  virtual float getYoutError() 	const { virtual_warning(" XoutError "); return -999.; }
  virtual float getZoutError() 	const { virtual_warning(" XoutError "); return -999.; }
  virtual float getPhi()  	const { virtual_warning(" Phi "); 	return -999.; }
  virtual float getChi2()  	const { virtual_warning(" Chi2 "); 	return -999.; }
  virtual float getAlpha()  	const { virtual_warning(" Alpha "); 	return -999.; }
  virtual float getPhiAtDch()  	const { virtual_warning(" PhiAtDch "); 	return -999.; }
  virtual float getAlphaAtDch() const { virtual_warning(" AlphaAtDch "); return -999.; }
  virtual float getSlope()  	const { virtual_warning(" Slope "); 	return -999.; }
  virtual float getIntercept()  const { virtual_warning(" Intercept "); return -999.; }
  virtual int   getNhits()  	const { virtual_warning(" Nhits "); 	return -999; }
  virtual int   getNhits(int)      const { virtual_warning(" Nhits[] ");     return -999; }
  virtual int   getNtr()  	const { virtual_warning(" Ntr "); 	return -999; }
  virtual float gettecMomentum() const { virtual_warning(" tecMomentum "); return -999.; }
  virtual int   gettecCharge()  const { virtual_warning(" tecCharge "); return -999; }
  virtual int   getNwires(int)  const { virtual_warning(" Nwires "); 	return -999; }
  virtual int   getNHITS(int)  	const { virtual_warning(" NHITS "); 	return -999; }
  virtual int   getNTR(int)  	const { virtual_warning(" NTR "); 	return -999; }
  virtual int   getNFplanes() 	const { virtual_warning(" NFplanes "); 	return -999; }
  virtual int   getNFwires() 	const { virtual_warning(" NFwires "); 	return -999; }
  virtual int   getPc3Pointer() const { virtual_warning(" Pc3Pointer "); return -999; }
  virtual float getPc3Distance() const { virtual_warning(" Pc3Distance "); return 999.; }
  virtual float getPc3Z()       const { virtual_warning(" Pc3Z "); return 999.; }
  virtual int   getPc3sPointer() const { virtual_warning(" Pc3sPointer "); return -999; }
  virtual float getPc3sDistance() const { virtual_warning(" Pc3sDistance "); return 999.; }
  virtual int   getEmcPointer() const { virtual_warning(" EmcPointer "); return -999; }
  virtual float getEmcDistance() const { virtual_warning(" EmcDistance "); return 999.; }
  virtual float getEmcZ()       const { virtual_warning(" EmcZ "); return 999.; }
  virtual float getEmcEcore()       const { virtual_warning(" EmcEcore "); return 999.; }
  virtual int   getEmcsPointer() const { virtual_warning(" EmcsPointer "); return -999; }
  virtual float getEmcsDistance() const { virtual_warning(" EmcsDistance "); return 999.; }
  virtual float getEmcsEcore()       const { virtual_warning(" EmcsEcore "); return 999.; }
  virtual float getCrkNpe0()       const { virtual_warning(" CrkNpe0 "); return 999.; }
  virtual int   getCrkN0() const { virtual_warning(" CrkN0 "); return -999; }
  virtual float getCrkChi2() const { virtual_warning(" CrkChi2 "); return 999.; }
  virtual float getCrksNpe0()       const { virtual_warning(" CrksNpe0 "); return 999.; }
  virtual float getCrksChi2()       const { virtual_warning(" CrksChi2 "); return 999.; }
  virtual int   getCrksN0() const { virtual_warning(" CrksN0 "); return -999; }
  virtual float getdEdX() 	const { virtual_warning(" dEdX "); 	return -999.; }
  virtual float getTr() 	const { virtual_warning(" Tr "); 	return -999.; }
  virtual int   getNdEdXbins() 	const { virtual_warning(" NdEdXbins ");	return -999; }
  virtual float getdEdX06() 	const { virtual_warning(" dEdX06 "); 	return -999.; }
  virtual int   getNdEdXbins(int) 	const { virtual_warning(" NdEdXbins[] ");	return -999; }
  virtual float getdEdX06(int) 	const { virtual_warning(" dEdX06[] "); 	return -999.; }
  virtual float getTrackLength() const { virtual_warning(" TrackLength "); return -999.; }
  virtual int   getNhits100()  	const { virtual_warning(" Nhits100 "); 	return -999; }
  virtual int   getNhits200()  	const { virtual_warning(" Nhits200 "); 	return -999; }
  virtual int   getNhits50()  	const { virtual_warning(" Nhits50 "); 	return -999; }
  virtual int   getNhits100(int)  	const { virtual_warning(" Nhits100[] "); 	return -999; }
  virtual int   getNhits20(int)  	const { virtual_warning(" Nhits20[] "); 	return -999; }
  virtual float getDE(int)  	const { virtual_warning(" DE "); 	return -999; }
  virtual float getDE()  	const { virtual_warning(" DE "); 	return -999; }
  virtual float getTR(int)  	const { virtual_warning(" TR "); 	return -999; }
  virtual float getWeightedTimeBin() const { virtual_warning(" WeightedTimeBin "); return -999.; }
  virtual float getWeightedTimeBin(int) const { virtual_warning(" WeightedTimeBin[] "); return -999.; }
  virtual float getWeightedTimeBinSq() const { virtual_warning(" WeightedTimeBinSq "); return -999.; }
  virtual float getLikelihood() const { virtual_warning(" hitLikelihood "); return -999.; }

  virtual void setXin(const float) 		{return;} 
  virtual void setYin(const float) 		{return;}
  virtual void setZin(const float)  		{return;}
  virtual void setXout(const float)  		{return;}
  virtual void setYout(const float)  		{return;}
  virtual void setZout(const float)  		{return;}
  virtual void setNhits(const int)  		{return;}
  virtual void setNtr(const int)  		{return;}
  virtual void setIndex(const int)  		{return;}
  virtual void setSector(const int)  		{return;}
  virtual void setSide(const int)  		{return;}
  virtual void setChi2(const float)  		{return;}
  virtual void setAlpha(const float)  		{return;}
  virtual void setPhi(const float)  		{return;}
  virtual void setNwires(const int, const int) 	{return;}
  virtual void setNHITS(const int, const int)  	{return;}
  virtual void setNTR(const int, const int)  	{return;}
  virtual void setdEdX(const float)  		{return;}
  virtual void setTr(const float)  		{return;}
  virtual void setNdEdXbins(const int)          {return;}
  virtual void setdEdX06(const float)  		{return;}
  virtual void setNdEdXbins(const int, const int){return;}
  virtual void setdEdX06(const int, const float){return;}
  virtual void setTrackLength(const float)  	{return;}
  virtual void setNhits100(const int)  		{return;}
  virtual void setNhits200(const int)  		{return;}
  virtual void setNhits50(const int)  		{return;}
  virtual void setNhits100(const int, const int)  		{return;}
  virtual void setNhits20(const int, const int)  		{return;}
  virtual void setDE(const int, const float)  	{return;}
  virtual void setTR(const int, const float)  	{return;}
  virtual void setWeightedTimeBin(const float)  {return;}
  virtual void setWeightedTimeBin(const int, const float)  {return;}
  virtual void setWeightedTimeBinSq(const float)  {return;}
  virtual void setLikelihood(const float) {return;}

  virtual void Initialize(float xyzin[3], float xyzout[3]) {return;}
  virtual void project2PC(PHCompositeNode*) {return;}
  virtual void project2InvertedPC(PHCompositeNode*) {return;}
  virtual void project2Emc(PHCompositeNode*) {return;}
  virtual void project2InvertedEmc(PHCompositeNode*) {return;}
  virtual void ShutUp(const int i = 1);
  
protected:

  void virtual_warning(const char *funcname) const;

  ClassDef(TecTrackTR,1)
};

#endif /* TECTRACKTR_H */                                                       
