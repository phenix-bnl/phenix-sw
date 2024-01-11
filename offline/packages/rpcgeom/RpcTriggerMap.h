#ifndef __RpcTriggerMap_H__
#define __RpcTriggerMap_H__

#include <RpcTriggerMap.h>
#include <PHPoint.h>
#include <TObject.h>

class PHPoint;
class RpcStrip_v2;

class RpcTriggerMap : public TObject
{

 public: 

  //constructor
  RpcTriggerMap(){ InitVal();}

  //destructor
  virtual ~RpcTriggerMap();
 
  void InitVal(); 
  //Set the reference positions for the strip finder
  // pos is 0/1 for the begin/end position of the MuTrg
  void setReferencePosition(int pos,
			    float fXbeg, float fYbeg, float fZbeg,
			    float fXend, float fYend, float fZend);
  void setReferencePosition(int pos, PHPoint pbeg, PHPoint pend);

  //Using the reference positions, find the nearest strip on the RPC plane
  void findNearestStrips(int fArm, int fStation, int fOctant);
  //check the boundary, move strips from edge, flag this was on the edge
  bool checkBoundary();
  bool checkBoundary2Strip();
  RpcStrip_v2 *getStrip(int fRadSeg);
  RpcStrip_v2 *getStripn(int fRadSeg);
  void setTDCChannels();
  
  //These functions are only to be used for checking the map ...
  void set_strip(int arm, int sta, int oct, int hoct, int rad0, int rad1, int rad2);
  void set_tdc(int arm, int sta, int oct,
	       int mod0, int mod1, int mod2,
	       int tdc0, int tdc1, int tdc2);
  void setGeom();
  void testGeomTDCMapping();
  
 private:
  
  PHPoint fRef0beg;
  PHPoint fRef0end;
  PHPoint fRef1beg;
  PHPoint fRef1end;
  
  RpcStrip_v2 *strip;  //Best match strip
  RpcStrip_v2 *stripn; //Next nearest strip

 public:
  
  //Best match strip
  int fRad0;//strip format
  int fRad1;
  int fRad2;
  int fTDCCh0;//TDC format
  int fTDCCh1;
  int fTDCCh2;

  //Next-nearest information
  int fRad0n;//strip format
  int fRad1n;
  int fRad2n;
  int fTDCCh0n;//TDC format
  int fTDCCh1n;
  int fTDCCh2n;

  //Common information between best and next-nearest strips
  int fCurrentStation;//strip format
  int fCurrentArm;
  int fCurrentOctant;
  int fCurrentHalfOctant;
  int fCurrentModule0;//TDC format
  int fCurrentModule1;
  int fCurrentModule2;

  ClassDef(RpcTriggerMap,1)
};

#endif /* __RpcTriggerMap_H__*/
