#ifndef TCRKMODULE_H_INCLUDED
#define TCRKMODULE_H_INCLUDED

#include "phool.h"
#include "PHTimeStamp.h"
class PHTable;
class PHCompositeNode;

class TCrkProjector;
class CrkDAO;

class TCrkModule {
public:
  TCrkModule();
  static PHBoolean setGeo(PHCompositeNode *top);

  static PHBoolean setCal(PHCompositeNode *top);
  static PHBoolean setCal(PHCompositeNode *top, PHTimeStamp time);
  static PHBoolean setCal(PHCompositeNode *top, int runNumber);
  static PHBoolean setCal(PHCompositeNode *top,
			  const char *adcphe, const char *adcped, const char *t0, const char *tac, const char *slew);
  static void      showCal(PHCompositeNode *top);

  static PHBoolean setUcal(PHCompositeNode *top);

  static PHBoolean ghitRaw(PHCompositeNode *top);
  static PHBoolean rawFem(PHCompositeNode *top);
  static PHBoolean rawFem(PHCompositeNode *top,const char *CrkrawNodeName,const char *CrkFEMNodeName);
  static PHBoolean femDcm(PHCompositeNode *top);
  static PHBoolean femDcm(PHCompositeNode *top,const char *dCrkFEMNodeName,const char *dCrkDCMNodeName);
  static PHBoolean dcmRaw(PHCompositeNode *top);
  // rawFem, femDcm, and dcmRaw will be replaced by 
  // prdfRaw and rawPrdf
  static PHBoolean prdfRaw(PHCompositeNode *top, CrkDAO *dao);
  //
  static PHBoolean rawPrdf(PHCompositeNode *top);

  static PHBoolean rawHit(PHCompositeNode *top);
  static PHBoolean pid(PHCompositeNode *top,
		       TCrkProjector *pj=NULL);

  static void temp(void);
};
#endif



