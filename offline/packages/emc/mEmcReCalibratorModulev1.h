#ifndef __mEmcReCalibratorModulev1_h__
#define __mEmcReCalibratorModulev1_h__

#include <SubsysReco.h>

class emcDataProcessor;
class PHTimeStamp;
class emcBadModules;

class mEmcReCalibratorModulev1 : public SubsysReco
{
public:
  mEmcReCalibratorModulev1(int runnumber, const PHTimeStamp&, 
			 bool constantGains);

  virtual ~mEmcReCalibratorModulev1();
   
  int process_event(PHCompositeNode*);

private:
  emcDataProcessor* fDataProcessor;
  PHTimeStamp* fTimeStamp;
  emcBadModules* fBadModules;
  bool fConstantGains;
  int fRunNumber;
};

#endif
