#ifndef __EMC_GEATOWER_SIMMAKER_H__
#define __EMC_GEATOWER_SIMMAKER_H__





#include <Rtypes.h>

#include <SubsysReco.h>



class EmcGeaTowerSimMaker: public SubsysReco {

public:
  EmcGeaTowerSimMaker(float lowgain = 0.001, float highgain = 0.008);
  ~EmcGeaTowerSimMaker();


public: 
  int InitRun(PHCompositeNode * root);
  int process_event(PHCompositeNode * root);
  int Reset(PHCompositeNode * root);
  int ResetEvent(PHCompositeNode * root){ return Reset(root); }
  int End(PHCompositeNode * root){ return Reset(root); }



protected: /* burnt in constats for raw prdf data generation */
  const float lowgain_convfac;
  const float highgain_convfac;
  const static float tdc_convfac = 0.05;
  const static int   low_ped = 4000;
  const static int   high_ped = 4000;
  const static int   minvalue = 100; 



  ClassDef(EmcGeaTowerSimMaker, 0)
};





#endif /* ! __EMC_GEATOWER_SIMMAKER_H__ */

