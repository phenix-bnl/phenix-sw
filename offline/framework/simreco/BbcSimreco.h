#ifndef __BBCSIMRECO_H__
#define __BBCSIMRECO_H__

#include <SubsysReco.h>

class PHCompositeNode;

#include<mBbcDCMModule.h>
#include<mBbcGhitRawModule.h>
#include<mBbcFEMModule.h>
#include<mBbcRawOutModule.h>
#include<mBbcSetGeoModule.h>
#include<mBbcSetUcalModule.h>
#include<mBbcUnpackModule.h>
#include<mBbcRawReCal.h>

class BbcSimreco: public SubsysReco
{
 public:
  BbcSimreco(const std::string &name = "BBC");
  virtual ~BbcSimreco(){}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);

  protected:
  
  //!@name modules
  /*! 
  direct reference of the objects is used in place
  of pointer so that they are created in parent object constructor
  and deleted in parent object destructor
  */
  //@{
  mBbcDCMModule mBbcDCM;
  mBbcGhitRawModule mBbcGhitRaw;
  mBbcFEMModule mBbcFEM;
  mBbcRawOutModule mBbcRawOut;
  mBbcSetGeoModule mBbcSetGeo;
  mBbcSetUcalModule mBbcSetUcal;
  mBbcUnpackModule mBbcUnpack;
  mBbcRawReCal mBbcRawReCalib;
  //@}
  
};

#endif /* __BBCSIMRECO_H__ */
