#ifndef __TOFSIMRECO_H__
#define __TOFSIMRECO_H__

#include <SubsysReco.h>

#ifndef __CINT__
#include "mTofEvaluateModule.h"
#include "mTofGhitGdigiModule.h"
#include "TofAddressObject.hh"
#include "TofCalibObject.hh"
#include "TofEvent.hh"
#endif

// forward declaration
class PHCompositeNode;
class TofGeometryObject;

class TofSimreco: public SubsysReco
{
 public:
  TofSimreco(const std::string &name = "TOF");
  virtual ~TofSimreco();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);

 protected:
  int copyWrapper(PHCompositeNode *); 
  
  #ifndef __CINT__
  //!@name modules
  /*! 
  direct reference of the objects is used in place
  of pointer so that they are created in parent object constructor
  and deleted in parent object destructor
  */
  //@{
  mTofEvaluateModule mTofEvaluate;
  mTofGhitGdigiModule mTofGhitGdigi;
  TofAddressObject TofAddress;
  TofCalibObject TofCalib;
  TofEvent tofevent;
  //@}
  #endif
  
  TofGeometryObject  * TofGeometry   ;

};

#endif /* __TOFSIMRECO_H__ */
