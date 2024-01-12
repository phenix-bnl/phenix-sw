#ifndef __TECSIMRECO_H__
#define __TECSIMRECO_H__

#include "SubsysReco.h"

class PHCompositeNode;

#include <mTecCalibModule.h>
#include <mTecUnpackModule.h>
#include <mTecHoughTrackModule.h>
#include <mTecTrackEvalModule.h>
#include <mTecDrawModule.h>
#include <mTecPackModule.h>
#include <mTecSlowSimModule.h>
#include <TecAddressObject.hh>

class TecGeometryObject;
class TecCalibrationObject;



class TecSimreco: public SubsysReco
{
 public:

  //! constructor
  TecSimreco(const std::string &name = "TEC");
  
  //! destructor
  virtual ~TecSimreco();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);
  
  TecGeometryObject* get_TecGeometry() 
  {return TecGeometry;}

  void setTecEvalFlag(int a) 
  {TecEvalFlag=a;}
  
  void setTecDrawFlag(int a) 
  {TecDrawFlag=a;}
  
  protected:
    
  //!@name modules
  /*! 
  direct reference of the objects is used in place
  of pointer so that they are created in parent object constructor
  and deleted in parent object destructor
  */
  //@{
  mTecCalibModule mTecCalib;
  mTecUnpackModule mTecUnpack;
  mTecHoughTrackModule mTecHoughTrack;
  mTecTrackEvalModule mTecTrackEval;
  mTecPackModule mTecPack;
  mTecDrawModule mTecDraw;
  mTecSlowSimModule mTecSlowSim;
  TecAddressObject TecAddress;
  //@}
  
  //! tec calibration object. 
  /*! A pointer is used because it gets written to the node tree */
  TecCalibrationObject* TecCalibration;

  //! tec geometry object. 
  /*! A pointer is used because it gets written to the node tree */
  TecGeometryObject* TecGeometry;
  
  int TecDrawFlag;
  int TecEvalFlag;

};

#endif /* __TECSIMRECO_H__ */
