///////////////////////////////////////////////////////////////
//   
// to d-calibrate ADC in dCrkRaw for the purpose of lvl2 simulation
//
//   problem report to: xiewei@rcf.rhic.bnl.gov 
//

#ifndef __CrkSimuRawReCal_H__
#define __CrkSimuRawReCal_H__

#define NPMT 5120

#include <phool.h>
#include <PHTimeStamp.h>

class PHCompositeNode;

class CrkSimuRawReCal
{

  private:

    float pedestal[NPMT];
    float gain[NPMT];                  

  private:
 
    void Reset();
    void SetCalibConst(PHTimeStamp time); //.. fill *pedestal/*gain
   
  public:

    CrkSimuRawReCal(){}
    virtual ~CrkSimuRawReCal(){}

    PHBoolean event(PHCompositeNode*);
    void SetCalibConst(int runNumber); 

    float  get_pedestal(int pmt) const {return pedestal[pmt];} 
    float  get_gain(int pmt) const {return gain[pmt];} 

    int RenumberPmt(int pmtin);
};
#endif /*__CrkSimuRawReCal_H__*/

