#ifndef __SPINEVENTGL1P_H__
#define __SPINEVENTGL1P_H__
#include <PHCompositeNode.h>
#include <PHObject.h>
#include <SpinDataEventOut.h>
#include <SpinEventConstants.h>

#include <iostream>

//=============================================
// Database access of the calibration for GL1p module.
//       Created by H.Torii 2003/Aug/28
//=============================================

class SpinEventGL1p : public TObject{
public:
  SpinEventGL1p(PHCompositeNode* topNode=NULL);
  virtual ~SpinEventGL1p() {}

  // Identify-identify-identify....
  void identify (std::ostream &os=std::cout) const {
    os <<" identify yourself: "<<ClassName()<< std::endl;
  }
 
  // Module-like method
  void initialize(PHCompositeNode *topNode);
  void event(PHCompositeNode *udstNode);

  // Basic method
  void Reset();

protected:
  int _runnum;
  bool _stat_offset_beamx;
  int _offset_beamx;
 
  ClassDef(SpinEventGL1p,1) // Calibration for GL1p
};

#endif




