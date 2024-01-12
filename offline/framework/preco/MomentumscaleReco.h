#ifndef __MOMENTUMSCALERECO_H__
#define __MOMENTUMSCALERECO_H__

#include "SubsysReco.h"

//
//  Hello Central Track Fan:
//    This is a simple Reco Module whose sole purpose is to apply
//  a momentum scale to all tracks found in the track list.  The actual 
//  scale to be applied is provided in the constructor.
//
//                                                TKH 
//                                                7-24-2003
//

class MomentumscaleReco: public SubsysReco
{
 public:
  MomentumscaleReco(float ScaleFactor = 1.000);
  virtual ~MomentumscaleReco() {}

  int Init(PHCompositeNode *topNode) {return 0;}
  int InitRun(PHCompositeNode *topNode) {return 0;}
  int process_event(PHCompositeNode *topNode);
  int Reset(PHCompositeNode *topNode) {return 0;}
  void Print(const std::string&) const {}

 protected:
  float ScaleFactor;

};

#endif /* __MOMENTUMSCALERECO_H__ */
