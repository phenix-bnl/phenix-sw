#ifndef __BEAMSHIFTRECO_H__
#define __BEAMSHIFTRECO_H__

#include "SubsysReco.h"

//
// This is a cut and past from MomentumScaleReco and
// MomChangeRecalReco.
// It does all the changes in DCH track in order to account
// the beam shift poosition. You should just set the delta_x and delta_y
// value obtained from, i.e. ,zero field runs
// Enjoy, 
//                      Cesar - slash@bnl.gov - Mar/08/2006

class BeamShiftReco: public SubsysReco
{
 public:
  BeamShiftReco(float delta_x = 0.000, float delta_y = 0.000);
  virtual ~BeamShiftReco() {}

  int Init(PHCompositeNode *topNode) {return 0;}
  int InitRun(PHCompositeNode *topNode) {return 0;}
  int process_event(PHCompositeNode *topNode);
  int Reset(PHCompositeNode *topNode) {return 0;}
  void Print(const std::string&) const {}

 protected:
  float XOffset, YOffset;
  float new_alpha(float alpha, float phi);
  float delta_phi0(float del_alpha);
};

#endif /* __BEAMSHIFTRECO_H__ */
