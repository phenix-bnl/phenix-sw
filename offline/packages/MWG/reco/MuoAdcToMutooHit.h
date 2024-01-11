#ifndef __MUOADCTOMUTOOHIT__
#define __MUOADCTOMUTOOHIT__

#include <MuonSubsysReco.h>

class PHMuoTracksOut;
class PHMuoTracksAdc;
class TMutHitMap;
class mMutCalibrate;

class MuoAdcToMutooHit : public MuonSubsysReco{
 public:
  MuoAdcToMutooHit(const char *name="MuoAdcToMutooHit");
  virtual ~MuoAdcToMutooHit(void);

  int InitRun(PHCompositeNode *top_node);
  int process_event(PHCompositeNode *top_node);

  int CreateNodeTree(PHCompositeNode *top_node);
  int FillHitAdc(void);
  int GetArmOct(double xpos,double ypos,double zpos,int &arm,int &oct,int &hoct);

  static const int MAX_GAP[3];

 protected:
  PHMuoTracksOut *muo_trks;
  PHMuoTracksAdc *muo_adcs;
  TMutHitMap *mut_hitmap;

  mMutCalibrate *mut_calib_mod;
};

#endif /* __MUOADCTOMUTOOHIT__ */
