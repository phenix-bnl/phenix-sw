#ifndef __EMC_GEATRACK_SIMMAKER_H__
#define __EMC_GEATRACK_SIMMAKER_H__





#include <Rtypes.h>

#include <SubsysReco.h>
#include <PHCompositeNode.h>
#include <emctypes.h>

class emcGeaTrackContent;
class emcGeaTrackContainer;



class EmcGeaTrackSimMaker: public SubsysReco {

public:
  EmcGeaTrackSimMaker(): SubsysReco("EmcGeaTrackSimMaker") {}
  ~EmcGeaTrackSimMaker(){}


  int InitRun(PHCompositeNode *);
  int process_event(PHCompositeNode *);
  int Reset(PHCompositeNode * root);
  int ResetEvent(PHCompositeNode * root){ return Reset(root); }
  int End(PHCompositeNode * root){ return Reset(root); }


protected:
  emcGeaTrackContent * add_track_recursive(emcGeaTrackContainer * tracks, emc_trkno_t trkno);
  
  ClassDef(EmcGeaTrackSimMaker, 0)
};





#endif /* ! __EMC_GEATRACK_SIMMAKER_H__ */

