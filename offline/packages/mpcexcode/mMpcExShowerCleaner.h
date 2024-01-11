#ifndef __MMPCEXSHOWERCLEANER_H__
#define __MMPCEXSHOWERCLEANER_H__

/**
 * @class  mMpcExShowerCleaner
 * @author ngrau@augie.edu 
 * @date   November 2017
 * @brief  Take the raw shower container and clean 
 *         it up by energy-ordering the towers and 
 *         then removing other showers whose central 
 *         tower is within the 3x3 MPC towers of 
 *         all higher energy showers.
 */

#include "SubsysReco.h"
class PHCompositeNode;
class TMpcExShowerContainer;

class mMpcExShowerCleaner : public SubsysReco {

 public:
  mMpcExShowerCleaner();
  virtual ~mMpcExShowerCleaner();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 private:
  TMpcExShowerContainer *_pruned_showers;

};

#endif /* __MMPCEXSHOWERCLEANER_H__ */
