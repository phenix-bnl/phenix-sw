#ifndef __emcOFTowerContainer_h__
#define __emcOFTowerContainer_h__

#include "emcObjectFiller.h"

/** (OLD?) Fills emcTowerContainerv1 from dEmcCalibTower. 
    @ingroup deprecated
 */

class emcOFTowerContainer : public emcObjectFiller
{
public:

  emcOFTowerContainer();
  virtual ~emcOFTowerContainer();

  bool canFill(PHObject& destination, int verbose=0) const;

  bool canFill(PHCompositeNode*, 
	       PHObject& destination,
	       int verbose=0) const;

  bool fill(PHCompositeNode* topNode, 
	    PHObject& destination,
	    int verbose=0);
  
  void identify(std::ostream& os = std::cout) const;

  int isValid() const;

  void Reset();
};

#endif
