#ifndef __emcOFClusterContainer_h__
#define __emcOFClusterContainer_h__

#include "emcObjectFiller.h"

/** (OLD?) Fills emcClusterContainerv1 from dEmcClsuterLocalExt.
    @ingroup deprecated
*/

class emcOFClusterContainer : public emcObjectFiller
{
public:

  emcOFClusterContainer();
  virtual ~emcOFClusterContainer();

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
