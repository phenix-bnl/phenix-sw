#ifndef __emcObjectFiller_h__
#define __emcObjectFiller_h__

#include "PHObject.h"

class PHCompositeNode;

/** (OLD?) ABC of a PHObject Filler. 

\deprecated The Object Filler scheme was developped at the time of the "multi-pass" dataproduction (PRDF to DST, DST to microDST, microDST to nanoDST). It never was fully finalized before we switched to Fun4All, and as such might be considered as deprecated, even though I could imagine some use cases for it, still.

Anyway, some old doc is at http://www.phenix.bnl.gov/~aphecetc/Computing/TODO/objectFillers.html

@ingroup deprecated

*/

class emcObjectFiller : public PHObject
{

public:

  emcObjectFiller();
  virtual ~emcObjectFiller() {}

  /** Method to advertise if we're *in principle* able to
      fill destination object (basically a check on destination
      real class). */
  virtual bool canFill(PHObject& destination,
		       int verbose=0) const = 0;

  /** Method to advertise if we really can fill the destination
      object from the topnode given. */
  virtual bool canFill(PHCompositeNode*, 
		       PHObject& destination,
		       int verbose=0) const
  { return canFill(destination); }

  /** Actually do the job. */
  virtual bool fill(PHCompositeNode* topNode, 
		    PHObject& destination,
		    int verbose=0) = 0;

private:

  void registerMe(void);

  ClassDef(emcObjectFiller,0)
};

#endif

