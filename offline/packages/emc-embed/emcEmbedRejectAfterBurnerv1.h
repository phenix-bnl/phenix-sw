#ifndef __emcEmbedRejectAfterBurnerv1_h__
#define __emcEmbedRejectAfterBurnerv1_h__

#include "EmcRejectAfterBurner.h"

class emcEmbedRejectAfterBurnerv1 : public EmcRejectAfterBurner
{
public:

  emcEmbedRejectAfterBurnerv1(){}
  virtual ~emcEmbedRejectAfterBurnerv1(){}

  void identify(std::ostream& os=std::cout) const;

  void apply(PHCompositeNode *dstnode);

protected:
  
  int computeDeadmap(int hwsector, short zpos, short ypos);

private:

  ClassDef(emcEmbedRejectAfterBurnerv1,1) // Tower based reject after burner
};

#endif
