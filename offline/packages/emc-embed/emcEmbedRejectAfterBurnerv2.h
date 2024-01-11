#ifndef __emcEmbedRejectAfterBurnerv2_h__
#define __emcEmbedRejectAfterBurnerv2_h__

#include "emcEmbedRejectAfterBurnerv1.h"

class emcEmbedRejectAfterBurnerv2 : public emcEmbedRejectAfterBurnerv1
{
public:

  emcEmbedRejectAfterBurnerv2(){}
  virtual ~emcEmbedRejectAfterBurnerv2(){}

  void identify(std::ostream& os=std::cout) const;

  void apply(PHCompositeNode *dstnode);

private:

  ClassDef(emcEmbedRejectAfterBurnerv2,1) // cluster based reject afterburner
};

#endif
