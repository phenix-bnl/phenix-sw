#ifndef __emcEmbedPbGlTofAfterBurnerv2_h__
#define __emcEmbedPbGlTofAfterBurnerv2_h__

#include "PHAfterBurner.h"

class emcEmbedPbGlTofAfterBurnerv2 : public PHAfterBurner
{
public:

  emcEmbedPbGlTofAfterBurnerv2();
  virtual ~emcEmbedPbGlTofAfterBurnerv2();

  void initialize(PHCompositeNode*);

  void identify(ostream& os=cout) const;

  void apply(PHCompositeNode *dstnode);
   
  ClassDef(emcEmbedPbGlTofAfterBurnerv2,1) // PbGl TOF AfterBurner
};

#endif
