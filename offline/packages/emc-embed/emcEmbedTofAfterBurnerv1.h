#ifndef __emcEmbedTofAfterBurnerv1_h__
#define __emcEmbedTofAfterBurnerv1_h__

#include "EmcTofAfterBurnerv1.h"

#include <iostream>

class emcEmbedTofAfterBurnerv1 : public EmcTofAfterBurnerv1
{
public:

  emcEmbedTofAfterBurnerv1(){}
  virtual ~emcEmbedTofAfterBurnerv1(){}

  void identify(std::ostream& os=std::cout) const;

  void apply(PHCompositeNode *dstnode);
   
private:

  float correctTOF(float tof, short iarm, short isect, short iy, short iz, float ecal);

  ClassDef(emcEmbedTofAfterBurnerv1,1)
};

#endif
