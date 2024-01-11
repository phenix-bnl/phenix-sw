#ifndef __emcEmbedEnergyAfterBurnerv1_h__
#define __emcEmbedEnergyAfterBurnerv1_h__

#include "EmcEnergyAfterBurnerv1.h"
#include<iostream>

class emcEmbedEnergyAfterBurnerv1 : public EmcEnergyAfterBurnerv1
{
public:

  emcEmbedEnergyAfterBurnerv1(){}
  virtual ~emcEmbedEnergyAfterBurnerv1(){}

  void identify(std::ostream& os=std::cout) const;

  void apply(PHCompositeNode *dstnode);
  
private:

  ClassDef(emcEmbedEnergyAfterBurnerv1,1)
};

#endif
