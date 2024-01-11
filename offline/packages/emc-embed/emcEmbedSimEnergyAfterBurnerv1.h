#ifndef __emcEmbedSimEnergyAfterBurnerv1_h__
#define __emcEmbedSimEnergyAfterBurnerv1_h__

#include "PHAfterBurner.h"

#include <iostream>

class TF1;

class emcEmbedSimEnergyAfterBurnerv1 : public PHAfterBurner
{
public:

  //emcEmbedSimEnergyAfterBurnerv1(){};
  emcEmbedSimEnergyAfterBurnerv1(const double PercentageOfSmearing); //{ fConstantTerm = PercentageOfSmearing; };
  virtual ~emcEmbedSimEnergyAfterBurnerv1();

  void initialize(PHCompositeNode*);

  void identify(std::ostream& os=std::cout) const;

  void apply(PHCompositeNode *dstnode);
   
private:

  TF1* fEnergyResolutionConstantTerm;
  double fConstantTerm;

  ClassDef(emcEmbedSimEnergyAfterBurnerv1,1) // Simulated Energy AfterBurner
};

#endif
