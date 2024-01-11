#ifndef __EMCENERGYAFTERBURNERV1_H__
#define __EMCENERGYAFTERBURNERV1_H__

#include "PHAfterBurner.h"

#include <iostream>

class PHCompositeNode;

class EmcEnergyAfterBurnerv1 : public PHAfterBurner
{
public:
  EmcEnergyAfterBurnerv1();
  virtual ~EmcEnergyAfterBurnerv1() {}

  // These are necessary for all PHObjects:
  int  isValid() const { return 1; }
  void identify (std::ostream &os=std::cout) const {
    os << "identify yourself: EmcEnergyAfterBurnerv1 Object" << std::endl;
  }
  void Reset();
 
  // These are purely virtual and must be overridden
  void initialize(PHCompositeNode *topNode );
  void initialize(int runNumber);
  void apply(PHCompositeNode *udstNode);

  // Gets...
  float get_tower_scalefactor (int iarm, int isect, int iy, int iz) const {
    return tower_scalefactor[iarm][isect][iy][iz];
  }

protected:
  float tower_scalefactor[2][4][48][96];

  ClassDef(EmcEnergyAfterBurnerv1,1)
};

#endif

