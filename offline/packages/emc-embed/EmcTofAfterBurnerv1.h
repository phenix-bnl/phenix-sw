#ifndef __EMCTOFAFTERBURNERV1_H__
#define __EMCTOFAFTERBURNERV1_H__

#include "PHCompositeNode.h"
#include "PHObject.h"
#include "PHAfterBurner.h"

class EmcTofAfterBurnerv1 : public PHAfterBurner
{
public:
  EmcTofAfterBurnerv1();
  virtual ~EmcTofAfterBurnerv1() {}

  // These are necessary for all PHObjects:
  int  isValid() const { return 1; }
  void identify (std::ostream &os=std::cout) const {
    os << "identify yourself: EmcTofAfterBurnerv1 Object" << std::endl;
  }
  void Reset();
 
  // These are purely virtual and must be overridden
  void initialize(PHCompositeNode *topNode );
  void apply(PHCompositeNode *udstNode);

protected:
  float sectoroffset[2][4];
  float toweroffset[2][4][48][96];
  float towerleastcount[2][4][48][96];
  float towerslew[2][4][48][96];
  float meanBbcT0;
  float hadronCorrFactor[2][4];
 
  ClassDef(EmcTofAfterBurnerv1,1)
};

#endif




