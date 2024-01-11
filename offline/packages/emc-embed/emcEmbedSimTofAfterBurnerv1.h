#ifndef __emcEmbedSimTofAfterBurnerv1_h__
#define __emcEmbedSimTofAfterBurnerv1_h__

#include "PHAfterBurner.h"

class mEmcGeometryModule;

class emcEmbedSimTofAfterBurnerv1 : public PHAfterBurner
{
public:

  emcEmbedSimTofAfterBurnerv1();
  virtual ~emcEmbedSimTofAfterBurnerv1();

  void initialize(PHCompositeNode*);

  void identify(std::ostream& os=std::cout) const;

  void apply(PHCompositeNode *dstnode);
   
private:

  mEmcGeometryModule* fSimulatedGeometry; //!

  ClassDef(emcEmbedSimTofAfterBurnerv1,1) // Simulated TOF AfterBurner
};

#endif
