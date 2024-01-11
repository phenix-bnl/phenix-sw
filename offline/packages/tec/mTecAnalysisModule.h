#ifndef __MTECANALYSISMODULE_H__
#define __MTECANALYSISMODULE_H__
#include "phool.h"

class PHCompositeNode;

/** Perform Tec-specific data analysis.
Detailed documentation: not yet ready.
*/

class mTecAnalysisModule
{
public:
///
  mTecAnalysisModule();
///
  virtual ~mTecAnalysisModule(){}
///
  PHBoolean event(PHCompositeNode *);

///
  void findDominantContributor(PHCompositeNode *);
///
  void checkAlignment(PHCompositeNode *);
///
  void setupTecOut(PHCompositeNode *);
///
  void fillTecOut(PHCompositeNode *);
///
  void testTecOut(PHCompositeNode *, int);
///
  void resetTecOut(PHCompositeNode *);

///
  void set_Verbose(const int verbose){Verbose=verbose;}
///
  void set_WriteHistograms(const int wh){WriteHistograms=wh;}

private:
///
  int Verbose;
///
  int WriteHistograms;
};
#endif /*__MTECANALYSISMODULE_H__*/

