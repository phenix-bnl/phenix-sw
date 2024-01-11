#ifndef __MTECPACKMODULE_H__
#define __MTECPACKMODULE_H__

#include "phool.h"

class PHCompositeNode;
class TecAddressObject;

/**
Converts dTecFemData to dTecDcmData DCM output format.
Detailed documentation: \URL{http://www.rhic.bnl.gov/~lebedev/tec/femdcm.html}
*/

class mTecPackModule
{
public:
///
  mTecPackModule();
///
  virtual ~mTecPackModule();
///
  PHBoolean event(PHCompositeNode*);
///
  PHBoolean event(PHCompositeNode*, TecAddressObject*);
///
  void set_Verbose(int verbose){Verbose=verbose;}
///
  void set_Suppress(int suppress){Suppress=suppress;}
///
  void set_UseListSum(int uls){UseListSum=uls;}
///
  void set_FillAllFEM(int fa){FillAllFEM=fa;}
///
  void set_FillTecGhitDcm(int ftgd){FillTecGhitDcm=ftgd;}
///
  void set_FillTecPackets(int i){FillTecPackets=i;}
///
  void set_Write2File(int w2f){Write2File=w2f;}

private:
///
  int Verbose;
///
  int Suppress;
///
  int UseListSum;
///
  int FillAllFEM;
///
  int FillTecGhitDcm;
///
  int FillTecPackets;
///
  int Write2File;
};
#endif /*__MTECPACKMODULE_H__*/
