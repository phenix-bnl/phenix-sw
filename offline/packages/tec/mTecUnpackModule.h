#ifndef __MTECUNPACKMODULE_H__
#define __MTECUNPACKMODULE_H__

#include "phool.h"

class PHCompositeNode;
class PHTimeStamp;
class TecAddressObject;

/** Reads prdf data from a file and fills dTecFemData table.
Detailed documentation: \URL{http://www.rhic.bnl.gov/~lebedev/tec/unpack.html}
*/

class mTecUnpackModule
{

public:
///
  mTecUnpackModule();
///
  virtual ~mTecUnpackModule() {}
///
  PHBoolean event(PHCompositeNode*, TecAddressObject*);
///
  PHBoolean event(PHCompositeNode*);
///
//  PHBoolean event(PHCompositeNode*);
///
  void set_Verbose(const int verbose){Verbose=verbose;}
///
  void set_FadcCut(const int tmp){FadcCut=tmp;}
///
  void set_FadcCutFromTimeStamp(PHTimeStamp* ts);

private:
///
  int Verbose;
///
  int FadcCut;

};
#endif 
