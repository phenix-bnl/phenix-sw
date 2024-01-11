#ifndef __MTECDECODEMODULE_H__
#define __MTECDECODEMODULE_H__

#include "phool.h"

class PHCompositeNode;
class TecAddressObject;

/** Reads prdf data from a file and fills dTecFemData table.
Detailed documentation: \URL{http://www.rhic.bnl.gov/~lebedev/tec/decode.html}
*/

class mTecDecodeModule
{
public:
///
  mTecDecodeModule();
///
  virtual ~mTecDecodeModule();
///
  PHBoolean event(PHCompositeNode*, TecAddressObject*);
///
  PHBoolean event(PHCompositeNode*);
///
  void set_Verbose(int verbose){Verbose=verbose;}
///
  void set_Write2File(int w2f){Write2File=w2f;}
///
  void set_UseOldFormat(int uof){UseOldFormat=uof;}
///
  void set_UseObjy(int uo){UseObjy=uo;}
///
  void set_FadcCut(int tmp){FadcCut=tmp;}


private:
///
  int Verbose;
///
  int Write2File;
///
  int UseOldFormat;
///
  int UseObjy;
///
  int FadcCut;
};
#endif /*__MTECDECODEMODULE_H__*/
