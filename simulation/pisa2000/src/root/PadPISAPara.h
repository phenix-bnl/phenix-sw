#ifndef _PADPISAPARA_
#define _PADPISAPARA_

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Description of the PAD parameters (event independent)                //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TObject.h"
#include "TClonesArray.h"


class PadPISAPara : public TObject {
private:
  int idatePC23;

public:
  PadPISAPara() {};
  virtual ~PadPISAPara() {};

   ClassDef(PadPISAPara,1)  // A Pad parameter instance
};

#endif
