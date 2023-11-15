#ifndef _PISAHISTO_
#define _PISAHISTO__

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// PISAHistogramManager                                                 //
//                                                                      //
// Description of the PISA histogram manager                            //
// Used for all subsystems                                              //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TObject.h"
#include "TClonesArray.h"
#include "TH1.h"



#include "PISAEvent.h"

class TDirectory;


class PISAHistogramManager {

 private:
  TH1F  *fBbcNhit;
  TH1F  *fBbcdel;
   
 public:
  PISAHistogramManager(TDirectory *dir);
  virtual ~PISAHistogramManager();
  void PISAHfill(PISAEvent *PISAevent);

  ClassDef(PISAHistogramManager,1) // Manages PISA histograms

};

#endif
