#ifndef _ROOTANC_
#define _ROOTANC_

#include "TROOT.h"
#include "TFile.h"
#include "TTree.h"
#include "TBranch.h"
#include "TClonesArray.h"
#include "TStopwatch.h"
#include "TDirectory.h"
#include "TRandom.h"
#include "TH1.h"
#include "TH2.h"
#include "TNtuple.h"

#include "PISAEventHeader.h"
#include "root_ptrk.h"



#include <math.h>

void rootAncPad(Int_t ancflag, PISAEventHeader *EventHeader);
void rootAncCrk(Int_t ancflag, PISAEventHeader *EventHeader);
void rootAncTec(Int_t ancflag, PISAEventHeader *EventHeader);
void rootAncTof(Int_t ancflag, PISAEventHeader *EventHeader);
void rootAncSvx(Int_t ancflag, PISAEventHeader *EventHeader);
void rootAncHbd(Int_t ancflag, PISAEventHeader *EventHeader);
//void rootAncNtc(Int_t ancflag, PISAEventHeader *EventHeader);
void rootAncTpc(Int_t ancflag, PISAEventHeader *EventHeader);
void rootAncEmcPad(Int_t ancflag, PISAEventHeader *EventHeader);
void rootAncPri(Int_t ancflag, PISAEventHeader *EventHeader);
void rootAncFcl(Int_t ancflag, PISAEventHeader *EventHeader);
void rootAncMuPC(Int_t ancflag, PISAEventHeader *EventHeader);
void rootAncPythia(Int_t ancflag, PISAEventHeader *EventHeader);

#endif


