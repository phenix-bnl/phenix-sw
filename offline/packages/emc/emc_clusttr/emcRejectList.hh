//==================================================================
#ifndef _emcRejectList_hh_
#define _emcRejectList_hh_
//==================================================================

#include <TH1.h>
#include <TH2.h>

#ifdef GLOBAL
int frunstat[100000];
int frejtwr[8][96][48];
TH2F* hRejsect[8];
#else
extern int frunstat[100000];
extern int frejtwr[8][96][48];
extern TH2F* hRejsect[8];
#endif

void emcRejectList(char* dir = "/direct/phenix+u/htorii/local/photon/Calibset1");

#endif
//==================================================================

