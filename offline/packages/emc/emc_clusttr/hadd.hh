#ifndef _hadd_hh_
#define _hadd_hh_

#ifdef COMPILE

#include <Rtypes.h>
#include <TObject.h>
#include <TDirectory.h>
#include <TFile.h>
#include <TList.h>
#include <TFile.h>
#include <TH1.h>
#include <TObject.h>
#include <TKey.h>
#include <TTree.h>
#endif

int AddRecursive(TDirectory* root0,TDirectory *root1,TDirectory* outdir);


#endif
//
