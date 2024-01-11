//==================================================================
#ifndef _fitpeak_hh_
#define _fitpeak_hh_
//==================================================================

#ifdef COMPILE
#include <cstdlib>
#include <stdio.h>
#include <unistd.h>
#include <cstring>
#include <iostream.h>
#include <cmath>
#include <Rtypes.h>
#include <TSystem.h>
#include <TROOT.h>
#include <TFile.h>
#include <TKey.h>
#include <TTree.h>
#include <TBranch.h>
#include <TNtuple.h>
#include <TCanvas.h>
#include <TPad.h>
#include <TStyle.h>
#include <TPostScript.h>
#include <TH1.h>
#include <TH2.h>
#include <TF1.h>
#include <fstream.h>
#include <vector>
#endif

TF1* fitpeak(TH1* h,float min,float max,float width=1.0,char* tail="_mip",char* fitopt="",float upwidth=0);

#endif
//==================================================================

