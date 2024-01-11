#ifndef _hmerge_hh_
#define _hmerge_hh_

#ifdef COMPILE
#include <cstdlib>
#include <stdio.h>
#include <unistd.h>
#include <cstring>
#include <iostream.h>
#include <fstream.h>
#include <cmath>
#include <Rtypes.h>
#include <TObject.h>
#include <TDirectory.h>
#include <TFile.h>
#include <TList.h>
#include <TH1.h>
#include <TObject.h>
#include <TKey.h>
#include <TTree.h>
#include <TSystem.h>
#endif

#include "hadd.hh"

int hmerge(char* filename = "chain_info.txt",char* outname = "hmerge.root");


#endif
//
