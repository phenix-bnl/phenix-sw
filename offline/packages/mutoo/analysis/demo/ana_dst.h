// ROOT
//
#include "TDataType.h"
#include "TFile.h"
#include "TH1.h"
#include "TNtuple.h"
#include "TH1.h"

// PHENIX
// 
#include "pdst.h"
#include "Event.h"
#include "TSystem.h"
#include "PhenixRun.hh"
#include "PhCanvas.hh"
#include "PhDchDisplay.hh"
#include "PhPadDisplay.hh"
#include "PhTecDisplay.hh"
#include "PhMuiDisplay.hh"
#include "PhMutDisplay.hh"
#include "PhEventDisplay.hh"

// Shield CINT from anything complicated
//
#ifndef __CINT__

// STL/BOOST
//
#include <iostream>

// MUTOO
//
#include "PhMutooDisplay.h"
#include "TMutNode.h"
#include "TMutMuiRoadMap.h"
#include "TMutHitMap.h"
#include "TMutClusMap.h"
#include "TMutCoordMap.h"
#include "TMutTrkMap.h"
#include "TMutClusMap.h"
#include "TMutMCHitMap.h"

#endif


int process_event (PHCompositeNode *dst_node); //++CINT
void setup_display(); //++CINT
int setup_all(PHCompositeNode *dst_node); //++CINT
int end_all(); //++CINT
void draw_plane(UShort_t arm, UShort_t octant); //++CINT


