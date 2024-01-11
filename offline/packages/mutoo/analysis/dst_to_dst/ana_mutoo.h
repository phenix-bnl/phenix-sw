#include "pdst.h"
#include "Event.h"
#include "TDataType.h"
#include "pdst.h"
#include <set>
#include "PHString.h"

#ifndef __CINT__  
// Module includes
//
#include<mMutUnpackMutDst.h>
#include<mMutMuiRoad.h>
#include<mMutFindClus.h>
#include<mMutFitClus.h>
#include<mMutFitClusPar.h>
#include<mMutFindGapCoord.h>
#include<mMutFindStub.h>
#include<mMutStubFit.h>
#include<mMutStubFitPar.h>
#include<mMutStartTracks.h>
#include<mMutFindTrack.h>
#include<mMutTrackFit.h>
#include<mMutKalFit.h>
#include<mMutKalVtx.h>
#include<mMutRejectTrack.h>
#include<mMutFindVtx.h>
#include<mMutFindVtxPar.h>
#include<mMutFitVtx.h>
#include<mMutFitVtxPar.h>
#include<mMutEvalFramework.h>
#include<PhMutooDisplay.h>
#endif

int dinit(); //++CINT
int process_event (PHCompositeNode* topNode); //++CINT
bool setup_display(); //++CINT
int setup_all(PHCompositeNode* topNode); //++CINT
int end_all(); //++CINT
void fill_ntuple(int event, PHCompositeNode* topNode); //++CINT
void fill_windows_ntuple(); //++CINT
void draw_plane(int arm=-1, int octant=-1); //++CINT
void draw_octant(int arm=-1, int octant=-1, int station=-1); //++CINT
void draw_side(int arm=-1, int octant=-1, int station=-1); //++CINT
int dstout_fopen(PHString dstout_filename, PHString ndstout_filename); //++CINT;
void dump_hit(); //++CINT
void dump_clus(); //++CINT
void dump_coord(); //++CINT
void dump_gap_coord(); //++CINT
void dump_stub(); //++CINT
void dump_trk(); //++CINT
void dump_vtx(); //++CINT
void dump_mc_trk(); //++CINT
void dump_mc_hit(); //++CINT
void dump_eval(); //++CINT
void dump_road(); //++CINT
void do_display(); //++CINT
bool keep_event(int event_number);
void initialize_keep_event();
 















