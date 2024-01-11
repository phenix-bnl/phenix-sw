#include "pdst.h"
#include "Event.h"
#include "PHString.h"
#include "TDataType.h"

int process_event (PHCompositeNode *topNode); //++CINT
int vertex_fit_and_eval(PHCompositeNode* topNode); //++CINT
int setup_all(PHCompositeNode* dst_node); //++CINT
bool setup_display(); //++CINT
int end_all(); //++CINT
int dstout_fopen(PHString dstout_filename); //++CINT;
void draw_plane(UShort_t arm, UShort_t octant); //++CINT
void draw_octant(UShort_t arm, int octant=-1, int station=-1); //++CINT
void draw_side(UShort_t arm, int octant=-1, int station=-1); //++CINT
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
void do_display(); //++CINT

#ifndef __CINT__  

// MUTOO includes
//
#include<TMutNode.h>

// IOC includes
//
#include<TMutHitMap.h>
#include<TMutClusMap.h>
#include<TMutCoordMap.h>
#include<TMutGapCoordMap.h>
#include<TMutStubMap.h>
#include<TMutTrkMap.h>
#include<TMutMCHitMap.h>
#include<TMutMCTrkMap.h>

// Module includes
//
#include<mMutUnpackMutDst.h>
#include<mMutUnpackMutDstPar.h>
#include<mMutFindClus.h>
#include<mMutFindClusPar.h>
#include<mMutFitClus.h>
#include<mMutFitClusPar.h>
#include<mMutFindGapCoord.h>
#include<mMutFindGapCoordPar.h>
#include<mMutFindStub.h>
#include<mMutFindStubPar.h>
#include<mMutStubFit.h>
#include<mMutStubFitPar.h>
#include<mMutStartTracks.h>
#include<mMutStartTracksPar.h>
#include<mMutKalFit.h>
#include<mMutKalFitPar.h>
#include<mMutFindTrackMC.h>
#include<mMutFindTrackMCPar.h>
#include<mMutFindTrack.h>
#include<mMutFindTrackPar.h>
#include<mMutTrackFit.h>
#include<mMutTrackFitPar.h>
#include<mMutFindVtx.h>
#include<mMutFindVtxPar.h>
#include<mMutFitVtx.h>
#include<mMutFitVtxPar.h>
#include<mMutEvalFramework.h>
#include<mMutEvalFrameworkPar.h>
#include<mMutResponse.h>
#include<mMutResponsePar.h>
#include<mMutEval.h>
#include<PhMutooDisplay.h>

// Old framework module includes
//
#include<mMutdbInit.hh>
#include<mMutZeroSuptoRawData.hh>
#include<mumdigiparWrapper.h>
#include<mumgeoWrapper.h>
#include<mMutCathodePulseParams.hh>
#include<mMutApplyCalib.hh>
#include<mMutFindCathodeClustsT.hh>
#include<mMutFitCathodeClustsTModule.h>
#include<dMutHistosWrapper.h>

#endif // __CINT__





