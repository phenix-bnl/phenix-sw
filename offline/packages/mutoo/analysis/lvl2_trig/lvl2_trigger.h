#include "pdst.h"
#include "Event.h"
#include "ezdst.h"
#include "TDataType.h"

int process_event (PHCompositeNode *topNode); //++CINT
int setup_all(PHCompositeNode* dst_node); //++CINT
void setup_display(); //++CINT
int end_all(); //++CINT
int dstout_fopen(PHString dstout_filename); //++CINT;
void draw_plane(UShort_t arm, UShort_t octant); //++CINT
void dump_mc_hit(); //++CINT
void dump_mc_trk(); //++CINT
float calc_mom(float,float); 
float calc_mass(float,float,float,float,float,float); 
float calc_orig_phi(float,float,float);

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
#include<mMutStartTracks.h>
#include<mMutStartTracksPar.h>
#include<mMutTrackFit.h>
#include<mMutTrackFitPar.h>
#include<mMutEvalFramework.h>
#include<mMutEvalFrameworkPar.h>
#include<mMutResponse.h>
#include<mMutResponsePar.h>
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





