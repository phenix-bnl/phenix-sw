#include "pdst.h"
#include "Event.h"
#include "TDataType.h"


int process_event (PHCompositeNode *topNode); //++CINT
int setup_all(); //++CINT
void setup_display(); //++CINT
int end_all(); //++CINT
int dstout_fopen(PHString dstout_filename); //++CINT;
void draw_plane(UShort_t arm, UShort_t octant); //++CINT
int dinit(); //++CINT
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

// Module includes
//
#include<mMutUnpackMutDst.h>
#include<mMutUnpackMutDstPar.h>
#include<mMutMuiRoad.h>
#include<mMutMuiRoadPar.h>
#include<mMutFindClus.h>
#include<mMutFindClusPar.h>
#include<mMutFitClus.h>
#include<mMutFitClusPar.h>
#include<mMutFindGapCoord.h>
#include<mMutFindGapCoordPar.h>
#include<mMutStartTracks.h>
#include<mMutStartTracksPar.h>
#include<mMutFindStub.h>
#include<mMutFindStubPar.h>
#include<mMutTrackFit.h>
#include<mMutTrackFitPar.h>
#include<mMutEvalFramework.h>
#include<mMutEvalFrameworkPar.h>
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





