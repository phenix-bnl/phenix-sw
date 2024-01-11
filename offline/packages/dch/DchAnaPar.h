//-------------------------------------------------------------------
// Updated by Federica Ceretto/Messer
//
// Date: 06/27/00
//
// Purpose: Variable definitions
// Here are a number of the variables we use in the drift chamber
// code.  Eventually we'll want these in an external file (or defined
// in a table for STAF) so that the code need not be recompiled every
// time we change one of these parameters.
//
//--------------------------------------------------------------------

#ifndef __DCHANAPAR_H__
#define __DCHANAPAR_H__

#include "DchDgoPar.h"

// define by FEderica for the FAST Candidate Searcher
#define houghThresholdOnXCell2 10   //* to be tuned
#define houghThresholdOnXMask2 15   //* to be tuned
#define houghThresholdOnUVCell2 1   //* to be tuned
#define houghThresholdOnUVMask2 3   //* to be tuned
#define purgeCandidateThreshold2 15 //* to be tuned
#define firstXHoughThreshold2 10 //* WAS 15
#define secondXHoughThreshold2 10 //* WAS 15
#define XHitsThreshold2 10 //*
//  Defines the range in the zed vs beta hough transform space over
// which we search.  This helps to pick out those tracks which came
// from the vertex.
#define delBetaCut2 .2 //*
#define deltaBetaCut2 0.2//*
const float wireResolution2 = .015; //*

#define numberOfAlphaBins2 300  //*
#define numberOfSinPhiBins2 6000 //*

#define maxSinPhi2 1.00 //*
#define minSinPhi2 -0.65 //*
#define maxAlpha2  0.8 //*
#define minAlpha2 -0.8 //*

#define numberOfBetaBins2 90 //*
#define numberOfZedBins2 400 //*

#define minBeta2 0.6 //*
#define maxBeta2 2.5 //*
#define minZed2 -100 //*
#define maxZed2  100 //*

#define numberOfAlphaBins_2 150
#define numberOfSinPhiBins_2 3000

const float initUVChi =  0.045; // wireresolution * 3
const float initXChi = 10.;

// defined by Federica : 6-15-98
#define maxEdge 768
#define numberOfTimeBins 800   // Histogrammer

#define maxNumberOfHits 4000  // DchData
#define minNumberOfHits  10 // was 5 // Tracker - FAst tracker
#define notAssociated -1

// here are the minimum and maximum z values for side = -1 (both
// sides) side =0 and side =1.  So in order to find the minimum and
// maximum, you need to look at uvPointMinZ[side+1] and
// uvPointMaxZ[side+1] respectively

const float uvPointMinZ[3] = {-100, -100, 0};
const float uvPointMaxZ[3] = {100,    0,   100};

const float dchXFittingArray[20] = {5, 4.7, 4.0, 3.0, 2.0, 1.0,
				    .8, .50, .27, .24, .21, .18,
				    .15, .12, .09, .06, .04, .04,
				    .04, .04};

const float dchUVFittingArray[20] = {5.0, 4.8, 4.6, 4.4, 4.2, 4.0,
				     3.8, 3.6, 3.4, 3.2, 3.0, 2.8,
				     2.6, 2.4, 2.2, 2.0, 1.8, 1.6,
				     1.4, 1.2};

#endif

