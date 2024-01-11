#ifndef __DDCHRECONSTRUCTIONPAR_H__
#define __DDCHRECONSTRUCTIONPAR_H__

typedef struct {
   short mirrorHitAnalysis;
   short houghThresholdOnXCell;
   short houghThresholdOnXMask;
   short houghThresholdOnUVCell;
   short houghThresholdOnUVMask;
   short purgeCandidateThreshold;
   short minimumNumberOfXHits;
   short minimumNumberOfUVHits;
   short firstXHoughThreshold;
   short secondXHoughThreshold;
   short XHitsThreshold;
   int numberOfAlphaBins;
   int numberOfPhiBins;
   int numberOfBetaBins;
   int numberOfZedBins;
   short cellDifferenceCut;
   float maxAlpha;
   float minAlpha;
   float maxPhi;
   float minPhi;
   float maxBeta;
   float minBeta;
   float maxZed;
   float minZed;
   float delBetaCut;
   float deltaBetaCut;
   float wireResolution;
   float initUVChi2;
   float initXChi2;
   float deltaBetaVertexCut;
} DDCHRECONSTRUCTIONPAR_ST;
#endif /*__DDCHRECONSTRUCTIONPAR_H__*/