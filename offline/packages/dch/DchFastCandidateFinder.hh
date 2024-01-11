#ifndef __DCHFASTCANDIDATEFINDER_HH__
#define __DCHFASTCANDIDATEFINDER_HH__

//  Purpose: The Hough Transform.  Performs x and ux transforms and
//  returns candidates

//  The CandidateFinder is the powerhouse of the drift chamber
//  reconstruction code.  It has an associated 2d array where the
//  hough transform is executed and a list of hits and candidates.  It
//  is initialized with the hits and performs the x and uv hough
//  transforms with the hits.  In the end it returns the candidates or
//  flattens them into a STAF table.

#include "DchArray.h"
#include "DchTrackCandidate.hh"
#include "DchSnglTrackv1.h"

#include "table_header.h"
#include "dDchHit.h"
#include "dDchTracks.h"
#include "dPadCluster.h"

//#include <cmath>
#include <set>

class PHDchGeometryObject;
class DchHitLineLists;
class DchPc1HitLists;
class DchTrack;
class DchHitLineTable;
class PHDchHistogrammer;
class DchHitLineTable;
class dDchTracksExtWrapper;

struct ltposition
{
  bool operator()( std::pair<int,int> s1, std::pair<int,int> s2) const
  {
    if  (s1.first < s2.first)
      {
	return True;
      }
    else
      {
	return s1.second < s2.second;
      }
  }
};


class DchFastCandidateFinder
{ 
public:

  DchFastCandidateFinder(PHDchGeometryObject* geo, 
			 DchHitLineLists* list,
			 TABLE_HEAD_ST *dDchHit_h, 
			 DDCHHIT_ST *dDchHit, 
			 DchHitLineTable* hitLineTable);
  virtual ~DchFastCandidateFinder(){}
  
  void fillXHoughArray (PHPointerList<DchHitLine>*, 
			PHPointerList<DchHitLine>*);
  void fillUVHoughArray (short, short, 
			 PHPointerList<DchHitLine>*, 
			 PHPointerList<DchHitLine>*);
  PHBoolean purgeCandidateList(); 
  PHBoolean transformCandidateFromLocalToGlobal(DchTrackCandidate*); 
  short integralAndCogIfLocalMaximum(short x, short y, 
				     float& xcog, float& ycog);
  PHBoolean surviveBetaAndZedCorrelation();
  
  void calculateClosestApproachToBeamAxis();

  PHBoolean searchXCandidates(short, short);
  PHBoolean searchUVCandidates(DchTrackCandidate* ); 
  PHBoolean getPC1Candidates(TABLE_HEAD_ST *dPadData_h, 
			     DPADCLUSTER_ST *dPadData);
  PHBoolean getPc1UVCandidates();
  PHBoolean getVertexUVCandidates();
  PHBoolean getXCandidates(short, short);            
  PHBoolean getX1Candidates(short, short);           
  PHBoolean getX2Candidates(short, short);           
  PHBoolean getUVCandidates(short, short);           
  
  void resizeHoughArray(short x, short y);
  void zeroHoughArray(short x, short y);

  void phiAndAlpha(DchHitLine*, DchHitLine*,double &, double&);
  void betaAndZed(const PHPoint&, const PHPoint&,double &, double&);
  
  void attachCandidateList(PHPointerList<DchTrackCandidate>* candiList) {
    candidates = candiList;
  }
  void attachHitLineLists(DchHitLineLists* list) {hitList = list;}
  void attachPc1HitLists(DchPc1HitLists* list) {pc1List = list;}
  void setQuality(int q);
  
  PHPointerList<DchTrackCandidate>* getCandidateList() {return candidates;}
  DchHitLineLists* getHitLineLists() {return hitList;}
        
  // AD analysis parameter
 
  short getCellDifferenceCut(){return pCellDifferenceCut;}
  int   getHoughThresholdOnXCell(){return pHoughThresholdOnXCell;}
  int   getHoughThresholdOnXMask(){return pHoughThresholdOnXMask;}
  int   getHoughThresholdOnUVCell(){return pHoughThresholdOnUVCell;}
  int   getHoughThresholdOnUVMask(){return pHoughThresholdOnUVMask;}
  int   getHoughThresholdOnUVMaskWithPc1Hit() { return pHoughThresholdOnUVMaskWithPc1Hit;}
  int   getNumberOfAlphaBins(){return pNumberOfAlphaBins;}
  int   getNumberOfPhiBins(){return pNumberOfPhiBins;}
  float getMaxAlpha(){return pMaxAlpha;}
  float getMinAlpha(){return pMinAlpha;}
  float getMaxPhi(){return pMaxPhi;}
  float getMinPhi(){return pMinPhi;}
  int   getNumberOfBetaBins(){return pNumberOfBetaBins ;}
  int   getNumberOfZedBins(){return pNumberOfZedBins ;}
  float getMaxBeta(){return pMaxAlpha;}
  float getMinBeta(){return pMinAlpha;}
  float getMaxZed(){return pMaxZed;}
  float getMinZed(){return pMinZed;}
  float getReferenceRadius(){return pReferenceRadius;}
  float getDeltaBetaVertexCut(){return pDeltaBetaVertexCut;}
  float getZMax(){return pZMax;}
  float getZMin(){return pZMin;}
  float getZAvg(){return pZAvg;}
  int   getMinimumNumberOfHits(){return pMinimumNumberOfHits;}
  int   getMinimumNumberOfXHits(){return pMinimumNumberOfXHits;}
  int   getMinimumNumberOfUVHits(){return pMinimumNumberOfUVHits;}
  float getMaxUVDistance(){return pMaxUVDistance;}

  void  setCellDifferenceCut(short v){pCellDifferenceCut = v;}
  void  setHoughThresholdOnXCell(int v){pHoughThresholdOnXCell = v;}
  void  setHoughThresholdOnXMask(int v){pHoughThresholdOnXMask = v  ;}
  void  setHoughThresholdOnUVCell(int v){pHoughThresholdOnUVCell = v  ;}
  void  setHoughThresholdOnUVMask(int v){pHoughThresholdOnUVMask = v ;}
  void  setHoughThresholdOnUVMaskWithPc1Hit(int v) { pHoughThresholdOnUVMaskWithPc1Hit = v;}
  void  setNumberOfAlphaBins(int v){pNumberOfAlphaBins = v ;}
  void  setNumberOfPhiBins(int v){pNumberOfPhiBins = v ;}
  void  setMaxAlpha(float v){pMaxAlpha = v;}
  void  setMinAlpha(float v){pMinAlpha = v;}
  void  setMaxPhi(float v){pMaxPhi = v;}
  void  setMinPhi(float v){pMinPhi = v;}
  void  setNumberOfBetaBins(int v){pNumberOfBetaBins = v ;}
  void  setNumberOfZedBins(int v){pNumberOfZedBins = v ;}
  void  setMaxBeta(float v){pMaxBeta = v;}
  void  setMinBeta(float v){pMinBeta = v;}
  void  setMaxZed(float v){pMaxZed = v;}
  void  setMinZed(float v){pMinZed = v;}
  void  setReferenceRadius(float v){pReferenceRadius = v;}
  void  setDeltaBetaVertexCut(float v){pDeltaBetaVertexCut = v;}
  void  setMinimumNumberOfHits(int v){pMinimumNumberOfHits = v;}
  void  setMinimumNumberOfXHits(int v){pMinimumNumberOfXHits = v;}
  void  setMinimumNumberOfUVHits(int v){pMinimumNumberOfUVHits = v;}
  void  setZMax(float v){pZMax = v;}
  void  setZMin(float v){pZMin = v;}
  void  setZAvg(float v){pZAvg = v;}
  void  setMaxUVDistance(float v){pMaxUVDistance = v;}

  void setVertex(PHPoint v) {vertex = v;}
  PHPoint getVertex() {return vertex;}
  void setBeamAxis(PHVector v) {beamaxis = v;}
  PHPoint getBeamAxis() {return beamaxis;}

  // flattens the candidates into the dDchTracks STAF table.
  void Flatten(int reset, 
	       TABLE_HEAD_ST *dDchTracks_h, 
	       DDCHTRACKS_ST *dDchTracks,
	       TABLE_HEAD_ST *dDchHit_h, 
	       DDCHHIT_ST *dDchHit, int suppressHits = 0);

  void fillOutputTables(DchTrack *, DchHitLineTable *, int suppressHits = 0);
 
  int  getVerbose() {return verbose;}
  void setVerbose(int v) {verbose = v;}
  void fillAligmentNtuple();
  void fitDchTracksAlpha();
  void attachHistogrammer(PHDchHistogrammer* histo) {histogrammer = histo;}

  void setDchTracksExtTable(dDchTracksExtWrapper*val ){dchTracksExt = val;}
private:
  dDchTracksExtWrapper * dchTracksExt;

private:

  PHDchHistogrammer* histogrammer;
  PHPointerList<DchSnglTrackv1> trackOutList;

  PHDchGeometryObject* geometry;

  std::set<std::pair<int,int>,ltposition> SparseHoughLookup;
  Dch2DimArray<short> HoughArray;

  DchHitLineLists* hitList;
  DchPc1HitLists* pc1List;
  PHPointerList<DchTrackCandidate> *candidates;
  PHPoint  cylinderNorthPoint[2];
  PHPoint  cylinderCenter[2];
  PHVector cylinderAxis[2];

  int verbose;

  // Vertex information

  PHPoint vertex;
  PHVector beamaxis;
  // here are the uv intersections with the hough plane used for the
  // uv hough transform

  long HoughDimensionX, HoughDimensionY;
  long thetaDimension;
  int  houghThresholdOnCell;
  int  houghThresholdOnMask;

  // parameters for X hough feature space
  int pNumberOfAlphaBins;
  int pNumberOfPhiBins;
  float pMaxPhi;
  float pMinPhi;
  float pMaxAlpha; 
  float pMinAlpha;

  // parameters to calculate alpha and phi
  short pCellDifferenceCut;     
  float pReferenceRadius;

  // parameters to limit fill region of UV feature space
  float pZMin;  //cm
  float pZAvg;
  float pZMax;
  float pMaxUVDistance;

  // parameters to search for X candidates in feature space
  int pHoughThresholdOnXCell;    // individual cell threshold
  int pHoughThresholdOnXMask;    // higher threshold in larger mask

  // parameters for UV hough feature space
  int pNumberOfZedBins;
  int pNumberOfBetaBins;
  float pMaxBeta;
  float pMinBeta;
  float pMaxZed; 
  float pMinZed;

  // parameters to search for UV candidates in feature space
  int pHoughThresholdOnUVCell;
  int pHoughThresholdOnUVMask;
  int pHoughThresholdOnUVMaskWithPc1Hit;
  float pDeltaBetaVertexCut;
  
  // parameters used to purge candidate list
  int pMinimumNumberOfHits;
  int pMinimumNumberOfXHits;
  int pMinimumNumberOfUVHits;

  // This is the variable that determines the maximum cell difference
  // between the X wires that the software will look at.  Making it
  // too small will decrease the tracking efficiency (first cutting
  // out low momentum tracks) while making it too large will increase
  // the processing time significantly

  PHBoolean tooManyHits;
  PHBoolean tooManyTracks;
  
public:
  PHBoolean hasTooManyHits();
  PHBoolean hasTooManyTracks();
};

#endif /* __DCHFASTCANDIDATEFINDER_HH__ */
