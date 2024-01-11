#ifndef __MNEWDCHNCHCANDIDATORY_H__
#define __MNEWDCHNCHCANDIDATORY_H__

#include <phool.h>
#include <PHCompositeNode.h>
#include <PHPointerList.h>

#include <DchTrackCandidate.hh>

#include <iostream>

//Used to make option setting more readable...
enum EDchCandidatoryOptions {
  kx1analysis       = BIT(0),
  kx2analysis       = BIT(1),
  kx1x2analysis     = BIT(2),
  kuvanalysis       = BIT(3),
  kpc13danalysis    = BIT(4),
  kpc1analysis      = BIT(5),
  kfitxanalysis     = BIT(6),
  kfit3danalysis    = BIT(7),
  kvertexanalysis   = BIT(8),
  kuvvertexanalysis = BIT(9),
  kuvpc1analysis    = BIT(10),
  kverbose          = BIT(11),
  kx1ExclusiveAnalysis =BIT(12),
  kx2ExclusiveAnalysis =BIT(13),
  kx1OddExclusiveAnalysis =BIT(14),
  kx2OddExclusiveAnalysis =BIT(15),
  kx1EvenExclusiveAnalysis=BIT(16),
  kx2EvenExclusiveAnalysis =BIT(17)
};

class DchHitAssociator;
class DchHitLineTable;
class DchLineTracker;
class DchFastCandidateFinder;
class PHDchGeometryObject; 
class PHDchAddressObject;
class PHDchNoiseObject;
class PHDchCalibrationObject;
class DchHitLineLists;
class DchPc1HitLists;
class DchTrack;
class PHDchHistogrammer;
class dDchTracksExtWrapper;

class mNewDchCandidatory
{

public:
  mNewDchCandidatory();
  mNewDchCandidatory(int option);
  virtual ~mNewDchCandidatory();
  PHBoolean event(PHCompositeNode *);
  int ResetEvent(PHCompositeNode *topNode);

  DchTrackCandidate* getCandidate(size_t i) { return (*trackCandidateList)[i];}
  int  numberOfCandidates() { return trackCandidateList->length();}
 
  void useX1X2HoughTransform() {x1x2analysis=1;}
  void useX1HoughTransform()   {x1analysis=1;}
  void useX2HoughTransform()   {x2analysis=1;}
  void useUVHoughTransform()   {uvanalysis=1;}
  void useVertexToFindUV()     {uvvertexanalysis=1;}
  void usePc1ToFindUV()        {uvpc1analysis=1;}
  void useFitToXProjection()   {fitxanalysis=1;}
  void useVertexAndPc1In3D()   {pc13danalysis=1;}
  void usePc1Hits()            {pc1analysis=1;}
  void useFitIn3D()            {fit3danalysis=0; std::cout << PHWHERE << "NOT implemented "<< std::endl;}
  void useVertexFromVtxOut()   {vertexanalysis=1;}
  void useMoveHitsToCandidate(){movehitstoline=1;}

  void KeepX1Exclusive() { x1ExclusiveAnalysis=1; } 
  void KeepX2Exclusive() { x2ExclusiveAnalysis=1; }  
  void KeepX1OddExclusive() {x1OddExclusiveAnalysis=1;std::cout   <<  PHWHERE << "NOT implemented "<< std::endl;} 
  void KeepX2OddExclusive() {x2OddExclusiveAnalysis=1;std::cout   <<  PHWHERE <<  "NOT implemented "<< std::endl;} 
  void KeepX1EvenExclusive() {x1EvenExclusiveAnalysis=1;std::cout <<  PHWHERE << "NOT implemented "<< std::endl;}
  void KeepX2EvenExclusive() {x2EvenExclusiveAnalysis=1;std::cout <<  PHWHERE << "NOT implemented "<< std::endl;}
  
  void clearAnalysisOptions();
  float getVertex();
  PHBoolean getPc1Hits();
  float delt0;
  float deldv;
  void setCalibCorr(float ddt, float ddv) { delt0 = ddt; deldv = ddv; }
  
  void fillAligmentNtuple(int flag) { fillAligment = flag;} 
  void setVerbose( int v ) {verbose = v;}
  int  getVerbose() {return verbose;}
  
  void setMaxEventForUnsuppressedHits(int val) { maxEventForUnsuppressedHits = val;}

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);

private:

  int fillAligment;
  PHDchHistogrammer           *histogrammer;
  PHDchGeometryObject         *dchGeometryObject;
  PHDchAddressObject          *dchAddressObject;
  PHDchCalibrationObject      *dchCalibrationObject;
  PHDchNoiseObject            *dchNoiseObject;
  PHPointerList<DchTrackCandidate> *trackCandidateList;
  PHPointerList<DchTrackCandidate> *X1CandidateList;
  PHPointerList<DchTrackCandidate> *X2CandidateList;
  PHPointerList<DchTrackCandidate> *X1X2CandidateList;
  DchHitLineLists                  *hitLineLists;
  DchPc1HitLists                   *pc1HitLists;  
  
  void setDefaultParameters(DchFastCandidateFinder*,DchHitAssociator*,DchLineTracker*);
  void printParameters(DchFastCandidateFinder*,DchHitAssociator*,DchLineTracker*);
  void FreeX1X2TrackHitsForFurtherAnalysis(DchHitAssociator * associator);
  void PrepareUVPC1Analysis(DchHitAssociator * associator,DchFastCandidateFinder * finder,DchLineTracker*);
  void AssociateFreeXHitsToTrackCandidates(DchHitAssociator * associator,DchFastCandidateFinder * finder);
  void Pc1Analysis(DchHitAssociator * associator,DchFastCandidateFinder * finder);
  void MergeAllCandidateLists();
  void X1Analysis(DchHitAssociator * associator,DchFastCandidateFinder * finder,DchLineTracker*);
  void X2Analysis(DchHitAssociator * associator,DchFastCandidateFinder * finder,DchLineTracker*);
  void X1X2Analysis(DchHitAssociator * associator,DchFastCandidateFinder * finder,DchLineTracker*);
  void Fit3DAnalysis(DchHitAssociator * associator,DchFastCandidateFinder * finder,DchLineTracker*);
  void UVAnalysis(DchHitAssociator * associator,DchFastCandidateFinder * finder);
  void UVPC1Analysis(DchHitAssociator * associator,DchFastCandidateFinder * finder,DchLineTracker*);
  void FitXAnalysis(DchHitAssociator * associator,DchFastCandidateFinder * finder,DchLineTracker*);

  int internalEventCounter;
  int maxEventForUnsuppressedHits;
  int verbose;

  // flags to select analysis options

  int x1ExclusiveAnalysis;    
  int x2ExclusiveAnalysis;    
  int x1OddExclusiveAnalysis; 
  int x2OddExclusiveAnalysis; 
  int x1EvenExclusiveAnalysis;
  int x2EvenExclusiveAnalysis;
  int x1analysis;
  int x2analysis;
  int x1x2analysis;
  int uvanalysis;
  int uvpc1analysis;
  int uvvertexanalysis;
  int pc13danalysis;
  int pc1analysis;
  int fitxanalysis;
  int fit3danalysis;
  int vertexanalysis;
  
  int movehitstoline;
  float vertexPoint;
 
  PHCompositeNode *topNode;
  DchHitLineTable* hitLineTablev1,*hitLineTablev2;
  DchTrack*   trackTablev1;
  dDchTracksExtWrapper* dchTracksExt;
};
#endif /*__MNEWDCHNCHCANDIDATORY_H__*/

