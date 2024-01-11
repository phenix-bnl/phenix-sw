#ifndef MCENTRALTRACKEVALUATOR_V1_H
#define MCENTRALTRACKEVALUATOR_V1_H

#include <phool.h>

class CrkPID;
class dcghitWrapper;
class DchHitLineTable;
class DchTrack;
class dDchGhitHitsWrapper;
class dDchTracksWrapper;
class fkinWrapper;
class McEvalSingleList;
class PHCompositeNode;


class mCentralTrackEvaluator_v1{
public:
  mCentralTrackEvaluator_v1();
  virtual ~mCentralTrackEvaluator_v1();
  PHBoolean event(PHCompositeNode *);
  PHBoolean associatePC(PHCompositeNode *, const int);
  PHBoolean associateEMC(PHCompositeNode *);
  PHBoolean associateCRK(PHCompositeNode *);
  PHBoolean associateTOF(PHCompositeNode *);
  void set_verbose(const short value) {verbose=value;}

protected:
  PHBoolean mainContributorCalculation(int&, int&, int& , int&, int&, int&, int&, int&, int&, int&, int&, int&, float&, float&, float&);
  
private:
  short verbose;
  unsigned int EventCounter;
  McEvalSingleList* EvalList;
  CrkPID* d_crkpid; 
  DchHitLineTable *HitList;
  DchTrack *TrackList;
  dcghitWrapper *dcghit;
  dDchGhitHitsWrapper *dDchGhitHits;
  dDchTracksWrapper *dDchTracksPerf;
  fkinWrapper *fkin;

};
#endif /*__MCENTRALTRACKEVALUATOR_V1_H__*/
