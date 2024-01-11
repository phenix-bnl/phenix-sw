#ifndef MNEWDCHEVALUATOR_H
#define MNEWDCHEVALUATOR_H

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"

#include "DchTrackCandidate.hh"
#include "DchMcRecoTrack.hh"

class PHDchGeometryObject; 
class PHDchAddressObject;
class PHDchNoiseObject;
class PHDchCalibrationObject;
class PHDchHistogrammer;
class PHCompositeNode;
class CrkGeometryObject;
class CrkPIDout;
class PHDchHistogrammer;

#ifndef __CINT__
#include <CrkPID.hh>
#include <utiMatch.h>
#endif

class mNewDchEvaluator{
public:
  mNewDchEvaluator();
  virtual ~mNewDchEvaluator();
  PHBoolean event(PHCompositeNode *);
  PHBoolean associateDchExt(PHCompositeNode *); 
  PHBoolean associatePC(PHCompositeNode *, int);
  PHBoolean associateEMC(PHCompositeNode *);
  PHBoolean associateCRK(PHCompositeNode *);
  PHBoolean associateTOF(PHCompositeNode *);
  PHBoolean fillCompleteEvaluation();
  PHBoolean extrapolateToVertex(PHCompositeNode *);

  void LoadMatchPar();
  PHBoolean associateMatching();
  void   MatchPc1(int type=1);
  void   MatchPc2(int type=1);
  void   MatchPc3(int type=1);
  void   MatchTof(int type=1);
  void   MatchEmc(int type=1);

  PHDchHistogrammer* getHistogrammer() { return dchHistogrammer;}

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
  PHBoolean mainContributorCalculation(PHPointerList<PHNode>&, int&, int&, int& , int&, int&, int&, int&, int&, int&, int&, int&, int&, float&, float&, float&);

public:  
  const PHDchAddressObject* getDAO() const { return dchAddressObject;}
  const PHDchGeometryObject* getDGO() const { return dchGeometryObject;}
  const PHDchCalibrationObject* getDCO() const { return dchCalibrationObject;}
  const PHDchNoiseObject* getDNO() const { return dchNoiseObject;}
  
private:
  short verbose;
  
  //!@name pointers to needed objects
  /*! 
  Pointers are used because these are retrieved from the node tree
  When an object is not found in the tree, "new" is called. However the
  created object *must* be added to the node tree so that it gets deleted
  when clearing the tree
  */
  //@{
  PHDchGeometryObject* dchGeometryObject;
  PHDchAddressObject* dchAddressObject;
  PHDchCalibrationObject* dchCalibrationObject;
  PHDchNoiseObject* dchNoiseObject;
  PHPointerList<DchTrackCandidate>* trackCandidateList;
  PHPointerList<DchMcRecoTrack>* mcRecoTrackList;
  PHDchHistogrammer* dchHistogrammer;
  //@}
  
  #ifndef __CINT__
  //!@name local objects.
  /*! 
  Pointers are not needed. Objects are created in the
  parent object constructor and automatically deleted in 
  the parent object destructor
  */
  //@{
  CrkPID d_crkpid; 
  utiMatch m;
  //@}
  #endif

  private:
  float APpc1,BPpc1,CPpc1,DPpc1;
  float AZpc1,BZpc1,CZpc1,DZpc1;
  float APpc2,BPpc2,CPpc2,DPpc2;
  float AZpc2,BZpc2,CZpc2,DZpc2;
  float APpc3,BPpc3,CPpc3,DPpc3;
  float AZpc3,BZpc3,CZpc3,DZpc3;
  float APtof,BPtof,CPtof,DPtof;
  float AZtof,BZtof,CZtof,DZtof;
  float APcrk,BPcrk,CPcrk,DPcrk;
  float AZcrk,BZcrk,CZcrk,DZcrk;
  float APemc,BPemc,CPemc,DPemc;
  float AZemc,BZemc,CZemc,DZemc;
};
#endif /*__MNEWDCHEVALUATOR_H__*/





