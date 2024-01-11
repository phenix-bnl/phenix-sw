#ifndef __MNEWDCHNOFIELDCANDIDATORY_H__
#define __MNEWDCHNOFIELDCANDIDATORY_H__

#include "phool.h"
#include "PHPointerList.h"

class PHCompositeNode;

class PHDchGeometryObject; 
class PHDchAddressObject;
class PHDchNoiseObject;
class PHDchCalibrationObject;
class DchHitLineLists;
class DchHitLineTable;
class DchTrack;
class DchTrackCandidate;

class mNewDchNoFieldCandidatory
{
public:
  mNewDchNoFieldCandidatory();
  virtual ~mNewDchNoFieldCandidatory();
  PHBoolean event(PHCompositeNode *);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);

public:  
  const PHDchAddressObject* getDAO() const { return dchAddressObject;}
  const PHDchGeometryObject* getDGO() const { return dchGeometryObject;}
  const PHDchCalibrationObject* getDCO() const { return dchCalibrationObject;}
  const PHDchNoiseObject* getDNO() const { return dchNoiseObject;}
  DchTrackCandidate* getCandidate(size_t i) { return (*trackCandidateList)[i];}
  int  numberOfCandidates() { return trackCandidateList->length();}
  
private:

  PHDchGeometryObject         *dchGeometryObject;
  PHDchAddressObject          *dchAddressObject;
  PHDchCalibrationObject      *dchCalibrationObject;
  PHDchNoiseObject            *dchNoiseObject;
  PHPointerList<DchTrackCandidate> *trackCandidateList;
  DchHitLineLists             *hitLineLists;

  DchHitLineTable* hitLineTablev1;
  DchHitLineTable* hitLineTablev2;
  DchTrack* trackTablev1;

};
#endif /*__MNEWDCHNOFIELDCANDIDATORY_H__*/
