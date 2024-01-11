#ifndef __DchMixer_H__
#define __DchMixer_H__

#include "TRandom3.h"

class PHCompositeNode;
class PHEmbedStat;
class PHDchCalibrationObject;
class PHDchAddressObject;
class PHDchGeometryObject;
class mNewDchCalibrator;

class DchMixer{
 public:
  DchMixer();
  virtual ~DchMixer();
  int InitRun(PHCompositeNode* sngl,PHCompositeNode* real,PHCompositeNode* merged);
  int merge();
  int       getVerbose()       {return verbose;}
  void      setVerbose(int val){verbose = val;}
  float     getdcT0East() {return dcT0East;}
  void      setdcT0East(float val){dcT0East = val;}
  float     getdcT0West() {return dcT0West;}
  void      setdcT0West(float val){dcT0West = val;}
  int       getkickOutHitsToSpeedupReco(){return kickOutHitsToSpeedupReco;}
  void      setkickOutHitsToSpeedupReco(int val){ kickOutHitsToSpeedupReco= val;}
  int       constructHitLineOut();
  void      checkhits();

 protected:

  TRandom3 *rand;

  int verbose;
  int kickOutHitsToSpeedupReco;
  //should have a class which tells the current MC particle index.
  PHCompositeNode *node1;  //single particle node
  PHCompositeNode *node2;  //nodes contains  DST
  PHCompositeNode *node3;  //nodes contains merged table information

  PHEmbedStat* embedStat; //status of embeded hits!
  PHDchAddressObject*     dchAddressObject;
  PHDchCalibrationObject* dchCalibrationObject;
  PHDchGeometryObject*    dchGeometry;
  float dcT0East;
  float dcT0West;  

  float delt0, deldv;

};
#endif
