#ifndef __TofwMixer_HH__
#define __TofwMixer_HH__

#include <TofwPar.h>

class PHCompositeNode;
class PHEmbedStat;
class TofwGeometryObject;
class TofwHit;
class TofwCalib;
class TofwMixer{
 public:
  TofwMixer();
  virtual ~TofwMixer();

  int InitRun(PHCompositeNode* sngl,PHCompositeNode* real,PHCompositeNode* merged);
  int merge();

  int       getVerbose()       {return verbose;}
  void      setVerbose(int val){verbose = val;}
  float     CalSlewing(int strip, float adc);
 protected:
  int verbose;
  //should have a class which tells the current MC particle index.
  PHCompositeNode *node1;  //single particle node
  PHCompositeNode *node2;  //nodes contains  DST
  PHCompositeNode *node3;  //nodes contains merged table information

  PHEmbedStat* embedStat;//status of the embeded tof hits 
  TofwGeometryObject*      TofwGeometry;

  TofwCalib *d_calib;
  //parameter
  float DeltaTRun;
  float DeltaT[TOFW_NSTRIP_TOTAL];
  float Slewing_A[TOFW_NSTRIP_TOTAL];
  float Slewing_B[TOFW_NSTRIP_TOTAL];
};
#endif
