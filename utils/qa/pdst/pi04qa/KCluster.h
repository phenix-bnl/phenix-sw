#ifndef __KCluster__
#define __KCluster__

// EMCal Cluster Class

#include "stdio.h"
#include "TBuffer.h"
#include "emcClusterContentv4.h"
#include "mEmcGeometryModule.h"
#include "THmulf.h"
#include "PHTrackOut.h"
#include "PadCluster.h"
#include "PHPoint.h"
#include "PHLine.h"

class KCluster : public emcClusterContentv4 {
 public:

  KCluster()
    {
      TOFCut = 0;
      chi2Cut = 0;    
      twrhitCut = 0;
    }

  ~KCluster(){ };

  void Clear(Option_t* option="") {};
  emcClusterContentv4* clone(void) const {return NULL;}

  void set_run(int runNumber) { run = runNumber; }
  int get_run() { return run; }

  void setArmSecIyIz();
  int getArm() { return karm; }
  int getSec() { return sec; }
  int getIy() { return iy; }
  int getIz() { return iz; }

  void setLocalPos(mEmcGeometryModule *EmcGeo);
  float getLx() { return lx; }
  float getLy() { return ly; }
  float getLz() { return lz; }

  void setVtxZ(float bbc_vz) { vtxZ = bbc_vz; }
  float getVtxZ() { return vtxZ; }
  
  void setTrackVector();
  float getTrackVector(int i) { return xyz_unit[i]; }

  int getTOFCut() { return TOFCut; }
  int getChi2Cut() { return chi2Cut; }
 
  int getTwrhitCut() { return twrhitCut; }

  int run;
  int karm, sec;
  int iy, iz;        // y and z index of cluster on sector
  float lx, ly, lz;  // cluster position local to sector 
  float vtxZ;
  float xyz_unit[3];
  int TOFCut;
  int chi2Cut;
  int twrhitCut; 
  float pt;

  void passTOFCut();
  void passCHI2Cut();
  void passTwrhitCut();

  bool passFiducialCuts();

 private:
  
  ClassDef( KCluster, 1 )
    
};

#endif




