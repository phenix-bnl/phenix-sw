#ifndef __KCluster__
#define __KCluster__

//enable for gamma ntuple output
#define BOOK_GAMMA_NTUPLE


// EMCal Cluster Class

//#include "stdio.h"
#include <math.h>

#include "TBuffer.h"
#include "emcClusterContentv4.h"
#include "mEmcGeometryModule.h"
#include "THmulf.h"
#include "TNtuple.h"
#include "PHTrackOut.h"
#include "PHPoint.h"
#include "PHLine.h"

class PHCentralTrack;

class KCluster : public emcClusterContentv4 {
 public:

  KCluster() :
    _pemcx(-99999.0), _pemcy(-99999.0), _pemcz(-99999.0)
    {
      TOFCut = 0;
      chi2Cut = 0;    
      twrhitCut = 0;
      chargeRejCut = 0;
      pc3RejCut = 0;
      flipChargeRejCut = 0;
      flipPc3RejCut = 0;
      stochCut[0] = 0; 
      stochCut[1] = 0; 
      stochCut[2] = 0; 
      stochCut[3] = 0; 
      stochCut[4] = 0; 

      stochCut_newe[0] = 0; 
      stochCut_newe[1] = 0; 
      stochCut_newe[2] = 0; 
      stochCut_newe[3] = 0; 
      stochCut_newe[4] = 0; 
      tightFiducial = 0;

      _minDist = _minFlipDist = 0.0;
      pt = pt_newe = 0.0;
      NewEcore = 0.0;
      phi = phiDEG = phiPHI = phiPHIDEG = 0.0;
	  rap = -999;
      iy = iz = lx = ly = lz = 0;
      vtxZ = 0.0;
      for(int i=0;i<3;i++){ xyz_unit[i] = 0.0; } 
      sec = karm = run = 0;  
      for(int i=0;i<3;i++){ ert_bit[i] = 0; } 
      tightmapcut = 0;
      et_02_ecore = et_00_ecore =  et_02_e =  et_00_e = 0.0;
      nevents=0.0;
      bbcqn = bbcqs=0.0;
    }

  ~KCluster(){ };

  void Clear(Option_t* option="") {};
  emcClusterContentv4* clone(void) const {return NULL;}

  void set_run(int runNumber) { run = runNumber; }
  int get_run() const  { return run; }

  void setArmSecIyIz();
  int getArm() const { return karm; }
  int getSec() const  { return sec; }
  int getIy() const  { return iy; }
  int getIz() const  { return iz; }
  int getStochCut(int i, int eflag) const 
         { if(eflag==0) return stochCut[i]; else return stochCut_newe[i]; }

  void setTightMapCut(int cut) { tightmapcut = cut; }
  int getTightMapCut() const  { return tightmapcut; }

  int getTightFiducial() const  { return tightFiducial; }

  void setLocalPos(mEmcGeometryModule *EmcGeo);
  float getLx() const  { return lx; }
  float getLy() const  { return ly; }
  float getLz() const { return lz; }

  void setVtxZ(float bbc_vz) { vtxZ = bbc_vz; }
  float getVtxZ() const { return vtxZ; }
  
  // This is for new ecore option
  void set_ecore_newe(float newe){NewEcore=newe;}
  float ecore_newe() const { return NewEcore;}

//   float getEcent() { return ecent(); }
  
  void setTrackVector(float theta);
  float getTrackVector(int i) const { return xyz_unit[i]; }

  float getPT() const { return pt; }
  float getPT_newe() const { return pt_newe; }

  int getTOFCut() const { return TOFCut; }
  int getChi2Cut() const { return chi2Cut; }
 
  int getTwrhitCut() const { return twrhitCut; }
  int getChargeRejCut() const { return chargeRejCut; }
  int getPc3RejCut() const { return pc3RejCut; }

  int getErtBit(int trig) const { return ert_bit[trig]; }
  void setErtBit(int ert4x4a, int ert4x4b, int ert4x4c) {
      ert_bit[0] = ert4x4a; ert_bit[1] = ert4x4b; ert_bit[2] = ert4x4c;
  }
  // Accessors for the projected nearest track coords
  //
  double getPEmcX() const { return _pemcx; }
  double getPEmcY() const { return _pemcy; }
  double getPEmcZ() const { return _pemcz; }

  void set_et_00_ecore(float ET_00_ecore) { et_00_ecore = ET_00_ecore;}
  void set_et_02_ecore(float ET_02_ecore) { et_02_ecore = ET_02_ecore;}
  void set_et_00_e(float ET_00_e) { et_00_e = ET_00_e;}
  void set_et_02_e(float ET_02_e) { et_02_e = ET_02_e;}

  float get_et_00_ecore() const { return et_00_ecore;}
  float get_et_02_ecore() const { return et_02_ecore;}
  float get_et_00_e() const { return et_00_e;}
  float get_et_02_e() const { return et_02_e;}

  void set_nevents(int NEVETS) { nevents = NEVETS;}
  void set_bbcqn(float BBCQN) { bbcqn = BBCQN;}
  void set_bbcqs(float BBCQS) { bbcqs = BBCQS;}

  int get_nevents() const { return nevents;}
  float get_bbcqn() const { return bbcqn;}
  float get_bbcqs() const { return bbcqs;}

  double getDispN() const { return sqrt(dispy()*dispy() + dispz()*dispz()); }
  
  int ert_bit[3];
  int tightmapcut;
  int tightFiducial;
  int run;
  int karm, sec;
  int iy, iz;        // y and z index of cluster on sector
  float lx, ly, lz;  // cluster position local to sector 
  float vtxZ;
  float xyz_unit[3];
  float phi, phiDEG, phiPHI,  phiPHIDEG;
  float rap;
  int TOFCut;
  int chi2Cut;
  int twrhitCut; 
  int chargeRejCut, pc3RejCut;
  int flipChargeRejCut, flipPc3RejCut;
  int stochCut[5];
  int stochCut_newe[5];
  float NewEcore;
  float pt;
  float pt_newe;

  float et_02_ecore;
  float et_00_ecore;
  float et_02_e;
  float et_00_e;

  int nevents;
  float bbcqn;
  float bbcqs;

  void passTOFCut(int sec);
  //void passTOFCut();
  void passCHI2Cut();
  void passTwrhitCut();
  void passStochasticCuts();

  bool passFiducialCuts();

  void passTrackRejCut(PHCentralTrack* track);
  void passPc3RejCut();

//  void fillHist( THmulf *gamma, THmulf *gamma2, THmulf *v2hist,
//		 THmulf* gammaRP, THmulf* gammaRP2, THmulf* charge,
//		 int cent, int rp );

//  void fillHist( THmulf *gamma1, int cent, int runn, TNtuple *gnt, int nclusters, int reacBin, int is_bookNt);
//  void fillHist(THmulf *gamma1, int cent, int runn, int nclusters, int reacBin);
  void fillHistRap( THmulf *gammarap, int cent);
  void fillHist(THmulf *gamma1, int cent, int runn, TNtuple *gnt, int is_bookNt, float inPtcut, int nclusters, int reacBin, int ERTb);

  static int inputFlag;

private:

  float    _minDist;
  float    _minFlipDist;
  
  // The emc coords of the closest track projection
  double _pemcx;
  double _pemcy;
  double _pemcz;

  ClassDef( KCluster, 1 )
    
};

#endif
