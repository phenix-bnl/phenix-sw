#ifndef __Combination__
#define __Combination__

#include "TH2.h"
#include "THmulf.h"
#include "TObject.h"
#include "KCluster.h"

class TNtuple;

// Combination class definition.

class Combination {
 public:
  Combination( ) { 

    BoxCut = 0;
    CTOFCut   = 0;
    CRTOFCut   = 0;
    CChi2Cut   = 0;
    CPadispCut   = 0;
    CTwrhitCut   = 0;
    CChargeRejCut = 0;
    CPc3RejCut = 0;
    CStochCut[0] = 0; 
    CStochCut[1] = 0; 
    CStochCut[2] = 0; 
    CStochCut[3] = 0; 
    CStochCut[4] = 0; 
    CTightFiducial = 0;
//    CProbPhotCut = 0; 

    inv_mass = 0.0;
    e = 0.0;
    pt = 0.0;
    cosine = 0.0;
    for(int i=0;i<3;i++){
       c1_xyz[i] = c2_xyz[i] = pxyz[i] = norm_tr1[i] = norm_tr2[i] = TrackVector[i] = 0.0;
    }
    asym = 0.0;
    kcm = 0.0;
    _phi = 0.0;
	_eta = 0.0;
	_rap = 0.0;
    ec_a = ec_b = 0.0;
    e1 = e2 = 0.0;
    _tofcorr_a = _tofcorr_b = 0.0;
    _cluster_a = _cluster_b = NULL;
    arm_a = arm_b = 0;
    sec_a = sec_b = 0;  
    iz_a = iz_b = iy_a = iy_b = 0;
    sm_a_iy = sm_a_iz = sm_b_iy = sm_b_iz = 0;
    reac = 0;
    tighty_a = tighty_b = 0;
    Ly = Lz = 0;
    goodFlag = EselFlag = 0;  
  };

  virtual ~Combination() {};

  // Data records. Data records have to be place in order of
  // ntuple members.
  //

  float      inv_mass;               // Invariant Mass
  float      e;                      // Energy
  float      pt;                     // Transverse momentum
  float      cosine;                 // cosine of opening angle of two tracks
  float      pxyz[3];
  float      c1_xyz[3];
  float      c2_xyz[3];
  float      asym;          // Energy asymmetry.
  float      kcm;           // relative momentum in CM frame
  float _phi;               // pair's phi
  float _eta;               // pair's eta
  float _rap;               // pair's rap

  float ec_a;            // Ecore of each
  float ec_b;
  double _tofcorr_a;     // Corrected TOF of each
  double _tofcorr_b;
  int arm_a, arm_b;
  int sec_a, sec_b;  
  int iz_a, iz_b;        // Tower index x of center
  int iy_a, iy_b;        // Tower index y of center
  int sm_a_iy, sm_a_iz;
  int sm_b_iy, sm_b_iz;
  int reac;                   // Reaction plane bin
  int tighty_a;
  int tighty_b;  
  int Ly,Lz;

  float e1, e2;              // Energy deposit in cluster (mease or edep)
  float norm_tr1[3], norm_tr2[3];    // Normalized track vectors

  void fillBoxesHist( THmulf *gghboxes, int mixFlag, int centBin );
  void fillAsymHist( THmulf* gghasym, int mixFlag, int centBin, int ERTb);
  void fill_sysErr_asym(THmulf *ggh_sysErr_Asym, int mixFlag, int centBin, int ERTb);
  void fillRapHist( THmulf* gghrap, int centBin);
  void fillSpinHist( THmulf* gghspin, int centBin, int spin);
  void fillSpinAsymHist( THmulf* gghspin, int centBin, int spin);
  void fillPhiEta(TH2F *hAcc);
  void fillSpinPhi( THmulf* gghspin, int centBin, int spin);
  void fillShuffle( THmulf *gghshuffle, int bunch);
  void fillCheck( THmulf* gghcheck, int mixFlag, int spin, int xing);
  void fillRPHist( THmulf* gghrp, int mixFlag, int centBin);
  void fillNtuple( TNtuple* ntp, int cent, float pTcut=5., int runnumber=-1) const;
  bool calcCombination( const KCluster *c1, const KCluster *c2, float theta, int req_trig, int vorder,TH2F *pair_timing_sec[]); // eflag select either old or new ecore definition

  int        getCTOF() const { return CTOFCut; };
  int        getCChi2() const { return CChi2Cut; };
  int        getCChargeRej() const { return CChargeRejCut; };
  int        getCPc3Rej() const { return CPc3RejCut; };
  int        getCStochCut(int i) { return CStochCut[i];};

  int  getStatus() const { return goodFlag; };

  double getTrackVector(const int i) const;
  
  float getAsymmetry() const { return asym; } 

  static const double pi0_asymmetry_cut;
  static const double eta_asymmetry_cut;

 private:

  const KCluster* _cluster_a;
  const KCluster* _cluster_b;

  int        CTOFCut;
  int        CRTOFCut;
  int        CChi2Cut;
  int        CPadispCut;
  int        CTwrhitCut;
  int        CChargeRejCut;
  int        CPc3RejCut;
  int        CStochCut[5];
//  int		 CProbPhotCut;
  int        CTightFiducial;
  int        BoxCut;

  int        goodFlag;

  int        EselFlag;

  double TrackVector[3];

  ClassDef( Combination, 1 )
};

#endif

