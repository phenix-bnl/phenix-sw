#ifndef __Combination__
#define __Combination__

#include "THmulf.h"
#include "KCluster.h"

// Combination class definition.

class Combination {
 public:
  Combination( ) { 

    CTOFCut   = 0;
    CRTOFCut   = 0;
    CChi2Cut   = 0;
    CPadispCut   = 0;
    CTwrhitCut   = 0;

  };

  Combination( KCluster *c1, KCluster *c2 ) { 

    CTOFCut   = 0;
    CRTOFCut   = 0;
    CChi2Cut   = 0;
    CPadispCut   = 0;
    CTwrhitCut   = 0;

    calcCombination( c1, c2);

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
  float      asym;                   // Energy asymmetry.

  float      ec_a, ec_b;             // Ecore of each
  int      arm_a, arm_b;
  int      sec_a, sec_b;  
  int      iz_a, iz_b;             // Tower index x of center
  int      iy_a, iy_b;             // Tower index y of center
  int      sm_a_iy, sm_a_iz;
  int      sm_b_iy, sm_b_iz;
  int        reac;                   // Reaction plane bin

  float e1, e2;              // Energy deposit in cluster (mease or edep)
  float norm_tr1[3], norm_tr2[3];    // Normalized track vectors

  void         fillHist( THmulf *, int, int );
  bool       calcCombination( KCluster *c1, KCluster *c2);

  int        getCTOF() const { return CTOFCut; };
  int        getCChi2() const { return CChi2Cut; };

  int  getStatus() const { return goodFlag; };

  double getTrackVector(const int i) const;

 private:

  int        CTOFCut;
  int        CRTOFCut;
  int        CChi2Cut;
  int        CPadispCut;
  int        CTwrhitCut;
  int        CChargeRejCut;
  int        CPc3RejCut;
  int        CStochCut[5];
 
  int        goodFlag;

  double TrackVector[3];

};

#endif


