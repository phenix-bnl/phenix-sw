#ifndef __PHYSICSQARECO_H__
#define __PHYSICSQARECO_H__

#include "SubsysReco.h"
#include <vector>
#include <string>

using namespace std;

class Fun4AllServer;
class PHCompositeNode;
class TH1;
//
//  Hello quality fan.  This routine is used to fill in
//  the standard physics-qa histograms that are used to 
//  verify the performance of PHENIX during the given run.
//  This routine was originally written by Sean Leckey as an
//  OnlCal module.  To run in the offline, it has been ported
//  to be a SubsysReco module.
//                       TKH 3-9-2003
//
//  We have added a new "End" method that will fit the histograms
//  and then write interesting results into Cesar's qasummary 
//  database.
//  
//  To make the code simple, we have decided to make a struct called
//  a QaEntry {double val, double err, string name}.  The
//  user will simply push_back QaEntry objects onto the vector
//  and then pass the vector to a single routine that places the
//  whole set in the database.
//
//                       TKH MPM 7-31-2004
//
//  Under Tom's guidance, I've moved the definition of QaEntry into QaEntry.h.
//                       -MPM 8/25/2004

class PhysicsqaReco: public SubsysReco
{

 public:
  PhysicsqaReco(int commit = 0);
  int Init         (PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End          (PHCompositeNode *topNode);
  
  //  Kluge until the framework is yet again able to 
  //  restore histograms from a file...
  int ReadHistogramsFromFile(const char *filename);

  // Allows Tags to be set from macro
  int SetProductionTag(char *tagname);

private:
  int PhotonQA(PHCompositeNode *topNode);
  int FillPi0(int arm_a, int sector_a, int arm_b, int sector_b,  float pt, float invmass, float cosine);

  unsigned int numphotons;
  unsigned int nevt;
  unsigned int runNumber;
  float QA_ETOFCUT;
  float QA_TOFFACTOR;
  float BBCTimeZero;

  int commit;
  string Tag;

  TH1 *meanpt         ; 
  TH1 *tofmass2       ; 
  TH1 *tofmass2pos    ; 
  TH1 *tofmass2neg    ; 
  TH1 *emcGlmass2     ; 
  TH1 *emcScmass2     ; 
  TH1 *numtracks      ; 
  TH1 *eoverpESc      ; 
  TH1 *eoverpEGl      ; 
  TH1 *invmassPi0_0_0 ; 
  TH1 *invmassPi0_0_1 ; 
  TH1 *invmassPi0_0_2 ; 
  TH1 *invmassPi0_0_3 ; 
  TH1 *invmassPi0_1_0 ; 
  TH1 *invmassPi0_1_1 ; 
  TH1 *invmassPi0_1_2 ; 
  TH1 *invmassPi0_1_3 ; 
  TH1 *CENTRAL        ; 
  TH1 *emcsGldphiH    ; 
  TH1 *emcsGldzH      ; 
  TH1 *emcsScdphiH    ; 
  TH1 *emcsScdzH      ; 
  TH1 *tofsdphiH      ; 
  TH1 *tofsdzH        ; 
  TH1 *pc2sdphi       ; 
  TH1 *pc2sdz         ; 
  TH1 *east_pc3sdphi  ; 
  TH1 *east_pc3sdz    ; 
  TH1 *west_pc3sdphi  ; 
  TH1 *west_pc3sdz    ; 
  TH1 *HIGHPC3SDPHI   ; 
  TH1 *SMDNX          ; 
  TH1 *SMDNY          ; 
  TH1 *SMDSX          ; 
  TH1 *SMDSY          ; 

};

#endif /*__PHYSICSQARECO_H__ */
  

