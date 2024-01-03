//-----------------------------------------------------------------------------
//
//  Check whether a particle is within the defined fiducial area
//  don't confuse with the PHENIX central arm acceptance
//  modify to allow different field settings:  5/23/06 AD
//  controled by new flag fieldSetting, also hand over ptcut and vtxcut
//
//  RETURN VALUE:
//  in acceptance:     return 0,1
//  not in acceptance: return <0
//
//  INPUT:
//
//  int fieldSetting
//  1: ++ field as used for run-4/5/6
//  2:  + field used earlier
//  3: +- field for running with HBD
//
//  1 is default value, determined by Alberica Toia from Run-4
//  2 values taken from old acceptance filter
//  3 values determined from field values given in PHENIX CDR
//
//  double pt_cut in GeV
//  double vtx_cut in cm
//
//  Particle,ParticlePropertyList
//-----------------------------------------------------------------------------

#include <TMath.h>
#include <cmath>
#include "Momentum.h"
#include "PHENIXFiducial.h"
#include "Particle.h"
#include "ParticleProperty.h"
#include "ParticlePropertyList.h"
#include "Tools.h"

int PHENIXFiducial(const int fieldSetting, const double pt_cut, const double vtx_cut,
    const Particle& PParticle, const ParticlePropertyList& PPList)
{
  // ------------- initialize ------------------------------
  // Phi acceptance of each arm
  const double phi_bot_w = -0.589; // MC info form dcgeom96.f
  const double phi_top_w =  0.982;
  const double phi_bot_e =  3.731; // MC info form dcgeom96.f
  const double phi_top_e =  2.160;
  // NOTE: Exodus generates 0 .. 2pi, PHENIX convention -pi/2 .. 3pi/2

  // Theta acceptance
  const double theta_min =  1.23;  // From data
  const double theta_max =  1.92;
  // slopes for each line tuned ++ field (Run-4/5/6)
  double sl1 =  0.309; // CRK cut
  double sl2 =  0.206; // DCH cut
  double sl3 =  0.275; // PC3 cut
  double slz =  0.004224; // Z_theta slope

  // EMCal sector boundary
  const double emc_boundary = 0.015;

  // old values for for + field
  if (fieldSetting==2) {
    sl1 = 0.235; // CRK cut
    sl2 = 0.155; // DCH cut
    // PC3 cut???
    slz = 0.0038; // Z_theta slope
  }
  // estimates for +- field need to be improved
  if (fieldSetting==3) {
    sl1 = 0.070; // CRK cut
    sl2 = 0.03; // DCH cut
    // PC3 cut???
    slz = 0.0038; // Z_theta slope
  }

  // --------------- get particle kinematics ------------------

  const Mom3&  mom(PParticle.Get4mom());

  const int    pid    = PParticle.GetID();
  const int    charge = PPList.GetByID(pid)->GetCharge();
  if (charge==0) return -1;

  const double px     = mom.Getpx();
  const double py     = mom.Getpy();
  const double pt     = std::sqrt(px*px+py*py);       // Calculate pt
  if (pt < pt_cut) return -2;                         // Lower pt cut

  const double zvtx   = PParticle.GetzVertex();      // zvertex in cm
  if (std::abs(zvtx) > vtx_cut) return -10;           // Vertex cut

  // --------------- acceptance check -------------------------

  // --------------- ACCEPTANCE CHECK -------------------------
  const double theta  = mom.Theta();

  // --------------------- Theta acceptance ----------------------

  const double th1 = theta-zvtx*slz;

  if (th1>theta_max || th1<theta_min) return -3;

  // --------------------- Phi acceptance ----------------------
  int phi_arm = -1;        //  Flag to fall in phi acceptance
  double phi  = phiPHENIX(mom.Phi());
  double phi1 = phiPHENIX(phi-(charge/pt)*sl1);
  double phi2 = phiPHENIX(phi-(charge/pt)*sl2);
  double phi3 = phiPHENIX(phi-(charge/pt)*sl3);

  int accepted=1;
  int fidcut=1;
  /*
   * return fidcut:
   * 1: ideal acceptance + EMC boundaries
   * 2: survived normal fiducial cut
   * 3: survived 30% stronger fiducial cut
   * 4: survived 100% stronger fiducial cut
   */

  // Fiducial cuts from Yorito
  // Dead Area
  // West
  if (-0.88+4.56*phi<charge/pt && charge/pt<-0.28+4.56*phi) accepted = 0;
  if (0.28+4.56*phi<charge/pt && charge/pt<0.58+4.56*phi)   accepted = 0;
  if (0.52+4.86*phi<charge/pt && charge/pt<0.88+4.86*phi)   accepted = 0;
  // normal cut
  if (-2.68+4.56*phi<charge/pt && charge/pt<-2.38+4.56*phi) accepted = 0;
  // East
  if (-13.78+4.56*phi<charge/pt && charge/pt<-13.48+4.56*phi) accepted = 0;
  if (accepted) fidcut=2;

  // 30% increased
  if (-2.73+4.56*phi<charge/pt && charge/pt<-2.33+4.56*phi)  accepted = 0;
  if(-13.83+4.56*phi<charge/pt && charge/pt<-13.43+4.56*phi) accepted = 0;
  if (accepted) fidcut=3;

  // 100% increased
  if (-2.83+4.56*phi<charge/pt && charge/pt<-2.23+4.56*phi)  accepted = 0;
  if(-13.93+4.56*phi<charge/pt && charge/pt<-13.33+4.56*phi) accepted = 0;
  if (accepted) fidcut=4;

  if (phi2>phi_top_e && phi2<phi_bot_e &&
      phi1>phi_top_e && phi1<phi_bot_e) // Fall into East phi acceptance
  {
    //emcal sectors
    if(phi3>phi_top_e &&
        phi3<(phi_bot_e - phi_top_e)/4.0 + phi_top_e - emc_boundary)
    {
      phi_arm = 3; // E3
    }
    else if(phi3>(phi_bot_e - phi_top_e)/4.0 + phi_top_e  + emc_boundary&&
        phi3<2.0*(phi_bot_e - phi_top_e)/4.0 + phi_top_e - emc_boundary)
    {
      phi_arm = 2; // E2
    }
    else if(phi3>2.0*(phi_bot_e - phi_top_e)/4.0 + phi_top_e + emc_boundary&&
        phi3<3.0*(phi_bot_e - phi_top_e)/4.0 + phi_top_e - emc_boundary)
    {
      phi_arm = 1; // E1
    }
    else if(phi3>3.0*(phi_bot_e - phi_top_e)/4.0 + phi_top_e + emc_boundary &&
        phi3<phi_bot_e)
    {
      phi_arm = 0; // E0
    }
  }
  else if (phi2>phi_bot_w && phi2<phi_top_w &&
      phi1>phi_bot_w && phi1<phi_top_w)  // Fall into West phi acceptance
  {
    // emcal sectors
    if(phi3>phi_bot_w &&
        phi3<(phi_top_w - phi_bot_w)/4.0 + phi_bot_w - emc_boundary)
    {
      phi_arm = 4; // W0
    }
    else if(phi3>(phi_top_w - phi_bot_w)/4.0 + phi_bot_w + emc_boundary &&
        phi3<2.0*(phi_top_w - phi_bot_w)/4.0 + phi_bot_w - emc_boundary)
    {
      phi_arm = 5; // W1
    }
    else if(phi3>2.0*(phi_top_w - phi_bot_w)/4.0 + phi_bot_w + emc_boundary &&
        phi3<3.0*(phi_top_w - phi_bot_w)/4.0 + phi_bot_w - emc_boundary)
    {
      phi_arm = 6; // W2
    }
    else if(phi3>3.0*(phi_top_w - phi_bot_w)/4.0 + phi_bot_w + emc_boundary &&
        phi3<phi_top_w)
    {
      phi_arm = 7; // W3
    }

  }

  if (phi_arm == -1) return -4; // it hit a EMC sector boundary or outside geometric acceptance


  // --------------------- Good track ----------------------------
  return fidcut;
}
