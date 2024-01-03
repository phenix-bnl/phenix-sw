#include "PHENIXFilter.h"
#include <cmath>
#include "Momentum.h"
#include "Particle.h"
#include "ParticleProperty.h"
#include "ParticlePropertyList.h"
#include "Tools.h"

int PHENIXFilter(const int fieldSetting, const double pt_cut, const double vtx_cut,
    const Particle& PParticle, const ParticlePropertyList& PPList)
{
  // ------------- initialize ------------------------------
  // Phi acceptance of each arm
  double phi_bot_w = -0.589; // MC info form dcgeom96.f
  double phi_top_w =  0.982;
  double phi_bot_e =  3.731; // MC info form dcgeom96.f
  double phi_top_e =  2.160;
  // NOTE: Exodus generates 0 .. 2pi, PHENIX convention -pi/2 .. 3pi/2

  // Theta acceptance
  const double theta_min = 1.23;  // From data
  const double theta_max = 1.92;
  // slopes for each line tuned ++ field (Run-4/5/6)
  double sl1 =  0.309;    // CRK cut
  double sl2 =  0.206;    // DCH cut
  double sl3 =  0.275;    // PC3 cut
  double slz =  0.004224; // Z_theta slope

  // old values for for + field
  if (fieldSetting==2) {
    sl1 = 0.235; // CRK cut
    sl2 = 0.155; // DCH cut
    // PC3 cut???
    slz = 0.0038; // Z_theta slope
  }
  // values for +- field
  if (fieldSetting==3) {
    sl1 = 0.118;  // CRK cut
    sl2 = 0.060;  // DCH cut
    sl3 = sl1;
    slz = 0.0041; // Z_theta slope

    phi_bot_w = -0.570;
    phi_top_w =  0.983;
    phi_bot_e =  3.718;
    phi_top_e =  2.153;
  }
  // --------------- get particle kinematics ------------------

  const Mom4& mom   = PParticle.Get4mom();
  const int  pid    = PParticle.GetID();
  const int  charge = PPList.GetByID(pid)->GetCharge();

  const double px = mom.Getpx();
  const double py = mom.Getpy();
  const double pt = hypot(px, py); // Calculate pt
  if (pt < pt_cut) return -2;           // Lower pt cut

  const double zvtx = PParticle.GetzVertex();      // zvertex in cm
  if (std::abs(zvtx) > vtx_cut) return -10;          // Vertex cut

  if (fieldSetting==5) {
    const double pz = mom.Getpz();
    const double E  = mom.GetE();
    const double y  = 0.5*std::log((E+pz)/(E-pz));
    return (std::abs(y)<1) ? 1 : -1;
  }

  // --------------- ACCEPTANCE CHECK -------------------------
  const double theta  = mom.Theta();

  // --------------------- Theta acceptance ----------------------
  const double th1 = theta-zvtx*slz;
  if (th1>theta_max || th1<theta_min) return -3;
  // --------------------- Phi acceptance ----------------------
  int phi_arm = -1;         //  Flag to fall in phi acceptance
  const double q_pt = charge/pt;
  const double phi  = phiPHENIX(mom.Phi());
  const double phi1 = phiPHENIX(phi-q_pt*sl1);
  const double phi2 = phiPHENIX(phi-q_pt*sl2);
  const double phi3 = phiPHENIX(phi-q_pt*sl3);

  if (phi2>phi_top_e && phi2<phi_bot_e &&
      phi1>phi_top_e && phi1<phi_bot_e) // Fall into East phi acceptance
  {
    //emcal sectors
    if(phi3>phi_top_e &&
        phi3<(phi_bot_e - phi_top_e)/4.0 + phi_top_e)
    {
      phi_arm = 3; // E3
    }
    else if(phi3>(phi_bot_e - phi_top_e)/4.0 + phi_top_e &&
        phi3<2.0*(phi_bot_e - phi_top_e)/4.0 + phi_top_e)
    {
      phi_arm = 2; // E2
    }
    else if(phi3>2.0*(phi_bot_e - phi_top_e)/4.0 + phi_top_e&&
        phi3<3.0*(phi_bot_e - phi_top_e)/4.0 + phi_top_e)
    {
      phi_arm = 1; // E1
    }
    else if(phi3>3.0*(phi_bot_e - phi_top_e)/4.0 + phi_top_e &&
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
        phi3<(phi_top_w - phi_bot_w)/4.0 + phi_bot_w)
    {
      phi_arm = 4; // W0
    }
    else if(phi3>(phi_top_w - phi_bot_w)/4.0 + phi_bot_w &&
        phi3<2.0*(phi_top_w - phi_bot_w)/4.0 + phi_bot_w)
    {
      phi_arm = 5; // W1
    }
    else if(phi3>2.0*(phi_top_w - phi_bot_w)/4.0 + phi_bot_w &&
        phi3<3.0*(phi_top_w - phi_bot_w)/4.0 + phi_bot_w)
    {
      phi_arm = 6; // W2
    }
    else if(phi3>3.0*(phi_top_w - phi_bot_w)/4.0 + phi_bot_w &&
        phi3<phi_top_w)
    {
      phi_arm = 7; // W3
    }
  }
  if (phi_arm == -1) return -4;

  // --------------------- Good track ----------------------------
  return phi_arm;
}
