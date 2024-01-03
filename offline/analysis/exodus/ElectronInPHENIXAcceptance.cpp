#include <TMath.h>
#include <cmath>
#include <limits>
#include "ElectronInPHENIXAcceptance.h"

int ElectronInPHENIXAcceptance(const double px, const double py, const double pz,
    const double vtx, const double charge,
    const double min_pt_cut,
    const double vtx_cut)
{
  // Declaration of the constants:
  // Phi acceptance of each arm
  const double phi_bot_w = -0.589; // MC info form dcgeom96.f
  const double phi_top_w =  0.982;
  const double phi_bot_e =  3.731; // MC info form dcgeom96.f
  const double phi_top_e =  2.160;
  // Theta acceptance
  const double theta_min =  1.22;  // From data
  const double theta_max =  1.92;

  // slopes for each line
  const double sl1       =  0.235; // PC3 cut
  const double sl2       =  0.155; // DCH cut

  const double slz       =  0.0038; // Z_theta slope

  double mom    = std::sqrt(px*px+py*py);                // Calculate pt
  if (mom < std::numeric_limits<double>::epsilon()) return -1;

  double phi0   = TMath::Pi()/2. - std::atan2(px,py);
  double theta0 = std::acos(pz/std::sqrt(px*px+py*py+pz*pz));

  if (std::abs(vtx) > vtx_cut) return -1;                // Vertex cut
  if (std::abs(1./mom) > 1./min_pt_cut) return -1;       // Low mom cut

  // --------------------- Phi acceptance ----------------------

  int phi_arm = -1;              //  Flag to fall in phii acceptance

  double phi1 = phi0+(charge/mom)*sl1;
  double phi2 = phi0+(charge/mom)*sl2;

  // upon those transformation we obtain regular rectangular regions in acceptance

  if (phi1>phi_bot_w && phi1<phi_top_w && phi2>phi_bot_w && phi2<phi_top_w)
    phi_arm = 1; // Fall into West phi acceptance
  if (phi1>phi_top_e && phi1<phi_bot_e && phi2>phi_top_e && phi2<phi_bot_e)
    phi_arm = 0; // Fall into East phi acceptance

  if (phi_arm == -1) return -1;

  // --------------------- Theta acceptance ----------------------

  double th1 = theta0-vtx*slz;

  if (th1>theta_max || th1<theta_min) return -1;

  // --------------------- Good track ----------------------------

  return phi_arm;
}
