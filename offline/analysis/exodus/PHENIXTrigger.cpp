//-----------------------------------------------------------------------------
//
//  Check whether a particle is triggered
//
//-----------------------------------------------------------------------------

#include <TF1.h>
#include <cmath>
#include "Momentum.h"
#include "PHENIXFilter.h"
#include "PHENIXTrigger.h"
#include "Particle.h"

class ParticlePropertyList;

double trigger_efficiency(const int, const double);
double double_fermi (const double*, const double*);
double double_fermi2(const double*, const double*);
double double_fermi3(const double*, const double*);
double double_fermi4(const double*, const double*);

static bool functionsInitialized = false;
bool initializeFunctions();
static TF1 *ert_eff_E0, *ert_eff_E1, *ert_eff_E2, *ert_eff_E3,
  *ert_eff_W0, *ert_eff_W1, *ert_eff_W2, *ert_eff_W3;

double PHENIXTrigger(const int fieldSetting,
    const double pt_cut, const double vtx_cut,
    const Particle& PParticle, const ParticlePropertyList& PPList)
{
  const Mom4& mom    = PParticle.Get4mom();
  const double p     = std::sqrt(mom.GetE()*mom.GetE()-mom*mom);
  const double theta = mom.Theta();
  const double pt    = p*std::sin(theta);
  const int secp     = PHENIXFilter(fieldSetting,pt_cut,vtx_cut,PParticle,PPList);

  return trigger_efficiency(secp,pt);
}

double trigger_efficiency(const int sector, const double pt)
{
  if (not functionsInitialized) initializeFunctions();

  switch(sector) {
    case 0:
      return ert_eff_E0->Eval(pt);
    case 1:
      return ert_eff_E1->Eval(pt);
    case 2:
      return ert_eff_E2->Eval(pt);
    case 3:
      return ert_eff_E3->Eval(pt);
    case 4:
      return ert_eff_W0->Eval(pt);
    case 5:
      return ert_eff_W1->Eval(pt);
    case 6:
      return ert_eff_W2->Eval(pt);
    case 7:
      return ert_eff_W3->Eval(pt);
    default:
      return 0.0;
  }
}

double double_fermi(const double *x, const double *par)
{
  if (x[0] > 0.0 && x[0] < 0.5)
  {
    return par[0]/(std::exp(-(x[0] - par[1])*par[2]) + 1.0);
  }
  return par[3]/(std::exp(-(x[0] - par[4])*par[5]) + 1.0);
}

double double_fermi2(const double *x, const double *par)
{
  if (x[0] > 0.0 && x[0] < 0.45)
  {
    return par[0]/(std::exp(-(x[0] - par[1])*par[2]) + 1.0);
  }
  return par[3]/(std::exp(-(x[0] - par[4])*par[5]) + 1.0);
}

double double_fermi3(const double *x, const double *par)
{
  if (x[0] > 0.0 && x[0] < 0.47)
  {
    return par[0]/(std::exp(-(x[0] - par[1])*par[2]) + 1.0);
  }
  return par[3]/(std::exp(-(x[0] - par[4])*par[5]) + 1.0);
}

double double_fermi4(const double *x, const double *par)
{
  if (x[0] > 0.0 && x[0] < 0.52)
  {
    return par[0]/(std::exp(-(x[0] - par[1])*par[2]) + 1.0);
  }
  return par[3]/(std::exp(-(x[0] - par[4])*par[5]) + 1.0);
}

bool initializeFunctions() {
  // helper function to initialize static objects
  static TF1 ert_eff_E0_("ert_eff_E0","[0]/(exp(-(x-[1])*[2])+1.0)",0.0,10.0);
  ert_eff_E0_.SetParameters(0.297018,0.325641,16.7253);
  ert_eff_E0 = &ert_eff_E0_;

  static TF1 ert_eff_E1_("ert_eff_E1",double_fermi2,0.0,10.0,6);
  ert_eff_E1_.SetParameters(0.382526,0.330689,19.4991,0.553251,0.394012,9.6119);
  ert_eff_E1 = &ert_eff_E1_;

  static TF1 ert_eff_E2_("ert_eff_E2",double_fermi3,0.0,10.0,6);
  ert_eff_E2_.SetParameters(0.41543,0.424667,27.7739,0.789563,0.49261,8.14709);
  ert_eff_E2 = &ert_eff_E2_;

  static TF1 ert_eff_E3_("ert_eff_E3",double_fermi2,0.0,10.0,6);
  ert_eff_E3_.SetParameters(0.465184,0.418243,24.3248,0.752594,0.469691,9.91603);
  ert_eff_E3 = &ert_eff_E3_;

  static TF1 ert_eff_W0_("ert_eff_W0",double_fermi,0.0,10.0,6);
  ert_eff_W0_.SetParameters(0.396265,0.43887,23.0604,0.675038,0.498875,9.0849);
  ert_eff_W0 = &ert_eff_W0_;

  static TF1 ert_eff_W1_("ert_eff_W1",double_fermi,0.0,10.0,6);
  ert_eff_W1_.SetParameters(0.459603,0.413129,26.597,0.886188,0.470753,6.03226);
  ert_eff_W1 = &ert_eff_W1_;

  static TF1 ert_eff_W2_("ert_eff_W2",double_fermi,0.0,10.0,6);
  ert_eff_W2_.SetParameters(0.30279,0.422319,24.3223,0.629717,0.504839,7.84686);
  ert_eff_W2 = &ert_eff_W2_;

  static TF1 ert_eff_W3_("ert_eff_W3",double_fermi4,0.0,10.0,6);
  ert_eff_W3_.SetParameters(66.6731,1.52794,6.32403,0.281022,0.562443,6.98841);
  ert_eff_W3 = &ert_eff_W3_;

  functionsInitialized = true;
  return true;
}
