//-----------------------------------------------------------------------------
//
// Generate lepton-pair mass distributions for Dalitz decays according
// to the Kroll-Wada parametrization
//
//-----------------------------------------------------------------------------

#include <TH1.h>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <iostream>
#include <string>
#include "CreateDalitzLeptonPairMass.h"
#include "DecayList.h"
#include "FormFactor.h"
#include "Particle.h"
#include "ParticleProperty.h"
#include "ParticlePropertyList.h"
#include "Tools.h"

TH1F* CreateDalitzLeptonPairMass(const Decay& DDalitz, const ParticlePropertyList& PPList)
{
  int ID;
  const int nbins=1000;
  double min, max, binwidth;
  double pmass, lmass, omass;
  double epsilon, delta, m_ll, q, kw_help, kroll_wada, form_factor, weight;

  ParticleProperty* PParent;
  ParticleProperty* PLepton = 0;
  ParticleProperty* POther = 0;

  PParent = PPList.GetByID(DDalitz.GetParentID());
  for ( int ibody=1; ibody<=3; ibody++ )
  {
    ID = DDalitz.GetChildID(ibody);
    if ( std::abs(ID)==11 || std::abs(ID)==13 )
      PLepton = PPList.GetByID(ID);
    else
      POther = PPList.GetByID(ID);
    if ( DDalitz.GetNBody()==4 ) POther = PPList.GetByID(22);
  }
  pmass = PParent->GetMass();
  assert(PLepton != 0 && POther != 0);
  lmass = PLepton->GetMass();
  omass = POther->GetMass();

  min = 2.0*lmass;
  max = pmass-omass;
  binwidth = (max-min)/nbins;
  TH1F * hdal = new TH1F(createString("hdal",PParent->GetID()).c_str(),
      "Dalitz",nbins,min,max);

  epsilon = (lmass/pmass)*(lmass/pmass);
  delta   = (omass/pmass)*(omass/pmass);

  for ( int ibin=1; ibin<=nbins; ibin++ )
  {
    m_ll = min + (ibin-1)*binwidth+binwidth/2.0;
    q    = (m_ll/pmass)*(m_ll/pmass);
    if ( q<=4.0*epsilon )
    {
      std::cerr << "Error in calculating Dalitz mass histogram binning!"
        << std::endl;
      hdal = 0;
      return hdal;
    }
    kw_help = (1.0+q/(1.0-delta))*(1.0+q/(1.0-delta))
      - 4.0*q/((1.0-delta)*(1.0-delta));
    if ( kw_help<=0.0 )
    {
      std::cerr << "Error in calculating Dalitz mass histogram binning!"
        << std::endl;
      hdal = 0;
      return hdal;
    }
    kroll_wada = (2.0/m_ll) * std::exp(1.5*std::log(kw_help))
      * std::sqrt(1.0-4.0*epsilon/q)
      * (1.0+2.0*epsilon/q);
    form_factor = FormFactor(m_ll*m_ll, *PParent);
    weight = kroll_wada * form_factor;
    hdal->AddBinContent(ibin,weight);
  }

  return hdal;

}
