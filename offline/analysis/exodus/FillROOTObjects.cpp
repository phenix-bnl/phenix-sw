//-----------------------------------------------------------------------------
//
//  Generate ROOT output file
//
//-----------------------------------------------------------------------------

#define  INCLUDEFLAG extern

#include <TH1.h>
#include <TH2.h>
#include <TH3.h>
#include <TNtuple.h>
#include <TRandom.h>
#include <gsl/gsl_math.h>
#include <cmath>
#include <cstdlib>
#include "ApplyResolution.h"
#include "DeclareROOTObjects.h"
#include "FillROOTObjects.h"
#include "Momentum.h"
#include "PHENIXFiducial.h"
#include "PHENIXFilter.h"
#include "PHENIXTrigger.h"
#include "Particle.h"
#include "ParticleIsLepton.h"
#include "ParticleList.h"
#include "ParticleProperty.h"
#include "ParticlePropertyList.h"
#include "Tools.h"

double calc_phiV(
    const double pxp, const double pyp, const double pzp,
    const double pxe, const double pye, const double pze)
{
  const double px = pxe + pxp;
  const double py = pye + pyp;
  const double pz = pze + pzp;
  const double pl = std::sqrt(px*px+py*py+pz*pz);

  //*************************************************
  //            calculation of phiV
  //*************************************************

  const double ux = px/pl;
  const double uy = py/pl;
  const double uz = pz/pl;
  //axis defined by (ux,uy,ux)X(0,0,1).
  // this is the axis that is perpendicular to the direction of
  // pair, and also perpendicular to the Z axis (field direction).
  // If the pair conversion is at R!=0, it must have (apparent)
  // momentum component in this axis (caused by field intergral from the
  // vertex point to the conversion point).
  // The sign of the component is opposite for e+ and e-.
  const double ax =  uy/std::sqrt(ux*ux+uy*uy);
  const double ay = -ux/std::sqrt(ux*ux+uy*uy);

  //vector product of pep X pem
  const double vpx  = pyp*pze - pzp*pye;
  const double vpy  = pzp*pxe - pxp*pze;
  const double vpz  = pxp*pye - pyp*pxe;
  const double vp   = std::sqrt(vpx*vpx+vpy*vpy+vpz*vpz);

  //unit vector of pep X pem
  const double vx = vpx/vp;
  const double vy = vpy/vp;
  const double vz = vpz/vp;

  //The third axis defined by vector product (ux,uy,uz)X(vx,vy,vz)
  const double wx = uy*vz - uz*vy;
  const double wy = uz*vx - ux*vz;
  // measure angle between (wx,wy,wz) and (ax,ay,0). The angle between them
  // should be small if the pair is conversion
  const double cosPhiV = wx*ax + wy*ay;
  const double phiV = std::acos(cosPhiV);

  return phiV;
}

void FillROOTObjects(const int setup, const unsigned int Nevents, const double dNdy_pi0, const double N_coll,
    const ParticleList& PList,
    const ParticlePropertyList& PPList,
    const bool fill_primaries,
    const bool fill_singles,
    const bool fill_pairs,
    const bool fill_photons)
{
  const double n_pi0_generated = Nevents;
  const double abs_norm        = dNdy_pi0; // for nucleus-nucleus: multiplicity
  const int fieldSetting = 1;
  const double pt_cut    = 0.2;
  const double vtx_cut   = 0.3;

  PLNode   * CurrentNode     = PList.GetHeadNode()->GetNextNode();
  Particle * CurrentParticle = CurrentNode->Get(0);

  PLNode* NextNode = 0;
  Particle* NextParticle = 0;
  if (PList.GetLength()>1) {
    NextNode       = CurrentNode->GetNextNode();
    NextParticle = NextNode->Get(0);
  }

  Particle* CurrentPrimary = CurrentParticle;

  int pid = 0;
  Mom4 mom, mom1, p_mom1, p_mom2;
  double weight, E, p, px, py, pz, pt, y;

  double fillweight, fillweight_inv;
  double minv, pxe, pye, pze, pxp, pyp, pzp;
  double binwidthcorr, binwidthcorrT, convprob;
  double pxpair, pypair, br_factor;
  double ptpair = 0;
  int    sece, secp;
  double trige = 0;
  double trigp = 0;
  int fide = -1;
  int fidp = -1;
  double pe, pp, the, thp, php, phe, phiV;
  double pt_parent = 0.0;
  int pide;
  for (int i=1; i<=PList.GetLength(); ++i) {
    mom1   = CurrentParticle->Get4mom();
    E      = mom1.GetE();
    px     = mom1.Getpx();
    py     = mom1.Getpy();
    pz     = mom1.Getpz();
    pt     = std::sqrt(px*px+py*py);
    y      = std::log((E+pz)/(E-pz))/2.0;

    if ( CurrentParticle->GetGeneration()==1 )
    {
      CurrentPrimary = CurrentParticle;
      pid            = CurrentParticle->GetID();
    }
    if (fill_primaries) {
      primaries->Fill(static_cast<float>(CurrentParticle->Get4mom().Abs()),
                      static_cast<float>(CurrentParticle->GetWeight()),
                      static_cast<float>(CurrentParticle->Get4mom().Mom3::Abs()),
                      static_cast<float>(CurrentParticle->Get4mom().Theta()),
                      static_cast<float>(CurrentParticle->Get4mom().Phi()),
                      static_cast<float>(CurrentParticle->GetID()));
    }

    bool final_state = false;
    if ( i != PList.GetLength() ) {
      if ( (CurrentParticle->GetGeneration()>=NextParticle->GetGeneration()) )
        final_state = true;
    } else {
      final_state = true;
    }

    if ( final_state ) {
      // parent:
      mom    = CurrentPrimary->Get4mom();
      pid    = CurrentPrimary->GetID();
      p      = std::sqrt(mom.GetE()*mom.GetE()-mom*mom);
      pt_parent = p*std::sin(mom.Theta());
      // particle:
      weight = CurrentParticle->GetWeight();
      mom1   = CurrentParticle->Get4mom();
      pide   = CurrentParticle->GetID();
      secp   = PHENIXFilter(fieldSetting,pt_cut,vtx_cut,*CurrentParticle, PPList);
      trigp = PHENIXTrigger(fieldSetting,pt_cut,vtx_cut,*CurrentParticle, PPList);
      CurrentParticle->SetAccept(static_cast<int>(secp));

      if (secp>=-10 && fill_singles) {
        //singles->Fill(weight,charge,secp,pp,thp,php,pide,p,th,ph,pid);
      }

      if ( fill_singles && (std::abs(pide)==11 || pide==21) ) {
        fillweight = abs_norm*weight/(0.5*n_pi0_generated);
        fillweight = fillweight/2.0;
        fillweight = fillweight/0.1;
        fillweight = fillweight/(2.0*M_PI);
        binwidthcorr  = 0.1/pteR->GetBinWidth(pteR->FindBin(pt));
        binwidthcorrT = 0.1/pteRT->GetBinWidth(pteRT->FindBin(pt));

        if (pt==0) pt = 0.0001;  // protection against division by zero
        fillweight_inv = fillweight/pt;

        // AuAu Run-2
        // convprob = 1.573; // first guess from photonic spectra
        // convprob = 1.17;  // update from pizero simulation
        // convprob = 1.25;  // update from pizero simulation, second try

        // AuAu Run-4
        //convprob = 0.403;

	// dAu Run-8
	convprob = 0.431;

        // pp Run-2
        // convprob = 0.730;

        // pp & dAu Run-3
        // convprob = 1.28; // stringent cuts
        // convprob = 1.37; // loose cuts

        if ( convprob<0.0 ) convprob = 0.0;

        if ( std::abs(y) <= 0.5 )
        {
          if (pid==111)
          {
            fillweight     = fillweight     * (1.+0.636/std::exp(7.74*pt_parent));
            fillweight_inv = fillweight_inv * (1.+0.636/std::exp(7.74*pt_parent));
            ptePion->Fill(pt,fillweight_inv);
            pteCPion->Fill(pt,convprob*fillweight_inv);
            pte->Fill(pt,fillweight_inv);
            pte->Fill(pt,convprob*fillweight_inv);
            pte_trig->Fill(pt,fillweight_inv*trigp);
            pte_trig->Fill(pt,convprob*fillweight_inv*trigp);
            pteC->Fill(pt,convprob*fillweight_inv);
            if (secp>=0) pte2Pion->Fill(pt,fillweight);
            pte2CPion->Fill(pt,convprob*fillweight);
            pte2->Fill(pt,fillweight);
            pte2->Fill(pt,convprob*fillweight);
            pte2C->Fill(pt,convprob*fillweight);
            pteRPion->Fill(pt,binwidthcorr*fillweight_inv);
            pteRCPion->Fill(pt,binwidthcorr*convprob*fillweight_inv);
            pteR->Fill(pt,binwidthcorr*fillweight_inv);
            pteR->Fill(pt,binwidthcorr*convprob*fillweight_inv);
            pteRC->Fill(pt,binwidthcorr*convprob*fillweight_inv);
            pteR2Pion->Fill(pt,binwidthcorr*fillweight);
            pteR2CPion->Fill(pt,binwidthcorr*convprob*fillweight);
            pteR2->Fill(pt,binwidthcorr*fillweight);
            pteR2->Fill(pt,binwidthcorr*convprob*fillweight);
            pteR2C->Fill(pt,binwidthcorr*convprob*fillweight);
            pteRTPion->Fill(pt,binwidthcorrT*fillweight_inv);
            pteRTCPion->Fill(pt,binwidthcorrT*convprob*fillweight_inv);
            pteRT->Fill(pt,binwidthcorrT*fillweight_inv);
            pteRT->Fill(pt,binwidthcorrT*convprob*fillweight_inv);
            pteRTC->Fill(pt,binwidthcorrT*convprob*fillweight_inv);
            pteRT2Pion->Fill(pt,binwidthcorrT*fillweight);
            pteRT2CPion->Fill(pt,binwidthcorrT*convprob*fillweight);
            pteRT2->Fill(pt,binwidthcorrT*fillweight);
            pteRT2->Fill(pt,binwidthcorrT*convprob*fillweight);
            pteRT2C->Fill(pt,binwidthcorrT*convprob*fillweight);
          }
          if (pid==221)
          {
            convprob = convprob/1.255;
            pteEta->Fill(pt,fillweight_inv);
            pteCEta->Fill(pt,convprob*fillweight_inv);
            pte->Fill(pt,fillweight_inv);
            pte->Fill(pt,convprob*fillweight_inv);
            pte_trig->Fill(pt,fillweight_inv*trigp);
            pte_trig->Fill(pt,convprob*fillweight_inv*trigp);
            pteC->Fill(pt,convprob*fillweight_inv);
            if (secp>=0) pte2Eta->Fill(pt,fillweight);
            pte2CEta->Fill(pt,convprob*fillweight);
            pte2->Fill(pt,fillweight);
            pte2->Fill(pt,convprob*fillweight);
            pte2C->Fill(pt,convprob*fillweight);
            pteREta->Fill(pt,binwidthcorr*fillweight_inv);
            pteRCEta->Fill(pt,binwidthcorr*convprob*fillweight_inv);
            pteR->Fill(pt,binwidthcorr*fillweight_inv);
            pteR->Fill(pt,binwidthcorr*convprob*fillweight_inv);
            pteRC->Fill(pt,binwidthcorr*convprob*fillweight_inv);
            pteR2Eta->Fill(pt,binwidthcorr*fillweight);
            pteR2CEta->Fill(pt,binwidthcorr*convprob*fillweight);
            pteR2->Fill(pt,binwidthcorr*fillweight);
            pteR2->Fill(pt,binwidthcorr*convprob*fillweight);
            pteR2C->Fill(pt,binwidthcorr*convprob*fillweight);
            pteRTEta->Fill(pt,binwidthcorrT*fillweight_inv);
            pteRTCEta->Fill(pt,binwidthcorrT*convprob*fillweight_inv);
            pteRT->Fill(pt,binwidthcorrT*fillweight_inv);
            pteRT->Fill(pt,binwidthcorrT*convprob*fillweight_inv);
            pteRTC->Fill(pt,binwidthcorrT*convprob*fillweight_inv);
            pteRT2Eta->Fill(pt,binwidthcorrT*fillweight);
            pteRT2CEta->Fill(pt,binwidthcorrT*convprob*fillweight);
            pteRT2->Fill(pt,binwidthcorrT*fillweight);
            pteRT2->Fill(pt,binwidthcorrT*convprob*fillweight);
            pteRT2C->Fill(pt,binwidthcorrT*convprob*fillweight);
          }
          if (pid==331)
          {
            convprob = convprob/3.50;
            pteEtaprime->Fill(pt,fillweight_inv);
            pteCEtaprime->Fill(pt,convprob*fillweight_inv);
            pte->Fill(pt,fillweight_inv);
            pte->Fill(pt,convprob*fillweight_inv);
            pte_trig->Fill(pt,fillweight_inv*trigp);
            pte_trig->Fill(pt,convprob*fillweight_inv*trigp);
            pteC->Fill(pt,convprob*fillweight_inv);
            if (secp>=0) pte2Etaprime->Fill(pt,fillweight);
            pte2CEtaprime->Fill(pt,convprob*fillweight);
            pte2->Fill(pt,fillweight);
            pte2->Fill(pt,convprob*fillweight);
            pte2C->Fill(pt,convprob*fillweight);
            pteREtaprime->Fill(pt,binwidthcorr*fillweight_inv);
            pteRCEtaprime->Fill(pt,binwidthcorr*convprob*fillweight_inv);
            pteR->Fill(pt,binwidthcorr*fillweight_inv);
            pteR->Fill(pt,binwidthcorr*convprob*fillweight_inv);
            pteRC->Fill(pt,binwidthcorr*convprob*fillweight_inv);
            pteR2Etaprime->Fill(pt,binwidthcorr*fillweight);
            pteR2CEtaprime->Fill(pt,binwidthcorr*convprob*fillweight);
            pteR2->Fill(pt,binwidthcorr*fillweight);
            pteR2->Fill(pt,binwidthcorr*convprob*fillweight);
            pteR2C->Fill(pt,binwidthcorr*convprob*fillweight);
            pteRTEtaprime->Fill(pt,binwidthcorrT*fillweight_inv);
            pteRTCEtaprime->Fill(pt,binwidthcorrT*convprob*fillweight_inv);
            pteRT->Fill(pt,binwidthcorrT*fillweight_inv);
            pteRT->Fill(pt,binwidthcorrT*convprob*fillweight_inv);
            pteRTC->Fill(pt,binwidthcorrT*convprob*fillweight_inv);
            pteRT2Etaprime->Fill(pt,binwidthcorrT*fillweight);
            pteRT2CEtaprime->Fill(pt,binwidthcorrT*convprob*fillweight);
            pteRT2->Fill(pt,binwidthcorrT*fillweight);
            pteRT2->Fill(pt,binwidthcorrT*convprob*fillweight);
            pteRT2C->Fill(pt,binwidthcorrT*convprob*fillweight);
          }
          if (pid==113)
          {
            pteRho->Fill(pt,fillweight_inv);
            pte->Fill(pt,fillweight_inv);
            pte_trig->Fill(pt,fillweight_inv*trigp);
            if (secp>=0) pte2Rho->Fill(pt,fillweight);
            pte2->Fill(pt,fillweight);
            pteRRho->Fill(pt,binwidthcorr*fillweight_inv);
            pteR->Fill(pt,binwidthcorr*fillweight_inv);
            pteR2Rho->Fill(pt,binwidthcorr*fillweight);
            pteR2->Fill(pt,binwidthcorr*fillweight);
            pteRTRho->Fill(pt,binwidthcorrT*fillweight_inv);
            pteRT->Fill(pt,binwidthcorrT*fillweight_inv);
            pteRT2Rho->Fill(pt,binwidthcorrT*fillweight);
            pteRT2->Fill(pt,binwidthcorrT*fillweight);
          }
          if (pid==223)
          {
            pteOmega->Fill(pt,fillweight_inv);
            pte->Fill(pt,fillweight_inv);
            pte_trig->Fill(pt,fillweight_inv*trigp);
            if (secp>=0) pte2Omega->Fill(pt,fillweight);
            pte2->Fill(pt,fillweight);
            pteROmega->Fill(pt,binwidthcorr*fillweight_inv);
            pteR->Fill(pt,binwidthcorr*fillweight_inv);
            pteR2Omega->Fill(pt,binwidthcorr*fillweight);
            pteR2->Fill(pt,binwidthcorr*fillweight);
            pteRTOmega->Fill(pt,binwidthcorrT*fillweight_inv);
            pteRT->Fill(pt,binwidthcorrT*fillweight_inv);
            pteRT2Omega->Fill(pt,binwidthcorrT*fillweight);
            pteRT2->Fill(pt,binwidthcorrT*fillweight);
          }
          if (pid==333)
          {
            ptePhi->Fill(pt,fillweight_inv);
            pte->Fill(pt,fillweight_inv);
            pte_trig->Fill(pt,fillweight_inv*trigp);
            if (secp>=0) pte2Phi->Fill(pt,fillweight);
            pte2->Fill(pt,fillweight);
            pteRPhi->Fill(pt,binwidthcorr*fillweight_inv);
            pteR->Fill(pt,binwidthcorr*fillweight_inv);
            pteR2Phi->Fill(pt,binwidthcorr*fillweight);
            pteR2->Fill(pt,binwidthcorr*fillweight);
            pteRTPhi->Fill(pt,binwidthcorrT*fillweight_inv);
            pteRT->Fill(pt,binwidthcorrT*fillweight_inv);
            pteRT2Phi->Fill(pt,binwidthcorrT*fillweight);
            pteRT2->Fill(pt,binwidthcorrT*fillweight);
          }
          if (pid==443)
          {
            pteJPsi->Fill(pt,fillweight_inv);
            pte->Fill(pt,fillweight_inv);
            pte_trig->Fill(pt,fillweight_inv*trigp);
            if (secp>=0) pte2JPsi->Fill(pt,fillweight);
            pte2->Fill(pt,fillweight);
          }
          if (pid==-111)
          {
            if ( pide==11 && ParticleIsLepton(*NextParticle) &&
                CurrentParticle->GetGeneration()==NextParticle->GetGeneration() &&
                PPList.GetByID(CurrentParticle->GetID())->GetCharge() !=
                PPList.GetByID(NextParticle->GetID())->GetCharge() )
            {
              p_mom1 = mom1;
              p_mom2 = NextParticle->Get4mom();
              pxpair = p_mom1.Getpx() + p_mom2.Getpx();
              pypair = p_mom1.Getpy() + p_mom2.Getpy();
              ptpair = sqrt(pxpair*pxpair+pypair*pypair);
            }
            if ( ptpair<0.55 )
            {
              br_factor = 1.255;
            }
            else
            {
              br_factor = 1.255+0.255*(log(ptpair/0.547)/log(0.547/0.135));
            }

            fillweight     = fillweight*N_coll/dNdy_pi0;
            fillweight_inv = fillweight_inv*N_coll/dNdy_pi0;
            pteGamma->Fill(pt,br_factor*fillweight_inv);
            pteCGamma->Fill(pt,convprob*fillweight_inv);
            pte->Fill(pt,br_factor*fillweight_inv);
            pte->Fill(pt,convprob*fillweight_inv);
            pte_trig->Fill(pt,fillweight_inv*trigp);
            pte_trig->Fill(pt,convprob*fillweight_inv*trigp);
            pteC->Fill(pt,convprob*fillweight_inv);
            pte2Gamma->Fill(pt,br_factor*fillweight);
            pte2CGamma->Fill(pt,convprob*fillweight);
            pte2->Fill(pt,br_factor*fillweight);
            pte2->Fill(pt,convprob*fillweight);
            pte2C->Fill(pt,convprob*fillweight);
            pteRGamma->Fill(pt,binwidthcorr*br_factor*fillweight_inv);
            pteRCGamma->Fill(pt,binwidthcorr*convprob*fillweight_inv);
            pteR->Fill(pt,binwidthcorr*br_factor*fillweight_inv);
            pteR->Fill(pt,binwidthcorr*convprob*fillweight_inv);
            pteRC->Fill(pt,binwidthcorr*convprob*fillweight_inv);
            pteR2Gamma->Fill(pt,binwidthcorr*br_factor*fillweight);
            pteR2CGamma->Fill(pt,binwidthcorr*convprob*fillweight);
            pteR2->Fill(pt,binwidthcorr*br_factor*fillweight);
            pteR2->Fill(pt,binwidthcorr*convprob*fillweight);
            pteR2C->Fill(pt,binwidthcorr*convprob*fillweight);
            pteRTGamma->Fill(pt,binwidthcorrT*br_factor*fillweight_inv);
            pteRTCGamma->Fill(pt,binwidthcorrT*convprob*fillweight_inv);
            pteRT->Fill(pt,binwidthcorrT*br_factor*fillweight_inv);
            pteRT->Fill(pt,binwidthcorrT*convprob*fillweight_inv);
            pteRTC->Fill(pt,binwidthcorrT*convprob*fillweight_inv);
            pteRT2Gamma->Fill(pt,binwidthcorrT*br_factor*fillweight);
            pteRT2CGamma->Fill(pt,binwidthcorrT*convprob*fillweight);
            pteRT2->Fill(pt,binwidthcorrT*br_factor*fillweight);
            pteRT2->Fill(pt,binwidthcorrT*convprob*fillweight);
            pteRT2C->Fill(pt,binwidthcorrT*convprob*fillweight);
          }
          if (pid==21)
          {
            pteKe3->Fill(pt,fillweight_inv);
            pte->Fill(pt,fillweight_inv);
            pte_trig->Fill(pt,fillweight_inv*trigp);
            pte2Ke3->Fill(pt,fillweight);
            pte2->Fill(pt,fillweight);
            pteRKe3->Fill(pt,binwidthcorr*fillweight_inv);
            pteR->Fill(pt,binwidthcorr*fillweight_inv);
            pteR2Ke3->Fill(pt,binwidthcorr*fillweight);
            pteR2->Fill(pt,binwidthcorr*fillweight);
            pteRTKe3->Fill(pt,binwidthcorrT*fillweight_inv);
            pteRT->Fill(pt,binwidthcorrT*fillweight_inv);
            pteRT2Ke3->Fill(pt,binwidthcorrT*fillweight);
            pteRT2->Fill(pt,binwidthcorrT*fillweight);
          }
        }
      }

      if ( fill_photons && pide==22 ) {
        fillweight = abs_norm*weight/(0.5*n_pi0_generated);
        fillweight = fillweight/0.1;
        fillweight = fillweight/(2.0*M_PI);
        fillweight_inv = fillweight/pt;

        if ( std::abs(y) <= 0.5 )
        {
          if (pid==111)
          {
            ptg->Fill(pt,fillweight_inv);
            ptgPion->Fill(pt,fillweight_inv);
          }
          if (pid==221)
          {
            ptg->Fill(pt,fillweight_inv);
            ptgEta->Fill(pt,fillweight_inv);
          }
          if (pid==331)
          {
            ptg->Fill(pt,fillweight_inv);
            ptgEtaprime->Fill(pt,fillweight_inv);
          }
          if (pid==223)
          {
            ptg->Fill(pt,fillweight_inv);
            ptgOmega->Fill(pt,fillweight_inv);
          }
          if (pid==333)
          {
            ptg->Fill(pt,fillweight_inv);
            ptgPhi->Fill(pt,fillweight_inv);
          }
          if (pid==-111)
          {
            fillweight_inv = fillweight_inv*N_coll/dNdy_pi0;
            ptg->Fill(pt,fillweight_inv);
            ptgGamma->Fill(pt,fillweight_inv);
          }
        }
      }
    }

    if ( i==PList.GetLength() ) break;

    if ( fill_pairs &&
        ParticleIsLepton(*CurrentParticle) &&
        ParticleIsLepton(*NextParticle) &&
        CurrentParticle->GetGeneration()==NextParticle->GetGeneration() &&
        PPList.GetByID(CurrentParticle->GetID())->GetCharge() !=
        PPList.GetByID(NextParticle->GetID())->GetCharge() )
    {
      p_mom1 = mom1;
      p_mom2 = NextParticle->Get4mom();
      ApplyResolution(setup,p_mom1);
      ApplyResolution(setup,p_mom2);

      if ( PPList.GetByID(CurrentParticle->GetID())->GetCharge()==1 )
      {
        pe  = std::sqrt(p_mom2.GetE()*p_mom2.GetE()-p_mom2*p_mom2);
        pxe = p_mom2.Getpx();
        pye = p_mom2.Getpy();
        pze = p_mom2.Getpz();
        phe = p_mom2.Phi();
        the = p_mom2.Theta();
        pp  = std::sqrt(p_mom1.GetE()*p_mom1.GetE()-p_mom1*p_mom1);
        pxp = p_mom1.Getpx();
        pyp = p_mom1.Getpy();
        pzp = p_mom1.Getpz();
        php = p_mom1.Phi();
        thp = p_mom1.Theta();
        sece = PHENIXFilter(fieldSetting,pt_cut,vtx_cut,*NextParticle,PPList);
        secp = PHENIXFilter(fieldSetting,pt_cut,vtx_cut,*CurrentParticle,PPList);
        fide = PHENIXFiducial(fieldSetting,pt_cut,vtx_cut,*NextParticle,PPList);
        fidp = PHENIXFiducial(fieldSetting,pt_cut,vtx_cut,*CurrentParticle,PPList);
        trige = PHENIXTrigger(fieldSetting,pt_cut,vtx_cut,*NextParticle,PPList);
        trigp = PHENIXTrigger(fieldSetting,pt_cut,vtx_cut,*CurrentParticle,PPList);
      }
      else
      {
        pe  = std::sqrt(p_mom1.GetE()*p_mom1.GetE()-p_mom1*p_mom1);
        pxe = p_mom1.Getpx();
        pye = p_mom1.Getpy();
        pze = p_mom1.Getpz();
        phe = p_mom1.Phi();
        the = p_mom1.Theta();
        pp  = std::sqrt(p_mom2.GetE()*p_mom2.GetE()-p_mom2*p_mom2);
        pxp = p_mom2.Getpx();
        pyp = p_mom2.Getpy();
        pzp = p_mom2.Getpz();
        php = p_mom2.Phi();
        thp = p_mom2.Theta();
        sece = PHENIXFilter(fieldSetting,pt_cut,vtx_cut,*CurrentParticle, PPList);
        secp = PHENIXFilter(fieldSetting,pt_cut,vtx_cut,*NextParticle, PPList);
        fidp = PHENIXFiducial(fieldSetting,pt_cut,vtx_cut,*NextParticle,PPList);
        fide = PHENIXFiducial(fieldSetting,pt_cut,vtx_cut,*CurrentParticle,PPList);
        trigp = PHENIXTrigger(fieldSetting,pt_cut,vtx_cut,*NextParticle,PPList);
        trige = PHENIXTrigger(fieldSetting,pt_cut,vtx_cut,*CurrentParticle,PPList);
      }
      double zvtxe = CurrentParticle->GetzVertex();      // zvertex in cm
      double zvtxp = NextParticle->GetzVertex();      // zvertex in cm

      weight     = CurrentParticle->GetWeight();
      fillweight = abs_norm*weight/(0.5*n_pi0_generated);
      fillweight = fillweight/0.025;

      const double ptele = std::sqrt(pxe*pxe + pye*pye);
      const double ptpos = std::sqrt(pxp*pxp + pyp*pyp);

      // p+p
      const double effe =  9.90749e-01 - 2.44424e-01/ptele + 9.62876e-03/ptele/ptele - 2.59690e-02*ptele + std::exp(-4.77995e-01 - 2.00747*ptele);
      const double effp = -6.62125e-01 - 4.00118e-02/ptpos - 4.31929e-03/ptpos/ptpos - 3.48514e-03*ptpos + std::exp( 4.17003e-01 + 5.60212e-09*ptpos);
      // Au+Au
      // const double effe =  8.24281e-01 - 1.68411e-01/ptele + 8.12107e-04/ptele/ptele - 3.24261e-02*ptele + std::exp(-3.40062e-01 - 3.14082*ptele);
      // const double effp =  8.17995e-01 - 1.81857e-01/ptpos + 8.88055e-04/ptpos/ptpos - 3.00694e-02*ptpos + std::exp(-2.35679e-01 - 3.14834*ptpos);
      //CuCu lg sim (Sarah Campbell)
      // const double effe =  0.5264 - 7.5e-05/ptele - 0.0193/ptele/ptele + 0.0023*ptele + std::exp(-1.78 - 0.0432*ptele);
      // const double effp =  0.6603 + 0.0463/ptpos - 0.0381/ptpos/ptpos + 0.0017*ptpos + std::exp( -1*(1e-08 + 7.0449*ptpos));

      acc_exodusphi->Fill(php, 1./ptpos,fillweight);
      acc_exodusphi->Fill(phe,-1./ptele,fillweight);
      php = phiPHENIX(php);
      phe = phiPHENIX(phe);

      // get phi at the EMC (PC3) to check for tower overlaps
      const double sl3 = 0.275; // PC3 cut
      const double phe3 = phe + sl3/ptele; // PHENIX -pi/2 to 3pi/2 (phe already converted)
      const double php3 = php - sl3/ptpos; // PHENIX -pi/2 to 3pi/2 (php already converted)

      if (secp>=0) acc->Fill(php, 1./ptpos,fillweight);
      if (sece>=0) acc->Fill(phe,-1./ptele,fillweight);
      if (secp>=0) acc_the0->Fill(zvtxp,thp,fillweight);
      if (sece>=0) acc_the0->Fill(zvtxe,the,fillweight);

      if (fidp>=1) acc_fid1->Fill(php, 1./ptpos,fillweight);
      if (fide>=1) acc_fid1->Fill(phe,-1./ptele,fillweight);
      if (fidp>=1) acc_the0_fid->Fill(zvtxp,thp,fillweight);
      if (fide>=1) acc_the0_fid->Fill(zvtxe,the,fillweight);

      if (fidp>=2) acc_fid2->Fill(php, 1./ptpos,fillweight);
      if (fide>=2) acc_fid2->Fill(phe,-1./ptele,fillweight);

      if (fidp>=3) acc_fid3->Fill(php, 1./ptpos,fillweight);
      if (fide>=3) acc_fid3->Fill(phe,-1./ptele,fillweight);

      if (fidp>=4) acc_fid4->Fill(php, 1./ptpos,fillweight);
      if (fide>=4) acc_fid4->Fill(phe,-1./ptele,fillweight);

      // now i calculate the pair propoerties
      E  = p_mom1.GetE() + p_mom2.GetE();
      px = pxe + pxp;
      py = pye + pyp;
      pz = pze + pzp;
      pt = std::sqrt(px*px+py*py);
      y  = std::log((E+pz)/(E-pz))/2.0;
      phiV = calc_phiV(pxp,pyp,pzp,pxe,pye,pze);
      minv = std::sqrt((p_mom1+p_mom2)*(p_mom1+p_mom2));
      weight     = CurrentParticle->GetWeight();
      //this 0.5 below is due to the fact that exodus generates particles in rapidity from
      //-1 to +1 but dN/dY is from -0.5 to 0.5  -> Does this make sense??? 
      fillweight = abs_norm*weight/(0.5*n_pi0_generated);
      //fillweight = fillweight/0.010;//My bin width correction.

      // calculate distance of tracks at EMC
      // don't have tower information, but with
      // the radius at the EMC of (510cm)
      // one can calculate the distance in cm
      // 1 PbSc tower has a diameter of 5.535 cm
      // 1 PbGl tower has a diameter of 4.00 cm
      const double ysecte = 510.0*std::sin(phe3);
      const double zsecte = 510.0*std::cos(the);
      const double ysectp = 510.0*std::sin(php3);
      const double zsectp = 510.0*std::cos(thp);
      double EMCdistance = 100.0;
      if (sece==secp) EMCdistance = std::sqrt((ysecte - ysectp)*(ysecte - ysectp) + (zsecte - zsectp)*(zsecte - zsectp));

      // now we calculate the pair properties with angular smearing

      const double dphi_mean_e =   0.002*(0.021 + 0.16/(pe*pe));
      const double dphi_mean_p =  -0.002*(0.021 + 0.16/(pp*pp));

      const double sigphe = 0.0023*std::sqrt(5.1 +0.46/(pe*pe*pe));
      const double sigphp = 0.0023*std::sqrt(5.1 +0.46/(pp*pp*pp));
      const double sigthe = 0.001 *std::sqrt(0.54+0.36/(pe*pe*pe));
      const double sigthp = 0.001 *std::sqrt(0.54+0.36/(pp*pp*pp));

      const double pe_rec  = pe;
      const double pp_rec  = pp;
      const double phe_rec = phe + gRandom->Gaus(dphi_mean_e,sigphe);
      const double the_rec = the + gRandom->Gaus(0,sigthe);
      const double php_rec = php + gRandom->Gaus(dphi_mean_p,sigphp);
      const double thp_rec = thp + gRandom->Gaus(0,sigthp);

      const double pxe_rec = pe_rec*std::sin(the_rec)*std::cos(phe_rec);
      const double pye_rec = pe_rec*std::sin(the_rec)*std::sin(phe_rec);
      const double pze_rec = pe_rec*std::cos(the_rec);

      const double pxp_rec = pp_rec*std::sin(thp_rec)*std::cos(php_rec);
      const double pyp_rec = pp_rec*std::sin(thp_rec)*std::sin(php_rec);
      const double pzp_rec = pp_rec*std::cos(thp_rec);

      const double phiV_rec = calc_phiV(pxp_rec,pyp_rec,pzp_rec,
          pxe_rec,pye_rec,pze_rec);

      // these are the pairs before acceptance
      if ( std::abs(y)<=0.5 ) // pair-rapidity
      {
        mee_accP->Fill(minv,fillweight);
        mptee_accP->Fill(minv,pt,fillweight);
      }

      if ( sece>=0 && secp>=0 && std::abs(y)<=0.5 ) // pair-rapidity and  acceptance
      {
        mee_accS->Fill(minv,fillweight); // is in PHENIX aperture
        mptee_accS->Fill(minv,pt,fillweight);

        if (EMCdistance < 11.7) // 2 tower separation
          mptee_EMCghost->Fill(minv,pt,fillweight);

        if (fide>=1 && fidp>=1)
        {
          mptee_accS_fid1->Fill(minv,pt,fillweight); // didn't survive normal fiducial cut
          mptee_accS_eid_fid1->Fill(minv,pt,fillweight*effe*effp);
        }
        if (fide>=2 && fidp>=2)
        {
          mptee_accS_fid2->Fill(minv,pt,fillweight); // didn't survive normal fiducial cut
          mptee_accS_eid_fid2->Fill(minv,pt,fillweight*effe*effp);
        }
        if (fide>=3 && fidp>=3)
        {
          mptee_accS_fid3->Fill(minv,pt,fillweight); // didn't survive 30% larger fiducial cut
          mptee_accS_eid_fid3->Fill(minv,pt,fillweight*effe*effp);
        }
        if (fide>=4 && fidp>=4)
        {
          mptee_accS_fid4->Fill(minv,pt,fillweight); // didn't survived 100% larger fiducial cut
          mptee_accS_eid_fid4->Fill(minv,pt,fillweight*effe*effp);
        }
        mptee_eid->Fill(minv,pt,fillweight*effe*effp);
        //mptee_eid->Fill(mass_rec,pTrec,fillweight*effe*effp);
        mptphiVee->Fill(pt,minv,phiV,fillweight);
        mptphiVee_rec->Fill(pt,minv,phiV_rec,fillweight);

        if ( pid==111 )
        {
          mee->Fill(minv,fillweight);
          mptee->Fill(minv,pt,fillweight);
          mptee_Pion->Fill(minv,pt,fillweight);
          mptee_ert->Fill(minv,pt,fillweight*(1-(1-trige)*(1-trigp)));
          ptyeePion->Fill(y,pt,fillweight);
          mphiVee->Fill(phiV,minv,fillweight);
          meePion->Fill(minv,fillweight);
        }
        if ( pid==221 )
        {
          mee->Fill(minv,fillweight);
          mptee->Fill(minv,pt,fillweight);
          mptee_Eta->Fill(minv,pt,fillweight);
          mptee_ert->Fill(minv,pt,fillweight*(1-(1-trige)*(1-trigp)));
          ptyeeEta->Fill(y,pt,fillweight);
          mphiVee->Fill(phiV,minv,fillweight);
          meeEta->Fill(minv,fillweight);
        }
        if ( pid==331 )
        {
          mee->Fill(minv,fillweight);
          mptee->Fill(minv,pt,fillweight);
          mptee_Etaprime->Fill(minv,pt,fillweight);
          mptee_ert->Fill(minv,pt,fillweight*(1-(1-trige)*(1-trigp)));
          ptyeeEtaprime->Fill(y,pt,fillweight);
          mphiVee->Fill(phiV,minv,fillweight);
          meeEtaprime->Fill(minv,fillweight);
        }
        if ( pid==113 )
        {
          mee->Fill(minv,fillweight);
          mptee->Fill(minv,pt,fillweight);
          mptee_Rho->Fill(minv,pt,fillweight);
          mptee_ert->Fill(minv,pt,fillweight*(1-(1-trige)*(1-trigp)));
          ptyeeRho->Fill(y,pt,fillweight);
          mphiVee->Fill(phiV,minv,fillweight);
          meeRho->Fill(minv,fillweight);
        }
        if ( pid==223 )
        {
          // for p+p adjust
          const double omegacorr = 1.0; // for Au+Au and also for p+p
          mee->Fill(minv,fillweight*omegacorr);
          mptee->Fill(minv,pt,fillweight*omegacorr);
          mptee_Omega->Fill(minv,pt,fillweight*omegacorr);
          mptee_ert->Fill(minv,pt,fillweight*omegacorr*(1-(1-trige)*(1-trigp)));
          ptyeeOmega->Fill(y,pt,fillweight*omegacorr);
          mphiVee->Fill(phiV,minv,fillweight*omegacorr);
          meeOmega->Fill(minv,fillweight*omegacorr);
        }
        if ( pid==333 )
        {
          // for Au+Au adjust to ppg016
          const double phicorr = 1.0; // for p+p
          mee->Fill(minv,fillweight*phicorr);
          mptee->Fill(minv,pt,fillweight*phicorr);
          mptee_Phi->Fill(minv,pt,fillweight*phicorr);
          mptee_ert->Fill(minv,pt,fillweight*phicorr*(1-(1-trige)*(1-trigp)));
          ptyeePhi->Fill(y,pt,fillweight);
          mphiVee->Fill(phiV,minv,fillweight*phicorr);
          meePhi->Fill(minv,fillweight*phicorr);
          ptPhi->Fill(pt,(fillweight)*phicorr/(2*M_PI*pt));
        }
        if ( pid==443 )
        {
          // for Au+Au adjust to ppg068
          const double jpsicorr =1.0; // for p+p
          mee->Fill(minv,fillweight*jpsicorr);
          mptee->Fill(minv,pt,fillweight*jpsicorr);
          mptee_JPsi->Fill(minv,pt,fillweight*jpsicorr);
          mptee_ert->Fill(minv,pt,fillweight*jpsicorr*(1-(1-trige)*(1-trigp)));
          ptyeeJPsi->Fill(y,pt,fillweight);
          mphiVee->Fill(phiV,minv,fillweight*jpsicorr);
          meeJPsi->Fill(minv,fillweight*jpsicorr);
          ptJPsi->Fill(pt,(fillweight)*jpsicorr/(2*M_PI*pt));
        }
        if ( pid==444 )
        {
          mee->Fill(minv,fillweight);
          mptee->Fill(minv,pt,fillweight);
          mptee_Psiprime->Fill(minv,pt,fillweight);
          mptee_ert->Fill(minv,pt,fillweight*(1-(1-trige)*(1-trigp)));
          ptyeePsiprime->Fill(y,pt,fillweight);
          mphiVee->Fill(phiV,minv,fillweight);
          meePsiprime->Fill(minv,fillweight);
        }
        if ( pid==553 )
        {
          mee->Fill(minv,fillweight);
          mptee->Fill(minv,pt,fillweight);
          mptee_Upsilon1S->Fill(minv,pt,fillweight);
          mptee_ert->Fill(minv,pt,fillweight*(1-(1-trige)*(1-trigp)));
          mphiVee->Fill(phiV,minv,fillweight);
          meeUpsilon1S->Fill(minv,fillweight);
        }
        if ( pid==100553 )
        {
          mee->Fill(minv,fillweight);
          mptee->Fill(minv,pt,fillweight);
          mptee_Upsilon2S->Fill(minv,pt,fillweight);
          mptee_ert->Fill(minv,pt,fillweight*(1-(1-trige)*(1-trigp)));
          mphiVee->Fill(phiV,minv,fillweight);
          meeUpsilon2S->Fill(minv,fillweight);
        }
        if ( pid==200553 )
        {
          mee->Fill(minv,fillweight);
          mptee->Fill(minv,pt,fillweight);
          mptee_Upsilon3S->Fill(minv,pt,fillweight);
          mptee_ert->Fill(minv,pt,fillweight*(1-(1-trige)*(1-trigp)));
          mphiVee->Fill(phiV,minv,fillweight);
          meeUpsilon3S->Fill(minv,fillweight);
        }
      }

      /*
      double the, phe; // TODO remove after refactoring
      double opangle = std::acos(std::abs(
            (p_mome->Getpx() * p_momp->Getpx() +
             p_mome->Getpy() * p_momp->Getpy() +
             p_mome->Getpz() * p_momp->Getpz() )/
            (std::pow(p_mome->Mom3::Abs() *
                      p_momp->Mom3::Abs(), 2) ) ) );
      if (std::isnan(opangle)) opangle = M_PI_2;

      float pairs_data[] = {
        minv,
        opangle,
        weight,
        sece,
        pe,
        the = p_mome->Theta(),
        phe = p_mome->Phi(),
        secp,
        pp,
        thp = p_momp->Theta(),
        php = p_momp->Phi(),
        p,
        th,
        ph,
        pid
      };
      */
      //pairs->Fill(pairs_data);
    }

    CurrentNode     = NextNode;
    CurrentParticle = NextParticle;
    if ( i<PList.GetLength()-1 )
    {
      NextNode = CurrentNode->GetNextNode();
      NextParticle    = NextNode->Get(0);
    }

  }
}
