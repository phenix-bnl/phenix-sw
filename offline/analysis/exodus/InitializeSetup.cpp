//-----------------------------------------------------------------------------
//
//  Initialize the ParticleGeneratorList according to the chosen setup
//
//-----------------------------------------------------------------------------

#include "InitializeExpPt.h"
#include "InitializeM.h"
#include "InitializePtCERES.h"
#include "InitializePtISR.h"
#include "InitializePtPHENIX.h"
#include "InitializeSetup.h"
#include "InitializeWhiteY.h"
#include "InitializeYCERES.h"
#include "InitializeYISR.h"
#include "InitializeYPHENIX.h"
#include "Particle.h"
#include "ParticleGenerator.h"
#include "ParticleGeneratorList.h"
#include "ParticleProperty.h"
#include "ParticlePropertyList.h"

#include <boost/lexical_cast.hpp>

#include <iostream>
#include <memory>
#include <string>

ParticleGeneratorList * InitializeSetup(const int setup,
    const ParticlePropertyList& PPList)
{
  std::auto_ptr<ParticleGeneratorList> PGList (new ParticleGeneratorList);
  std::auto_ptr<ParticleGenerator> Pion       (new ParticleGenerator(111));
  std::auto_ptr<ParticleGenerator> directPion (new ParticleGenerator(-111));
  std::auto_ptr<ParticleGenerator> Ke3        (new ParticleGenerator(21));
  std::auto_ptr<ParticleGenerator> Eta        (new ParticleGenerator(221));
  std::auto_ptr<ParticleGenerator> Etaprime   (new ParticleGenerator(331));
  std::auto_ptr<ParticleGenerator> Rho        (new ParticleGenerator(113));
  std::auto_ptr<ParticleGenerator> Omega      (new ParticleGenerator(223));
  std::auto_ptr<ParticleGenerator> Phi        (new ParticleGenerator(333));
  std::auto_ptr<ParticleGenerator> Piplus     (new ParticleGenerator(211));
  std::auto_ptr<ParticleGenerator> Piminus    (new ParticleGenerator(-211));
  std::auto_ptr<ParticleGenerator> Kplus      (new ParticleGenerator(321));
  std::auto_ptr<ParticleGenerator> Kminus     (new ParticleGenerator(-321));
  std::auto_ptr<ParticleGenerator> Proton     (new ParticleGenerator(2212));
  std::auto_ptr<ParticleGenerator> Antiproton (new ParticleGenerator(-2212));
  std::auto_ptr<ParticleGenerator> JPsi       (new ParticleGenerator(443));
  std::auto_ptr<ParticleGenerator> Psiprime   (new ParticleGenerator(444));
  std::auto_ptr<ParticleGenerator> Upsilon    (new ParticleGenerator(553));
  std::auto_ptr<ParticleGenerator> Upsilon2S  (new ParticleGenerator(100553));
  std::auto_ptr<ParticleGenerator> Upsilon3S  (new ParticleGenerator(200553));

  int    pt_setup  = 1;
  double mass, inv_slope;
  double f_c  = 1.;
  double f_p0 = 1.;
  double f_a = 1.;
  double f_b = 1.;
  double f_n = 1.;
  double t = 1.;
  double w = 1.;
  double A = 1.;
  double p0 = 1.;
  double m = 1.;
  double B = 1.;
  double n = 1.;

  std::string line;
  switch(setup)
  {
    case 1:  std::cout << "Initializing CERES" << std::endl;

             Pion->SetWeight(1.0);
             Eta->SetWeight(0.053);
             Etaprime->SetWeight(0.009);
             Rho->SetWeight(0.065);
             Omega->SetWeight(0.065);
             Phi->SetWeight(0.0033);

             Pion->SetPtHistogram(InitializePtCERES(Pion->GetID(),PPList));
             Eta->SetPtHistogram(InitializePtCERES(Eta->GetID(),PPList));
             Etaprime->SetPtHistogram(InitializePtCERES(Etaprime->GetID(),PPList));
             Rho->SetPtHistogram(InitializePtCERES(Rho->GetID(),PPList));
             Omega->SetPtHistogram(InitializePtCERES(Omega->GetID(),PPList));
             Phi->SetPtHistogram(InitializePtCERES(Phi->GetID(),PPList));

             Pion->SetYHistogram(InitializeYCERES(Pion->GetID(),PPList));
             Eta->SetYHistogram(InitializeYCERES(Eta->GetID(),PPList));
             Etaprime->SetYHistogram(InitializeYCERES(Etaprime->GetID(),PPList));
             Rho->SetYHistogram(InitializeYCERES(Rho->GetID(),PPList));
             Omega->SetYHistogram(InitializeYCERES(Omega->GetID(),PPList));
             Phi->SetYHistogram(InitializeYCERES(Phi->GetID(),PPList));

             Rho->SetMHistogram(InitializeM(Rho->GetID(),0.,0.,PPList));
             Omega->SetMHistogram(InitializeM(Omega->GetID(),0.,0.,PPList));
             Phi->SetMHistogram(InitializeM(Phi->GetID(),0.,0.,PPList));

             PGList->Insert(Pion.release());
             PGList->Insert(Eta.release());
             PGList->Insert(Etaprime.release());
             PGList->Insert(Rho.release());
             PGList->Insert(Omega.release());
             PGList->Insert(Phi.release());

             break;

    case 2:  std::cout << "Initializing ISR" << std::endl;

             Pion->SetWeight(1.0);
             Eta->SetWeight(0.053);
             Etaprime->SetWeight(0.009);
             Rho->SetWeight(0.065);
             Omega->SetWeight(0.065);
             Phi->SetWeight(0.0033);

             Pion->SetPtHistogram(InitializePtISR(Pion->GetID(),PPList));
             Eta->SetPtHistogram(InitializePtISR(Eta->GetID(),PPList));
             Etaprime->SetPtHistogram(InitializePtISR(Etaprime->GetID(),PPList));
             Rho->SetPtHistogram(InitializePtISR(Rho->GetID(),PPList));
             Omega->SetPtHistogram(InitializePtISR(Omega->GetID(),PPList));
             Phi->SetPtHistogram(InitializePtISR(Phi->GetID(),PPList));

             Pion->SetYHistogram(InitializeYISR(Pion->GetID(),PPList));
             Eta->SetYHistogram(InitializeYISR(Eta->GetID(),PPList));
             Etaprime->SetYHistogram(InitializeYISR(Etaprime->GetID(),PPList));
             Rho->SetYHistogram(InitializeYISR(Rho->GetID(),PPList));
             Omega->SetYHistogram(InitializeYISR(Omega->GetID(),PPList));
             Phi->SetYHistogram(InitializeYISR(Phi->GetID(),PPList));

             Rho->SetMHistogram(InitializeM(Rho->GetID(),0.,0.,PPList));
             Omega->SetMHistogram(InitializeM(Omega->GetID(),0.,0.,PPList));
             Phi->SetMHistogram(InitializeM(Phi->GetID(),0.,0.,PPList));

             PGList->Insert(Pion.release());
             PGList->Insert(Eta.release());
             PGList->Insert(Etaprime.release());
             PGList->Insert(Rho.release());
             PGList->Insert(Omega.release());
             PGList->Insert(Phi.release());

             break;

    case 3:  std::cout << "Initializing PHENIX" << std::endl << std::endl;

             do
             {
               std::cout << "Choose the shape of the pt distribution:"
                 << std::endl;
               std::cout << "----------------------------------------"
                 << std::endl;
               std::cout << std::endl;
               std::cout << "1) power law" << std::endl;
               std::cout << "2) exponential (with flow)" << std::endl;
               std::cout << "3) Run-2 AuAu" << std::endl;
               std::cout << "4) Run-2 pp" << std::endl;
               std::cout << "5) Run-8 dAu" << std::endl;
               std::cout << std::endl;
               std::cout << "Your choice (1-5): ";
               std::getline(std::cin, line);
               pt_setup = boost::lexical_cast<int>(line);
               std::cout << std::endl;
             } while ( pt_setup<1 || pt_setup>5 );

             std::cout << "Pion weight: ";
             std::getline(std::cin, line);
             std::cout << std::endl;
             Pion->SetWeight(boost::lexical_cast<double>(line));
             std::cout << "Eta weight: ";
             std::getline(std::cin, line);
             std::cout << std::endl;
             Eta->SetWeight(boost::lexical_cast<double>(line));
             std::cout << "Etaprime weight: ";
             std::getline(std::cin, line);
             std::cout << std::endl;
             Etaprime->SetWeight(boost::lexical_cast<double>(line));
             std::cout << "Rho weight: ";
             std::getline(std::cin, line);
             std::cout << std::endl;
             Rho->SetWeight(boost::lexical_cast<double>(line));
             std::cout << "Omega weight: ";
             std::getline(std::cin, line);
             std::cout << std::endl;
             Omega->SetWeight(boost::lexical_cast<double>(line));
             std::cout << "Phi weight: ";
             std::getline(std::cin, line);
             std::cout << std::endl;
             Phi->SetWeight(boost::lexical_cast<double>(line));
             std::cout << "J/Psi weight: ";
             std::getline(std::cin, line);
             std::cout << std::endl;
             JPsi->SetWeight(boost::lexical_cast<double>(line));
             std::cout << "Psiprime weight: ";
             std::getline(std::cin, line);
             std::cout << std::endl;
             Psiprime->SetWeight(boost::lexical_cast<double>(line));
             std::cout << "Direct Photon weight: ";
             std::getline(std::cin, line);
             std::cout << std::endl;
             directPion->SetWeight(boost::lexical_cast<double>(line));
             std::cout << "Ke3 weight: ";
             std::getline(std::cin, line);
             std::cout << std::endl;
             Ke3->SetWeight(boost::lexical_cast<double>(line));
             std::cout << "Upsilon1S weight: ";
             std::getline(std::cin, line);
             std::cout << std::endl;
             Upsilon->SetWeight(boost::lexical_cast<double>(line));
             std::cout << "Upsilon2S weight: ";
             std::getline(std::cin, line);
             std::cout << std::endl;
             Upsilon2S->SetWeight(boost::lexical_cast<double>(line));
             std::cout << "Upsilon3S weight: ";
             std::getline(std::cin, line);
             std::cout << std::endl;
             Upsilon3S->SetWeight(boost::lexical_cast<double>(line));

             if (pt_setup>=1 and pt_setup<=4) {
               std::cout << "f_c: ";
               std::getline(std::cin, line);
               f_c = boost::lexical_cast<double>(line);
               std::cout << std::endl;
               std::cout << "f_p0: ";
               std::getline(std::cin, line);
               f_p0 = boost::lexical_cast<double>(line);
               std::cout << std::endl;
               std::cout << "f_a: ";
               std::getline(std::cin, line);
               f_a = boost::lexical_cast<double>(line);
               std::cout << std::endl;
               std::cout << "f_b: ";
               std::getline(std::cin, line);
               f_b = boost::lexical_cast<double>(line);
               std::cout << std::endl;
               std::cout << "f_n: ";
               std::getline(std::cin, line);
               f_n = boost::lexical_cast<double>(line);
               std::cout << std::endl;
             }
             if (pt_setup==5) {
               std::cout << "t: ";
               std::getline(std::cin, line);
               t = boost::lexical_cast<double>(line);
               std::cout<< std::endl;
               std::cout << "w: ";
               std::getline(std::cin, line);
               w = boost::lexical_cast<double>(line);
               std::cout << std::endl;
               std::cout << "A: ";
               std::getline(std::cin, line);
               A = boost::lexical_cast<double>(line);
               std::cout << std::endl;
               std::cout << "p0: ";
               std::getline(std::cin, line);
               p0 = boost::lexical_cast<double>(line);
               std::cout << std::endl;
               std::cout << "m: ";
               std::getline(std::cin, line);
               m = boost::lexical_cast<double>(line);
               std::cout << std::endl;
               std::cout << "B: ";
               std::getline(std::cin, line);
               B = boost::lexical_cast<double>(line);
               std::cout << std::endl;
               std::cout << "n: ";
               std::getline(std::cin, line);
               n = boost::lexical_cast<double>(line);
               std::cout << std::endl;
             }

             Pion->SetPtHistogram(InitializePtPHENIX(pt_setup,Pion->GetID(),
                   f_c, f_p0, f_a, f_b, f_n,
                   t, w, A, p0, m, B, n,
                   PPList));
             directPion->SetPtHistogram(InitializePtPHENIX(pt_setup,directPion->GetID(),
                   f_c, f_p0, f_a, f_b, f_n,
                   t, w, A, p0, m, B, n,
                   PPList));
             Ke3->SetPtHistogram(InitializePtPHENIX(pt_setup,Ke3->GetID(),
                   f_c, f_p0, f_a, f_b, f_n,
                   t, w, A, p0, m, B, n,
                   PPList));
             Eta->SetPtHistogram(InitializePtPHENIX(pt_setup,Eta->GetID(),
                   f_c, f_p0, f_a, f_b, f_n,
                   t, w, A, p0, m, B, n,
                   PPList));
             Etaprime->SetPtHistogram(InitializePtPHENIX(pt_setup,Etaprime->GetID(),
                   f_c, f_p0, f_a, f_b, f_n,
                   t, w, A, p0, m, B, n,
                   PPList));
             Rho->SetPtHistogram(InitializePtPHENIX(pt_setup,Rho->GetID(),
                   f_c, f_p0, f_a, f_b, f_n,
                   t, w, A, p0, m, B, n,
                   PPList));
             Omega->SetPtHistogram(InitializePtPHENIX(pt_setup,Omega->GetID(),
                   f_c, f_p0, f_a, f_b, f_n,
                   t, w, A, p0, m, B, n,
                   PPList));
             Phi->SetPtHistogram(InitializePtPHENIX(pt_setup,Phi->GetID(),
                   f_c, f_p0, f_a, f_b, f_n,
                   t, w, A, p0, m, B, n,
                   PPList));
             JPsi->SetPtHistogram(InitializePtPHENIX(pt_setup,JPsi->GetID(),
                   f_c, f_p0, f_a, f_b, f_n,
                   t, w, A, p0, m, B, n,
                   PPList));
             Psiprime->SetPtHistogram(InitializePtPHENIX(pt_setup,Psiprime->GetID(),
                   f_c, f_p0, f_a, f_b, f_n,
                   t, w, A, p0, m, B, n,
                   PPList));
             Upsilon->SetPtHistogram(InitializePtPHENIX(pt_setup,Upsilon->GetID(),
                   f_c, f_p0, f_a, f_b, f_n,
                   t, w, A, p0, m, B, n,
                   PPList));
             Upsilon2S->SetPtHistogram(InitializePtPHENIX(pt_setup,Upsilon2S->GetID(),
                   f_c, f_p0, f_a, f_b, f_n,
                   t, w, A, p0, m, B, n,
                   PPList));
             Upsilon3S->SetPtHistogram(InitializePtPHENIX(pt_setup,Upsilon3S->GetID(),
                   f_c, f_p0, f_a, f_b, f_n,
                   t, w, A, p0, m, B, n,
                   PPList));

             Pion->SetYHistogram(InitializeYPHENIX(Pion->GetID()));
             directPion->SetYHistogram(InitializeYPHENIX(directPion->GetID()));
             Ke3->SetYHistogram(InitializeYPHENIX(Ke3->GetID()));
             Eta->SetYHistogram(InitializeYPHENIX(Eta->GetID()));
             Etaprime->SetYHistogram(InitializeYPHENIX(Etaprime->GetID()));
             Rho->SetYHistogram(InitializeYPHENIX(Rho->GetID()));
             Omega->SetYHistogram(InitializeYPHENIX(Omega->GetID()));
             Phi->SetYHistogram(InitializeYPHENIX(Phi->GetID()));
             JPsi->SetYHistogram(InitializeYPHENIX(JPsi->GetID()));
             Psiprime->SetYHistogram(InitializeYPHENIX(Psiprime->GetID()));
             Upsilon->SetYHistogram(InitializeYPHENIX(Upsilon->GetID()));
             Upsilon2S->SetYHistogram(InitializeYPHENIX(Upsilon2S->GetID()));
             Upsilon3S->SetYHistogram(InitializeYPHENIX(Upsilon3S->GetID()));

             Rho->SetMHistogram(InitializeM(Rho->GetID(),0.,0.,PPList));
             Omega->SetMHistogram(InitializeM(Omega->GetID(),0.,0.,PPList));
             Phi->SetMHistogram(InitializeM(Phi->GetID(),0.,0.,PPList));
             JPsi->SetMHistogram(InitializeM(JPsi->GetID(),2.5,3.6,PPList));
             Psiprime->SetMHistogram(InitializeM(Psiprime->GetID(),3.0,4.6,PPList));
             Upsilon->SetMHistogram(InitializeM(Upsilon->GetID(),8.0,12.0,PPList));
             Upsilon2S->SetMHistogram(InitializeM(Upsilon2S->GetID(),8.0,12.0,PPList));
             Upsilon3S->SetMHistogram(InitializeM(Upsilon3S->GetID(),8.0,12.0,PPList));

             PGList->Insert(Pion.release());
             PGList->Insert(directPion.release());
             PGList->Insert(Ke3.release());
             PGList->Insert(Eta.release());
             PGList->Insert(Etaprime.release());
             PGList->Insert(Rho.release());
             PGList->Insert(Omega.release());
             PGList->Insert(Phi.release());
             PGList->Insert(JPsi.release());
             PGList->Insert(Psiprime.release());
             PGList->Insert(Upsilon.release());
             PGList->Insert(Upsilon2S.release());
             PGList->Insert(Upsilon3S.release());

             break;

    case 4:  std::cout << "Initializing Phi->KK" << std::endl;

             Phi->SetWeight(1.);

             mass = PPList.GetByID(Phi->GetID())->GetMass();
             inv_slope = 0.240;
             Phi->SetPtHistogram(InitializeExpPt(0.,4.,mass,inv_slope));
             Phi->SetYHistogram(InitializeWhiteY(-0.5,0.5));
             Phi->SetMHistogram(InitializeM(Phi->GetID(),0.,0.,PPList));

             PGList->Insert(Phi.release());

             break;

    case 5:  std::cout << "Initializing single-particle generator" << std::endl;

             break;

    case 6:  std::cout << "Initializing single-particle generator" << std::endl;

             break;

    case 7:  std::cout << "Initializing PHENIX: complete events" << std::endl << std::endl;

             do
             {
               std::cout << "Choose the shape of the pt distribution:"
                 << std::endl;
               std::cout << "----------------------------------------"
                 << std::endl;
               std::cout << std::endl;
               std::cout << "1) power law" << std::endl;
               std::cout << "2) exponential (with flow)" << std::endl;
               std::cout << "3) Au+Au, 200 GeV, 10% most central" << std::endl;
               std::cout << std::endl;
               std::cout << "Your choice (1-3): ";
               std::getline(std::cin, line);
               pt_setup = boost::lexical_cast<int>(line);
               std::cout << std::endl;
             } while ( pt_setup<1 || pt_setup>3 );

             Piplus->SetWeight(0.401);
             Piminus->SetWeight(0.401);
             Kplus->SetWeight(0.062);
             Kminus->SetWeight(0.062);
             Proton->SetWeight(0.039);
             Antiproton->SetWeight(0.036);
             Pion->SetWeight(0.445);
             Eta->SetWeight(0.043);
             /*
                Etaprime->SetWeight(0.0038);
                Rho->SetWeight(1.0);
                Omega->SetWeight(1.0);
                Phi->SetWeight(1.0);
                JPsi->SetWeight(1.0);
                Upsilon->SetWeight(1.0);
                */

             // parameters for pizero modified Hagedorn parameterization
             // default: 10% most central Au+Au at 200 GeV
             if ( pt_setup==3 ) {
               f_c  = 1744.0;
               f_a  = 0.4802;
               f_b  = 0.3389;
               f_p0 = 0.6827;
               f_n  = 8.129;
             }

             Piplus->SetPtHistogram(InitializePtPHENIX(pt_setup,Piplus->GetID(),
                   f_c, f_p0, f_a, f_b, f_n,
                   t, w, A, p0, m, B, n,
                   PPList));
             Piminus->SetPtHistogram(InitializePtPHENIX(pt_setup,Piminus->GetID(),
                   f_c, f_p0, f_a, f_b, f_n,
                   t, w, A, p0, m, B, n,
                   PPList));
             Kplus->SetPtHistogram(InitializePtPHENIX(pt_setup,Kplus->GetID(),
                   f_c, f_p0, f_a, f_b, f_n,
                   t, w, A, p0, m, B, n,
                   PPList));
             Kminus->SetPtHistogram(InitializePtPHENIX(pt_setup,Kminus->GetID(),
                   f_c, f_p0, f_a, f_b, f_n,
                   t, w, A, p0, m, B, n,
                   PPList));
             Proton->SetPtHistogram(InitializePtPHENIX(pt_setup,Proton->GetID(),
                   f_c, f_p0, f_a, f_b, f_n,
                   t, w, A, p0, m, B, n,
                   PPList));
             Antiproton->SetPtHistogram(InitializePtPHENIX(pt_setup,Antiproton->GetID(),
                   f_c, f_p0, f_a, f_b, f_n,
                   t, w, A, p0, m, B, n,
                   PPList));
             Pion->SetPtHistogram(InitializePtPHENIX(pt_setup,Pion->GetID(),
                   f_c, f_p0, f_a, f_b, f_n,
                   t, w, A, p0, m, B, n,
                   PPList));
             Eta->SetPtHistogram(InitializePtPHENIX(pt_setup,Eta->GetID(),
                   f_c, f_p0, f_a, f_b, f_n,
                   t, w, A, p0, m, B, n,
                   PPList));
             /*
                Etaprime->SetPtHistogram(
                InitializePtPHENIX(pt_setup,Etaprime->GetID(),
                f_c, f_p0, f_a, f_b, f_n,
                t, w, A, p0, m, B, n,
                PPList));
                Rho->SetPtHistogram(
                InitializePtPHENIX(pt_setup,Rho->GetID(),
                f_c, f_p0, f_a, f_b, f_n,
                t, w, A, p0, m, B, n,
                PPList));
                Omega->SetPtHistogram(
                InitializePtPHENIX(pt_setup,Omega->GetID(),
                f_c, f_p0, f_a, f_b, f_n,
                t, w, A, p0, m, B, n,
                PPList));
                Phi->SetPtHistogram(
                InitializePtPHENIX(pt_setup,Phi->GetID(),
                f_c, f_p0, f_a, f_b, f_n,
                t, w, A, p0, m, B, n,
                PPList));
                JPsi->SetPtHistogram(
                InitializePtPHENIX(pt_setup,JPsi->GetID(),
                f_c, f_p0, f_a, f_b, f_n,
                t, w, A, p0, m, B, n,
                PPList));
                Upsilon->SetPtHistogram(
                InitializePtPHENIX(pt_setup,Upsilon->GetID(),
                f_c, f_p0, f_a, f_b, f_n,
                t, w, A, p0, m, B, n,
                PPList));
                */

             Piplus->SetYHistogram(InitializeYPHENIX(Piplus->GetID()));
             Piminus->SetYHistogram(InitializeYPHENIX(Piminus->GetID()));
             Kplus->SetYHistogram(InitializeYPHENIX(Kplus->GetID()));
             Kminus->SetYHistogram(InitializeYPHENIX(Kminus->GetID()));
             Proton->SetYHistogram(InitializeYPHENIX(Proton->GetID()));
             Antiproton->SetYHistogram(InitializeYPHENIX(Antiproton->GetID()));
             Pion->SetYHistogram(InitializeYPHENIX(Pion->GetID()));
             Eta->SetYHistogram(InitializeYPHENIX(Eta->GetID()));
             /*
                Etaprime->SetYHistogram(InitializeYPHENIX(Etaprime->GetID()));
                Rho->SetYHistogram(HistoInitializeYPHENIX(Rho->GetID()));
                Omega->SetYHistogram(HisInitializeYPHENIX(Omega->GetID()));
                Phi->SetYHistogram(HistoInitializeYPHENIX(Phi->GetID()));
                JPsi->SetYHistogram(HistInitializeYPHENIX(JPsi->GetID()));
                Upsilon->SetYHistogram(HInitializeYPHENIX(Upsilon->GetID()));
                */

             /*
                Rho->SetMHistogram(InitializeM(Rho->GetID(),0.,0.,PPList));
                Omega->SetMHistogram(InitializeM(Omega->GetID(),0.,0.,PPList));
                Phi->SetMHistogram(InitializeM(Phi->GetID(),0.,0.,PPList));
                JPsi->SetMHistogram(InitializeM(JPsi->GetID(),3.0927,3.1014,PPList));
                Upsilon->SetMHistogram(InitializeM(Upsilon->GetID(),9.4574,9.4626,PPList));
                */

             PGList->Insert(Piplus.release());
             PGList->Insert(Piminus.release());
             PGList->Insert(Kplus.release());
             PGList->Insert(Kminus.release());
             PGList->Insert(Proton.release());
             PGList->Insert(Antiproton.release());
             PGList->Insert(Pion.release());
             PGList->Insert(Eta.release());
             /*
                PGList->Insert(Etaprime.release());
                PGList->Insert(Rho.release());
                PGList->Insert(Omega.release());
                PGList->Insert(Phi.release());
                PGList->Insert(JPsi.release());
                PGList->Insert(Upsilon.release());
                */

             break;

    default: std::cerr << "Error: Setup " << setup << " not predefined" << std::endl;

             break;
  }

  std::cout << std::endl;

  return PGList.release();
}
