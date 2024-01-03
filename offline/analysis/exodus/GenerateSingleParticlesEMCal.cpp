//=============================================================
//  Adaptation of "official" Oscar single-particle generator
//  to EMCal needings
//=============================================================

#include <TFile.h>
#include <TH1.h>
#include <TMath.h>
#include <TRandom.h>
#include <boost/lexical_cast.hpp>
#include <cmath>
#include <iostream>
#include <limits>
#include <string>
#include "GenerateSingleParticlesEMCal.h"
#include "Particle.h"
#include "ParticleList.h"
#include "ParticleProperty.h"
#include "ParticlePropertyList.h"

void GenerateSingleParticlesEMCal(ParticleList *PList, const int events,
    ParticlePropertyList *PPList)
{
  // the size of the EMCal and the distance to the beam axis define the rapidity window
  const double emcxMax = 540.;
  const double emcyMax = 98.52; //PbGl Size as in Klaus acceptance Program (PbSc is slightly smaller)
  const double emczMax = 197.04;

  std::string line;
  int setup;
  do
  {
    std::cout << "Choose the shape of the pt distribution:" << std::endl;
    std::cout << "----------------------------------------" << std::endl;
    std::cout << std::endl;
    std::cout << "1) flat" << std::endl;
    std::cout << "2) low pt enhancement" << std::endl;
    std::cout << "3) Power-law (n=-7) " << std::endl;
    std::cout << std::endl;
    std::cout << "Your choice (1-3): ";
    std::getline(std::cin, line);
    setup = boost::lexical_cast<int>(line);
    std::cout << std::endl;
  } while ( setup<1 || setup>3 );

  std::cout << "Select particle by ID from the following list :" << std::endl << std::endl;

  int iProperty = 1;
  ParticleProperty* Property = PPList->Get(iProperty);
  while ( Property->GetID()!=0 )
  {
    std::cout << "ID: " << Property->GetID() << " is a " << Property->GetName() << std::endl;
    iProperty++;
    Property = PPList->Get(iProperty);
  }
  std::cout << std::endl;
  delete Property;
  Property = 0;

  std::cout << "ID of particle to generate ? : ";
  std::getline(std::cin, line);
  const int pID = boost::lexical_cast<int>(line);
  std::cout << std::endl;

  ParticleProperty* PProperty = PPList->GetByID(pID);
  double mass = PProperty->GetMass();

  std::cout << "Minimum momentum (GeV) of the single-particle ? : ";
  std::getline(std::cin, line);
  double ptmin = boost::lexical_cast<double>(line);
  if ( ptmin < 1.5 && setup==3 ) ptmin = 1.5; // Lower cut-off for power-law
  std::cout << std::endl;
  std::cout << "Maximum momentum (GeV) of the single-particle ? : ";
  std::getline(std::cin, line);
  const double ptmax = boost::lexical_cast<double>(line);
  std::cout << std::endl;

  // Generation of pT random histo
  const int nbins = 10000;
  const int binwidth = static_cast<int>((ptmax-ptmin)/nbins);
  TH1F * pthistogram = new TH1F("pt","pt",nbins,ptmin,ptmax);

  for ( int ibin=1; ibin<=nbins; ibin++ )
  {
    const double pt = ptmin+(ibin-1)*binwidth+binwidth/2.0;
    double weight = 1.0;
    if ( setup==2 )
    {
      weight = 1.0 + 10.0*std::exp(-2.0*pt);
    }
    if ( setup==3 ) // PHENIX at 130A GeV: pow(x,n) with n=-7.0
    {
      weight = 20.0/std::pow(pt,7.);
    }
    pthistogram->AddBinContent(ibin,weight);
  }

  int phi_acc;
  do
  {
    std::cout << "EMCal Rapidity-Phi range: " << std::endl << std::endl ;
    std::cout << " 1. Run-1 PbSc W0-W1 [-34. deg, 12. deg]" << std::endl;
    std::cout << " 2. Run-1 PbGl E1 [168. deg, 192 deg.] " << std::endl;
    std::cout << " 3. Run-1 EMCal (PbSc W0-W1 + PbGl E1) " << std::endl;
    std::cout << " 4. Run-2 EMCal (PbSc W0-W3, E2-E3 + PbGl E0-E1) " << std::endl;
    std::cout << " 5. \"1-unit rapidity & full phi\" (|y| < 0.5 , phi = 0-2pi) "<< std::endl;
    std::getline(std::cin, line);
    phi_acc = boost::lexical_cast<int>(line);
    std::cout << std::endl;
  } while ( phi_acc<1 || phi_acc>5 );

  // the phi acceptance is slightly larger
  double phimin = 0., phimax = 0.;
  double phimin1 = 0., phimax1 = 0., phimin2 = 0., phimax2 = 0.;
  if      (phi_acc == 1) { phimin  = -TMath::Pi()*34/180.   ;  phimax  =  TMath::Pi()*12/180. ; }
  else if (phi_acc == 2) { phimin  =  TMath::Pi()*167./180. ;  phimax  =  TMath::Pi()*193./180. ; }
  else if (phi_acc == 3) { phimin1 = -TMath::Pi()*34/180.   ;  phimax1 =  TMath::Pi()*12/180. ;
    phimin2 =  TMath::Pi()*167/180.  ;  phimax2 =  TMath::Pi()*193./180. ; }
  else if (phi_acc == 4) { phimin1 = -TMath::Pi()*34./180.  ;  phimax1 =  TMath::Pi()*56./180. ;
    phimin2 =  TMath::Pi()*123/180.  ;  phimax2 =  TMath::Pi()*214./180. ; }
  else if (phi_acc == 5) { phimin  = -TMath::Pi()*90./180.  ;  phimax  =  TMath::Pi()*270./180. ; }

  const double zVTXmax = 30.0;

  // Info output
  std::cout << "INFO: " << std::endl ;
  std::cout << std::endl << "<I> Vertex max: |vtx_z| < " << zVTXmax << " cm " << std::endl;

  std::cout << "<I> pT domain: " << ptmin << " - " << ptmax ;
  if ( setup==1 ) std::cout << " (Flat)" << std::endl;
  if ( setup==2 ) std::cout << " (low-pT enhanc.)" << std::endl;
  if ( setup==3 ) std::cout << " (power-law)" << std::endl;

  double ymin, ymax;
  if ( phi_acc != 5 ) {
    ymin             = -0.37; // should include PbSc (0.37) and PbGl (0.35)
    ymax             =  0.37; // should include PbSc (0.37) and PbGl (0.35)
  } else { // 1-unit rapidity
    ymin             = -0.5;
    ymax             =  0.5;
  }
  std::cout << "<I> Rapidity domain: " << ymin << " - " << ymax << std::endl;

  if (phimin > std::numeric_limits<double>::epsilon())
  {
    std::cout << "<I> Phi domain: " << phimin << " - " << phimax << std::endl << std::endl ;
  }
  else if (phimin1 > std::numeric_limits<double>::epsilon())
  {
    std::cout << "<I> Phi domain (PbSc): " << phimin1 << " - " << phimax1 << std::endl ;
    std::cout << "<I> Phi domain (PbGl): " << phimin2 << " - " << phimax2 << std::endl << std::endl ;
  }

  // Control histos
  TH1F * h_pt   = new TH1F("h_pt"  ,"pt",100,ptmin,ptmax);
  TH1F * h_zvtx = new TH1F("h_zvtx","zvtx",100,-2.*zVTXmax,2.*zVTXmax);
  TH1F * h_phi  = new TH1F("h_phi" ,"phi",360,0.,360.);
  TH1F * h_y    = new TH1F("h_y"   ,"y",100,2.*ymin,2.*ymax);

  // Generation of Exodus file
  for (int ievent=0; ievent<events; ievent++)
  {
    if (ievent % 10000 == 0)
      std::cout << ievent << " particles generated" << std::endl;

    double phi;
    double rand = 0.;
    if ( phi_acc==3 )
    {
      rand = gRandom->Rndm(); // 1/3 of the events for each block
      if ( rand <1./3. )
        phi = (phimin1+phimax1)/2.+((phimax1-phimin1)/2.)*gRandom->Rndm(); // PbSc W1: -11. <--> 13.
      else if ( rand >= (1./3.) && rand < (2./3.) )
        phi = phimin1+((phimax1-phimin1)/2.)*gRandom->Rndm(); // PbSc W0 : -35. <--> -11.
      else
        phi = phimin2+(phimax2-phimin2)*gRandom->Rndm(); // PbGl E1 :  167. <--> 193.
    }
    else if ( phi_acc==4 )
    {
      rand = gRandom->Rndm(); // 1/2 of the events for each side (E/W)
      if ( rand <1./2. )
        phi = phimin1+(phimax1-phimin1)*gRandom->Rndm(); // West :  -35. <--> 52.
      else
        phi = phimin2+(phimax2-phimin2)*gRandom->Rndm(); // East :  123. <--> 215.
    }
    else
    {
      phi = phimin+(phimax-phimin)*gRandom->Rndm();
    }
    if ( phi<0. ) phi = 2.0*TMath::Pi()+phi;

    //const double pt = ptmax*gRandom->Rndm();
    const double pt = pthistogram->GetRandom();

    const double zVTX     = 2.0*zVTXmax*(gRandom->Rndm()-0.5);

    if ( phi_acc!=5 )
    {
      // Slightly larger rapidity adapted to z-vtx position
      const double zUp   = emczMax + std::abs(zVTX);
      const double zDown = emczMax - std::abs(zVTX);
      const double r = std::sqrt(emcxMax*emcxMax+emcyMax*emcyMax+zUp*zUp);
      if(zVTX>=0)
      {
        ymax =  0.5*std::log((r+zDown)/(r-zDown));
        ymin = -0.5*std::log(  (r+zUp)/(r-zUp));
      }
      else
      { // ZVtx < 0 the other way around => more particles with positive rapidity
        ymax =  0.5*std::log(  (r+zUp)/(r-zUp));
        ymin = -0.5*std::log((r+zDown)/(r-zDown));
      }
    }

    const double y = ymin+(ymax-ymin)*gRandom->Rndm();

    const double mt = std::sqrt(pt*pt+mass*mass);
    const double E  = mt*std::cosh(y);
    const double px = pt*std::cos(phi);
    const double py = pt*std::sin(phi);
    const double pz = mt*std::sinh(y);

    Particle* PParticle = new Particle;
    PParticle->SetID(pID);
    PParticle->Set4mom(E,px,py,pz);
    PParticle->SetDecaysum(0.0);
    PParticle->SetWeight(1.0);
    PParticle->SetGeneration(1);
    PParticle->SetVertex(0.0,0.0,zVTX);
    PLNode* Current = PList->GetHeadNode();
    PList->InsertAfter(Current, PParticle);

    // control histos
    h_pt->Fill(pt);
    h_phi->Fill(180.*phi/TMath::Pi());
    h_zvtx->Fill(zVTX);
    h_y->Fill(y);
  }

  TFile *inputKin = new TFile("inputKin.root","recreate");
  h_pt->Write();
  h_zvtx->Write();
  h_phi->Write();
  h_y->Write();
  inputKin->Close();

  std::cout << std::endl;
  std::cout << " ==========> You can check the 'goodness' of"
    << " your selected kinematics doing:  root inputKin.root " << std::endl;
  std::cout << std::endl ;

  return;
}
