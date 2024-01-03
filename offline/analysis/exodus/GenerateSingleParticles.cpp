#include <TH1.h>
#include <TMath.h>
#include <TRandom.h>
#include <boost/lexical_cast.hpp>
#include <cmath>
#include <iostream>
#include <string>
#include "GenerateSingleParticles.h"
#include "InitializeM.h"
#include "PHENIXFilter.h"
#include "Particle.h"
#include "ParticleList.h"
#include "ParticleProperty.h"
#include "ParticlePropertyList.h"

void GenerateSingleParticles(ParticleList *PList, int events,
    const ParticlePropertyList& PPList)
{
  std::string line;
  int setup;
  do
  {
    std::cout << "Choose the shape of the pt distribution:" << std::endl;
    std::cout << "----------------------------------------" << std::endl;
    std::cout << std::endl;
    std::cout << "1) flat" << std::endl;
    std::cout << "2) low pt enhancement" << std::endl;
    std::cout << "3) power law: pt^x" << std::endl;
    std::cout << std::endl;
    std::cout << "Your choice (1-3): ";
    std::getline(std::cin, line);
    setup = boost::lexical_cast<int>(line);
    std::cout << std::endl;
  } while ( setup<1 || setup>3 );

  double power = 0;
  if ( setup==3 )
  {
    std::cout << std::endl;
    std::cout << "Your choice for the power x: ";
    std::getline(std::cin, line);
    power = boost::lexical_cast<double>(line);
    std::cout << std::endl;
  }

  std::cout << "Select particle by ID from the following list:"
    << std::endl << std::endl;

  int iProperty = 1;
  ParticleProperty* Property = PPList.Get(iProperty);
  int itotal = 0;
  while ( Property->GetID()!=0 )
  {
    std::cout << "ID: "
      << Property->GetID()
      << " is a "
      << Property->GetName()
      << std::endl;
    iProperty++;
    Property = PPList.Get(iProperty);
  }
  std::cout << std::endl;
  delete Property;
  Property = 0;

  std::cout << "ID of particle to generate: ";
  std::getline(std::cin, line);
  const int pID = boost::lexical_cast<int>(line);
  std::cout << std::endl;

  ParticleProperty* PProperty = PPList.GetByID(pID);
  double mass_fixed = PProperty->GetMass();
  double width      = PProperty->GetWidth();
  TH1* masshist = (width > 0.) ? InitializeM(pID,0.,0.,PPList): 0;
  // please check masshist for 0 before usage

  std::cout << "minimum pt: ";
  std::getline(std::cin, line);
  const double ptmin = boost::lexical_cast<double>(line);
  std::cout << std::endl;
  std::cout << "maximum pt: ";
  std::getline(std::cin, line);
  const double ptmax = boost::lexical_cast<double>(line);
  std::cout << std::endl;

  std::cout << "minimum rapidity: ";
  std::getline(std::cin, line);
  const double ymin = boost::lexical_cast<double>(line);
  std::cout << std::endl;
  std::cout << "maximum rapidity: ";
  std::getline(std::cin, line);
  const double ymax = boost::lexical_cast<double>(line);
  std::cout << std::endl;

  std::cout << "vertex range (+-cm): ";
  std::getline(std::cin, line);
  const double zVTXmax = boost::lexical_cast<double>(line);
  std::cout << std::endl;

  std::cout << "minimum phi: ";
  std::getline(std::cin, line);
  const double phimin = boost::lexical_cast<double>(line);
  std::cout << std::endl;
  std::cout << "maximum phi: ";
  std::getline(std::cin, line);
  const double phimax = boost::lexical_cast<double>(line);
  std::cout << std::endl;

  const int nbins = 10000;
  const double binwidth = (ptmax-ptmin)/nbins;
  TH1F * pthistogram = new TH1F("pt","pt",nbins,ptmin,ptmax);

  const int fieldSetting = 1;
  const double pt_cut    = 0.2;
  const double vtx_cut   = 0.3;

  double pt;
  for ( int ibin=1; ibin<=nbins; ibin++ )
  {
    pt = ptmin+(ibin-1)*binwidth+binwidth/2.0;
    double weight = 1.0;
    if ( setup==2 )
    {
      weight = 1.0 + 10.0*std::exp(-2.0*pt);
    }
    if ( setup==3 )
    {
      weight = std::pow(pt,power);
    }
    pthistogram->AddBinContent(ibin,weight);
  }

  for ( int ievent=1; ievent<=events; ievent++)
  {
    itotal++;
    if ( itotal % 10000 == 0 )
      std::cout << itotal << " particles generated" << std::endl;

    double phi = phimin+(phimax-phimin)*gRandom->Rndm();
    if ( phi<0. ) phi = 2.0*TMath::Pi()+phi;

    pt   = pthistogram->GetRandom();
    const double zVTX = 2.0*zVTXmax*(gRandom->Rndm()-0.5);
    double y    = ymin+(ymax-ymin)*gRandom->Rndm();

    double mass = (masshist != 0) ? masshist->GetRandom() : mass_fixed;

    double mt   = std::sqrt(pt*pt+mass*mass);
    double E    = mt*std::cosh(y);
    double px   = pt*std::cos(phi);
    double py   = pt*std::sin(phi);
    double pz   = mt*std::sinh(y);

    Particle* PParticle = new Particle;
    PParticle->SetID(pID);
    PParticle->Set4mom(E,px,py,pz);
    PParticle->SetDecaysum(0.0);
    PParticle->SetWeight(1.0);
    PParticle->SetGeneration(1);
    PParticle->SetVertex(0.0,0.0,zVTX);
    int sector = PHENIXFilter(fieldSetting,pt_cut,vtx_cut,*PParticle,PPList);
    PParticle->SetAccept(sector);
    PLNode* Current = PList->GetHeadNode();
    PList->InsertAfter(Current, PParticle);
  }
}
