#include "EmcSimTowerSmearer.h"


#include "EmcIndexer.h"
#include "emcTowerContainer.h"
#include "emcTowerContent.h"

#include "Fun4AllReturnCodes.h"
#include "Fun4AllServer.h"
#include "getClass.h"

#include "PHCompositeNode.h"

#include "TF1.h"
#include "TRandom.h"

#include <iostream>

//_____________________________________________________________________________
EmcSimTowerSmearer::EmcSimTowerSmearer(const char* simuTopNode,
				       double percentageOfSmearing,
				       unsigned int iseed)
  : SubsysReco("EmcSimTowerSmearer"),
    fSimuTopNode(simuTopNode), 
    fEnergyResolutionConstantTerm(0),
    fConstantTerm(percentageOfSmearing),
    fIseed(iseed)
{
}

//_____________________________________________________________________________
EmcSimTowerSmearer::~EmcSimTowerSmearer()
{
  delete fEnergyResolutionConstantTerm;
}

//_____________________________________________________________________________
int
EmcSimTowerSmearer::Init(PHCompositeNode*)
{
  fEnergyResolutionConstantTerm = 
    new TF1("fEmcEnergyResolutionConstantTerm","gaus",-10,10);

  fEnergyResolutionConstantTerm->SetParameters(1,0,1);

  gRandom->SetSeed(fIseed);

  std::cout << PHWHERE << " My % of smearing is "
	    << fConstantTerm << " - My ";
  if (fIseed)
    {
      std::cout << "(externally fixed) ";
    }
  std::cout << "seed is " << gRandom->GetSeed() << std::endl;

  return 0;
}

//_____________________________________________________________________________
int
EmcSimTowerSmearer::process_event(PHCompositeNode*)
{
  // Here must get the emcTowerContainer from the SIMU DST
  // and smear the energies.

  Fun4AllServer* se = Fun4AllServer::instance();

  PHCompositeNode* topNode = se->topNode(fSimuTopNode.c_str());
  
  if (!topNode)
    {
      std::cerr << PHWHERE << " did not find "
		<< " topNode=" << fSimuTopNode << std::endl;
      return ABORTEVENT;
    }

  emcTowerContainer* towers = 
    findNode::getClass<emcTowerContainer>(topNode,"emcTowerContainer");

  if (!towers)
    {
      std::cerr << PHWHERE << " did not find emcTowerContainer under "
		<< fSimuTopNode << std::endl;
      return ABORTEVENT;
    }

  double random = fEnergyResolutionConstantTerm->GetRandom();

  for ( size_t i = 0; i < towers->size(); ++i ) 
    {
      emcTowerContent* t = towers->getTower(i);
      int is,iz,iy;
      EmcIndexer::decodeTowerId(t->towerid(),is,iz,iy);
      if ( is < 8 )
	{
	  float oldenergy = t->Energy();
	  
	  float newenergy = oldenergy + oldenergy*fConstantTerm*random;

	  t->SetCalibrated(newenergy,t->ToF());
	}
    }

  return EVENT_OK;
}
