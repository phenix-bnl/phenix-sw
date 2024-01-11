#include <EmcEnergyAfterBurnerv1.h>
#include <RunHeader.h>
#include <EmcClusterLocalExt.h>
#include <PHIODataNode.h>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh> 
#include <PdbEmcEScaleTower.hh>

#include <cassert>
#include <cstdlib>

using namespace std;

// These typedefs should cover most user's needs...
typedef PHIODataNode<RunHeader> RunHeaderNode_t;
typedef PHIODataNode<EmcClusterLocalExt> EmcClusterLocalExtNode_t;

ClassImp(EmcEnergyAfterBurnerv1)
  
EmcEnergyAfterBurnerv1::EmcEnergyAfterBurnerv1()
{
}

void EmcEnergyAfterBurnerv1::Reset()
{
  for (int iarm=0; iarm<2; iarm++)
    {
      for (int isect=0; isect<4; isect++)
        {
          for (int iy=0; iy<48; iy++)
            {
              for (int iz=0; iz<96; iz++)
                {
                  tower_scalefactor[iarm][isect][iy][iz] = 1.;
                }
            }
        }
    }

}

void EmcEnergyAfterBurnerv1::initialize(PHCompositeNode *topNode)
{
  // First find the run header and get the run number...
  RunHeader *d_runhdr;
  PHTypedNodeIterator<RunHeader> iRUN(topNode);
  RunHeaderNode_t *RUN = iRUN.find("RunHeader");
  if (!RUN)
    {
      cout << PHWHERE << "EmcEnergyAfterBurnerv1::no RunHeader Node...cancelling corrections" << endl;
      return;
    }
  d_runhdr = RUN->getData();
  if (!d_runhdr)
    {
      cout << PHWHERE << "EmcEnergyAfterBurnerv1::no runheader...cancelling corrections" << endl;
      return;
    }
  int RunNumber = d_runhdr->get_RunNumber();
  
  cout << "EmcEnergyAfterBurnerv1::Getting Calibration adjustments for run #" << RunNumber << endl;

  initialize(RunNumber);
}

void EmcEnergyAfterBurnerv1::initialize(int RunNumber)
{
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbBankID bankID(0);

  PdbEmcEScaleTower  *tower = 0;
  //  PdbEmcEScaleTower  *tower = new PdbEmcEScaleTower();

  Reset();

  if (application->startRead())
    {
      const char *calibname = "calib.emc.eScale_Tower";

      PdbCalBank *emcBank = bankManager->fetchBank("PdbEmcEScaleTowerBank", bankID, calibname, RunNumber);

      if (emcBank)
	{
	  tower =  ( PdbEmcEScaleTower*) & (emcBank->getEntry(0));
	  delete emcBank;
	}
      else
	{
	  cout << PHWHERE << "error getting tower energy scale factors" << endl;
	  exit(1);
	}

      application->commit();

      for (int iarm=0; iarm<2; iarm++)
        {
          for (int isect=0; isect<4; isect++)
            {
              for (int iy=0; iy<48; iy++)
                {
                  for (int iz=0; iz<96; iz++)
                    {
                      tower_scalefactor[iarm][isect][iy][iz] = tower->getTwrEScaleFactor(iarm, isect, iy, iz);

                    }
                }
            }
        }
    }
  else
    {
      cout << PHWHERE << "error accessing database" << endl;
      exit(1);
    }
}

void EmcEnergyAfterBurnerv1::apply(PHCompositeNode *udstNode)
{
  EmcClusterLocalExt *d_emc;
  PHTypedNodeIterator<EmcClusterLocalExt> iEMC(udstNode);
  EmcClusterLocalExtNode_t *EMC = iEMC.find("EmcClusterLocalExt");
  if (!EMC)
    {
      cout << "EmcEnergyAfterBurnerv1::no Emc Track in udst" << endl;
      return;
    }

  // loop over all the EmcClusterLocal Objects and apply the timing offset
  d_emc = EMC->getData();
  for (unsigned int iclus=0; iclus<d_emc->get_EmcNCluster(); iclus++)
    {
      unsigned int iarm  = d_emc->get_arm(iclus);
      unsigned int isect = d_emc->get_sector(iclus);
      unsigned int iy    = d_emc->get_ind(iclus,1);
      unsigned int iz    = d_emc->get_ind(iclus,0);
      float e = d_emc->get_e(iclus);
      float ecore = d_emc->get_ecore(iclus);
      float ecorr = d_emc->get_ecorr(iclus);
      float ecent = d_emc->get_ecent(iclus);
      float e9 = d_emc->get_e9(iclus);

      assert( iarm<2 && isect<4 && iy<48 && iz<96 );

      float energyscale = tower_scalefactor[iarm][isect][iy][iz];

      d_emc->set_e(iclus, e * energyscale);
      d_emc->set_ecore(iclus, ecore * energyscale);
      d_emc->set_ecorr(iclus, ecorr * energyscale);
      d_emc->set_ecent(iclus, ecent * energyscale);
      d_emc->set_e9(iclus, e9 * energyscale);
    }
}

