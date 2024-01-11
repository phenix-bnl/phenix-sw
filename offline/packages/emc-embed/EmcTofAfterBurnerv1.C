#include <EmcTofAfterBurnerv1.h>
#include <RunHeader.h>
#include <EmcClusterLocalExt.h>
#include <PHIODataNode.h>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh> 
#include <PdbDouble.hh>
#include <PdbEmcT0Sector.hh>
#include <PdbEmcT0Tower.hh>

#include <cassert>
#include <cmath>
#include <cstdlib>

using namespace std;

// These typedefs should cover most user's needs...
typedef PHIODataNode<RunHeader> RunHeaderNode_t;
typedef PHIODataNode<EmcClusterLocalExt> EmcClusterLocalExtNode_t;

ClassImp(EmcTofAfterBurnerv1)
  
EmcTofAfterBurnerv1::EmcTofAfterBurnerv1()
{
}

void EmcTofAfterBurnerv1::Reset()
{
  for (int iarm=0; iarm<2; iarm++)
    {
      for (int isect=0; isect<4; isect++)
        {
          sectoroffset[iarm][isect] = 0.;
          for (int iy=0; iy<48; iy++)
            {
              for (int iz=0; iz<96; iz++)
                {
                  toweroffset[iarm][isect][iy][iz] = 0.;
                  towerleastcount[iarm][isect][iy][iz] = 1.;
                  towerslew[iarm][isect][iy][iz] = 0.;
                }
            }
        }
    }

}

void EmcTofAfterBurnerv1::initialize(PHCompositeNode *topNode)
{
  // First find the run header and get the run number...
  RunHeader *d_runhdr;
  PHTypedNodeIterator<RunHeader> iRUN(topNode);
  RunHeaderNode_t *RUN = iRUN.find("RunHeader");
  if (!RUN)
    {
      cout << PHWHERE << "EmcTofAfterBurnerv1::no RunHeader Node...cancelling corrections" << endl;
      return;
    }
  d_runhdr = RUN->getData();
  if (!d_runhdr)
    {
      cout << PHWHERE << "EmcTofAfterBurnerv1::no runheader...cancelling corrections" << endl;
      return;
    }
  int RunNumber = d_runhdr->get_RunNumber();
  
  cout << "EmcTofAfterBurnerv1::Getting Calibration adjustments for run #" << RunNumber << endl;

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbBankID bankID(0);
  PdbCalBank *emcBank;
  PdbCalBank *bbcBank;

  PdbEmcT0Sector *sector = 0;
  PdbEmcT0Tower  *tower = 0;
  PdbDouble *meanBbc = 0;

  Reset();

  if (application->startRead())
    {

      const char *calibname = "calib.emc.bbcT0";

      // Modified by Hisa for ppv01DST
      //bbcBank = bankManager->fetchBank("PdbDoubleBank", bankID, calibname, RunNumber);
      bbcBank = bankManager->fetchClosestBank("PdbDoubleBank", bankID, calibname, RunNumber);

      if (bbcBank)
        {
          meanBbc =  ( PdbDouble*) & (bbcBank->getEntry(0));
	  delete bbcBank;
        }     
      else
	{
	  cout << PHWHERE << "error getting mean BBC T0 offset" << endl;
	  exit(1);
	}

      calibname = "calib.emc.deltaT0_Sector";

      // Modified by Hisa for ppv01DST
      //emcBank = bankManager->fetchBank("PdbEmcT0SectorBank", bankID, calibname, RunNumber);
      emcBank = bankManager->fetchClosestBank("PdbEmcT0SectorBank", bankID, calibname, RunNumber);



      if (emcBank)
        {
          sector =  ( PdbEmcT0Sector*) & (emcBank->getEntry(0));
	  delete emcBank;
        }     
      else
	{
	  cout << PHWHERE << "error getting run T0 offset" << endl;
	  exit(1);
	}

      calibname = "calib.emc.deltaT0_Tower";

      PdbCalBank *emcBank = bankManager->fetchBank("PdbEmcT0TowerBank", bankID, calibname, RunNumber);

      if (emcBank)
	{
	  tower =  ( PdbEmcT0Tower*) & (emcBank->getEntry(0));
	  delete emcBank;
	}
      else
	{
	  cout << PHWHERE << "error getting tower T0 offsets" << endl;
	  exit(1);
	}

      application->commit();

      meanBbcT0 = meanBbc->getValue();
      cout << "Bbc Mean T0 = " << meanBbcT0 << endl;
      
      for (int iarm=0; iarm<2; iarm++)
        {
          for (int isect=0; isect<4; isect++)
            {
              sectoroffset[iarm][isect] = sector->getSectorT0Correction(iarm,isect);
	      if ( sectoroffset[iarm][isect] != 0. ) cout << "soff" << iarm << "\t" << isect << sectoroffset[iarm][isect] << endl;
	      
	      hadronCorrFactor[iarm][isect] = 1.0;
	      
	      // sorry for hard-coding this run number 
	      // (last minute correction for afterburner)
	      if (RunNumber >= 29888)
		{
		  if(iarm==0&&(isect==2||isect==3)) 
		    {
		      hadronCorrFactor[iarm][isect] /=0.7891;
		    }
		}
	      // yes for PbSc every sector
	      if(!(iarm==1&&(isect==0||isect==1))) 
		{
		  hadronCorrFactor[iarm][isect] /= 1.046 ;
		}
	      
              for (int iy=0; iy<48; iy++)
                {
                  for (int iz=0; iz<96; iz++)
                    {
                      toweroffset[iarm][isect][iy][iz] = tower->getTwrT0Correction(iarm, isect, iy, iz);
                      towerleastcount[iarm][isect][iy][iz] = tower->getLeastCountCorrection(iarm, isect, iy, iz);
                      towerslew[iarm][isect][iy][iz] = tower->getSlewingCorrection(iarm, isect, iy, iz);

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

void EmcTofAfterBurnerv1::apply(PHCompositeNode *udstNode)
{
  EmcClusterLocalExt *d_emc;
  PHTypedNodeIterator<EmcClusterLocalExt> iEMC(udstNode);
  EmcClusterLocalExtNode_t *EMC = iEMC.find("EmcClusterLocalExt");
  if (!EMC)
    {
      cout << "EmcTofAfterBurnerv1::no Emc Track in udst" << endl;
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
      float tofcorr = d_emc->get_tofcorr(iclus);
      float tofmin = d_emc->get_tofmin(iclus);
      float tofmax = d_emc->get_tofmax(iclus);
      float ecent = d_emc->get_ecent(iclus);

      assert( iarm<2 && isect<4 && iy<48 && iz<96 );

      float tOffset = -1.*sectoroffset[iarm][isect] - toweroffset[iarm][isect][iy][iz];

      float tofcorrCorr=1.0;   
      float tofminCorr=1.0;   
      float tofmaxCorr=1.0;   

      tofcorrCorr = hadronCorrFactor[iarm][isect] * (tofcorr + tOffset)
	- ( hadronCorrFactor[iarm][isect] - 1 ) * meanBbcT0 ; 
      
      tofminCorr = hadronCorrFactor[iarm][isect] * (tofmin + tOffset)
	- ( hadronCorrFactor[iarm][isect] - 1 ) * meanBbcT0 ; 
      
      tofmaxCorr = hadronCorrFactor[iarm][isect] * (tofmax + tOffset)
	- ( hadronCorrFactor[iarm][isect] - 1 ) * meanBbcT0 ; 

      // Functional form of timing shift as a function of energy      
      // Correct for this shift only for ecent>1, where no hadron PID
      float tofEshift = 0.17 * (log(ecent) + 0.91) * (log(ecent) + 0.91);
      if (ecent > 1.0)
	{
	  tofcorrCorr = tofcorrCorr - tofEshift;
	  tofminCorr = tofminCorr - tofEshift;
	  tofmaxCorr = tofmaxCorr - tofEshift;
	} 

      d_emc->set_tofcorr(iclus, tofcorrCorr);
      d_emc->set_tofmin(iclus, tofminCorr);
      d_emc->set_tofmax(iclus, tofmaxCorr);
    }
}

