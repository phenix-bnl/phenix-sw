#include "EmcT0RecalReco.h"


#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>
#include <recoConsts.h>
#include <PHGlobal.h>
#include <emcClusterContainer.h>
#include <emcClusterContent.h>
#include <EmcIndexer.h>

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh>

#include <PHCompositeNode.h>
#include <getClass.h>

#include <TH1.h>

#include <iostream>

using namespace std;

float towercorr[15552];

// bins for the hit frequency histograms
// must start at 0
float threshold[9] = {0.0, 0.5, 1.0, 1.5, 2.0, 4.0, 6.0, 8.0, 10000000.0};

EmcT0RecalReco::EmcT0RecalReco(const string &name): SubsysReco(name)
{
  return ;
}

int
EmcT0RecalReco::InitRun(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();
  // this rc flag is set by the framework
  if ( (run = rc->get_IntFlag("RUNNUMBER")) )
    {
      PdbBankManager * bankManager = PdbBankManager::instance();
      PdbApplication *application = bankManager->getApplication();
      PdbBankID bankID(0);

      // read in every correction even though not all of them
      // will be applied
      if (application->startRead())
        {
          PdbCalBank *emcBank;
          //	  PdbEmcT0Sector *sector;
          //	  PdbEmcT0Tower  *tower;
          //	  PdbEmcT0Sector *sector_vd;

          const char *calibname = "calib.emc.T0_Sector";

          // run by run sector by sector corrections
          //      emcBank = bankManager->fetchClosestBank("PdbEmcT0SectorBank", bankID, calibname, run);
          emcBank = bankManager->fetchBank("PdbEmcT0SectorBank", bankID, calibname, run);

          if (emcBank)
            {
              TheSector = *( (PdbEmcT0Sector *)( & emcBank->getEntry(0) ) );
              cout << "<I> EmcT0RecalReco::readDataFromObjy(): Sector corrections read in." << endl;
              delete emcBank;
              isvalid = 1;
            }

          // tower by tower corrections
          calibname = "calib.emc.T0_Tower";
          emcBank = bankManager->fetchBank("PdbEmcT0TowerBank", bankID, calibname, run);
          if (emcBank)
            {
              TheTower = *( (PdbEmcT0Tower *)( & emcBank->getEntry(0) ) );
              cout << "<I> EmcT0RecalReco::readDataFromObjy(): Tower corrections read in." << endl;
              delete emcBank;
              isvalid = 1;
            }

          // global default sector T0 offset
          calibname = "calib.emc.VD_T0Sector";
          emcBank = bankManager->fetchBank("PdbEmcT0SectorBank", bankID, calibname, run);
          if (emcBank)
            {
              TheSectorVd = *( (PdbEmcT0Sector *)( & emcBank->getEntry(0) ) );
              cout << "<I> EmcT0RecalReco::readDataFromObjy(): VD corrections read in." << endl;
              delete emcBank;
              isvalid = 1;
            }

        }
      return EVENT_OK;
    }
  else
    {
      cerr << "<E> Run number could not be determined, aborting..." << endl;
      return ABORTRUN;
    }

}


float EmcT0RecalReco::get_correction( int iarm, int isect, int iy, int iz, float ecent = 0)
{

  //     cout << iarm << "\t" << isect << "\t" << iy << "\t" << iz << "\t"
  // 	 <<  TheSectorVd.getSectorT0Correction(iarm,isect) << "\t"
  // 	 << TheSector.getSectorT0Correction(iarm,isect) << "\t"
  // 	 << TheTower.getTwrT0Correction(iarm, isect, iy, iz)
  // 	 << endl;

  // if PbGl, do nothing

  float corr = 0;
  if (arm == 1 && sector < 2)
    {
      return 0.0;
    }
  else
    {
      corr = TheSector.getSectorT0Correction(iarm, isect) +
	TheTower.getTwrT0Correction(iarm, isect, iy, iz) +
	TheSectorVd.getSectorT0Correction(iarm, isect);

      float ecentcorr = 0;
      // correct for energy dependence if ecent makes sense
      if (ecent > 1.0e-16)
        {
          // OK, this is a hack, but it describes the dependence, OK?
          if (ecent < 1.61)
            {
              ecentcorr = -3.41 + 29.73 * ecent - 121.10 * pow(ecent, 2)
		+ 305.39 * pow(ecent, 3) - 508.80 * pow(ecent, 4)
		+ 564.73 * pow(ecent, 5) - 411.71 * pow(ecent, 6)
		+ 188.77 * pow(ecent, 7) - 49.21 * pow(ecent, 8)
		+ 5.55 * pow(ecent, 9);
            }
          else
            {
              ecentcorr = -0.7711 * (log(ecent) + 0.0165);
            }
        }
      corr = corr + ecentcorr;
    }
  return corr;
}


int EmcT0RecalReco::process_event(PHCompositeNode *topNode)
{
  if (verbosity > 1)
    {
      cout << "Calling process_event in EmcT0RecalReco......." << endl;
    }
  int iret = EVENT_OK;


  emcClusterContainer *clusters = findNode::getClass<emcClusterContainer>(topNode, "emcClusterContainer");
  if (!clusters)
    {
      cout << PHWHERE << "emcClusterContainer Node missing, aborting" << endl;
      return ABORTRUN;
    }

  float ecent = -1;
  float tof = -1, tofmin = -1, tofmax = -1;
  float tofcorr = -1, tofcorrmin = -1, tofcorrmax = -1;
  int towerid = -1;

  if ( !clusters->isValid() )
    return DISCARDEVENT;

  for (size_t i = 0; i < clusters->size();i++)
    {
      emcClusterContent *clus = clusters->getCluster(i);
      ecent = clus->ecent();
      tof = clus->tof();
      tofmin = clus->tofmin();
      tofmax = clus->tofmax();
      tofcorr = clus->tofcorr();
      tofcorrmin = clus->tofcorrmin();
      tofcorrmax = clus->tofcorrmax();

      towerid = clus->towerid(0);

      // apply towercorr
      EmcIndexer::TowerLocation(towerid, arm, sector, iy, iz);

      float corr = get_correction(arm, sector, iy, iz, ecent);

      clus->set_tof(tof - corr);
      clus->set_tofmin(tofmin - corr);
      clus->set_tofmax(tofmax - corr);

      clus->set_tofcorr(tofcorr - corr);
      clus->set_tofcorrmin(tofcorrmin - corr);
      clus->set_tofcorrmax(tofcorrmax - corr);
    }
  return iret;

}
