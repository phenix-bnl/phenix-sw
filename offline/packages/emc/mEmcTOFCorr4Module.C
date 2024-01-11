//-------------------------------------------------------------------------
//
// 
// Package: offline/packages/emc
// 
// Copyright (C) PHENIX collaboration, 2000 
//
// Implementation of class : mEmcTOFCorr3Module
//
// Ken Oyama, CNS, University of Tokyo.
//          Modified by Hisayuki Torii Dec 2000
//          Modified for V05 production by H.Torii Aug/15/2001
//          Modified for Run2V02 production by H.Torii Feb/1/2002
//-------------------------------------------------------------------------
#include "mEmcTOFCorr4Module.h"
#include "dEmcCalibTower.h"
#include "dEmcCalibTowerWrapper.h"
#include "emcCalibrator.h"
#include "emcRawDataCalibrator.h"
#include "emcRawDataAccessor.h"
#include "emcDataManager.h"
#include "emcPedestals.h"
#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbBankID.hh"
#include "PdbCalBank.hh"
#include "emcDBMS.h"
#include "RunHeader.h"

#include "PHNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHDataNodeIterator.h"

#include <iostream>
#include <fstream>
#include <cassert>

using namespace std;

typedef PHIODataNode <RunHeader> RunHeaderNode_t;

mEmcTOFCorr4Module* mEmcTOFCorr4Module::_instance = NULL;

//-------------------------------------------------------------------------

mEmcTOFCorr4Module::mEmcTOFCorr4Module()
{ 

  isvalid =0;  // we mark this class as unusable

};

//-------------------------------------------------------------------------

mEmcTOFCorr4Module* mEmcTOFCorr4Module::instance()
{
  if (! _instance )
    {
      _instance = new mEmcTOFCorr4Module();
    }
  return _instance;
};

//-------------------------------------------------------------------------


void mEmcTOFCorr4Module::readDataFromDB(const int run_number)
{

  int run = run_number;

  PdbBankManager *bankManager = PdbBankManager::instance();
  assert(bankManager!=0);

  PdbApplication *application = bankManager->getApplication();
  PdbBankID bankID(0);

  PdbCalBank *emcBank;


  if (application->startRead())
    {
      PdbEmcT0Sector *sector;
      PdbEmcT0Tower *tower;

      const char *calibname = "calib.emc.T0_Sector";

      emcBank = bankManager->fetchClosestBank("PdbEmcT0SectorBank", bankID, calibname, run);

      if (emcBank)
        {
          sector = ( PdbEmcT0Sector*) & (emcBank->getEntry(0));
          delete emcBank;
        }
      else
        {
          sector = new PdbEmcT0Sector();
        }

      calibname = "calib.emc.T0_Tower";
      PdbCalBank *emcBank = bankManager->fetchBank("PdbEmcT0TowerBank", bankID, calibname, run_number);

      if (emcBank)
        {
          tower = ( PdbEmcT0Tower*) & (emcBank->getEntry(0));
          delete emcBank;
        }
      else
        {
          tower = new PdbEmcT0Tower();
        }

      TheTower = *tower;
      TheSector = *sector;
      isvalid = 1;         //ok, we are usable now

      application->commit();
    }
}
//-------------------------------------------------------------------------
//
// Find index in the run-by-run table and return total correction includes tower-by-tower correction.
//
float mEmcTOFCorr4Module::get_correction( int iarm, int isect, int iz, int iy )
{
 
  return ( TheSector.getSectorT0Correction(iarm,isect) + TheTower.getTwrT0Correction(iarm, isect, iy, iz) );

}

//-------------------------------------------------------------------------
PHBoolean mEmcTOFCorr4Module::eventFirst(PHCompositeNode *root)
{
  cout<<" mEmcTOFCorr4Module::eventFirst() This is obsolete method.. "<<endl;
  return false;
};
//-------------------------------------------------------------------------
PHBoolean mEmcTOFCorr4Module::event(PHCompositeNode *root) 
{  
  int iarm, isect, iy, iz;
  float tof,tofcorr,energy;

  if (! isvalid) return 0;

  PHNodeIterator i(root);  

  PHTypedNodeIterator<RunHeader> runiter(root);
  RunHeaderNode_t *RunHeaderNode = runiter.find("RunHeader");
  if (RunHeaderNode)
    {
      // int irun = RunHeaderNode->getData()->get_RunNumber();
    }
  else
    {
      cout << PHWHERE << "RunHeader Node missing" << endl;
    }

  // === Patch dEmcCalibTower ===
  PHIODataNode<PHTable>* dEmcCalibTowerNode = (PHIODataNode<PHTable>*)i.findFirst("PHIODataNode","dEmcCalibTower");
  dEmcCalibTowerWrapper * dEmcCalibTower = static_cast<dEmcCalibTowerWrapper*>(dEmcCalibTowerNode->getData());
    
  for( size_t j = 0 ; j < dEmcCalibTower->RowCount(); j++) 
    {
      iarm = dEmcCalibTower->get_arm(j);
      isect = dEmcCalibTower->get_sector(j);
      iy   = dEmcCalibTower->get_ind(1,j);
      iz   = dEmcCalibTower->get_ind(0,j);
      tof = dEmcCalibTower->get_tof(j);
      energy = dEmcCalibTower->get_ecal(j);
      tofcorr = tof;


      if( iarm >= 0 && iarm < 2 && isect >= 0 && isect < 4 &&
	  iz >= 0 && iz < 96 && iy >= 0 && iy < 48 )
	{
	  tofcorr *= TheTower.getLeastCountCorrection(iarm, isect, iy, iz);
	  
	  tofcorr -= TheTower.getSlewingCorrection(iarm, isect, iy, iz) / energy;
	  
	  tofcorr += get_correction(iarm,isect,iz,iy);

	  dEmcCalibTower->set_tof( j ,tofcorr);
	} 
      else 
	{
	  cout << " mEmcTOFCorr4Module:: arm(" << iarm << "),sect(" << isect
	       <<  "),z(" << iz << "),y(" << iy << ") index are out of range." << endl;
	}
    }
  

  return true;
  
}

