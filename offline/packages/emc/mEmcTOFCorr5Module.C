//-------------------------------------------------------------------------
// 
// Package: offline/packages/emc
// 
// Copyright (C) PHENIX collaboration, 2000 
//
// Implementation of class : mEmcTOFCorr5Module
//
// Ken Oyama, CNS, University of Tokyo.
//          Modified by Hisayuki Torii Dec 2000
//          Modified for V05 production by H.Torii Aug/15/2001
//          Modified for Run2V02 production by H.Torii Feb/1/2002
//          Modified for Run03V01 production by Peter Tarjan July/2/2002
//-------------------------------------------------------------------------
#include <Fun4AllReturnCodes.h>
#include "mEmcTOFCorr5Module.h"
#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbCalBank.hh"
#include "recoConsts.h"
#include "EmcIndexer.h"
#include "emcDBMS.h"
#include "emcTowerContainer.h"
#include "emcTowerContent.h"

#include "PHIODataNode.h"

#include <iostream>

using namespace std;


//-------------------------------------------------------------------------

mEmcTOFCorr5Module::mEmcTOFCorr5Module(): SubsysReco("mEmcTOFCorr5Module")
{ 

  isvalid =0;  // we mark this class as unusable

};

//-------------------------------------------------------------------------

mEmcTOFCorr5Module* mEmcTOFCorr5Module::instance()
{ 
  static mEmcTOFCorr5Module* instance_ = new mEmcTOFCorr5Module;
  return instance_;
};

//-------------------------------------------------------------------------


void mEmcTOFCorr5Module::readDataFromDB(const int run_number)
{
  
  int run = run_number;

  PdbBankManager *bankManager = PdbBankManager::instance();  
  PdbApplication *application = bankManager->getApplication();  
  PdbBankID bankID(0);

  // read in every correction even though not all of them
  // will be applied
  if (application->startRead()) 
    {
      PdbCalBank *emcBank;
      
      const char *calibname = "calib.emc.T0_Sector";
      
      emcBank = bankManager->fetchBank("PdbEmcT0SectorBank", bankID, calibname, run);
      if (emcBank)
	{
	  TheSector = *( (PdbEmcT0Sector *)( & emcBank->getEntry(0) ) );
	  cout << "<I> mEmcTOFCorr5Module::readDataFromDB(): Sector corrections read in." << endl;
	  delete emcBank;
	  isvalid = 1;
	}      

      
      calibname = "calib.emc.T0_Tower";     
      emcBank = bankManager->fetchBank("PdbEmcT0TowerBank", bankID, calibname, run_number);
      if(emcBank)
	{
	  TheTower = *( (PdbEmcT0Tower *)( & emcBank->getEntry(0) ) );
	  cout << "<I> mEmcTOFCorr5Module::readDataFromDB(): Tower corrections read in." << endl;
	  delete emcBank;
	  isvalid = 1;
	}


      // ptarjan 2003/06/20
      calibname = "calib.emc.VD_T0Sector";
      emcBank = bankManager->fetchBank("PdbEmcT0SectorBank", bankID, calibname, run_number);
      if (emcBank)
	{
	  TheSectorVd = *( (PdbEmcT0Sector *)( & emcBank->getEntry(0) ) );
	  cout << "<I> mEmcTOFCorr5Module::readDataFromDB(): VD corrections read in." << endl;
	  delete emcBank;
	  isvalid = 1;
	}      
      // ptarjan
     
      application->commit();
    }

  
}
//-------------------------------------------------------------------------
//
// Find index in the run-by-run table and return total correction includes tower-by-tower correction.
//
float mEmcTOFCorr5Module::get_correction( int iarm, int isect, int iz, int iy )
{
 
  // ptarjan 2003/06/20
  // the sectorT0 correction are already applied at this point
//    cout 
//      << iarm << ".." << isect << ".." << iy << ".." << iz << "......" 
//      << "....." << TheSectorVd.getSectorT0Correction(iarm,isect)
//      << "....." << TheTower.getTwrT0Correction(iarm, isect, iy, iz)
//      << endl;
  return ( /*TheSector.getSectorT0Correction(iarm,isect)*/ + TheTower.getTwrT0Correction(iarm, isect, iy, iz) + TheSectorVd.getSectorT0Correction(iarm,isect));
  // ptarjan

}

//-------------------------------------------------------------------------
int mEmcTOFCorr5Module::process_event(PHCompositeNode *topNode) 
{  
  //  int irun;
  float tof,tofcorr;

  if (! isvalid) return ABORTRUN;

  PHNodeIterator i(topNode);  
  //  recoConsts *rc = recoConsts::instance();
  //  irun = rc->get_IntFlag("RUNNUMBER");
  

  PHIODataNode<PHObject>* emcTowerContainerNode = 
    (PHIODataNode<PHObject>*)i.findFirst("PHIODataNode","emcTowerContainer");

  if ( !emcTowerContainerNode ) 
    {
      cerr << __FILE__ << ":" << __LINE__ << " cannot find emcTowerContainer "
	   << "node." << endl;
      return ABORTRUN;
    }

  emcTowerContainer* towers = 
    static_cast<emcTowerContainer*>(emcTowerContainerNode->getData());
  if (!towers)
    {
      cerr << __FILE__ << ":" << __LINE__ << " cannot find emcTowerContainer "
	   << "object." << endl;
      return ABORTRUN;
    }

  if ( !towers->isValid() ) return ABORTRUN;


  for ( size_t i = 0; i < towers->size(); ++i ) 
    {
      emcTowerContent* t = towers->getTower(i);
      int towerID = t->TowerID();
      int iarm,isect,iy,iz;
      float corr;
      
      EmcIndexer::TowerLocation(towerID,iarm,isect,iy,iz);


        if( iarm >= 0 && iarm < 2 && isect >= 0 && isect < 4 &&
  	  iz >= 0 && iz < 96 && iy >= 0 && iy < 48 )
	{
	  tof = t->ToF();
	  //	  energy = t->Energy();
	  
	  corr = get_correction(iarm,isect,iz,iy);
	  tofcorr = tof - corr;

	  // write back the modified time
	  t->SetToF(tofcorr);
	  
  	}
        else 
  	{
	  cout << " mEmcTOFCorr5Module:: arm(" << iarm << "),sect(" << isect
	       <<  "),z(" << iz << "),y(" << iy << ") index are out of range." << endl;
  	}
	
    }

  return EVENT_OK;
  
}

