#include "emcManageable.h"
#include "emcDataManager.h"
#include "EmcIndexer.h"
#include "PHTimeStamp.h"
#include <iostream>
#include <vector>
#include <cstdlib>
#include "emcCalibrationData.h"
#include "emcGainFEM.h"
#include "emcQAFEM.h"
#include <fstream>
#include <sstream>
#include "convertNormalization.h"
#include "emcQAs.h"

using namespace std;

// Reads IniCal and Gains to produce "normalization" files, to
// be used e.g. by LV2. 
//
// Usage: convertNormalization(dataSource, [timestamp ptr])

void convertSector(const emcCalibrationData& sector, 
		   const PHTimeStamp& when, 
		   std::ostream& out)
{
  emcDataManager* dm = emcDataManager::GetInstance();

  vector<emcGainFEM*> gains(18);
  vector<emcQAFEM*> qa(18);

  int iS = sector.GetNumber();

  for ( size_t ifem = 0; ifem < gains.size(); ++ifem )
    {
      // SM144 ranges from 0 to 171 (i.e. absolute fem number for PbSc)
      int SM144 = EmcIndexer::iSiSM144_PXSM144(iS,ifem);
      gains[ifem] = new emcGainFEM(SM144);
      gains[ifem]->SetSource(sector.GetSource());
      bool ok = dm->Read(*(gains[ifem]),when);
      if (!ok)
	{
	  cerr << "<E> Cannot read gains for fem " << SM144 << endl;
	  continue;
	}
      qa[ifem] = new emcQAFEM(SM144);
      qa[ifem]->SetSource(sector.GetSource());
      ok = dm->Read(*(qa[ifem]),when);
       if (!ok)
	{
	  cerr << "<E> Cannot read qa for fem " << SM144 << endl;
	  continue;
	}
    }

  ostream::fmtflags oldflags = out.flags() ;

  out << "TowerId FEM FEM-Ch SoftwKey Arm Sector Yrow Zrow   Calib      Bad?" << endl;

  for ( size_t ist = 0; ist < sector.GetSize(); ++ist )
    {
      int iSM, iSMT;
      EmcIndexer::iSiSTiSMiSMT(iS, ist, iSM ,  iSMT);
      int towerid = EmcIndexer::iSiSMiSMTiPX(iS, iSM, iSMT);
      int ifem, ifemChannel;
      EmcIndexer::PXPXSM144CH(towerid,ifem,ifemChannel);

      int arm,sector_in_arm,yrow,zrow;

      EmcIndexer::TowerLocation(towerid,arm,sector_in_arm,yrow,zrow);
			
      bool bad=false;

      float calib=0;

      float g = gains[iSM]->getValue(ifemChannel,0);

      if ( g > 0 ) 
	{
	  calib = sector.GetValue(ist,0)*sector.GetValue(ist,1)/g;
	}

      if ( static_cast<INT32>(qa[iSM]->getValue(ifemChannel,0)) != 0 ) 
	{
	  bad=true;
	}

      out << setw(7) << towerid << " "
	  << setw(3) << ifem << " "
	  << setw(6) << ifemChannel << " "
	  << setw(8) << EmcIndexer::SoftwareKey(towerid) << " "
	  << setw(3) << arm << " "
	  << setw(6) << sector_in_arm << " "
	  << setw(4) << yrow << " "
	  << setw(4) << zrow << " ";
      out.setf(ostream::scientific);
      out << setw(13)
	  << calib << " "
	  << bad << endl;


    }

  // Clean up
   for ( size_t ifem = 0; ifem < gains.size(); ++ifem )
    {
      delete gains[ifem];
      gains[ifem]=0;
    }

   out.setf(oldflags);
}

//_____________________________________________________________________________
void convertNormalization(emcManageable::EStorage fromWhere, PHTimeStamp* ts)
{ 
  emcDataManager* dm = emcDataManager::GetInstance();

  for ( size_t isector = 0; isector < 6; ++isector )
    {
      emcCalibrationData sector(emcCalibrationData::kIniCal,isector);
      sector.SetSource(fromWhere);
      PHTimeStamp when;
      if ( ts ) 
	{
	  when=*ts;
	}

      bool ok = dm->Read(sector,when);
	
      if ( !ok ) 
	{
	  cerr << "<E> Cannot read sector " << isector << endl;
	  continue;
	}
      else
	{
	  std::ostringstream name;

	  name << EmcIndexer::EmcSectorId(isector) << ".INICAL.LVL2";

	  std::ofstream out(name.str().c_str());

	  convertSector(sector,when,out);

	  out.close();
	}
    }
}
