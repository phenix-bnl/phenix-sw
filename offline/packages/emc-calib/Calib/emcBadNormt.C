#include "emcBadNormt.h"
#include "emcCalibrationDataHelper.h"
#include "emcTimeStamp.h"
#include "emcDataManager.h"
#include "PHTimeStamp.h"
#include "EmcIndexer.h"
#include "emcCalFEM.h"
#include "emcRejectList.h"
#include "TSystem.h"
#include "TFile.h"
#include "TH2.h"
#include "TGraph.h"
#include <iostream>
#include <cassert>
#include <sstream>

//_____________________________________________________________________________
emcBadNormt::emcBadNormt(const char* outputdir /* = "/tmp" */ )
  : fOutputDir(gSystem->ExpandPathName(outputdir))
{
  for ( size_t i = 0; i < 6; ++i )
    {
      fHistos.push_back(new TH2F(EmcIndexer::EmcSectorId(i),
				 EmcIndexer::EmcSectorId(i),
				 72,
				 -0.5,71.5,
				 36,
				 -0.5,35.5));
    }
}

//_____________________________________________________________________________
void
emcBadNormt::initGainBaseLine(emcCalibrationDataHelper& cdh)
{
  // BaseLines at end of Run3. Obtained with trim.C to be found in
  // cvs:offline/packages/emc-calib/Calib
  // details="0:xmax:-3:-9:-15

  const float BL[] = { 147.85, // W0 w/o FEM 3,9,15
		       155.23, // W1
		       142.10,   // W2
		       127.58, // W3
		       131.15, // E2
		       129.74  // E3
  };
	
  fGainBaseLineFactor.clear();

  for ( size_t i = 0; i < 108; ++i ) 
    {
      int is,ism;
      EmcIndexer::PXSM144_iSiSM144(i,is,ism);
      fGainBaseLineFactor.push_back(cdh.getGainBaseLine(is,"value")/BL[is]);
    }
}

//_____________________________________________________________________________
void
emcBadNormt::print(std::ostream& os /* = std::cout */)
{
  for (size_t i = 0; i < fRuns.size(); ++i ) 
    {
      os << fRuns[i];
      if ( i < fNbad.size() )
	{
	  os << " " << fNbad[i];
	}
      else
	{
	  os << " not computed.";
	}
      os << std::endl;
    }
}

//_____________________________________________________________________________
void
emcBadNormt::process(float normt_limit /* = 47.84 */ )
{  
  const time_t fTimeOffset=5;

  emcTimeStamp ets;
  ets.SetSource(emcManageable::kDB_Objy);
  emcDataManager* dm = emcDataManager::GetInstance();
  PHTimeStamp ts;

  emcRejectList globalRejectList;  
  emcRejectList* first = 0;

  for ( size_t i = 0; i < fRuns.size(); ++i ) 
    {
      int run = fRuns[i];
      
      std::cout << "Working on run " << run << std::endl;
      
      emcCalibrationDataHelper* cdh;
      
      bool ok = dm->Read(ets,run);
      assert(ok==true);
      ts = ets.getTimeStamp();
      ts += fTimeOffset;
	      
      cdh = new emcCalibrationDataHelper(run,ts,
					 false,
					 emcManageable::kDB_Objy,
					 "pbsc");

      initGainBaseLine(*cdh);

      emcRejectList rl;
      rl.SetDestination(emcManageable::kFile_ASCII);

      for ( size_t ifem = 0; ifem < 108; ++ifem ) 
	{
	  const emcCalFEM* calfem = cdh->getCalibration(ifem,"Gains");
	  assert(calfem!=0);
	  for ( size_t ichannel = 0; ichannel < 144; ++ichannel)
	    {
	      float normt = calfem->getValue(ichannel,ts.getTics());

	      normt /= fGainBaseLineFactor[ifem];

	      if ( normt <= normt_limit )
		{
		  int towerid = EmcIndexer::PXSM144iCH_iPX(ifem,ichannel);
		  rl.set(towerid,1,0,0,0);
		  globalRejectList.set_or(towerid,1,0,0,0);
		  int is,iz,iy;
		  EmcIndexer::decodeTowerId(towerid,is,iz,iy);
		  assert(is>=0 && is < (int)(fHistos.size()));
		  fHistos[is]->Fill(iz,iy,1.0/fRuns.size());
		}
	    }
	}

      if (!first)
	{
	  first = new emcRejectList(rl);
	}

      std::cout << "Number of bad normt=" << rl.size() << std::endl;

      fNbad.push_back(rl.size());

      if ( rl.size() > 0 )
	{
	  std::ostringstream str;
	  str << fOutputDir << "/" << run;
	  dm->SetDestinationDir(str.str().c_str());
	  bool ok = dm->Write(rl);
	  assert(ok==true);				
	}

      rl -= *first;

      std::cout << "Diff to first=" << rl.size() << std::endl;
    }

  std::ostringstream str;
  str << fOutputDir << "/all";
  dm->SetDestinationDir(str.str().c_str());
  globalRejectList.SetDestination(emcManageable::kFile_ASCII);
  bool ok = dm->Write(globalRejectList);
  assert(ok==true);	

  delete first;

  size_t n = fRuns.size();

  double* x = new double[n];
  double* y = new double[n];

  for ( size_t i = 0; i < fRuns.size(); ++i )
    {
      x[i] = fRuns[i];
      y[i] = fNbad[i];
    }

  TGraph g(n,x,y);
  g.SetName("NBADS");

  std::ostringstream out;
  out << fOutputDir << "/histos.root";
  TFile f(out.str().c_str(),"RECREATE");
  for ( size_t i = 0; i < fHistos.size(); ++i )
    {
      fHistos[i]->Write();
    }
  g.Write();
  f.Write();
  f.Close();
}
