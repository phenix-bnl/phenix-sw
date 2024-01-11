#include "GainBaseLinePlayer.h"

#include <cassert>
#include "emcCalibrationDataHelper.h"
#include "Fun4AllServer.h"
#include "RunHeader.h"
#include "emcClusterContainer.h"
#include "emcClusterContent.h"
#include <algorithm>
#include <cctype>
#include <iostream>
#include <map>
#include "EmcIndexer.h"

//_____________________________________________________________________________
GainBaseLinePlayer::GainBaseLinePlayer(const char* runningmode)
  : fCH(0)
{
  ThisName = "GainBaseLinePlayer";
  fRunningMode = runningmode;
  std::transform(fRunningMode.begin(),fRunningMode.end(),
		 fRunningMode.begin(), ::toupper);
  assert(fRunningMode=="UNDO" ||
	 fRunningMode=="TRIM30");
}

//_____________________________________________________________________________
GainBaseLinePlayer::~GainBaseLinePlayer()
{
  delete fCH;
}

//_____________________________________________________________________________
int
GainBaseLinePlayer::End(PHCompositeNode*)
{
  return 0;
}

//_____________________________________________________________________________
int
GainBaseLinePlayer::EndRun(PHCompositeNode*)
{
  return 0;
}

//_____________________________________________________________________________
int
GainBaseLinePlayer::Init(PHCompositeNode*)
{
  return 0;
}

//_____________________________________________________________________________
int
GainBaseLinePlayer::InitRun(PHCompositeNode* topNode)
{
  assert(fCH==0);

  Fun4AllServer* se = Fun4AllServer::instance();

  RunHeader* rh = se->getClass<RunHeader>(topNode,"RunHeader");
  assert(rh!=0);
  int runnumber = rh->get_RunNumber();

  fCH = new emcCalibrationDataHelper(runnumber,false,
				     emcManageable::kDB_Objy,
				     "pbsc");

  // BaseLines at end of Run3. Obtained with trim.C to be found in
  // cvs:offline/packages/emc-calib/Calib
  // details="0:xmax:-3:-9:-15

  const float BL[] = { 147.85, // W0 w/o FEM 3,9,15
		       155.23, // W1
		       142.10, // W2
		       127.58, // W3
		       131.15, // E2
		       129.74  // E3
  };

  std::map<int,float> mcorr;

  for ( size_t is = 0; is < 6; ++is )
    {
      float corr = 1.0;
      if ( fRunningMode == "UNDO" )
	{
	  corr = BL[is]/fCH->getGainBaseLine(is,"value");
	}
      else if ( fRunningMode == "TRIM30" )
	{
	  corr = 
	    fCH->getGainBaseLine(is,"value","30:xmax:-3:-9:-15")/
	    fCH->getGainBaseLine(is,"value");
	}

      mcorr[is] = corr;
    }
  		
  std::cout << "<I> GainBaseLinePlayer::InitRun : Running in "
	    << fRunningMode << " mode. "
	    << std::endl
	    << "Multiplicative correction factors for PbSc energies = "
	    << std::endl;

  std::map<int,float>::const_iterator it;
  for ( it = mcorr.begin(); it != mcorr.end(); ++it ) 
    {
      std::cout << EmcIndexer::EmcSectorId(it->first)
		<< " " 
		<< it->second
		<< std::endl;
    }

  for ( size_t itower = 0; itower < 108*144; ++itower )
    {
      int is,iy,iz;
      EmcIndexer::decodeTowerId(itower,is,iy,iz);
      fCorrectionFactor.push_back(mcorr[is]);
    }

  assert(fCorrectionFactor.size()==108*144);
  return 0;
}

//_____________________________________________________________________________
int
GainBaseLinePlayer::process_event(PHCompositeNode* topNode)
{
  Fun4AllServer* se = Fun4AllServer::instance();

  emcClusterContainer* cont =
    se->getClass<emcClusterContainer>(topNode,"emcClusterContainer");
  assert(cont!=0);

  for ( size_t i = 0; i < cont->size(); ++i )
    {
      emcClusterContent* c = cont->getCluster(i);
      assert(c!=0);
      if ( c->type() == 1 ) 
	{
	  int towerid = c->towerid(0);
	  c->set_e(c->e()*fCorrectionFactor[towerid]);
	  c->set_e9(c->e9()*fCorrectionFactor[towerid]);
	  c->set_ecore(c->ecore()*fCorrectionFactor[towerid]);
	  c->set_ecent(c->ecent()*fCorrectionFactor[towerid]);
	  c->set_etofmin(c->etofmin()*fCorrectionFactor[towerid]);
	  c->set_etofmax(c->etofmax()*fCorrectionFactor[towerid]);
	}
    }

  return 0;
}

//_____________________________________________________________________________
int
GainBaseLinePlayer::Reset(PHCompositeNode*)
{
  return 0;
}

//_____________________________________________________________________________
int
GainBaseLinePlayer::ResetEvent(PHCompositeNode*)
{
  return 0;
}

//_____________________________________________________________________________
void
GainBaseLinePlayer::Print(const char* what) const
{
  
}

