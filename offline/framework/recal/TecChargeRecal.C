#include "TecChargeRecal.h"
#include "TecOutV7.hh"
#include "TecTrackV2.hh"
#include "TecCalibrationObject.hh"
#include "Fun4AllServer.h"
#include "Fun4AllReturnCodes.h"
#include "getClass.h"

#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbBankID.hh"
#include "PdbCalBank.hh"
#include "TClonesArray.h"

#include "PHCompositeNode.h"
#include "PHTimeStamp.h"
#include "recoConsts.h"

#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>

using namespace std;

//_____________________________________________________________________________
TecChargeRecal::TecChargeRecal(): Recalibrator("TecChargeRecal"), TCO(NULL)
{
  baseclasses.insert("TecOut");
}

TecChargeRecal::~TecChargeRecal()
{
  delete TCO;
}

int
TecChargeRecal::isValidRun(const int runno) const
{
  if (runno >= 166416 && runno <= 179829)
    {
      return 1;
    }
  return 0;
}

//_____________________________________________________________________________
int
TecChargeRecal::Init(PHCompositeNode* topNode)
{
  return 0;
}

//_____________________________________________________________________________
int
TecChargeRecal::InitRun(PHCompositeNode* topNode)
{
  TecOutV7* tecout = NULL;
  tecout = findNode::getClass<TecOutV7>(topNode, inputnodename.c_str());
  if (!tecout)
    {
      return 0;
    }

  recoConsts* rc = recoConsts::instance();

  PHTimeStamp t2(rc->get_TimeStamp());

  //  int runno = rc->get_IntFlag("RUNNUMBER");
  TCO = new TecCalibrationObject();
  TCO->setCalibName("calib.tec.tecgain_run04"); // the TEC recalibration table in psql
  TCO->setTimeStamp(t2);
  TCO->FetchAbsGain();

  return 0;
}

//_____________________________________________________________________________
int
TecChargeRecal::process_event(PHCompositeNode* topNode)
{
  TecOutV7* tecout = NULL;
  tecout = findNode::getClass<TecOutV7>(topNode, inputnodename.c_str());
  if (!tecout)
    {
	cout << PHWHERE << inputnodename.c_str() << " Node missing, aborting" << endl;
      return 0;
    }

  for (int tectrack=0; tectrack<tecout->getNTracks(); tectrack++)
    {
      TecTrackV2* tectracktmp;
      tectracktmp = (TecTrackV2*)tecout->GetTecTracks()->UncheckedAt(tectrack);
      int sector = tectracktmp->getSector();
      int side = tectracktmp->getSide();
      for (int k=0; k<6; k++)
	{
	  int index = sector*12 + k*2 + side;
	  float cal = TCO->getAbsoluteGain(index);
	  float val = tectracktmp->getDE(k) * cal;
	  tecout->setTrackDE(tectrack, k, val);
	  val = tectracktmp->getTR(k) * cal;
	  tecout->setTrackTr(tectrack, k, val);	  
	  val = tectracktmp->getWeightedTimeBin(k) * cal;
	  tecout->setTrackWeightedTimeBin(tectrack, k, val);
	  val = tectracktmp->getdEdX06(k) * cal;
	  tecout->setTrackdEdX06(tectrack, k, val);	  
	}
    }
  return 0;
}

