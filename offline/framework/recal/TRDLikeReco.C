#include "TRDLikeReco.h"
#include "TecOutV6.hh"
#include "TecOutV7.hh"
#include "TecTrackTRv3.hh"
#include "TecTrackV2.hh"
#include "TRDLike.hh"
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

using namespace std;

//_____________________________________________________________________________
//_____________________________________________________________________________
TRDLikeReco::TRDLikeReco(): Recalibrator("TRDLikeReco")
{
  baseclasses.insert("TecOut");
  memset(LNH,NULL,sizeof(LNH));
}
TRDLikeReco::~TRDLikeReco()
{
    for (int i=12; i<36; i++)
    {
      delete LNH[i];
    }
}

int
TRDLikeReco::isValidRun(const int runno) const
{
  if (runno >= 166416 && runno <= 179829)
    {
      return 1;
    }
  return 0;
}

//_____________________________________________________________________________
int
TRDLikeReco::Init(PHCompositeNode* topNode)
{
  return 0;
}

//_____________________________________________________________________________
int
TRDLikeReco::InitRun(PHCompositeNode* topNode)
{
    TecOutV7* tecout = NULL;
    tecout = findNode::getClass<TecOutV7>(topNode, inputnodename.c_str());
    if (!tecout)  
    {
	cout << PHWHERE << inputnodename.c_str() << " Node missing, aborting" << endl;
	return 0;
    }
    
    recoConsts* rc = recoConsts::instance();
    
    int run = rc->get_IntFlag("RUNNUMBER");
    
    for (int i=12; i<36; i++)
    {
// 	LDE[i] = new TRDLike(TRDvar::DE, i);
// 	LDE[i]->Fetch(run);
// 	LTR[i] = new TRDLike(TRDvar::TR, i);
// 	LTR[i]->Fetch(run);
// 	LWT[i] = new TRDLike(TRDvar::WTB, i);
// 	LWT[i]->Fetch(run);
	LNH[i] = new TRDLike(TRDvar::NHITS, i);
	LNH[i]->Fetch(run);
//	LNTR[i] = new TRDLike(TRDvar::NTR, i);
//	LNTR[i]->Fetch(run);
    }
    
    return 0;
}

//_____________________________________________________________________________
int
TRDLikeReco::process_event(PHCompositeNode* topNode)
{
  TecOutV7* tecout = NULL;
  tecout = findNode::getClass<TecOutV7>(topNode, inputnodename.c_str());
  if (!tecout)  
  {
      cout << PHWHERE << inputnodename.c_str() << " Node missing, aborting" << endl;
      return 0;
  }

  //  float TRlike, DElike, WTlike, NTRlike, NHlike;
  float NHlike;
  for (int itec = 0; itec < tecout->getNTracks(); itec++)
    {
      //      TRlike = 0.;
      //      DElike = 0.;
      //      NTRlike = 0.;
      NHlike = 0.;
      //      WTlike = 0.;
      for (int k = 0; k < 6; k++)
        {
          int index = tecout->getTrackSector(itec) * 12 + k * 2 + tecout->getTrackSide(itec);

//           float x = tecout->getTrackTr(itec, k);
//           TRlike += LTR[index]->likelihood(x);

//           x = tecout->getTrackWeightedTimeBin(itec, k);
//           WTlike += LWT[index]->likelihood(x);

//           x = tecout->getTrackNtr(itec, k);
//           NTRlike += LNTR[index]->likelihood(x);

//           x = tecout->getTrackDE(itec, k);
//           DElike += LDE[index]->likelihood(x);

          float x = (float)tecout->getTrackNhits(itec, k);
          NHlike += LNH[index]->likelihood(x);
        }
      tecout->setTrackLikelihood(itec,NHlike);
    }
  return 0;
}


