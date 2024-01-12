
#include <iostream>

#include "getClass.h"
#include "PHGlobal.h"

#include "recoConsts.h"
#include "Fun4AllReturnCodes.h"

#include "RpSumXY.h"
#include "RpConst.h"
#include "ReactionPlaneCalib.h"
#include "Run4ReactionPlaneRecalReco.h"

//static ReactionPlaneCalib* rpcalib = 0;

using namespace std;
using namespace RP;

Run4ReactionPlaneRecalReco::Run4ReactionPlaneRecalReco()
  : Recalibrator("Run4ReactionPlaneRecalReco")
{
  d_global = 0;
  baseclasses.insert("PHGlobal");
  runNumber = 0;

  is62GeV = false;
  rpsumxy = 0;
  rpcalib = 0;
}

Run4ReactionPlaneRecalReco::~Run4ReactionPlaneRecalReco()
{
  delete rpsumxy;
  delete rpcalib;
}

int
Run4ReactionPlaneRecalReco::isValidRun(const int runno) const
{
  // Valid only Run4
  if (runno >= 107445 && runno <= 123564)
    {
      return 1;
    }
  return 0;
}

int Run4ReactionPlaneRecalReco::InitRun(PHCompositeNode* topNode)
{
  recoConsts *rc = recoConsts::instance();
  // this rc flag is set by the framework
  if ( (runNumber = rc->get_IntFlag("RUNNUMBER")) ){
    cout << "This is run " << runNumber << endl;
  }
  else{
    cerr << "<E> Run number could not be determined, aborting..." << endl;
    return ABORTRUN;
  }
  if (!rpcalib)
    {
      rpcalib = new ReactionPlaneCalib();
    }
  if (rpcalib->Fetch(runNumber)) // not 0 means failure to load calibs
    {
      cout << PHWHERE << "No Reaction Plane calibration for run " 
	   << runNumber 
	   << ", no Reaction Plane for this run" 
           << endl;
      calibration_ok = 0;
    }
  else
    {
      calibration_ok = 1;
    }

  // 62.4 GeV Au+Au, Run4
  if( runNumber >= 122470  &&  runNumber <= 123564 )
    {
      is62GeV = true;

      // Initialize RpSumXY for 62.4 GeV
      rpsumxy = new RpSumXY();
      rpsumxy->InitRun(runNumber);
    }

  return 0;
}

int Run4ReactionPlaneRecalReco::process_event(PHCompositeNode* topNode)
{
  if (!calibration_ok)
    {
      return EVENT_OK;
    }
  d_global = NULL;
  d_global = findNode::getClass<PHGlobal>(topNode, inputnodename.c_str());

  if(!d_global) 
    {
      cerr << PHWHERE << " No PHGlobal object !" << endl;
      return ABORTEVENT;
    }

  float bbcz = d_global->getBbcZVertex();
  bool isBbcZVertexOK = fabs(bbcz)<30.0;

  if( !isBbcZVertexOK ) {
    SetInvalid();

    return EVENT_OK;
  }

  int cent = rpcalib->GetCentrality( topNode );
  int imul = rpcalib->GetCentralityBin( cent );
  if(imul<0) {
    if(verbosity>1){
      cout << PHWHERE << " Error : Multiplicity out of range. "
	   << "imul= " << imul << endl;
    }
    return EVENT_OK;
  }

  float Qx[NDET][NHAR];
  float Qy[NDET][NHAR];

  for(int id=0;id<NDET;id++){
    for(int ih=0;ih<NHAR;ih++){
      Qx[id][ih] = -9999.;
      Qy[id][ih] = -9999.;
    }
  }

  //__________________________________________________________________________
  // Au+Au 62.4 GeV
  if( is62GeV )
    {
      rpsumxy->fillFlowVectorAll(topNode);

      for(int id=0;id<NDET;id++){
        for(int ih=0;ih<NHAR;ih++){
          Qx[id][ih] = rpsumxy->get_Qx(id, ih);
          Qy[id][ih] = rpsumxy->get_Qy(id, ih);
        }
      }

    }
  else // 200 GeV
    {
      // BBC
      Qx[0][0]  = d_global->getBBCsumX00();
      Qx[1][0]  = d_global->getBBCsumX01();
      Qx[2][0]  = d_global->getBBCsumX02();
      Qy[0][0]  = d_global->getBBCsumY00();
      Qy[1][0]  = d_global->getBBCsumY01();
      Qy[2][0]  = d_global->getBBCsumY02();
      Qx[0][1]  = d_global->getBBCsumX10();
      Qx[1][1]  = d_global->getBBCsumX11();
      Qx[2][1]  = d_global->getBBCsumX12();
      Qy[0][1]  = d_global->getBBCsumY10();
      Qy[1][1]  = d_global->getBBCsumY11();
      Qy[2][1]  = d_global->getBBCsumY12();
  
      // SMD
      Qx[3][0]  = d_global->getSMDsumX00();
      Qx[4][0]  = d_global->getSMDsumX01();
      Qx[5][0]  = d_global->getSMDsumX02();
      Qy[3][0]  = d_global->getSMDsumY00();
      Qy[4][0]  = d_global->getSMDsumY01();
      Qy[5][0]  = d_global->getSMDsumY02();
  
      // FCL
      d_global->ShutUp();
      int impl = d_global->isImplemented(d_global->getFCLsumX00());
      d_global->ShutUp(0);
  
      if (impl)
        {
          Qx[9][0] = d_global->getFCLsumX00();
          Qx[10][0] = d_global->getFCLsumX01();
          Qx[11][0] = d_global->getFCLsumX02();
          Qy[9][0] = d_global->getFCLsumY00();
          Qy[10][0] = d_global->getFCLsumY01();
          Qy[11][0] = d_global->getFCLsumY02();
        }
  
      // CNT
      d_global->ShutUp();
      impl = d_global->isImplemented(d_global->getCNTsumX10());
      d_global->ShutUp(0);
  
      if (impl)
        {
          Qx[12][1] = d_global->getCNTsumX10();
          Qx[13][1] = d_global->getCNTsumX11();
          Qx[14][1] = d_global->getCNTsumX12();
          Qx[15][1] = d_global->getCNTsumX13();
          Qx[16][1] = d_global->getCNTsumX14();
          Qy[12][1] = d_global->getCNTsumY10();
          Qy[13][1] = d_global->getCNTsumY11();
          Qy[14][1] = d_global->getCNTsumY12();
          Qy[15][1] = d_global->getCNTsumY13();
          Qy[16][1] = d_global->getCNTsumY14();
      }

    }


  // Corrected SumX and SumY
  float Qx_cor[NDET][NHAR];
  float Qy_cor[NDET][NHAR];

  for(int id=0;id<NDET;id++){
    for(int ih=0;ih<NHAR;ih++){

      if( Qx[id][ih]>-9999 && Qy[id][ih]>-9999 ){
        float xmean  = rpcalib->GetSumXmean(id, ih, imul);
        float xsigma = rpcalib->GetSumXsigma(id, ih, imul);
        float ymean  = rpcalib->GetSumYmean(id, ih, imul);
        float ysigma = rpcalib->GetSumYsigma(id, ih, imul);

	if(xsigma==0 || ysigma==0){
          if(verbosity>1){
            cout << PHWHERE << " sigma == 0. (idet,ihar)=(" << id << "," << ih << ")" << endl;
	  }

	  Qx_cor[id][ih] = -9999.;
	  Qy_cor[id][ih] = -9999.;

	  continue;
	}

	Qx_cor[id][ih] = ( Qx[id][ih] - xmean ) / xsigma;
	Qy_cor[id][ih] = ( Qy[id][ih] - ymean ) / ysigma;
      }
      else{
	Qx_cor[id][ih] = -9999.;
	Qy_cor[id][ih] = -9999.;
      }

    }
  }


  // Set corrected SumX and SumY in PHGlobal
  // suppress virtual warnings for setting variables
  d_global->ShutUp();
  d_global->setBBCsumX00( Qx_cor[0][0]  );
  d_global->setBBCsumX01( Qx_cor[1][0]  );
  d_global->setBBCsumX02( Qx_cor[2][0]  );
  d_global->setBBCsumY00( Qy_cor[0][0]  );
  d_global->setBBCsumY01( Qy_cor[1][0]  );
  d_global->setBBCsumY02( Qy_cor[2][0]  );
  d_global->setBBCsumX10( Qx_cor[0][1]  );
  d_global->setBBCsumX11( Qx_cor[1][1]  );
  d_global->setBBCsumX12( Qx_cor[2][1]  );
  d_global->setBBCsumY10( Qy_cor[0][1]  );
  d_global->setBBCsumY11( Qy_cor[1][1]  );
  d_global->setBBCsumY12( Qy_cor[2][1]  );

  d_global->setSMDsumX00( Qx_cor[3][0]  );
  d_global->setSMDsumX01( Qx_cor[4][0]  );
  d_global->setSMDsumX02( Qx_cor[5][0]  );
  d_global->setSMDsumY00( Qy_cor[3][0]  );
  d_global->setSMDsumY01( Qy_cor[4][0]  );
  d_global->setSMDsumY02( Qy_cor[5][0]  );

  d_global->setFCLsumX00( Qx_cor[9][0]  );
  d_global->setFCLsumX01( Qx_cor[10][0] );
  d_global->setFCLsumX02( Qx_cor[11][0] );
  d_global->setFCLsumY00( Qy_cor[9][0]  );
  d_global->setFCLsumY01( Qy_cor[10][0] );
  d_global->setFCLsumY02( Qy_cor[11][0] );

  d_global->setCNTsumX10( Qx_cor[12][1] );
  d_global->setCNTsumX11( Qx_cor[13][1] );
  d_global->setCNTsumX12( Qx_cor[14][1] );
  d_global->setCNTsumX13( Qx_cor[15][1] );
  d_global->setCNTsumX14( Qx_cor[16][1] );
  d_global->setCNTsumY10( Qy_cor[12][1] );
  d_global->setCNTsumY11( Qy_cor[13][1] );
  d_global->setCNTsumY12( Qy_cor[14][1] );
  d_global->setCNTsumY13( Qy_cor[15][1] );
  d_global->setCNTsumY14( Qy_cor[16][1] );
  d_global->ShutUp(0);

  return EVENT_OK;
}

int Run4ReactionPlaneRecalReco::SetInvalid()
{
  d_global->ShutUp();
  d_global->setBBCsumX00( -9999 );
  d_global->setBBCsumX01( -9999 );
  d_global->setBBCsumX02( -9999 );
  d_global->setBBCsumY00( -9999 );
  d_global->setBBCsumY01( -9999 );
  d_global->setBBCsumY02( -9999 );
  d_global->setBBCsumX10( -9999 );
  d_global->setBBCsumX11( -9999 );
  d_global->setBBCsumX12( -9999 );
  d_global->setBBCsumY10( -9999 );
  d_global->setBBCsumY11( -9999 );
  d_global->setBBCsumY12( -9999 );

  d_global->setSMDsumX00( -9999 );
  d_global->setSMDsumX01( -9999 );
  d_global->setSMDsumX02( -9999 );
  d_global->setSMDsumY00( -9999 );
  d_global->setSMDsumY01( -9999 );
  d_global->setSMDsumY02( -9999 );

  d_global->setFCLsumX00( -9999 );
  d_global->setFCLsumX01( -9999 );
  d_global->setFCLsumX02( -9999 );
  d_global->setFCLsumY00( -9999 );
  d_global->setFCLsumY01( -9999 );
  d_global->setFCLsumY02( -9999 );

  d_global->setCNTsumX10( -9999 );
  d_global->setCNTsumX11( -9999 );
  d_global->setCNTsumX12( -9999 );
  d_global->setCNTsumX13( -9999 );
  d_global->setCNTsumX14( -9999 );
  d_global->setCNTsumY10( -9999 );
  d_global->setCNTsumY11( -9999 );
  d_global->setCNTsumY12( -9999 );
  d_global->setCNTsumY13( -9999 );
  d_global->setCNTsumY14( -9999 );
  d_global->ShutUp(0);

  return 0;
}
