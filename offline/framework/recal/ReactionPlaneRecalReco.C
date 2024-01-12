
#include <iostream>

#include "getClass.h"
#include "recoConsts.h"
#include "Fun4AllReturnCodes.h"
#include "PHGlobal.h"
#include "ReactionPlaneCalib.h"
#include "RpConst.h"
#include "RpSumXYObject.h"

#include "ReactionPlaneRecalReco.h"

using namespace std;
using namespace RP;

ReactionPlaneRecalReco::ReactionPlaneRecalReco()
  : Recalibrator("ReactionPlaneRecalReco")
{
  d_rpsumxy = 0;
  rpcalib = 0;
  baseclasses.insert("RpSumXYObject");
  runNumber = 0;
}

ReactionPlaneRecalReco::~ReactionPlaneRecalReco()
{
  delete rpcalib;
}

int
ReactionPlaneRecalReco::isValidRun(const int runno) const
{
  if (runno >= 150513 && runno <= 160487) // Run5 Cu+Cu 200GeV
    {
      return 1;
    }
  if (runno >= 161208 && runno <= 163463) // Run5 Cu+Cu 62.4GeV
    {
      return 1;
    }
  if (runno >= 227016 && runno <= 240121)
    {
      return 1;
    }
  if (runno >= 122470 && runno <= 123564) // Run4 Au+Au 62.4 GeV
    {
      return 1;
    }
  if (runno >= 300474 && runno <= 310454) // Run10 AuAu 200 GeV
    {
      return 1;
    }
  return 0;
}

int ReactionPlaneRecalReco::InitRun(PHCompositeNode* topNode)
{
  recoConsts *rc = recoConsts::instance();
  // t:is rc flag is set by the framework
  if ( (runNumber = rc->get_IntFlag("RUNNUMBER")) ){
    cout << "This is run " << runNumber << endl;
  }
  else{
    cerr << "<E> Run number could not be determined, aborting..." << endl;
    return ABORTRUN;
  }
  rpcalib = new ReactionPlaneCalib();

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
    //Not use DB
      //      char file[50];
      //      sprintf(file, "/phenix/hl/data65/phnxhl03/cesar/run10AuAu/reaction_plane_recal/par/reactionPlane.table.run%d", runNumber);
      //      cout << "RpCalibrator:: InitRun(), Fetch RP parameters from " << file << endl;
      //      rpcalib->Fetch(file);
      calibration_ok = 1;
    }
  return 0;
}

int ReactionPlaneRecalReco::process_event(PHCompositeNode* topNode)
{
  if (!calibration_ok)
    {
      return EVENT_OK;
    }

  PHGlobal* d_global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");

  if(!d_global) 
    {
      cerr << PHWHERE << " No PHGlobal object !" << endl;
      return ABORTEVENT;
    }

  d_rpsumxy = findNode::getClass<RpSumXYObject>(topNode, inputnodename.c_str());

  if(!d_rpsumxy)
    {
      cerr << PHWHERE << " No RpSumXYObject object !" << endl;
      return ABORTEVENT;
    }

  float bbcz = d_global->getBbcZVertex();
  bool isBbcZVertexOK = fabs(bbcz)<30.0;

  if( !isBbcZVertexOK ) {
    SetInvalid();

    return EVENT_OK;
  }
  
  int cent=-1;
  if(runNumber>=227016)// && runNumber<=240121)
  {cent = (int)d_global->getCentrality();}
  else{cent = rpcalib->GetCentrality( topNode );}

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

  // BBC
  Qx[0][0]  = d_rpsumxy->getBBCsumX00();
  Qx[1][0]  = d_rpsumxy->getBBCsumX01();
  Qx[2][0]  = d_rpsumxy->getBBCsumX02();
  Qy[0][0]  = d_rpsumxy->getBBCsumY00();
  Qy[1][0]  = d_rpsumxy->getBBCsumY01();
  Qy[2][0]  = d_rpsumxy->getBBCsumY02();
  Qx[0][1]  = d_rpsumxy->getBBCsumX10();
  Qx[1][1]  = d_rpsumxy->getBBCsumX11();
  Qx[2][1]  = d_rpsumxy->getBBCsumX12();
  Qy[0][1]  = d_rpsumxy->getBBCsumY10();
  Qy[1][1]  = d_rpsumxy->getBBCsumY11();
  Qy[2][1]  = d_rpsumxy->getBBCsumY12();

  // SMD
  Qx[3][0]  = d_rpsumxy->getSMDsumX00();
  Qx[4][0]  = d_rpsumxy->getSMDsumX01();
  Qx[5][0]  = d_rpsumxy->getSMDsumX02();
  Qy[3][0]  = d_rpsumxy->getSMDsumY00();
  Qy[4][0]  = d_rpsumxy->getSMDsumY01();
  Qy[5][0]  = d_rpsumxy->getSMDsumY02();

  // FCL
  Qx[9][0]  = d_rpsumxy->getFCLsumX00();
  Qx[10][0] = d_rpsumxy->getFCLsumX01();
  Qx[11][0] = d_rpsumxy->getFCLsumX02();
  Qy[9][0]  = d_rpsumxy->getFCLsumY00();
  Qy[10][0] = d_rpsumxy->getFCLsumY01();
  Qy[11][0] = d_rpsumxy->getFCLsumY02();

  // CNT
  Qx[12][1] = d_rpsumxy->getCNTsumX10();
  Qx[13][1] = d_rpsumxy->getCNTsumX11();
  Qx[14][1] = d_rpsumxy->getCNTsumX12();
  Qx[15][1] = d_rpsumxy->getCNTsumX13();
  Qx[16][1] = d_rpsumxy->getCNTsumX14();
  Qy[12][1] = d_rpsumxy->getCNTsumY10();
  Qy[13][1] = d_rpsumxy->getCNTsumY11();
  Qy[14][1] = d_rpsumxy->getCNTsumY12();
  Qy[15][1] = d_rpsumxy->getCNTsumY13();
  Qy[16][1] = d_rpsumxy->getCNTsumY14();

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

  // Set corrected SumX and SumY in RpSumXYObject
  d_rpsumxy->setBBCsumX00( Qx_cor[0][0]  );
  d_rpsumxy->setBBCsumX01( Qx_cor[1][0]  );
  d_rpsumxy->setBBCsumX02( Qx_cor[2][0]  );
  d_rpsumxy->setBBCsumY00( Qy_cor[0][0]  );
  d_rpsumxy->setBBCsumY01( Qy_cor[1][0]  );
  d_rpsumxy->setBBCsumY02( Qy_cor[2][0]  );
  d_rpsumxy->setBBCsumX10( Qx_cor[0][1]  );
  d_rpsumxy->setBBCsumX11( Qx_cor[1][1]  );
  d_rpsumxy->setBBCsumX12( Qx_cor[2][1]  );
  d_rpsumxy->setBBCsumY10( Qy_cor[0][1]  );
  d_rpsumxy->setBBCsumY11( Qy_cor[1][1]  );
  d_rpsumxy->setBBCsumY12( Qy_cor[2][1]  );

  d_rpsumxy->setSMDsumX00( Qx_cor[3][0]  );
  d_rpsumxy->setSMDsumX01( Qx_cor[4][0]  );
  d_rpsumxy->setSMDsumX02( Qx_cor[5][0]  );
  d_rpsumxy->setSMDsumY00( Qy_cor[3][0]  );
  d_rpsumxy->setSMDsumY01( Qy_cor[4][0]  );
  d_rpsumxy->setSMDsumY02( Qy_cor[5][0]  );

  d_rpsumxy->setFCLsumX00( Qx_cor[9][0]  );
  d_rpsumxy->setFCLsumX01( Qx_cor[10][0] );
  d_rpsumxy->setFCLsumX02( Qx_cor[11][0] );
  d_rpsumxy->setFCLsumY00( Qy_cor[9][0]  );
  d_rpsumxy->setFCLsumY01( Qy_cor[10][0] );
  d_rpsumxy->setFCLsumY02( Qy_cor[11][0] );

  d_rpsumxy->setCNTsumX10( Qx_cor[12][1] );
  d_rpsumxy->setCNTsumX11( Qx_cor[13][1] );
  d_rpsumxy->setCNTsumX12( Qx_cor[14][1] );
  d_rpsumxy->setCNTsumX13( Qx_cor[15][1] );
  d_rpsumxy->setCNTsumX14( Qx_cor[16][1] );
  d_rpsumxy->setCNTsumY10( Qy_cor[12][1] );
  d_rpsumxy->setCNTsumY11( Qy_cor[13][1] );
  d_rpsumxy->setCNTsumY12( Qy_cor[14][1] );
  d_rpsumxy->setCNTsumY13( Qy_cor[15][1] );
  d_rpsumxy->setCNTsumY14( Qy_cor[16][1] );


  if ((runNumber > 227016 && runNumber <= 240121) ||
      (runNumber >= 300474 && runNumber <=310455))
  {
    float Qx2[NDET2-NDET][NHAR];
    float Qy2[NDET2-NDET][NHAR];
    for(int id=NDET; id<NDET2; id++)
    {
      for(int ih=0; ih<NHAR; ih++)
      {
        Qx2[id-NDET][ih] = -9999.;
        Qy2[id-NDET][ih] = -9999.;
      }
    }
   
      // RXN
    Qx2[0][0] = d_rpsumxy->getRXNsumX00();
    Qx2[1][0] = d_rpsumxy->getRXNsumX01();
    Qx2[2][0] = d_rpsumxy->getRXNsumX02();
    Qx2[3][0] = d_rpsumxy->getRXNsumX03();
    Qx2[4][0] = d_rpsumxy->getRXNsumX04();
    Qx2[5][0] = d_rpsumxy->getRXNsumX05();
    Qx2[6][0] = d_rpsumxy->getRXNsumX06();
    Qx2[7][0] = d_rpsumxy->getRXNsumX07();
    Qx2[8][0] = d_rpsumxy->getRXNsumX08();
    Qy2[0][0] = d_rpsumxy->getRXNsumY00();
    Qy2[1][0] = d_rpsumxy->getRXNsumY01();
    Qy2[2][0] = d_rpsumxy->getRXNsumY02();
    Qy2[3][0] = d_rpsumxy->getRXNsumY03();
    Qy2[4][0] = d_rpsumxy->getRXNsumY04();
    Qy2[5][0] = d_rpsumxy->getRXNsumY05();
    Qy2[6][0] = d_rpsumxy->getRXNsumY06();
    Qy2[7][0] = d_rpsumxy->getRXNsumY07();
    Qy2[8][0] = d_rpsumxy->getRXNsumY08();
    Qx2[0][1] = d_rpsumxy->getRXNsumX10();
    Qx2[1][1] = d_rpsumxy->getRXNsumX11();
    Qx2[2][1] = d_rpsumxy->getRXNsumX12();
    Qx2[3][1] = d_rpsumxy->getRXNsumX13();
    Qx2[4][1] = d_rpsumxy->getRXNsumX14();
    Qx2[5][1] = d_rpsumxy->getRXNsumX15();
    Qx2[6][1] = d_rpsumxy->getRXNsumX16();
    Qx2[7][1] = d_rpsumxy->getRXNsumX17();
    Qx2[8][1] = d_rpsumxy->getRXNsumX18();
    Qy2[0][1] = d_rpsumxy->getRXNsumY10();
    Qy2[1][1] = d_rpsumxy->getRXNsumY11();
    Qy2[2][1] = d_rpsumxy->getRXNsumY12();
    Qy2[3][1] = d_rpsumxy->getRXNsumY13();
    Qy2[4][1] = d_rpsumxy->getRXNsumY14();
    Qy2[5][1] = d_rpsumxy->getRXNsumY15();
    Qy2[6][1] = d_rpsumxy->getRXNsumY16();
    Qy2[7][1] = d_rpsumxy->getRXNsumY17();
    Qy2[8][1] = d_rpsumxy->getRXNsumY18();
   
    // MPC
    Qx2[9][0]  = d_rpsumxy->getMPCsumX00();
    Qx2[10][0] = d_rpsumxy->getMPCsumX01();
    Qx2[11][0] = d_rpsumxy->getMPCsumX02();
    Qy2[9][0]  = d_rpsumxy->getMPCsumY00();
    Qy2[10][0] = d_rpsumxy->getMPCsumY01();
    Qy2[11][0] = d_rpsumxy->getMPCsumY02();
    Qx2[9][1]  = d_rpsumxy->getMPCsumX10();
    Qx2[10][1] = d_rpsumxy->getMPCsumX11();
    Qx2[11][1] = d_rpsumxy->getMPCsumX12();
    Qy2[9][1]  = d_rpsumxy->getMPCsumY10();
    Qy2[10][1] = d_rpsumxy->getMPCsumY11();
    Qy2[11][1] = d_rpsumxy->getMPCsumY12();
   
    float Qx_cor2[NDET2-NDET][NHAR];
    float Qy_cor2[NDET2-NDET][NHAR];
    for(int id=NDET; id<NDET2; id++)
    {
      for(int ih=0;ih<NHAR;ih++)
      {
        if( Qx2[id-NDET][ih]>-9999 && Qy2[id-NDET][ih]>-9999 )
        {
          float xmean  = rpcalib->GetSumXmean(id, ih, imul);
          float xsigma = rpcalib->GetSumXsigma(id, ih, imul);
          float ymean  = rpcalib->GetSumYmean(id, ih, imul);
          float ysigma = rpcalib->GetSumYsigma(id, ih, imul);
   
          if(xsigma==0 || ysigma==0)
          {
            if(verbosity>1)
            {
              cout << PHWHERE << " sigma == 0. (idet,ihar)=(" << id << "," << ih << ")" << endl;
            }
            Qx_cor2[id-NDET][ih] = -9999.;
            Qy_cor2[id-NDET][ih] = -9999.;
   
            continue;
          }
   
          Qx_cor2[id-NDET][ih] = ( Qx2[id-NDET][ih] - xmean ) / xsigma;
          Qy_cor2[id-NDET][ih] = ( Qy2[id-NDET][ih] - ymean ) / ysigma;
        }else{
          Qx_cor2[id-NDET][ih] = -9999.;
          Qy_cor2[id-NDET][ih] = -9999.;
        }
      }
    }

    d_rpsumxy->setRXNsumX00( Qx_cor2[0][0] );
    d_rpsumxy->setRXNsumX01( Qx_cor2[1][0] );
    d_rpsumxy->setRXNsumX02( Qx_cor2[2][0] );
    d_rpsumxy->setRXNsumX03( Qx_cor2[3][0] );
    d_rpsumxy->setRXNsumX04( Qx_cor2[4][0] );
    d_rpsumxy->setRXNsumX05( Qx_cor2[5][0] );
    d_rpsumxy->setRXNsumX06( Qx_cor2[6][0] );
    d_rpsumxy->setRXNsumX07( Qx_cor2[7][0] );
    d_rpsumxy->setRXNsumX08( Qx_cor2[8][0] );
    d_rpsumxy->setRXNsumY00( Qy_cor2[0][0] );
    d_rpsumxy->setRXNsumY01( Qy_cor2[1][0] );
    d_rpsumxy->setRXNsumY02( Qy_cor2[2][0] );
    d_rpsumxy->setRXNsumY03( Qy_cor2[3][0] );
    d_rpsumxy->setRXNsumY04( Qy_cor2[4][0] );
    d_rpsumxy->setRXNsumY05( Qy_cor2[5][0] );
    d_rpsumxy->setRXNsumY06( Qy_cor2[6][0] );
    d_rpsumxy->setRXNsumY07( Qy_cor2[7][0] );
    d_rpsumxy->setRXNsumY08( Qy_cor2[8][0] );
    d_rpsumxy->setRXNsumX10( Qx_cor2[0][1] );
    d_rpsumxy->setRXNsumX11( Qx_cor2[1][1] );
    d_rpsumxy->setRXNsumX12( Qx_cor2[2][1] );
    d_rpsumxy->setRXNsumX13( Qx_cor2[3][1] );
    d_rpsumxy->setRXNsumX14( Qx_cor2[4][1] );
    d_rpsumxy->setRXNsumX15( Qx_cor2[5][1] );
    d_rpsumxy->setRXNsumX16( Qx_cor2[6][1] );
    d_rpsumxy->setRXNsumX17( Qx_cor2[7][1] );
    d_rpsumxy->setRXNsumX18( Qx_cor2[8][1] );
    d_rpsumxy->setRXNsumY10( Qy_cor2[0][1] );
    d_rpsumxy->setRXNsumY11( Qy_cor2[1][1] );
    d_rpsumxy->setRXNsumY12( Qy_cor2[2][1] );
    d_rpsumxy->setRXNsumY13( Qy_cor2[3][1] );
    d_rpsumxy->setRXNsumY14( Qy_cor2[4][1] );
    d_rpsumxy->setRXNsumY15( Qy_cor2[5][1] );
    d_rpsumxy->setRXNsumY16( Qy_cor2[6][1] );
    d_rpsumxy->setRXNsumY17( Qy_cor2[7][1] );
    d_rpsumxy->setRXNsumY18( Qy_cor2[8][1] );
   
    d_rpsumxy->setMPCsumX00( Qx_cor2[9][0] );
    d_rpsumxy->setMPCsumX01( Qx_cor2[10][0] );
    d_rpsumxy->setMPCsumX02( Qx_cor2[11][0] );
    d_rpsumxy->setMPCsumY00( Qy_cor2[9][0] );
    d_rpsumxy->setMPCsumY01( Qy_cor2[10][0] );
    d_rpsumxy->setMPCsumY02( Qy_cor2[11][0] );
    d_rpsumxy->setMPCsumX10( Qx_cor2[9][1] );
    d_rpsumxy->setMPCsumX11( Qx_cor2[10][1] );
    d_rpsumxy->setMPCsumX12( Qx_cor2[11][1] );
    d_rpsumxy->setMPCsumY10( Qy_cor2[9][1] );
    d_rpsumxy->setMPCsumY11( Qy_cor2[10][1] );
    d_rpsumxy->setMPCsumY12( Qy_cor2[11][1] );
  }

  return EVENT_OK;
}

int ReactionPlaneRecalReco::SetInvalid()
{
  d_rpsumxy->setBBCsumX00( -9999 );
  d_rpsumxy->setBBCsumX01( -9999 );
  d_rpsumxy->setBBCsumX02( -9999 );
  d_rpsumxy->setBBCsumY00( -9999 );
  d_rpsumxy->setBBCsumY01( -9999 );
  d_rpsumxy->setBBCsumY02( -9999 );
  d_rpsumxy->setBBCsumX10( -9999 );
  d_rpsumxy->setBBCsumX11( -9999 );
  d_rpsumxy->setBBCsumX12( -9999 );
  d_rpsumxy->setBBCsumY10( -9999 );
  d_rpsumxy->setBBCsumY11( -9999 );
  d_rpsumxy->setBBCsumY12( -9999 );

  d_rpsumxy->setSMDsumX00( -9999 );
  d_rpsumxy->setSMDsumX01( -9999 );
  d_rpsumxy->setSMDsumX02( -9999 );
  d_rpsumxy->setSMDsumY00( -9999 );
  d_rpsumxy->setSMDsumY01( -9999 );
  d_rpsumxy->setSMDsumY02( -9999 );

  d_rpsumxy->setFCLsumX00( -9999 );
  d_rpsumxy->setFCLsumX01( -9999 );
  d_rpsumxy->setFCLsumX02( -9999 );
  d_rpsumxy->setFCLsumY00( -9999 );
  d_rpsumxy->setFCLsumY01( -9999 );
  d_rpsumxy->setFCLsumY02( -9999 );

  d_rpsumxy->setCNTsumX10( -9999 );
  d_rpsumxy->setCNTsumX11( -9999 );
  d_rpsumxy->setCNTsumX12( -9999 );
  d_rpsumxy->setCNTsumX13( -9999 );
  d_rpsumxy->setCNTsumX14( -9999 );
  d_rpsumxy->setCNTsumY10( -9999 );
  d_rpsumxy->setCNTsumY11( -9999 );
  d_rpsumxy->setCNTsumY12( -9999 );
  d_rpsumxy->setCNTsumY13( -9999 );
  d_rpsumxy->setCNTsumY14( -9999 );

  if ((runNumber > 227016 && runNumber <= 240121) ||
      (runNumber >= 300474 && runNumber <= 310455))
  {
    d_rpsumxy->setRXNsumX00( -9999 );
    d_rpsumxy->setRXNsumX01( -9999 );
    d_rpsumxy->setRXNsumX02( -9999 );
    d_rpsumxy->setRXNsumX03( -9999 );
    d_rpsumxy->setRXNsumX04( -9999 );
    d_rpsumxy->setRXNsumX05( -9999 );
    d_rpsumxy->setRXNsumX06( -9999 );
    d_rpsumxy->setRXNsumX07( -9999 );
    d_rpsumxy->setRXNsumX08( -9999 );
    d_rpsumxy->setRXNsumY00( -9999 );
    d_rpsumxy->setRXNsumY01( -9999 );
    d_rpsumxy->setRXNsumY02( -9999 );
    d_rpsumxy->setRXNsumY03( -9999 );
    d_rpsumxy->setRXNsumY04( -9999 );
    d_rpsumxy->setRXNsumY05( -9999 );
    d_rpsumxy->setRXNsumY06( -9999 );
    d_rpsumxy->setRXNsumY07( -9999 );
    d_rpsumxy->setRXNsumY08( -9999 );
    d_rpsumxy->setRXNsumX10( -9999 );
    d_rpsumxy->setRXNsumX11( -9999 );
    d_rpsumxy->setRXNsumX12( -9999 );
    d_rpsumxy->setRXNsumX13( -9999 );
    d_rpsumxy->setRXNsumX14( -9999 );
    d_rpsumxy->setRXNsumX15( -9999 );
    d_rpsumxy->setRXNsumX16( -9999 );
    d_rpsumxy->setRXNsumX17( -9999 );
    d_rpsumxy->setRXNsumX18( -9999 );
    d_rpsumxy->setRXNsumY10( -9999 );
    d_rpsumxy->setRXNsumY11( -9999 );
    d_rpsumxy->setRXNsumY12( -9999 );
    d_rpsumxy->setRXNsumY13( -9999 );
    d_rpsumxy->setRXNsumY14( -9999 );
    d_rpsumxy->setRXNsumY15( -9999 );
    d_rpsumxy->setRXNsumY16( -9999 );
    d_rpsumxy->setRXNsumY17( -9999 );
    d_rpsumxy->setRXNsumY18( -9999 );
   
    d_rpsumxy->setMPCsumX00( -9999 );
    d_rpsumxy->setMPCsumX01( -9999 );
    d_rpsumxy->setMPCsumX02( -9999 );
    d_rpsumxy->setMPCsumY00( -9999 );
    d_rpsumxy->setMPCsumY01( -9999 );
    d_rpsumxy->setMPCsumY02( -9999 );
    d_rpsumxy->setMPCsumX10( -9999 );
    d_rpsumxy->setMPCsumX11( -9999 );
    d_rpsumxy->setMPCsumX12( -9999 );
    d_rpsumxy->setMPCsumY10( -9999 );
    d_rpsumxy->setMPCsumY11( -9999 );
    d_rpsumxy->setMPCsumY12( -9999 );
  }

  return 0;
}
