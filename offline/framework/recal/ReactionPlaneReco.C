
#include "ReactionPlaneReco.h"
#include "PHGlobal.h"
#include "ReactionPlaneCalib.h"
#include "ReactionPlaneObject.h"
#include "ReactionPlaneObjectv1.h"
#include "ReactionPlaneObjectv3.h"
#include "RpConst.h"
#include "RpSumXYObject.h"


#include "Fun4AllReturnCodes.h"
#include "recoConsts.h"
#include "getClass.h"

#include "PHNodeReset.h"
#include "PHNodeIterator.h"
#include "PHIODataNode.h"


#include <iostream>
#include <cmath>

typedef PHIODataNode <ReactionPlaneObject> ReactionPlaneObjectNode_t;

using namespace RP;
using namespace std;

ReactionPlaneReco::ReactionPlaneReco()
  : Recalibrator("ReactionPlaneReco")
{
  d_rp = 0;
  rpcalib = 0;
  baseclasses.insert("RpSumXYObject");
}

ReactionPlaneReco::~ReactionPlaneReco()
{
  delete rpcalib;
  return;
}

int
ReactionPlaneReco::isValidRun(const int runno) const
{
  if (runno >= 150513 && runno <= 160487) // Run5 Cu+Cu 200GeV
    {
      return 1;
    }
  if (runno >= 161208 && runno <= 163463) // Run5 Cu+Cu 62.4GeV
    {
      return 1;
    }
  if (runno >= 122470 && runno <= 123564)  // Run4 Au+Au 62 GeV
    {
      return 1;
    }
   if (runno >= 227016 && runno <= 240121)
   {
      return 1;
   }
   if (runno >= 300474 && runno <= 310454) // Run10 AuAu 200 GeV
   {
     return 1;
   }
   if (runno >= 310500 && runno <= 315000) // Run10 AuAu 62 & 39 GeV
   {
     return 1;
   }

  return 0;
}

int ReactionPlaneReco::InitRun(PHCompositeNode* topNode)
{
  recoConsts *rc = recoConsts::instance();
  runNumber = rc->get_IntFlag("RUNNUMBER");
  CreateNodeTree(topNode, runNumber);
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

int 
ReactionPlaneReco::process_event(PHCompositeNode* topNode)
{
  if (!calibration_ok)
    {
      return EVENT_OK;
    }

  PHGlobal* global = NULL;
  global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");

  if(!global) 
    {
      cerr << PHWHERE << " No PHGlobal object !" << endl;
      return ABORTEVENT;
    }


  RpSumXYObject* d_rpsumxy = NULL;
  d_rpsumxy = findNode::getClass<RpSumXYObject>(topNode, inputnodename.c_str());

  if(!d_rpsumxy) 
    {
      cerr << PHWHERE << " No RpSumXYObject object !" << endl;
      return ABORTEVENT;
    }


  float bbcz = global->getBbcZVertex();
  bool isBbcZVertexOK = fabs(bbcz)<30.0;
  if( !isBbcZVertexOK ) {
    d_rp->Reset(); // set rp to -9999.
    return EVENT_OK;
  }

  int cent=-1;
  if(runNumber>=227016)// runNumber<=240121)
  {cent = (int)global->getCentrality();}
  else{cent = rpcalib->GetCentrality( topNode );}

  int imul = rpcalib->GetCentralityBin(cent);
  int izps = rpcalib->GetZVertexBin(bbcz);

  if(imul<0 || izps<0){
    if(verbosity>1){
      cout << PHWHERE << " Error : Multiplicity or Z-vertex, out of range. "
	   << "(imul,izps)=(" << imul << "," << izps << ")" << endl;
    }
    d_rp->Reset(); // set rp to -9999.

    return EVENT_OK;
  }

  float Qx[NDET][NHAR];
  float Qy[NDET][NHAR];
  float psi[NDET][NHAR];

  for(int id=0;id<NDET;id++){
    for(int ih=0;ih<NHAR;ih++){
      Qx[id][ih]  = -9999.;
      Qy[id][ih]  = -9999.;
      psi[id][ih] = -9999;
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

  for (int idet = 0;idet < NDET;idet++) {
    for (int ihar = 0;ihar < NHAR;ihar++) {
      if( Qx[idet][ihar]>-9999 && Qy[idet][ihar]>-9999){
        float psiObs    = atan2(Qy[idet][ihar], Qx[idet][ihar]) / (ihar+1.0);
	psi[idet][ihar] = rpcalib->Flattening(idet, ihar, imul, izps, psiObs);
      }
    }
  }

  // Set reaction plane in ReactionPlaneObject
  d_rp->setBBCrp00( psi[0][0] );
  d_rp->setBBCrp01( psi[1][0] );
  d_rp->setBBCrp02( psi[2][0] );
  d_rp->setBBCrp10( psi[0][1] );
  d_rp->setBBCrp11( psi[1][1] );
  d_rp->setBBCrp12( psi[2][1] );

  d_rp->setSMDrp00( psi[3][0] );
  d_rp->setSMDrp01( psi[4][0] );
  d_rp->setSMDrp02( psi[5][0] );


  if(runNumber<227016)
  {
    d_rp->setMVDrp00( -9999. );
    d_rp->setMVDrp01( -9999. );
    d_rp->setMVDrp02( -9999. );
    d_rp->setMVDrp10( -9999. );
    d_rp->setMVDrp11( -9999. );
    d_rp->setMVDrp12( -9999. );
  }

  d_rp->setFCLrp00( psi[9][0] );
  d_rp->setFCLrp01( psi[10][0] );
  d_rp->setFCLrp02( psi[11][0] );

  d_rp->setCNTrp10( psi[12][1] );
  d_rp->setCNTrp11( psi[13][1] );
  d_rp->setCNTrp12( psi[14][1] );
  d_rp->setCNTrp13( psi[15][1] );
  d_rp->setCNTrp14( psi[16][1] );

  if ((runNumber >= 227016 && runNumber <= 240121) ||
      (runNumber >= 300474 && runNumber <= 310454) ||
      (runNumber >= 310500 && runNumber <= 315000) )
  {
    imul = rpcalib->GetCentralityBin2(cent);
    izps = rpcalib->GetZVertexBin2(bbcz);

    if(imul<0 || izps<0)
    {
      if(verbosity>1)
      {
        cout << PHWHERE << " Error : Multiplicity or Z-vertex, out of range. "
             << "(imul,izps)=(" << imul << "," << izps << ")" << endl;
      }
      d_rp->Reset(); // set rp to -9999.

      return EVENT_OK;
    }
    float Qx2[NDET2-NDET][NHAR];
    float Qy2[NDET2-NDET][NHAR];
    float psi2[NDET2-NDET][NHAR];
    for(int id=NDET; id<NDET2; id++)
    {
      for(int ih=0; ih<NHAR; ih++)
      {
        Qx2[id-NDET][ih]  = -9999.;
        Qy2[id-NDET][ih]  = -9999.;
        psi2[id-NDET][ih] = -9999;
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
   
    for(int idet=NDET; idet<NDET2; idet++)
    {
      for(int ihar=0; ihar<NHAR2; ihar++)
      {
        if(Qx2[idet-NDET][ihar]>-9999 && Qy2[idet-NDET][ihar]>-9999)
        {
          float psiObs = atan2(Qy2[idet-NDET][ihar], Qx2[idet-NDET][ihar]) / (ihar+1.0);
          psi2[idet-NDET][ihar] = rpcalib->Flattening(idet, ihar, imul, izps, psiObs);
/*
if(idet==26 && ihar==1) cout << "MPC_S  psiObs*2 = " << psiObs*2 << "  psi*2 = " << psi2[idet-NDET][ihar]*2 << endl;
if(idet==27 && ihar==1) cout << "MPC_N  psiObs*2 = " << psiObs*2 << "  psi*2 = " << psi2[idet-NDET][ihar]*2 << endl;
if(idet==28 && ihar==1) cout << "MPC_F  psiObs*2 = " << psiObs*2 << "  psi*2 = " << psi2[idet-NDET][ihar]*2 << endl;
*/
        }
      }
    }

    d_rp->setRXNrp00( psi2[0][0] );
    d_rp->setRXNrp01( psi2[1][0] );
    d_rp->setRXNrp02( psi2[2][0] );
    d_rp->setRXNrp03( psi2[3][0] );
    d_rp->setRXNrp04( psi2[4][0] );
    d_rp->setRXNrp05( psi2[5][0] );
    d_rp->setRXNrp06( psi2[6][0] );
    d_rp->setRXNrp07( psi2[7][0] );
    d_rp->setRXNrp08( psi2[8][0] );
    d_rp->setRXNrp10( psi2[0][1] );
    d_rp->setRXNrp11( psi2[1][1] );
    d_rp->setRXNrp12( psi2[2][1] );
    d_rp->setRXNrp13( psi2[3][1] );
    d_rp->setRXNrp14( psi2[4][1] );
    d_rp->setRXNrp15( psi2[5][1] );
    d_rp->setRXNrp16( psi2[6][1] );
    d_rp->setRXNrp17( psi2[7][1] );
    d_rp->setRXNrp18( psi2[8][1] );

    d_rp->setMPCrp00( psi2[9][0] );
    d_rp->setMPCrp01( psi2[10][0] );
    d_rp->setMPCrp02( psi2[11][0] );
    d_rp->setMPCrp10( psi2[9][1] );
    d_rp->setMPCrp11( psi2[10][1] );
    d_rp->setMPCrp12( psi2[11][1] );
  }
  return EVENT_OK;
}

bool ReactionPlaneReco::CreateNodeTree(PHCompositeNode* topNode, int runno)
{

  PHNodeIterator iter(topNode);
  PHCompositeNode* dstNode;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode","DST"));

  ReactionPlaneObjectNode_t* rp = static_cast<ReactionPlaneObjectNode_t*>(iter.findFirst("PHIODataNode","ReactionPlaneObject"));

  // Make the data object
  if((runno>=227016 && runno<=240121) ||
     (runno>=300474 && runno<=310454) ||
     (runno>=310500 && runno<=315000) )
  {
    if(!rp)
    {
      d_rp = new ReactionPlaneObjectv3();
      rp = new PHIODataNode<ReactionPlaneObject>(d_rp, "ReactionPlaneObject", "PHObject");
      dstNode->addNode(rp);
      cout << " ... ReacionPlaneObject version 3 instead. " << endl;
    }
    else{d_rp = rp->getData();}
  }else if((runno>=150513 && runno<=160487) || (runno>=161208 && runno<=163463)){
    if(!rp)
    {
      d_rp = new ReactionPlaneObjectv1();
      rp = new PHIODataNode<ReactionPlaneObject>(d_rp, "ReactionPlaneObject", "PHObject");
      dstNode->addNode(rp);
      cout << " ... ReacionPlaneObject version 1 instead. " << endl;
    }else{d_rp = rp->getData();}
  }else{
    cout << PHWHERE << Name() << " ... Unknown runNumber requested: " << runno << endl;
    cout << PHWHERE << Name() << " ... You will receive Object version 1 instead. " << endl;
    if(!rp)
    {
      d_rp = new ReactionPlaneObjectv1();
      rp = new PHIODataNode<ReactionPlaneObject>(d_rp, "ReactionPlaneObject", "PHObject");
      dstNode->addNode(rp);
    }else{d_rp = rp->getData();}
  }

  return True;
}

void ReactionPlaneReco::Verbosity(const int v)
{
  verbosity = v;
  rpcalib->Verbosity(v);
}
