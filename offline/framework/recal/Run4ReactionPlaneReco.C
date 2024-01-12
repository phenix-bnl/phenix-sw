#include "Run4ReactionPlaneReco.h"
#include "PHGlobal.h"
#include "RpConst.h"
#include "ReactionPlaneCalib.h"
#include "ReactionPlaneObject.h"
#include "ReactionPlaneObjectv1.h"


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

Run4ReactionPlaneReco::Run4ReactionPlaneReco(const int ver)
  : Recalibrator("Run4ReactionPlaneReco"),version(ver)
{
  d_rp = 0;
  rpcalib = 0;
  baseclasses.insert("PHGlobal");
}

Run4ReactionPlaneReco::~Run4ReactionPlaneReco()
{
  delete rpcalib;
}

int
Run4ReactionPlaneReco::isValidRun(const int runno) const
{
  // Valid only Run4
  if (runno >= 107445 && runno <= 123564)
    {
      return 1;
    }
  return 0;
}

int Run4ReactionPlaneReco::InitRun(PHCompositeNode* topNode)
{
  CreateNodeTree(topNode);
  recoConsts *rc = recoConsts::instance();
  int runNumber = rc->get_IntFlag("RUNNUMBER");
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
      calibration_ok = 1;
    }

  return 0;
}

int 
Run4ReactionPlaneReco::process_event(PHCompositeNode* topNode)
{
  if (!calibration_ok)
    {
      return EVENT_OK;
    }

  PHGlobal* global = NULL;
  global = findNode::getClass<PHGlobal>(topNode, inputnodename.c_str());

  if(!global) 
    {
      cerr << PHWHERE << " No PHGlobal object !" << endl;
      return ABORTEVENT;
    }

  float bbcz = global->getBbcZVertex();
  bool isBbcZVertexOK = fabs(bbcz)<30.0;
  if( !isBbcZVertexOK ) {
    d_rp->Reset(); // set rp to -9999.
    return EVENT_OK;
  }

  int cent = rpcalib->GetCentrality(topNode);
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
  Qx[0][0]  = global->getBBCsumX00();
  Qx[1][0]  = global->getBBCsumX01();
  Qx[2][0]  = global->getBBCsumX02();
  Qy[0][0]  = global->getBBCsumY00();
  Qy[1][0]  = global->getBBCsumY01();
  Qy[2][0]  = global->getBBCsumY02();
  Qx[0][1]  = global->getBBCsumX10();
  Qx[1][1]  = global->getBBCsumX11();
  Qx[2][1]  = global->getBBCsumX12();
  Qy[0][1]  = global->getBBCsumY10();
  Qy[1][1]  = global->getBBCsumY11();
  Qy[2][1]  = global->getBBCsumY12();

  // SMD
  Qx[3][0]  = global->getSMDsumX00();
  Qx[4][0]  = global->getSMDsumX01();
  Qx[5][0]  = global->getSMDsumX02();
  Qy[3][0]  = global->getSMDsumY00();
  Qy[4][0]  = global->getSMDsumY01();
  Qy[5][0]  = global->getSMDsumY02();

  // FCL

  global->ShutUp();
  int impl = global->isImplemented(global->getFCLsumX00());
  global->ShutUp(0);
  if (impl)
    {
      Qx[9][0] = global->getFCLsumX00();
      Qx[10][0] = global->getFCLsumX01();
      Qx[11][0] = global->getFCLsumX02();
      Qy[9][0] = global->getFCLsumY00();
      Qy[10][0] = global->getFCLsumY01();
      Qy[11][0] = global->getFCLsumY02();
  }

  // CNT
  global->ShutUp();
  impl = global->isImplemented(global->getCNTsumX10());
  global->ShutUp(0);
  if (impl)
    {

      Qx[12][1] = global->getCNTsumX10();
      Qx[13][1] = global->getCNTsumX11();
      Qx[14][1] = global->getCNTsumX12();
      Qx[15][1] = global->getCNTsumX13();
      Qx[16][1] = global->getCNTsumX14();
      Qy[12][1] = global->getCNTsumY10();
      Qy[13][1] = global->getCNTsumY11();
      Qy[14][1] = global->getCNTsumY12();
      Qy[15][1] = global->getCNTsumY13();
      Qy[16][1] = global->getCNTsumY14();
  }

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

  d_rp->setMVDrp00( -9999. );
  d_rp->setMVDrp01( -9999. );
  d_rp->setMVDrp02( -9999. );
  d_rp->setMVDrp10( -9999. );
  d_rp->setMVDrp11( -9999. );
  d_rp->setMVDrp12( -9999. );

  d_rp->setFCLrp00( psi[9][0] );
  d_rp->setFCLrp01( psi[10][0] );
  d_rp->setFCLrp02( psi[11][0] );

  d_rp->setCNTrp10( psi[12][1] );
  d_rp->setCNTrp11( psi[13][1] );
  d_rp->setCNTrp12( psi[14][1] );
  d_rp->setCNTrp13( psi[15][1] );
  d_rp->setCNTrp14( psi[16][1] );

  return EVENT_OK;
}

bool Run4ReactionPlaneReco::CreateNodeTree(PHCompositeNode* topNode)
{

  PHNodeIterator iter(topNode);
  PHCompositeNode* dstNode;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode","DST"));

  ReactionPlaneObjectNode_t* rp = static_cast<ReactionPlaneObjectNode_t*>(iter.findFirst("PHIODataNode","ReactionPlaneObject"));

  // Make the data object
  switch ( version ){
    case 1:
      if( !rp ){
        d_rp = new ReactionPlaneObjectv1();
	rp = new PHIODataNode<ReactionPlaneObject>(d_rp, "ReactionPlaneObject", "PHObject");
	dstNode->addNode(rp);
      }
      else{
        d_rp = rp->getData();
      }

      break;

    default:
      cout << PHWHERE << Name() << " ... Unknown version requested: " << version << endl;
      cout << PHWHERE << Name() << " ... You will receive version 1 instead. " << endl;
      if( !rp ){
        d_rp = new ReactionPlaneObjectv1();
	rp = new PHIODataNode<ReactionPlaneObject>(d_rp, "ReactionPlaneObject", "PHObject");
	dstNode->addNode(rp);
      }
      else{
        d_rp = rp->getData();
      }

      break;

  }

  return True;
}

void Run4ReactionPlaneReco::Verbosity(const int v)
{
  verbosity = v;
  rpcalib->Verbosity(v);
}
