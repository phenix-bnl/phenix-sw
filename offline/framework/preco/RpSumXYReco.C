#include <RpSumXYReco.h>
#include <RpSumXY.h>
#include <RpSumXYObject.h>
#include <RpSumXYObjectv1.h>
#include <RpSumXYObjectv2.h>

#include <Fun4AllReturnCodes.h>

#include <phool.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <PHCompositeNode.h>
#include <RunHeader.h>
#include <RunNumberRanges.h>

#include <getClass.h>

#include <iostream>

using namespace std;
using namespace findNode;

typedef PHIODataNode<RpSumXYObject> RpSumXYObjectNode_t;

static RpSumXY* rpsumxy = 0;
 


RpSumXYReco::RpSumXYReco(const string &name) 
  : SubsysReco(name)
{
  rpsumxy = new RpSumXY();
  myeve = 0;
  d_rpsumxy = 0;
}

RpSumXYReco::~RpSumXYReco()
{
  delete rpsumxy;
}

int RpSumXYReco::End(PHCompositeNode* topNode)
{
  rpsumxy->EndRun();
  return EVENT_OK;
}

int RpSumXYReco::InitRun(PHCompositeNode* topNode)
{

  RunHeader* d_runhdr = getClass<RunHeader>(topNode,"RunHeader");
  if( !d_runhdr ){
    cerr << PHWHERE << " No RunHeader " << endl;
    return ABORTRUN;
  }

  myrun = d_runhdr->get_RunNumber();
  // Create node for RpSumXYObject
  CreateNodeTree(topNode);

  rpsumxy->InitRun( d_runhdr->get_RunNumber() );

  return EVENT_OK;
}

int RpSumXYReco::process_event(PHCompositeNode* topNode)
{
//cout << "RpSumXYReco::process_event" << endl;
  rpsumxy->fillFlowVectorAll(topNode);

  d_rpsumxy->setBBCsumW0( rpsumxy->get_Qw( RpSumXY::RP_BBCS, 1 ) );
  d_rpsumxy->setBBCsumW1( rpsumxy->get_Qw( RpSumXY::RP_BBCN, 1 ) );
  d_rpsumxy->setBBCsumW2( rpsumxy->get_Qw( RpSumXY::RP_BBCSN, 1 ) );
  d_rpsumxy->setFCLsumW0( rpsumxy->get_Qw( RpSumXY::RP_FCLS, 0 ) );
  d_rpsumxy->setFCLsumW1( rpsumxy->get_Qw( RpSumXY::RP_FCLN, 0 ) );
  d_rpsumxy->setFCLsumW2( rpsumxy->get_Qw( RpSumXY::RP_FCLSN, 0 ) );
  d_rpsumxy->setCNTsumW0( rpsumxy->get_Qw( RpSumXY::RP_CNT0, 1 ) );
  d_rpsumxy->setCNTsumW1( rpsumxy->get_Qw( RpSumXY::RP_CNT1, 1 ) );
  d_rpsumxy->setCNTsumW2( rpsumxy->get_Qw( RpSumXY::RP_CNT2, 1 ) );
  d_rpsumxy->setCNTsumW3( rpsumxy->get_Qw( RpSumXY::RP_CNT3, 1 ) );
  d_rpsumxy->setCNTsumW4( rpsumxy->get_Qw( RpSumXY::RP_CNT4, 1 ) );
  
//###################add sumW RXN & MPC 07/3/20###########################//
  if (myrun>BEGIN_OF_RUN7) {
  d_rpsumxy->setRXNsumW0( rpsumxy->get_Qw( RpSumXY::RP_RXNSin, 1 ) );
  d_rpsumxy->setRXNsumW1( rpsumxy->get_Qw( RpSumXY::RP_RXNSout, 1 ) );
  d_rpsumxy->setRXNsumW2( rpsumxy->get_Qw( RpSumXY::RP_RXNS, 1 ) );
  d_rpsumxy->setRXNsumW3( rpsumxy->get_Qw( RpSumXY::RP_RXNNin, 1 ) );
  d_rpsumxy->setRXNsumW4( rpsumxy->get_Qw( RpSumXY::RP_RXNNout, 1 ) );
  d_rpsumxy->setRXNsumW5( rpsumxy->get_Qw( RpSumXY::RP_RXNN, 1 ) );
  d_rpsumxy->setRXNsumW6( rpsumxy->get_Qw( RpSumXY::RP_RXNin, 1 ) );
  d_rpsumxy->setRXNsumW7( rpsumxy->get_Qw( RpSumXY::RP_RXNout, 1 ) );
  d_rpsumxy->setRXNsumW8( rpsumxy->get_Qw( RpSumXY::RP_RXNSN, 1 ) );

  d_rpsumxy->setMPCsumW0( rpsumxy->get_Qw( RpSumXY::RP_MPCS, 1 ) );
  d_rpsumxy->setMPCsumW1( rpsumxy->get_Qw( RpSumXY::RP_MPCN, 1 ) );
  d_rpsumxy->setMPCsumW2( rpsumxy->get_Qw( RpSumXY::RP_MPCSN, 1 ) );
  }
//##################################################################//
  
  d_rpsumxy->setBBCsumX00( rpsumxy->get_Qx( RpSumXY::RP_BBCS, 0 ) );
  d_rpsumxy->setBBCsumX01( rpsumxy->get_Qx( RpSumXY::RP_BBCN, 0 ) );
  d_rpsumxy->setBBCsumX02( rpsumxy->get_Qx( RpSumXY::RP_BBCSN, 0 ) );
  d_rpsumxy->setBBCsumX10( rpsumxy->get_Qx( RpSumXY::RP_BBCS, 1 ) );
  d_rpsumxy->setBBCsumX11( rpsumxy->get_Qx( RpSumXY::RP_BBCN, 1 ) );
  d_rpsumxy->setBBCsumX12( rpsumxy->get_Qx( RpSumXY::RP_BBCSN, 1 ) );
  d_rpsumxy->setBBCsumY00( rpsumxy->get_Qy( RpSumXY::RP_BBCS, 0 ) );
  d_rpsumxy->setBBCsumY01( rpsumxy->get_Qy( RpSumXY::RP_BBCN, 0 ) );
  d_rpsumxy->setBBCsumY02( rpsumxy->get_Qy( RpSumXY::RP_BBCSN, 0 ) );
  d_rpsumxy->setBBCsumY10( rpsumxy->get_Qy( RpSumXY::RP_BBCS, 1 ) );
  d_rpsumxy->setBBCsumY11( rpsumxy->get_Qy( RpSumXY::RP_BBCN, 1 ) );
  d_rpsumxy->setBBCsumY12( rpsumxy->get_Qy( RpSumXY::RP_BBCSN, 1 ) );

  d_rpsumxy->setSMDsumX00( rpsumxy->get_Qx( RpSumXY::RP_SMDS, 0 ) );
  d_rpsumxy->setSMDsumX01( rpsumxy->get_Qx( RpSumXY::RP_SMDN, 0 ) );
  d_rpsumxy->setSMDsumX02( rpsumxy->get_Qx( RpSumXY::RP_SMDSN, 0 ) );
  d_rpsumxy->setSMDsumY00( rpsumxy->get_Qy( RpSumXY::RP_SMDS, 0 ) );
  d_rpsumxy->setSMDsumY01( rpsumxy->get_Qy( RpSumXY::RP_SMDN, 0 ) );
  d_rpsumxy->setSMDsumY02( rpsumxy->get_Qy( RpSumXY::RP_SMDSN, 0 ) );

  d_rpsumxy->setFCLsumX00( rpsumxy->get_Qx( RpSumXY::RP_FCLS, 0 ) );
  d_rpsumxy->setFCLsumX01( rpsumxy->get_Qx( RpSumXY::RP_FCLN, 0 ) );
  d_rpsumxy->setFCLsumX02( rpsumxy->get_Qx( RpSumXY::RP_FCLSN, 0 ) );
  d_rpsumxy->setFCLsumY00( rpsumxy->get_Qy( RpSumXY::RP_FCLS, 0 ) );
  d_rpsumxy->setFCLsumY01( rpsumxy->get_Qy( RpSumXY::RP_FCLN, 0 ) );
  d_rpsumxy->setFCLsumY02( rpsumxy->get_Qy( RpSumXY::RP_FCLSN, 0 ) );

  d_rpsumxy->setCNTsumX10( rpsumxy->get_Qx( RpSumXY::RP_CNT0, 1 ) );
  d_rpsumxy->setCNTsumX11( rpsumxy->get_Qx( RpSumXY::RP_CNT1, 1 ) );
  d_rpsumxy->setCNTsumX12( rpsumxy->get_Qx( RpSumXY::RP_CNT2, 1 ) );
  d_rpsumxy->setCNTsumX13( rpsumxy->get_Qx( RpSumXY::RP_CNT3, 1 ) );
  d_rpsumxy->setCNTsumX14( rpsumxy->get_Qx( RpSumXY::RP_CNT4, 1 ) );
  d_rpsumxy->setCNTsumY10( rpsumxy->get_Qy( RpSumXY::RP_CNT0, 1 ) );
  d_rpsumxy->setCNTsumY11( rpsumxy->get_Qy( RpSumXY::RP_CNT1, 1 ) );
  d_rpsumxy->setCNTsumY12( rpsumxy->get_Qy( RpSumXY::RP_CNT2, 1 ) );
  d_rpsumxy->setCNTsumY13( rpsumxy->get_Qy( RpSumXY::RP_CNT3, 1 ) );
  d_rpsumxy->setCNTsumY14( rpsumxy->get_Qy( RpSumXY::RP_CNT4, 1 ) );

//##################add sumX,Y RXN & MPC 07/3/20#####################//
  if (myrun>BEGIN_OF_RUN7) {
  d_rpsumxy->setRXNsumX00( rpsumxy->get_Qx( RpSumXY::RP_RXNSin, 0 ) );
  d_rpsumxy->setRXNsumX01( rpsumxy->get_Qx( RpSumXY::RP_RXNSout, 0 ) );
  d_rpsumxy->setRXNsumX02( rpsumxy->get_Qx( RpSumXY::RP_RXNS, 0 ) );
  d_rpsumxy->setRXNsumX03( rpsumxy->get_Qx( RpSumXY::RP_RXNNin, 0 ) );
  d_rpsumxy->setRXNsumX04( rpsumxy->get_Qx( RpSumXY::RP_RXNNout, 0 ) );
  d_rpsumxy->setRXNsumX05( rpsumxy->get_Qx( RpSumXY::RP_RXNN, 0 ) );
  d_rpsumxy->setRXNsumX06( rpsumxy->get_Qx( RpSumXY::RP_RXNin, 0 ) );
  d_rpsumxy->setRXNsumX07( rpsumxy->get_Qx( RpSumXY::RP_RXNout, 0 ) );
  d_rpsumxy->setRXNsumX08( rpsumxy->get_Qx( RpSumXY::RP_RXNSN, 0 ) );
  d_rpsumxy->setRXNsumY00( rpsumxy->get_Qy( RpSumXY::RP_RXNSin, 0 ) );
  d_rpsumxy->setRXNsumY01( rpsumxy->get_Qy( RpSumXY::RP_RXNSout, 0 ) );
  d_rpsumxy->setRXNsumY02( rpsumxy->get_Qy( RpSumXY::RP_RXNS, 0 ) );
  d_rpsumxy->setRXNsumY03( rpsumxy->get_Qy( RpSumXY::RP_RXNNin, 0 ) );
  d_rpsumxy->setRXNsumY04( rpsumxy->get_Qy( RpSumXY::RP_RXNNout, 0 ) );
  d_rpsumxy->setRXNsumY05( rpsumxy->get_Qy( RpSumXY::RP_RXNN, 0 ) );
  d_rpsumxy->setRXNsumY06( rpsumxy->get_Qy( RpSumXY::RP_RXNin, 0 ) );
  d_rpsumxy->setRXNsumY07( rpsumxy->get_Qy( RpSumXY::RP_RXNout, 0 ) );
  d_rpsumxy->setRXNsumY08( rpsumxy->get_Qy( RpSumXY::RP_RXNSN, 0 ) );

  d_rpsumxy->setRXNsumX10( rpsumxy->get_Qx( RpSumXY::RP_RXNSin, 1 ) );
  d_rpsumxy->setRXNsumX11( rpsumxy->get_Qx( RpSumXY::RP_RXNSout, 1 ) );
  d_rpsumxy->setRXNsumX12( rpsumxy->get_Qx( RpSumXY::RP_RXNS, 1 ) );
  d_rpsumxy->setRXNsumX13( rpsumxy->get_Qx( RpSumXY::RP_RXNNin, 1 ) );
  d_rpsumxy->setRXNsumX14( rpsumxy->get_Qx( RpSumXY::RP_RXNNout, 1 ) );
  d_rpsumxy->setRXNsumX15( rpsumxy->get_Qx( RpSumXY::RP_RXNN, 1 ) );
  d_rpsumxy->setRXNsumX16( rpsumxy->get_Qx( RpSumXY::RP_RXNin, 1 ) );
  d_rpsumxy->setRXNsumX17( rpsumxy->get_Qx( RpSumXY::RP_RXNout, 1 ) );
  d_rpsumxy->setRXNsumX18( rpsumxy->get_Qx( RpSumXY::RP_RXNSN, 1 ) );
  d_rpsumxy->setRXNsumY10( rpsumxy->get_Qy( RpSumXY::RP_RXNSin, 1 ) );
  d_rpsumxy->setRXNsumY11( rpsumxy->get_Qy( RpSumXY::RP_RXNSout, 1 ) );
  d_rpsumxy->setRXNsumY12( rpsumxy->get_Qy( RpSumXY::RP_RXNS, 1 ) );
  d_rpsumxy->setRXNsumY13( rpsumxy->get_Qy( RpSumXY::RP_RXNNin, 1 ) );
  d_rpsumxy->setRXNsumY14( rpsumxy->get_Qy( RpSumXY::RP_RXNNout, 1 ) );
  d_rpsumxy->setRXNsumY15( rpsumxy->get_Qy( RpSumXY::RP_RXNN, 1 ) );
  d_rpsumxy->setRXNsumY16( rpsumxy->get_Qy( RpSumXY::RP_RXNin, 1 ) );
  d_rpsumxy->setRXNsumY17( rpsumxy->get_Qy( RpSumXY::RP_RXNout, 1 ) );
  d_rpsumxy->setRXNsumY18( rpsumxy->get_Qy( RpSumXY::RP_RXNSN, 1 ) );

  d_rpsumxy->setMPCsumX00( rpsumxy->get_Qx( RpSumXY::RP_MPCS, 0 ) );
  d_rpsumxy->setMPCsumX01( rpsumxy->get_Qx( RpSumXY::RP_MPCN, 0 ) );
  d_rpsumxy->setMPCsumX02( rpsumxy->get_Qx( RpSumXY::RP_MPCSN, 0 ) );
  d_rpsumxy->setMPCsumY00( rpsumxy->get_Qy( RpSumXY::RP_MPCS, 0 ) );
  d_rpsumxy->setMPCsumY01( rpsumxy->get_Qy( RpSumXY::RP_MPCN, 0 ) );
  d_rpsumxy->setMPCsumY02( rpsumxy->get_Qy( RpSumXY::RP_MPCSN, 0 ) );
  
  d_rpsumxy->setMPCsumX10( rpsumxy->get_Qx( RpSumXY::RP_MPCS, 1 ) );
  d_rpsumxy->setMPCsumX11( rpsumxy->get_Qx( RpSumXY::RP_MPCN, 1 ) );
  d_rpsumxy->setMPCsumX12( rpsumxy->get_Qx( RpSumXY::RP_MPCSN, 1 ) );
  d_rpsumxy->setMPCsumY10( rpsumxy->get_Qy( RpSumXY::RP_MPCS, 1 ) );
  d_rpsumxy->setMPCsumY11( rpsumxy->get_Qy( RpSumXY::RP_MPCN, 1 ) );
  d_rpsumxy->setMPCsumY12( rpsumxy->get_Qy( RpSumXY::RP_MPCSN, 1 ) );
  }
//######################################################################//
  myeve++;
//if (myeve%1000==0) cout << "RpSumXYReco::process_event " << myeve << endl;
  return EVENT_OK;
}


bool RpSumXYReco::CreateNodeTree(PHCompositeNode* topNode)
{

  // Find the DST node so we can put objects there
  PHNodeIterator iter(topNode);
  PHCompositeNode* dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode","DST"));

  RpSumXYObjectNode_t *iRPSUMXY =  static_cast<RpSumXYObjectNode_t*>(iter.findFirst("PHIODataNode","RpSumXYObject"));

  // make the data objects
  if( !iRPSUMXY ){
    if (myrun>BEGIN_OF_RUN7) {
      d_rpsumxy = new RpSumXYObjectv2();
    } else {
      d_rpsumxy = new RpSumXYObjectv1();
    }
    iRPSUMXY  = new PHIODataNode<RpSumXYObject>(d_rpsumxy,"RpSumXYObject","PHObject");

    dstNode->addNode(iRPSUMXY);
  }else{
    d_rpsumxy = iRPSUMXY->getData();
    cout << "RpSumXYObjectv1/v2 has been given" << endl;
  }

  return True;
}
