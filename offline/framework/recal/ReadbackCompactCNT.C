#include "ReadbackCompactCNT.h"

#include <compactCNT/RecoverTrackProjections.h>
#include <compactCNT/RecoverTrackLineProjections.h>
#include <compactCNT/RecoverTrackPathLengths.h>
#include <compactCNT/RecoverTrackHits.h>
#include <compactCNT/RecoverDchHits.h>
#include <compactCNT/RecoverPadHits.h>
#include <compactCNT/RecoverTofeHits.h>
#include <compactCNT/RecoverTofwHits.h>
#include <compactCNT/RecoverCrkHits.h>
#include <compactCNT/RecoverTecHits.h>
#include <compactCNT/RecoverAccHits.h>
#include <compactCNT/RecoverHbdHits.h>
#include <compactCNT/RecoverEmcHits.h>
#include <compactCNT/RecoverSvxHits.h>
#include <compactCNT/SvxCompactToDST.h>
#include <compactCNT/CreateCNT.h>
#include <compactCNT/FillCNT_TrackProjections.h>
#include <compactCNT/FillCNT_TrackPathLengths.h>
#include <compactCNT/FillCNT_TrackHits.h>
#include <compactCNT/FillCNT_DchHits.h>
#include <compactCNT/FillCNT_TofeHits.h>
#include <compactCNT/FillCNT_TofwHits.h>
#include <compactCNT/FillCNT_PadHits.h>
#include <compactCNT/FillCNT_CrkHits.h>
#include <compactCNT/FillCNT_TecHits.h>
#include <compactCNT/FillCNT_AccHits.h>
#include <compactCNT/FillCNT_EmcHits.h>
#include <compactCNT/FillCNT_EmcPc3.h>
#include <PHCompositeNode.h>
#include <Fun4AllReturnCodes.h>
#include <RunNumberRanges.h>
#include <PHCentralTrackv24.h>
#include <getClass.h>
#include <recoConsts.h>
#include <iostream>

using namespace std;
using namespace findNode;

typedef PHIODataNode<PHObject> PHObjectNode_t;

ReadbackCompactCNT::ReadbackCompactCNT(const std::string& name) : Recalibrator(name) {
  // need a single baseclass that corresponds to a single node that will always be around...
  // TrackLineProjection_VarArray is the only node that uses VariableArrayInt
  baseclasses.insert("VariableArrayInt");

  fRecoverTrackProjections = new RecoverTrackProjections();
  fRecoverTrackLineProjections = new RecoverTrackLineProjections();
  fRecoverTrackPathLengths = new RecoverTrackPathLengths();
  fRecoverTrackHits = new RecoverTrackHits();
  fRecoverDchHits = new RecoverDchHits();
  fRecoverPadHits = new RecoverPadHits();
  fRecoverTofeHits = new RecoverTofeHits();
  fRecoverTofwHits = new RecoverTofwHits();
  fRecoverCrkHits = new RecoverCrkHits();
  fRecoverTecHits = new RecoverTecHits();
  fRecoverAccHits = new RecoverAccHits();
  fRecoverHbdHits = new RecoverHbdHits();
  fRecoverEmcHits = new RecoverEmcHits();
  fRecoverSvxHits = new RecoverSvxHits();
  fCreateCNT = new CreateCNT;
  fFillTrackProjections = new FillCNT_TrackProjections();
  fFillTrackPathLengths = new FillCNT_TrackPathLengths();
  fFillTrackHits = new FillCNT_TrackHits();
  fFillDchHits = new FillCNT_DchHits();
  fFillTofeHits = new FillCNT_TofeHits();
  fFillTofwHits = new FillCNT_TofwHits();
  fFillPadHits = new FillCNT_PadHits();
  fFillCrkHits = new FillCNT_CrkHits();
  fFillTecHits = new FillCNT_TecHits();
  fFillAccHits = new FillCNT_AccHits();
  fFillEmcHits = new FillCNT_EmcHits();
  fFillEmcPc3 = new FillCNT_EmcPc3();
  fSvxCompactToDST = new SvxCompactToDST();
}

ReadbackCompactCNT::~ReadbackCompactCNT(){
  delete fRecoverTrackProjections;
  delete fRecoverTrackLineProjections;
  delete fRecoverTrackPathLengths;
  delete fRecoverTrackHits;
  delete fRecoverDchHits;
  delete fRecoverPadHits;
  delete fRecoverTofeHits;
  delete fRecoverTofwHits;
  delete fRecoverCrkHits;
  delete fRecoverTecHits;
  delete fRecoverAccHits;
  delete fRecoverHbdHits;
  delete fRecoverEmcHits;
  delete fRecoverSvxHits;
  delete fCreateCNT;
  delete fFillTrackProjections;
  delete fFillTrackPathLengths;
  delete fFillTrackHits;
  delete fFillDchHits;
  delete fFillTofeHits;
  delete fFillTofwHits;
  delete fFillPadHits;
  delete fFillCrkHits;
  delete fFillTecHits;
  delete fFillAccHits;
  delete fFillEmcHits;
  delete fFillEmcPc3;
  delete fSvxCompactToDST;
}

int ReadbackCompactCNT::Init(PHCompositeNode *topNode)
{

  PHCentralTrack *phcentraltrack_exists = findNode::getClass<PHCentralTrackv24>(topNode, "PHCentralTrack");

  if(!phcentraltrack_exists)
    {
      PHNodeIterator iter(topNode);
      PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

      PHCentralTrack *phcnt = new PHCentralTrackv24();

      // Make node

      PHObjectNode_t *recal_cgl_node = new PHObjectNode_t(phcnt, "PHCentralTrack", "PHObject");

      // Put the node in the tree below "DST"...
      dstNode->addNode(recal_cgl_node);

      cout << PHWHERE << "      Added PHCentralTrack node to node tree " << endl;
    }

  return EVENT_OK;

}


int ReadbackCompactCNT::isValidRun(const int irun) const {
  //valid for Run-10 and beyond
  if(irun >= BEGIN_OF_RUN10)
    return 1;
  return 0;
}

int ReadbackCompactCNT::InitRun(PHCompositeNode *topNode){
  int ret = fRecoverTrackProjections->InitRun(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverTrackLineProjections->InitRun(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverTrackPathLengths->InitRun(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverTrackHits->InitRun(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverDchHits->InitRun(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverPadHits->InitRun(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverTofeHits->InitRun(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverTofwHits->InitRun(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverCrkHits->InitRun(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverTecHits->InitRun(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverAccHits->InitRun(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverHbdHits->InitRun(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverEmcHits->InitRun(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverSvxHits->InitRun(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fCreateCNT->InitRun(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillTrackProjections->InitRun(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillTrackPathLengths->InitRun(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillTrackHits->InitRun(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillDchHits->InitRun(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillTofeHits->InitRun(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillTofwHits->InitRun(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillPadHits->InitRun(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillCrkHits->InitRun(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillTecHits->InitRun(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillAccHits->InitRun(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillEmcHits->InitRun(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillEmcPc3->InitRun(topNode);
  if(ret != EVENT_OK)
    return ret;

  recoConsts* rc = recoConsts::instance();
  if( rc->FlagExist(  "RECAL_SVXCOMPACTTODST_SKIP") &&
      rc->get_IntFlag("RECAL_SVXCOMPACTTODST_SKIP")!=0 )
  {
    cout<<"RdBckCNT SvxCompactToDST::InitRun skipped"<<endl;
  } else {
    cout<<"RdBckCNT SvxCompactToDST::InitRun "<<endl;
    ret = fSvxCompactToDST->InitRun(topNode);
    if(ret != EVENT_OK)
      return ret;
  }

  return ret;
}

int ReadbackCompactCNT::process_event(PHCompositeNode *topNode){
  int ret = fRecoverTrackProjections->process_event(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverTrackLineProjections->process_event(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverTrackPathLengths->process_event(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverTrackHits->process_event(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverDchHits->process_event(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverPadHits->process_event(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverTofeHits->process_event(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverTofwHits->process_event(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverCrkHits->process_event(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverTecHits->process_event(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverAccHits->process_event(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverHbdHits->process_event(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverEmcHits->process_event(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverSvxHits->process_event(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fCreateCNT->process_event(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillTrackProjections->process_event(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillTrackPathLengths->process_event(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillTrackHits->process_event(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillDchHits->process_event(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillTofeHits->process_event(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillTofwHits->process_event(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillPadHits->process_event(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillCrkHits->process_event(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillTecHits->process_event(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillAccHits->process_event(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillEmcHits->process_event(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillEmcPc3->process_event(topNode);
  if(ret != EVENT_OK)
    return ret;

  recoConsts* rc = recoConsts::instance();
  if( rc->FlagExist(  "RECAL_SVXCOMPACTTODST_SKIP") &&
      rc->get_IntFlag("RECAL_SVXCOMPACTTODST_SKIP")!=0 )
  {
    if(verbosity>0) 
      cout<<"RdBckCNT SvxCompactToDST::process_event skipped"<<endl;
  } else {
    if(verbosity>0) 
      cout<<"RdBckCNT SvxCompactToDST::process_event"<<endl;
    ret = fSvxCompactToDST->process_event(topNode);
    if(ret != EVENT_OK)
      return ret;
  }

  return ret;
}

int ReadbackCompactCNT::End(PHCompositeNode *topNode){
  int ret = fRecoverTrackProjections->End(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverTrackLineProjections->End(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverTrackPathLengths->End(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverTrackHits->End(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverDchHits->End(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverPadHits->End(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverTofeHits->End(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverTofwHits->End(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverCrkHits->End(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverTecHits->End(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverAccHits->End(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverHbdHits->End(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverEmcHits->End(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fRecoverSvxHits->End(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillTrackProjections->End(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillTrackProjections->End(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillTrackHits->End(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillDchHits->End(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillTofeHits->End(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillTofwHits->End(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillPadHits->End(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillCrkHits->End(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillTecHits->End(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillAccHits->End(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillEmcHits->End(topNode);
  if(ret != EVENT_OK)
    return ret;
  ret = fFillEmcPc3->End(topNode);
  if(ret != EVENT_OK)
    return ret;

  recoConsts* rc = recoConsts::instance();
  if( rc->FlagExist(  "RECAL_SVXCOMPACTTODST_SKIP") &&
      rc->get_IntFlag("RECAL_SVXCOMPACTTODST_SKIP")!=0 )
  {
    if(verbosity>0) 
      cout<<"RdBckCNT SvxCompactToDST::End skipped"<<endl;
  } else {
    if(verbosity>0) 
      cout<<"RdBckCNT SvxCompactToDST::End"<<endl;
    ret = fSvxCompactToDST->End(topNode);
    return ret;
  }

  return ret;
}
