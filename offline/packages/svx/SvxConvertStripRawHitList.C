#include "SvxConvertStripRawHitList.h"

//phenix libraries
#include "getClass.h"

#include "PHTypedNodeIterator.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include <Fun4AllReturnCodes.h>

#include <SvxRawhitList.h>
#include <SvxRawhitListv5.h>
#include <SvxStripRawHitList.h>
#include <svxAddress.hh>

// C libraries
#include <iostream>
#include <iomanip>
#include <string>



//===========================================================================
SvxConvertStripRawHitList::SvxConvertStripRawHitList() :
  d_rawhitlist(NULL),
  d_striprawhitlist(NULL),
  d_address(NULL),
  d_offset(0),
  d_testmode(false),
  verbosity(0)
{
  ThisName = "SvxConvertStripRawHitList";

}


//===========================================================================
int SvxConvertStripRawHitList::Init(PHCompositeNode *topNode)
{

  return CreateNodes(topNode);
}

//===========================================================================
int SvxConvertStripRawHitList::End(PHCompositeNode *topNode)
{

  return 0;
}

//===========================================================================
int SvxConvertStripRawHitList::InitRun(PHCompositeNode *topNode)
{

  d_address = findNode::getClass<svxAddress>(topNode, "svxAddress");
  if ( d_address == NULL)
  {
    std::cout << PHWHERE  <<  "Can't find svxAddress ... creating one "
              << std::endl;

    d_address = new svxAddress();
    d_address->Initialize();


    if (!d_address)
      std::cout << PHWHERE << " svxAddress is null after creation!!" << std::endl;
    // return ABORTRUN;
  }

  return 0;

}

//===========================================================================
int SvxConvertStripRawHitList::process_event(PHCompositeNode *topNode)
{

  // get the data nodes
  int retval = GetNodes(topNode);
  if (retval != EVENT_OK)
    return retval;

  int preraw = d_rawhitlist->get_nRawhits();

  int nphits = d_striprawhitlist->fill_rawhitlist(d_rawhitlist, d_address, d_offset);

  if (verbosity > 0)
  {
    std::cout << PHWHERE
              << " prerawhits: " << std::setw(5) << preraw
              << " striprawhits: " << std::setw(5) << d_striprawhitlist->get_nhits()
              << " filledrawhits: " << std::setw(5) << nphits
              << " rawhits:" << std::setw(5) << d_rawhitlist->get_nRawhits()
              << std::endl;
  }

  if(d_testmode){
    SvxRawhitList* orgrawhitlist = NULL;
    orgrawhitlist = findNode::getClass<SvxRawhitList>(topNode, "SvxRawhitList");
    if (!orgrawhitlist)
    {
      std::cout << PHWHERE << "ERROR!! Can't find SvxRawhitList(true)!" << std::endl;
      return -1;
    }

    compareRawhitList(d_rawhitlist, orgrawhitlist);
  }

  return 0;
}


//===========================================================================
int SvxConvertStripRawHitList::CreateNodes(PHCompositeNode *topNode)
{

  PHNodeIterator iter(topNode);

  // Look for the DST node
  PHCompositeNode *dstNode;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
  {
    std::cout << PHWHERE << "DST node missing, doing nothing." << std::endl;
    return EVENT_OK;
  }

  // Find/Create the SVX node.
  PHCompositeNode* svxNode = dynamic_cast<PHCompositeNode*>
                             (iter.findFirst("PHCompositeNode", "SVX"));
  if (! svxNode)
  {
    svxNode = new PHCompositeNode("SVX");
    dstNode->addNode(svxNode);
  }

  std::string snode = (d_testmode) ? "SvxRawhitTestList" : "SvxRawhitList";

  // Find/Create raw hits node
  PHIODataNode<PHObject>* SvxRawhitListNode = NULL;
  SvxRawhitListNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", snode.c_str());
  if (!SvxRawhitListNode)
  {
    SvxRawhitListv5 *rawhit = new SvxRawhitListv5();
    SvxRawhitListNode =
      new PHIODataNode<PHObject>(rawhit, snode.c_str(), "PHObject");
    svxNode->addNode(SvxRawhitListNode);
  }

  return EVENT_OK;
}


//===========================================================================
int SvxConvertStripRawHitList::GetNodes(PHCompositeNode *topNode)
{
  //----------------------- SvxStripHits -----------------------------//
  d_striprawhitlist = NULL;
  d_striprawhitlist = findNode::getClass<SvxStripRawHitList>(topNode, "SvxStripHits");
  if (!d_striprawhitlist)
  {
    std::cout << PHWHERE << "ERROR!! Can't find SvxStripRawhitList!" << std::endl;
    return -1;
  }
  //------------------------------------------------------------------//

  //----------------------- SvxRawhitList ----------------------------//
  std::string snode = (d_testmode) ? "SvxRawhitTestList" : "SvxRawhitList";

  d_rawhitlist = NULL;
  d_rawhitlist = findNode::getClass<SvxRawhitList>(topNode, snode.c_str());
  if (!d_rawhitlist)
  {
    std::cout << PHWHERE << "ERROR!! Can't find SvxRawhitList!" << std::endl;
    return -1;
  }
  //------------------------------------------------------------------//

  return EVENT_OK;
}


bool SvxConvertStripRawHitList::compareRawhitList(SvxRawhitList *newlist, SvxRawhitList* orglist)
{
  if(newlist==NULL||orglist==NULL){
    return false;
  }

  int nnew = newlist->get_nRawhits();
  int norg = orglist->get_nRawhits();
 
  if(nnew!=norg){
    std::cout<<"Nrawhit is wrong : "<<nnew<<" "<<norg<<std::endl;
    return false;
  }

  bool result = true;
  for(int i=0; i<nnew; i++){
    SvxRawhit *hitnew = newlist->get_Rawhit(i);
    SvxRawhit *hitorg = orglist->get_Rawhit(i);

    result &= compareRawhit(hitnew, hitorg);
  }
 
  return result;
}

bool SvxConvertStripRawHitList::compareRawhit(SvxRawhit *newhit, SvxRawhit* orghit)
{
  if(newhit==NULL||orghit==NULL){
    return false;
  }

  bool result_all = true, result=true;
  {
    int newlayer = newhit->get_layer();
    int orglayer = orghit->get_layer();
    result_all &= result = (newlayer==orglayer);
    if(!result){ std::cout<<"err :layer "<<newlayer<<" "<<orglayer<<std::endl; }
  }
  {
    int newladder = newhit->get_ladder();
    int orgladder = orghit->get_ladder();
    result_all &= result = (newladder==orgladder);
    if(!result){ std::cout<<"err :ladder "<<newladder<<" "<<orgladder<<std::endl;}
  }
  {
    int newsensor = newhit->get_sensor();
    int orgsensor = orghit->get_sensor();
    result_all &= result = (newsensor==orgsensor);
    if(!result){ std::cout<<"err :sensor "<<newsensor<<" "<<orgsensor<<std::endl;}
  }
  {
    int newsensor = newhit->get_sensor();
    int orgsensor = orghit->get_sensor();
    result_all &= result = (newsensor==orgsensor);
    if(!result){ std::cout<<"err :sensor "<<newsensor<<" "<<orgsensor<<std::endl;}
  }
  {
    int newsection = newhit->get_sensorSection();
    int orgsection = orghit->get_sensorSection();
    result_all &= result = (newsection==orgsection);
    if(!result){ std::cout<<"err :sensorSection "<<newsection<<" "<<orgsection<<std::endl;}
  }
  {
    int newreadout = newhit->get_sensorReadout();
    int orgreadout = orghit->get_sensorReadout();
    result_all &= result = (newreadout==orgreadout);
    if(!result){ std::cout<<"err :sensorReadout "<<newreadout<<" "<<orgreadout<<std::endl;}
  }
  {
    int newtype = newhit->get_sensorType();
    int orgtype = orghit->get_sensorType();
    result_all &= result = (newtype==orgtype);
    if(!result){ std::cout<<"err :sensorType "<<newtype<<" "<<orgtype<<std::endl;}
  }
  {
    int newadc = newhit->get_adc();
    int orgadc = orghit->get_adc();
    result_all &= result = (newadc==orgadc);
    if(!result){ std::cout<<"err :adc "<<newadc<<" "<<orgadc<<std::endl;}
  }
  {
    int newch = newhit->get_channel();
    int orgch = orghit->get_channel();
    result_all &= result = (newch==orgch);
    if(!result){ std::cout<<"err :channel "<<newch<<" "<<orgch<<std::endl;}
  }
  {
    int newmod = newhit->get_pixelModule();
    int orgmod = orghit->get_pixelModule();
    result_all &= result = (newmod==orgmod);
    if(!result){ std::cout<<"err :pixelModule "<<newmod<<" "<<orgmod<<std::endl;}
  }
  {
    int newroc = newhit->get_pixelROC();
    int orgroc = orghit->get_pixelROC();
    result_all &= result = (newroc==orgroc);
    if(!result){ std::cout<<"err :pixelROC "<<newroc<<" "<<orgroc<<std::endl;}
  }

  return result;
}
