#include "SvxConvertPixelRawHitList.h"

//phenix libraries
#include "getClass.h"

#include "PHTypedNodeIterator.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include <Fun4AllReturnCodes.h>

#include <SvxRawhitList.h>
#include <SvxRawhitListv5.h>
#include <SvxPixelRawHitList.h>
#include <svxAddress.hh>

// C libraries
#include <iostream>
#include <iomanip>



//===========================================================================
SvxConvertPixelRawHitList::SvxConvertPixelRawHitList() :
  d_rawhitlist(NULL),
  d_pixelrawhitlist(NULL),
  d_address(NULL),
  verbosity(0)
{
  ThisName = "SvxConvertPixelRawHitList";

}


//===========================================================================
int SvxConvertPixelRawHitList::Init(PHCompositeNode *topNode)
{

  return CreateNodes(topNode);
}

//===========================================================================
int SvxConvertPixelRawHitList::End(PHCompositeNode *topNode)
{

  return 0;
}

//===========================================================================
int SvxConvertPixelRawHitList::InitRun(PHCompositeNode *topNode)
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
int SvxConvertPixelRawHitList::process_event(PHCompositeNode *topNode)
{

  // get the data nodes
  int retval = GetNodes(topNode);
  if (retval != EVENT_OK)
    return retval;

  int preraw = d_rawhitlist->get_nRawhits();

  int nphits = d_pixelrawhitlist->fill_rawhitlist(d_rawhitlist, d_address);

  if (verbosity > 0)
  {
    std::cout << PHWHERE
              << " prerawhits: " << std::setw(5) << preraw
              << " pixelrawhits: " << std::setw(5) << d_pixelrawhitlist->get_nhits()
              << " filledrawhits: " << std::setw(5) << nphits
              << " rawhits:" << std::setw(5) << d_rawhitlist->get_nRawhits()
              << std::endl;
  }

  return 0;
}


//===========================================================================
int SvxConvertPixelRawHitList::CreateNodes(PHCompositeNode *topNode)
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

  // Find/Create raw hits node
  PHIODataNode<PHObject>* SvxRawhitListNode = NULL;
  SvxRawhitListNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", "SvxRawhitList");
  if (!SvxRawhitListNode)
  {
    SvxRawhitListv5 *rawhit = new SvxRawhitListv5();
    SvxRawhitListNode =
      new PHIODataNode<PHObject>(rawhit, "SvxRawhitList", "PHObject");
    svxNode->addNode(SvxRawhitListNode);
  }

  return EVENT_OK;
}


//===========================================================================
int SvxConvertPixelRawHitList::GetNodes(PHCompositeNode *topNode)
{
  //----------------------- SvxPixelHits -----------------------------//
  d_pixelrawhitlist = NULL;
  d_pixelrawhitlist = findNode::getClass<SvxPixelRawHitList>(topNode, "SvxPixelHits");
  if (!d_pixelrawhitlist)
  {
    std::cout << PHWHERE << "ERROR!! Can't find SvxPixelRawhitList!" << std::endl;
    return -1;
  }
  //------------------------------------------------------------------//

  //----------------------- SvxRawhitList ----------------------------//
  d_rawhitlist = NULL;
  d_rawhitlist = findNode::getClass<SvxRawhitList>(topNode, "SvxRawhitList");
  if (!d_rawhitlist)
  {
    std::cout << PHWHERE << "ERROR!! Can't find SvxRawhitList!" << std::endl;
    return -1;
  }
  //------------------------------------------------------------------//

  return EVENT_OK;
}


