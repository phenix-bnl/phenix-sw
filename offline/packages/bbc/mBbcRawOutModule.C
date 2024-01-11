#include "mBbcRawOutModule.h"
#include "dBbcRawHitParWrapper.h"
#include "dBbcRawWrapper.h"
#include "BbcEvent.hh"

#include <PHTable.hh>

#include <getClass.h>

#include <cassert>
#include <iostream>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

PHBoolean
mBbcRawOutModule::event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root);
 PHNode *n;
 PHCompositeNode *dstNode, *parNode, *bbcNode, *outNode;

 parNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "PAR"));
 if (!parNode) {
   parNode = new PHCompositeNode("PAR");
   root->addNode(parNode);
 }

 bbcNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "BBC"));
 if (!bbcNode) {
   bbcNode = new PHCompositeNode("BBC");
   root->addNode(bbcNode);
 }

 dstNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DST"));
 if (!dstNode) {
   dstNode = new PHCompositeNode("DST");
   root->addNode(dstNode);
 }

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  outNode = parNode;
  n = i.findFirst("PHIODataNode", "dBbcRawHitPar");
  if (!n) {
    cout << "ERROR:  'in' parameter dBbcRawHitPar not found" << endl;
     PHTable *w = new dBbcRawHitParWrapper("dBbcRawHitPar", 1);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dBbcRawHitPar");
     outNode->addNode(n);
  }
  nodes.append(n);
//
// Fetch BBC Event object status flags
//

  outNode = parNode;
  BbcCalib* calibPar = findNode::getClass<BbcCalib>(outNode, "BbcCalibPar");
  if (!calibPar)
    {
      cout << "ERROR??: Never generate BbcCalib object before. Default value are used" << endl;
      calibPar = new BbcCalib();
      int status = calibPar->restore();
      if ( status == 0 )
	{
	  calibPar->showParameters();
	}
      else if ( status != 0 )
	{
	  return 1;
      }

      PHDataNode<BbcCalib> *CalibNode = new PHDataNode<BbcCalib>(calibPar, "BbcCalibPar");
      outNode->addNode(CalibNode);
    }
//
// Fetch BBCRAW data from BBC node tree.
//

  dBbcRawWrapper* BbcRawWrapper = NULL;
  DBBCRAW_ST* bbcrawwrap = NULL;

  PHTypedNodeIterator<dBbcRawWrapper> bbcwrpiter(root);
  PHIODataNode <dBbcRawWrapper>  *BbcRawWrapperNode = bbcwrpiter.find("dBbcRaw");
  if (BbcRawWrapperNode)
    {
      BbcRawWrapper = BbcRawWrapperNode->getData();
      if (BbcRawWrapper)
	{
          bbcrawwrap = BbcRawWrapper->TableData();
	}
    }
  else
    {
      cout << "dBbcRaw Node missing" << endl;
    }

  //
  // Analyze BBC outputs.
  //
  static int nEvent=0;
  BbcEvent bbc;
  // 
  bbc.Clear();
  bbc.setCalibDataAll(calibPar);
  assert(BbcRawWrapper != NULL);
  if ((int)(BbcRawWrapper->RowCount()) != BBC_N_PMT) 
    {
      cout << "Here, I suppose 128 PMTs, but ... " << BbcRawWrapper->RowCount() << endl;
    }
  //
  // filling data into the 'bbc' object
  //
  bbc.setEventNumber(nEvent);
  if (bbcrawwrap) {

    for (int i = 0; i < BBC_N_PMT; i++) {
      int indx;
      // If simulation flag in BbcCalib is set as 2, flip North/South.
      // This is because hardwired order in BbcGhitRaw.c is North/South
      // after 07/03/01.
      if (calibPar->getSimulation() == 2) {
        if (i < 64) {
          indx = i + 64;
        } else {
          indx = i - 64;
        } 
      } else {
        indx = i;
      }
      bbc.setAdc(bbcrawwrap[i].Adc,indx);
      bbc.setTdc0(bbcrawwrap[i].Tdc0,indx);
      bbc.setTdc1(bbcrawwrap[i].Tdc1,indx);
    }

    // extracting the end products.
    bbc.calculate();
    bbc.DstStore(root);
  }
  return True;
}

