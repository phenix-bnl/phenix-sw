/****************************************************************************
 *        Convert from mTofEvaluate.c to mTofEvaluateModule.C
 *                     [ STAF wrapper => pure PHOOL module ]
 ***************************************************************************/
/*:>--------------------------------------------------------------------
**: FILE:       mTofEvaluate.c
**: AUTHOR:     Akio Kiyomichi (Univ. of Tsukuba)
**: HISTORY:     6/19/98 A.Kiyomichi  First Version
**:              8/09/98 A.Kiyomichi  Add Vertex info. using DIO function
**:              9/29/98 H.Sako       Added eval_h->maxlen checking
**:             12/21/00 A.Kiyomichi  create pure PHOOL module
**:<------------------------------------------------------------------*/


#include "mTofEvaluateModule.h"

#include "dTofGdigiWrapper.h"
#include "dTofGhitGdigiWrapper.h"
#include "dTofGhitRawWrapper.h"
#include "dTofRawWrapper.h"
#include "dTofRawRecWrapper.h"
#include "dTofReconstructedWrapper.h"
#include "dTofGdigiRecWrapper.h"   // Output

#include "PHIODataNode.h"
#include "PHTable.hh"

#include <cmath>
#include <cstdlib>
#include <iostream>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

PHBoolean
mTofEvaluateModule::event(PHCompositeNode *root) {
  PHPointerList<PHNode> nodes;
  PHNodeIterator iii(root), *jjj;
  PHNode *n;
  TableNode_t *d;
  //PHTable *w;
  PHCompositeNode *parNode, *tofNode, *geaNode, *dstNode, *evaNode, *outNode;
  
  parNode = static_cast<PHCompositeNode*>(iii.findFirst("PHCompositeNode", "PAR"));
  if (!parNode) {
    parNode = new PHCompositeNode("PAR");
    root->addNode(parNode);
  }
  
  tofNode = static_cast<PHCompositeNode*>(iii.findFirst("PHCompositeNode", "TOF"));
  if (!tofNode) {
    tofNode = new PHCompositeNode("TOF");
    root->addNode(tofNode);
  }
  
  geaNode = static_cast<PHCompositeNode*>(iii.findFirst("PHCompositeNode", "GEA"));
  if (!geaNode) {
    geaNode = new PHCompositeNode("GEA");
    root->addNode(geaNode);
  }
  
  dstNode = static_cast<PHCompositeNode*>(iii.findFirst("PHCompositeNode", "DST"));
  if (!dstNode) {
    dstNode = new PHCompositeNode("DST");
    root->addNode(dstNode);
  }
  
  evaNode = static_cast<PHCompositeNode*>(iii.findFirst("PHCompositeNode", "EVA"));
  if (!evaNode) {
    evaNode = new PHCompositeNode("EVA");
    root->addNode(evaNode);
  }

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  // Extract the data from the dTofGdigi
  dTofGdigiWrapper *TofGdigiWrapper;
  outNode = evaNode;
  n = iii.findFirst("PHIODataNode", "dTofGdigi");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofGdigi not found" << endl;
     TofGdigiWrapper = new dTofGdigiWrapper("dTofGdigi", 300);
     if (!TofGdigiWrapper) {
       return 1;
     }
     n = new TableNode_t(TofGdigiWrapper,"dTofGdigi");
     outNode->addNode(n);
  }
  nodes.append(n);
  jjj = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofGdigi"));
  if (!d) {
    cerr << "  Error  in dTofGdigi"<< endl; return 1;
  } else {
    TofGdigiWrapper = static_cast<dTofGdigiWrapper*>(d->getData());
    if (!TofGdigiWrapper) {cerr<<" Error in dTofGdigi"<< endl; return 1;}
  }
  delete jjj;
  //DTOFGDIGI_ST *dTofGdigi = TofGdigiWrapper->TableData();

  // Extract the data from the dTofGhitGdigi
  dTofGhitGdigiWrapper *TofGhitGdigiWrapper;
  outNode = evaNode;
  n = iii.findFirst("PHIODataNode", "dTofGhitGdigi");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofGhitGdigi not found" << endl;
     TofGhitGdigiWrapper = new dTofGhitGdigiWrapper("dTofGhitGdigi", 300);
     if (!TofGhitGdigiWrapper) {
       return 1;
     }
     n = new TableNode_t(TofGhitGdigiWrapper,"dTofGhitGdigi");
     outNode->addNode(n);
  }
  nodes.append(n);
  jjj = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofGhitGdigi"));
  if (!d) {
    cerr << "  Error  in dTofGhitGdigi"<< endl; return 1;
  } else {
    TofGhitGdigiWrapper = static_cast<dTofGhitGdigiWrapper*>(d->getData());
    if (!TofGhitGdigiWrapper) {cerr<<" Error in dTofGhitGdigi"<< endl; return 1;}
  }
  delete jjj;
  DTOFGHITGDIGI_ST *dTofGhitGdigi = TofGhitGdigiWrapper->TableData();

  // Extract the data from the dTofGhitRaw
  dTofGhitRawWrapper *TofGhitRawWrapper;
  outNode = evaNode;
  n = iii.findFirst("PHIODataNode", "dTofGhitRaw");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofGhitRaw not found" << endl;
     TofGhitRawWrapper = new dTofGhitRawWrapper("dTofGhitRaw", 300);
     if (!TofGhitRawWrapper) {
       return 1;
     }
     n = new TableNode_t(TofGhitRawWrapper,"dTofGhitRaw");
     outNode->addNode(n);
  }
  nodes.append(n);
  jjj = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofGhitRaw"));
  if (!d) {
    cerr << "  Error  in dTofGeo"<< endl; return 1;
  } else {
    TofGhitRawWrapper = static_cast<dTofGhitRawWrapper*>(d->getData());
    if (!TofGhitRawWrapper) {cerr<<" Error in dTofGhitRaw"<< endl; return 1;}
  }
  delete jjj;
  DTOFGHITRAW_ST *dTofGhitRaw = TofGhitRawWrapper->TableData();

  // Extract the data from the dTofRawRec
  dTofRawRecWrapper *TofRawRecWrapper;
  outNode = dstNode;
  n = iii.findFirst("PHIODataNode", "dTofRawRec");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofRawRec not found" << endl;
     TofRawRecWrapper = new dTofRawRecWrapper("dTofRawRec", 300);
     if (!TofRawRecWrapper) {
       return 1;
     }
     n = new TableNode_t(TofRawRecWrapper,"dTofRawRec");
     outNode->addNode(n);
  }
  nodes.append(n);
  jjj = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofRawRec"));
  if (!d) {
    cerr << "  Error  in dTofGeo"<< endl; return 1;
  } else {
    TofRawRecWrapper = static_cast<dTofRawRecWrapper*>(d->getData());
    if (!TofRawRecWrapper) {cerr<<" Error in dTofRawRec"<< endl; return 1;}
  }
  delete jjj;
  //DTOFRAWREC_ST *dTofRawRec = TofRawRecWrapper->TableData();

  // Extract the data from the dTofRaw
  dTofRawWrapper *TofRawWrapper;
  outNode = dstNode;
  n = iii.findFirst("PHIODataNode", "dTofRaw");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofRaw not found" << endl;
     TofRawWrapper = new dTofRawWrapper("dTofRaw", 300);
     if (!TofRawWrapper) {
       return 1;
     }
     n = new TableNode_t(TofRawWrapper,"dTofRaw");
     outNode->addNode(n);
  }
  nodes.append(n);
  jjj = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofRaw"));
  if (!d) {
    cerr << "  Error  in dTofGeo"<< endl; return 1;
  } else {
    TofRawWrapper = static_cast<dTofRawWrapper*>(d->getData());
    if (!TofRawWrapper) {cerr<<" Error in dTofRaw"<< endl; return 1;}
  }
  delete jjj;
  //DTOFRAW_ST *dTofRaw = TofRawWrapper->TableData();

  // Extract the data from the dTofReconstructed
  dTofReconstructedWrapper *TofReconstructedWrapper;
  outNode = dstNode;
  n = iii.findFirst("PHIODataNode", "dTofReconstructed");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofReconstructed not found" << endl;
     TofReconstructedWrapper = new dTofReconstructedWrapper("dTofReconstructed", 300);
     if (!TofReconstructedWrapper) {
       return 1;
     }
     n = new TableNode_t(TofReconstructedWrapper,"dTofReconstructed");
     outNode->addNode(n);
  }
  nodes.append(n);
  jjj = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofReconstructed"));
  if (!d) {
    cerr << "  Error  in dTofGeo"<< endl; return 1;
  } else {
    TofReconstructedWrapper = static_cast<dTofReconstructedWrapper*>(d->getData());
    if (!TofReconstructedWrapper) {cerr<<" Error in dTofReconstructed"<< endl; return 1;}
  }
  delete jjj;
  DTOFRECONSTRUCTED_ST *dTofReconstructed = TofReconstructedWrapper->TableData();

  // Extract the data from the dTofGdigiRec
  dTofGdigiRecWrapper *TofGdigiRecWrapper;
  outNode = tofNode;
  jjj = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofGdigiRec")))) {
     TofGdigiRecWrapper = new dTofGdigiRecWrapper("dTofGdigiRec", 960);
     if (!TofGdigiRecWrapper) {
       return 1;
     }
     d = new TableNode_t(TofGdigiRecWrapper,"dTofGdigiRec");
     outNode->addNode(d);
  } else {
    TofGdigiRecWrapper = static_cast<dTofGdigiRecWrapper*>(d->getData());
    if (!TofGdigiRecWrapper){ cerr<<" Error"<< endl; exit(1);}
    TofGdigiRecWrapper->SetMaxRowCount(960);
  }
  delete jjj;
  nodes.append(d);
  DTOFGDIGIREC_ST *dTofGdigiRec = TofGdigiRecWrapper->TableData();

  //=======================================================================
  // Convert from mTofEvaluate.c
  //=======================================================================

  int ngdigirec = 0;
  short slatid = 0;
  short recid = 0;
  short id, ghitid, gdigiid; // rawid,

  for(unsigned i = 0; i < TofGhitGdigiWrapper->RowCount(); i++){
    ghitid  = dTofGhitGdigi[i].ghitid;
    gdigiid = dTofGhitGdigi[i].gdigiid;

    for(unsigned j = 0; j < TofGhitRawWrapper->RowCount(); j++){
      if(ghitid == dTofGhitRaw[j].ghitid){
//	rawid = dTofGhitRaw[j].rawid;
	slatid = dTofGhitRaw[j].slatid;
	break;
      }
    }
    // gdigiid <=> ghitid <=> rawid,slatid <=> recid

    id = -1;
    for(int j = 0; j < ngdigirec; j++){
      if(gdigiid == dTofGdigiRec[j].gdigiid && 
	 slatid  == dTofGdigiRec[j].slatid){
	id = j;
	break;
      }
    }
    if(id < 0){ 
      for(unsigned j = 0; j < TofReconstructedWrapper->RowCount(); j++){
	if(slatid ==  dTofReconstructed[j].slatid){
	  recid  = dTofReconstructed[j].id;
	  break;
	}
      }
      dTofGdigiRec[ngdigirec].gdigiid = gdigiid;
      dTofGdigiRec[ngdigirec].slatid  = slatid;
      dTofGdigiRec[ngdigirec].recid   = recid;
      ngdigirec++;
    }
  }

  TofGdigiRecWrapper->SetRowCount(ngdigirec);

  return True;
}


