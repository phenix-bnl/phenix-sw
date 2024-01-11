/****************************************************************************
 *       Convert from mTofGdigiRec.c to mTofGdigiRecModule.C
 *                    [ STAF wrapper => pure PHOOL module ]
 **************************************************************************** 
 *:>--------------------------------------------------------------------
 *: FILE:       mTofGdigiRec.c.template
 *: AUTHOR:     Akio Kiyomichi (Univ. of Tsukuba)
 *: HISTORY:    11/25/97  First Version
 *:             04/10/98 A.Kiyomichi  Add perfpar table
 *:              5/08/00 A.Kiyomichi  change dTofReconstructed members
 *:              6/11/00 A.Kiyomichi create pure PHOOL module
 *:<------------------------------------------------------------------*/

#include "Tof.hh"
#include "mTofGdigiRecModule.h"
#include "dTofPerfParWrapper.h"
#include "dTofGeoWrapper.h"
#include "dTofGdigiWrapper.h"
#include "dTofReconstructedWrapper.h"
#include "dTofGdigiRecWrapper.h"

#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include <cstdlib>
#include <iostream>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

PHBoolean
mTofGdigiRecModule::event(PHCompositeNode *root) {
  PHPointerList<PHNode> nodes;
  PHNodeIterator iii(root), *jjj;
  PHNode *n;
  TableNode_t *d;
  //PHTable *w;
  PHCompositeNode *parNode, *tofNode, *dstNode, *outNode;

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

  dstNode = static_cast<PHCompositeNode*>(iii.findFirst("PHCompositeNode", "DST"));
  if (!dstNode) {
    dstNode = new PHCompositeNode("DST");
    root->addNode(dstNode);
  }

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  // Extract the data from the dTofPerfPar
  dTofPerfParWrapper *TofPerfParWrapper;
  outNode = parNode;
  n = iii.findFirst("PHIODataNode", "dTofPerfPar");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofPerfPar not found" << endl;
     TofPerfParWrapper = new dTofPerfParWrapper("dTofPerfPar", 1);
     if (!TofPerfParWrapper) {
       return 1;
     }
     n = new TableNode_t(TofPerfParWrapper,"dTofPerfPar");
     outNode->addNode(n);
  }
  nodes.append(n);
  jjj = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofPerfPar"));
  if (!d) {
    cerr << "  Error  in dTofPerfPar"<< endl; return 1;
  } else {
    TofPerfParWrapper = static_cast<dTofPerfParWrapper*>(d->getData());
    if (!TofPerfParWrapper) {cerr<<" Error in dTofPerfPar"<< endl; return 1;}
  }
  delete jjj;
  //DTOFPERFPAR_ST *perfpar = TofPerfParWrapper->TableData();

  // Extract the data from the dTofGeo
  dTofGeoWrapper *TofGeoWrapper;
  outNode = parNode;
  n = iii.findFirst("PHIODataNode", "dTofGeo");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofGeo not found" << endl;
     TofGeoWrapper = new dTofGeoWrapper("dTofGeo", 1500);
     if (!TofGeoWrapper) {
       return 1;
     }
     n = new TableNode_t(TofGeoWrapper,"dTofGeo");
     outNode->addNode(n);
  }
  nodes.append(n);
  jjj = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofGeo"));
  if (!d) {
    cerr << "  Error  in dTofGeo"<< endl; return 1;
  } else {
    TofGeoWrapper = static_cast<dTofGeoWrapper*>(d->getData());
    if (!TofGeoWrapper) {cerr<<" Error in dTofGeo"<< endl; return 1;}
  }
  delete jjj;
  DTOFGEO_ST *geo = TofGeoWrapper->TableData();

  // Extract the data from the dTofGdigi
  dTofGdigiWrapper *TofGdigiWrapper;
  outNode = tofNode;
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
    cerr << "  Error  in dTofGeo"<< endl; return 1;
  } else {
    TofGdigiWrapper = static_cast<dTofGdigiWrapper*>(d->getData());
    if (!TofGdigiWrapper) {cerr<<" Error in dTofGdigi"<< endl; return 1;}
  }
  delete jjj;
  DTOFGDIGI_ST *digi = TofGdigiWrapper->TableData();

  // Extract the data from the dTofReconstructed
  dTofReconstructedWrapper *TofRecWrapper;
  outNode = dstNode;
  jjj = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofReconstructed")))) {
     TofRecWrapper = new dTofReconstructedWrapper("dTofReconstructed", 300);
     if (!TofRecWrapper) {
       return 1;
     }
     d = new TableNode_t(TofRecWrapper,"dTofReconstructed");
     outNode->addNode(d);
  } else {
    TofRecWrapper = static_cast<dTofReconstructedWrapper*>(d->getData());
    if (!TofRecWrapper){ cerr<<" Error"<< endl; exit(1);}
    TofRecWrapper->SetMaxRowCount(300);
  }
  delete jjj;
  nodes.append(d);
  DTOFRECONSTRUCTED_ST *rec = TofRecWrapper->TableData();

  // Extract the data from the dTofGdigiRec
  dTofGdigiRecWrapper *TofGdigiRecWrapper;
  outNode = parNode;
  jjj = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofGdigiRec")))) {
     TofGdigiRecWrapper = new dTofGdigiRecWrapper("dTofGdigiRec", 300);
     if (!TofGdigiRecWrapper) {
       return 1;
     }
     d = new TableNode_t(TofGdigiRecWrapper,"dTofGdigiRec");
     outNode->addNode(d);
  } else {
    TofGdigiRecWrapper = static_cast<dTofGdigiRecWrapper*>(d->getData());
    if (!TofGdigiRecWrapper){ cerr<<" Error"<< endl; exit(1);}
    TofGdigiRecWrapper->SetMaxRowCount(300);
  }
  delete jjj;
  nodes.append(d);
  DTOFGDIGIREC_ST *gdigirec = TofGdigiRecWrapper->TableData();

  //=======================================================================
  // Convert from mTofGdigiRec.c
  //=======================================================================
 
  short  i;
  short  ndigi_total = TofGdigiWrapper->RowCount();;
  short  gdigiid;
  short  slatid;

  if (TofGdigiWrapper->RowCount() > TofGdigiRecWrapper->MaxRowCount()) {
    cout << "TofGdigiWrapper->RowCount() " << TofGdigiWrapper->RowCount()
	 << " > TofGdigiRecWrapper->MaxRowCount() " << TofGdigiRecWrapper->MaxRowCount()
	 << endl;
    return False;
  }

  if (TofGdigiWrapper->RowCount() > TofRecWrapper->MaxRowCount()) {
    cout << "TofGdigiWrapper->RowCount() " << TofGdigiWrapper->RowCount()
	 << " > TofRecWrapper->MaxRowCount() " << TofRecWrapper->MaxRowCount()
	 << endl;
    return False;
  }

  for(i=0; i<ndigi_total; i++){
    gdigiid   = digi[i].id;
    slatid    = digi[i].slatid;
    gdigirec[i].gdigiid  = gdigiid;
    gdigirec[i].slatid   = slatid;
    gdigirec[i].recid    = i;

    rec[i].id          = i;
    rec[i].slatid      = slatid;
    rec[i].sector      = geo[slatid].sector;
    rec[i].side        = geo[slatid].side;
    rec[i].panel       = geo[slatid].panel;
    rec[i].slat        = geo[slatid].slat;
    rec[i].tof         = digi[i].tof;
    rec[i].tof_err     = 0.;
    rec[i].eloss       = digi[i].eloss;
    rec[i].eloss_err   = 0.;
    rec[i].xtof[0]     = digi[i].pos_m[0];
    rec[i].xtof[1]     = digi[i].pos_m[1];
    rec[i].xtof[2]     = digi[i].pos_m[2];
    rec[i].xtof_err[0] = 0.;
    rec[i].xtof_err[1] = 0.;
    rec[i].xtof_err[2] = 0.;
  } /* for(i=0; i<ndigi_total; i++){ */

  TofGdigiRecWrapper->SetRowCount(ndigi_total);
  TofRecWrapper->SetRowCount(ndigi_total);
#ifdef DEBUG
    printf("Number of rows in dTofReconstructed = %d\n",ndigi_total);
#endif

  return True;
}
