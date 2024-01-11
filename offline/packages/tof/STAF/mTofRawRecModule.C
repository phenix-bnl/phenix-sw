/****************************************************************************
 *         Convert from mTofRawRec.c to mTofRawRecModule.C
 *                    [ STAF wrapper => pure PHOOL module ]
 **************************************************************************** 
 *:>--------------------------------------------------------------------
 *: FILE:       mTofRawRec.c
 *: AUTHOR:     Akio Kiyomichi (Univ. of Tsukuba)
 *: HISTORY:    11/25/97  First Version
 *:              5/24/98 A.Kiyomichi  Add parameter table dTofRawRecPar 
 *:              5/26/98 A.Kiyomichi  Change PMT ID (Lower=0, Upper=1)
 *:              5/29/98 A.Kiyomichi  Add slewing effect correction
 *:              6/21/98 A.Kiyomichi  Fixed position calc.
 *:              5/08/00 A.Kiyomichi  change dTofReconstructed members
 *:              6/09/00 A.kiyomichi  create pure PHOOL module
 *:              7/02/00 A.Kiyomichi  Add raw data
 *:              7/18/00 A.Kiyomichi  Add TofEvent->RawToDst()
 *:              7/20/00 A.Kiyomichi  dTofRaw move to dstNode
 *:             12/21/00 A.Kiyomichi  set evaNode (dTofRawRec)
 *:<------------------------------------------------------------------*/

#include "Tof.hh"
#include "TofEvent.hh"
#include "mTofRawRecModule.h"
#include "dTofRawRecParWrapper.h"
#include "dTofGeoWrapper.h"
#include "dTofCalWrapper.h"
#include "dTofRawWrapper.h"
#include "dTofReconstructedWrapper.h"
#include "dTofRawRecWrapper.h"
#include "TofAddressObject.hh"
#include "TofGeometryObject.hh"
#include "TofCalibObject.hh"

#include "phool.h"
#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"
#include "PHPointerList.h"

#include "gsl/gsl_math.h"

#include <iostream>
#include <cmath>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

// forward declarations of PRIVATE functions
static void conv_rec (DTOFRECONSTRUCTED_ST    *rec ,
		      DTOFRAW_ST              *raw ,
		      DTOFGEO_ST              *geo ,
		      DTOFCAL_ST              *cal );

// Default constructor and destructor to pacify CINT
mTofRawRecModule::mTofRawRecModule(){
  chargecut  = 0.0;
  tvcpedecut = 10.0;
}

PHBoolean 
mTofRawRecModule::setCutParameter(float charge, float tvcpede){
  chargecut  = charge;
  tvcpedecut = tvcpede;
  return True;
}

PHBoolean 
mTofRawRecModule::event(PHCompositeNode    *root, 
			TofAddressObject   *address, 
			TofGeometryObject  *geometry,
			TofCalibObject     *calib,
			float charge, float tvcpede) {

  mTofRawRecModule::setCutParameter(charge,tvcpede);
  mTofRawRecModule::event(root, address, geometry, calib);
  return True;
}

PHBoolean 
mTofRawRecModule::event(PHCompositeNode    *root, 
			TofAddressObject   *address, 
			TofGeometryObject  *geometry,
			TofCalibObject     *calib) {
  TofEvent tofevent;
  tofevent.setCutParameter(chargecut, tvcpedecut);
  tofevent.RawToDst(root, address, geometry, calib);
  return True;
}

PHBoolean
mTofRawRecModule::event(PHCompositeNode *root) {
  PHPointerList<PHNode> nodes;
  PHNodeIterator iii(root), *jjj;
  PHNode *n;
  TableNode_t *d;
  //PHTable *w;
  PHCompositeNode *parNode, *tofNode, *dstNode, *evaNode, *outNode;

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

  evaNode = static_cast<PHCompositeNode*>(iii.findFirst("PHCompositeNode", "EVA"));
  if (!evaNode) {
    evaNode = new PHCompositeNode("EVA");
    root->addNode(evaNode);
  }

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  // Extract the data from the dTofRawRecPar
  dTofRawRecParWrapper *TofRawRecParWrapper;
  outNode = parNode;
  n = iii.findFirst("PHIODataNode", "dTofRawRecPar");
  if (!n) {
    cout << "ERROR:  'inout' dTofRawRecParameter dTofRawRecPar not found" << endl;
     TofRawRecParWrapper = new dTofRawRecParWrapper("dTofRawRecPar", 1);
     if (!TofRawRecParWrapper) {
       return 1;
     }
     n = new TableNode_t(TofRawRecParWrapper,"dTofRawRecPar");
     outNode->addNode(n);
  }
  nodes.append(n);
  jjj = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofRawRecPar"));
  if (!d) {
    cerr << "  Error "<< endl; return 1;
  } else {
    TofRawRecParWrapper = static_cast<dTofRawRecParWrapper*>(d->getData());
    if (!TofRawRecParWrapper) {cerr<<" Error"<< endl; return 1;}
  }
  delete jjj;
  DTOFRAWRECPAR_ST *par = TofRawRecParWrapper->TableData();

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
    cerr << "  Error "<< endl; return 1;
  } else {
    TofGeoWrapper = static_cast<dTofGeoWrapper*>(d->getData());
    if (!TofGeoWrapper) {cerr<<" Error"<< endl; return 1;}
  }
  delete jjj;
  DTOFGEO_ST *geo = TofGeoWrapper->TableData();

  // Extract the data from the dTofCal
  dTofCalWrapper *TofCalWrapper;
  outNode = parNode;
  n = iii.findFirst("PHIODataNode", "dTofCal");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofCal not found" << endl;
     TofCalWrapper = new dTofCalWrapper("dTofCal", 1500);
     if (!TofCalWrapper) {
       return 1;
     }
     n = new TableNode_t(TofCalWrapper,"dTofCal");
     outNode->addNode(n);
  }
  nodes.append(n);
  jjj = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofCal"));
  if (!d) {
    cerr << "  Error "<< endl; return 1;
  } else {
    TofCalWrapper = static_cast<dTofCalWrapper*>(d->getData());
    if (!TofCalWrapper) {cerr<<" Error"<< endl; return 1;}
  }
  delete jjj;
  DTOFCAL_ST *cal = TofCalWrapper->TableData();

  // Extract the data from the dTofRaw
  dTofRawWrapper *TofRawWrapper;
  outNode = dstNode;
  n = iii.findFirst("PHIODataNode", "dTofRaw");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofRaw not found" << endl;
     TofRawWrapper = new dTofRawWrapper("dTofRaw", 960);
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
    cerr << "  Error "<< endl; return 1;
  } else {
    TofRawWrapper = static_cast<dTofRawWrapper*>(d->getData());
    if (!TofRawWrapper) {cerr<<" Error"<< endl; return 1;}
  }
  delete jjj;
  DTOFRAW_ST *raw = TofRawWrapper->TableData();

  // Extract the data from the dTofReconstructed
  dTofReconstructedWrapper *TofRecWrapper;
  outNode = dstNode;
  jjj = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofReconstructed")))) {
     TofRecWrapper = new dTofReconstructedWrapper("dTofReconstructed", 960);
     if (!TofRecWrapper) {
       return 1;
     }
     d = new TableNode_t(TofRecWrapper,"dTofReconstructed");
     outNode->addNode(d);
  } else {
    TofRecWrapper = static_cast<dTofReconstructedWrapper*>(d->getData());
    if (!TofRecWrapper){ cerr<<" Error"<< endl; exit(1);}
    TofRecWrapper->SetMaxRowCount(960);
  }
  delete jjj;
  nodes.append(d);
  DTOFRECONSTRUCTED_ST *rec = TofRecWrapper->TableData();


  // Extract the data from the dTofRawRec
  dTofRawRecWrapper *TofRawRecWrapper;
  outNode = dstNode;
  jjj = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofRawRec")))) {
    TofRawRecWrapper = new dTofRawRecWrapper("dTofRawRec", 960);
     if (!TofRawRecWrapper) {
       return 1;
     }
     d = new TableNode_t(TofRawRecWrapper,"dTofRawRec");
     outNode->addNode(d);
  } else {
    TofRawRecWrapper = static_cast<dTofRawRecWrapper*>(d->getData());
    if (!TofRawRecWrapper){ cerr<<" Error"<< endl; exit(1);}
    TofRawRecWrapper->SetMaxRowCount(960);
  }
  delete jjj;
  nodes.append(d);
  DTOFRAWREC_ST *rawrec = TofRawRecWrapper->TableData();

  //=======================================================================
  // Convert from mTofRawRec.c
  //=======================================================================
  //short  i, j, k;
  short  i, j;
  short  nraw_total = TofRawWrapper->RowCount();
  short  rawid;
  short  slatid;
  short  irec = 0;

  if (TofRawWrapper->RowCount() > TofRawRecWrapper->MaxRowCount()) {
    cout<<"TofRawWrapper->RowCount() "         <<TofRawWrapper->RowCount()
        <<" > TofRawRecWrapper->MaxRowCount() "<<TofRawRecWrapper->MaxRowCount()<<endl;
    return False;
  }

  for(i=0; i<nraw_total; i++){
    rawid     = raw[i].id;
    slatid    = raw[i].slatid;
    j = raw[i].slatid;

    if((raw[i].qvc[0] > 0)&&(raw[i].qvc[0] > 0)&&
       (raw[i].tvc[0] > 0)&&(raw[i].tvc[0] < cal[j].tvc_ped[0])&&
       (raw[i].tvc[1] > 0)&&(raw[i].tvc[1] < cal[j].tvc_ped[0])){

      rawrec[irec].rawid    = rawid;
      rawrec[irec].slatid   = slatid;
      rawrec[irec].recid    = irec;

      rec[irec].qvc[0]      = raw[i].qvc[0];
      rec[irec].qvc[1]      = raw[i].qvc[1];
      rec[irec].tvc[0]      = raw[i].tvc[0];
      rec[irec].tvc[1]      = raw[i].tvc[1];
      /* Convert dTofRaw (qvc tvc) to dTofReconstructed */
      rec[irec].id          = irec;
      rec[irec].slatid      = j;
      conv_rec(rec+irec, raw+i, geo+j, cal+j);
      irec++;
    }
  } /* for(i=0; i<nraw_total; i++){ */

  TofRawRecWrapper->SetRowCount(irec);
  TofRecWrapper->SetRowCount(irec);
  if (par[0].verbose){
    cout<<"Number of rows in dTofReconstructed = "<<nraw_total<<endl;
  }

  return True;
}

/* PRIVATE functions (implementation) */
static void conv_rec (DTOFRECONSTRUCTED_ST    *rec ,
		      DTOFRAW_ST              *raw ,
		      DTOFGEO_ST              *geo ,
		      DTOFCAL_ST              *cal )
{
  /* Convert dTofRaw (qvc tvc) to dTofReconstructed */
  /*--------------------------------------------------------*
   *     IN:      raw  - raw data of TOF                    *
   *              geo  - geometry table of TOF              *
   *              cal  - calibration constant of TOF        *
   *    OUT:      rec  - End-product data of TOF            *
   *--------------------------------------------------------*/
  float  pos[3];       /* TOF slat position */
  float  r;            /* TOF slat   r-position */
  float  phi;          /* TOF slat phi-position */

  float  tof;          /* Time-of-flight */
  float  eloss;        /* Energy loss */
  float  xtof[3];      /* TOF hit position */

  float  slat_halflength;
  float  fqvc[2];
  float  ftvc[2];
  float  tofslew;
  float  sqrtqvc; 
  float  ftof; 
  float  ptof; 
  float  ypos;
  //float  rpos;
  //float  phipos;
  
  pos[0] = geo->pos[0];
  pos[1] = geo->pos[1];
  pos[2] = geo->pos[2];
  r      = geo->r;
  phi    = geo->phi;

  if((geo->slat<=15)||(geo->slat>=80)){
    slat_halflength = TOF_SSLAT_LENGTH/2.;
  } else {
    slat_halflength = TOF_LSLAT_LENGTH/2.;
  }

  /* eloss calc. */
  fqvc[0] = raw->qvc[0]*cal->qvc_corr[0]*cal->qvc_corrlsr[0];
  fqvc[1] = raw->qvc[1]*cal->qvc_corr[1]*cal->qvc_corrlsr[1];
  sqrtqvc = sqrt(fqvc[0]*fqvc[1])/exp(-slat_halflength/cal->scint_attenu);
  eloss   = sqrtqvc*cal->eloss_mip/cal->eloss_conv ;

  /* tof calc. */
  ftvc[0] = raw->tvc[0] - cal->t0[0];
  ftvc[1] = raw->tvc[1] - cal->t0[1];

  /* slewing effect [ps] */
  tofslew = cal->slew_a[0] + cal->slew_a[1] + 
    (cal->slew_b[0]/sqrt((float)raw->qvc[0]) + 
     cal->slew_b[1]/sqrt((float)raw->qvc[1]))/2;

  ftof = (ftvc[0]*cal->tvc_conv[0]/1.e+3 + ftvc[1]*cal->tvc_conv[1]/1.e+3)/2;
  tof  = ftof - slat_halflength/cal->scint_vlight - tofslew*1.e-3;
  if(tof < 0.) tof = 0.;

  /* position calc. */
  /* TOF panels are in east arm (168.75<phi<213.75) */
  ptof = (ftvc[0]*cal->tvc_conv[0]/1.e+3 - ftvc[1]*cal->tvc_conv[1]/1.e+3)/2;
  ypos = ptof*cal->scint_vlight;
  if(geo->sector == 0){
    xtof[0] = pos[0] - ypos*sin(22.5 * M_PI / 180.0);
    xtof[1] = pos[1] + ypos*cos(22.5 * M_PI / 180.0);
  } else {
    xtof[0] = pos[0];
    xtof[1] = pos[1] + ypos;
  }
  xtof[2] = pos[2];

  rec->sector      = geo->sector;
  rec->side        = geo->side;
  rec->panel       = geo->panel;
  rec->slat        = geo->slat;
  rec->tof         = tof;
  rec->tof_err     = 0.;  /* fill later */
  rec->eloss       = eloss;
  rec->eloss_err   = 0.;  /* fill later */
  rec->xtof[0]     = xtof[0];
  rec->xtof[1]     = xtof[1];
  rec->xtof[2]     = xtof[2];
  rec->xtof_err[0] = 0.;  /* fill later */
  rec->xtof_err[1] = 0.;
  rec->xtof_err[2] = 0.;
}
