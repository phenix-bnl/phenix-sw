/****************************************************************************
 *        Convert from mTofGhitRaw.c to mTofGhitRawModule.C
 *                    [ STAF wrapper => pure PHOOL module ]
 **************************************************************************** 
 *:>--------------------------------------------------------------------
 *: FILE:       mTofGhitRaw.c
 *: AUTHOR:     Akio Kiyomichi (Univ. of Tsukuba)
 *: HISTORY:    11/16/97  First Version
 *:             12/31/97 A.Kiyomichi  Change from itrack to mctrack 
 *:              5/24/98 A.Kiyomichi  Add parameter table dTofGhitRawPar 
 *:              5/26/98 A.Kiyomichi  Change PMT ID (Lower=0, Upper=1)
 *:              5/26/98 A.Kiyomichi  Add time res. and slewing effect
 *:              6/10/00 A.Kiyomichi  create pure PHOOL module
 *:              6/13/00 A.Kiyomichi  Add q1 q2 t3 t4 in dtofRaw
 *:              7/20/00 A.Kiyomichi  dTofRaw move to dstNode
 *:             11/29/01 A.Kiyomichi  Add TofEvent->GeaToRaw
 *:<------------------------------------------------------------------*/

#include "utiPrototype.hh"
#include "Tof.hh"
#include "TofEvent.hh"
#include "mTofGhitRawModule.h"
#include "tofghitWrapper.h"
#include "dTofGhitRawParWrapper.h"
#include "dTofGeoWrapper.h"
#include "dTofUcalWrapper.h"
#include "dTofRawWrapper.h"
#include "dTofGhitRawWrapper.h"

#include "TofAddressObject.hh"
#include "TofGeometryObject.hh"
#include "TofCalibObject.hh"

#include "phool.h"
#include "PHPointerList.h"
#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include <cmath>
#include <iostream>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

/* forward declarations of static functions */
static void add_hit  (TOFGHIT_ST             *ghit ,
		      DTOFUCAL_ST            *ucal ,
		      float                *fqvc_l ,
		      float                *fqvc_u ,
		      float                *ftvc_l ,
		      float                *ftvc_u ,
		      short                  rawid );

static void conv_raw (DTOFRAW_ST              *raw ,
		      DTOFGHITRAWPAR_ST       *par ,
		      DTOFGEO_ST              *geo ,
		      DTOFUCAL_ST            *ucal ,
		      float                *fqvc_l ,
		      float                *fqvc_u ,
		      float                *ftvc_l ,
		      float                *ftvc_u );

/* forward declarations of TOF functions */
extern "C" short tofghit_slatid(TOFGHIT_ST             *ghit );

// Default constructor and destructor to pacify CINT
mTofGhitRawModule::mTofGhitRawModule(){
  timingsigma = 0.080;  //[ns]
  attenuation = 128.0;  //[cm]
}

PHBoolean mTofGhitRawModule::setTimingResolution(float sigma){
  timingsigma = sigma;
  return True;
}
PHBoolean mTofGhitRawModule::setAttenuationLength(float atten){
  attenuation = atten;
  return True;
}

PHBoolean
mTofGhitRawModule::event(PHCompositeNode   *root, 
			 TofAddressObject  *address, 
			 TofGeometryObject *geometry,
			 TofCalibObject    *calib) {
  TofEvent tofevent;
  tofevent.setDebugLevel(iDebug);
  tofevent.setTimingResolution(timingsigma);
  tofevent.setAttenuationLength(attenuation);
  tofevent.GeaToRaw(root, address, geometry, calib);
  return True;
}

PHBoolean
mTofGhitRawModule::event(PHCompositeNode *root) {
  PHPointerList<PHNode> nodes;
  PHNodeIterator iii(root), *jjj;
  PHNode *n;
  TableNode_t *d;
  PHCompositeNode *parNode, *tofNode, *dstNode, *geaNode, *evaNode, *outNode;

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
  
  geaNode = static_cast<PHCompositeNode*>(iii.findFirst("PHCompositeNode", "GEA"));
  if (!geaNode) {
    geaNode = new PHCompositeNode("GEA");
    root->addNode(geaNode);
  }

  evaNode = static_cast<PHCompositeNode*>(iii.findFirst("PHCompositeNode", "EVA"));
  if (!evaNode) {
    evaNode = new PHCompositeNode("EVA");
    root->addNode(evaNode);
  }

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  // Extract the data from the tofghit
  tofghitWrapper *tofghitWrap;
  outNode = geaNode;
  n = iii.findFirst("PHIODataNode", "tofghit");
  if (!n) {
    cout << "ERROR:  'in' parameter tofghit not found" << endl;
     tofghitWrap = new tofghitWrapper("tofghit", 3000);
     if (!tofghitWrap) {
       return 1;
     }
     n = new TableNode_t(tofghitWrap,"tofghit");
     outNode->addNode(n);
  }
  nodes.append(n);
  jjj = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","tofghit"));
  if (!d) {
    cerr << "  Error "<< endl; return 1;
  } else {
    tofghitWrap = static_cast<tofghitWrapper*>(d->getData());
    if (!tofghitWrap) {cerr<<" Error"<< endl; return 1;}
  }
  delete jjj;
  TOFGHIT_ST *ghit = tofghitWrap->TableData();

  // Extract the data from the dTofGhitRawPar
  dTofGhitRawParWrapper *TofGhitRawParWrapper;
  outNode = parNode;
  n = iii.findFirst("PHIODataNode", "dTofGhitRawPar");
  if (!n) {
    cout << "ERROR:  'inout' parameter dTofGhitRawPar not found" << endl;
     TofGhitRawParWrapper = new dTofGhitRawParWrapper("dTofGhitRawPar", 10);
     if (!TofGhitRawParWrapper) {
       return 1;
     }
     n = new TableNode_t(TofGhitRawParWrapper,"dTofGhitRawPar");
     outNode->addNode(n);
  }
  nodes.append(n);
  jjj = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofGhitRawPar"));
  if (!d) {
    cerr << "  Error "<< endl; return 1;
  } else {
    TofGhitRawParWrapper = static_cast<dTofGhitRawParWrapper*>(d->getData());
    if (!TofGhitRawParWrapper) {cerr<<" Error"<< endl; return 1;}
  }
  delete jjj;
  DTOFGHITRAWPAR_ST *par = TofGhitRawParWrapper->TableData();

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

  // Extract the data from the dTofUcal
  dTofUcalWrapper *TofUcalWrapper;
  outNode = parNode;
  n = iii.findFirst("PHIODataNode", "dTofUcal");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofUcal not found" << endl;
     TofUcalWrapper = new dTofUcalWrapper("dTofUcal", 1100);
     if (!TofUcalWrapper) {
       return 1;
     }
     n = new TableNode_t(TofUcalWrapper,"dTofUcal");
     outNode->addNode(n);
  }
  nodes.append(n);
  jjj = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofUcal"));
  if (!d) {
    cerr << "  Error "<< endl; return 1;
  } else {
    TofUcalWrapper = static_cast<dTofUcalWrapper*>(d->getData());
    if (!TofUcalWrapper) {cerr<<" Error"<< endl; return 1;}
  }
  delete jjj;
  DTOFUCAL_ST *ucal = TofUcalWrapper->TableData();

  // Extract the data from the dTofRaw
  dTofRawWrapper *TofRawWrapper;
  outNode = dstNode;
  jjj = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofRaw")))) {
     TofRawWrapper = new dTofRawWrapper("dTofRaw", 1100);
     if (!TofRawWrapper) {
       return 1;
     }
     d = new TableNode_t(TofRawWrapper,"dTofRaw");
     outNode->addNode(d);
  } else {
    TofRawWrapper = static_cast<dTofRawWrapper*>(d->getData());
    if (!TofRawWrapper){ cerr<<" Error"<< endl; exit(1);}
    TofRawWrapper->SetMaxRowCount(1100);
  }
  delete jjj;
  nodes.append(d);
  DTOFRAW_ST *raw = TofRawWrapper->TableData();

  // Extract the data from the dTofGhitRaw
  dTofGhitRawWrapper *TofGhitRawWrapper;
  outNode = evaNode;
  jjj = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofGhitRaw")))) {
     TofGhitRawWrapper = new dTofGhitRawWrapper("dTofGhitRaw", 3000);
     if (!TofGhitRawWrapper) {
       return 1;
     }
     d = new TableNode_t(TofGhitRawWrapper,"dTofGhitRaw");
     outNode->addNode(d);
  } else {
    TofGhitRawWrapper = static_cast<dTofGhitRawWrapper*>(d->getData());
    if (!TofGhitRawWrapper){ cerr<<" Error"<< endl; exit(1);}
    TofGhitRawWrapper->SetMaxRowCount(1100);
  }
  delete jjj;
  nodes.append(d);
  DTOFGHITRAW_ST *ghitraw = TofGhitRawWrapper->TableData();

  //=======================================================================
  // Convert from mTofGhitRaw.c
  //=======================================================================

  short  i, j;
  short  nghit_total = tofghitWrap->RowCount();
  short  nraw = 0;
  short  id;
  short  rawid;
  short  slatid[TOF_NSLAT];
  float  fqvc_l[TOF_NSLAT];
  float  fqvc_u[TOF_NSLAT];
  float  ftvc_l[TOF_NSLAT];
  float  ftvc_u[TOF_NSLAT];
  short  partl;
  long   mctrack;

#ifdef DEBUG
    printf("Number of rows in           TOFGHIT = %d\n",nghit_total);
#endif

  for(i=0; i<TOF_NSLAT; i++){
    /* Initialize */
    slatid[i] = -1;
    fqvc_l[i] = 0.;
    fqvc_u[i] = 0.;
    ftvc_l[i] = 4096./1.e+3*ucal->tvc_conv[0];
    ftvc_u[i] = 4096./1.e+3*ucal->tvc_conv[1];
  }
  for(i=0; i<nghit_total; i++){
    partl          = ghit[i].partl;
    mctrack        = ghit[i].mctrack;

    if((partl <= 0)||(mctrack <= 0)){
      /* No data in tofghit */
      ghitraw[i].ghitid = i;
      ghitraw[i].slatid = -1;
      ghitraw[i].rawid  = -1;
    } else {
      id = -1;
      /* Search new hit */
      for(j = 0; j<nraw; j++){
	if(slatid[j] == tofghit_slatid(ghit+i)){
	  id = j;
	  break;
	}
      }
      /* Add dele and find fastest tof for each PMT */
      if(id >= 0){
	rawid = id;
      } else {
	rawid = nraw;
	slatid[nraw] = tofghit_slatid(ghit+i);
	nraw++;
      }
      j = slatid[rawid];
      add_hit(ghit+i, ucal+j, fqvc_l, fqvc_u, ftvc_l, ftvc_u, rawid );
      /* Relational table between tofghit and dTofRaw */
      ghitraw[i].ghitid = i;
      ghitraw[i].slatid = j;
      ghitraw[i].rawid  = rawid;
    } /* if((partl <= 0)||(mctrack <= 0)){ -- } else { */
  } /* for(i=0; i<nghit_total; i++){ */
  for(i=0; i<nraw; i++){
    /* Digitized qvc and tvc, and fill to dTofRaw */
    j = slatid[i];
    raw[i].id     = i;
    raw[i].slatid = j;
    conv_raw(raw+i, par, geo+j, ucal+j, 
	     fqvc_l+i, fqvc_u+i, ftvc_l+i, ftvc_u+i);
  }

  TofGhitRawWrapper->SetRowCount(nghit_total);
  TofRawWrapper->SetRowCount(nraw);

  if (TofGhitRawWrapper->RowCount() > TofGhitRawWrapper->MaxRowCount()){
    cout << "dTofGhitRawWrapper->RowCount() = " << TofGhitRawWrapper->RowCount()
	 << " dTofGhitRawWrapper->MaxRowCount() = " << TofGhitRawWrapper->MaxRowCount()
	 << endl;
    return False;
  }
  if (TofRawWrapper->RowCount() > TofRawWrapper->MaxRowCount()){
    cout << "dTofRawWrapper->RowCount() " << TofRawWrapper->RowCount()
	 << " dTofRawWrapper->MaxRowCount() = " <<  TofRawWrapper->MaxRowCount()
	 << endl;
    return False;
  }

  if (par[0].verbose){
    printf("Number of rows in         dTofRaw   = %d\n", nraw);
  }

  return True;
}

/* PRIVATE functions (implementation) */
static void add_hit  (TOFGHIT_ST             *ghit ,
		      DTOFUCAL_ST            *ucal ,
		      float                *fqvc_l ,
		      float                *fqvc_u ,
		      float                *ftvc_l ,
		      float                *ftvc_u ,
		      short                  rawid ){
  /* Add dele and find fastest tof for each PMT */
  /*--------------------------------------------------------*
   *     IN:     ghit  - GEANT hits of TOF                  *
   *             ucal  - un-calibration constant of TOF     *
   *            rawid  - pointer to dTofRaw.id              *
   *  INOUT:   fqvc_l  - Energy loss on Lower PMT [GeV]     *
   *           fqvc_u  - Energy loss on Upper PMT [GeV]     *
   *           ftvc_l  - Time of flight on lower PMT [ns]   *
   *           ftvc_u  - Time of flight on Upper PMT [ns]   *
   *    OUT:                                                *
   *--------------------------------------------------------*/
  float  slat_halflength;
  float  ypos;
  float  tof;
  float  dele;
  float  ftof[2];

  if(ghit->subvol == 1){
    slat_halflength = TOF_SSLAT_LENGTH/2.;
  } else {
    slat_halflength = TOF_LSLAT_LENGTH/2.;
  }
  if(ghit->column%2 == 0){
    ypos = - (ghit->pos_hit_slat);
  } else {
    ypos = + (ghit->pos_hit_slat);
  }
  dele = ghit->dele;
  fqvc_l[rawid] += dele*exp(-(slat_halflength + ypos)/ucal->scint_attenu);
  fqvc_u[rawid] += dele*exp(-(slat_halflength - ypos)/ucal->scint_attenu);

  tof  = ghit->tof;
  ftof[0] = tof + (slat_halflength + ypos)/ucal->scint_vlight;
  ftof[1] = tof + (slat_halflength - ypos)/ucal->scint_vlight;
  if(ftof[0] < ftvc_l[rawid]) ftvc_l[rawid] =  ftof[0];
  if(ftof[1] < ftvc_u[rawid]) ftvc_u[rawid] =  ftof[1];
}

static void conv_raw (DTOFRAW_ST              *raw ,
		      DTOFGHITRAWPAR_ST       *par ,
		      DTOFGEO_ST              *geo ,
		      DTOFUCAL_ST            *ucal ,
		      float                *fqvc_l ,
		      float                *fqvc_u ,
		      float                *ftvc_l ,
		      float                *ftvc_u ){
  /* Digitized qvc and tvc, and fill to dTofRaw */
  /*--------------------------------------------------------*
   *     IN:      geo  - geometry table of TOF              *
   *             ucal  - un-calibration constant of TOF     *
   *           fqvc_l  - Energy loss on Lower PMT [GeV]     *
   *           fqvc_u  - Energy loss on Upper PMT [GeV]     *
   *           ftvc_l  - Time of flight on lower PMT [ns]   *
   *           ftvc_u  - Time of flight on Upper PMT [ns]   *
   *  INOUT:      par  - parameter table (random no. seed)  *
   *    OUT:      raw  - raw data of TOF                    *
   *--------------------------------------------------------*/

  float mean, sigma;
  float qvcl = *fqvc_l;
  float qvcu = *fqvc_u;
  float tvcl = *ftvc_l;
  float tvcu = *ftvc_u;
  short qvc[2];
  short tvc[2];
  float slew[2];
  long  randseed;

  raw->sector   = geo->sector;
  raw->side     = geo->side;
  raw->panel    = geo->panel;
  raw->slat     = geo->slat;
  raw->cell[0]  = par[0].min_cell;
  raw->cell[1]  = par[0].min_cell;
  raw->qvc[0]   = par[0].min_qvc;
  raw->qvc[1]   = par[0].min_qvc;
  raw->q1[0]    = par[0].max_qvc;
  raw->q1[1]    = par[0].max_qvc;
  raw->q2[0]    = par[0].max_qvc;
  raw->q2[1]    = par[0].max_qvc;
  raw->tvc[0]   = par[0].max_tvc;
  raw->tvc[1]   = par[0].max_tvc;
  raw->t3[0]    = par[0].max_tvc;
  raw->t3[1]    = par[0].max_tvc;
  raw->t4[0]    = par[0].max_tvc;
  raw->t4[1]    = par[0].max_tvc;
  
  qvc[0] = (short)(qvcl*1.e+9/ucal->qvc_chgain[0]);
  qvc[1] = (short)(qvcu*1.e+9/ucal->qvc_chgain[1]);

  /* Add time resolution */
  sigma = ucal->tof_sigma*1.e-3*sqrt(2.);
  mean = tvcl;
  randseed = par[0].randseed;
  utiGaussian(&mean,&sigma,&randseed,&tvcl);
  mean = tvcu;
  utiGaussian(&mean,&sigma,&randseed,&tvcu);
  par[0].randseed = randseed;

  /* slewing effect [ps] */
  slew[0] = 2*ucal->slew_a[0] + ucal->slew_b[0]/sqrt((float)qvc[0]);
  slew[1] = 2*ucal->slew_a[1] + ucal->slew_b[1]/sqrt((float)qvc[1]);

  tvcl = tvcl + slew[0]*1.e-3;
  tvcu = tvcu + slew[1]*1.e-3;

  tvc[0] = (short)(tvcl*1.e+3/ucal->tvc_conv[0]);
  tvc[1] = (short)(tvcu*1.e+3/ucal->tvc_conv[1]);

  /* conversion to dTofRaw */
  if((par[0].min_qvc<=qvc[0])&&(qvc[0]<=par[0].max_qvc)){
    raw->qvc[0] = qvc[0];
  } else if(qvc[0]>par[0].max_qvc){
    raw->qvc[0] = par[0].max_qvc;
  }
  if((par[0].min_qvc<=qvc[1])&&(qvc[1]<=par[0].max_qvc)){
    raw->qvc[1] = qvc[1];
  } else if(qvc[1]>par[0].max_qvc){
    raw->qvc[1] = par[0].max_qvc;
  }

  if((par[0].min_tvc<=tvc[0])&&(tvc[0]<=ucal->tvc_ped[0])){
    raw->tvc[0] = tvc[0];
  } else if(tvc[0]>ucal->tvc_ped[0]){
    raw->tvc[0] = (short)ucal->tvc_ped[0];
  }
  if((par[0].min_tvc<=tvc[1])&&(tvc[1]<=ucal->tvc_ped[1])){
    raw->tvc[1] = tvc[1];
  } else if(tvc[1]>ucal->tvc_ped[0]){
    raw->tvc[1] = (short)ucal->tvc_ped[0];
  }
}

short tofghit_slatid(TOFGHIT_ST             *ghit ){
  /*--------------------------------------------------------*
   *      Conversion slat ID from PISA-TOF ghit data        *
   *           panel,column,pslat   -->   slatid            *
   *--------------------------------------------------------*/
  short slatid;
  short slat;
  if        ((ghit->column % 2 == 0)&&(ghit->pslat == 0)){
    slat =  0 + (ghit->column - 1)/2;
  } else if ((ghit->column % 2 == 1)&&(ghit->pslat == 2)){
    slat = 16 + (ghit->column - 1)/2;
  } else if ((ghit->column % 2 == 0)&&(ghit->pslat == 1)){
    slat = 32 + (ghit->column - 1)/2;
  } else if ((ghit->column % 2 == 1)&&(ghit->pslat == 1)){
    slat = 48 + (ghit->column - 1)/2;
  } else if ((ghit->column % 2 == 0)&&(ghit->pslat == 2)){
    slat = 64 + (ghit->column - 1)/2;
  } else if ((ghit->column % 2 == 1)&&(ghit->pslat == 0)){
    slat = 80 + (ghit->column - 1)/2;
  } else {
    slat = -1000;
  }
  slatid = slat + (ghit->panel - 1)*TOF_NSLAT_PANEL;
  return(slatid);
}
