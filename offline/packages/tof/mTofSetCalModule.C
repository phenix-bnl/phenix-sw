/****************************************************************************
 *         Convert from mTofSetCal.c to mTofSetCalModule.C
 *                    [ STAF wrapper => pure PHOOL module ]
 **************************************************************************** 
 *:>--------------------------------------------------------------------
 *: FILE:       mTofSetCal.c
 *: AUTHOR:     Akio Kiyomichi (Univ. of Tsukuba)
 *: HISTORY:    11/18/97  First Version
 *:              5/29/98 A.Kiyomichi  Add slewing parameter a,b
 *:             08/01/98 H. Sako      TVC pedestal value changed to 4095
 *:             06/11/00 A.Kiyomichi create pure PHOOL module
 *:<------------------------------------------------------------------*/

#include "Tof.hh"
#include "mTofSetCalModule.h"
#include "dTofCalParWrapper.h"
#include "dTofCalWrapper.h"

#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include <cstdlib>
#include <iostream>
using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

/* defalut calibration parameters */
static DTOFCAL_ST default_cal = {
  0,      /* Slat ID */
  {
   1.0,    /* QVC correction parameter (Lower PMT) */
   1.0     /* QVC correction parameter (Upper PMT) */
  },
  {
   1.0,    /* QVC correction parameter for LASER (Lower PMT) */
   1.0     /* QVC correction parameter for LASER (Upper PMT) */
  },
  800.,   /* Energy loss conversion parameter [ch/mip] */
  0.003,  /* m.i.p. in one slat [GeV] */
  {
   27.0,   /* Time conversion parameter for TVC [ps/ch] (Lower PMT) */
   27.0    /* Time conversion parameter for TVC [ps/ch] (Upper PMT) */
  },
  {
   4095.,  /* TVC pedestal [ch] (Lower PMT) */
   4095.   /* TVC pedestal [ch] (Upper PMT) */
  },
  {
   0.0,    /* Timing offset of TVC [ch] (Lower PMT) */
   0.0     /* Timing offset of TVC [ch] (Upper PMT) */
  },
  {
   0.0,    /* Timing offset of TVC for LASER [ch] (Lower PMT) */
   0.0     /* Timing offset of TVC for LASER [ch] (Upper PMT) */
  },
  {
   -12.0,  /* Slewing parameter a (Lower PMT) */
   -12.0   /* Slewing parameter a (Upper PMT) */
  },
  {
   650.,   /* Slewing parameter b (Lower PMT) */
   650.    /* Slewing parameter b (Upper PMT) */
  },
  14.0,   /* Light velocity in scintillator [cm/ns] */
  128.    /* Light attenuation length [cm] */
};

PHBoolean
mTofSetCalModule::event(PHCompositeNode *root) {
  PHPointerList<PHNode> nodes;
  PHNodeIterator i(root), *j;
  PHNode *n;
  TableNode_t *d;
  PHCompositeNode *parNode, *outNode;

  parNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "PAR"));
  if (!parNode) {
    parNode = new PHCompositeNode("PAR");
    root->addNode(parNode);
  }

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  // Extract the data from the dTofCalPar
  dTofCalParWrapper *TofCalParWrapper;
  outNode = parNode;
  n = i.findFirst("PHIODataNode", "dTofCalPar");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofCalPar not found" << endl;
     TofCalParWrapper = new dTofCalParWrapper("dTofCalPar", 1100);
     if (!TofCalParWrapper) {
       return 1;
     }
     n = new TableNode_t(TofCalParWrapper,"dTofCalPar");
     outNode->addNode(n);
  }
  nodes.append(n);
  j = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dTofCalPar"));
  if (!d) {
    cerr << "  Error "<< endl; return 1;
  } else {
    TofCalParWrapper = static_cast<dTofCalParWrapper*>(d->getData());
    if (!TofCalParWrapper) {cerr<<" Error"<< endl; return 1;}
  }
  delete j;
  DTOFCALPAR_ST *calpar = TofCalParWrapper->TableData();

  // Extract the data from the dTofCal
  dTofCalWrapper *TofCalWrapper;
  outNode = parNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dTofCal")))) {
     TofCalWrapper = new dTofCalWrapper("dTofCal", 1100);
     if (!TofCalWrapper) {
       return 1;
     }
     d = new TableNode_t(TofCalWrapper,"dTofCal");
     outNode->addNode(d);
  } else {
    TofCalWrapper = static_cast<dTofCalWrapper*>(d->getData());
    if (!TofCalWrapper){ cerr<<" Error"<< endl; exit(1);}
    TofCalWrapper->SetMaxRowCount(1100);
  }
  delete j;
  nodes.append(d);
  DTOFCAL_ST *cal = TofCalWrapper->TableData();

  //=======================================================================
  // Convert from mTofSetCal.c
  //=======================================================================

  int k;

  if(TofCalParWrapper->RowCount() == 0){
    for(k = 0; k < TOF_NSLAT; k++){
      cal[k] = default_cal;
      cal[k].slatid = k;
    }
  } else {
    for(k = 0; k < TOF_NSLAT; k++){
      cal[k].slatid = k;
      if(calpar[0].option == 0){
	cal[k].qvc_corr[0]     = calpar[0].qvc_corr[0];
	cal[k].qvc_corr[1]     = calpar[0].qvc_corr[1];
	cal[k].qvc_corrlsr[0]  = calpar[0].qvc_corrlsr[0];
	cal[k].qvc_corrlsr[1]  = calpar[0].qvc_corrlsr[1];
	cal[k].eloss_conv      = calpar[0].eloss_conv;
	cal[k].eloss_mip       = calpar[0].eloss_mip;
	cal[k].tvc_conv[0]     = calpar[0].tvc_conv[0];
	cal[k].tvc_conv[1]     = calpar[0].tvc_conv[1];
	cal[k].tvc_ped[0]      = calpar[0].tvc_ped[0];
	cal[k].tvc_ped[1]      = calpar[0].tvc_ped[1];
	cal[k].t0[0]           = calpar[0].t0[0];
	cal[k].t0[1]           = calpar[0].t0[1];
	cal[k].t0_lsr[0]       = calpar[0].t0_lsr[0];
	cal[k].t0_lsr[1]       = calpar[0].t0_lsr[1];
	cal[k].slew_a[0]       = calpar[0].slew_a[0];
	cal[k].slew_a[1]       = calpar[0].slew_a[1];
	cal[k].slew_b[0]       = calpar[0].slew_b[0];
	cal[k].slew_b[1]       = calpar[0].slew_b[1];
	cal[k].scint_vlight    = calpar[0].scint_vlight;
	cal[k].scint_attenu    = calpar[0].scint_attenu;
      } else if(calpar[0].option == 1){
	/* Not yet develop  --AK   Jun-5-98 */
	/* calpar[0].datafile */
      }
    }
  }
  TofCalWrapper->SetRowCount(TOF_NSLAT);

  return True;
}
