/****************************************************************************
 *        Convert from mTofSetUcal.c to mTofSetUcalModule.C
 *                    [ STAF wrapper => pure PHOOL module ]
 **************************************************************************** 
 *:>--------------------------------------------------------------------
 *: FILE:       mTofSetUcal.c
 *: AUTHOR:     Akio Kiyomichi (Univ. of Tsukuba)
 *: HISTORY:    11/14/97  First Version
 *:             05/29/98 A.Kiyomichi  Add slewing parameter a,b
 *:             06/05/98 A.Kiyomichi  Add parameter table dTofUcalPar
 *:             08/01/98 H. Sako      Pedestal ch changed to 4095.
 *:             06/10/00 A.Kiyomichi create pure PHOOL module
 *:<------------------------------------------------------------------*/

#include "Tof.hh"
#include "mTofSetUcalModule.h"
#include "dTofUcalParWrapper.h"
#include "dTofUcalWrapper.h"

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
static DTOFUCAL_ST default_ucal = {
  0,      /* Slat ID */
  {
   3750,   /* Energy Loss per QVC channel [eV/ch] (Lower PMT) */
   3750    /* Energy Loss per QVC channel [eV/ch] (Upper PMT) */
  },
  {
   27.0,   /* Time conversion parameter for TVC [ps/ch] (Lower PMT) */
   27.0    /* Time conversion parameter for TVC [ps/ch] (Upper PMT) */
  },
  {
   4095.,  /* TVC pedestal (Lower PMT) */
   4095.   /* TVC pedestal (Upper PMT) */
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
  128.,   /* Light attenuation length [cm] */
  80.0    /* Time resolution [ps] */
};

PHBoolean
mTofSetUcalModule::event(PHCompositeNode *root) {
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

  // Extract the data from the dTofUcalPar
  dTofUcalParWrapper *TofUcalParWrapper;
  outNode = parNode;
  n = i.findFirst("PHIODataNode", "dTofUcalPar");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofUcalPar not found" << endl;
     TofUcalParWrapper = new dTofUcalParWrapper("dTofUcalPar", 1);
     if (!TofUcalParWrapper) {
       return 1;
     }
     n = new TableNode_t(TofUcalParWrapper,"dTofUcalPar");
     outNode->addNode(n);
  }
  nodes.append(n);
  j = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dTofUcalPar"));
  if (!d) {
    cerr << "  Error "<< endl; return 1;
  } else {
    TofUcalParWrapper = static_cast<dTofUcalParWrapper*>(d->getData());
    if (!TofUcalParWrapper) {cerr<<" Error"<< endl; return 1;}
  }
  delete j;
  DTOFUCALPAR_ST *ucalpar = TofUcalParWrapper->TableData();

  // Extract the data from the dTofUcal
  dTofUcalWrapper *TofUcalWrapper;
  outNode = parNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dTofUcal")))) {
     TofUcalWrapper = new dTofUcalWrapper("dTofUcal", 1100);
     if (!TofUcalWrapper) {
       return 1;
     }
     d = new TableNode_t(TofUcalWrapper,"dTofUcal");
     outNode->addNode(d);
  } else {
    TofUcalWrapper = static_cast<dTofUcalWrapper*>(d->getData());
    if (!TofUcalWrapper){ cerr<<" Error"<< endl; exit(1);}
    TofUcalWrapper->SetMaxRowCount(1100);
  }
  delete j;
  nodes.append(d);
  DTOFUCAL_ST *ucal = TofUcalWrapper->TableData();

  //=======================================================================
  // Convert from mTofSetUcal.c
  //=======================================================================

  int k;

  if(TofUcalParWrapper->RowCount() == 0){
    for(k = 0; k < TOF_NSLAT; k++){
      ucal[k] = default_ucal;
      ucal[k].slatid = k;
    }
  } else {
    for(k = 0; k < TOF_NSLAT; k++){
      ucal[k].slatid = k;
      if(ucalpar[0].option == 0){
	ucal[k].qvc_chgain[0]  = ucalpar[0].qvc_chgain[0];
	ucal[k].qvc_chgain[1]  = ucalpar[0].qvc_chgain[1];
	ucal[k].tvc_conv[0]    = ucalpar[0].tvc_conv[0];
	ucal[k].tvc_conv[1]    = ucalpar[0].tvc_conv[1];
	ucal[k].tvc_ped[0]     = ucalpar[0].tvc_ped[0];
	ucal[k].tvc_ped[1]     = ucalpar[0].tvc_ped[1];
	ucal[k].slew_a[0]      = ucalpar[0].slew_a[0];
	ucal[k].slew_a[1]      = ucalpar[0].slew_a[1];
	ucal[k].slew_b[0]      = ucalpar[0].slew_b[0];
	ucal[k].slew_b[1]      = ucalpar[0].slew_b[1];
	ucal[k].scint_vlight   = ucalpar[0].scint_vlight;
	ucal[k].scint_attenu   = ucalpar[0].scint_attenu;
	ucal[k].tof_sigma      = ucalpar[0].tof_sigma;
      } else if(ucalpar[0].option == 1){
	/* Not yet develop  --AK   Jun-5-98 */
	/* ucalpar[0].datafile */
      }
    }
  }
  TofUcalWrapper->SetRowCount(TOF_NSLAT);

  return True;
}
