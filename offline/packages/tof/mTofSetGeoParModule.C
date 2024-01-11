/****************************************************************************
 *      Convert from mTofSetGeoPar.c to mTofSetGeoParModule.C
 *                    [ STAF wrapper => pure PHOOL module ]
 **************************************************************************** 
 *:>--------------------------------------------------------------------
 *: FILE:       mTofSetGeoPar.c
 *: AUTHOR:     Akio Kiyomichi (Univ. of Tsukuba)
 *: HISTORY:    11/17/97  First Version
 *:             06/11/00 A.Kiyomichi create pure PHOOL module
 *:<------------------------------------------------------------------*/
/*------------------------------------------------------------------------*
 *          Slat position define from PISA-TOF routine                    *
 *c  v_m_name  =  'CLMN'       ! mother volum                             *
 *c  ========================================                             *
 *  scintz(1)=pCLMN_dim(3)-(send_gap+SCTS_dim(3)) ! short from positive z *
 *  scintz(2)=scintz(1)-(SCTS_dim(3)+btwn_gap+SCTL_dim(3)) ! long         *
 *  scintz(3)=scintz(2)-(SCTL_dim(3)+btwn_gap+SCTL_dim(3)) ! long         *
 *  real    send_gap/2.654/,lend_gap/5.486/,btwn_gap/4.445/               *
 *  real    pCLMN_dim(3)/1.5,5.5,93.98/                                   *
 *  real    SCTL_dim(3) /0.7505,0.7505,31.885/                            *
 *  real    SCTS_dim(3) /0.7505,0.7505,21.695/                            *
 *  real    Slat_halfwidth/0.76327/                                       *
 *------------------------------------------------------------------------*/

#include "mTofSetGeoParModule.h"
#include "dTofGeoParWrapper.h"

#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include <cstdlib>
#include <iostream>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

/* defalut geometry parameters */
static DTOFGEOPAR_ST default_geopar = {
  3.2905,    /* Slat R-position from panel front surface */
  1.52654,   /* Slat width */
  {
   69.631,    /* Slat local-Z position (short scinti) */
   11.606,    /* Slat local-Z position (long1 scinti) */
   56.609     /* Slat local-Z position (long2 scinti) */
  },
  {
   503.0,     /* TOF Panel   R-position [1] */
   503.0,     /* TOF Panel   R-position [2] */
   503.0,     /* TOF Panel   R-position [3] */
   503.0,     /* TOF Panel   R-position [4] */
   503.0,     /* TOF Panel   R-position [5] */
   503.0,     /* TOF Panel   R-position [6] */
   503.0,     /* TOF Panel   R-position [7] */
   503.0,     /* TOF Panel   R-position [8] */
   503.0,     /* TOF Panel   R-position [9] */
   503.0,     /* TOF Panel   R-position [10]*/
   0.0        /* TOF Panel   R-position [11]*/
  },
  {
   180.0,     /* TOF Panel phi-position [1] */
   180.0,     /* TOF Panel phi-position [2] */
   180.0,     /* TOF Panel phi-position [3] */
   180.0,     /* TOF Panel phi-position [4] */
   180.0,     /* TOF Panel phi-position [5] */
   180.0,     /* TOF Panel phi-position [6] */
   180.0,     /* TOF Panel phi-position [7] */
   180.0,     /* TOF Panel phi-position [8] */
   202.5,     /* TOF Panel phi-position [9] */
   202.5,     /* TOF Panel phi-position [10]*/
   0.0        /* TOF Panel phi-position [11]*/
  },
  {
   -170.972,  /* TOF Panel   Z-position [1] */
   -122.123,  /* TOF Panel   Z-position [2] */
   -73.2739,  /* TOF Panel   Z-position [3] */
   -24.4246,  /* TOF Panel   Z-position [4] */
   24.4246,   /* TOF Panel   Z-position [5] */
   73.2739,   /* TOF Panel   Z-position [6] */
   122.123,   /* TOF Panel   Z-position [7] */
   170.972,   /* TOF Panel   Z-position [8] */
   -24.4246,  /* TOF Panel   Z-position [9] */
   24.4246,   /* TOF Panel   Z-position [10]*/
   0.0        /* TOF Panel   Z-position [11]*/
  }
};

PHBoolean
mTofSetGeoParModule::event(PHCompositeNode *root) {
  PHPointerList<PHNode> nodes;
  PHNodeIterator i(root), *j;
  TableNode_t *d;
  PHCompositeNode *parNode, *outNode;

  parNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "PAR"));
  if (!parNode) {
    parNode = new PHCompositeNode("PAR");
    root->addNode(parNode);
  }

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  // Extract the data from the dTofGeoPar
  dTofGeoParWrapper *TofGeoParWrapper;
  outNode = parNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dTofGeoPar")))) {
     TofGeoParWrapper = new dTofGeoParWrapper("dTofGeoPar", 1100);
     if (!TofGeoParWrapper) {
       return 1;
     }
     d = new TableNode_t(TofGeoParWrapper,"dTofGeoPar");
     outNode->addNode(d);
  } else {
    TofGeoParWrapper = static_cast<dTofGeoParWrapper*>(d->getData());
    if (!TofGeoParWrapper){ cerr<<" Error"<< endl; exit(1);}
    TofGeoParWrapper->SetMaxRowCount(1);
  }
  delete j;
  nodes.append(d);
  DTOFGEOPAR_ST *par = TofGeoParWrapper->TableData();

  //=======================================================================
  // Convert from mTofSetGeoPar.c
  //=======================================================================

  memcpy(par,&default_geopar,sizeof(DTOFGEOPAR_ST));
  TofGeoParWrapper->SetRowCount(1);
  return True;
}
