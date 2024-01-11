/****************************************************************************
 *         Convert from mTofUnpack.c to mTofUnpackModule.C
 *                    [ STAF wrapper => pure PHOOL module ]
 **************************************************************************** 
 * File: mTofUnpack.c
 * Author: Hiroyuki Sako (Univ. of Tsukuba)
 * Description: Convert dTofDCM to dTofRaw
 * If dTofFEMhitGhit is not empty, create dTofGhitRaw (relation table
 * between tofghit and dTofRaw)
 *
 * Schematic flow of dTofFEM, dTofDCM and dTofRaw tables
 *
 *                    tofghit(PISA)
 *                      |      \
 * (dTofGhitRaw)        V       \
 *      \   +-------> dTofRaw   dTofGhitRaw
 *       \  |           |
 *        \ |<----------|<---- dTofFEMmap(map bet. TOF slat to FEM channel)
 *      mTofUnpack      V
 *          |         dTofFEM------+
 *          |           |          |
 * dTofGeo->|           |          |
 *          |           V          |
 *          +---------dTofDCM  dTofFEMhitGhit (relation table between
 *                /                |           FEM crate/slot/channel
 *                |                |           <-> ID of tofghit)
 *                +----------------+
 *
 *
 *
 * input  : dTofGeo, dTofDCM, dTofFEMmap
 * output : dTofRaw
 *
 * Date: 07/26/98 H. Sako, First version
 *       09/29/98 H. Sako, Added checking of dTofRaw_h->maxlen,
 *                dTofFEMhitGhit_h->maxlen, and dTofGhitRaw_h->maxlen added.
 *       10/23/98 H. Sako  changed exit(1) to return STAFCV_BAD
 *       01/31/00 A.Kiyomichi Delete dTofGhitRaw and dTofFEMhitGhit
 *       05/28/00 A.Kiyomichi create pure PHOOL module
 *       06/08/00 A.Kiyomichi add TofEvent->DcmToRaw(root, address)
 *       06/13/00 A.Kiyomichi add q1 q2 t3 t4 in dtofRaw
 *       07/20/00 A.Kiyomichi dTofRaw move to dstNode
 ****************************************************************************/

#include "TofEvent.hh"
#include "mTofUnpackModule.h"
#include "dTofGeoWrapper.h"
#include "dTofDCMWrapper.h"
#include "dTofFEMmapWrapper.h"
#include "dTofRawWrapper.h"

#include "TofAddressObject.hh"


#include "PHPointerList.h"
#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include <iostream>
using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

// Default constructor and destructor to pacify CINT
mTofUnpackModule::mTofUnpackModule(){
  iDebug = 0;
}

PHBoolean
mTofUnpackModule::event(PHCompositeNode *root, TofAddressObject *address){
  TofEvent tofevent;

  tofevent.DcmToRaw(root, address);

  return True;
}

PHBoolean
mTofUnpackModule::event(PHCompositeNode *root) {
  PHPointerList<PHNode> nodes;
  PHNodeIterator iii(root), *jjj;
  PHNode *n;
  TableNode_t *d;
  PHTable *w;
  PHCompositeNode *parNode, *tofNode, *dstNode, *dcmNode, *outNode;

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

  dcmNode = static_cast<PHCompositeNode*>(iii.findFirst("PHCompositeNode", "DCM"));
  if (!dcmNode) {
    dcmNode = new PHCompositeNode("DCM");
    root->addNode(dcmNode);
  }

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

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
  DTOFGEO_ST *dTofGeo = TofGeoWrapper->TableData();

  // Extract the data from the dTofDCM
  dTofDCMWrapper *TofDCMWrapper;
  outNode = dcmNode;
  n = iii.findFirst("PHIODataNode", "dTofDCM");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofDCM not found" << endl;
     TofDCMWrapper = new dTofDCMWrapper("dTofDCM", 8);
     if (!TofDCMWrapper) {
       return 1;
     }
     n = new TableNode_t(TofDCMWrapper,"dTofDCM");
     outNode->addNode(n);
  }
  nodes.append(n);
  jjj = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofDCM"));
  if (!d) {
    cerr << "  Error "<< endl; return 1;
  } else {
    TofDCMWrapper = static_cast<dTofDCMWrapper*>(d->getData());
    if (!TofDCMWrapper) {cerr<<" Error"<< endl; return 1;}
  }
  delete jjj;
  DTOFDCM_ST *dTofDCM = TofDCMWrapper->TableData();

  // Extract the data from the dTofFEMmap
  dTofFEMmapWrapper *TofFEMmapWrapper;
  outNode = parNode;
  n = iii.findFirst("PHIODataNode", "dTofFEMmap");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofFEMmap not found" << endl;
     TofFEMmapWrapper = new dTofFEMmapWrapper("dTofFEMmap", 960);
     if (!TofFEMmapWrapper) {
       return 1;
     }
     n = new TableNode_t(w,"dTofFEMmap");
     outNode->addNode(n);
  }
  nodes.append(n);  
  jjj = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofFEMmap"));
  if (!d){ 
    cerr << "  Error "<< endl; return 1;
  } else {
    TofFEMmapWrapper = static_cast<dTofFEMmapWrapper*>(d->getData());
    if (!TofFEMmapWrapper){ cerr<<" Error"<< endl; exit(1);}
  }
  delete jjj;
  DTOFFEMMAP_ST *dTofFEMmap = TofFEMmapWrapper->TableData();

  // Extract the data from the dTofRaw
  dTofRawWrapper *TofRawWrapper;
  outNode = dstNode;
  jjj = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofRaw"));
  if (!d) {
     TofRawWrapper = new dTofRawWrapper("dTofRaw", 960);
     if (!TofRawWrapper) {
       return 1;
     }
     d = new TableNode_t(TofRawWrapper,"dTofRaw");
     outNode->addNode(d);
  } else {
    TofRawWrapper = static_cast<dTofRawWrapper*>(d->getData());
    if (!TofRawWrapper){ cerr<<" Error"<< endl; exit(1);}
    TofRawWrapper->SetMaxRowCount(960);
  }
  delete jjj;
  nodes.append(d);
  DTOFRAW_ST *dTofRaw = TofRawWrapper->TableData();

  //=======================================================================
  // Convert from mTofUnpack.c
  //=======================================================================
  
  long i,j;
  unsigned int *dcm;
  short scheme;
  long idcm;
  unsigned long nword;
  long iraw = 0; /* No. of dTofRaw */
  long jraw;

  const short day = 1; /* Day1 = 1, Day-N = 2 */
  const short nslat = 96;
  const short ntotslat = nslat*10;

  /* Loop of dTofDCM */

  for (i=0; i<TofDCMWrapper->RowCount(); i++) {
    scheme = dTofDCM[i].scheme;
    nword  = dTofDCM[i].nWord;
    dcm    = dTofDCM[i].DCM;
    
#ifdef DEBUG
    printf("DCM ID %ld, scheme %d, nword %lu\n", i, scheme, nword);
#endif
    idcm = 5; /* Skip DCM header */

    while (idcm < nword) {
      /* This is FEM header */
      long idcm0  = idcm;
      short icrate = (short)((dcm[idcm0+1] & 0xf00) / 0x100); /* Crate ID */
      short islot  = (short)((dcm[idcm0+1] & 0x0f0) / 0x010); /* Slot ID */
      short icell  = (short)(dcm[idcm0+2] & 0xffff); /* Cell ID is common
					     for Time,Q-Pre and Q-Post)*/
      unsigned short tvc, qvc_pre, qvc_pos;
      unsigned short data_type = 1;

      short slatid;
      short tb; /* Bottom PMT : tb = 0, Top PMT : tb = 1 */

#ifdef DEBUG
	printf("Start while loop\n");
	printf("idcm = %ld, nword = %lu\n", idcm, nword);
#endif


      for (idcm += 5; /* Go to channel data */
	   idcm < nword; idcm += 3) {

	/* Present data is TVC-QVC data */
	short ich = (short)((dcm[idcm] & 0xff00000)/0x100000);
	unsigned short tvc     = (dcm[idcm]   & 0xfff); /* Lower 16 bits */
	unsigned short qvc_pre = (dcm[idcm+1] & 0xfff);
	unsigned short qvc_pos = (dcm[idcm+2] & 0xfff);
#ifdef DEBUG
	printf("Inside for ch-data loop, idcm = %ld\n", idcm);
#endif

	data_type = (dcm[idcm] & 0xf0000) / 0x10000;

#ifdef DEBUG
	printf("dcm[%ld] = %x\n", idcm, dcm[idcm]);
	printf("dcm[%ld] = %x\n", idcm+1,dcm[idcm+1]);
	printf("dcm[%ld] = %x\n", idcm+2,dcm[idcm+2]);
	printf("data_type = %d\n", data_type);
	printf("icrate %d, islot %d, ich %d\n",
	       icrate, islot, ich);
	printf("ich %d : tvc %d, qvc_pre %d, qvc_pos %d\n",
	       ich, tvc, qvc_pre, qvc_pos);
#endif
	if (data_type != 12) {
#ifdef DEBUG
	  printf("Now data_type %d != 12 at idcm = %ld\n",
		 data_type, idcm);
#endif
	  break;
	}

	if ((tvc == 0) && (qvc_pre == 0) && (qvc_pos == 0)) {/* empty data */
	  if(iDebug>1){
	    printf("empty data at ich = %d\n", ich);
	    printf("Going to next 3 channels\n");
	  }
	  continue; /* go next chanel data */
	}

	tb = -1;
	for (slatid=0; slatid<ntotslat; slatid++) { /* Search for slatid */
	  if ((icrate == dTofFEMmap[slatid].crate)
	      && (islot == dTofFEMmap[slatid].slot)) {
	    if (ich == dTofFEMmap[slatid].ch[0]) {
	      tb = 0;
	      break;
	    }
	    else if (ich == dTofFEMmap[slatid].ch[1]) {
	      tb = 1;
	      break;
	    }
	  }
	}  /* End of slatid loop */

	if (tb == -1) { /* fail to find slatid ... */
	  printf("Couldn't find slatid\n");
	  return False;
	}

	if(iDebug>1){
	  printf("slatid (%d) found. tb = %d\n", slatid, tb);
	}


	for (jraw=0; jraw<iraw; jraw++) { /* Search for pair PMT data */
	  if (dTofRaw[jraw].slatid == slatid) {
#ifdef DEBUG
	    printf("Pair PMT found at dTofRaw[%ld], slatid %d !\n",
		   jraw, slatid);
#endif
	    /* Valid dat with top and bot PMT's*/
	    dTofRaw[jraw].cell[tb] = icell;
	    /* Valid dat with top and bot PMT's*/
	    dTofRaw[jraw].qvc[tb] = qvc_pos - qvc_pre;
	    dTofRaw[iraw].q1[tb]  = qvc_pre;
	    dTofRaw[iraw].q2[tb]  = qvc_pos;
	    dTofRaw[iraw].tvc[tb] = tvc;
	    dTofRaw[iraw].t3[tb]  = tvc;
	    dTofRaw[iraw].t4[tb]  = tvc;
	    break;
	  }
	} /* End search for pair PMT data */
	if (jraw == iraw) { /* If PMT with no pair */

	  if(iDebug>0){
	    if(iraw%5 == 0){
	      printf("Now filling dTofRaw[%ld]. slatid = %d\n",
		     iraw, slatid);
	    }
	  }
	  dTofRaw[iraw].id     = iraw;
	  dTofRaw[iraw].slatid = slatid;
	  
	  dTofRaw[iraw].sector = dTofGeo[slatid].sector;
	  dTofRaw[iraw].side   = dTofGeo[slatid].side;
	  dTofRaw[iraw].panel  = dTofGeo[slatid].panel;
	  dTofRaw[iraw].slat   = dTofGeo[slatid].slat;


	  dTofRaw[iraw].cell[tb] = icell; 
	  dTofRaw[iraw].qvc[tb] = qvc_pos - qvc_pre;
	  dTofRaw[iraw].q1[tb]  = qvc_pre;
	  dTofRaw[iraw].q2[tb]  = qvc_pos;
	  dTofRaw[iraw].tvc[tb] = tvc;
	  dTofRaw[iraw].t3[tb]  = tvc;
	  dTofRaw[iraw].t4[tb]  = tvc;

	  /* single PMT data still invalid */	  
	  if (tb == 0) {
	    dTofRaw[iraw].cell[1] = -1;
	    dTofRaw[iraw].qvc[1] = 0;
	    dTofRaw[iraw].q1[1]  = 4095;
	    dTofRaw[iraw].q2[1]  = 4095;
	    dTofRaw[iraw].tvc[1] = 0;
	    dTofRaw[iraw].t3[1]  = 0;
	    dTofRaw[iraw].t4[1]  = 0;
	  }
	  else {
	    dTofRaw[iraw].cell[0] = -1;
	    dTofRaw[iraw].qvc[0] = 0;
	    dTofRaw[iraw].q1[0]  = 4095;
	    dTofRaw[iraw].q2[0]  = 4095;
	    dTofRaw[iraw].tvc[0] = 0;
	    dTofRaw[iraw].t3[0]  = 0;
	    dTofRaw[iraw].t4[0]  = 0;
	  }
	  
	  iraw++;

	  if (iraw > TofRawWrapper->MaxRowCount()){
	    printf("iraw %ld > TofRawWrapper->MaxRowCount() %u\n",
		   iraw, TofRawWrapper->MaxRowCount());
	    return False;
	  }
	} 
      } /* Loop of ch data */

      if (data_type == 6)
	continue; /* Go to next FEM header */
      else if (data_type == 7)
	break; /* End of DCM data*/
      else {
	printf("Strange data_type = %d, dcm[%ld] :%x\n",
	       data_type, idcm, dcm[idcm]);
	return False;
      }

    } /* dcm data loop */
    
  } /* Loop of DCM */

  TofRawWrapper->SetRowCount(iraw); /* Adjust size of dTofRaw */

  if(iDebug>0){
    printf("Number of rows in dTofRaw = %ld\n",iraw);
  }

  return True;
}
/* Data format by J.Nagle
      Bits 31-29  28-20  19-16     15-0
index 
---------------------------------------------
  0         0       0      15   Flag Word
  1         0       0       6   Module #  DCM module? 
  2         0       0       6   Event #
  3         0       0       6   Beam CLK #
  4         0       0       6   Detector ID = 0x0700
  ..................
  m         0       0       6   FEM Event #    <--- at each FEM
  m+1       0       0       6   FEM Address
  m+2       0       0       6   AMU Cell Time
  m+3       0       0       6   AMU Cell Pre
  m+4       0       0       6   AMU Cell Post
  ..................
  m+n*3     0       n      12   ch N time      <---- for each ch
  m+n*3+1   0       n      10   ch N pre        (skipped if zero suppressed)
  m+n*3+2   0       n       9   ch N post
  ..................
  last-1    0       7       7   Parity Word
  last      0       7       7   Last Word
---------------------------------------------
Word length (no zero suppression)
= 5 + 5*16 + 3*16*16 + 2
= 855

10 panels = 8 FEM crates

Day1:
8 DCM's (2 DCB's)
1 FEM crate / DCM

DayN:
16 DCM's (4 DCB's)
1/2 FEM crate /DCM
*/
