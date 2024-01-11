#include "mBbcUnpackMDC2Module.h"
#include "dBbcDCMMDC2Wrapper.h"
#include "dBbcGeoWrapper.h"
#include "dBbcRawWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNode.h"
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"
#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNodeIterator.h"
#include "PHTable.hh"

#include <iostream>
using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

PHBoolean
mBbcUnpackMDC2Module::event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 TableNode_t *d;
 PHTable *w;
 PHCompositeNode *parNode, *bbcNode, *dcmNode, *dstNode, *outNode;

 parNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "PAR"));
 if (!parNode) {
   parNode = new PHCompositeNode("PAR");
   root->addNode(parNode);
 }

 dcmNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DCM"));
 if (!dcmNode) {
   dcmNode = new PHCompositeNode("DCM");
   root->addNode(dcmNode);
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
//
  dBbcDCMMDC2Wrapper* BbcDCMWrapper;

  outNode = dcmNode;
  j = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dBbcDCM"));
  if (!d) {
    cout << "ERROR:  'in' parameter dBbcDCM not found" << endl;
    BbcDCMWrapper = new dBbcDCMMDC2Wrapper("dBbcDCM", 1);
    if (!BbcDCMWrapper) {
      return 1;
    }
    d = new TableNode_t(BbcDCMWrapper,"dBbcDCM");
    outNode->addNode(d);
  }
  else {
    BbcDCMWrapper = static_cast<dBbcDCMMDC2Wrapper*>(d->getData());
    if (!BbcDCMWrapper) {
      return 1;
    }
  }
  delete j;
  nodes.append(d);

  DBBCDCMMDC2_ST* bbcdcm = BbcDCMWrapper->TableData();

/*****
  for (int i=0;i<408;i++ ) {
    cout << " i = " << i << " " << bbcdcm->DCM[i] << endl;
  }
*****/

  outNode = parNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dBbcGeo")))) {
    cout << "ERROR:  'in' parameter dBbcGeo not found" << endl;
    w = new dBbcGeoWrapper("dBbcGeo", 1);
    if (!w) {
      return 1;
    }
    d = new TableNode_t(w,"dBbcGeo");
    outNode->addNode(d);
  }
  delete j;
  nodes.append(d);

  dBbcRawWrapper* BbcRawWrapper;

  outNode = dstNode;
  j = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dBbcRaw"));
  if (!d) {
    BbcRawWrapper = new dBbcRawWrapper("dBbcRaw", 128);
    if (!BbcRawWrapper) {
      return 1;
    }
    d = new TableNode_t(BbcRawWrapper,"dBbcRaw");
    outNode->addNode(d);
  }
  else { 
    BbcRawWrapper = static_cast<dBbcRawWrapper*>(d->getData());
    if (!BbcRawWrapper) {
      return 1;
    }
  }
  delete j;
  nodes.append(d);

  DBBCRAW_ST* bbcraw = BbcRawWrapper->TableData();

  unsigned long DCMwcnt;
  unsigned long DCMword;

  short fem_board_id = 0;
  short remainder1; 
  short remainder2;
  short pmt_number; 

  DBBCRAW_ST *p1;

  BbcRawWrapper->SetRowCount(128);

  unsigned long nloop;
  if ( bbcdcm->nWord == 408 ) {
    nloop = bbcdcm->nWord;
  } 
  else {
    nloop = 408;
  } 

  for (DCMwcnt = 1; DCMwcnt < nloop; DCMwcnt++) 
    {
    DCMword = bbcdcm->DCM[DCMwcnt];
    if ( (DCMword&0xf0000) == 0x00000 ) {
      if ( bbcdcm->nWord == 408 ) 
        {
        remainder1 = (DCMwcnt-6)%25;
      }
      else {
        remainder1 = (DCMwcnt-5)%25;
      } 
      if (remainder1 == 0 ) {
        if ( bbcdcm->nWord == 408 ) {
          fem_board_id = bbcdcm->DCM[DCMwcnt]-1; 
        }
        else {
          fem_board_id = bbcdcm->DCM[DCMwcnt]; 
        }
      } 
      else {
        if ( bbcdcm->nWord == 408 ) {
          remainder2 = (remainder1%3);
          if      ( remainder2 == 1 ) {
            pmt_number = (fem_board_id*8) + (remainder1/3);
            if ( pmt_number < 128 ) {
              p1 = bbcraw + pmt_number;
              p1->Pmt = pmt_number;
              p1->Adc = bbcdcm->DCM[DCMwcnt];
              p1->Arm = 0;
              p1->Half= 0;
              p1->Ring= 0;
              p1->Tube= 0; 
            }
          } 
          else if ( remainder2 == 2 ) {       
            pmt_number = (fem_board_id*8) + (remainder1/3);
            if ( pmt_number < 128 ) {
              p1 = bbcraw + pmt_number;
              p1->Pmt = pmt_number;
              p1->Tdc0= bbcdcm->DCM[DCMwcnt];
              p1->Arm = 0;
              p1->Half= 0;
              p1->Ring= 0;
              p1->Tube= 0; 
            }
          } 
          else if ( remainder2 == 0 ) {
            pmt_number = (fem_board_id*8) + (remainder1/3) - 1;
            if ( pmt_number < 128 ) {
              p1 = bbcraw + pmt_number;
              p1->Pmt = pmt_number;
              p1->Tdc1= bbcdcm->DCM[DCMwcnt];
              p1->Arm = 0;
              p1->Half= 0;
              p1->Ring= 0;
              p1->Tube= 0; 
            }
          } 
        }
        else {
          remainder2 = (remainder1%3);
          if      ( remainder2 == 1 ) {
            pmt_number = (fem_board_id*8) + (remainder1/3);
            if ( pmt_number < 128 ) {
              p1 = bbcraw + pmt_number;
              p1->Pmt = pmt_number;
              p1->Tdc0= bbcdcm->DCM[DCMwcnt];
              p1->Arm = 0;
              p1->Half= 0;
              p1->Ring= 0;
              p1->Tube= 0; 
            }
          } 
          else if ( remainder2 == 2 ) {       
            pmt_number = (fem_board_id*8) + (remainder1/3);
            if ( pmt_number < 128 ) {
              p1 = bbcraw + pmt_number;
              p1->Pmt = pmt_number;
              p1->Tdc1= bbcdcm->DCM[DCMwcnt];
              p1->Arm = 0;
              p1->Half= 0;
              p1->Ring= 0;
              p1->Tube= 0; 
            }
          } 
          else if ( remainder2 == 0 ) {
            pmt_number = (fem_board_id*8) + (remainder1/3) - 1;
            if ( pmt_number < 128 ) {
              p1 = bbcraw + pmt_number;
              p1->Pmt = pmt_number;
              p1->Adc = bbcdcm->DCM[DCMwcnt];
              p1->Arm = 0;
              p1->Half= 0;
              p1->Ring= 0;
              p1->Tube= 0; 
            }
          } 
        }
      }
    }
  }
/******
  for ( int iii=0;iii<128;iii++ ) {
    p1=bbcraw+iii;
    printf("Unpacked!!! %d %d %d %d\n",p1->Pmt,p1->Adc,p1->Tdc0,p1->Tdc1);
  }
******/
  return 1;
}
