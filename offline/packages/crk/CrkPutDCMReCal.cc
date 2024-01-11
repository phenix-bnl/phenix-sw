#include "dCrkDCMWrapper.h"

#include "encodeString.h"

#include "Event.h"
#include "PHIODataNode.h"
#include "PHRawDataNode.h"

#include "packetConstants.h"

#include <cstdlib>
#include <iostream>
using namespace std;

typedef PHDataNode<Event> EventNode_t;
typedef PHIODataNode<PHTable> TableNode_t;

long
CrkPutDCMReCal(PHCompositeNode* topNode)
{
  int dbg=0;

  PHNodeIterator iter(topNode), *j;
  PHCompositeNode* dcmNode;
  PHCompositeNode* prdfNode;
  PHNode *n1;
  TableNode_t *d;

  static Int_t iCall = 0; // used to initialize the rawdata pointers

  // Find the DCM Node.
  n1 = iter.findFirst("PHCompositeNode", "DCM");
  if (!n1) {
    cout << "\n CrkPutDCMReCal <E>: Unable to find DCM subnode, exiting \n\n" << endl;
    exit(1);
  }
  else {
    dcmNode = static_cast<PHCompositeNode*>(n1);
  }

  //Find the PRDF node
  n1 = iter.findFirst("PHCompositeNode", "PRDFRECAL");
  if (!n1) {
    cout << "\n CrkPutDCMReCal <E>: Unable to find PRDFRECAL subnode, exiting \n\n" << endl;
    exit(1);  
  }
  else {
    prdfNode = static_cast<PHCompositeNode*>(n1);
  }

  // Extract the data from the dCrkDCM in DCM Node.
  dCrkDCMWrapper *w;
  j = new PHNodeIterator(dcmNode);
  d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dCrkDCMReCal"));
  if (!d) {
    cout << "\n CrkPutDCMreCal <E>: Unable to find STAF Table dCrkDCMReCal, exiting \n\n" << endl;
    exit(1);
  }
  else {
    w = static_cast<dCrkDCMWrapper*>(d->getData());
    if (!w) {
      cout << "\n CrkPutDCMreCal <E>: Unable to get pointer for wrapped table, exiting \n\n" << endl;
      exit(1);
    }
  }
  delete j;

  TABLE_HEAD_ST fTableHeader = w->TableHeader();
  Int_t dcmRows = fTableHeader.nok;
  static Int_t dcmRowsLast = 0;

  DCRKDCM_ST* dCrkDCM = w->TableData();

  Int_t numberOfWords = 498; // this was hardwired in the staf mCrkDCMoutput
  Int_t dataID = 6000; // also hardwired
  Int_t bytesPerWord = 4;
  Int_t hitFormat = IDRICH_DCM0; // also hardwired
  
  if( dcmRows>1500 || numberOfWords>498 ) {
    cout << "\n CrkPutDCMreCal <E>: dcmRows = " << dcmRows;
    cout << ",  numberOfWords = " << numberOfWords << endl;
    cout << ";  Limits are 1500 and 498; exiting" << endl;
    exit(1);
  }

  static PHRawDataNode **rawDataArray;
  if(iCall == 0) {
    rawDataArray = new PHRawDataNode *[1500];
    for(Int_t iRow=0; iRow<1500; iRow++) {
      rawDataArray[iRow] = 0;
    }
  }  // initialization of rawDataArray pointers
  iCall++;

  char crkPRDF[11] = "crkPRDF000";  // assumes maximum of 999 rows
  Int_t packetid = 0;
  for (Int_t iRow=0; iRow<dcmRows; iRow++){

    // We want to match the packet numbers to those we would find in
    // real data, since that is what Lvl2 expects
    // The 32 real RICH packets start with 6001 and go to 6038, with gaps

    if(iRow < 8)
      packetid = iRow+1;
    else if(iRow > 7 && iRow < 16)
      packetid = iRow + 3;
    else if(iRow > 15 && iRow < 24)
      packetid = iRow + 5;
    else 
      packetid = iRow + 7;

    encodeString(crkPRDF, packetid);
    Int_t packID = dataID + packetid;
    hitFormat = IDRICH_DCM0;

    if(dbg)
      cout << " crk packID = " << packID << " for " << crkPRDF << endl;
    
    PHDWORD *dPtr = (PHDWORD*)&dCrkDCM[iRow].flag;
    if(rawDataArray[iRow] == 0) {
      rawDataArray[iRow] = new PHRawDataNode(dPtr, crkPRDF, numberOfWords, packID, bytesPerWord, hitFormat);
      prdfNode->addNode(rawDataArray[iRow]);
    }   
    else {
      PHRawDataNode *rPtr = rawDataArray[iRow];
      rPtr->setData(dPtr);
      rPtr->setLength(numberOfWords);
      rPtr->setID(packID);
      rPtr->setWordLength(bytesPerWord);
      rPtr->setHitFormat(hitFormat);
    }
  } // loop over dcmRows

  if(iCall > 1 && dcmRows < dcmRowsLast){
    for(Int_t iRow=dcmRows; iRow<dcmRowsLast; iRow++) {
      PHRawDataNode *rPtr = rawDataArray[iRow];
      rPtr->setLength(0);
      rPtr->setID(0);
    }  // zero out excess rows
  }
  
  dcmRowsLast = dcmRows;  // save for next event
  return 0;
}
