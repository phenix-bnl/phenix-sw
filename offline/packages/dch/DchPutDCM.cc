#include "dDchDCMWrapper.h"

#include "encodeString.h"

#include "Event.h"
#include "PHIODataNode.h"
#include "PHRawDataNode.h"

#include <cstdlib>
#include <iostream>

using namespace std;

typedef PHDataNode<Event> EventNode_t;
typedef PHIODataNode<PHTable> TableNode_t;

long
DchPutDCM(PHCompositeNode* topNode)
{
  PHNodeIterator iter(topNode), *j;
  PHCompositeNode* dcmNode;
  PHCompositeNode* prdfNode;
  PHNode *n1;
  TableNode_t *d;

  static Int_t iCall = 0;   // used for initialization of rawData pointers

  // Find the DCM Node.
  n1 = iter.findFirst("PHCompositeNode", "DCM");
  if (!n1) {
    cout << "\n DchPutDCM <E>: Unable to find DCM subnode, exiting \n\n" << endl;
    exit(1);
  }
  else {
    dcmNode = static_cast<PHCompositeNode*>(n1);
  }

  //Find the PRDF node
  n1 = iter.findFirst("PHCompositeNode", "SIMPRDF");
  static int iWarn = 1;
  if(!n1) {
    if(iWarn) {
      cout << "\n DchPutDCM <E>: Unable to find SIMPRDF subnode, trying PRDF name from Run2 PRECO usage \n\n" << endl;
      iWarn = 0;
    }
    n1 = iter.findFirst("PHCompositeNode", "PRDF");
  }  // maintaining backward compatibility
  if (!n1) {
    cout << "\n DchPutDCM <E>: Unable to find SIMPRDF or PRDF subnode, exiting \n\n" << endl;
    exit(1);  
  }
  else {
    prdfNode = static_cast<PHCompositeNode*>(n1);
  }

  // Extract the data from the dDchDCM in DCM Node.
  dDchDCMWrapper *w;
  j = new PHNodeIterator(dcmNode);
  d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dDchDCM"));
  if (!d) {
    cout << "\n DchPutDCM <E>: Unable to find STAF Table dDchDCM, exiting \n\n" << endl;
    exit(1);
  }
  else {
    w = static_cast<dDchDCMWrapper*>(d->getData());
    if (!w) {
      cout << "\n DchPutDCM <E>: Unable to get pointer for wrapped table, exiting \n\n" << endl;
      exit(1);
    }
  }
  delete j;

  TABLE_HEAD_ST fTableHeader = w->TableHeader();
  Int_t dcmRows = fTableHeader.nok;
  static Int_t dcmRowsLast = 0;

  DDCHDCM_ST* dDchDCM = w->TableData();

  Int_t numberOfWords = dDchDCM[0].nWord;
  Int_t dataID = dDchDCM[0].packetID;
  Int_t bytesPerWord = 4;
  Int_t hitFormat = dDchDCM[0].scheme;
  
  if( dcmRows>160 || numberOfWords>971 ) {
    cout << "\n DchPutDCM <E>: dcmRows = " << dcmRows;
    cout << ",  numberOfWords = " << numberOfWords << endl;
    cout << ";  Limits are 160 and 971; exiting" << endl;
    exit(1);
  }

  static PHRawDataNode **rawDataArray;
  if(iCall == 0) {
    rawDataArray = new PHRawDataNode *[160];
    for(Int_t iRow=0; iRow<160; iRow++) {
      rawDataArray[iRow] = 0;
    }
  }  // initialization of rawDataArray pointers
  iCall++;

  char dchPRDF[11] = "dchPRDF000";  // assumes maximum of 999 rows

  for (Int_t iRow=0; iRow<dcmRows; iRow++){
    numberOfWords = dDchDCM[iRow].nWord;
    encodeString(dchPRDF, iRow);
    dataID = dDchDCM[iRow].packetID;
    hitFormat = dDchDCM[iRow].scheme;

    PHDWORD *dPtr = (PHDWORD*)&dDchDCM[iRow].DCM[0];
    if(rawDataArray[iRow] == 0) {
      rawDataArray[iRow] = new PHRawDataNode(dPtr, dchPRDF, numberOfWords, dataID, bytesPerWord, hitFormat);
      prdfNode->addNode(rawDataArray[iRow]);
    }   
    else {
      PHRawDataNode *rPtr = rawDataArray[iRow];
      rPtr->setData(dPtr);
      rPtr->setLength(numberOfWords);
      rPtr->setID(dataID);
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
