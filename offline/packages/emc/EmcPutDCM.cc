#include "dEmcDCMDataWrapper.h"

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
EmcPutDCM(PHCompositeNode* topNode)
{
  PHNodeIterator iter(topNode), *j;
  PHCompositeNode* DCMNode;
  PHCompositeNode* prdfNode;
  PHNode *n1;
  TableNode_t *d;

  static Int_t iCall = 0; // used to initialize the rawdata pointers 

  // Find the DCM Node.
  n1 = iter.findFirst("PHCompositeNode", "DCM");
  if (!n1) {
    cout << "\n EmcPutDCM <E>: Unable to find DCM subnode, exiting \n\n" << endl;
    exit(1);
  }
  else {
    DCMNode = static_cast<PHCompositeNode*>(n1);
  }

  //Find the PRDF node
  n1 = iter.findFirst("PHCompositeNode", "SIMPRDF");
  static int iWarn = 1;
  if(!n1) {
    if(iWarn) {
      cout << "\n EmcPutDCM <E>: Unable to find SIMPRDF subnode, trying PRDF name from Run2 PRECO usage \n\n" << endl;
      iWarn = 0;
    }
    n1 = iter.findFirst("PHCompositeNode", "PRDF");
  }  // maintaining backward compatibility
  if (!n1) {
    cout << "\n EmcPutDCM <E>: Unable to find SIMPRDF or PRDF subnode, exiting \n\n" << endl;
    exit(1);  
  }
  else {
    prdfNode = static_cast<PHCompositeNode*>(n1);
  }

  // Extract the data from the dEmcDCMdata in DCM Node.
  dEmcDCMDataWrapper *w;
  j = new PHNodeIterator(DCMNode);
  d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dEmcDCMData"));
  if (!d) {
    cout << "\n EmcPutDCM <E>: Unable to find STAF Table dEmcDCMData, exiting \n\n" << endl;
    exit(1);
  }
  else {
    w = static_cast<dEmcDCMDataWrapper*>(d->getData());
    if (!w) {
      cout << "\n EmcPutDCM <E>: Unable to get pointer for wrapped table, exiting \n\n" << endl;
      exit(1);
    }
  }
  delete j;

  TABLE_HEAD_ST fTableHeader = w->TableHeader();
  Int_t dcmRows = fTableHeader.nok;
  static Int_t dcmRowsLast = 0;

  DEMCDCMDATA_ST* dEmcDCMData = w->TableData();

  Int_t numberOfWords = dEmcDCMData[0].nWords;
  Int_t bytesPerWord = 4;
  
  if( dcmRows>1500 || numberOfWords>450 ) {
    cout << "\n EmcPutDCM <E>: dcmRows = " << dcmRows;
    cout << ",  numberOfWords = " << numberOfWords << endl;
    cout << ";  Limits are 1500 and 450; exiting" << endl;
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

  char emcPRDF[11] = "emcPRDF000";  // assumes maximum of 999 rows

  for (Int_t iRow=0; iRow<dcmRows; iRow++){
    numberOfWords = dEmcDCMData[iRow].nWords;
    encodeString(emcPRDF, iRow);
    Int_t dataID = dEmcDCMData[iRow].packetID;
    Int_t hitFormat = dEmcDCMData[iRow].scheme;

    PHDWORD *dPtr = (PHDWORD*)&dEmcDCMData[iRow].DCM[0];
    if(rawDataArray[iRow] == 0) {
      rawDataArray[iRow] = new PHRawDataNode(dPtr, emcPRDF, numberOfWords, dataID, bytesPerWord, hitFormat);
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










