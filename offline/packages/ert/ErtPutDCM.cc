#include "dErtDcmDataWrapper.h"
#include "ErtPutDCM.h"

#include "encodeString.h"

#include "phool.h"
#include "Event.h"
#include "PHIODataNode.h"
#include "PHCompositeNode.h"
#include "PHNodeIterator.h"
#include "PHRawOManager.h"
#include "PHRawDataNode.h"

#include <cstdlib>
#include <iostream>
#include <packetConstants.h>

using namespace std;

typedef PHDataNode<Event> EventNode_t;
typedef PHIODataNode<PHTable> TableNode_t;

long ErtPutDCM(PHCompositeNode* topNode)
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
    cout << "\n ErtPutDCM<E>: Unable to find DCM subnode,exiting \n\n" << endl;
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
      cout << "\n ErtPutDCM <E>: Unable to find SIMPRDF subnode, trying PRDF name from Run2 PRECO usage \n\n" << endl;
      iWarn = 0;
    }
    n1 = iter.findFirst("PHCompositeNode", "PRDF");
  }  // maintaining backward compatibility
  if (!n1) {
    cout << "\nErtPutDCM<E>: Unable to find SIMPRDF or PRDF subnode, exiting \n\n" << endl;
    exit(1);  
  }
  else {
    prdfNode = static_cast<PHCompositeNode*>(n1);
  }

  // Extract the data from the dTecDCMdata in DCM Node.
  dErtDcmDataWrapper *w;
  j = new PHNodeIterator(dcmNode);
  d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dErtDcmData"));
  if (!d) {
    cout << "\n ErtPutDCM <E>: Unable to find STAF Table dErtDcmData, exiting \n\n" << endl;
    exit(1);
  }
  else {
    w = static_cast<dErtDcmDataWrapper*>(d->getData());
    if (!w) {
      cout << "\n ErtPutDCM <E>: Unable to get pointer for wrapped table, exiting \n\n" << endl;
      exit(1);
    }
  }
  delete j;

  TABLE_HEAD_ST fTableHeader = w->TableHeader();
  Int_t dcmRows = fTableHeader.nok;
  static Int_t dcmRowsLast = 0;

  DERTDCMDATA_ST* dErtDcmData = w->TableData();

  Int_t numberOfWords = dErtDcmData[0].Nwords;
  Int_t dataID = dErtDcmData[0].packetID;
  Int_t bytesPerWord = 4;
  Int_t hitFormat = dErtDcmData[0].hitformat;
  
  if( dcmRows>2 || numberOfWords>135 ) {
    cout << "\n ErtPutDCM <E>: dcmRows = " << dcmRows;
    cout << ",  numberOfWords = " << numberOfWords << endl;
    cout << ";  Limits are 2 and 135; exiting" << endl;
    exit(1);
  }

  static PHRawDataNode **rawDataArray;
  if(iCall == 0) {
    rawDataArray = new PHRawDataNode *[800];
    for(Int_t iRow=0; iRow<800; iRow++) {
      rawDataArray[iRow] = 0;
    }
  }  // initialization of rawDataArray pointers
  iCall++;

  char ertPRDF[11] = "ertPRDF000";  // assumes maximum of 999 rows

  for (Int_t iRow=0; iRow<dcmRows; iRow++){
    numberOfWords = dErtDcmData[iRow].Nwords;
    encodeString(ertPRDF, iRow);
    dataID = dErtDcmData[iRow].packetID;
    hitFormat = dErtDcmData[iRow].hitformat;

    PHDWORD *dPtr = (PHDWORD*)&dErtDcmData[iRow].word[0];
    if(rawDataArray[iRow] == 0) {
      rawDataArray[iRow] = new PHRawDataNode(dPtr, ertPRDF, numberOfWords, dataID, bytesPerWord, hitFormat);
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
