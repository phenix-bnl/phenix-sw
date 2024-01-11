#include "dPadDCMWrapper.h"
#include "PadPutDCM.h"

#include "encodeString.h"

#include "phool.h"
#include "Event.h"
#include "PHIODataNode.h"
#include "PHCompositeNode.h"
#include "PHNodeIterator.h"
#include "PHRawOManager.h"
#include "PHRawDataNode.h"

#include "packetConstants.h"

#include <cstdlib>
#include <iostream>

using namespace std;

typedef PHDataNode<Event> EventNode_t;
typedef PHIODataNode<PHTable> TableNode_t;

long
PadPutDCM(PHCompositeNode* topNode, Int_t pcNumber)
{
  const char* tableName[3] = {"dPc1DCM","dPc2DCM","dPc3DCM"};

  if(pcNumber<0 || pcNumber>2) {
    cout << "\n PadPutDCM <E>: invalid pcNumber = " << pcNumber << endl;
    exit(1);
  }

  PHNodeIterator iter(topNode), *j;
  PHCompositeNode* dcmNode;
  PHCompositeNode* prdfNode;
  PHNode *n1;
  TableNode_t *d;

  static Int_t iCall = 0;   // used for initialization of rawData pointers

  // Find the DCM Node.
  n1 = iter.findFirst("PHCompositeNode", "DCM");
  if (!n1) {
    cout << "\n PadPutDCM <E>: Unable to find DCM subnode, exiting \n\n" << endl;
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
      cout << "\n PadPutDCM <E>: Unable to find SIMPRDF subnode, trying PRDF name from Run2 PRECO usage \n\n" << endl;
      iWarn = 0;
    }
    n1 = iter.findFirst("PHCompositeNode", "PRDF");
  }  // maintaining backward compatibility
  if (!n1) {
    cout << "\n PadPutDCM <E>: Unable to find SIMPRDF or PRDF subnode, exiting \n\n" << endl;
    exit(1);  
  }
  else {
    prdfNode = static_cast<PHCompositeNode*>(n1);
  }

  // Extract the data from the dPadDCM in DCM Node.
  dPadDCMWrapper *w;
  j = new PHNodeIterator(dcmNode);
  d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode",tableName[pcNumber]));
  if (!d) {
    cout << "\n PadPutDCM <E>: Unable to find STAF Table " << tableName[pcNumber];
    cout << ";  exiting \n\n" << endl;
    exit(1);
  }
  else {
    w = static_cast<dPadDCMWrapper*>(d->getData());
    if (!w) {
      cout << "\n PadPutDCM <E>: Unable to get pointer for wrapped table, exiting \n\n" << endl;
      exit(1);
    }
  }
  delete j;

  TABLE_HEAD_ST fTableHeader = w->TableHeader();
  Int_t dcmRows = fTableHeader.nok;
  static Int_t dcmRowsLast0 = 0;
  static Int_t dcmRowsLast1 = 0;
  static Int_t dcmRowsLast2 = 0;

  DPADDCM_ST* dPadDCM = w->TableData();

  Int_t numberOfWords = 132;  // this was hardwired in the STAF mPadDCMoutput
  Int_t dataID = 4000 + pcNumber*32;  // also hardwired in mPadDCMoutput
  Int_t bytesPerWord = 4;
  Int_t hitFormat = IDPC_FPGA;  // new version, March 20, 2000 (see PC listserver) 
  
  if( dcmRows>32 || numberOfWords>132 ) {
    cout << "\n PadPutDCM <E>: dcmRows = " << dcmRows;
    cout << ",  numberOfWords = " << numberOfWords << endl;
    cout << ";  Limits are 32 and 132; exiting" << endl;
    exit(1);
  }

  static PHRawDataNode **rawDataArray0;
  static PHRawDataNode **rawDataArray1;
  static PHRawDataNode **rawDataArray2;
  if(iCall == 0) {
    rawDataArray0 = new PHRawDataNode *[32];
    rawDataArray1 = new PHRawDataNode *[32];
    rawDataArray2 = new PHRawDataNode *[32];
    for(Int_t iRow=0; iRow<32; iRow++) {
      rawDataArray0[iRow] = 0;
      rawDataArray1[iRow] = 0;
      rawDataArray2[iRow] = 0;
    }
  }  // initialization of rawDataArray pointers
  iCall++;

  char padPRDF[11] = "pc1PRDF000";  // assumes maximum of 999 rows
  padPRDF[2] = 49 + pcNumber;       // set as 1, 2, or 3

  for (Int_t iRow=0; iRow<dcmRows; iRow++){
    encodeString(padPRDF, iRow);
    dataID++;    // starts at 4001 for PC1, 4101 for PC2, 4201 for PC3

    PHDWORD *dPtr = (PHDWORD*)&dPadDCM[iRow].Word[0];
    if(pcNumber == 0){
      if(rawDataArray0[iRow] == 0) {
        rawDataArray0[iRow] = new PHRawDataNode(dPtr, padPRDF, numberOfWords, dataID, bytesPerWord, hitFormat);
        prdfNode->addNode(rawDataArray0[iRow]);
      }   
      else {
        PHRawDataNode *rPtr = rawDataArray0[iRow];
        rPtr->setData(dPtr);
        rPtr->setLength(numberOfWords);
        rPtr->setID(dataID);
        rPtr->setWordLength(bytesPerWord);
        rPtr->setHitFormat(hitFormat);
      }  // check on rawDataArray0 being initialized
    } // check on PC1

    if(pcNumber == 1){
      if(rawDataArray1[iRow] == 0) {
        rawDataArray1[iRow] = new PHRawDataNode(dPtr, padPRDF, numberOfWords, dataID, bytesPerWord, hitFormat);
        prdfNode->addNode(rawDataArray1[iRow]);
      }   
      else {
        PHRawDataNode *rPtr = rawDataArray1[iRow];
        rPtr->setData(dPtr);
        rPtr->setLength(numberOfWords);
        rPtr->setID(dataID);
        rPtr->setWordLength(bytesPerWord);
        rPtr->setHitFormat(hitFormat);
      }  // check on rawDataArray1 being initialized
    } // check on PC2

    if(pcNumber == 2){
      if(rawDataArray2[iRow] == 0) {
        rawDataArray2[iRow] = new PHRawDataNode(dPtr, padPRDF, numberOfWords, dataID, bytesPerWord, hitFormat);
        prdfNode->addNode(rawDataArray2[iRow]);
      }   
      else {
        PHRawDataNode *rPtr = rawDataArray2[iRow];
        rPtr->setData(dPtr);
        rPtr->setLength(numberOfWords);
        rPtr->setID(dataID);
        rPtr->setWordLength(bytesPerWord);
        rPtr->setHitFormat(hitFormat);
      }  // check on rawDataArray2 being initialized
    } // check on PC3
  } // loop over dcmRows

  if(pcNumber == 0){
    if(iCall > 1 && dcmRows < dcmRowsLast0){
      for(Int_t iRow=dcmRows; iRow<dcmRowsLast0; iRow++) {
        PHRawDataNode *rPtr = rawDataArray0[iRow];
        rPtr->setLength(0);
        rPtr->setID(0);
      }  // zero out excess rows
    }
    dcmRowsLast0 = dcmRows;  // save for next event
  }  // check on PC1

  if(pcNumber == 1){
    if(iCall > 1 && dcmRows < dcmRowsLast1){
      for(Int_t iRow=dcmRows; iRow<dcmRowsLast1; iRow++) {
        PHRawDataNode *rPtr = rawDataArray1[iRow];
        rPtr->setLength(0);
        rPtr->setID(0);
      }  // zero out excess rows
    }
    dcmRowsLast1 = dcmRows;  // save for next event
  }  // check on PC2

  if(pcNumber == 2){
    if(iCall > 1 && dcmRows < dcmRowsLast2){
      for(Int_t iRow=dcmRows; iRow<dcmRowsLast2; iRow++) {
        PHRawDataNode *rPtr = rawDataArray2[iRow];
        rPtr->setLength(0);
        rPtr->setID(0);
      }  // zero out excess rows
    }
    dcmRowsLast2 = dcmRows;  // save for next event
  }  // check on PC3

  return 0;
}
