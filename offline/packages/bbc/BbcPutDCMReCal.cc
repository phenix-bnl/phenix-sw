#include "dBbcDCMWrapper.h"

#include <encodeString.h>

#include <PHIODataNode.h>
#include <PHRawDataNode.h>

#include <cstdlib>
#include <iostream>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

long
BbcPutDCMReCal(PHCompositeNode* topNode)
{
  PHNodeIterator iter(topNode), *j;
  PHCompositeNode* dcmNode;
  PHCompositeNode* prdfNode;
  PHNode *n1;
  TableNode_t *d;

  static Int_t iCall = 0;   // used for initialization of raw data pointers

  // Find the DCM Node.
  n1 = iter.findFirst("PHCompositeNode", "DCM");
  if (!n1) {
    cout << "\n BbcPutDCMreCal <E>: Unable to find DCM subnode, exiting \n\n" << endl;
    exit(1);
  }
  else {
    dcmNode = static_cast<PHCompositeNode*>(n1);
  }

  //Find the PRDF node
  n1 = iter.findFirst("PHCompositeNode", "PRDFRECAL");
  if (!n1) {
    cout << "\n BbcPutDCMreCal <E>: Unable to find PRDFRECAL subnode, exiting \n\n" << endl;
    exit(1);  
  }
  else {
    prdfNode = static_cast<PHCompositeNode*>(n1);
  }

  // Extract the data from the dBbcDCMReCal in DCM Node.
  dBbcDCMWrapper *w;
  j = new PHNodeIterator(dcmNode);
  d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dBbcDCMReCal"));
  if (!d) {
    cout << "\n BbcPutDCMReCal <E>: Unable to find STAF Table dBbcDCMReCal, exiting \n\n" << endl;
    exit(1);
  }
  else {
    w = static_cast<dBbcDCMWrapper*>(d->getData());
    if (!w) {
      cout << "\n BbcPutDCMReCal <E>: Unable to get pointer for wrapped table, exiting \n\n" << endl;
      exit(1);
    }
  }
  delete j;

  TABLE_HEAD_ST fTableHeader = w->TableHeader();
  Int_t dcmRows = fTableHeader.nok;
  static Int_t dcmRowsLast = 0;

  DBBCDCM_ST* dBbcDCM = w->TableData();

  Int_t numberOfWords = dBbcDCM[0].nWord;
  Int_t bytesPerWord = 4;
  
  if( dcmRows>1 || numberOfWords>408 ) {
    cout << "\n BbcPutDCMLReCal <E>: dcmRows = " << dcmRows;
    cout << ",  numberOfWords = " << numberOfWords << endl;
    cout << ";  Limits are 1 and 408; exiting" << endl;
    exit(1);
  }

  static PHRawDataNode **rawDataArray;
  if(iCall == 0) {
    cout << "BbcPutDCMReCal: Initializing rawDataArray pointer on first event" 
	 << endl; 
    rawDataArray = new PHRawDataNode *[1];
    for(Int_t iRow=0; iRow<1; iRow++) {
      rawDataArray[iRow] = 0;
    }
  }  // initialization of rawDataArray pointers
  iCall++;

  char bbcPRDF[11] = "bbcPRDF000";  // assumes maximum of 999 rows

  for (Int_t iRow=0; iRow<dcmRows; iRow++){
    numberOfWords = dBbcDCM[iRow].nWord;
    encodeString(bbcPRDF, iRow);
    Int_t dataID = dBbcDCM[iRow].packetID;
    Int_t hitFormat = dBbcDCM[iRow].scheme;

    PHDWORD *dPtr = (PHDWORD*)&dBbcDCM[iRow].DCM[0];
    if(rawDataArray[iRow] == 0) {
      cout << "BbcPutDCMReCal: rawDataArray[0] =0, initialize it" << endl; 
      rawDataArray[iRow] = new PHRawDataNode(dPtr, bbcPRDF, numberOfWords, dataID, bytesPerWord, hitFormat);
      prdfNode->addNode(rawDataArray[iRow]);
    }   
    else {
      cout << "BbcPutDCMReCal: rawDataArray[0] initialized, write to it"<<endl; 
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
