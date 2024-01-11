//
// Original author: Charles F. Maguire
// Creation date: June 9, 2002
//
// Purpose: Do random removal of pcXghit to account for
//          HV channel dropouts during Run2
//

#include "pcghitWrapper.h"
#include "dPadRawWrapper.h"
#include "dPadRawClusWrapper.h"
#include "dPadGhitRawWrapper.h"
#include "dPadNibbleGhitWrapper.h"
#include "dPadGhitClusWrapper.h"
#include "dPadDCMWrapper.h"
#include "dPadClusterWrapper.h"
#include "PadTableSet.h"

#include "PHString.h"
#include "phool.h"
#include "PHIODataNode.h"
#include "PHCompositeNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include <cstdlib>
#include <iostream>

using namespace std;

void dInputError(const PHString &inputName);
void dOutputError(const PHString &outputName);

void wInputError(const PHString &inputName);
void wOutputError(const PHString &outputName);

static Int_t inputWarn = 0;
const Int_t MAXWARN = 10;
static Int_t iFirst = 1;
void checkInput(Int_t inputRows);

typedef PHIODataNode<PHTable> TableNode_t;

long
PadTableSet(PHCompositeNode* topNode, const Float_t& pc1EastEffic, const Float_t& pc1WestEffic,
            const Float_t& pc2WestEffic, const Float_t& pc3EastEffic, const Float_t& pc3WestEffic,
            const Int_t& modePc2West, const Float_t& pc2WestEffic2)
{

  if(iFirst==1) {
    cout << "\n\n PadTableSet <I>: Simulation correction for random HV channel dropouts during Run2" << endl;
    cout << "  pc1EastEfficency = " << pc1EastEffic << endl;
    cout << "  pc1WestEfficency = " << pc1WestEffic << endl;
    cout << "  pc2WestEfficency = " << pc2WestEffic << endl;
    cout << "  pc3EastEfficency = " << pc3EastEffic << endl;
    cout << "  pc3WestEfficency = " << pc3WestEffic << endl;
    if(modePc2West==1) 
      cout << " second pc2WestEfficency = " << pc2WestEffic2 << endl;
    cout << endl;
  }
  PHNodeIterator iter(topNode);
  PHCompositeNode* subNode;
  PHNode *node;                
  TableNode_t *dInput;

  // Find the subNode "directory" which must exist already

  PHString subName = "GEA";
  node = iter.findFirst("PHCompositeNode", subName);
  if (!node) {
    cout << "\n PadTableSet: unable to find subNode GEA" << endl;
    exit(1);
  }
  subNode = static_cast<PHCompositeNode*>(node);

  PHNodeIterator *j = new PHNodeIterator(subNode);

  PHString inputName = "pc1ghit";
  dInput = static_cast<TableNode_t*>(j->findFirst("PHIODataNode",inputName));  
  if (!dInput) dInputError(inputName);

  delete j;

  //
  // GEA subnode has only the GEANT hits information
  //

  pcghitWrapper* wInput = static_cast<pcghitWrapper*>(dInput->getData());
  if(!wInput) wInputError("pc1ghit");

  TABLE_HEAD_ST fTableHeader = wInput->TableHeader();
  Int_t inputRows = fTableHeader.nok;
  checkInput(inputRows);

  return 0;
}

void dInputError(const PHString &inputName ){
  cout << "\n PadTableSet: unable to find input node name " << inputName << endl;
  exit(3);
}

void wInputError(const PHString &inputName ){
  cout << "\n PadTableSet: unable to find input wrapper name " << inputName << endl;
  exit(5);
}

void checkInput(Int_t inputRows) {
  if(inputRows < 1 && inputWarn < MAXWARN ) {
    inputWarn++;
    cout << "\n Number of input rows = " << inputRows << endl;
    if(inputWarn == MAXWARN) cout << " This is the last warning " << endl;
    cout << "\n";
  }
}

