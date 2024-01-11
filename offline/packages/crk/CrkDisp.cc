#include "TNode.h"
#include "TBRIK.h"

//INCLUDECHECKER: Removed this line: #include "CrkDisp.h"

void test_disp(TBRIK** pb) {
  TBRIK *brik1 = new TBRIK("BRIK1","ahaha","void",100,100,100);
  TNode *node1 = new TNode("NODE1","hehehe","BRIK1");
  node1->Draw();
  *pb = brik1;
}

