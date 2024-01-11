#include "mBbcOutEvalModule.h"
#include "headerWrapper.h"
#include "dBbcOutWrapper.h"
#include "dBbcEvalWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNode.h"
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"
#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNodeIterator.h"
//INCLUDECHECKER: Removed this line: #include "PHTable.hh"

#include <iostream>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

PHBoolean
mBbcOutEvalModule::event(PHCompositeNode *root) 
{
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 TableNode_t *d;
 PHCompositeNode *dstNode, *outNode, *evaNode;

 evaNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode","EVA"));
 if (!evaNode) 
   {
     evaNode = new PHCompositeNode("EVA");
     root->addNode(evaNode);
   }

 dstNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DST"));
 if (!dstNode) 
   {
     dstNode = new PHCompositeNode("DST");
     root->addNode(dstNode);
   }

  headerWrapper* HeaderWrapper;

  outNode = evaNode;
  j = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","header"));
  if (!d) 
    {
      cout << "ERROR:  'in' parameter header not found" << endl;
      HeaderWrapper = new headerWrapper("header", 10);
      if (!HeaderWrapper) 
	{
	  return 1;
	}
    d = new TableNode_t(HeaderWrapper,"header"); 
    outNode->addNode(d);
    }
  else 
    {
      HeaderWrapper = static_cast<headerWrapper*>(d->getData());
      if (!HeaderWrapper) 
	{
	  return 1;
	}
    }
  delete j;
  
  HEADER_ST* headerTable = HeaderWrapper->TableData();

  dBbcOutWrapper* BbcOutWrapper;

  outNode = dstNode;
  j = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dBbcOut"));
  if (!d) {
     cout << " 'out' parameter dBbcOut was created at this moment" << endl;
     BbcOutWrapper = new dBbcOutWrapper("dBbcOut", 1);
     if (!BbcOutWrapper) {
       return 1;
     }
     d = new TableNode_t(BbcOutWrapper,"dBbcOut");
     outNode->addNode(d);
  }
  else {
    BbcOutWrapper = static_cast<dBbcOutWrapper*>(d->getData());
    if (!BbcOutWrapper) {
       return 1;
    }
  }
  delete j;
  nodes.append(d);

  DBBCOUT_ST* bbcout = BbcOutWrapper->TableData();

  dBbcEvalWrapper* BbcEvalWrapper;

  outNode = evaNode;
  j = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dBbcEval"));
  if (!d) {
     cout << " 'out' parameter dBbcEval was created at this moment" << endl;
     BbcEvalWrapper = new dBbcEvalWrapper("dBbcEval", 1);
     if (!BbcEvalWrapper) {
       return 1;
     }
     d = new TableNode_t(BbcEvalWrapper,"dBbcEval");
     outNode->addNode(d);
  }
  else {
    BbcEvalWrapper = static_cast<dBbcEvalWrapper*>(d->getData());
    if (!BbcEvalWrapper) {
       return 1;
    }
  }
  delete j;
  nodes.append(d);

  DBBCEVAL_ST* bbceval = BbcEvalWrapper->TableData();

  bbceval->NHitPmtSouth   = bbcout->NhitPmtNorth;
  bbceval->NHitPmtNorth   = bbcout->NhitPmtSouth;
  bbceval->ChargeSumNorth = bbcout->ChargeSumNorth;
  bbceval->ChargeSumSouth = bbcout->ChargeSumSouth;
  bbceval->b              = headerTable->b;
  bbceval->TimeZero       = bbcout->TimeZero;
  bbceval->ZVertexCal     = bbcout->VertexPoint;
  bbceval->ZVertexSim     = headerTable->vertex[2];

  cout << bbceval->b << " " << headerTable->b << endl;
  cout << " NHitPmtSouth        = " << bbceval->NHitPmtSouth << endl;
  cout << " NHitPmtNorth        = " << bbceval->NHitPmtNorth << endl;
  cout << " Simulated  Z vertex = " << bbceval->ZVertexSim << endl;
  cout << " Calculated Z vertex = " << bbceval->ZVertexCal << endl; 

  return True;
}
