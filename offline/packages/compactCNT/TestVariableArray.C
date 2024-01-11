#include <PHCompositeNode.h>
#include <TestVariableArray.h>
#include <Fun4AllReturnCodes.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>

#include <vararray/VariableArray.h>

#include <iostream>
#include <sstream>
#include <fstream>

using namespace std;


TestVariableArray::TestVariableArray(const string &name): SubsysReco(name)
{
  T = gsl_rng_mt19937;
  rand = gsl_rng_alloc (T);
  gsl_rng_set (rand, 41074); // set seed
  fout = new ofstream("input.dump");
  return;
}

TestVariableArray::~TestVariableArray()
{
  gsl_rng_free(rand);
  return;
}

void TestVariableArray::Print(const string &what) const
{
  cout << Name() << " did not implement Print method" << endl;
  return;
}

int
TestVariableArray::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (dstNode)
    {
      cout << "Got the DST node" << endl;
    }
  VariableArray *arr = new VariableArray(2001);
  arr->SplitLevel(0);
  PHIODataNode<PHObject> *PHObjectIONode = new PHIODataNode<PHObject>(arr, "VarArray", "PHObject");
  dstNode->addNode(PHObjectIONode);
  return 0;
}

int
TestVariableArray::process_event(PHCompositeNode *topNode)
{
  unsigned int n = (int)(gsl_ran_flat(rand,0.,10000.)) + 1;
  vector<short> nval;
  nval.resize(n);
  for (unsigned int i=0; i<n;i++)
    {
      nval[i] = (int) (gsl_ran_flat(rand,0.,10000.));
    }
  VariableArray *arr = findNode::getClass<VariableArray>(topNode,"VarArray");
  if (arr)
    {
      arr->set_val(nval);
      arr->identify(*fout);
    }
  else
    {
      cout << "could not find WarArray node" << endl;
      exit(1);
    }
  return EVENT_OK;
}

int
TestVariableArray::End(PHCompositeNode *topNode)
{
  delete fout;
  fout = 0;
  return 0;
}
