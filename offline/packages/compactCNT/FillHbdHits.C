#include <FillHbdHits.h>
#include "setIntflag.h"

#include <vararray/VariableArray.h>
#include <VariableArrayInt.h>
#include <HbdMiniCellListv1.h>

#include <Fun4AllReturnCodes.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>

#include <set>
#include <sstream>

using namespace std;

union floatint
{
  float    f32;
  int      i32;
};

FillHbdHits::FillHbdHits(const std::string &name): SubsysReco(name)
{
#ifdef DUMP
  dumpfile.open("/phenix/scratch/frawley/fillhbdhits.dump");
#endif

  return;
}

int
FillHbdHits::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

  // HbdMiniCellList uses only short ints, so in this case we always use short ints to store the dat

  VariableArray *hbdhit = findNode::getClass<VariableArray>(topNode, "HbdHit_VarArray" );
  if (!hbdhit)
    {
      hbdhit = new VariableArray(6000);
      PHIODataNode<PHObject> *PHObjectIONode = new PHIODataNode<PHObject>(hbdhit, "HbdHit_VarArray", "PHObject");
      dstNode->addNode(PHObjectIONode);
    }

  return EVENT_OK;
}

int
FillHbdHits::process_event(PHCompositeNode *topNode)
{
#ifdef DUMP
  dumpfile << "New event for FillHbdHits" << endl;
#endif

  vector<short int> savethis;

  // This is the compactCNT output node
  // We want a short int array for this one
  
  VariableArray *hbdarray;
  hbdarray = findNode::getClass<VariableArray>(topNode, "HbdHit_VarArray");
  
  // Get the HBD input node
  
  HbdMiniCellList *MiniCellList = findNode::getClass<HbdMiniCellList>(topNode, "HbdMiniCellList");
  if (!MiniCellList){
    cout << PHWHERE << "Did not find HbdMiniCellList. Return and do nothing!" << endl;
    return 0;
  }
  
  for(int i=0; i<MiniCellList->get_nCells(); i++)
    {
      HbdMiniCell *icell = (HbdMiniCell*)MiniCellList->get_cell(i);
      
      short int adcch = icell->get_adcch();
      savethis.push_back(adcch);
      short int charge = icell->get_charge();
      savethis.push_back(charge);
      
#ifdef DUMP
      dumpfile << "Saving HbdMiniCellList Entry: " << i;
      dumpfile << " adcch " << adcch
	       << " charge " << charge
	       << endl;
#endif
    }

  // Save the hits for this event to HbdHit_VarArray
  hbdarray->set_val(savethis);

  return EVENT_OK;
}

int
FillHbdHits::End(PHCompositeNode *topNode)
{
#ifdef DUMP
  dumpfile.close();
#endif
  
  return 0;
}
