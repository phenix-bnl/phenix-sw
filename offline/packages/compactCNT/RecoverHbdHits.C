#include <RecoverHbdHits.h>
#include "setIntflag.h"

#include <HbdHitMapEntry.h>
#include <HbdHitMap.h>
#include <HbdMiniCellListv2.h>
#include <HbdMiniCellv1.h>
#include <vararray/VariableArray.h>
#include <VariableArrayInt.h>

#include <Fun4AllReturnCodes.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>

#include <cstdlib>
#include <sstream>
#include <fstream>

using namespace std;

union floatint
{
  float    f32;
  int      i32;
};

RecoverHbdHits::RecoverHbdHits(const std::string &name): SubsysReco(name)
{

#ifdef DUMP
  dumprecover.open("/phenix/scratch/frawley/recoverhbdhits.dump");
#endif

  return;
}

int
RecoverHbdHits::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  VariableArray *hbdhit;
  HbdHitMap *hbdmap;
  HbdMiniCellList *minicell;
  hbdhit = findNode::getClass<VariableArray>(topNode, "HbdHit_VarArray");
  if (hbdhit)
    {
      hbdmap  = findNode::getClass<HbdHitMap>(topNode, "HbdMiniCellList_comp");
      if (!hbdmap)
        {
          hbdmap = new HbdHitMap();
	  PHIODataNode<PHObject> *PHObjectIONode = new PHIODataNode<PHObject>(hbdmap, "HbdMiniCellList_comp" , "PHObject");
          dstNode->addNode(PHObjectIONode);
        }

      minicell  = findNode::getClass<HbdMiniCellList>(topNode, "HbdMiniCellList");
      if (!minicell)
        {
          minicell = new HbdMiniCellListv2();
          minicell->SetConvFactor(2.0);
	  PHIODataNode<PHObject> *MiniNode = new PHIODataNode<PHObject>(minicell, "HbdMiniCellList" , "PHObject");
          dstNode->addNode(MiniNode);
        }
    }
  //      hbdhit->identify();
  return EVENT_OK;
}

int
RecoverHbdHits::process_event(PHCompositeNode *topNode)
{
  VariableArray *hitarray = findNode::getClass<VariableArray>(topNode, "HbdHit_VarArray");
  if (hitarray)
    {
      HbdHitMap *hbdmap  = findNode::getClass<HbdHitMap>(topNode, "HbdMiniCellList_comp");
      if (!hbdmap)
        {
          cout << PHWHERE << "Cannot locate output node HbdMiniCellList_comp, return and do nothing!" << endl;
        }

      HbdMiniCellList *minicell  = findNode::getClass<HbdMiniCellList>(topNode, "HbdMiniCellList");
      if (!minicell)
        {
          cout << PHWHERE << "Cannot locate output node HbdMiniCellList, return and do nothing!" << endl;
        }
      unsigned int size = hitarray->get_array_size();
      const short int *array = hitarray->get_array();
      HbdHitMapEntry hbdentry;
      short int id=0;
      while (size > 0)
        {
          hbdentry.set_adcch(*array++);
          size--;
          hbdentry.set_charge(*array++);
          size--;
          hbdmap->AddHit(id, hbdentry);
	  id++;

          int ncell = minicell->get_nCells();
          minicell->AddCell(ncell);
          minicell->set_nCells(ncell+1);
          minicell->get_cell(ncell)->set_adcch(hbdentry.get_adcch());
          minicell->get_cell(ncell)->set_charge(hbdentry.get_charge());
//          cout <<"HitCell: "<<ncell <<", size: " <<size<<endl;
	}

#ifdef DUMP
	  hbdmap->identify(dumprecover);
	  minicell->identify(dumprecover);
#endif
    }
  return EVENT_OK;
}

int
RecoverHbdHits::End(PHCompositeNode *topNode)
{

#ifdef DUMP
  dumprecover.close();
#endif

  return 0;
}
