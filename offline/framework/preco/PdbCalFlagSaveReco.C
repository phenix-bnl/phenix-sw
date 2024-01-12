#include "PdbCalFlagSaveReco.h"

#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>

#include <PdbCalBankSavev1.h>
#include <recoConsts.h>
#include <FlagSavev1.h>
#include <RunToTime.hh>
#include <getClass.h>

#include <Event.h>
#include <phool.h>
#include <PHTimeStamp.h>

#include <PHIODataNode.h>
#include <PHCompositeNode.h>
#include <PdbBankManager.hh>

#include <cmath>
#include <cstdlib>
#include <iostream>
#include <string>
#include <sstream>

using namespace std;

typedef PHIODataNode <PHObject> PHObjectNode_t;

PdbCalFlagSaveReco::PdbCalFlagSaveReco(const string &name): SubsysReco(name)
{
  return ;
}


int PdbCalFlagSaveReco::InitRun(PHCompositeNode *topNode)
{
  int iret = CreateNodeTree(topNode);
  return iret;

}

int 
PdbCalFlagSaveReco::CreateNodeTree(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *RunNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "RUN"));
  PdbCalBankSave *pdbbanks = findNode::getClass<PdbCalBankSave>(RunNode,"PdbCalBankSave");
  if (! pdbbanks)
    {
      pdbbanks = new PdbCalBankSavev1();
      PHObjectNode_t *PHObjectNode =
	new PHObjectNode_t(pdbbanks, "PdbCalBankSave", "PHObject"); // contain PHObject
      RunNode->addNode(PHObjectNode);
    }  

  FlagSave *flgsv = findNode::getClass<FlagSave>(topNode,"Flags");
  if (! flgsv)
    {
      flgsv = new FlagSavev1();
      PHObjectNode_t *FlagSaveNode =
	new PHObjectNode_t(flgsv, "Flags", "PHObject");
      RunNode->addNode(FlagSaveNode);
    }

  return 0;
}

int PdbCalFlagSaveReco::process_event(PHCompositeNode *topNode)
{
  int iret = 0;

  return iret;
}

int
PdbCalFlagSaveReco::EndRun(const int runno)
{
  Fun4AllServer *se = Fun4AllServer::instance();
  FlagSave* flagsave = findNode::getClass<FlagSave>(se->topNode(), "Flags");
  if (flagsave)
    {
      recoConsts *rc = recoConsts::instance();
      cout << "Saving recoConst Flags: " << endl;
      flagsave->FillFromPHFlag(rc);
      flagsave->identify();
    }
  else
    {
      cout << PHWHERE << " could not save recoConsts" << endl;
    }
  PdbCalBankSave *pdbsave = findNode::getClass<PdbCalBankSave>(se->topNode(), "PdbCalBankSave");
  if (pdbsave)
    {
      PdbBankManager *pb = PdbBankManager::instance();
      map<string, set<int> > banks;
      pb->GetUsedBankRids(banks);

      map<string, set<int> >::const_iterator bankiter;
      for (bankiter = banks.begin(); bankiter != banks.end(); bankiter++)
        {
          set<int>:: const_iterator siter;
          for (siter = (bankiter->second).begin(); siter != (bankiter->second).end(); siter++)
            {
              pdbsave->AddBank(bankiter->first, *siter);
            }
        }
      if (verbosity > 0)
	{
          pdbsave->identify();
	}
      pb->ClearUsedBankRids();
    }
  return 0;
}
