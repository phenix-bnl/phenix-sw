#include <PdbBankAddNew.h>

#include <PdbCalBankSave.h>
#include <recoConsts.h>

#include <getClass.h>

#include <PdbBankManager.hh>

using namespace std;

PdbBankAddNew::PdbBankAddNew(const std::string &name) : SubsysReco(name)
{
  pdbbanksave = 0;
  return;
}

int
PdbBankAddNew::InitRun(PHCompositeNode *topNode)
{
  pdbbanksave = findNode::getClass<PdbCalBankSave>(topNode, "PdbCalBankSave");
  int iret = 0;
  if (!pdbbanksave)
    {
      iret = -1;
    }
  return iret;
}

int
PdbBankAddNew::EndRun(const int runno)
{
  if (pdbbanksave)
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
              pdbbanksave->AddBank(bankiter->first, *siter);
            }
        }
    }
  return 0;
}

