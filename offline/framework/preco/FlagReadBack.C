#include <FlagReadBack.h>

#include <FlagSave.h>
#include <recoConsts.h>

#include <getClass.h>

using namespace std;

FlagReadBack::FlagReadBack(const std::string &name) : SubsysReco(name)
{
  flagsave = 0;
  return;
}

int
FlagReadBack::InitRun(PHCompositeNode *topNode)
{
  flagsave = findNode::getClass<FlagSave>(topNode, "Flags");
  int iret = 0;
  if (flagsave)
    {
      recoConsts *rc = recoConsts::instance();
      iret = flagsave->PutFlagsBack(rc);
    }
  return iret;
}

int
FlagReadBack::EndRun(const int runno)
{
  if (flagsave)
    {
      recoConsts *rc = recoConsts::instance();
      flagsave->FillFromPHFlag(rc);
    }
  return 0;
}

