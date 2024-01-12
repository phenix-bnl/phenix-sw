#ifndef __FLAGREADBACK_H__
#define __FLAGREADBACK_H__

#include <SubsysReco.h>

#include <string>

class FlagSave;
class PHCompositeNode;

class FlagReadBack: public SubsysReco
{
 public:
 FlagReadBack(const std::string &name = "FLAGREADBACK");
 virtual ~FlagReadBack() {}

 int InitRun(PHCompositeNode *topNode);
 int EndRun(const int runno);

 protected:
 FlagSave* flagsave;

};

#endif /* __FLAGREADBACK_H__ */
