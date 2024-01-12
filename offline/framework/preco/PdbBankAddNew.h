#ifndef __PDBBANKADDNEW_H__
#define __PDBBANKADDNEW_H__

#include <SubsysReco.h>

#include <string>

class PdbCalBankSave;
class PHCompositeNode;

class PdbBankAddNew: public SubsysReco
{
 public:
 PdbBankAddNew(const std::string &name = "PDBBANKADDNEW");
 virtual ~PdbBankAddNew() {}

 int InitRun(PHCompositeNode *topNode);
 int EndRun(const int runno);

 protected:
 PdbCalBankSave* pdbbanksave;

};

#endif /* __PDBBANKADDNEW_H__ */
