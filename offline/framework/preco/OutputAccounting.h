#ifndef __OUTPUTACCOUNTING_H__
#define __OUTPUTACCOUNTING_H__

#include <SubsysReco.h>

#include <map>
#include <set>
#include <string>
#include <vector>

class OutputAccounting: public SubsysReco
{
 public:
  OutputAccounting(const std::string &name = "OUTPUTACCOUNTING");
  virtual ~OutputAccounting();

  int End(PHCompositeNode *topNode);

 protected:
  int GetConnection();
  int CheckAndCreateTable(const std::string &tablename);
  int InsertNewRow(const std::string &tablename, const int runnumber, const int segment, const std::string &filename, const int nevents);
};

#endif /* __OUTPUTACCOUNTING_H__ */


