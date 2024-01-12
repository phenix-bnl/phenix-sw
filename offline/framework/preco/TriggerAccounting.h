#ifndef TRIGGERACCOUNTING_H__
#define TRIGGERACCOUNTING_H__

#include <SubsysReco.h>

#include <map>
#include <set>
#include <string>

class TriggerAccounting: public SubsysReco
{
 public:
  TriggerAccounting(const std::string &name = "TRIGGERACCOUNTING");
  virtual ~TriggerAccounting();

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int EndRun(const int runno);
  int End(PHCompositeNode *topNode);

 protected:
  int GetConnection();
  int CheckAndCreateTable(const std::string &tablename);
  int CheckAndCreateIndexTable(const std::string &tablename);
  int InsertNewRow(const std::string &tablename, const std::string &filename, const int runnumber);
  int AddTriggers(const std::string &tablename, const std::string &filename, const int runnumber);
  int AddEventTypes(const std::string &tablename, const std::string &filename, const int runnumber);
  int FillIndexTable(std::map<std::string, std::pair<std::string,int> > &map, const std::string &prefix);
  int AddMissingColumns(const std::map<std::string, std::pair<std::string,int> > &map, const std::string &tablename);
  std::string indextable;
  std::map<int, int> eventtypes;
  std::map<int, std::string> eventTypeNames;
  std::string trigname[32];
  std::map<std::string, std::pair<std::string,int> > evttypesfired;
  std::map<std::string, std::pair<std::string,int> > trigfired;
  std::set<int> runs;
  unsigned int itrig[32];
};

#endif /* __TRIGGERACCOUNTING_H__ */


