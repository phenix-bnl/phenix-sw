#ifndef __RAWDATACHECKDBODBC_H__
#define __RAWDATACHECKDBODBC_H__

#include "BadEvent.h"

#include <string>
#include <list>

class RawDataCheckDBodbc
{

 public:
  RawDataCheckDBodbc();
  virtual ~RawDataCheckDBodbc() {}
  //  void Dump(const int nrows = 0) const;
  void identify(std::ostream& out = std::cout) const;
  void Verbosity(const int i) {verbosity = i;}
  int  CheckAndCreateTable(const std::string &name);
  int  CheckAndCreateTable();
  
  int AddBadEvent(BadEvent *newbad);
  int AddBadEvents(std::list<BadEvent> *badlist);
  void TableName(const std::string &name);

 private:
  int verbosity;
  std::string dbname;
  std::string dbowner;
  std::string dbpasswd;
  std::string table;
  int tableexists;

};

#endif
