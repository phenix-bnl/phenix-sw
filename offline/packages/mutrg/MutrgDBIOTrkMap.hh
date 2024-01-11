#ifndef __MUTRGDBIOTRKMAP__
#define __MUTRGDBIOTRKMAP__

#include <vector>
#include <string>
#include <iostream>

#include "PHTimeStamp.h"
#include "MutrgPar.hh"

class PdbMutrgRpcTrk;

class MutrgDBIOTrkMap{
public:
  MutrgDBIOTrkMap();
  virtual ~MutrgDBIOTrkMap();

  void Reset();

  int ReadFile(char *filename);
  int ReadDB();
  int WriteDB();

  void SetVersion(MutrgPar::TrkDBVersion ver){version=ver;}
  void SetDescription(const char *desc){description=desc;}
  void SetUserName(const char *name){user_name=name;}
  void SetTimeStart(int year,int mon,int day,int hour,int min,int sec);
  void SetTimeStart(PHTimeStamp ts){time_start=ts;}
  void SetTimeEnd(int year,int mon,int day,int hour,int min,int sec);
  void SetTimeEnd(PHTimeStamp ts){time_end=ts;}
  void SetTimeSearch(int year,int mon,int day,int hour,int min,int sec);
  void SetTimeSearch(PHTimeStamp ts){time_search=ts;}

  const char* GetClassName(void){return class_name.c_str();}
  const char* GetCalibName(void){return calib_name.c_str();}
  MutrgPar::TrkDBVersion GetVersion(void){return version;}
  const char* GetDescription(void){return description.c_str();}
  const char* GetUserName(void){return user_name.c_str();}
  PHTimeStamp GetTimeInsert(void){return time_insert;}
  PHTimeStamp GetTimeStart(void){return time_start;}
  PHTimeStamp GetTimeEnd(void){return time_end;}
  PHTimeStamp GetTimeSearch(void){return time_search;}
  const std::vector<PdbMutrgRpcTrk*>& GetMap(){return mutrg_map;}

  void print(std::ostream& os=std::cout, std::string delim=" ") const;

protected:
  std::vector<PdbMutrgRpcTrk*> mutrg_map;

  static const std::string class_name;
  static const std::string calib_name;

  MutrgPar::TrkDBVersion version;
  std::string description;
  std::string user_name;
  PHTimeStamp time_insert;
  PHTimeStamp time_start;
  PHTimeStamp time_end;
  PHTimeStamp time_search;
};

#endif /* __MUTRGDBIOTRKMAP__ */
