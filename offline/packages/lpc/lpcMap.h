#ifndef __LPCMAP_H__
#define __LPCMAP_H__

#include <PHTimeStamp.h>
#include <PdbBankID.hh>
#include <string>

class lpcMap
{
 public:
  lpcMap();
  virtual ~lpcMap();
  int *FillChannelMap(const PHTimeStamp &ts, unsigned int &size);
  int FillMyArray(const PHTimeStamp &ts);
  void SetArray(const int *array, const unsigned int size);
  void Print();
  void SetTimeStart(PHTimeStamp &ts) {TStart = ts;}
  void SetTimeStop(PHTimeStamp &ts) {TStop = ts;}
  int Commit();
  int verify_readback(); // check if zdcchannelmap is fit for DB storage
  void Description(const std::string &desc) {description = desc;}
 
 protected:

  std::string description;
  PHTimeStamp TStart;
  PHTimeStamp TStop;
  int *lpcchannelmap;
  unsigned int lpcchansize;
};

#endif
