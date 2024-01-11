#ifndef __COUNTERS_H__
#define __COUNTERS_H__

#include <string>
#include <map>
#include <iostream>

class Counters
{
 public:

  bool add(const char* counterName, int initialvalue=0);

  bool incr(const char* counterName, int byamount=1);

  void print(std::ostream& os = std::cout) const;

  int value(const char* counterName) const;

 private:
  typedef std::map<std::string,int> TMAP;
  TMAP fCounters;
};
#endif
