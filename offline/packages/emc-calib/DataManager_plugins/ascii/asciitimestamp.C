#include "asciitimestamp.h"
#include <map>
#include <string>
#include <cstdlib>

//_____________________________________________________________________________
PHTimeStamp
getTimeStamp(const char* cdate)
{
  static std::map<std::string,int> months;

  if (months.empty()) {
    months["Jan"] = 1;
    months["Feb"] = 2;
    months["Mar"] = 3;
    months["Apr"] = 4; 
    months["May"] = 5;
    months["Jun"] = 6;
    months["Jul"] = 7;
    months["Aug"] = 8;
    months["Sep"] = 9;
    months["Oct"] = 10;
    months["Nov"] = 11;
    months["Dec"] = 12;
  }

  std::string thedate(cdate);

  std::string smonth = thedate.substr(4,3);
  std::string sday = thedate.substr(8,2);
  std::string shour = thedate.substr(11,2);
  std::string smin = thedate.substr(14,2);
  std::string ssec = thedate.substr(17,2);
  std::string syear = thedate.substr(20,4);

  int year = atoi(syear.c_str());
  int month = months[smonth];
  int day = atoi(sday.c_str());
  int hour = atoi(shour.c_str());
  int min = atoi(smin.c_str());
  int sec = atoi(ssec.c_str());

  return PHTimeStamp(year,month,day,hour,min,sec,0);
}

//_____________________________________________________________________________
PHTimeStamp
getTimeStamp(std::ifstream& in)
{
  char thedate[200];

  in.getline(thedate,200,'\n');
  return getTimeStamp(thedate);
}
