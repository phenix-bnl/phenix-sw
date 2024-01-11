#include "RunTimesFactory.h"
#include <cstdlib>

/** Ascii implementation of RunTimes. */

class RunTimesAscii : public RunTimes
{
protected:
  virtual bool Init();
};

namespace
{
  RunTimes* creator()
  {
    return new RunTimesAscii;
  }

  const std::string name = "Ascii";
  const bool registered =
  RunTimesFactory::instance().registerCreator(name,
					      creator,
					      "RunTimes");


  PHTimeStamp
  BuildTimeStamp(const std::string& thedate)
  {
    static std::map<std::string, int> months;

    if (months.empty())
      {
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

    std::string smonth = thedate.substr(4, 3);
    std::string sday = thedate.substr(8, 2);
    std::string shour = thedate.substr(11, 2);
    std::string smin = thedate.substr(14, 2);
    std::string ssec = thedate.substr(17, 2);
    std::string syear = thedate.substr(20, 4);

    int year = atoi(syear.c_str());
    int month = months[smonth];
    int day = atoi(sday.c_str());
    int hour = atoi(shour.c_str());
    int min = atoi(smin.c_str());
    int sec = atoi(ssec.c_str());

    return PHTimeStamp(year, month, day, hour, min, sec, 0);
  }

}

#include "emcDefines.h"
#include <fstream>

using namespace std;

//_____________________________________________________________________________
bool
RunTimesAscii::Init()
{
  ifstream in(fFilename.c_str());
  if (!in)
    {
      cerr << EMC_ERROR_MSG << "RunTimes::InitFromFile : cannot open "
	   << fFilename << endl;
      return false;
    }

  int n = 0;

  PHTimeStamp future(2010, 1, 1, 0, 0, 0, 0);

  char s[80];
  string str;

  while ( in.getline(s, 80, '\n') )
    {

      str = s;

      if (str[0] == '#')
        continue; // skip comment lines

      size_t p1 = str.find(',');
      size_t p2 = str.find_last_of(',');

      string srun = str.substr(0, p1);
      string st1 = str.substr(p1 + 1, p2 - p1 - 1);
      string st2 = str.substr(p2 + 1);

      PHTimeStamp t1, t2;
      t1 = BuildTimeStamp(st1);
      t2 = BuildTimeStamp(st2);
      int run = atoi(srun.c_str());
      //    if (fVerbose) cout << t1 << " | " << t2 << endl;
      fTimes[run] = new RunLite(run, t1, t2);
      //    if (fVerbose) fTimes[run]->Print();
      n++;
    }

  in.close();

  cout << EMC_INFO_MSG << "RunTimes::InitFromFile : Successfully read "
  << n << " runs from file " << fFilename << endl;

  fInit = true;

  return true;
}
