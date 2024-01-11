#ifndef __RUNTIMES_H__
#define __RUNTIMES_H__

#include <vector>
#include <map>
#include <string>

#include "PHTimeStamp.h"

/** Utility class to make a map of run numbers to run validity ranges. 
 */

class RunTimes
{
public:

  RunTimes();
  virtual ~RunTimes();

  void MinRunNumber(int minrunnumber);
  int MinRunNumber() const { return fMinRunNumber; }

  void Output();

  void Reset(void);

  const PHTimeStamp RunEnd(int runNumber);

  void RunList(const PHTimeStamp& t1, const PHTimeStamp& t2,
	       std::vector<int>& runnumbers);

  int RunNumber(const PHTimeStamp& ts);

  const PHTimeStamp RunStart(int runNumber);

protected:

  virtual bool Init() { return false; }

  class RunLite
  {

  public:
    RunLite(int number, const PHTimeStamp& start, const PHTimeStamp& end);

    const PHTimeStamp End(void) const
    {
      return fEnd;
    }

    int Number(void) const
    {
      return fNumber;
    }

    void Print(void) const;

    const PHTimeStamp Start(void) const
    {
      return fStart;
    }

    void Set(int number, const PHTimeStamp& start, const PHTimeStamp& end);

  public:
    PHTimeStamp fStart;
    PHTimeStamp fEnd;
    int fNumber;
  };

  std::map<int, RunLite* > fTimes;
  std::string fFilename;
  bool fInit;
  int fMinRunNumber;
};

#endif

