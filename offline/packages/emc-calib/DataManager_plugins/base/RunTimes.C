#include "RunTimes.h"
#include <fstream>

//_____________________________________________________________________________
RunTimes::RunTimes()
  : fFilename("emc_runtimes_initfromfile.tmp.txt"),
    fInit(false),
    fMinRunNumber(0)
{
  // file to look for/from is hard-coded on purpose, as it will only
  // be accurate for a limited amount of time. Should not be archived
  // or whatever.
}

//_____________________________________________________________________________
RunTimes::~RunTimes()
{
  std::map<int, RunLite*>::iterator it;
  for ( it = fTimes.begin(); it != fTimes.end(); it++)
    {
      delete it->second;
      it->second = 0;
    }
}

//_____________________________________________________________________________
void
RunTimes::MinRunNumber(int n)
{
  fMinRunNumber=n;
}

//_____________________________________________________________________________
void
RunTimes::Output()
{
  if ( !fInit ) 
    {
      Init();
    }

  std::cout << "RunTimes::Writing " << fFilename 
	    << " file " << std::endl;

  // write out the file
  std::ofstream out(fFilename.c_str());
  
  std::map<int, RunLite* >::iterator it;

  for ( it = fTimes.begin(); it != fTimes.end(); it++ )
    {
      RunLite* rl = it->second;
      out << rl->Number() << "," << rl->Start() << "," << rl->End() 
	  << std::endl;
    }

  out.close();
}

//_____________________________________________________________________________
void
RunTimes::Reset(void)
{
  std::map<int, RunLite*>::iterator it;
  for ( it = fTimes.begin(); it != fTimes.end(); it++)
    {
      delete it->second;
      it->second = 0;
    }

  fTimes.clear();
}

//_____________________________________________________________________________
const PHTimeStamp
RunTimes::RunEnd(int runNumber)
{
  if (!fInit)
    {
      Init();
    }

  static PHTimeStamp zero(0);

  std::map<int, RunLite* >::iterator it;

  it = fTimes.find(runNumber);

  if ( it == fTimes.end() )
    {
      return PHTimeStamp(0);
    }

  return it->second->End();
}

//_____________________________________________________________________________
void
RunTimes::RunList(const PHTimeStamp& t1, const PHTimeStamp& t2,
                  std::vector<int>& runnumbers)
{
  if (!fInit)
    {
      Init();
    }

  runnumbers.clear();

  std::map<int, RunLite* >::iterator it;

  for ( it = fTimes.begin(); it != fTimes.end(); it++ )
    {
      RunLite* rl = it->second;
      PHTimeStamp start = rl->Start();
      PHTimeStamp end = rl->End();

      if ( start >= t1 && start <= t2 &&
           end <= t2 )
        {
          runnumbers.push_back(it->first);
        }
    }
}

//_____________________________________________________________________________
int
RunTimes::RunNumber(const PHTimeStamp& ts)
{
  if (!fInit)
    {
      Init();
    }

  std::map<int, RunLite* >::iterator it;
  int run = 0;

  for ( it = fTimes.begin(); it != fTimes.end() && run == 0; it++ )
    {
      RunLite* rl = it->second;
      if ( ts >= rl->Start() && ts <= rl->End() )
        {
          run = it->first;
        }
    }
  return run;
}

//___________________________________________________________________________
const PHTimeStamp
RunTimes::RunStart(int runNumber)
{
  if (!fInit)
    {
      Init();
    }

  std::map<int, RunLite* >::iterator it;

  it = fTimes.find(runNumber);

  if ( it == fTimes.end() )
    {
      return PHTimeStamp(0);
    }
  return it->second->Start();
}

//_____________________________________________________________________________
//
//   internal class definition below
//_____________________________________________________________________________


//_____________________________________________________________________________
RunTimes::RunLite::RunLite(int number,
                           const PHTimeStamp& start,
                           const PHTimeStamp& end)
{
  Set(number, start, end);
}

//_____________________________________________________________________________
void
RunTimes::RunLite::Print(void) const
{
  std::cout << "Run " << fNumber << " Start " << fStart
	    << " End " << fEnd << std::endl;
}

//_____________________________________________________________________________
void
RunTimes::RunLite::Set(int number,
                       const PHTimeStamp& start,
                       const PHTimeStamp& end)
{
  fStart = start;
  fEnd = end;
  fNumber = number;
}
