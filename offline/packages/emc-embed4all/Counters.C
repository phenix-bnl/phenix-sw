#include "Counters.h"

//_____________________________________________________________________________
bool
Counters::add(const char* counterName, int initialvalue)
{
  TMAP::const_iterator it = fCounters.find(counterName);

  if ( it != fCounters.end() )
    {
      return false;
    }
  else
    {
      fCounters[counterName]=initialvalue;
      return true;
    }
}

//_____________________________________________________________________________
bool
Counters::incr(const char* counterName, int byamount)
{
   TMAP::iterator it = fCounters.find(counterName);
   if ( it != fCounters.end() )
     {
       it->second += byamount;
       return true;
     }
   return false;
}

//_____________________________________________________________________________
void
Counters::print(std::ostream& os) const
{
  TMAP::const_iterator it;
  for ( it = fCounters.begin(); it != fCounters.end(); ++it ) 
    {
      os << it->first << " : " << it->second << std::endl;
    }
}

//_____________________________________________________________________________
int
Counters::value(const char* counterName) const
{
  TMAP::const_iterator it = fCounters.find(counterName);
  if ( it != fCounters.end() )
    {
      return it->second;
    }
  else
    {
      std::cerr << "Counters::value : counter " << counterName
		<< " does not exist." 
		<< std::endl;
      return -1;
    }
}
