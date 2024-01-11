#include "emcQAFEM.h"
#include "emcDefines.h"
#include "emcCalFEMFactory.h"
#include <cassert>
#include <iostream>
#include <iomanip>
#include <map>
#include <vector>
#include <iterator>
#include <string>

using namespace std;

namespace
{
  static string name = "emcQAFEM";
  static string title = "Q&A calibration data";
  static string classname = "emcQAFEM";

  emcCalFEM* creator(int absPosition,
		     const PHTimeStamp& start,
		     const PHTimeStamp& end,
		     bool isDefault)
  {
    if ( isDefault )
      {
	return emcQAFEM::Default(absPosition,start,end);
      }
    else
      {
	return new emcQAFEM(absPosition,start,end);
      }
  }

  static bool r = emcCalFEMFactory::registerCreator("QAs",
						    creator);

}

//_____________________________________________________________________________
emcQAFEM::emcQAFEM(int absPosition)
  : emcCalFEM(absPosition)
{
  NameIt(name, title, classname);
  First();
}

//_____________________________________________________________________________
emcQAFEM::emcQAFEM(int absPosition,
                   const PHTimeStamp& t1, const PHTimeStamp& t2)
  : emcCalFEM(absPosition, t1, t2)
{
  NameIt(name, title, classname);
  First();
}

//_____________________________________________________________________________
emcQAFEM::emcQAFEM(const emcQAFEM& o)
  : emcCalFEM(o.AbsolutePosition())
{
  o.Copy(*this);
  First();
}

//_____________________________________________________________________________
emcQAFEM&
emcQAFEM::operator=(const emcQAFEM& o)
{
  if ( this == &o )
    return * this;
  Reset();
  o.Copy(*this);
  First();
  return *this;
}

//_____________________________________________________________________________
void
emcQAFEM::Copy(emcQAFEM& o) const
{
  emcCalFEM::Copy(o);
  o.Reset();

  map<int, vector<INT32> >::const_iterator p;

  for ( p = fQA.begin(); p != fQA.end(); p++)
    {
      o.fQA[p->first] = p->second;
    }
}

//_____________________________________________________________________________
emcQAFEM::~emcQAFEM()
{
  Reset();
}

//_____________________________________________________________________________
void emcQAFEM::AppendOneChannel(int channel, INT32 error, INT32 warning)
{
  assert(channel >= 0 && channel < 144);
  vector<INT32> one;
  one.push_back(error);
  one.push_back(warning);
  fQA[channel] = one;
}

//_____________________________________________________________________________
emcQAFEM*
emcQAFEM::Default(int absPosition,
                  const PHTimeStamp& tStart, const PHTimeStamp& tEnd)
{
  emcQAFEM* fem = new emcQAFEM(absPosition, tStart, tEnd);

  size_t i;

  for ( i = 0; i < 144; i++)
    {
      fem->AppendOneChannel(i, 0, 0);
    }
  return fem;

}

//_____________________________________________________________________________
void emcQAFEM::First(void)
{
  fIterator = fQA.begin();
}

//_____________________________________________________________________________
float
emcQAFEM::getValue(int ichannel, int what) const
{
  if (what != 0 && what != 1)
    {

      cerr << EMC_WARNING_MSG
	   << " emcQAFEM::getValue(ichannel,what) : what="
	   << what << " is incorrect. Returning default value of "
	   << DefaultReturnValue() << " instead" << endl;

      return DefaultReturnValue();

    }

  return getValueFast(ichannel, what);
}

//_____________________________________________________________________________
float
emcQAFEM::getValueFast(int ichannel, int what) const
{
  if (what == 0)
    return static_cast<float>(GetError(ichannel));
  if (what == 1)
    return static_cast<float>(GetWarning(ichannel));
  assert(0 == 1);
  return 0;
}

//_____________________________________________________________________________
INT32 emcQAFEM::GetError(int channel) const
{
  map<int, vector<INT32> >::const_iterator p = fQA.find(channel);
  if (p != fQA.end())
    {
      return (p->second)[0];
    }
  else
    {
      return static_cast<INT32>(0);
    }
}

//_____________________________________________________________________________
INT32 emcQAFEM::GetWarning(int channel) const
{
  map<int, vector<INT32> >::const_iterator p = fQA.find(channel);
  if (p != fQA.end())
    {
      return (p->second)[1];
    }
  else
    {
      return static_cast<INT32>(0);
    }
}

//_____________________________________________________________________________
bool
emcQAFEM::IsEqual(const emcCalFEM& obj) const
{
  if ( !dynamic_cast<const emcQAFEM*>(&obj) )
    return false;

  if ( size() != obj.size() )
    return false;

  for ( size_t i = 0; i < size(); i++)
    {
      for ( int j = 0; j < 2; j++ )
	{
	  if ( getValue(i, j) != obj.getValue(i, j) )
	    return false;
	}
    }
  return true;
}

//_____________________________________________________________________________
bool emcQAFEM::Next(int& channel, INT32& error, INT32& warning)
{
  if (fIterator != fQA.end())
    {
      vector<INT32> vec = fIterator->second;
      channel = fIterator->first;
      error = vec[0];
      warning = vec[1];
      fIterator++;
      return true;
    }
  else
    {
      return false;
    }
}

//_____________________________________________________________________________
ostream&
emcQAFEM::Print(ostream& out, int level) const
{
  emcCalFEM::Print(out, level);

  if (level)
    {
      ostream::fmtflags oldflags = out.flags();
      
      map<int, vector<INT32> >::const_iterator p;
      
      for (p = fQA.begin();p != fQA.end();p++ )
	{
	  vector<INT32> vec = p->second;
	  out << dec << "Channel #" << p->first 
	      << " Error=0x" << hex << vec[0]
	      << " Warning=0x" << hex << vec[1] << endl;
	}
      out.setf(oldflags);
    }
  return out;
}

//_____________________________________________________________________________
void emcQAFEM::Reset(void)
{
  fQA.clear();
}
