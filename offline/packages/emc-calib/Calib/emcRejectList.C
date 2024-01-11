#include "emcRejectList.h"
#include <cassert>
#include <iomanip>

using namespace std;

namespace
{
  void warning(const char* type, unsigned int value)
  {
    std::cerr << __FILE__ << ":" << __LINE__ << " "
	      << type << "=" << value << " is more than the required 4 bits. "
	      << "Will be tripped down to 4 bits only"
	      << std::endl;
  }

  void trip(unsigned int& amperror, unsigned int& ampwarning,
	    unsigned int& toferror, unsigned int& tofwarning)
  {
    // Trip to 4 bits. Echo warning if more than 4 bits.
    if ( amperror & 0xFFF0 )
      {
	warning("amperror",amperror);
      }
    if ( ampwarning & 0xFFF0 )
      {
	warning("ampwarning",ampwarning);
      }
    if ( toferror & 0xFFF0 )
      {
	warning("toferror",toferror);
      }
    if ( tofwarning & 0xFFF0 )
      {
	warning("tofwarning",tofwarning);
      }

    amperror = amperror & 0xF;
    ampwarning = ampwarning & 0xF;
    toferror = toferror & 0xF;
    tofwarning = tofwarning & 0xF;
  }
}

//_____________________________________________________________________________
emcRejectList::SixInts::SixInts(unsigned int amperror, unsigned int ampwarning,
				unsigned int toferror, unsigned int tofwarning) 
{
  set(amperror,ampwarning,toferror,tofwarning);
}

//_____________________________________________________________________________
bool
emcRejectList::SixInts::nonZero() const
{
  return ( error_ != 0 || warning_ != 0 );
}

//_____________________________________________________________________________
void
emcRejectList::SixInts::set(unsigned int amperror, unsigned int ampwarning,
			    unsigned int toferror, unsigned int tofwarning) 
{
  trip(amperror,ampwarning,toferror,tofwarning);

  ampError_ = amperror;
  ampWarning_ = ampwarning;
  tofError_ = toferror;
  tofWarning_ = tofwarning;

  error_ = ( tofError_ << 4 ) | ampError_;
  warning_ = ( tofWarning_ << 4 ) | ampWarning_;
}

//_____________________________________________________________________________
void
emcRejectList::SixInts::set_or(unsigned int amperror, unsigned int ampwarning,
			       unsigned int toferror, unsigned int tofwarning) 
{
  trip(amperror,ampwarning,toferror,tofwarning);

  ampError_ |= amperror;
  ampWarning_  |= ampwarning;
  tofError_ |= toferror;
  tofWarning_ |= tofwarning;

  error_ = ( tofError_ << 4 ) | ampError_;
  warning_ = ( tofWarning_ << 4 ) | ampWarning_;
}

//_____________________________________________________________________________
void
emcRejectList::SixInts::zero()
{
  ampWarning_=ampError_=tofError_=tofWarning_=0;
  error_=warning_=0;
}

//_____________________________________________________________________________
emcRejectList::emcRejectList()
  : emcManageable("emcRejectList","List of EMCAL towers known to be bad",
		  "emcRejectList")
{
  fStart.setToSystemTime();
  fEnd.setToFarFuture();
  Reset();
}

//_____________________________________________________________________________
emcRejectList::emcRejectList(const emcRejectList& o) : emcManageable()
{
  o.Copy(*this);
}

//_____________________________________________________________________________
emcRejectList&
emcRejectList::operator=(const emcRejectList& o)
{
  if ( this != &o )
    {
      Reset();
      o.Copy(*this);
    }
  return *this;
}

//_____________________________________________________________________________
emcRejectList&
emcRejectList::operator+=(const emcRejectList& o)
{
  for ( size_t itower = 0; itower < o.maxsize(); ++itower )
    {
      if ( o.nonZero(itower) )
	{
	  set_or(itower,
		 o.AmplitudeError(itower),o.AmplitudeWarning(itower),
		 o.TimingError(itower),o.TimingWarning(itower));
	}
    }
  return *this;
}

//_____________________________________________________________________________
emcRejectList&
emcRejectList::operator-=(const emcRejectList& o)
{
  for ( size_t itower = 0; itower < o.maxsize(); ++itower )
    {
      if ( o.nonZero(itower) )
	{
	  if ( nonZero(itower) )
	    {
	      fTowers.erase(itower);
	    }
	}
    }
  return *this;
}

//_____________________________________________________________________________
emcRejectList::~emcRejectList()
{
}

//_____________________________________________________________________________
emcRejectList::SixInts
emcRejectList::getTower(int towerid) const
{
  TMAP::const_iterator it = fTowers.find(towerid);
  if ( it != fTowers.end() )
    {
      return it->second;
    }
  else
    {
      return emcRejectList::SixInts();
    }
}

//_____________________________________________________________________________
unsigned int
emcRejectList::AmplitudeError(int towerid) const
{
  return getTower(towerid).ampError();
}

//_____________________________________________________________________________
unsigned int
emcRejectList::AmplitudeWarning(int towerid) const
{
  return getTower(towerid).ampWarning();
}

//_____________________________________________________________________________
void
emcRejectList::Copy(emcRejectList& to) const
{
  to.fTowers = fTowers;
  to.fStart = fStart;
  to.fEnd = fEnd;
}

//_____________________________________________________________________________
unsigned int 
emcRejectList::Error(int towerid) const
{
  return getTower(towerid).error();
}

//_____________________________________________________________________________
bool
emcRejectList::nonZero(int towerid) const
{
  return getTower(towerid).nonZero();
}

//_____________________________________________________________________________
void
emcRejectList::print(std::ostream& out) const
{  
  std::ostream::fmtflags oldflags = out.flags();
  
  TMAP::const_iterator it;
  for ( it = fTowers.begin(); it != fTowers.end(); ++it ) 
    {
      if ( it->second.nonZero() ) 
	{
	  out << dec << "TOWERID=" << setw(6) << it->first
	      << " AMP ERR=" << setw(6) << hex << it->second.ampError()
	      << " AMP WARN =" << setw(6) << hex << it->second.ampWarning()
	      << " TOF ERR=" << setw(6) << hex << it->second.tofError()
	      << " TOF WARN =" << setw(6) << hex << it->second.tofWarning()
	    << std::endl;
	}
    }
  out.setf(oldflags);
}

//_____________________________________________________________________________
void
emcRejectList::Reset()
{
  fTowers.clear();
}

//_____________________________________________________________________________
void
emcRejectList::set(int towerid, 
		   unsigned int amp_error, unsigned int amp_warning, 
		   unsigned int timing_error, unsigned int timing_warning)
{
  SixInts& v = fTowers[towerid];
  v.set(amp_error,amp_warning,timing_error,timing_warning);
}

//_____________________________________________________________________________
void
emcRejectList::set_or(int towerid, 
		      unsigned int amp_error, unsigned int amp_warning, 
		      unsigned int timing_error, unsigned int timing_warning)
{
  SixInts& v = fTowers[towerid];
  v.set_or(amp_error,amp_warning,timing_error,timing_warning);
}

//_____________________________________________________________________________
unsigned int
emcRejectList::TimingError(int towerid) const
{
  return getTower(towerid).tofError();
}

//_____________________________________________________________________________
unsigned int
emcRejectList::TimingWarning(int towerid) const
{
  return getTower(towerid).tofWarning();
}


//_____________________________________________________________________________
unsigned int 
emcRejectList::Warning(int towerid) const
{
  return getTower(towerid).warning();
}
