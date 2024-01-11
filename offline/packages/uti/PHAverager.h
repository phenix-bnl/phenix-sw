//////////////////////////////////////////////////////////////////////
//
// PHAverager.h: interface for the PHAverager class.
//
//   Defines a class that provides the ability to perform a rolling
//     average of a quantity. "inPeriod" defines the number of values
//     over which the average is performed. When more than inPeriod
//     values have been inserted, for every new value inserted, the
//     oldest value is removed. 
//
// Author: B. Cole
// Date:   July, 2000
//
// $Log: PHAverager.h,v $
// Revision 1.1  2009/12/07 00:18:10  winter
// Initial revision
//
// Revision 1.13  2005/12/02 21:01:10  phoncs
// DLW: replace double-quotes with brackets in include directives
//
// Revision 1.12  2005/09/15 18:34:45  phoncs
// update to linux versions
//
// Revision 1.10.14.2  2004/12/07 00:58:30  phoncs
// DLW: protect against zero return values
//
// Revision 1.10.14.1  2004/10/06 19:04:11  phoncs
// DLW: tweaks to address -Wall messages
//
// Revision 1.10  2003/11/19 04:24:26  winter
// DLW: VC6 doesn't support std::abs???
//
// Revision 1.9  2003/11/18 17:58:40  winter
// DLW: suse std::abs instead so that RH8 compiles
//
// Revision 1.8  2003/11/01 23:30:20  phoncs
// DLW: compute diff from mean in smooth average
//
// Revision 1.7  2003/10/31 18:57:45  phoncs
// DLW: checkin of Jiamin's first changes to the Timer class + it's relatives (Smoothed stats)
//
// Revision 1.6  2001/04/02 18:01:31  cole
// Add copy constructor and operator=
//
// Revision 1.5  2001/03/22 13:39:18  cole
// Cosmetic changes to header
//
//
//////////////////////////////////////////////////////////////////////

#ifndef __PHAVERAGER_H__
#define __PHAVERAGER_H__

//#include <phenixEvB.h>
#include <algorithm>
#include <cmath>

template<class T> class PHAverager  
{
  T* _ringbuffer;
  T _periodSum;

  unsigned int _period;
  unsigned int _totalCount;
  unsigned int _totalRejected;  

  unsigned int _writeIndex;

public:

  PHAverager(unsigned int inPeriod = 100) : 
    _ringbuffer(0), _periodSum(0), _period(inPeriod), _totalCount(0), _totalRejected(0), _writeIndex(0)
  {
    _ringbuffer = new T[_period];

    for (unsigned int i = 0; i < _period; i++) _ringbuffer[i] = 0;
  }

  virtual ~PHAverager() {delete[] _ringbuffer;}

  PHAverager& operator=(const PHAverager& inAverager)
  {
    delete[] _ringbuffer;

    _period = inAverager._period;

    _ringbuffer = new T[_period];
    for (unsigned int i = 0; i < _period; i++) _ringbuffer[i] = 0;

    _writeIndex = 0;
    _totalCount = 0;
    _totalRejected = 0;
    _periodSum = 0;

    return *this;
  }

  PHAverager(const PHAverager& inAverager) : 
    _ringbuffer(0), _periodSum(0), _period(inAverager._period), 
    _totalCount(0), _totalRejected(0), _writeIndex(0)
  {
    _ringbuffer = new T[_period];
    for (unsigned int i = 0; i < _period; i++) _ringbuffer[i] = 0;
  }
  
  void insert(T inValue)
  {
    _periodSum += inValue - _ringbuffer[_writeIndex];
    _ringbuffer[_writeIndex] = inValue;
    _writeIndex = (_writeIndex + 1) % _period;
    _totalCount++;
  }

  //T getAverage() const { return (_totalCount < _period ? 0 : _periodSum/getPeriodCount()); }
  T getAverage() const { return _periodSum/getPeriodCount(); }

  unsigned int getPeriodCount() const {
    if ( _totalCount == 0 ) return 1; // Not well-defined, just guarantee zero is never returned
    return (_totalCount < _period ? _totalCount : _period);
  }

  unsigned int getTotalCount() const {return _totalCount;}

  T getRMS() const 
  { 
    T sumsq = 0;
    for (unsigned int i = 0; i < _period; i++)
      {
	sumsq += _ringbuffer[i]*_ringbuffer[i];
      }

    T avgsq = sumsq/_period;
    T average = getAverage();
    T rms = sqrt(avgsq - average*average);

    return rms;
  }

  T getSmoothAverage() const 
  {
    T sum = 0; 
    unsigned int  count = 0;

    for(unsigned int i = 0; i < _period; i++)
      {
	if( fabs(_ringbuffer[i]-getAverage()) <= 6.0*getRMS())
	  {
	    sum +=_ringbuffer[i];
	    count++;
	  }
      }
    if (count != 0 ) 
      return sum/count;
    else return 0;
  }

  T getSmoothRMS() const 
  { 
    T sumsq = 0;
    unsigned int count = 0;
    for (unsigned int i = 0; i < _period; i++)
      {
	if( fabs(_ringbuffer[i]-getAverage()) <= 6.0*getRMS())
	  {
	    sumsq += _ringbuffer[i]*_ringbuffer[i];
	    count ++;
	  }
      }

    if (count != 0)
      {
	T avgsq = sumsq/count;
	T average = getSmoothAverage();
	T rms = sqrt(avgsq - average*average);
	return rms;
      }
    else return 0;
  }

  T getMin() const
  {
    return *std::min_element(&_ringbuffer[0], &_ringbuffer[_period]);
  }

  T getMax() const
  {
    return *std::max_element(&_ringbuffer[0], &_ringbuffer[_period]);
  }

  void clear() {
    _totalCount = 0;
    _totalRejected = 0;
    _periodSum = T();
    _writeIndex = 0;
    for (unsigned int i = 0; i < _period; i++) _ringbuffer[i] = 0;
  }

};

#endif // __PHAVERAGER_H__

