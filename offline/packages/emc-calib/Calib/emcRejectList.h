#ifndef __EMCREJECTLIST_H__
#define __EMCREJECTLIST_H__

#include <vector>
#include <map>
#include <iostream>

#ifndef __EMCMANAGEABLE_H__
#include "emcManageable.h"
#endif
#ifndef __PHTIMESTAMP_H__
#include "PHTimeStamp.h"
#endif

/** Stores list of not-to-be-used towers.
@ingroup calibration
@sa emcBadModules
 */

class emcRejectList : public emcManageable
{
public:

  /// CREATORS.
  emcRejectList();
  emcRejectList(const emcRejectList&);
  emcRejectList& operator=(const emcRejectList&);

  emcRejectList& operator+=(const emcRejectList&);
  emcRejectList& operator-=(const emcRejectList&);

  virtual ~emcRejectList();

  /// ACCESSORS.

  unsigned int AmplitudeError(int towerid) const;
  unsigned int AmplitudeWarning(int towerid) const;

  unsigned int Error(int towerid) const;

  const char* GetCategory(void) const { return "RejectList"; }

  /// Get the end-of-validity time
  const PHTimeStamp& GetEndValTime(void) const { return fEnd; }

  /// Get the start-of-validity time
  const PHTimeStamp& GetStartValTime(void) const { return fStart; }

  bool nonZero(int towerid) const;

  void print(std::ostream& out=std::cout) const;

  size_t size() const { return fTowers.size(); }
  
  size_t maxsize() const { return 144*172; }

  unsigned int TimingError(int towerid) const;
  unsigned int TimingWarning(int towerid) const;

  unsigned int Warning(int towerid) const;

  /// MUTATORS.

  void Reset(void);
 
  /** Set flags for a given tower. 
      All flags are 4 bits maximum.
   */
  void set(int towerid, unsigned int amp_error, unsigned int amp_warning, 
	   unsigned int timing_error, unsigned int timing_warning);
 
  /** Set/update (make a logical OR on the existing bits) flags to
      a given tower. */
  void set_or(int towerid, unsigned int amp_error, unsigned int amp_warning, 
	      unsigned int timing_error, unsigned int timing_warning);

  void SetValidityPeriod(const PHTimeStamp& t1, const PHTimeStamp& t2) 
  {
    fStart = t1; fEnd = t2;
  }

private:

  void Copy(emcRejectList&) const;

  class SixInts 
  { 
  public:
    
    SixInts(unsigned int amperror=0, unsigned int ampwarning=0,
	    unsigned int toferror=0, unsigned int tofwarning=0);

    bool nonZero() const;
    
    void zero();

    void set(unsigned int amperror, unsigned int ampwarning,
	     unsigned int toferror, unsigned int tofwarning);
 
    void set_or(unsigned int amperror, unsigned int ampwarning,
		unsigned int toferror, unsigned int tofwarning);

    unsigned int ampError() const { return ampError_; }
    unsigned int ampWarning() const { return ampWarning_; }
    unsigned int tofError() const { return tofError_; }
    unsigned int tofWarning() const { return tofWarning_; }

    unsigned int error() const { return error_; }
    unsigned int warning() const { return warning_; }

  private:
    unsigned int ampError_;
    unsigned int ampWarning_;
    unsigned int tofError_;
    unsigned int tofWarning_;
    unsigned int error_;
    unsigned int warning_;
  };
  typedef std::map<int,SixInts> TMAP;
  TMAP fTowers;

  SixInts getTower(int towerid) const;

  PHTimeStamp fStart;
  PHTimeStamp fEnd;
};

inline
std::ostream& operator<<(std::ostream& out, const emcRejectList& rl)
{
  rl.print(out);
  return out;
}

#endif
