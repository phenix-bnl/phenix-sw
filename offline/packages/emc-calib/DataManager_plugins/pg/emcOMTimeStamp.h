#ifndef __EMCOMTIMESTAMP_H__
#define __EMCOMTIMESTAMP_H__

#ifndef __EMCOBJECTMANAGER_H__
#include "emcObjectManager.h"
#endif

#include <map>

class PHTimeStamp;

/** Object to wrap the access to RunToTimePg and cache results. */

class emcOMTimeStamp : public emcObjectManager
{
public:

  emcOMTimeStamp(const char* name="", const char* title="");
  virtual ~emcOMTimeStamp();

  virtual bool CanCollect(const emcManageable&) const
  { return false; }

  virtual bool CanRead(const emcManageable& object) const;

  virtual bool CanWrite(const emcManageable&) const
  { return false; }

  virtual emcManageable* Collect(const emcManageable&,
				 const PHTimeStamp&)
  { return 0; }

  virtual bool Read(emcManageable& ,
		    const PHTimeStamp&,
		    int) 
  { return false; }

  virtual bool Read(emcManageable& object, int runnumber);

  virtual void Reset(void);

  virtual bool Write(const emcManageable&,
		     const PHTimeStamp&,
		     int=-1)
  { return false; }

private:
  std::map<int,PHTimeStamp*> fTSMap;
};

#endif
