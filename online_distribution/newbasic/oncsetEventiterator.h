#ifndef __ONCSETEVENTITERATOR_H__
#define __ONCSETEVENTITERATOR_H__

#include <Eventiterator.h>

#ifndef __CINT__
#include "et.h"
#endif

/** The oncsetEventiterator creates Event object from the ET pool

*/
#ifndef __CINT__
class WINDOWSEXPORT oncsetEventiterator : public Eventiterator {
#else
class  oncsetEventiterator : public Eventiterator {
#endif
public:

  oncsetEventiterator(const char *etname);
  oncsetEventiterator(const char *etname, const char *stationname);

  oncsetEventiterator(const char *etname, int &status);
  oncsetEventiterator(const char *etname, const char *stationname, int &status);


  ~oncsetEventiterator();

  virtual const char * getIdTag() const;

  virtual void identify(std::ostream& os = std::cout) const ;


  Event *getNextEvent();
  int *getNextEventData();
  int releaseEventData();

  void setBlockingMode(const int mode);
  int getBlockingMode() const;

  void setSelectMode(const int mode);
  int getSelectMode() const;

  void setSelectWords(const int i1, const int i2, const int i3, const int i4 );
  void getSelectWords (int val[]) const ;


protected:
  virtual int setup(const char *etname, const char *stationname
		    ,const int isAllowedToAttach=0 );

private:


#ifndef __CINT__
  static  pthread_t tid;

  char *real_etname;
  char *real_hostname;

  int pthreadParentId;
  et_event       *pe;
  et_openconfig   openconfig;
  et_statconfig   sconfig;
  et_att_id       attach1;
  et_stat_id      my_stat;
  et_sys_id       id;
  int objectIsFunctional;
  struct timespec timeout;
#endif

};

#endif /* __ONCSETEVENTITERATOR_H__ */

