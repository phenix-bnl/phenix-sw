#ifndef __ETEVENTITERATOR_H__
#define __ETEVENTITERATOR_H__

#include <Eventiterator.h>

#ifndef __CINT__
#include "et.h"
#endif

/** The etEventiterator creates Event object from the ET pool

*/
#ifndef __CINT__
class WINDOWSEXPORT etEventiterator : public Eventiterator {
#else
class  etEventiterator : public Eventiterator {
#endif
public:

  etEventiterator(const char *etname);
  etEventiterator(const char *etname, const char *stationname);

  etEventiterator(const char *etname, int &status);
  etEventiterator(const char *etname, const char *stationname, int &status);


  ~etEventiterator();

  virtual const char * getIdTag() const;

  virtual void identify(std::ostream& os = std::cout) const ;



  Event *getNextEvent();
  int *getNextEventData();
  int releaseEventData();

  void setBlockingMode(const int mode);
  int getBlockingMode() const;

  void setSelectMode(const int mode);
  int getSelectMode() const;

  void setSelectWords(int i1, int i2, int i3, int i4, int disable);
  void setSelectWords(int i1, int i2, int i3, int i4) { // unhide previous function
    setSelectWords(i1, i2, i3, i4, 0);
  }
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

#endif /* __ETEVENTITERATOR_H__ */

