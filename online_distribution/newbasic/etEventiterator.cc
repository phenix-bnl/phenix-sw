#include "etEventiterator.h"
#include "A_Event.h"
#include "msg_profile.h"
#include "msg_control.h"
#include <cstddef>
#include <string>
#include <sstream>
#include <unistd.h>

//static void * signal_thread (void *arg);

pthread_t etEventiterator::tid = 0;

etEventiterator::etEventiterator(const char *etname)
{
  std::ostringstream stationname;
  stationname << "station_" << getpid() << std::ends;

  setup(etname, stationname.str().c_str());

}
etEventiterator::etEventiterator(const char *etname, int &status)
{
  std::ostringstream stationname;
  stationname << "station_" << getpid() << std::ends;

  // the last 0 means that we are NOT allowed to attach to
  // an existing station. We did not specify one, thius name is
  // meant to be unique.
  status = setup(etname, stationname.str().c_str(), 0);

}


etEventiterator::etEventiterator(const char *etname, const char *stationname)
{

  // the 1 says that we are allowed to attach to an existing station.
  // we specified the name, so we assume the user knows what to do.
  setup(etname, stationname, 1);

}

etEventiterator::etEventiterator(const char *etname, const char *stationname
                                 , int &status)
{
  // the 1 says that we are allowed to attach to an existing station.
  // we specified the name, so we assume the user knows what to do.
  status = setup(etname, stationname, 1);
}

etEventiterator::~etEventiterator()
{

  if ( real_etname)
    {
      delete [] real_etname;
    }
  if ( real_hostname)
    {
      delete [] real_hostname;
    }

  msg_control *Message = new msg_control(MSG_TYPE_MONITORING,
                                         MSG_SOURCE_ET,
                                         MSG_SEV_INFORMATIONAL,
                                         "etEventiterator");
  COUT << *Message << "detaching the station" << std::endl;
  delete Message;
  et_station_detach(id, attach1);
  sleep(2);
  et_station_remove(id, my_stat);
  et_close(id);
}


int etEventiterator::setup(const char *etname, const char *stationname
                           , const int isAllowedToAttach)
{

  // let's start on the assumption that we do get a functional
  // system
  objectIsFunctional = 1;

  real_etname = 0;
  real_hostname = 0;

  int status;

  //  COUT << "in etEventiterator::setup" << std::endl;

  pthreadParentId = 0;


  /* spawn signal handling thread */
  //  if (!tid)
  //  {
  //    pthread_create(&tid, NULL, signal_thread, (void *)NULL);
  //    pthreadParentId=getpid();
  //  }

  et_open_config_init(&openconfig);
  et_open_config_setwait(openconfig, ET_OPEN_WAIT);

  // now, is there an "@" in the station?

  // first, we copy our const string to a local one
  char *s0 = new char [ strlen(etname) + 1];
  strcpy ( s0, etname);

  // we don't modify s0, s may be modified.
  char *s = s0;
  char *name, *host;
  if ( strchr( s, '@') ) // yes, sir, an @ in there
    {
#if defined(SunOS) || defined(OSF1)
      name = strtok ( s, "@");
      host = strtok ( s, "@");
#else

      name = strsep ( &s, "@");
      host = strsep ( &s, "@");
#endif

      real_etname = new char[ strlen(name) + 1];
      strcpy (real_etname, name);
      real_hostname = new char[ strlen(host) + 1];
      strcpy (real_hostname, host);

      et_open_config_sethost(openconfig, host);
      et_open_config_setcast(openconfig, ET_DIRECT);
      msg_control *Message = new msg_control(MSG_TYPE_MONITORING,
                                             MSG_SOURCE_ET,
                                             MSG_SEV_INFORMATIONAL,
                                             "etEventiterator");

      for (unsigned short int i = 11111;i < 11115;i++)
        {
          COUT << *Message
	       << "etEventiterator: Checking for et system at port "
	       << i << std::endl;
          et_open_config_setserverport(openconfig, i);
          if (et_open(&id, name , openconfig) == ET_OK)
            {
              COUT << *Message << "etEventiterator: Found et system at port "
		   << i << std::endl;
              delete Message;
              goto opensuccess;
            }
        }
      delete Message;
      delete [] s0;
      Message = new msg_control(MSG_TYPE_MONITORING,
                                MSG_SOURCE_ET,
                                MSG_SEV_SEVEREERROR,
                                "etEventiterator");
      COUT << *Message << "etEventiterator: error opening " << etname << std::endl;
      objectIsFunctional = 0;
      delete Message;

      return -1;

    }
  else
    {
      name = s0;

      real_etname = new char[ strlen(name) + 1];
      strcpy (real_etname, name);

      et_open_config_sethost(openconfig, ET_HOST_LOCAL);
      if (et_open(&id, name , openconfig) != ET_OK)
        {
          delete [] s0;
          msg_control *Message = new msg_control(MSG_TYPE_MONITORING,
                                                 MSG_SOURCE_ET,
                                                 MSG_SEV_SEVEREERROR,
                                                 "etEventiterator");
          COUT << *Message << "etEventiterator: error opening "
	       << etname << std::endl;
          delete Message;
          objectIsFunctional = 0;
          return -1;
        }
    }
 opensuccess:
  delete [] s0;
  et_open_config_destroy(openconfig);

  et_station_config_init(&sconfig);
  et_station_config_setuser(sconfig, ET_STATION_USER_MULTI);
  et_station_config_setrestore(sconfig, ET_STATION_RESTORE_OUT);
  et_station_config_setprescale(sconfig, 1);
  et_station_config_setcue(sconfig, 3);


  et_station_config_setselect(sconfig, ET_STATION_SELECT_ALL);
  et_station_config_setblock(sconfig, ET_STATION_NONBLOCKING);

  if ((status = et_station_create(id, &my_stat, stationname, sconfig)) < ET_OK)
    {
      if (status == ET_ERROR_EXISTS)
        {
          // if this happens, we look at isAllowdToAttach to find
          // out if we can just attach.
          if ( isAllowedToAttach )

            {
              et_station_name_to_id(id, &my_stat, stationname);
            }
          else
            {
              /* my_stat contains pointer to existing station */
              msg_control *Message = new msg_control(MSG_TYPE_MONITORING,
                                                     MSG_SOURCE_ET,
                                                     MSG_SEV_SEVEREERROR,
                                                     "etEventiterator");
              COUT << *Message << "etEventiterator: station"
		   << stationname << " already exists" << std::endl;
              delete Message;
              objectIsFunctional = 0;
              return -1;
            }
        }
      else if (status == ET_ERROR_TOOMANY)
        {
          msg_control *Message = new msg_control(MSG_TYPE_MONITORING,
                                                 MSG_SOURCE_ET,
                                                 MSG_SEV_SEVEREERROR,
                                                 "etEventiterator");
          COUT << *Message << "etEventiterator: too many stations created"
	       << std::endl;
          delete Message;
          objectIsFunctional = 0;
          return -1;
        }
      else
        {
          msg_control *Message = new msg_control(MSG_TYPE_MONITORING,
                                                 MSG_SOURCE_ET,
                                                 MSG_SEV_SEVEREERROR,
                                                 "etEventiterator");
          COUT << *Message << "etEventiterator: error in station creation"
	       << std::endl;
          delete Message;
          objectIsFunctional = 0;
          return -1;
        }
    }

  et_station_config_destroy(sconfig);

  if (et_station_attach(id, my_stat, &attach1) < 0)
    {
      msg_control *Message = new msg_control(MSG_TYPE_MONITORING,
                                             MSG_SOURCE_ET,
                                             MSG_SEV_SEVEREERROR,
                                             "etEventiterator");
      COUT << *Message << "etEventiterator: error in station attach"
	   << std::endl;
      delete Message;
      objectIsFunctional = 0;
      return -1;
    }

  timeout.tv_sec = 0;
  timeout.tv_nsec = 0;
  return 0;

}


void
etEventiterator::identify (OSTREAM &os) const
{
  os << "etEventiterator reading from " << real_etname;
  if (real_hostname)
    {
      os << " on host " << real_hostname;
    }
  os << std::endl;

};

const char * etEventiterator::getIdTag () const
{
  return "etEventiterator";
};



// and, finally, the only non-constructor member function to
// retrieve events from the iterator.

Event *
etEventiterator::getNextEvent()
{
  Event *evt;
  int status;
  if (!et_alive(id))
    {
      msg_control *Message = new msg_control(MSG_TYPE_MONITORING,
                                             MSG_SOURCE_ET,
                                             MSG_SEV_ERROR,
                                             "etEventiterator");
      COUT << *Message << "ET " << real_etname << " is dead" << std::endl;
      delete Message;
      return 0;
    }
  status = et_event_get(id, attach1, &pe, ET_SLEEP, 0);
  if (status != ET_OK)
    {
      msg_control *Message = new msg_control(MSG_TYPE_MONITORING,
                                             MSG_SOURCE_ET,
                                             MSG_SEV_ERROR,
                                             "etEventiterator");
      COUT << *Message << "Error getting Event: Status: "
	   << status << std::endl;
      delete Message;
      return 0;
    }

  int *data;
  int len;
  et_event_getdata(pe, (void **) &data);
  et_event_getlength(pe, &len);

  evt = new A_Event(data);
  evt->convert();

  status = et_event_put(id, attach1, pe);

  return evt;

}

int *etEventiterator::getNextEventData()
{
  int status;

  if (!et_alive(id))
    {
      msg_control *Message = new msg_control(MSG_TYPE_MONITORING,
                                             MSG_SOURCE_ET,
                                             MSG_SEV_ERROR,
                                             "etEventiterator");
      COUT << *Message << "ET is not alive" << std::endl;
      delete Message;
      return 0;
    }
  status = et_event_get(id, attach1, &pe, ET_SLEEP, 0);
  if (status != ET_OK)
    {
      msg_control *Message = new msg_control(MSG_TYPE_MONITORING,
                                             MSG_SOURCE_ET,
                                             MSG_SEV_ERROR,
                                             "etEventiterator");
      COUT << *Message << "Error getting Event: Status: "
	   << status << std::endl;
      delete Message;
      return 0;
    }

  int *data;
  et_event_getdata(pe, (void **) &data);

  return data;

}


int etEventiterator::releaseEventData()
{
  int status;
  status = et_event_put(id, attach1, pe);

  if (status != ET_OK)
    {
      return -1;
    }
  return 0;
}

void etEventiterator::setBlockingMode(const int mode)
{
  if ( mode != ET_STATION_NONBLOCKING && mode != ET_STATION_BLOCKING)
    {
      msg_control *Message = new msg_control(MSG_TYPE_MONITORING,
                                             MSG_SOURCE_ET,
                                             MSG_SEV_WARNING,
                                             "etEventiterator");
      COUT << *Message << "Wrong blocking mode, unchanged" << std::endl;
      delete Message;
      return ;
    }
  et_station_config_setblock(sconfig, mode);
}

int etEventiterator::getBlockingMode() const
{
  int val;
  et_station_getblock(id, my_stat, &val);
  return val;
}


void etEventiterator::setSelectMode(const int mode)
{
  if ( mode != ET_STATION_SELECT_ALL &&
       //    mode != ET_STATION_SELECT_USER &&  ...will be implemented later
       mode != ET_STATION_SELECT_MATCH)
    {
      msg_control *Message = new msg_control(MSG_TYPE_MONITORING,
                                             MSG_SOURCE_ET,
                                             MSG_SEV_WARNING,
                                             "etEventiterator");
      COUT << *Message << "Wrong Select mode, unchanged" << std::endl;
      delete Message;
      return ;
    }

  et_station_config_setselect(sconfig, ET_STATION_SELECT_ALL);
}

int etEventiterator::getSelectMode() const
{
  int val;
  et_station_getselect(id, my_stat, &val);
  return val;
}


void etEventiterator::setSelectWords(const int i1, const int i2, const int i3, const int i4, const int disable )
{
  int val[4] = {i1, i2, i3, i4};

  et_station_detach(id, attach1);
  et_station_remove(id, my_stat);

  et_station_config_init(&sconfig);
  et_station_config_setuser(sconfig, ET_STATION_USER_MULTI);
  et_station_config_setrestore(sconfig, ET_STATION_RESTORE_OUT);
  et_station_config_setprescale(sconfig, 1);

  et_station_config_setblock(sconfig, ET_STATION_NONBLOCKING);

  if (! disable)
    {
      et_station_config_setcue(sconfig, 5);
      et_station_config_setselectwords(sconfig, val);
      et_station_config_setselect(sconfig, ET_STATION_SELECT_MATCH);
    }
  else
    {
      et_station_config_setcue(sconfig, 3);
      et_station_config_setselect(sconfig, ET_STATION_SELECT_ALL);
    }

  std::ostringstream stationname;
  stationname << "station_" << getpid();
  et_station_create(id, &my_stat, stationname.str().c_str(), sconfig);
  et_station_config_destroy(sconfig);
  et_station_attach(id, my_stat, &attach1);
}


void etEventiterator::getSelectWords (int val[]) const
{
  et_station_getselect(id, my_stat, val);
}
