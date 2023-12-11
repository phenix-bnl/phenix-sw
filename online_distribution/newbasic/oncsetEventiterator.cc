//
// fileEventIterator   mlp 4/19/1997
//


#include <oncsetEventiterator.h>
#include <oncsEvent.h>
#include <stddef.h>
#include <string.h>
#include <unistd.h>

pthread_t oncsetEventiterator::tid = 0;

oncsetEventiterator::oncsetEventiterator(const char *etname)
{
  char stationname[132];
  sprintf (stationname,"station_%d", getpid() );
  

  setup(etname, stationname);

}
oncsetEventiterator::oncsetEventiterator(const char *etname, int &status)
{
  char stationname[132];
  sprintf (stationname,"station_%d", getpid() );
  
  // the last 0 means that we are NOT allowed to attach to
  // an existing station. We did not specify one, thius name is 
  // meant to be unique. 
  status = setup(etname, stationname,0);

}


oncsetEventiterator::oncsetEventiterator(const char *etname, const char *stationname)
{

  // the 1 says that we are allowed to attach to an existing station. 
  // we specified the name, so we assume the user knows what to do.
  setup(etname, stationname, 1);

}

oncsetEventiterator::oncsetEventiterator(const char *etname, const char *stationname
				 , int &status)
{
  // the 1 says that we are allowed to attach to an existing station. 
  // we specified the name, so we assume the user knows what to do.
  status = setup(etname, stationname,1);
}

oncsetEventiterator::~oncsetEventiterator()
{

  if ( real_etname) delete [] real_etname;
  if ( real_hostname) delete [] real_hostname;

  COUT << "detaching the station" << std::endl;
  et_station_detach(id, attach1);
  sleep(2);
  et_station_remove(id, my_stat);

}  


int oncsetEventiterator::setup(const char *etname, const char *stationname
			   , const int isAllowedToAttach)
{

  // let's start on the assumption that we do get a functional
  // system
  objectIsFunctional = 1; 

  real_etname = 0;
  real_hostname = 0;
  
  int status;

  //  COUT << "in etEventiterator::setup" << std::endl;

  pthreadParentId= 0;

  
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
  char *s0 = new char [ strlen(etname) +1];
  strcpy ( s0, etname);

  // we don't modify s0, s may be modified.
  char *s = s0;
  char *name, *host;

  if ( strchr( s, '@') ) // yes, sir, an @ in there
    {
#if defined(SunOS) || defined(OSF1)
      name =  strtok ( s,"@");
      host =  strtok ( s,"@");
#else
      name =  strsep ( &s,"@");
      host =  strsep ( &s,"@");
#endif
      real_etname = new char[ strlen(name)+1];
      strcpy (real_etname, name);
      real_hostname = new char[ strlen(host)+1];
      strcpy (real_hostname, host);

      et_open_config_sethost(openconfig,host);
      et_open_config_setcast(openconfig, ET_DIRECT);
      for (unsigned short int i=11111;i<11115;i++)
	{
	  COUT << "etEventiterator: Checking for et system at port " << i << std::endl;
	  et_open_config_setserverport(openconfig,i);
	  if (et_open(&id,name , openconfig) == ET_OK)
	    {
	      COUT << "etEventiterator: Found et system at port " << i << std::endl;
	      goto opensuccess; 
	    }
	}
      delete [] s0;
      COUT << "etEventiterator: error opening " << etname << std::endl;
      objectIsFunctional = 0;
      return -1;

    }
  else
    {
      name = s0;

      real_etname = new char[ strlen(name)+1];
      strcpy (real_etname, name);

      et_open_config_sethost(openconfig,ET_HOST_LOCAL);
      if (et_open(&id,name , openconfig) != ET_OK) 
	{
	  delete [] s0;
	  COUT << "etEventiterator: error opening " << etname << std::endl;
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
  //  et_station_config_setcue(sconfig, 150);


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
	      COUT << "etEventiterator: station" 
		   << stationname << " already exists" << std::endl;
	      objectIsFunctional = 0;
	      return -1;
	    }
	}
      else if (status == ET_ERROR_TOOMANY) 
	{
	  COUT << "etEventiterator: too many stations created" << std::endl; 
	  objectIsFunctional = 0;
	  return -1;
	}
      else 
	{
	  COUT << "etEventiterator: error in station creation" << std::endl;
	  objectIsFunctional = 0;
	  return -1;
	}
    }

  et_station_config_destroy(sconfig);

  if (et_station_attach(id, my_stat, &attach1) < 0) 
    {
      COUT << "etEventiterator: error in station attach" << std::endl;
      objectIsFunctional = 0;
      return -1;
    }

  timeout.tv_sec  = 0;
  timeout.tv_nsec = 0;
  return 0;

}  


void  
oncsetEventiterator::identify (OSTREAM &os) const
{ 
  os << "oncsetEventiterator reading from " << real_etname;
  if (real_hostname)
    {
      os << " on host " << real_hostname;
    }
  os << std::endl;

};

const char * oncsetEventiterator::getIdTag () const
{ 
  return "oncsetEventiterator";
};



// and, finally, the only non-constructor member function to
// retrieve events from the iterator.

Event *
oncsetEventiterator::getNextEvent()
{
  Event *evt;
  int status;

  if (!et_alive(id)) 
    {
      COUT << "not alive" << std::endl;
      return 0;
    }
  status = et_event_get(id, attach1, &pe, ET_SLEEP, 0);
  if (status != ET_OK) 
    {
      COUT << "not ok" << std::endl;
      return 0;
    }
 
  int *data;
  int len;
  et_event_getdata(pe, (void **) &data);
  et_event_getlength(pe,&len);
 
  evt = new oncsEvent(data);
  evt->convert();

  status = et_event_put(id, attach1, pe); 

  return evt;

}

int *oncsetEventiterator::getNextEventData()
{
  int status;

  if (!et_alive(id)) 
    {
      COUT << "not alive" << std::endl;
      return 0;
    }
  status = et_event_get(id, attach1, &pe, ET_SLEEP, 0);
  if (status != ET_OK) 
    {
      COUT << "not ok" << std::endl;
      return 0;
    }
 
  int *data;
  et_event_getdata(pe, (void **) &data);

  return data;

}


int oncsetEventiterator::releaseEventData()
{
  int status;
  status = et_event_put(id, attach1, pe); 

  if (status != ET_OK)
    {
      return -1;
    }
  return 0;
}
  
void oncsetEventiterator::setBlockingMode(const int mode)
{
  if ( mode != ET_STATION_NONBLOCKING &&  mode != ET_STATION_BLOCKING) 
    {
      COUT << "Wrong blocking mode, unchanged" << std::endl;
      return;
    }
  et_station_config_setblock(sconfig, mode);
}

int oncsetEventiterator::getBlockingMode() const
{
  int val;
  et_station_getblock(id, my_stat, &val);
  return val;
}


void oncsetEventiterator::setSelectMode(const int mode)
{
  if ( mode != ET_STATION_SELECT_ALL &&
       //    mode != ET_STATION_SELECT_USER &&  ...will be implemented later
       mode != ET_STATION_SELECT_MATCH)
    {
      COUT << "Wrong Select mode, unchanged" << std::endl;
      return;
    }

  et_station_config_setselect(sconfig, ET_STATION_SELECT_ALL);
}

int oncsetEventiterator::getSelectMode() const
{
  int val;
  et_station_getselect(id, my_stat, &val);
  return val;
}
 

void oncsetEventiterator::setSelectWords(const int i1, const int i2, const int i3, const int i4 )
{
  int val[4]={i1,i2,i3,i4};

  et_station_config_setselectwords(sconfig, val);
}


void oncsetEventiterator::getSelectWords (int val[]) const
{
  et_station_getselect(id, my_stat, val);
}

