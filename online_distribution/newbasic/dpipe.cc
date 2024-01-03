#include <stdlib.h>
#include <signal.h>
#include <dlfcn.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <fileEventiterator.h>
#include <testEventiterator.h>
#include <listEventiterator.h>
#include <pfsEventiterator.h>

#ifndef WIN32
#include <unistd.h>
#ifndef OSF1
#include <etEventiterator.h>
#endif
#include <ogzBuffer.h>
#include <olzoBuffer.h>
#include <oamlBuffer.h>
#else
#include <oBuffer.h>
#endif

#include "dpipe_filter.h"

#include <phenixTypes.h>
#include <oEvent.h>
#include <stdio.h>
#include <EventTypes.h>
#ifdef HAVE_GETOPT_H
#ifndef WIN32
#include "getopt.h"
#else
#include "win_getopt.h"
#endif
#endif

#define ETEVENTITERATOR 1
#define FILEEVENTITERATOR 2
#define TESTEVENTITERATOR 3
#define ETPOOL 4
#define DFILE 5
#define DNULL 6
#define LISTEVENTITERATOR 7
#define PFSEVENTITERATOR 8
#define OAML 8


#ifndef WIN32
#if defined(SunOS) || defined(Linux) || defined(OSF1) 
void sig_handler(int);
#else
void sig_handler(...);
#endif
#endif


void exitmsg()
{
  COUT << "** usage: dpipe -s -d -v -w -n -i -z -l -x source destination" << std::endl;
  COUT << "          dpipe -h for more help" << std::endl;
  exit(0);
}

void evtcountexitmsg()
{
  COUT << "** cannot specify both -e and -c!" << std::endl;
  COUT << "    type  dpipe -h  for more help" << std::endl;
  exit(0);
}

void compressionexitmsg()
{
  COUT << "** cannot specify both -z and -l!" << std::endl;
  COUT << "    type  dpipe -h  for more help" << std::endl;
  exit(0);
}

void exithelp()
{
  COUT <<  std::endl;
  COUT << " dpipe reads events from one source and writes it to a destination. The source can" << std::endl;
  COUT << " be any of the standard sources (ET pool, file, filelist, or test stream). The destination " << std::endl;
  COUT << " can be a file, a ET pool, an AML server, or no destination at all (you cannot write to a test " << std::endl;
  COUT << " stream). Like with a Unix pipe, you can move events through a chain of sources " << std::endl;
  COUT << " and destinations, for example, from one ET pool into another, or into a file. " << std::endl;
  COUT <<  std::endl;
  COUT << " While the events move through dpipe, you can have them identify themselves. If the " << std::endl;
  COUT << " destination is null, this is a simple way to sift through a stream of events " << std::endl;
  COUT << " and look at their identification messages. " << std::endl;
  COUT <<  std::endl;
  COUT << " You can throttle the data flow with the -w (wait) option, and you can stop after" << std::endl;
  COUT << " a given number of events with the -n option. " << std::endl;
  COUT <<  std::endl;
  COUT << " In order to write events from a ET pool called ONLINE to a file (d.evt), use" << std::endl;
  COUT <<  std::endl;
  COUT << " > dpipe -s etpool -d file ONLINE d.evt" << std::endl;
  COUT <<  std::endl;
  COUT << " if you want to see which events are coming, add -i:" << std::endl;
  COUT <<  std::endl;
  COUT << " > dpipe -s etpool -d file -i ONLINE d.evt" << std::endl;
  COUT <<  std::endl;
  COUT << " If the output is a aml server, specify the destination as hostname:port: " << std::endl;
  COUT << " > dpipe -s f -d a -i filename phnxbox4.phenix.bnl.gov:8900" << std::endl << std::endl;
  COUT << " Note that you can abbreviate the etpool, file, listfile, and Test to d, f, l, and T." << std::endl;
  COUT << " > dpipe -s etpool -d file  ONLINE d.evt" << std::endl;
  COUT << " is equivalent to " << std::endl;
  COUT << " > dpipe -s d  -d f  ONLINE d.evt" << std::endl;
  COUT <<  std::endl;
  COUT << "  List of options: " << std::endl;
  COUT << " -s [d or f or l or T or p] source is et pool, file, listfile, Test stream, or pfs server" << std::endl;
  COUT << " -b <size in MB> in case you write to a file, specify the buffer size (default 4MB)" << std::endl;
  COUT << " -d [d or f or a  or n] destination is et pool or file , aml server or nothing" << std::endl;
  COUT << " -v verbose" << std::endl;
  COUT << " -w (time in milliseconds> wait time interval (in ms) between events to throttle the data flow" << std::endl;
  COUT << " -e <event number>  start from event number" << std::endl;
  COUT << " -c <number> get nth event (-e gives event with number n)" << std::endl;
  COUT << " -n <number> stop after so many events" << std::endl;
  COUT << " -i have each event identify itself" << std::endl;
  COUT << " -z gzip-compress each output buffer" << std::endl;
  COUT << " -l LZO-compress each output buffer" << std::endl;
  COUT << " -x sharedlibrary.so load a plugin that can select events" << std::endl;
  COUT << " -h this message" << std::endl << std::endl;
  exit(0);
}


// The global pointer to the Eventiterator (we must be able to 
// get at it in the signal handler) 
Eventiterator *it;

#if !defined(WIN32) && !defined(OSF1)
et_att_id	  attach1;
et_sys_id       id;
et_openconfig   openconfig;
et_event       *pe;
#endif
int we_use_et;

char *sharedlib;
int load_lib = 0;

DpipeFilter *filter = 0;


#if !defined(WIN32) && !defined(OSF1)
int insertinPool (Event *evt)
{
  char *pdata;
  // evt->getEvtLength() is words, et needs length in bytes (len*4)
  // <<2 is same as *4
  et_event_new(id, attach1, &pe, ET_SLEEP, NULL, (evt->getEvtLength())<<2);

  et_event_getdata(pe, (void **) &pdata);
  int nw;
  evt->Copy((int *) pdata, evt->getEvtLength(), &nw);
  
  et_event_setlength(pe, (evt->getEvtLength())<<2 );      
  static int ctrl[4] = {-1,-1,-1,-1};
  ctrl[0] = evt->getEvtType();
  // this will make non data events pass the et trigger selection
  if (ctrl[0] != DATAEVENT)
    {
      ctrl[1] = 0xFFFFFFFF;
      ctrl[3] = 0xFFFFFFFF;
    }
  else
    {
      ctrl[1] = evt->getTagWord(0);
      ctrl[3] = evt->getTagWord(1);
    }
  et_event_setcontrol(pe,ctrl,4);
  et_event_put(id, attach1, pe);


  return 0;
}
#endif

void dpipe_register ( DpipeFilter *T)
{
  if ( filter ) delete filter;
  filter = T;

}

void dpipe_unregister ( DpipeFilter *T)
{
  if ( filter && T == filter ) 
    {
      filter = 0;
    }

}




int 
main(int argc, char *argv[])
{
  int c;
  int status = false;

  int sourcetype =DFILE;
  int destinationtype = ETPOOL;
  int waitinterval = 0;
  int verbose = 0;
  int identify = 0;
  int maxevents = 0;
  int eventnr = 0;
  int gzipcompress = 0;
  int lzocompress = 0;
  int eventnumber =0;
  int countnumber =0;
  we_use_et = 0;
  void *voidpointer;


  PHDWORD  *buffer;
  oBuffer *ob = 0;
  int fd = 0;
  int buffer_size = 256*1024*4 ;  // makes  4MB (specifies how many dwords, so *4)


  // initialize the it pointer to 0;
  it = 0;

  //  if (argc < 3) exitmsg();
  //	COUT << "parsing input" << std::endl;

#ifndef WIN32
  while ((c = getopt(argc, argv, "e:b:c:s:d:n:w:x:vhizl")) != EOF)
    {
      switch (c) 
	{
	  // the -s (source type) switch
	case 'e':
	  if ( !sscanf(optarg, "%d", &eventnumber) ) exitmsg();
	  break;

	case 'b':
	  if ( !sscanf(optarg, "%d", &buffer_size) ) exitmsg();
	  buffer_size = buffer_size*256*1024;
	  break;

	case 'c':
	  if ( !sscanf(optarg, "%d", &countnumber) ) exitmsg();
	  break;

	case 's':
	  if ( *optarg == 'T' ) sourcetype = TESTEVENTITERATOR;
	  else if ( *optarg == 'f' ) sourcetype = FILEEVENTITERATOR;
	  else if ( *optarg == 'e' ) sourcetype = ETEVENTITERATOR;
	  else if ( *optarg == 'l' ) sourcetype = LISTEVENTITERATOR;
	  else if ( *optarg == 'p' ) sourcetype = PFSEVENTITERATOR;
	  else  exitmsg();
	  break;
					
	  // the -d (destination type) switch
	case 'd':
	  if ( *optarg == 'd' ) destinationtype = ETPOOL;
	  else if ( *optarg == 'f' ) destinationtype = DFILE;
	  else if ( *optarg == 'n' ) destinationtype = DNULL;
	  else if ( *optarg == 'e' ) destinationtype = ETPOOL;
	  else if ( *optarg == 'a' ) destinationtype = OAML;
	  else exitmsg();
	  break;

	case 'v':   // verbose
	  verbose++;
	  break;

	case 'i':   // identify
	  identify = 1;
	  break;

	case 'w':   // wait interval
	  if ( !sscanf(optarg, "%d", &waitinterval) ) exitmsg();
	  break;

	case 'n':   // number of events
	  if ( !sscanf(optarg, "%d", &maxevents) ) exitmsg();
	  break;

	case 'z':   // gzip-compress
	  gzipcompress = 1;
	  break;

	case 'l':   // lzo-compress
	  lzocompress = 1;
	  break;

	case 'x':   // load a filter shared lib
	  voidpointer = dlopen(optarg, RTLD_GLOBAL | RTLD_NOW);
	  if (!voidpointer) 
	    {
	      std::cout << "Loading of the filter library " 
		   << optarg << " failed: " << dlerror() << std::endl;

	    }
	  if (filter) std::cout <<" Filter \"" << filter->idString() << "\" registered" << std::endl; 

	  sharedlib=optarg;
	  load_lib=1;
	  break;

	case 'h':
	  exithelp();
	  break;

	default:
	  break;
						
	}
    }
#else
  char* pszParam;             // gotten parameter
  char chOpt;

  while ( (chOpt = GetOption(argc, argv, "e:b:c:s:d:n:w:vhiz", &pszParam) ) >1)
    {
      // COUT << "option is " << chOpt  << std::endl;
      // chOpt is valid argument
      switch (chOpt)
	{
	case 'e':
	  //COUT << "parameter is " <<  pszParam << std::endl;
	  if ( !sscanf(pszParam, "%d", &eventnumber) ) exitmsg();
	  break;
	  
	case 'b':
	  //COUT << "parameter is " <<  pszParam << std::endl;
	  if ( !sscanf(pszParam, "%d", &buffer_size) ) exitmsg();
	  buffer_size = buffer_size*256*1024;
	  break;

	case 'c':
	  //COUT << "parameter is " <<  pszParam << std::endl;
	  if ( !sscanf(pszParam, "%d", &countnumber) ) exitmsg();
	  break;

	case 's':
	  //COUT << "parameter is " <<  pszParam << std::endl;
	  if ( *pszParam == 'T' ) sourcetype = TESTEVENTITERATOR;
	  else if ( *pszParam == 'f' ) sourcetype = FILEEVENTITERATOR;
	  else  exitmsg();
	  break;
					
	  // the -d (destination type) switch
	case 'd':
	  // COUT << "parameter is " <<  pszParam << std::endl;
	  if ( *pszParam == 'f' ) destinationtype = DFILE;
	  else if ( *pszParam == 'n' ) destinationtype = DNULL;
	  else exitmsg();
	  break;

	case 'v':   // verbose
	  verbose = 1;
	  break;

	case 'i':   // identify
	  identify = 1;
	  break;

	case 'w':   // wait interval
	  if ( !sscanf(pszParam, "%d", &waitinterval) ) exitmsg();
	  break;

	case 'n':   // number of events
	  //COUT << "parameter is " <<  pszParam << std::endl;
	  if ( !sscanf(pszParam, "%d", &maxevents) ) exitmsg();
	  break;

	case 'z':   // gzip-compress
	  gzipcompress = 1;
	  break;

	case 'h':
	  exithelp();
	  break;

	default:
	  break;
	   
	 }
     }
#endif


  if ( eventnumber && countnumber) evtcountexitmsg();
  if ( gzipcompress && lzocompress ) compressionexitmsg();

  // install some handlers for the most common signals
#ifndef WIN32
  signal(SIGKILL, sig_handler);
  signal(SIGTERM, sig_handler);
  signal(SIGINT,  sig_handler);
#endif

  // see if we can open the file
  switch (sourcetype)
    {
    case ETEVENTITERATOR:

#if !defined(WIN32) && !defined(OSF1)
      it = new etEventiterator(argv[optind], status);
#else
      status=1;
#endif
      break;

    case  TESTEVENTITERATOR:
      
      it = new testEventiterator();
      status =0;
      break;

    case  FILEEVENTITERATOR:
#ifndef WIN32
      it = new fileEventiterator(argv[optind], status);
#else
      //      COUT <<  " filename  is " << pszParam << std::endl;      
      it = new fileEventiterator(pszParam, status);
#endif
      break;
       
#ifndef WIN32
    case  LISTEVENTITERATOR:
      it = new listEventiterator(argv[optind], status);
      break;
    case  PFSEVENTITERATOR:
      it = new pfsEventiterator(argv[optind], status);
      break;
#endif
       
    default:
      exitmsg();
      break;
    }

  if (status)
    {
      delete it;
      COUT << "Could not open input stream" << std::endl;
      exit(1);
    }

  // set the verbosity of the iterator
  it->setVerbosity(verbose);

  // now we'll see where the data go to

  if ( destinationtype == DNULL )
    {
      //	identify = 1;
    }
#if !defined(WIN32) && !defined(OSF1)

  else if ( destinationtype == ETPOOL )
    {
      we_use_et = 1;
      /* open ET system */
      et_open_config_init(&openconfig);

      et_open_config_sethost(openconfig,ET_HOST_ANYWHERE);

      if (et_open(&id, argv[optind+1], openconfig) != ET_OK) 
	{
	  COUT << "could not open the ET pool " << argv[optind+1] << std::endl;
	  exit(1);
	}
      et_open_config_destroy(openconfig);

 
      /* set level of debug output (everything) */
      et_system_setdebug(id, ET_DEBUG_INFO);
  
      /* attach to grandcentral station */
      if (et_station_attach(id, ET_GRANDCENTRAL, &attach1) < 0) {
	COUT << "error in et_station_attach" << std::endl;
	exit(1);
      }

    }
#endif

  else if (  destinationtype == DFILE)
    {
      buffer = new PHDWORD [buffer_size];
#ifndef WIN32
      unlink (argv[optind+1]);
 
      fd = open (argv[optind+1], O_WRONLY | O_CREAT | O_EXCL | O_LARGEFILE , 
		  S_IRWXU | S_IROTH | S_IRGRP );

      if ( fd < 0) 
	{
	  COUT << "Could not open file: " <<  argv[optind+1] << std::endl;
	  exit (1);
	}
      if (gzipcompress) 
	{
	  ob = new ogzBuffer (fd, buffer, buffer_size);
	}
      else if ( lzocompress)
	{
	  ob = new olzoBuffer (fd, buffer, buffer_size);
	}
      else
	{
	  ob = new oBuffer (fd, buffer, buffer_size);
	}
#else
      chOpt = GetOption(argc, argv, "e:c:s:d:n:w:vhiz", &pszParam);
      int status;
      ob = new oBuffer (pszParam, buffer, buffer_size, status);
      if (status)
	{
	  exit(1);
	}
#endif



    }


  else if (  destinationtype == OAML)
    {
      buffer = new PHDWORD [buffer_size];
      ob = new oamlBuffer (argv[optind+1], buffer, buffer_size);
    }

			
  else 
    {
      COUT << "invalid destination" << std::endl;
      exitmsg();
    }
	

  //  COUT << "waitinterval is " << waitinterval << std::endl;
  // ok. now go through the events
  Event *evt;
  int take_this;
  int count = 0;
  //COUT << " max events = " <<  maxevents << std::endl;
  while ( ( maxevents == 0 || eventnr < maxevents)  &&
	  ( evt = it->getNextEvent())  )
    {
      take_this = 1;
      count++;

      if ( eventnumber )
	{
	  if ( evt->getEvtSequence() == eventnumber)
	    eventnumber = 0;
	  else
	    take_this = 0;
	}

      if ( countnumber && count < countnumber)
	take_this = 0;

      if (take_this)
	{
	  if ( (! filter) ||  filter->select(evt) )
	    {
	      if (identify) evt->identify();
	      if ( destinationtype == DFILE || destinationtype == OAML )
		{
		  status = ob->addEvent(evt);
		}
#if !defined(WIN32) && !defined(OSF1)
	      else if ( destinationtype == ETPOOL )
		{
		  status = insertinPool(evt);
		}
#endif

	      if ( status ) 
		{
		  COUT << "Error writing events " << std::endl;
		  break;
		}
	      eventnr++;
	    }
	}
      
      delete evt;
      
#ifndef WIN32
      if (waitinterval > 0) usleep(waitinterval*1000);
#else
      if (waitinterval > 0) Sleep(waitinterval);
#endif

    }
  delete it;

  if ( destinationtype == DFILE  )
    {
      delete ob;
      
#ifndef WIN32
      close(fd);
#endif
    }

#ifndef WIN32
  else if ( destinationtype == OAML  )
    {
      delete ob;
    }
#endif

  
  return 0;
}

#ifndef WIN32
#if defined(SunOS) || defined(Linux) || defined(OSF1) 
void sig_handler(int i)
#else
  void sig_handler(...)
#endif
{
  COUT << "sig_handler: signal seen " << std::endl;
  if (it) delete it;

#ifndef OSF1
  if (we_use_et)
    {
      et_station_detach(id, attach1);
      sleep(2);
    }
#endif
  exit(0);
}
#endif

  
