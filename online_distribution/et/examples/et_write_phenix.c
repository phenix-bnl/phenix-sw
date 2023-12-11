#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/time.h>
#include <time.h>
#ifdef sun
#include <thread.h>
#endif
#include "et.h"

#include "phenixTypes.h"

#define NUMEVENTS 20000
#define CHUNK 100

  FILE *fp;

/* prototype */
static void * signal_thread (void *arg);
int write_buffer (DWORD data[], FILE *fp, int *nw);

int main(int argc,char **argv) {  
  int             i, j, status, swtch, numread=0, totalread=0;
  int		  con[ET_STATION_SELECT_INTS];
  pthread_t       tid;
  et_statconfig   sconfig;
  et_event       *pe;
  et_openconfig   openconfig;
  sigset_t        sigblock;
  struct timespec timeout;
  et_att_id       attach1;
  et_stat_id      my_stat;
  et_sys_id       id;
  int             selections[] = {17,15,-1,-1}; /* 17,15 are arbitrary */

  unsigned long int *bigevent;
          double time;
	  struct timeval t1;
	  struct timeval t2;

 
  if(argc != 4) {
    printf("Usage: et_client <et_filename> <station_name> <mode>\n");
    exit(1);
  }


  timeout.tv_sec  = 2;
  timeout.tv_nsec = 0;

  printf("opening file aargh_et.evt\n");

  /*************************/
  /* setup signal handling */
  /*************************/
  /* block all signals */
  sigfillset(&sigblock);
  status = pthread_sigmask(SIG_BLOCK, &sigblock, NULL);
  if (status != 0) {
    printf("et_client: pthread_sigmask failure\n");
    exit(1);
  }

#ifdef sun
  /* prepare to run signal handling thread concurrently */
  thr_setconcurrency(thr_getconcurrency() + 1);
#endif

  /* spawn signal handling thread */
  pthread_create(&tid, NULL, signal_thread, (void *)NULL);
  
restartLinux:
  /* open ET system */
  et_open_config_init(&openconfig);
  et_open_config_setwait(openconfig, ET_OPEN_WAIT);
  if (et_open(&id, argv[1], openconfig) != ET_OK) {
    printf("et_client: et_open problems\n");
    exit(1);
  }
  et_open_config_destroy(openconfig);
  
  swtch = atoi(argv[3]);
    
  et_station_config_init(&sconfig);
  et_station_config_setuser(sconfig, ET_STATION_USER_MULTI);
  et_station_config_setrestore(sconfig, ET_STATION_RESTORE_OUT);
  et_station_config_setprescale(sconfig, 1);
  et_station_config_setcue(sconfig, 150);
 
  if (swtch==1) {
      /* DD system "all" mode */
    printf("opening et system all mode\n");
      et_station_config_setselect(sconfig, ET_STATION_SELECT_ALL);
      et_station_config_setblock(sconfig, ET_STATION_BLOCKING);
  }
  else if (swtch==2) {
      /* DD system "on req" mode */
      et_station_config_setselect(sconfig, ET_STATION_SELECT_ALL);
      et_station_config_setblock(sconfig, ET_STATION_NONBLOCKING);
  }
  else if (swtch==3) {
      /* DD system "condition" mode */
      et_station_config_setselect(sconfig, ET_STATION_SELECT_MATCH);
      et_station_config_setblock(sconfig, ET_STATION_BLOCKING);
      et_station_config_setselectwords(sconfig, selections);
  }
  else if (swtch==4) {
      /* new non-blocking "condition" mode */
      et_station_config_setselect(sconfig, ET_STATION_SELECT_MATCH);
      et_station_config_setblock(sconfig, ET_STATION_NONBLOCKING);
      et_station_config_setselectwords(sconfig, selections);
  }
  else if (swtch==5) {
      /* user's condition, blocking  mode */
      et_station_config_setselect(sconfig, ET_STATION_SELECT_USER);
      et_station_config_setblock(sconfig, ET_STATION_BLOCKING);
      et_station_config_setselectwords(sconfig, selections);
      if (et_station_config_setfunction(sconfig, "et_carls_function") == ET_ERROR) {
	printf("et_client: cannot set function\n");
	exit(1);
      }
      if (et_station_config_setlib(sconfig, "/home/timmer/cvs/coda/source/et/src/libet_user.so") == ET_ERROR) {
        printf("et_client: cannot set library\n");
	exit(1);
      }
  }
  else if (swtch==6) {
      /* user's condition, nonblocking mode */
      et_station_config_setselect(sconfig, ET_STATION_SELECT_USER);
      et_station_config_setblock(sconfig, ET_STATION_NONBLOCKING);
      et_station_config_setselectwords(sconfig, selections);
      et_station_config_setfunction(sconfig, "et_carls_function");
      et_station_config_setlib(sconfig, "/home/timmer/cvs/coda/source/et/src/libet_user.so");
  }
  
    /* set debug level */
    et_system_setdebug(id, ET_DEBUG_INFO);
  
    if ((status = et_station_create(id, &my_stat, argv[2], sconfig)) < ET_OK) {
      if (status == ET_ERROR_EXISTS) {
        /* my_stat contains pointer to existing station */;
        printf("et_client: station already exists\n");
      }
      else if (status == ET_ERROR_TOOMANY) {
        printf("et_client: too many stations created\n");
        goto error;
      }
      else {
        printf("et_client: error in station creation\n");
        goto error;
      }
    }
    et_station_config_destroy(sconfig);

    if (et_station_attach(id, my_stat, &attach1) < 0) {
      printf("et_client: error in station attach\n");
      goto error;
    }
   
  fp = fopen("aargh_et.evt","w");
     	      gettimeofday(&t1, NULL);
    while (et_alive(id)) {
      /**************/
      /* get events */
      /**************/
    
      /* example of single, timeout read */
      /* status = et_event_get(id, attach1, &pe[0], ET_TIMED, &timeout); */
    
      /* example of single, asynchronous read */
      /* status = et_event_get(id, attach1, &pe[0], ET_ASYNC, NULL);*/
  
      /* example of reading array of up to "CHUNK" events */
      /* status = et_events_get(id, attach1, pe, ET_SLEEP, NULL, CHUNK, &numread);*/
      
      /* example of array, timeout read */
      /* status = et_events_get(id, attach1, pe, ET_TIMED, &timeout, CHUNK, &numread);*/
      
      status = et_event_get(id, attach1, &pe, ET_SLEEP,  &timeout);
      if (status == ET_OK) {
	numread++;
      }
      else if (status == ET_ERROR_DEAD) {
        printf("et_client: ET system is dead\n");
        goto end;
      }
      else if (status == ET_ERROR_TIMEOUT) {
        printf("et_client: got timeout\n");
        goto end;
      }
      else if (status == ET_ERROR_EMPTY) {
        printf("et_client: no events\n");
        goto end;
      }
      else if (status == ET_ERROR_BUSY) {
        printf("et_client: station is busy\n");
        goto end;
      }
      else if (status == ET_ERROR_WAKEUP) {
        printf("et_client: someone told me to wake up\n");
        goto end;
      }
      else if ((status == ET_ERROR_WRITE) || (status == ET_ERROR_READ)) {
        printf("et_client: socket communication error\n");
        goto end;
      }
      else if (status != ET_OK) {
        printf("et_client: get error\n");
        goto error;
      }
      {
        DWORD *data;
        int len;
        et_event_getdata(pe, (void **) &data);
        et_event_getlength(pe,&len);
        *data = len;
	/*
        // printf("getting event at %x, len: %d, numread: %d\n",data,len,numread);
        // printf("write buffer status: %d\n",status = write_buffer (data,fp, &len));
	*/
        if (status = write_buffer (data,fp, &len))
        printf("error writing buffer: %x\n",status);
	/*
        if (!( bigevent=malloc(len)))
	  {
            printf("malloc of %d bytes failed\n",len);
          }
        else
	  {
            memcpy( (void *) bigevent, (void *) data, len);
          }        
	*/
      }
      if (numread > 1000)
	{ 
     	 gettimeofday(&t2, NULL);
              time = (double)(t2.tv_sec - t1.tv_sec) + 1.e-6*(t2.tv_usec - t1.tv_usec);
 printf(" end run -- number of events: %d, elapsed time: %f sec\n",numread,time);
         fclose(fp);
         exit(0);
	}
      /**************/
      /* print data */
      /**************/
      if (0) {
        int pri, len;
	int *data;
	/* char *data; */
	
        for (j=0; j< numread; j++) {
	  et_event_getdata(pe, (void **) &data);
	  et_event_getpriority(pe, &pri);
	  et_event_getlength(pe, &len);
          et_event_getcontrol(pe, con);
          /* printf("et_client data = %s, pri = %d, len = %d\n", data, pri, len); */
          printf("et_client data = %x %x %x %x %x, pri = %d, len = %d\n", *data,*(data+1),*(data+2),*(data+3),*(bigevent+4),  pri, len);
          for (i=0; i < ET_STATION_SELECT_INTS; i++) {
            printf("          con[%d] = %d\n", i, con[i]);
	  }
        }
      }
      /*                  free(bigevent); */
      /**************/
      /* put events */
      /**************/

      /* example of putting single event */
      status = et_event_put(id, attach1, pe); 
    
      /* example of putting array of events */
      /* status = et_events_put(id, attach1, pe, numread); */
  
      if (status == ET_ERROR_DEAD) {
        printf("et_client: ET is dead\n");
        goto end;
      }
      else if ((status == ET_ERROR_WRITE) || (status == ET_ERROR_READ)) {
        printf("et_client: socket communication error\n");
        goto end;
      }
      else if (status != ET_OK) {
        printf("et_client: put error\n");
        goto error;
      }
      totalread = numread;
      
      end:
        /* print something out after having read NUMEVENTS events */
        if (totalread >= NUMEVENTS) {
          totalread = 0;
          numread = 0;
          printf("et_client: %d events\n", NUMEVENTS);
        }
        /* if ET system is dead, wait here until it comes back */
	while (!et_alive(id)) {
	  status = et_wait_for_alive(id);
	  if (status == ET_OK) {
	    int locality;
	    et_system_getlocality(id, &locality);
	    /* if Linux, re-establish connection to ET system since socket broken */
	    if (locality == ET_LOCAL_NOSHARE) {
              printf("et_client: try to reconnect Linux client\n");
	      et_forcedclose(id);
	      goto restartLinux;
	    }
	  }
	}
    } /* while(alive) */
    
    error:
      free(pe);
      printf("et_client: ERROR\n");
      exit(0);
}

/************************************************************/
/*              separate thread to handle signals           */
static void * signal_thread (void *arg)
{
  sigset_t   signal_set;
  int        sig_number;
 
  sigemptyset(&signal_set);
  sigaddset(&signal_set, SIGINT);
  
  /* Not necessary to clean up as ET system will do it */
  sigwait(&signal_set, &sig_number);
  printf("et_client: got a control-C, exiting\n");
  fclose(fp);
  exit(1);
}

int write_buffer (DWORD bp[], FILE *fp, int *nbytes)
{
  int ip =0;
  char *cp = (char *) bp;
  *nbytes = 0;
  while (ip<bp[0])
    {
      int iiw  =fwrite ( cp, 8192, 1, fp);
      if (ferror(fp)) 
	  {
	    return -1;
	  }
      cp += 8192;
      ip+=8192;
      *nbytes+= 8192;
    }
  return 0;
}


