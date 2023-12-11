
// build this thing by
// g++ -I$ONLINE_MAIN/include oBufferExample.cc -o oBufferExample -L$ONLINE_MAIN/lib -lNoRootEvent
  
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <ogzBuffer.h>
#include <olzoBuffer.h>

#include <phenixTypes.h>
#include <packetConstants.h>
#include <EventTypes.h>
#include <oEvent.h>

#include <iostream>


// we are using the oBuffer  (or one of its derivatives olzoBuffer or ogzBuffer)
// to assemble events from scratch (individual chunks of packet payload data).
// The oBuffer opens a file. With each nextEvent(...) call, we are starting the
// assembly of a new event; the object will wrap up a previous event, if any. 
// After a nextEvent call, call addUnstructPacketData once for each packet.
// Start the next event with the nextEvent call, or after the last event, delete the 
// object to flush the current buffer.
// Note that the object manages the writing on its own. When the next event 
// does no longer fit into the buffer, the buffer is flushed and a new buffer 
// is started.

// It is important to understand that the event length parameter in nextEvent
// denotes the *maximum* length an event can assume. With zero-suppression and
// all, the events are virtually always smaller than that, but we need to take care
// of the worst-case scenario (in an actual DAQ, an event that doesn't get 
// zero-supressed, such as a PPG event where the laser fires all channels or so).
// The events will take just their actual space in the buffer. Unless we are 
// excessively generous with length, it doesn't matter much if our 
// length is a bit too long.



int 
main(int argc, char *argv[])
{

  if (argc <2)
    {
      std::cout << "usage " << argv[0] << " output_filename" << std::endl;
      return 1;
    }


  int i;
  int status;
  int count;

  DWORD  *buffer; // some work memory to hold the buffer
  oBuffer *ob;    

  int buffer_size = 256*1024*4 ;  // makes  4MB (specifies how many dwords, so *4)

  buffer = new DWORD [buffer_size];
  
  // some parameters:
  int irun = 20001; // run number
  int p1_id = 35001; // packet id 1
  int p2_id = 35002; // packet id 2
  
  ob = new oBuffer (argv[1], buffer, buffer_size, status, irun); 
  if ( status ) 
    {
      std::cout << "Could not open file: " <<  argv[1] << std::endl;
      return 1;
    }

  // also try one day
  // ob = new olzoBuffer (argv[1], buffer, buffer_size, status, irun);



  // we make some memory that we fill with some nice numbers that we write out
  // over and over in 2 packets. This array would normally come from some readout device 
  // and change in content AND length for each event.

  DWORD p1[1000];
  DWORD p2[2000];

  for (i= 0; i < 1000; i++)
    {
      p1[i] = i;
    }

  for (i= 0; i < 2000; i++)
    {
      p2[i] = 2*i+1;
    }

  int max_evt_length = 1000 + 2000 + 200; // 200 is too much, but see above

  // if you have to know: payload + packetcount*6 + framecount*8 + 8 (evt header) 
  // 3000 + 2*6 + 1*8 + 8 = 3028 in this case

  // we add an empty begin-run event just to be meticulous
  ob->nextEvent( max_evt_length, BEGRUNEVENT ); 
      
  for ( count = 0; count < 1000; count++)
    {
      // just calling nextEvent wraps up the previous event, if any
      ob->nextEvent( max_evt_length, DATAEVENT); // we start assembling a new data event

      // Packet 1
      ob->addUnstructPacketData (p1, 1000, p1_id, 4, ID4EVT);

      // Packet 2
      ob->addUnstructPacketData (p2, 2000, p2_id, 4, ID4EVT);
      
    }

  // we add an empty end-run event 
  ob->nextEvent( max_evt_length, ENDRUNEVENT ); 
      
  // deleting the object will flush the current buffer
  delete ob;

  return 0;
}



