/*----------------------------------------------------------------------------*
 *  Copyright (c) 2000        Southeastern Universities Research Association, *
 *                            Thomas Jefferson National Accelerator Facility  *
 *                                                                            *
 *    This software was developed under a United States Government license    *
 *    described in the NOTICE file included as part of this distribution.     *
 *                                                                            *
 * TJNAF Data Acquisition Group, 12000 Jefferson Ave., Newport News, VA 23606 *
 *      heyes@cebaf.gov   Tel: (804) 269-7030    Fax: (804) 269-5800          *
 *----------------------------------------------------------------------------*
 * Description:
 *      Java class for containing attachment data from an ET system
 *
 * Author:
 *	Carl Timmer
 *	TJNAF Data Acquisition Group
 *
 * Revision History:
 *
 *----------------------------------------------------------------------------*/

import java.lang.*;
import java.io.*;

class EtAttachmentData {
  private int num;		// unique id #
  private int proc;		// id # of proc that created this attachment
  private int stat;		// id # of station attached to
  private int pid;		// pid of unix process that created this att
  private int blocked;		// is blocked waiting to read events?
  private int quit;		// has it been told to quit reading & return
  private int eventsOwned;	// # of events owned

  private long eventsPut;	// # of events put back into station
  private long eventsGet;	// # of events read from station
  private long eventsDump;	// # of events dumped
  private long eventsMake;	// # of new events made

  private String host;		// name of host running att
  private String stationName;	// name of station attached to
  
  
  public int    getNum()         {return num;}
  public int    getProc()        {return proc;}
  public int    getStat()        {return stat;}
  public int    getPid()         {return pid;}
  public int    getBlocked()     {return blocked;}
  public int    getQuit()        {return quit;}
  public int    getEventsOwned() {return eventsOwned;}
  public long   getEventsPut()   {return eventsPut;}
  public long   getEventsGet()   {return eventsGet;}
  public long   getEventsDump()  {return eventsDump;}
  public long   getEventsMake()  {return eventsMake;}
  public String getHost()        {return new String(host);}
  public String getStationName() {return new String(stationName);}
  
  
  public void read(DataInputStream dis) throws IOException {
    num         = dis.readInt();
    proc        = dis.readInt();
    stat        = dis.readInt();
    pid         = dis.readInt();
    blocked     = dis.readInt();
    quit        = dis.readInt();
    eventsOwned = dis.readInt();
    eventsPut   = dis.readLong();
    eventsGet   = dis.readLong();
    eventsDump  = dis.readLong();
    eventsMake  = dis.readLong();
    
    // read strings, lengths first
    int length1, length2;
    byte buf[];
    
    length1 = dis.readInt();
    length2 = dis.readInt();

    buf = new byte[length1];
    dis.read(buf, 0, length1);
    host = new String(buf, 0, length1-1, "ASCII");
    
    buf = new byte[length2];
    dis.read(buf, 0, length2);
    stationName = new String(buf, 0, length2-1, "ASCII");
  }
}



