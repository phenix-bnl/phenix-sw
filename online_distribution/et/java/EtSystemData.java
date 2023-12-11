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
 *      Java class for containing system data from an ET system
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

class EtSystemData {
  // values which can change
  private int alive;		// is system alive?
  private int heartbeat;	// heartbeat count
  private int temps;		// current # of temp events
  private int stations;		// current # of stations (idle or active)
  private int attachments;	// current # of attachments
  private int processes;	// current # of processes
  private int eventsOwned;	// # of events owned by the system
  private int mutex;		// is system mutex locked?
  private int statMutex;	// is station mutex locked ?
  private int statAddMutex;	// is add station mutex locked ?
  
  // values which do NOT change
  private int endian;		// endian value of host running system
  private int share;		// can system share mutexes between processes?
  private int mainPid;		// pid of ET system process
  private int selects;		// number of ints in station's select array
  private int events;		// total # of events
  private int eventSize;	// size of "normal" events in bytes
  private int tempsMax;		// max # of temp events allowed
  private int stationsMax;	// max # of stations allowed
  private int attachmentsMax;	// max # of attachments allowed
  private int processesMax;	// max # of processes allowed
  
  private int tcpPort;		// port of ET server
  private int udpPort;		// port for udp broad/multicast to find server
  
  private int interfaceCount;	// # of network interfaces
  private int multicastCount;	// # of multicast addresses
  
  private String interfaceAddresses[];	// dotted-decimal ip addrs of network interfaces
  private String multicastAddresses[];	// dotted-decimal ip multicast addrs
  private String etName;		// name of ET system file
  
  public int getAlive()              {return alive;}
  public int getHeartbeat()          {return heartbeat;}
  public int getTemps()              {return temps;}
  public int getStations()           {return stations;}
  public int getAttachments()        {return attachments;}
  public int getProcesses()          {return processes;}
  public int getEventsOwned()        {return eventsOwned;}
  public int getMutex()              {return mutex;}
  public int getStatMutex()          {return statMutex;}
  public int getStatAddMutex()       {return statAddMutex;}
  public int getEndian()             {return endian;}
  public int getShare()              {return share;}
  public int getMainPid()            {return mainPid;}
  public int getSelects()            {return selects;}
  public int getEvents()             {return events;}
  public int getEventSize()          {return eventSize;}
  public int getTempsMax()           {return tempsMax;}
  public int getStationsMax()        {return stationsMax;}
  public int getAttachmentsMax()     {return attachmentsMax;}
  public int getProcessesMax()       {return processesMax;}
  public int getTcpPort()            {return tcpPort;}
  public int getUdpPort()            {return udpPort;}
  public int getInterfaceCount()     {return interfaceCount;}
  public int getMulticastCount()     {return multicastCount;}
  public String[] getInterfaceAddresses() {return (String[]) interfaceAddresses.clone();}
  public String[] getMulticastAddresses() {return (String[]) multicastAddresses.clone();}
  public String   getEtName()             {return new String(etName);}
  
  public void read(DataInputStream dis) throws IOException {
    
    alive          = dis.readInt();
    heartbeat      = dis.readInt();
    temps          = dis.readInt();
    stations       = dis.readInt();
    attachments    = dis.readInt();
    processes      = dis.readInt();
    eventsOwned    = dis.readInt();
    mutex          = dis.readInt();
    statMutex      = dis.readInt();
    statAddMutex   = dis.readInt();
    
    endian         = dis.readInt();
    share          = dis.readInt();
    mainPid        = dis.readInt();
    selects        = dis.readInt();
    events         = dis.readInt();
    eventSize      = dis.readInt();
    tempsMax       = dis.readInt();
    stationsMax    = dis.readInt();
    attachmentsMax = dis.readInt();
    processesMax   = dis.readInt();
    
    tcpPort        = dis.readInt();
    udpPort        = dis.readInt();
    
    interfaceCount = dis.readInt();
    multicastCount = dis.readInt();
    
    // read string lengths first
    int lengths[] = new int[interfaceCount+multicastCount+1];
    byte buf[];
    
    for (int i=0; i < interfaceCount+multicastCount+1; i++) {
      lengths[i] = dis.readInt();
    }
    
    // read network interface addresses
    interfaceAddresses = new String[interfaceCount];
    for (int i=0; i < interfaceCount; i++) {
      buf = new byte[lengths[i]];
      dis.read(buf, 0, lengths[i]);
      interfaceAddresses[i] = new String(buf, 0, lengths[i]-1, "ASCII");
    }
    
    // read multicast addresses
    multicastAddresses = new String[multicastCount];
    for (int i=0; i < multicastCount; i++) {
      buf = new byte[lengths[i+interfaceCount]];
      dis.read(buf, 0, lengths[i+interfaceCount]);
      multicastAddresses[i] = new String(buf, 0, lengths[i+interfaceCount]-1, "ASCII");
    }
    
    // read et name
    buf = new byte[lengths[interfaceCount+multicastCount]];
    dis.read(buf, 0, lengths[interfaceCount+multicastCount]);
    etName = new String(buf, 0, lengths[interfaceCount+multicastCount]-1, "ASCII");
  }
  
}










