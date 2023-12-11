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
 *      Java class for making a network connection to and getting data
 *	from an ET system
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
import java.net.*;
import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*; 
import javax.swing.border.*; 

public class EtConnection {
  private EtSystem etSys;
  private String   etHostName;
  private int      tcpPort = 0;
  private Socket   sock;
  // properties of ET system
  private int      systemEndian;
  private int      numEvents;
  private int      eventSize;
  private int      version;
  private int      numSelectInts;
  // status indicators
  private final boolean  foundServer=true, cannotFindServer=false;
  private final boolean  gotMatch=true, noMatch=false;
  // set of all ET systems that respond
  private HashMap responders = new HashMap(20);

  // Constructors
  public EtConnection (EtSystem sys) {
    etSys = new EtSystem(sys);
  }
  
  public int getNumEvents() {return numEvents;}
  public int getEventSize() {return eventSize;}
  public int getTcpPort()   {return tcpPort;}
  public String getHost()   {return new String(etHostName);}
  public String getEtName() {return new String(etSys.getEtName());}
 
  // find the ET system's tcp server port
  private boolean etFindServerPort() throws IOException, UnknownHostException {
    boolean match = noMatch;
    int   status, totalPacketsSent = 0, sendPacketLimit = 4;
    int   timeOuts[] = {0, 2000, 4000, 7000};
    int   waitTime, socketTimeOut = 20000; // socketTimeOut > sum of timeOuts    
    
    // Put outgoing packet info into a byte array to send to ET systems
    ByteArrayOutputStream  baos = new ByteArrayOutputStream();
    DataOutputStream        dos = new DataOutputStream(baos);
    // write ET version
    dos.writeInt(EtConstants.version());		//IOEx
    // write string length of ET name
    dos.writeInt(etSys.getEtName().length() + 1);	//IOEx
    dos.flush();					//IOEx
    // write ET name
    try {
      OutputStreamWriter osw = new OutputStreamWriter(baos,"ASCII");	//UnsupportedEncodingEx
      osw.write(etSys.getEtName() + '\0');				//IOEx
      osw.flush();
    }
    catch (UnsupportedEncodingException encodeEx) {
      // This will never happen.
    }
    // construct byte array to send over a socket
    final byte sbuffer[] = baos.toByteArray();
    
    
    // We may need to send packets over many different sockets
    // as there may be broadcasting on multiple subnets as well
    // as multicasts on several addresses. Keep track of these
    // sockets, addresses, & packets:
    class send {
      String          address;
      InetAddress     addr;
      MulticastSocket socket;
      DatagramPacket  packet;
      
      send (String saddr, MulticastSocket sock) throws UnknownHostException {
        address = saddr;
	socket  = sock;
	addr    = InetAddress.getByName(address);	//UnknownHostEx
        packet  = new DatagramPacket(sbuffer, sbuffer.length,
                                     addr, etSys.getUdpPort());
      }
    }
    
    int numBroadcastAddrs = etSys.getNumBroadcastAddrs();
    int numMulticastAddrs = etSys.getNumMulticastAddrs();
    int numAddrs = numBroadcastAddrs + numMulticastAddrs;
    String broadcastAddrs[] = etSys.getBroadcastAddrs();
    String multicastAddrs[] = etSys.getMulticastAddrs();
    send sendIt[] = new send[numAddrs];
    
    // If the host is not remote or anywhere out there. If it's
    // local or we know its name, send a UDP packet to it alone.
    if ((etSys.getHost().equals(EtConstants.hostRemote())   == false) &&
        (etSys.getHost().equals(EtConstants.hostAnywhere()) == false))  {

      // We can use multicast socket for regular UDP - it works
      MulticastSocket socket = new MulticastSocket();	//IOEx
      // Socket will unblock after timeout,
      // letting reply collecting thread quit
      try {socket.setSoTimeout(socketTimeOut);}
      catch (SocketException ex) {}
      
      // if it's local, find host's name
      if ((etSys.getHost().equals(EtConstants.hostLocal())) ||
          (etSys.getHost().equals("localhost")))  {
 	
	sendIt[0] = new send(InetAddress.getLocalHost().getHostName(), socket); //UnknownHostEx
      // else if we know host's name ...
      } else {
	sendIt[0] = new send(etSys.getHost(), socket);
      }
      numAddrs = 1;
//System.out.println("etFindServerPort: send to local or specified host");
      
    } else {
    
      // setup broadcast sockets & packets first
      if ( (etSys.getCast() == EtConstants.broadcast()) ||
           (etSys.getCast() == EtConstants.broadAndMulticast())) {

	for (int i=0; i < numBroadcastAddrs; i++) {
	  // We can use multicast socket for broadcasting - it works
	  MulticastSocket socket = new MulticastSocket();	//IOEx
	  // Socket will unblock after timeout,
	  // letting reply collecting thread quit
	  try {
	    socket.setSoTimeout(socketTimeOut);
	  }
	  catch (SocketException ex) {}

	  sendIt[i] = new send(broadcastAddrs[i], socket);
//System.out.println("etFindServerPort: broadcasting to " + broadcastAddrs[i]);
	}
      } else {
	numBroadcastAddrs = 0;
      }

      // setup multicast sockets & packets next
      if ( (etSys.getCast() == EtConstants.multicast()) ||
           (etSys.getCast() == EtConstants.broadAndMulticast())) {

	for (int i=0; i < numMulticastAddrs; i++) {
	  MulticastSocket socket = new MulticastSocket();	//IOEx
	  try {
	    socket.setSoTimeout(socketTimeOut);
	  }
	  catch (SocketException ex) {}

	  if (etSys.getTTL() != 1) {
	    socket.setTimeToLive(etSys.getTTL());		//IOEx
	  }

	  sendIt[i + numBroadcastAddrs] = new send(multicastAddrs[i], socket);
//System.out.println("etFindServerPort: multicasting to " + multicastAddrs[i]);
	}
      } else {
	numMulticastAddrs = 0;
      }
      numAddrs = numBroadcastAddrs + numMulticastAddrs;
   }
    
    // a class to help receive a packet on a socket
    class get {
      // data size = 4*4 + 2 + EtConstants.ipAddrStrLen() +
      //             2*EtConstants.maxHostNameLen(); = 546 bytes
      // but give us a bit of extra room with 1k bytes
      byte buffer[] = new byte[1000];
      DatagramReceive thread;
      DatagramPacket  packet;
      MulticastSocket socket;
      
      get (MulticastSocket sock) {
        packet = new DatagramPacket(buffer, buffer.length);
        socket = sock;
      }
      
      // start up thread to receive single udp packet on single socket
      void start() {
        thread = new DatagramReceive(packet, socket);
	thread.start();
      }
    }
    
    // store things here
    get receiveIt[] = new get[numAddrs];

    // start reply collecting threads
    for (int i=0; i < numAddrs; i++) {
      receiveIt[i] = new get(sendIt[i].socket);
      // start single thread
//System.out.println("etFindServerPort: starting thread to socket " + sendIt[i].socket);
      receiveIt[i].start();
    }
    
    Thread.currentThread().yield();
        
    // Create a progress monitor to inform user how much more
    // time to wait before giving up search for ET system
    int progress = 0;
    ProgressMonitor monitor = new ProgressMonitor(new JFrame(),
	  "Looking for " + etSys.getEtName() + " ET system",
	  "12 seconds left", 0, 13);
    monitor.setMillisToDecideToPopup(1000);
    monitor.setProgress(progress); 
       
  send:
    // set a limit on the total # of packet groups sent out to find a server
    while (totalPacketsSent < sendPacketLimit) {      
      // send packets out on all sockets
      for (int i=0; i < numAddrs; i++) {
        sendIt[i].socket.send(sendIt[i].packet); //IOException
      }
      // set time to wait for reply (gets longer with each round)
      waitTime = timeOuts[totalPacketsSent++];

  get:
      while (true) {
//System.out.println("  wait for " + waitTime + " milliseconds");
	// wait for replies
	try {
	  Thread.sleep(waitTime);
	  progress += waitTime/1000;
          monitor.setProgress(progress);    
          monitor.setNote((12-progress) + " seconds left");    
	}
	catch (InterruptedException ix) {}
	
	// check for replies on all sockets
        for (int i=0; i < numAddrs; i++) {
	  status = receiveIt[i].thread.waitForReply(10);
//System.out.println("etFindServerPort: receive on socket " + receiveIt[i].socket +
//                   ", status = " + status);
	
	  // if error or timeout ...
	  if ((status == -1) || (status == 0)) {
	    continue;
	  }
	  
	  // else if got packet ...
	  else if (status == 1) {
	    // analyze packet to see it matches the ET system we were
	    // looking for; if not, try to get another packet
	    if (etReplyMatch(receiveIt[i].packet) == gotMatch) { // IOEx, UnknownHostEx
//System.out.println("  found match");
	      // if we have a match stop the warning message
	      if (match == noMatch) {
		monitor.close();
		match = gotMatch;
	      }
	    }
	    else {
//System.out.println("  no match");
	    }
	    // See if there are other packets cued up,
	    // but don't wait too long. The thread we
	    // started is ended so start another up again.
	    waitTime = 50;
            receiveIt[i].start();
            Thread.currentThread().yield();

	    continue get;
	  }
        }
	
	// if we don't have a match, try again
	if (match == noMatch) {
	  // If max # of packets not yet sent, send another
	  // batch and try again with a longer wait
	  if (totalPacketsSent < sendPacketLimit) {
//System.out.println("  timedout, try again with longer wait");
	    continue send;
	  }
	}

	break send;

      } // while (true)
    } // while (totalPacketsSent < sendPacketLimit)
      
      
    if (match == gotMatch) {
      // if we're looking remotely or anywhere
      if ( (etSys.getHost().equals(EtConstants.hostAnywhere())) ||
	   (etSys.getHost().equals(EtConstants.hostRemote()))  )  {
	// and if we have more than one responding ET system
	if (responders.size() > 1) {
	    // give users a choice
	    String[] hosts = (String []) responders.keySet().toArray(new String[0]);

	    String host = (String) JOptionPane.showInputDialog(
	      new JFrame(),
              "Choose the ET system responding from host:",
              "ET System Choice",
              JOptionPane.PLAIN_MESSAGE,
	      null,
	      hosts,
	      hosts[0]
	    );

	    if (host == null) {
	      return cannotFindServer;
	    }
	    etHostName = host;
	    tcpPort = ((Integer) responders.get(host)).intValue();
	}
      }
      return foundServer;
    }
//System.out.println(" cannot find server, quitting");
    etHostName = null;
    tcpPort = 0;
    return cannotFindServer;
  }
  
  // analyze UDP packet & see if it matches system we're looking for
  private boolean etReplyMatch(DatagramPacket packet)
			throws IOException, UnknownHostException {
    try {
      int version, port, length;
      byte buf[];
      ByteArrayInputStream  bais = new ByteArrayInputStream(packet.getData());
      DataInputStream        dis = new DataInputStream(bais);

      // ET version #
      version = (int) dis.readInt();         //IOEx

      // we're reading an UNSIGNED short
      port = (int) dis.readUnsignedShort();

      // read length of fully qualified hostname of responding interface
      length = dis.readInt();
      // read fully qualified ET server host name (minus ending null)
      buf = new byte[length];
      dis.read(buf, 0, length);
      String repliedHostName = new String(buf, 0, length-1, "ASCII");	//UnsupportedEncodingEx

      // read length of IP address (dotted-decimal) of responding interface
      length = dis.readInt();
      // read IP address
      buf = new byte[length];
      dis.read(buf, 0, length);
      String repliedIpAddress = new String(buf, 0, length-1, "ASCII");

      // Read length of fully qualified hostname from "uname".
      // Used as identifier of this host no matter which interface used.
      length = dis.readInt();
      // read uname
      buf = new byte[length];
      dis.read(buf, 0, length);
      String repliedUname = new String(buf, 0, length-1, "ASCII");

// System.out.println("etReplyMatch: port = " + port + 
//		", server = " + repliedHostName +
//		", IP addr = " + repliedIpAddress +
//		", uname = " + repliedUname);

      // set ip address values for replied & local host
      InetAddress repliedHost = InetAddress.getByName(repliedHostName);	//UnknownHostEx
      InetAddress localHost   = InetAddress.getLocalHost();		//UnknownHostEx

      // if we're looking for a host anywhere
      if (etSys.getHost().equals(EtConstants.hostAnywhere())) {
//System.out.println("etReplyMatch: .anywhere");
	// Store name in hashtable in case there are several systems
	// that respond and user must chose which one he wants
	responders.put(repliedHostName, new Integer(port));
	// store info here in case only 1 response
	etHostName = repliedHostName;
	tcpPort = port;
	return gotMatch;
      }
      // else if we're looking for a remote host
      else if (etSys.getHost().equals(EtConstants.hostRemote())) {
//System.out.println("etReplyMatch: .remote");
	if (localHost.equals(repliedHost) == false) {
	  // Store name in hashtable in case there are several systems
	  // that respond and user must chose which one he wants
	  responders.put(repliedHostName, new Integer(port));
	  // store info here in case only 1 response
	  etHostName = repliedHostName;
	  tcpPort = port;
	  return gotMatch;
	}
      }
      // else if we're looking for a local host
      else if ((etSys.getHost().equals(EtConstants.hostLocal())) ||
               (etSys.getHost().equals("localhost")))  {
//System.out.println("etReplyMatch: .local");
	if (localHost.equals(repliedHost) == true) {
	  // Store values. In this case no other match will be found.
	  etHostName = repliedHostName;
	  tcpPort = port;
	  return gotMatch;
	}
      }
      // else a specific host name has been specified
      else {
//System.out.println("etReplyMatch: <name>");
	// "etSys.getHost()" is the host name we're looking for
	InetAddress etHost = InetAddress.getByName(etSys.getHost());	//UnknownHostEx
	if (etHost.equals(repliedHost) == true) {
	  // Store values. In this case no other match will be found.
	  etHostName = repliedHostName;
	  tcpPort = port;
	  return gotMatch;
	}
      }

    }
    catch (UnsupportedEncodingException encodeEx){
      // This will never happen.
    }
    
    return noMatch;
  }
  
  
  // open an ET system
  private void etOpen() throws IOException, EtException {

    try {
      DataInputStream  dis = new DataInputStream ( sock.getInputStream()  );
      DataOutputStream dos = new DataOutputStream( sock.getOutputStream() );
      OutputStreamWriter osw = new OutputStreamWriter( sock.getOutputStream(),"ASCII"); // UnsupportedEncodingEx
      int systemEndian, nevents, esize, version, nselects, skipped;

      // write our endian, length of ET filename, and ET filename
      dos.writeInt(EtConstants.bigEndian());		// IOEx
      dos.writeInt(etSys.getEtName().length() + 1);	// .
      //dos.writeInt(0);
      //dos.writeInt(0);
      dos.writeLong(0);	// write one long instead of 2 ints since =0 anyway
      dos.flush();					// .
      osw.write(etSys.getEtName() + '\0');		// .
      osw.flush();					// .

      // read what server sends back
      systemEndian  = dis.readInt();			// .
      numEvents     = dis.readInt();			// .
      eventSize     = dis.readInt();			// .
      version       = dis.readInt();			// .
      numSelectInts = dis.readInt();			// .
      skipped       = dis.skipBytes(12);		// .

      // check to see if connecting to same version ET software
      if (version != EtConstants.version()) {
	throw new EtException("May not open wrong version ET system");
      }
      // double check to see if # of select ints are the same
      if (numSelectInts != EtConstants.stationSelectInts()) {
	throw new EtException("May not open ET system with different # of select integers");
      }

      // print out stuff for debugging
      //System.out.println("systemEndian = " + systemEndian +
	//		 ", nevents = " + numEvents +
	//		 ", event size = " + eventSize);

    }
    catch (UnsupportedEncodingException encodeEx) {
      // This will never happen.
    }
    return;
  }
  
  
  public void connect() throws IOException, UnknownHostException, EtException {

    if (etSys.getCast() == EtConstants.direct()) {
      // if making direct connection, we have host & port
//System.out.println("connect: make a direct connection");
      tcpPort = etSys.getTcpPort();
      
      // We prefer a fully qualifed host name. If there are no "."'s
      // in it, try getHostName even though that is not guaranteed
      // to return a fully qualified name. If we don't get a fully
      // qualifed name, there's a chance we'll display a duplicate system
      // when given the unqualified name.
      if (etSys.getHost().indexOf(".") < 0) {
//System.out.println("connect: try to make " + etSys.getHost() + " a fully qualified name");
        etHostName = InetAddress.getByName(etSys.getHost()).getHostName();
      }
      else {
        etHostName = etSys.getHost();
      }
    } else {
//System.out.println("connect: try to find server port");
      // send a UDP broad or multicast packet to find ET TCP server & port
      if (etFindServerPort() == cannotFindServer) {	// IOEx, UnknownHostEx
	throw new EtException("Cannot find ET system");
      }
    }
//System.out.println("connect: try a socket");
    
    // Open our connection to an ET system TCP Server
    sock = new Socket(etHostName, tcpPort);		// IOEx
    // Set NoDelay option for fast response
    try {
      sock.setTcpNoDelay(true);
    } catch (SocketException ex) {
//System.out.println("connect: socket exception");
    }

    try {
      // open the ET system (et_open in C)
//System.out.println("connect: try to open ET system");
      etOpen();						// IOEx, EtEx
    }
    catch (Exception ex) {
      try {sock.close();} catch (IOException ioEx) {}
      if (ex instanceof IOException)
        throw (IOException) ex;
      else if (ex instanceof UnknownHostException)
        throw (UnknownHostException) ex;
      else if (ex instanceof EtException)
        throw (EtException) ex;
    }
  }
    
    
  public void disconnect() {
    try {sock.close();}
    catch (IOException ioex) {}
  }
  
  
  public void getData(EtAllData data) throws IOException {
    // Get I/O streams from the socket
    DataInputStream  dis = new DataInputStream(sock.getInputStream());
    DataOutputStream dos = new DataOutputStream(sock.getOutputStream());
    int i, count, dataSize;
    
    // tell ET server what command to run
    dos.writeInt(EtConstants.getSystemData()); //IOEx
    
    // Receive the incoming data
    dataSize = dis.readInt();                  //IOEx
    
    // read everything at once (4X faster that way),
    // put it into a byte array, and read from that
    byte[] bytes = new byte[dataSize];
    ByteArrayInputStream bis = new ByteArrayInputStream(bytes);
    DataInputStream dis2 = new DataInputStream(bis);
    dis.readFully(bytes);			//IOEx

    // system data
    data.sysData.read(dis2);                    //IOEx

    // station data
    count = dis2.readInt();                     //IOEx
    data.statData = new EtStationData[count];
    for (i=0; i < count; i++) {
      data.statData[i] = new EtStationData();
      data.statData[i].read(dis2);              //IOEx
    }
    
    // attachment data
    count = dis2.readInt();                     //IOEx
    data.attData = new EtAttachmentData[count];
    for (i=0; i < count; i++) {
      data.attData[i] = new EtAttachmentData();
      data.attData[i].read(dis2);               //IOEx
    }
    
    // process data
    count = dis2.readInt();                     //IOEx
    data.procData = new EtProcessData[count];
    for (i=0; i < count; i++) {
      data.procData[i] = new EtProcessData();
      data.procData[i].read(dis2);              //IOEx
    }
    
  }
  
  
  public void getHistogram(int hData[]) throws IOException, EtException {
    // Get I/O streams from the socket
    DataInputStream  dis = new DataInputStream(sock.getInputStream());
    DataOutputStream dos = new DataOutputStream(sock.getOutputStream());
    
    if (hData.length < numEvents+1) {
      throw new EtException("histogram integer array is too small");
    }
    
    // tell ET server to send us histogram data
    dos.writeInt(EtConstants.getHistogram());	//IOEx
    
    // receive error code
    if (dis.readInt() != EtConstants.ok()) {	//IOEx
      throw new EtException("Error getting histogram");
    }
    
    // read everything at once (4X faster that way),
    // put it into a byte array, and read from that
    byte[] bytes = new byte[4*(numEvents+1)];
    ByteArrayInputStream bis = new ByteArrayInputStream(bytes);
    DataInputStream dis2 = new DataInputStream(bis);
    dis.readFully(bytes);			//IOEx
    for (int i=0; i < numEvents+1; i++) {
      hData[i] = dis2.readInt();
    }
  }
}

// class to receive UDP packets
class DatagramReceive extends Thread {
  private DatagramPacket packet;
  private DatagramSocket socket;
  private int waitTime = 5000; // 5 seconds
  // allowed states
  private final int timedOut=0, receivedPacket=1, error=-1;
  private volatile int status = timedOut;
  private Thread runThread;
  
  public DatagramReceive(DatagramPacket recvPacket,
                         DatagramSocket recvSocket,
			 int totalWaitTime)
  {
    packet = recvPacket;
    socket = recvSocket;
    waitTime = totalWaitTime < 0 ? waitTime : totalWaitTime;
  }
  
  public DatagramReceive(DatagramPacket recvPacket,
                         DatagramSocket recvSocket)
  {
    packet = recvPacket;
    socket = recvSocket;
  }
  
  public void stopThread()  {runThread.interrupt();}
  public void setWaitTime(int time) {waitTime = time < 0 ? waitTime : time;}
  public int  getWaitTime() {return waitTime;}
  public int  getStatus()   {return status;}
  
  // this needs to be synchronized so the "wait" will work
  public synchronized int waitForReply()
  {
    if (status != timedOut) {
      return status;
    }
    try {wait(waitTime);}
    catch (InterruptedException intEx) {}
    return status;
  }
  
  // this needs to be synchronized so the "wait" will work
  public synchronized int waitForReply(int time)
  {
   if (status != timedOut) {
      return status;
    }
    try {wait(time);}
    catch (InterruptedException intEx) {}
    return status;
  }
  
  
  // No need to synchronize run as it can only be called once
  // by this object. Furthermore, if it is synchronized, then
  // if no packet is received, it is blocked with the mutex.
  // That, in turn, does not let the wait statement return from
  // a timeout. Since run is the only method that changes "status",
  // status does not have to be mutex-protected.
  public void run()
  {
    runThread = Thread.currentThread();
    status = timedOut;
    try {
      socket.receive(packet);
      status = receivedPacket;
    }
    catch (IOException iox) {
      status = error;
      return;
    }
    synchronized (this) {
      notify();
    }
  }
  
}




