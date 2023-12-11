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
 *      Java class for storing parameters needed to connect to an ET system
 *
 * Author:
 *	Carl Timmer
 *	TJNAF Data Acquisition Group
 *
 * Revision History:
 *
 *----------------------------------------------------------------------------*/

import java.lang.*;

public class EtSystem {
  
  private String  name;                    // ET system file name
  private String  host;                    // destination of broadcast or hostname
  private String  broadcastAddrs[] = null; // local subnet broadcast addresses
  private String  multicastAddrs[] = null; // multicast addresses
  private int     cast;                    // broadcast, multicast, or direct
  private int     tcpPort;                 // TCP server port
  private int     udpPort;                 // broadcast/multicast UDP port
  private int     ttl;                     // multicast TTL value
  
  
  // General Constructor
  public EtSystem (String etName, String hostName,
                   String[] bAddrs, String[] mAddrs,
		   int castType, int tPort, int uPort, int ttlNum) {

    name = new String(etName);
    host = new String(hostName);
    if (bAddrs != null) {
      broadcastAddrs = (String[]) bAddrs.clone();
    }
    if (mAddrs != null) {
      multicastAddrs = (String[]) mAddrs.clone();
    }
    if ((castType != EtConstants.multicast()) &&
        (castType != EtConstants.broadcast()) &&
        (castType != EtConstants.broadAndMulticast()) &&
        (castType != EtConstants.direct()))     {
      cast = EtConstants.broadcast();
    } else {
      cast = castType;
    }
    if ((tPort < 1024) || (tPort > 65535)) {
      tcpPort = EtConstants.serverPort();
    } else {
      tcpPort = tPort;
    }
    if ((uPort < 1024) || (uPort > 65535)) {
      udpPort = EtConstants.broadcastPort();
    } else {
      udpPort = uPort;
    }
    if ((ttlNum < 0) || (ttlNum > 254)) {
      ttl = EtConstants.multicastTTL();
    } else {
      ttl = ttlNum;
    }
  }
    
  // Broadcasting Constructor
  public EtSystem (String etName, String hostName, String[] bAddrs, int uPort) {
    // call general constructor
    this (etName, hostName, bAddrs, null, -1, -1, uPort, -1);
  }
  
  // Multicast Constructor
  public EtSystem (String etName, String hostName, String[] mAddrs, int uPort, int ttlNum) {
    this (etName, hostName, null, mAddrs,
          EtConstants.multicast(), -1, uPort, ttlNum);
  }

  // Direct Constructor
  public EtSystem (String etName, String hostName, int tPort) {
    this (etName, hostName, null, null, EtConstants.direct(), tPort, -1, -1);
  }
  
  public EtSystem (EtSystem etSys) {
    name           = etSys.getEtName();
    host           = etSys.getHost();
    broadcastAddrs = etSys.getBroadcastAddrs();
    multicastAddrs = etSys.getMulticastAddrs();
    cast           = etSys.getCast();
    udpPort        = etSys.getUdpPort();
    tcpPort        = etSys.getTcpPort();
    ttl            = etSys.getTTL();
  }
  
  // Get methods
  public String   getEtName()            {return new String(name);}
  public String   getHost()              {return new String(host);}
  public String[] getBroadcastAddrs()    {if (broadcastAddrs == null) return null;
                                          else return (String[]) broadcastAddrs.clone();}
  public String[] getMulticastAddrs()    {if (multicastAddrs == null) return null;
                                          else return (String[]) multicastAddrs.clone();}
  public int      getCast()              {return cast;}
  public int      getTcpPort()           {return tcpPort;}
  public int      getUdpPort()           {return udpPort;}
  public int      getTTL()               {return ttl;}
  public int      getNumBroadcastAddrs() {if (broadcastAddrs == null) return 0;
                                          else return broadcastAddrs.length;}
  public int      getNumMulticastAddrs() {if (multicastAddrs == null) return 0;
                                          else return multicastAddrs.length;}
  
  // Set methods
  public void setEtName(String etName) {name = new String(etName);}
  public void setHost(String hostName) {host = new String(hostName);}
  public void setBroadcastAddrs(String[] addrs)  {broadcastAddrs = (String[]) addrs.clone();}
  public void setMulticastAddrs(String[] addrs)  {multicastAddrs = (String[]) addrs.clone();}
  public void setCast(int castType) {
    if ((castType != EtConstants.multicast()) &&
        (castType != EtConstants.broadcast()) &&
        (castType != EtConstants.broadAndMulticast()) &&
        (castType != EtConstants.direct()))     {
      return; 
    }
    cast = castType;
  }
  public void setTcpPort(int port) {
    if ((port < 1024) || (port > 65535)) {
      return;
    }
    tcpPort = port;
  }
  public void setUdpPort(int port) {
    if ((port < 1024) || (port > 65535)) {
      return;
    }
    udpPort = port;
  }
  public void setTTL(int ttlNum) {
    if ((ttlNum < 0) || (ttlNum > 254)) {
      return;
    }
    ttl = ttlNum;
  }
}
