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
 *      Java class for storing important parameters from an ET system.
 *	Native methods are used (elsewhere) to get ET parameters kept
 *	in C include files. If native methods cannot be used, default values
 *	are set here.
 *
 * Author:
 *	Carl Timmer
 *	TJNAF Data Acquisition Group
 *
 * Revision History:
 *
 *----------------------------------------------------------------------------*/

public final class EtConstants {

  // cannot construct object
  private EtConstants () {
  }
  
  // The reason the following variables are not made public & final, is
  // so they can be set by a native method in EtNative.java which gets
  // their values directly from the C header files. Since they are not
  // final, access to private variables thru methods is implemented.
  
  // constants from et.h
  private static String ET_MULTICAST_ADDR       = "239.200.0.0";
  private static String ET_HOST_LOCAL           = ".local";
  private static String ET_HOST_REMOTE          = ".remote";
  private static String ET_HOST_ANYWHERE        = ".anywhere";
  private static int    ET_MULTICAST            = 0;
  private static int    ET_BROADCAST            = 1;
  private static int    ET_DIRECT               = 2;
  private static int    ET_BROADANDMULTICAST    = 3;
  private static int    ET_BROADCAST_PORT       = 11111;
  private static int    ET_MULTICAST_PORT       = 11111;
  private static int    ET_SERVER_PORT          = 11111;
  private static int    ET_MULTICAST_TTL        = 1;
  private static int    ET_STATION_SELECT_INTS  = 4;
  private static int    ET_FILENAME_LENGTH      = 101;
  private static int    ET_FUNCNAME_LENGTH      = 51;
  private static int    ET_STATNAME_LENGTH      = 51;
  
  private static int    ET_STATION_UNUSED       = 0;
  private static int    ET_STATION_CREATING     = 1;
  private static int    ET_STATION_IDLE         = 2;
  private static int    ET_STATION_ACTIVE       = 3;
  
  private static int    ET_STATION_USER_MULTI   = 0;
  private static int    ET_STATION_USER_SINGLE  = 1;
  
  private static int    ET_STATION_NONBLOCKING  = 0;
  private static int    ET_STATION_BLOCKING     = 1;
  
  private static int    ET_STATION_SELECT_ALL   = 1;
  private static int    ET_STATION_SELECT_MATCH = 2;
  private static int    ET_STATION_SELECT_USER  = 3;

  private static int    ET_STATION_RESTORE_OUT  = 0;
  private static int    ET_STATION_RESTORE_IN   = 1;
  private static int    ET_STATION_RESTORE_GC   = 2;

  // error codes from et.h
  private static int    ET_OK                   =  0;
  private static int    ET_ERROR                = -1;
  private static int    ET_ERROR_TOOMANY        = -2;
  private static int    ET_ERROR_EXISTS         = -3;
  private static int    ET_ERROR_WAKEUP         = -4;
  private static int    ET_ERROR_TIMEOUT        = -5;
  private static int    ET_ERROR_EMPTY          = -6;
  private static int    ET_ERROR_BUSY           = -7;
  private static int    ET_ERROR_DEAD           = -8;
  private static int    ET_ERROR_READ           = -9;
  private static int    ET_ERROR_WRITE          = -10;
  private static int    ET_ERROR_REMOTE         = -11;
  private static int    ET_ERROR_NOREMOTE       = -12;

  
  // constants from private.h
  private static int    ET_VERSION              = 5;
  private static int    ET_MINORVERSION         = 0;
  private static int    ET_IPADDRSTRLEN         = 16;
  private static int    ET_MAXHOSTNAMELEN       = 256;
  private static int    ET_ATTACHMENTS_MAX      = 50;
  private static int    ET_NET_SYS_DATA         = 53;
  private static int    ET_NET_SYS_HIST         = 54;
  private static int    ET_MUTEX_UNLOCKED       = 0;
  private static int    ET_MUTEX_LOCKED         = 1;
  private static int    ET_MUTEX_SHARE          = 0;
  private static int    ET_MUTEX_NOSHARE        = 1;
  private static int    ET_ATT_CONTINUE         = 0;
  private static int    ET_ATT_QUIT             = 1;
  private static int    ET_ATT_UNBLOCKED        = 0;
  private static int    ET_ATT_BLOCKED          = 1;
  
  // constants from network.h
  private static int    ET_ENDIAN_BIG           = 0;
  private static int    ET_ENDIAN_LITTLE        = 1;
  
  
  // public access to constants
  public static String multicastAddr()	    {return new String(ET_MULTICAST_ADDR);}
  public static String hostLocal()	    {return new String(ET_HOST_LOCAL);}
  public static String hostRemote()	    {return new String(ET_HOST_REMOTE);}
  public static String hostAnywhere()	    {return new String(ET_HOST_ANYWHERE);}
  public static int    multicast()	    {return ET_MULTICAST;}
  public static int    broadcast()	    {return ET_BROADCAST;}
  public static int    broadAndMulticast()  {return ET_BROADANDMULTICAST;}
  public static int    direct()		    {return ET_DIRECT;}
  public static int    broadcastPort()	    {return ET_BROADCAST_PORT;}
  public static int    multicastPort()	    {return ET_MULTICAST_PORT;}
  public static int    serverPort()	    {return ET_SERVER_PORT;}
  public static int    multicastTTL()	    {return ET_MULTICAST_TTL;}
  public static int    stationSelectInts()  {return ET_STATION_SELECT_INTS;}
  public static int    fileNameLength()	    {return ET_FILENAME_LENGTH;}
  public static int    functionNameLength() {return ET_FUNCNAME_LENGTH;}
  public static int    stationNameLength()  {return ET_STATNAME_LENGTH;}
  
  public static int    stationUnused()      {return ET_STATION_UNUSED;}
  public static int    stationCreating()    {return ET_STATION_CREATING;}
  public static int    stationIdle()        {return ET_STATION_IDLE;}
  public static int    stationActive()      {return ET_STATION_ACTIVE;}
  public static int    stationMultiUser()   {return ET_STATION_USER_MULTI;}
  public static int    stationSingleUser()  {return ET_STATION_USER_SINGLE;}
  public static int    stationBlocking()    {return ET_STATION_BLOCKING;}
  public static int    stationNonBlocking() {return ET_STATION_NONBLOCKING;}
  public static int    stationSelectAll()   {return ET_STATION_SELECT_ALL;}
  public static int    stationSelectMatch() {return ET_STATION_SELECT_MATCH;}
  public static int    stationSelectUser()  {return ET_STATION_SELECT_USER;}
  public static int    stationRestoreOut()  {return ET_STATION_RESTORE_OUT;}
  public static int    stationRestoreIn()   {return ET_STATION_RESTORE_IN;}
  public static int    stationRestoreGC()   {return ET_STATION_RESTORE_GC;}

  public static int    ok()                 {return ET_OK;}
  public static int    error()              {return ET_ERROR;}
  public static int    errorTooMany()       {return ET_ERROR_TOOMANY;}
  public static int    errorExists()        {return ET_ERROR_EXISTS;}
  public static int    errorWakeup()        {return ET_ERROR_WAKEUP;}
  public static int    errorTimeout()       {return ET_ERROR_TIMEOUT;}
  public static int    errorEmpty()         {return ET_ERROR_EMPTY;}
  public static int    errorBusy()          {return ET_ERROR_BUSY;}
  public static int    errorDead()          {return ET_ERROR_DEAD;}
  public static int    errorRead()          {return ET_ERROR_READ;}
  public static int    errorWrite()         {return ET_ERROR_WRITE;}
  public static int    errorRemote()        {return ET_ERROR_REMOTE;}
  public static int    errorNoRemote()      {return ET_ERROR_NOREMOTE;}

  public static int    version()	    {return ET_VERSION;}
  public static int    minorVersion()	    {return ET_MINORVERSION;}
  public static int    ipAddrStrLen()	    {return ET_IPADDRSTRLEN;}
  public static int    maxHostNameLen()	    {return ET_MAXHOSTNAMELEN;}
  public static int    attachmentsMax()	    {return ET_ATTACHMENTS_MAX;}
  public static int    getSystemData()	    {return ET_NET_SYS_DATA;}
  public static int    getHistogram()	    {return ET_NET_SYS_HIST;}
  public static int    mutexUnlocked()      {return ET_MUTEX_UNLOCKED;}
  public static int    mutexLocked()	    {return ET_MUTEX_LOCKED;}
  public static int    mutexShare()	    {return ET_MUTEX_SHARE;}
  public static int    mutexNoShare()	    {return ET_MUTEX_NOSHARE;}
  public static int    attachContinue()	    {return ET_ATT_CONTINUE;}
  public static int    attachQuit()	    {return ET_ATT_QUIT;}
  public static int    attachBlocked()	    {return ET_ATT_BLOCKED;}
  public static int    attachUnblocked()    {return ET_ATT_UNBLOCKED;}
  
  public static int    bigEndian()	    {return ET_ENDIAN_BIG;}
  public static int    littleEndian()	    {return ET_ENDIAN_LITTLE;}
}
