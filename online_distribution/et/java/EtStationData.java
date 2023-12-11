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
 *      Java class for containing station data from an ET system
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

class EtStationData {
  private int num;		// unique id of this station
  private int status;		// status
  private int mutex;		// is event transfer mutex locked?
  private int attachments;	// # of attachments to this station
  private int attIds[] = new int[EtConstants.attachmentsMax()]; // id #'s of these attachments
  
  private int  inListMutex;	// input list mutex status
  private int  inListCount;	// # of events in input list
  private long inListTry;	// # of events that were tried to put in
  private long inListIn;	// # of events put into input list
  private int  outListMutex;	// output list mutex status
  private int  outListCount;	// # of events in output list
  private long outListOut;	// # of events taken out output list
				// station definition parameters ...
  private int userMode;
  private int restoreMode;
  private int blockMode;
  private int prescale;
  private int cue;
  private int selectMode;
  private int select[] = new int[EtConstants.stationSelectInts()];
  
  private String function;
  private String library;
  private String stationName;
    
  public int    getNum()          {return num;}
  public int    getStatus()       {return status;}
  public int    getMutex()        {return mutex;}
  public int    getAttachments()  {return attachments;}
  public int[]  getAttIds()       {return (int[])attIds.clone();}
  public int    getInListMutex()  {return inListMutex;}
  public int    getInListCount()  {return inListCount;}
  public long   getInListTry()    {return inListTry;}
  public long   getInListIn()     {return inListIn;}
  public int    getOutListMutex() {return outListMutex;}
  public int    getOutListCount() {return outListCount;}
  public long   getOutListOut()   {return outListOut;}
  public int    getUserMode()     {return userMode;}
  public int    getRestoreMode()  {return restoreMode;}
  public int    getBlockMode()    {return blockMode;}
  public int    getPrescale()     {return prescale;}
  public int    getCue()          {return cue;}
  public int    getSelectMode()   {return selectMode;}
  public int[]  getSelect()       {return (int[])select.clone();}
  public String getFunction()     {return new String(function);}
  public String getLibrary()      {return new String(library);}
  public String getStationName()  {return new String(stationName);}
  
  public void read(DataInputStream dis) throws IOException {
    
    num          = dis.readInt();
    status       = dis.readInt();
    mutex        = dis.readInt();
    attachments  = dis.readInt();
    for (int i=0; i < EtConstants.attachmentsMax(); i++) {
      attIds[i]  = dis.readInt();
    }
    
    inListMutex  = dis.readInt();
    inListCount  = dis.readInt();
    inListTry    = dis.readLong();
    inListIn     = dis.readLong();
    outListMutex = dis.readInt();
    outListCount = dis.readInt();
    outListOut   = dis.readLong();
    
    userMode     = dis.readInt();
    restoreMode  = dis.readInt();
    blockMode    = dis.readInt();
    prescale     = dis.readInt();
    cue          = dis.readInt();
    selectMode   = dis.readInt();
    for (int i=0; i < EtConstants.stationSelectInts(); i++) {
      select[i]  = dis.readInt();
    }
    
    // read strings, lengths first
    int length1, length2, length3;
    byte buf[];
    
    length1 = dis.readInt();
    length2 = dis.readInt();
    length3 = dis.readInt();
    
    buf    = new byte[length1];
    dis.read(buf, 0, length1);
    function = new String(buf, 0, length1-1, "ASCII");
    
    buf    = new byte[length2];
    dis.read(buf, 0, length2);
    library = new String(buf, 0, length2-1, "ASCII");
    
    buf    = new byte[length3];
    dis.read(buf, 0, length3);
    stationName = new String(buf, 0, length3-1, "ASCII");
  }
}

