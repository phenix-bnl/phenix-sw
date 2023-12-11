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
 *      Java class for containing process data from an ET system
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

class EtProcessData {
  private int num;		// unique id of process
  private int heartbeat;	// heartbeat count
  private int pid;		// pid of this unix process
  private int attachments;	// # of attachments this process created
  private int attIds[] = new int[EtConstants.attachmentsMax()]; // id #'s of these attachments

  public int   getNum()         {return num;}
  public int   getHeartbeat()   {return heartbeat;}
  public int   getPid()         {return pid;}
  public int   getAttachments() {return attachments;}
  public int[] getAttIds()      {return (int[])attIds.clone();}
  
  public void read(DataInputStream dis) throws IOException {
    num         = dis.readInt();
    heartbeat   = dis.readInt();
    pid         = dis.readInt();
    attachments = dis.readInt();
    for (int i=0; i < EtConstants.attachmentsMax(); i++) {
      attIds[i] = dis.readInt();
    }
  }
}







