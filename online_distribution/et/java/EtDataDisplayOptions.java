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
 *      Java class for setting display options in the ET viewer
 *
 * Author:
 *	Carl Timmer
 *	TJNAF Data Acquisition Group
 *
 * Revision History:
 *
 *----------------------------------------------------------------------------*/

import java.lang.*;

// class to handle what-to-show-in-GUI options
public class EtDataDisplayOptions {
  // by default show everything
  private boolean system        = true;
  private boolean idleStations  = true;
  private boolean configuration = true;
  private boolean attachments   = true;
  private boolean rate          = true;
  private boolean events        = true;
  // show multiple systems simultaneously
  // or in separate tabs of a tabbedpane?
  private boolean tabbed	= true;

  public EtDataDisplayOptions() {
  }

  public EtDataDisplayOptions(EtDataDisplayOptions options) {
    system        = options.showSystem();
    idleStations  = options.showIdleStations();
    configuration = options.showConfiguration();
    attachments   = options.showAttachments();
    rate          = options.showRate();
    events        = options.showEvents();
    tabbed        = options.inTabbedPane();
  }

  public boolean showSystem()        {return system;}
  public boolean showIdleStations()  {return idleStations;}
  public boolean showConfiguration() {return configuration;}
  public boolean showAttachments()   {return attachments;}
  public boolean showRate()          {return rate;}
  public boolean showEvents()        {return events;}
  public boolean inTabbedPane()      {return tabbed;}

  public void setSystem(boolean val)        {system = val;}
  public void setIdleStations(boolean val)  {idleStations = val;}
  public void setConfiguration(boolean val) {configuration = val;}
  public void setAttachments(boolean val)   {attachments = val;}
  public void setRate(boolean val)          {rate = val;}
  public void setEvents(boolean val)        {events = val;}
  public void setTabbedPane(boolean val)    {tabbed = val;}
  
  public void setShow(boolean sys, boolean idle, boolean con,
		      boolean att, boolean rt, boolean ev) {
    system        = sys;
    idleStations  = idle;
    configuration = con;
    attachments   = att;
    rate          = rt;
    events        = ev;
  }
  
  public void setAll(boolean sys, boolean idle, boolean con,
		      boolean att, boolean rt, boolean ev, boolean tab) {
    system        = sys;
    idleStations  = idle;
    configuration = con;
    attachments   = att;
    rate          = rt;
    events        = ev;
    tabbed        = tab;
  }
}
