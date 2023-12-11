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
 *      Java class for running a thread which gets data from an ET system
 *	and plots it in a GUI. It also calculates event rates through the
 *	system.
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
import javax.swing.*;

// import jas.hist.*;

// each connection to an ET system has its own thread
public class EtConnectionThread extends Thread {

  // instance variables
  private EtConnection	    myConnection;
  private EtViewer	    viewer;
  // for rate calculations
  private HashMap	    oldCount, newCount, rates;
  private long		    startTime;
  // HashMap is shared with everybody
  private volatile Map      allConnections;
  
  // constructor
  public EtConnectionThread(EtSystem etSys, Map connections,
			    EtConnection connection, EtViewer viewer) {
    myConnection   = connection;
    allConnections = connections;
    this.viewer    = viewer;
    oldCount       = new HashMap(20);
    newCount       = new HashMap(20);
    rates          = new HashMap(20);
    startTime      = 0;
  }
    
  // calculate event rates into stations
  private void findRates(EtAllData data) {
    // start the clock running if necessary
    if (startTime == 0) {
      startTime = System.currentTimeMillis();
      // store stations' total input events in hash table
      for (int i=0; i<data.statData.length ; i++) {
	oldCount.put(data.statData[i].getStationName(),
		     new Long(data.statData[i].getInListIn()));
      }
      return;
    }
    
    String key;
    long   previousCount, currentCount, stopTime;
    Long   longObj;
    int    freq;
    float  time;

    stopTime = System.currentTimeMillis();	// milliseconds
    time = (float) .001*(stopTime - startTime);	// seconds
    startTime = stopTime;

    // store stations' total input events in hash table
    for (int i=0; i<data.statData.length ; i++) {
      newCount.put(data.statData[i].getStationName(),
		 new Long(data.statData[i].getInListIn()));
    }
    
    // iterate through set of new input counts
    rates.clear();
    for (Iterator i = newCount.keySet().iterator(); i.hasNext(); ) {
      key = (String) i.next();
      if ((longObj = (Long) oldCount.get(key)) == null) {
	// this station (key) wasn't around last time
	continue;
      }
      previousCount = longObj.longValue();
      currentCount  = ((Long) newCount.get(key)).longValue();
      freq = (int) ((currentCount-previousCount)/time);
      // store event rates in hash table
      rates.put(key, new Integer(freq));
    }

    // save new values of total input events as old ones
    oldCount.clear();
    oldCount.putAll(newCount);
    
    return;
  }
  
  
  public void run() {
    // place to store data from ET system
    EtAllData data = new EtAllData();
    // place to display data
    final EtDataDisplay dDisplay = new EtDataDisplay(data, viewer,
					myConnection, rates);
        
    while (true) {
      try {
        // get new data from ET system
	myConnection.getData(data);
	// calculate event rates
	findRates(data);
        // put display on screen through swing thread
        SwingUtilities.invokeLater(dDisplay);
	// sleep for delay time
	Thread.sleep(1000*viewer.getUpdatePeriod());
      }
      catch (Exception ex) {
	// remove this connection from the HashMap
	allConnections.remove(myConnection.getEtName()+myConnection.getHost());
	return;
      }     
    }
    // EtHistogramDisplay hDisplay = new EtHistogramDisplay(hData);
  }
  
  
  
  
}
