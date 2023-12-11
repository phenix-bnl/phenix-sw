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
 *      Java class for saving and loading ET viewer's configuration files
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
import java.util.*;
import java.awt.*;

// SUN parser
// import javax.xml.parsers.*;

// Xerces parser from apache/IBM
import org.apache.xerces.parsers.*;
import org.xml.sax.helpers.*;

// SUN & Xerces, DOM & SAX
// import org.w3c.dom.*;
import org.xml.sax.*;

public class EtConfiguration {
  
  // levels of hierarchy in XML config file for DOM parsing
  // private static final int levels = 5;
  
  // save Configuration parameters to file
  public static void save(File file, EtViewer viewer, Map connections) throws IOException {
    try {
      FileOutputStream fos   = new FileOutputStream(file);
      OutputStreamWriter osw = new OutputStreamWriter(fos, "ASCII");

      StringBuffer text = new StringBuffer(1000);
      
      // configuration file is in XML format
      text.append("<?xml version=\"1.0\" standalone=\"yes\"?>\n\n");
      
      // document type definition
      text.append("<!DOCTYPE ET_VIEWER_CONFIGURATION [\n");
      text.append("<!ELEMENT ET_VIEWER_CONFIGURATION (DESCRIPTION, WINDOWS, DATA_DISPLAY, CONNECTION*)>\n");
      text.append("<!ELEMENT DESCRIPTION (COMMENTS*, VERSION, COMMENTS*)>\n");
      text.append("<!ELEMENT COMMENTS (#PCDATA)>\n");
      text.append("<!ELEMENT VERSION (#PCDATA)>\n");
      text.append("<!ELEMENT WINDOWS (MAIN_WIDTH, MAIN_HEIGHT, MAIN_XPOS, MAIN_YPOS, ");
      text.append("DATA_WIDTH, DATA_HEIGHT, DATA_XPOS, DATA_YPOS)>\n");
      text.append("<!ELEMENT MAIN_WIDTH (#PCDATA)>\n");
      text.append("<!ELEMENT MAIN_HEIGHT (#PCDATA)>\n");
      text.append("<!ELEMENT MAIN_XPOS (#PCDATA)>\n");
      text.append("<!ELEMENT MAIN_YPOS (#PCDATA)>\n");
      text.append("<!ELEMENT DATA_WIDTH (#PCDATA)>\n");
      text.append("<!ELEMENT DATA_HEIGHT (#PCDATA)>\n");
      text.append("<!ELEMENT DATA_XPOS (#PCDATA)>\n");
      text.append("<!ELEMENT DATA_YPOS (#PCDATA)>\n");
      text.append("<!ELEMENT DATA_DISPLAY (GENERAL, SYSTEM, STATIONS)>\n");
      text.append("<!ELEMENT GENERAL (IN_TABBED_PANE, UPDATE_PERIOD)>\n");
      text.append("<!ELEMENT IN_TABBED_PANE (#PCDATA)>\n");
      text.append("<!ELEMENT UPDATE_PERIOD (#PCDATA)>\n");
      text.append("<!ELEMENT SYSTEM (SHOW_SYSTEM)>\n");
      text.append("<!ELEMENT SHOW_SYSTEM (#PCDATA)>\n");
      text.append("<!ELEMENT STATIONS (SHOW_IDLE, SHOW_CONFIGURATION, SHOW_ATTACHMENTS, SHOW_RATE, SHOW_EVENTS)>\n");
      text.append("<!ELEMENT SHOW_IDLE (#PCDATA)>\n");
      text.append("<!ELEMENT SHOW_CONFIGURATION (#PCDATA)>\n");
      text.append("<!ELEMENT SHOW_ATTACHMENTS (#PCDATA)>\n");
      text.append("<!ELEMENT SHOW_RATE (#PCDATA)>\n");
      text.append("<!ELEMENT SHOW_EVENTS (#PCDATA)>\n");
      text.append("<!ELEMENT CONNECTION (ET_NAME, HOST, PORT)>\n");
      text.append("<!ELEMENT ET_NAME (#PCDATA)>\n");
      text.append("<!ELEMENT HOST (#PCDATA)>\n");
      text.append("<!ELEMENT PORT (#PCDATA)>\n");
      text.append("]>\n\n");
      
      // general data
      text.append("<ET_VIEWER_CONFIGURATION>\n");
      text.append("  <DESCRIPTION>\n");
      text.append("    <COMMENTS>This is a configuration file in XML format belonging to the EtViewer</COMMENTS>\n");
      text.append("    <VERSION>1.0</VERSION>\n");
      text.append("  </DESCRIPTION>\n");
      
      // window size & position data
      text.append("  <WINDOWS>\n");
      Rectangle bounds = viewer.getMainWindowBounds();
      text.append("    <MAIN_WIDTH>" + bounds.width + "</MAIN_WIDTH>\n");
      text.append("    <MAIN_HEIGHT>" + bounds.height + "</MAIN_HEIGHT>\n");
      text.append("    <MAIN_XPOS>" + bounds.x + "</MAIN_XPOS>\n");
      text.append("    <MAIN_YPOS>" + bounds.y + "</MAIN_YPOS>\n");
      bounds = viewer.getDataWindowBounds();
      text.append("    <DATA_WIDTH>" + bounds.width + "</DATA_WIDTH>\n");
      text.append("    <DATA_HEIGHT>" + bounds.height + "</DATA_HEIGHT>\n");
      text.append("    <DATA_XPOS>" + bounds.x + "</DATA_XPOS>\n");
      text.append("    <DATA_YPOS>" + bounds.y + "</DATA_YPOS>\n");
      text.append("  </WINDOWS>\n");
      
      // display data
      text.append("  <DATA_DISPLAY>\n");
      text.append("    <GENERAL>\n");
      if (viewer.inTabbedPane())
	text.append("      <IN_TABBED_PANE>yes</IN_TABBED_PANE>\n");
      else
	text.append("      <IN_TABBED_PANE>no</IN_TABBED_PANE>\n");
      text.append("      <UPDATE_PERIOD>"+viewer.getUpdatePeriod()+"</UPDATE_PERIOD>\n");
      text.append("    </GENERAL>\n");
      text.append("    <SYSTEM>\n");
      if (viewer.showSystem())
	text.append("      <SHOW_SYSTEM>yes</SHOW_SYSTEM>\n");
      else
	text.append("      <SHOW_SYSTEM>no</SHOW_SYSTEM>\n");
      text.append("    </SYSTEM>\n");
      text.append("    <STATIONS>\n");
      if (viewer.showIdleStations())
	text.append("      <SHOW_IDLE>yes</SHOW_IDLE>\n");
      else
	text.append("      <SHOW_IDLE>no</SHOW_IDLE>\n");
      if (viewer.showConfiguration())
	text.append("      <SHOW_CONFIGURATION>yes</SHOW_CONFIGURATION>\n");
      else
	text.append("      <SHOW_CONFIGURATION>no</SHOW_CONFIGURATION>\n");
      if (viewer.showAttachments())
	text.append("      <SHOW_ATTACHMENTS>yes</SHOW_ATTACHMENTS>\n");
      else
	text.append("      <SHOW_ATTACHMENTS>no</SHOW_ATTACHMENTS>\n");
      if (viewer.showRate())
	text.append("      <SHOW_RATE>yes</SHOW_RATE>\n");
      else
	text.append("      <SHOW_RATE>no</SHOW_RATE>\n");
      if (viewer.showEvents())
	text.append("      <SHOW_EVENTS>yes</SHOW_EVENTS>\n");
      else
	text.append("      <SHOW_EVENTS>no</SHOW_EVENTS>\n");
      text.append("    </STATIONS>\n");
      text.append("  </DATA_DISPLAY>\n");
      
      // connection data
      Iterator i = connections.values().iterator();
      while (i.hasNext()) {
	EtConnection conn = (EtConnection) i.next();
        text.append("  <CONNECTION>\n");
        text.append("    <ET_NAME>" + conn.getEtName() + "</ET_NAME>\n");
        text.append("    <HOST>" + conn.getHost() + "</HOST>\n");
        text.append("    <PORT>" + conn.getTcpPort() + "</PORT>\n");
        text.append("  </CONNECTION>\n");
      }
      text.append("</ET_VIEWER_CONFIGURATION>\n");
      
      osw.write(text.toString());
      osw.close();
      fos.close();
    }
    catch (UnsupportedEncodingException ex) {}
  }
  
    
  // read from Configuration parameter file and set
  public static void load(File file, EtViewer viewer) throws IOException {
    loadSAX2(file, viewer);
  }
  
  
  // read from Configuration parameter file and set
  private static void loadSAX2(final File file, final EtViewer viewer) throws IOException {
     
    class MyHandler extends DefaultHandler {
      
      private String element    = null;
      private String etName     = null;
      private String etHost     = null;
      private int    port       = 0;
      private int    mainWidth  = 0;
      private int    mainHeight = 0;
      private int    mainX      = 0;
      private int    mainY      = 0;
      private int    dataWidth  = 0;
      private int    dataHeight = 0;
      private int    dataX      = 0;
      private int    dataY      = 0;
      
      //=============================
      // SAX DocumentHandler methods
      //=============================

      public void startDocument () throws SAXException {
         // do nothing
      }

      public void endDocument () throws SAXException {
         // do nothing
      }

      public void startElement (String uri, String localName, String qName,
				Attributes attributes) throws SAXException {

	  if (localName.equals("MAIN_WIDTH")         ||
	      localName.equals("MAIN_HEIGHT")        ||
	      localName.equals("MAIN_XPOS")          ||
	      localName.equals("MAIN_YPOS")          ||
	      localName.equals("DATA_WIDTH")         ||
	      localName.equals("DATA_HEIGHT")        ||
	      localName.equals("DATA_XPOS")          ||
	      localName.equals("DATA_YPOS")          ||
	      localName.equals("IN_TABBED_PANE")     ||
	      localName.equals("UPDATE_PERIOD")      ||
	      localName.equals("SHOW_SYSTEM")        ||
	      localName.equals("SHOW_IDLE")          ||
	      localName.equals("SHOW_CONFIGURATION") ||
	      localName.equals("SHOW_ATTACHMENTS")   ||
	      localName.equals("SHOW_RATE")          ||
	      localName.equals("SHOW_EVENTS")        ||
	      localName.equals("ET_NAME")            ||
	      localName.equals("HOST")               ||
	      localName.equals("PORT")) {
	      
	    element = localName;
	  }
      }

      public void endElement (String uri, String localName, String qName) throws SAXException {
	  element = null;
      }

      public void characters (char buf [], int offset, int len) throws SAXException {
          if (element == null) {
	    return;
	  }
	  String s = new String(buf, offset, len);
	  setViewerParametersSAX(file, viewer, s);
      }

      private void setViewerParametersSAX(File file, EtViewer viewer, String s)
		    throws SAXParseException {

	String value;

	if (element.equals("MAIN_WIDTH")) {
	  try {mainWidth = Integer.parseInt(s.trim());}
	  catch (NumberFormatException x) {
	    throw new SAXParseException("Bad value for \"MAIN_WIDTH\" in configuration file",
	    			       null, file.getName(), 0, 0);
	  }
	}
	else if (element.equals("MAIN_HEIGHT")) {
	  try {mainHeight = Integer.parseInt(s.trim());}
	  catch (NumberFormatException x) {
	    throw new SAXParseException("Bad value for \"MAIN_HEIGHT\" in configuration file",
	    			       null, file.getName(), 0, 0);
	  }
	}
	else if (element.equals("MAIN_XPOS")) {
	  try {mainX = Integer.parseInt(s.trim());}
	  catch (NumberFormatException x) {
	    throw new SAXParseException("Bad value for \"MAIN_XPOS\" in configuration file",
	    			       null, file.getName(), 0, 0);
	  }
	}
	else if (element.equals("MAIN_YPOS")) {
	  try {mainY = Integer.parseInt(s.trim());}
	  catch (NumberFormatException x) {
	    throw new SAXParseException("Bad value for \"MAIN_YPOS\" in configuration file",
	    			       null, file.getName(), 0, 0);
	  }
	}
	else if (element.equals("DATA_WIDTH")) {
	  try {dataWidth = Integer.parseInt(s.trim());}
	  catch (NumberFormatException x) {
	    throw new SAXParseException("Bad value for \"DATA_WIDTH\" in configuration file",
	    			       null, file.getName(), 0, 0);
	  }
	}
	else if (element.equals("DATA_HEIGHT")) {
	  try {dataHeight = Integer.parseInt(s.trim());}
	  catch (NumberFormatException x) {
	    throw new SAXParseException("Bad value for \"DATA_HEIGHT\" in configuration file",
	    			       null, file.getName(), 0, 0);
	  }
	}
	else if (element.equals("DATA_XPOS")) {
	  try {dataX = Integer.parseInt(s.trim());}
	  catch (NumberFormatException x) {
	    throw new SAXParseException("Bad value for \"DATA_XPOS\" in configuration file",
	    			       null, file.getName(), 0, 0);
	  }
	}
	else if (element.equals("DATA_YPOS")) {
	  try {dataY = Integer.parseInt(s.trim());}
	  catch (NumberFormatException x) {
	    throw new SAXParseException("Bad value for \"DATA_YPOS\" in configuration file",
	    			       null, file.getName(), 0, 0);
	  }
	  // Once this is reached, all other window parameters have been
	  // specified, so go ahead and set windows to desired size & position.
	  viewer.setMainWindowBounds(new Rectangle(mainX, mainY, mainWidth, mainHeight));
	  viewer.setDataWindowBounds(new Rectangle(dataX, dataY, dataWidth, dataHeight));
	}
	else if (element.equals("IN_TABBED_PANE")) {
	  value = s.trim().toLowerCase();
	  if (value.equals("yes")) {
	    viewer.putInTabbedPane(true);
	  }
	  else if (value.equals("no")) {
	    viewer.putInTabbedPane(false);
	  }
	  else {
	    throw new SAXParseException("Bad value for \"IN_TABBED_PANE\" in configuration file",
	    			       null, file.getName(), 0, 0);
	  }
	}
	else if (element.equals("UPDATE_PERIOD")) {
	  int time;
	  try {
	    time = Integer.parseInt(s.trim());
	  }
	  catch (NumberFormatException x) {
	    throw new SAXParseException("Bad value for \"UPDATE_PERIOD\" in configuration file",
	    			       null, file.getName(), 0, 0);
	  }
	  viewer.setUpdatePeriod(time);
	}
	else if (element.equals("SHOW_SYSTEM")) {
	  value = s.trim().toLowerCase();
	  if (value.equals("yes")) {
	    viewer.showSystem(true);
	  }
	  else if (value.equals("no")) {
	    viewer.showSystem(false);
	  }
	  else {
	    throw new SAXParseException("Bad value for \"IN_TABBED_PANE\" in configuration file",
	    				null, file.getName(), 0, 0);
	  }
	}
	else if (element.equals("SHOW_IDLE")) {
	  value = s.trim().toLowerCase();
	  if (value.equals("yes")) {
	    viewer.showIdleStations(true);
	  }
	  else if (value.equals("no")) {
	    viewer.showIdleStations(false);
	  }
	  else {
	    throw new SAXParseException("Bad value for \"SHOW_IDLE\" in configuration file",
	    				null, file.getName(), 0, 0);
	  }
	}
	else if (element.equals("SHOW_CONFIGURATION")) {
	  value = s.trim().toLowerCase();
	  if (value.equals("yes")) {
	    viewer.showConfiguration(true);
	  }
	  else if (value.equals("no")) {
	    viewer.showConfiguration(false);
	  }
	  else {
	    throw new SAXParseException("Bad value for \"SHOW_CONFIGURATION\" in configuration file",
	    				null, file.getName(), 0, 0);
	  }
	}
	else if (element.equals("SHOW_ATTACHMENTS")) {
	  value = s.trim().toLowerCase();
	  if (value.equals("yes")) {
	    viewer.showAttachments(true);
	  }
	  else if (value.equals("no")) {
	    viewer.showAttachments(false);
	  }
	  else {
	    throw new SAXParseException("Bad value for \"SHOW_ATTACHMENTS\" in configuration file",
	    				null, file.getName(), 0, 0);
	  }
	}
	else if (element.equals("SHOW_RATE")) {
	  value = s.trim().toLowerCase();
	  if (value.equals("yes")) {
	    viewer.showRate(true);
	  }
	  else if (value.equals("no")) {
	    viewer.showRate(false);
	  }
	  else {
	    throw new SAXParseException("Bad value for \"SHOW_RATE\" in configuration file",
	    				null, file.getName(), 0, 0);
	  }
	}
	else if (element.equals("SHOW_EVENTS")) {
	  value = s.trim().toLowerCase();
	  if (value.equals("yes")) {
	    viewer.showEvents(true);
	  }
	  else if (value.equals("no")) {
	    viewer.showEvents(false);
	  }
	  else {
	    throw new SAXParseException("Bad value for \"SHOW_EVENTS\" in configuration file",
	    				null, file.getName(), 0, 0);
	  }
	}
	// make connections
	else if (element.equals("ET_NAME")) {
	  etName = s.trim();
	}
	else if (element.equals("HOST")) {
	  etHost = s.trim();
	}
	else if (element.equals("PORT")) {
	  try {
	    port = Integer.parseInt(s.trim());
	  }
	  catch (NumberFormatException x) {
	    throw new SAXParseException("Bad value for \"PORT\" in configuration file",
	    				null, file.getName(), 0, 0);
	  }
	  // reaching here means we already have etName and etHost,
	  // so make a direct connection
	  EtSystem etSys = new EtSystem(etName, etHost, port);
	  viewer.makeConnection(etSys, viewer);
	}

      }

      //==========================
      // SAX ErrorHandler methods
      //==========================

      // treat validation errors as fatal
      public void error (SAXParseException e) throws SAXException {
          throw e;
      }

      // dump warnings too
      public void warning (SAXParseException err) throws SAXException {
          System.out.println ("** Warning"
              + ", line " + err.getLineNumber ()
              + ", uri " + err.getSystemId ());
          System.out.println("   " + err.getMessage ());
      }
    }
 
    try {
        // Parse the file
	
        // Xerces, SAX2
        XMLReader xr = new SAXParser();
        MyHandler handler = new MyHandler();
	//xr.setFeature("http://xml.org/sax/features/validation", true);
	xr.setContentHandler(handler);
        xr.setErrorHandler(handler);
        xr.parse(new InputSource(new FileReader(file.getName())));
	
    }
    catch (SAXParseException spe) {
       // Error generated by the parser
       System.out.println ("\n** Parsing error" 
          + ", line " + spe.getLineNumber ()
          + ", uri " + spe.getSystemId ());
       System.out.println("   " + spe.getMessage() );

       // Use the contained exception, if any
       Exception  x = spe;
       if (spe.getException() != null)
           x = spe.getException();
       x.printStackTrace();

    }
    catch (SAXException sxe) {
       // Error generated by this application
       // (or a parser-initialization error)
       Exception  x = sxe;
       if (sxe.getException() != null)
           x = sxe.getException();
       x.printStackTrace();
    
    }
  }
  
  
  
}
