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
 *      Java class for displaying data from a single ET system
 *
 * Author:
 *	Carl Timmer
 *	TJNAF Data Acquisition Group
 *
 * Revision History:
 *
 *----------------------------------------------------------------------------*/

import java.lang.*;
import java.util.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;
import javax.swing.*;
import javax.swing.border.*;

// change GUI only in Event Dispatch thread
public class EtDataDisplay extends Thread {

  private EtViewer viewer;
  private EtAllData data;
  private EtConnection connection;
  private JPanel panel;
  private JLabel systemDynamicValues;
  private JLabel processInfo;
  private HashMap rates;
  private boolean initGraphics, systemIsShown;
  // for stopping thread handling connection
  private Thread parentThread;
  private final JScrollPane scrollPane = new JScrollPane();
  
  // static members
  private static EtDataDisplayOptions options = new EtDataDisplayOptions();
    
  public static void setDisplayOptions(EtDataDisplayOptions newOptions) {
    options = new EtDataDisplayOptions(newOptions);
  }
  
  
  // constructor
  public EtDataDisplay(EtAllData data, EtViewer viewer,
		       EtConnection connection, HashMap rates) {
    this.data   = data;
    this.viewer = viewer;
    this.rates  = rates;
    this.connection = connection;
    
    initGraphics  = true;
    systemIsShown = false;
    
    // save thread id for future thread interruption
    parentThread = Thread.currentThread();
    
    // set font for widget
    systemDynamicValues = new JLabel();
    systemDynamicValues.setFont(EtFonts.displayFont);
    systemDynamicValues.setForeground(EtColors.text);
    //scrollPane.getViewport().setBackingStoreEnabled(true);
  }
  
  
  // graphics stuff done only once
  private void initializeGraphics() {    
    // name for tabbed pane that holds scrolled pane
    String tabName;
    // check if name has "." in it
    if (connection.getHost().indexOf(".") < 0) {
      tabName = new String(
	  connection.getEtName() + " (" +  connection.getHost() + ")");
    }
    else {
      tabName = new String(
	  connection.getEtName() + " (" +
	  connection.getHost().substring(0, connection.getHost().indexOf(".")) +
	  ")"
      );
    }
    
    // final JScrollPane scrollPane = new JScrollPane();
    // create panel to hold ET system info in scrolled pane
    panel = new JPanel(true);
    panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
    //panel.setMinimumSize(new Dimension(2000,1000));
    //panel.setPreferredSize(new Dimension(1000,600));
    //panel.setMaximumSize(new Dimension(3000,2000));
    
    scrollPane.setViewportView(panel);
    scrollPane.setName(tabName);
    
    if (viewer.inTabbedPane()) {
      viewer.tabbedPane.addTab(tabName, scrollPane);
    }
    else {
      viewer.ddwPanel.add(scrollPane);
    }
    viewer.ddwPanel.updateUI();
    
    // make menu item used to remove this connection, display, etc.
    final JMenuItem menuItem = new JMenuItem(tabName);
    menuItem.setBackground(EtColors.background);
    viewer.disconnectMenu.add(menuItem);
    menuItem.addActionListener(
      new ActionListener() {
	public void actionPerformed(ActionEvent e) {
          // remove menu item just added above
	  viewer.disconnectMenu.remove(menuItem);
	  // remove the whole display for this connection
	  if (viewer.inTabbedPane()) {
            viewer.tabbedPane.remove(scrollPane);
 	  }
	  else {
	    viewer.ddwPanel.remove(scrollPane);
	    viewer.ddwPanel.updateUI();
	  }
	  // interrupt the thread handling the connection
	  parentThread.interrupt();
        }
      }
    );
    
  }
    
    
  public void run() {
    // set viewing options
    options.setAll(viewer.showSystem(),
		   viewer.showIdleStations(),
		   viewer.showConfiguration(),
		   viewer.showAttachments(),
		   viewer.showRate(),
		   viewer.showEvents(),
		   viewer.inTabbedPane()
    );
    
    if (initGraphics) {
      initializeGraphics();
      panel.add(systemDisplayStatic());
      if (options.showSystem() == true) {
	systemIsShown = true;
      }
      initGraphics = false;
    }
    
    // Delete all panels and redraw except if we want to
    // show system data and it's currently showing.
    int limit = 0;
    if (options.showSystem() && systemIsShown) {
      limit = 1;
    }
    else {
      systemIsShown = false;
    }
    
    // remove existing panels
    for (int i=panel.getComponentCount(); i>limit ; i--) {
      panel.remove(i-1);
    }
    
    // update system info
    if (options.showSystem()) {
      if (systemIsShown == false) {
        panel.add(systemDisplayStatic());
      }
      systemDisplayUpdate();
      systemIsShown = true;
    }
    else if (systemIsShown == false) {
      // draw only title label
      panel.add(systemDisplayStatic());
    }
    
    // add new station and attachment info
    for (int i=0; i<data.statData.length ; i++) {
      if ((options.showIdleStations() == false) &&
	  (data.statData[i].getStatus() != EtConstants.stationActive()) ) {
        continue;
      }
      stationDisplay(data.statData[i]);
    }
    panel.add(Box.createHorizontalGlue());

    
    // update GUI
    //Dimension dim = scrollPane.getViewport().getExtentSize();
//System.out.println(" extent size = " + dim);
    //Dimension vs = scrollPane.getViewport().getViewSize();
//System.out.println(" view size = " + vs);
   // Point pt = scrollPane.getViewport().getViewPosition();
//System.out.println(" view position = " + pt);
   // Rectangle rec = scrollPane.getViewport().getViewRect();
//System.out.println(" rectangle size = " + rec);
    //Rectangle setRec = new Rectangle(new Point(0,0), scrollPane.getViewport().getExtentSize());
    //scrollPane.getViewport().scrollRectToVisible(setRec);
    //scrollPane.getViewport().setViewPosition(new Point(0,0));
    //panel.revalidate();
    //scrollPane.getViewport().reshape(0,0,
	//	panel.getWidth(), panel.getHeight());
    panel.updateUI();
    
   }
  

  // display panel of system info's static parts
  private JPanel systemDisplayStatic() {
    // construct widgets that need to be done only once
    
    // System name
    String title = new String(
	"<html><p style=\"text-align: center\"<b>" +
	EtFonts.titleHTML +
	connection.getEtName() +
	"</font></b></p><p style=\"text-align: center\"<b>" +
	EtFonts.titleHTML +
	connection.getHost() +
	"</font></b></p>"
    );
    JLabel name = new JLabel();
    name.setMaximumSize(new Dimension(400, 40));
    name.setOpaque(true);
    name.setForeground(EtColors.text);
    name.setBackground(EtColors.background);
    name.setText(title);
    name.setHorizontalAlignment(JLabel.CENTER);
    name.setAlignmentX(Component.CENTER_ALIGNMENT);
    name.setBorder(new CompoundBorder(new SoftBevelBorder(BevelBorder.RAISED), 
				      new LineBorder(EtColors.border, 2)));

    if (options.showSystem() == false) {
      JPanel systemPanel = new JPanel();
      systemPanel.setLayout(new BoxLayout(systemPanel, BoxLayout.Y_AXIS));
      systemPanel.setBorder(new EmptyBorder(10,10,10,5));
      systemPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
      systemPanel.setAlignmentY(Component.TOP_ALIGNMENT);
      systemPanel.add(name);
      systemPanel.add(Box.createVerticalGlue());
      return systemPanel;
    }
    
    // list of static parameters
    JLabel systemStaticItems = new JLabel();
    //systemStaticItems.setFont(EtFonts.displayFont);
    systemStaticItems.setForeground(EtColors.text);
    systemStaticItems.setBorder(new EmptyBorder(0,0,0,10));
    StringBuffer text = new StringBuffer(400);
    text.append("<html>");
    text.append(EtFonts.displayHTML);
    text.append("<p>Configuration:</p>");
    text.append("<p>   pid</p><p>   host</p><p>   total events</p><p>   normal event size</p>");
    text.append("<p>   max temp events</p><p>   max stations</p><p>   max attachments</p>");
    text.append("<p>   max processes</p><p>   select ints</p><p>   mutexes</p><p>   endian</p>");
    text.append("<p>   tcp server port</p>"); 
    for (int i=0; i < data.sysData.getInterfaceCount(); i++) {
      text.append("<p>   network interface ");
      text.append(i+1); 
      text.append("</p>"); 
    }
    for (int i=0; i < data.sysData.getMulticastCount(); i++) {
      text.append("<p>   multicast address ");
      text.append(i+1); 
      text.append("</p>"); 
    } 
    text.append("</font>"); 
    systemStaticItems.setText(text.toString());
    
    // list of dynamic parameters   
    text = new StringBuffer(400);
    text.append("<html>");
    text.append(EtFonts.displayHTML);
    text.append("<p>Current Status:</p>");
    text.append("<p>   alive</p><p>   heartbeat</p><p>   owned events</p><p>   temp  events</p>");
    text.append("<p>   stations</p><p>   attachments</p><p>   local processes</p>");
    text.append("<p>   system mutex</p><p>   station mutex</p><p>   add station mutex</p></font>");
    JLabel systemDynamicItems = new JLabel();
    //systemDynamicItems.setFont(EtFonts.displayFont);
    systemDynamicItems.setForeground(EtColors.text);
    systemDynamicItems.setBorder(new EmptyBorder(0,0,0,10));
    systemDynamicItems.setText(text.toString());
    
    // static item values    
    text = new StringBuffer(400);
    text.append("<html>");
    text.append(EtFonts.displayHTML);
    text.append("<p></p><p>"); text.append(data.sysData.getMainPid());
    text.append("</p><p>"); text.append(connection.getHost());
    text.append("</p><p>"); text.append(data.sysData.getEvents());
    text.append("</p><p>"); text.append(data.sysData.getEventSize());
    text.append("</p><p>"); text.append(data.sysData.getTempsMax());
    text.append("</p><p>"); text.append(data.sysData.getStationsMax());
    text.append("</p><p>"); text.append(data.sysData.getAttachmentsMax());
    text.append("</p><p>"); text.append(data.sysData.getProcessesMax());
    text.append("</p><p>"); text.append(data.sysData.getSelects());
    if (data.sysData.getShare() == EtConstants.mutexShare()) {
      text.append("</p><p>can share");
    }
    else {
      text.append("</p><p>cannot share");
    }
    if (data.sysData.getEndian() == EtConstants.bigEndian()) {
      text.append("</p><p>big");
    }
    else {
      text.append("</p><p>little");
    }
    text.append("</p><p>"); text.append(data.sysData.getTcpPort());
    if (data.sysData.getInterfaceCount() > 0) {
      String interfaces[] = data.sysData.getInterfaceAddresses();
      for (int i=0; i < data.sysData.getInterfaceCount(); i++) {
        text.append("</p><p>"); text.append(interfaces[i]);
      }
      interfaces = null;
    }
    if (data.sysData.getMulticastCount() > 0) {
      String addresses[] = data.sysData.getMulticastAddresses();
      for (int i=0; i < data.sysData.getMulticastCount(); i++) {
        text.append("</p><p>"); text.append(addresses[i]);
      }
      addresses = null;
    }
    text.append("</p></font>");
    
    JLabel systemStaticValues = new JLabel();
    systemStaticValues.setForeground(EtColors.text);
    systemStaticValues.setText(text.toString());
    //systemStaticValues.setFont(EtFonts.displayFont);
    
    
    // process info widget gets its info updated regularly
    processInfo = new JLabel();
    processInfo.setFont(EtFonts.displayFont);
    processInfo.setForeground(EtColors.text);
    processInfo.setAlignmentX(Component.CENTER_ALIGNMENT);
    processInfo.setBorder(new EmptyBorder(5,5,5,5));
    
    // organize static stuff into one panel
    JPanel upperPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
    upperPanel.setAlignmentX(Component.CENTER_ALIGNMENT);
    upperPanel.setBackground(EtColors.textBackground);
    upperPanel.add(systemStaticItems);
    upperPanel.add(systemStaticValues);
    upperPanel.setBorder(new EmptyBorder(5,5,5,5));
    
    // organize static stuff into one panel
    JPanel lowerPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
    lowerPanel.setAlignmentX(Component.CENTER_ALIGNMENT);
    lowerPanel.setBackground(EtColors.textBackground);
    lowerPanel.add(systemDynamicItems);
    lowerPanel.add(systemDynamicValues);
    lowerPanel.setBorder(new EmptyBorder(5,5,5,5));
    
    // put static & dynamic stuff into one panel
    JPanel allTheStuff = new JPanel();
    allTheStuff.setBackground(EtColors.textBackground);
    allTheStuff.setLayout(new BoxLayout(allTheStuff, BoxLayout.Y_AXIS));
    allTheStuff.setBorder(new SoftBevelBorder(BevelBorder.RAISED));
    allTheStuff.setMaximumSize(new Dimension(500, 1500));
    allTheStuff.add(name);
    allTheStuff.add(upperPanel);
    allTheStuff.add(lowerPanel);
    allTheStuff.add(processInfo);
   
    JPanel systemPanel = new JPanel();
    systemPanel.setLayout(new BoxLayout(systemPanel, BoxLayout.Y_AXIS));
    systemPanel.setBorder(new EmptyBorder(10,10,10,5));
    systemPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
    systemPanel.setAlignmentY(Component.TOP_ALIGNMENT);
    systemPanel.add(name);
    systemPanel.add(allTheStuff);
    systemPanel.add(Box.createVerticalGlue());
    //systemPanel.setMaximumSize(new Dimension(500,1500));
   
    return systemPanel;
  } 
  
  
  // update the system display's dynamic info
  private void systemDisplayUpdate() {
    StringBuffer text = new StringBuffer(400);
    
    text.append("<html>");
    text.append(EtFonts.displayHTML);
    
    if (data.sysData.getAlive() == 1) {
      text.append("<p></p><p>yes");
    } 
    else {
      text.append("<p></p><p>no");
    }
    
    text.append("</p><p>"); text.append(data.sysData.getHeartbeat());
    text.append("</p><p>"); text.append(data.sysData.getEventsOwned());
    text.append("</p><p>"); text.append(data.sysData.getTemps());
    text.append("</p><p>"); text.append(data.sysData.getStations());
    text.append("</p><p>"); text.append(data.sysData.getAttachments());
    text.append("</p><p>"); text.append(data.sysData.getProcesses());
    
    if (data.sysData.getMutex() == EtConstants.mutexLocked()) {
      text.append("</p><p>locked");
    } 
    else {
      text.append("</p><p>unlocked");
    }
    
    if (data.sysData.getStatMutex() == EtConstants.mutexLocked()) {
      text.append("</p><p>locked");
    }
    else {
      text.append("</p><p>unlocked");
    }
    
    if (data.sysData.getStatAddMutex() == EtConstants.mutexLocked()) {
      text.append("</p><p>locked");
    }
    else {
      text.append("</p><p>unlocked");
    }
    text.append("</p></font>");
    systemDynamicValues.setText(text.toString());
    
    // process info
    text = new StringBuffer(2000);
    text.append("<html>");
    text.append(EtFonts.displayHTML);
    text.append("<p>LOCAL PROCESSES MAPPING</p><p>SHARED MEMORY:</p><p>");
    
    if (data.procData.length < 1) {
	text.append("</p><p>  no processes exist</p>");
    }
    else {
      for (int i=0; i < data.procData.length; i++) {
	text.append("</p><p>Process #");
	text.append(data.procData[i].getNum());
	text.append( "</p><p>  pid ");
	text.append(data.procData[i].getPid());
	text.append(",  heartbeat ");
	text.append(data.procData[i].getHeartbeat());
	text.append("</p><p>  ");
	text.append(data.procData[i].getAttachments());
	text.append(" attachments with Ids ");

	int attachmentIds[] = data.procData[i].getAttIds();
	for (int j=0, k=0; j < attachmentIds.length; j++) {
          if (attachmentIds[j] < 0) {
	    continue;
	  }
	  if (k++ == 0) {
            text.append(attachmentIds[j]);
	  }
	  else {
            text.append(","); text.append(attachmentIds[j]);
	  }
	}
      }
      text.append("</p></font>");
    }
    processInfo.setText(text.toString());
  }
   
  
  // display all station/attachment data for 1 station
  private void stationDisplay(EtStationData sData) {
    
    // panel to hold all station/attachment data
    JPanel statAndAttPanel = new JPanel();
    statAndAttPanel.setLayout(new BoxLayout(statAndAttPanel, BoxLayout.Y_AXIS));
    statAndAttPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
    statAndAttPanel.setAlignmentY(Component.TOP_ALIGNMENT);
    statAndAttPanel.setMaximumSize(new Dimension(400, 2000));
    statAndAttPanel.setBorder(new EmptyBorder(10,5,10,5));
    
    // station name
    JLabel name = new JLabel(sData.getStationName(), JLabel.CENTER);
    name.setMaximumSize(new Dimension(400, 40));
    name.setOpaque(true);
    name.setForeground(EtColors.text);
    if (sData.getStatus() == EtConstants.stationActive()) {
      name.setBackground(EtColors.active);
    }
    else {
      name.setBackground(EtColors.inActive);
    }
    name.setAlignmentX(Component.CENTER_ALIGNMENT);
    name.setBorder(new CompoundBorder(new SoftBevelBorder(BevelBorder.RAISED), 
				      new LineBorder(EtColors.border, 2)));
    name.setFont(EtFonts.titleFont);
    statAndAttPanel.add(name);
    
    // show configuration data
    if (options.showConfiguration() == true) {
      statAndAttPanel.add(stationConfigDisplay(sData));
    }
    
    // show rate data
    if (options.showRate() == true) {
      // rate
      Integer rate = (Integer) rates.get(sData.getStationName());
      if (rate == null) {
	rate = new Integer(0);
      }
      JLabel inRate = new JLabel();
      inRate.setBorder(new SoftBevelBorder(BevelBorder.RAISED));
      inRate.setFont(EtFonts.rateFont);
      inRate.setOpaque(true);
      inRate.setForeground(EtColors.text);
      inRate.setBackground(EtColors.textBackground);
      inRate.setMaximumSize(new Dimension(400, 40));
      inRate.setAlignmentX(Component.CENTER_ALIGNMENT);
      inRate.setHorizontalAlignment(JLabel.CENTER);
      inRate.setText(rate + " Hz");
      statAndAttPanel.add(inRate);
    }
    
    // show input and output list data
    if (options.showEvents() == true) {
      statAndAttPanel.add(stationEventsDisplay(sData));
    }
    
    // add attachment panels - make one for each
    if (options.showAttachments() == true) {
      int atts[] = sData.getAttIds();
      int foundAtts = 0, stationAttsTotal = sData.getAttachments();

      // for each station attachment id ...
      for (int i=0; i < atts.length ; i++) {
	if (atts[i] < 0) continue;
	// for each real attachment in ET system ...
	for (int j=0; j < data.attData.length; j++) {
          // if one of station's attachment ids is found, display it
	  if (data.attData[j].getNum() == atts[i]) {
	    statAndAttPanel.add(Box.createRigidArea(new Dimension(0, 10)));
	    statAndAttPanel.add(attachmentDisplay(data.attData[j]));
	    break;
	  }
	}
	if (++foundAtts == stationAttsTotal) break;
      }
    }
    
    // add glue to force panels up to the top
    statAndAttPanel.add(Box.createVerticalGlue());
    
    // add to panel in scrolled pane
    panel.add(statAndAttPanel);
  }
  
  
  // display all station configuration data for 1 station
  private JPanel stationConfigDisplay(EtStationData sData) {
    // panel for configuration info
    JPanel configPanel = new JPanel();
    configPanel.setLayout(new BoxLayout(configPanel, BoxLayout.Y_AXIS));
    configPanel.setBorder(new SoftBevelBorder(BevelBorder.RAISED));
    configPanel.setBackground(EtColors.textBackground);
    configPanel.setMaximumSize(new Dimension(400, 500));
    
    // configuration info
    JLabel configInfo = new JLabel();
    configInfo.setFont(EtFonts.displayFont);
    configInfo.setForeground(EtColors.text);
    configInfo.setAlignmentX(Component.CENTER_ALIGNMENT);
    configInfo.setBorder(
	new TitledBorder(new LineBorder(EtColors.border),
			"Configuration",
			TitledBorder.LEFT,
			TitledBorder.TOP,
			EtFonts.displayFont,
			EtColors.text)
    );
        
    StringBuffer text = new StringBuffer(300);
    
    text.append("<html><font size=1><p> id="); text.append(sData.getNum());
    
    if (sData.getBlockMode() == EtConstants.stationBlocking()) {
      text.append(", blocking, ");
    }
    else {
      text.append(", nonblocking, ");
    }
    
    if (sData.getUserMode() == EtConstants.stationMultiUser()) {
      text.append("multi-user</p><p>");
    }
    else {
      text.append("single-user</p><p>");
    }

    if (sData.getRestoreMode() == EtConstants.stationRestoreOut()) {
      text.append(" restore events to output </p><p>");
    }
    else if (sData.getRestoreMode() == EtConstants.stationRestoreIn()) {
      text.append(" restore events to input </p><p>");
    }
    else {
      text.append(" restore events to GC </p><p>");
    }
    
    if (sData.getSelectMode() == EtConstants.stationSelectAll()) {
      text.append(" prescale = "); text.append(sData.getPrescale());
      text.append(", cue = "); text.append(sData.getCue());
      text.append(" </p><p> accept all events</p><p></p><p></p></font>");
    }
    else if (sData.getRestoreMode() == EtConstants.stationSelectUser()) {
      text.append(" prescale = "); text.append(sData.getPrescale());
      text.append(", cue = "); text.append(sData.getCue());
      text.append(" </p><p> user event selection</p><p>   select ints = ");
      
      int selectInts[] = sData.getSelect();
      for (int i=0; i < selectInts.length; i++) {
	if (i == 0) {
	  text.append(selectInts[i]);
	}
	else {
	  text.append(","); text.append(selectInts[i]);
	}
      }
      
      text.append("</p><p>   lib = "); text.append(sData.getLibrary());
      text.append("</p><p>   func = "); text.append(sData.getFunction());
      text.append("</p></font>");
    }
    else {
      text.append(" prescale = "); text.append(sData.getPrescale());
      text.append(", cue = "); text.append(sData.getCue());
      text.append(" </p><p> builtin event selection</p><p>   select ints = ");
      
      int selectInts[] = sData.getSelect();
      for (int i=0; i < selectInts.length; i++) {
	if (i == 0) {
	  text.append(selectInts[i]);
	}
	else {
	  text.append(","); text.append(selectInts[i]);
	}
      }
      text.append("</p><p></p></font>");
     }
    configInfo.setText(text.toString());
    configPanel.add(configInfo);
    return configPanel;
  }
  
  
  // display all station input/output list data for 1 station
  private JPanel stationEventsDisplay(EtStationData sData) {
    // hold everything
    JPanel all = new JPanel();
    all.setLayout(new BoxLayout(all, BoxLayout.Y_AXIS));
    all.setBorder(new SoftBevelBorder(BevelBorder.RAISED));
    all.setBackground(EtColors.textBackground);
    all.setMaximumSize(new Dimension(400, 400));
    
    // transfer mutex status
    JLabel tMutex = new JLabel();
    tMutex.setFont(EtFonts.displayFont);
    tMutex.setBackground(EtColors.textBackground);
    tMutex.setMaximumSize(new Dimension(400, 40));
    tMutex.setAlignmentX(Component.CENTER_ALIGNMENT);
    tMutex.setHorizontalAlignment(JLabel.CENTER);
    tMutex.setOpaque(true);
    if (sData.getMutex() == EtConstants.mutexUnlocked()) {
      tMutex.setForeground(EtColors.unlocked);
      tMutex.setText("transfer mutex unlocked");
    }
    else {
      tMutex.setForeground(EtColors.locked);
      tMutex.setText("transfer mutex locked");
    }
    all.add(tMutex);
    
    // hold input and output panels
    JPanel inAndOut = new JPanel();
    inAndOut.setAlignmentX(Component.CENTER_ALIGNMENT);
    inAndOut.setAlignmentY(Component.TOP_ALIGNMENT);
    inAndOut.setBackground(EtColors.textBackground);
    
    // split things into input list & output list panels
    JPanel input = new JPanel();
    input.setBackground(EtColors.textBackground);
    input.setLayout(new BoxLayout(input, BoxLayout.Y_AXIS));
    input.setAlignmentX(Component.CENTER_ALIGNMENT);
    input.setBorder(new LineBorder(EtColors.inList, 2));

    JPanel output = new JPanel();
    output.setBackground(EtColors.textBackground);
    output.setLayout(new BoxLayout(output, BoxLayout.Y_AXIS));
    output.setAlignmentX(Component.CENTER_ALIGNMENT);
    output.setBorder(new LineBorder(EtColors.outList, 2));
    
    // input & output list labels
    JLabel inLabel  = new JLabel(" INPUT LIST  ", JLabel.CENTER);
    inLabel.setFont(EtFonts.displayFont);
    inLabel.setOpaque(true);
    inLabel.setForeground(EtColors.text);
    inLabel.setBackground(EtColors.textBackground);
    inLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
    inLabel.setBorder(new LineBorder(EtColors.inList, 1));
    input.add(inLabel);
    
    JLabel outLabel = new JLabel(" OUTPUT LIST ", JLabel.CENTER);
    outLabel.setFont(EtFonts.displayFont);
    outLabel.setOpaque(true);
    outLabel.setForeground(EtColors.text);
    outLabel.setBackground(EtColors.textBackground);
    outLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
    outLabel.setBorder(new LineBorder(EtColors.outList, 1));
    output.add(outLabel);
    
    // mutexes
    JLabel inMutex  = new JLabel();
    inMutex.setFont(EtFonts.displayFont);
    inMutex.setAlignmentX(Component.CENTER_ALIGNMENT);
    
    inMutex.setHorizontalAlignment(JLabel.CENTER);
    inMutex.setOpaque(true);
    inMutex.setBackground(EtColors.textBackground);
    if (sData.getInListMutex() == EtConstants.mutexUnlocked()) {
      inMutex.setText(" unlocked ");
      inMutex.setForeground(EtColors.unlocked);
    }
    else {
      inMutex.setText(" locked ");
      inMutex.setForeground(EtColors.locked);
    }
    input.add(inMutex);
    
    JLabel outMutex = new JLabel();
    outMutex.setFont(EtFonts.displayFont);
    outMutex.setAlignmentX(Component.CENTER_ALIGNMENT);
    outMutex.setHorizontalAlignment(JLabel.CENTER);
    outMutex.setOpaque(true);
    outMutex.setBackground(EtColors.textBackground);
    if (sData.getOutListMutex() == EtConstants.mutexUnlocked()) {
      outMutex.setText(" unlocked ");
      outMutex.setForeground(EtColors.unlocked);
    }
    else {
      outMutex.setText(" locked ");
      outMutex.setForeground(EtColors.locked);
    }
    output.add(outMutex);
    
    // total events in & out
    JLabel inTotalLabel = new JLabel("Total In = ", JLabel.CENTER);
    inTotalLabel.setOpaque(true);
    inTotalLabel.setForeground(EtColors.text);
    inTotalLabel.setBackground(EtColors.textBackground);
    inTotalLabel.setFont(EtFonts.displayFont);
    inTotalLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
    input.add(inTotalLabel);
    
    JLabel outTotalLabel = new JLabel("Total Out =", JLabel.CENTER);
    outTotalLabel.setOpaque(true);
    outTotalLabel.setForeground(EtColors.text);
    outTotalLabel.setBackground(EtColors.textBackground);
    outTotalLabel.setFont(EtFonts.displayFont);
    outTotalLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
    output.add(outTotalLabel);
    
    JLabel inTotal = new JLabel("" + sData.getInListIn(), JLabel.CENTER);
    inTotal.setOpaque(true);
    inTotal.setForeground(EtColors.text);
    inTotal.setBackground(EtColors.textBackground);
    inTotal.setFont(EtFonts.displayFont);
    inTotal.setAlignmentX(Component.CENTER_ALIGNMENT);
    input.add(inTotal);
    
    JLabel outTotal = new JLabel("" + sData.getOutListOut(), JLabel.CENTER);
    outTotal.setOpaque(true);
    outTotal.setForeground(EtColors.text);
    outTotal.setBackground(EtColors.textBackground);
    outTotal.setFont(EtFonts.displayFont);
    outTotal.setAlignmentX(Component.CENTER_ALIGNMENT);
    output.add(outTotal);
    
    // event count
    
    // find limits of input & output lists
    int inputLimit, outputLimit;
    outputLimit = inputLimit = data.sysData.getEvents();
    if (sData.getBlockMode() == EtConstants.stationNonBlocking()) {
      inputLimit = sData.getCue();
    }

    JProgressBar inCount  = new JProgressBar(JProgressBar.VERTICAL, 0, inputLimit);
    inCount.setBorder(new LineBorder(EtColors.inList));
    inCount.setAlignmentX(Component.CENTER_ALIGNMENT);
    inCount.setForeground(EtColors.inList);
    inCount.setValue(sData.getInListCount());
    inCount.setStringPainted(true);
    inCount.setString("" + sData.getInListCount());
    inCount.setMaximumSize(new Dimension(100, 100));
    inCount.setPreferredSize(new Dimension(50, 75));
    input.add(inCount);
    
    JProgressBar outCount = new JProgressBar(JProgressBar.VERTICAL, 0, outputLimit);
    outCount.setBorder(new LineBorder(EtColors.outList));
    outCount.setAlignmentX(Component.CENTER_ALIGNMENT);
    outCount.setForeground(EtColors.outList);
    outCount.setValue(sData.getOutListCount());
    outCount.setStringPainted(true);
    outCount.setString("" + sData.getOutListCount());
    outCount.setMaximumSize(new Dimension(100, 100));
    outCount.setPreferredSize(new Dimension(50, 75));
    output.add(outCount);    
    
    inAndOut.add(input);
    inAndOut.add(output);
    all.add(inAndOut);
    return all;
  }
  
  
  // display attachment data
  private JPanel attachmentDisplay(EtAttachmentData aData) {
    
    JPanel attPanel = new JPanel();
    attPanel.setLayout(new BoxLayout(attPanel, BoxLayout.Y_AXIS));
    attPanel.setBorder(new SoftBevelBorder(BevelBorder.RAISED));
    attPanel.setBackground(EtColors.textBackground);
    attPanel.setMaximumSize(new Dimension(400, 500));
    
    // name, id
    JLabel name = new JLabel("Attachment " + aData.getNum(), JLabel.CENTER);
    name.setMaximumSize(new Dimension(400, 40));
    name.setOpaque(true);
    name.setForeground(EtColors.text);
    name.setBackground(EtColors.textBackground);
    name.setAlignmentX(Component.CENTER_ALIGNMENT);
    name.setBorder(new LineBorder(EtColors.border, 2));
    name.setFont(EtFonts.titleFont);
    attPanel.add(name);
    
    // configuration info
    JLabel attInfo = new JLabel();
    attInfo.setFont(EtFonts.displayFont);
    attInfo.setForeground(EtColors.text);
    attInfo.setBorder(new  EmptyBorder(5,5,5,5));
    attInfo.setAlignmentX(Component.CENTER_ALIGNMENT);
        
    StringBuffer text = new StringBuffer(100);
    text.append("<html>");
    text.append(EtFonts.displayHTML);
    text.append("<p>pid "); text.append(aData.getPid());
    text.append(" on "); text.append(aData.getHost());
    
    if (aData.getProc() < 0) {
      text.append("</p><p>through ET server ");
    }
    else {
      text.append("</p><p>process #" + aData.getProc());
    }
    
    int addNLines=0;
    if (aData.getBlocked() == EtConstants.attachBlocked()) {
      text.append("</p><p>blocked in read");
    }
    else {addNLines++;}
    
    if (aData.getQuit() == EtConstants.attachQuit()) {
      text.append("</p><p>quitting read");
    }
    else {addNLines++;}
    
    if (addNLines > 0) {
      for (int i=0; i<addNLines; i++) {
        text.append("</p><p>");
      }
    }
    text.append("</p></font");
    attInfo.setText(text.toString());
    attPanel.add(attInfo);
    
    JPanel eventPanel = new JPanel();
    eventPanel.setLayout(new BoxLayout(eventPanel, BoxLayout.X_AXIS));
    eventPanel.setBorder(new  EmptyBorder(5,5,5,5));
    eventPanel.setBackground(EtColors.textBackground);
    
    JLabel eventType = new JLabel();
    eventType.setFont(EtFonts.displayFont);
    eventType.setForeground(EtColors.text);
    eventType.setAlignmentX(Component.CENTER_ALIGNMENT);
    eventType.setText("<html>"+ EtFonts.displayHTML +"<p>Events:</p><p>  Own</p><p>  Make</p><p>  Get</p><p>  Put</p><p>  Dump</p>");
    
    JLabel eventInfo = new JLabel();
    eventInfo.setFont(EtFonts.displayFont);
    eventInfo.setForeground(EtColors.text);
    eventInfo.setAlignmentX(Component.CENTER_ALIGNMENT);
    text = new StringBuffer(100);
    text.append("<html>");
    text.append(EtFonts.displayHTML);
    text.append("<p></p><p>");text.append(aData.getEventsOwned());
    text.append("</p><p>");text.append(aData.getEventsMake());
    text.append("</p><p>");text.append(aData.getEventsGet());
    text.append("</p><p>");text.append(aData.getEventsPut());
    text.append("</p><p>");text.append(aData.getEventsDump());
    text.append("</p></font");
    eventInfo.setText(text.toString());
    eventPanel.add(eventType);
    eventPanel.add(eventInfo);
     
    attPanel.add(eventPanel);
    return attPanel;
  }
  
  
}
