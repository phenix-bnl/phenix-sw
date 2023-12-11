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
 *      Java class for making a GUI to view ET system information
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
import javax.swing.event.*;


public class EtViewer {
  
  static {
    // try loading lib, otherwise use existing constants
    try {
      // load library for native methods
      System.loadLibrary("EtNative");
      // use to find ET parameters (#defines)
      EtNative.findConstants();
    }
    catch (Exception x) {
      System.out.println("Error loading EtNative.so library, make do without");
    }
    catch (Error x) {
      System.out.println("Error loading EtNative.so library, make do without");
    }
  }
 
  // important widgets' names
  private JComboBox cb_baddress, cb_maddress;
  private JComboBox cb_etname, cb_hostname, cb_cast;
  private WholeNumberField ttl, udpPort, tcpPort, period;
  private JCheckBoxMenuItem systemItem, idleStationsItem, configurationItem;
  private JCheckBoxMenuItem attachmentsItem, rateItem;
  private JCheckBoxMenuItem eventsItem, layoutItem;
  private JFrame dataFrame, mainFrame;
   
  // important widgets' names to which access is needed
  public  JPanel      ddwPanel;
  public  JMenu       disconnectMenu;
  public  JTabbedPane tabbedPane = new JTabbedPane();
  
  // other variables
  private File configurationFile = null;
  private boolean broadcastActivate[];
  
  // keep track of connections to ET systems
  private static Map connections = Collections.synchronizedMap(new HashMap(100));


  public EtViewer () {
    // final members need to be initialized in all constructors
    ddwPanel          = new JPanel();
    dataFrame         = new JFrame("ET System Data Viewing");    
    mainFrame         = new JFrame("ET System Monitoring");    
    disconnectMenu    = new JMenu("Disconnect");
    
    // data display options
    systemItem        = new JCheckBoxMenuItem("System Info", true);
    idleStationsItem  = new JCheckBoxMenuItem("Idle Stations", true);
    configurationItem = new JCheckBoxMenuItem("Configuration", true);
    attachmentsItem   = new JCheckBoxMenuItem("Attachments", true);
    rateItem          = new JCheckBoxMenuItem("Rate", true);
    eventsItem        = new JCheckBoxMenuItem("Events", true);
    layoutItem        = new JCheckBoxMenuItem("In Tabbed Pane", true);
    
    tabbedPane.setBackground(EtColors.background);
    systemItem.setBackground(EtColors.background);
    idleStationsItem.setBackground(EtColors.background);
    configurationItem.setBackground(EtColors.background);
    attachmentsItem.setBackground(EtColors.background);
    rateItem.setBackground(EtColors.background);
    eventsItem.setBackground(EtColors.background);
    layoutItem.setBackground(EtColors.background);
  }
  
  // value of checkboxes telling what data to show
  public boolean showSystem()        {return systemItem.getState();}
  public boolean showIdleStations()  {return idleStationsItem.getState();}
  public boolean showConfiguration() {return configurationItem.getState();}
  public boolean showAttachments()   {return attachmentsItem.getState();}
  public boolean showRate()          {return rateItem.getState();}
  public boolean showEvents()        {return eventsItem.getState();}
  public boolean inTabbedPane()      {return layoutItem.getState();}
  public int     getUpdatePeriod()   {return period.getValue();}
  
  // allow setting of above parameters
  public void showSystem(boolean state)        {systemItem.setState(state);}
  public void showIdleStations(boolean state)  {idleStationsItem.setState(state);}
  public void showConfiguration(boolean state) {configurationItem.setState(state);}
  public void showAttachments(boolean state)   {attachmentsItem.setState(state);}
  public void showRate(boolean state)          {rateItem.setState(state);}
  public void showEvents(boolean state)        {eventsItem.setState(state);}
  public void putInTabbedPane(boolean state)   {layoutItem.setState(state);}
  public void setUpdatePeriod(int seconds)     {period.setValue(seconds);}
  
  // routines to get & set window sizes and positions
  public Rectangle getMainWindowBounds()       {return mainFrame.getBounds();}
  public Rectangle getDataWindowBounds()       {return dataFrame.getBounds();}
  public void setMainWindowBounds(Rectangle r) {mainFrame.setBounds(r);mainFrame.validate();}
  public void setDataWindowBounds(Rectangle r) {dataFrame.setBounds(r);dataFrame.validate();}
  
  
  // main
  public static void main(String args[]) throws IOException, UnknownHostException {
    // check command line arguments
    if (args.length > 2) {
	JOptionPane.showMessageDialog(new JFrame(),
		"Command line arguments are: -config <configFileName>",
		"Error",
		JOptionPane.ERROR_MESSAGE);
      System.exit(0);
    }
    else if (args.length == 1) {
      if (args[0].equalsIgnoreCase("-h")     ||
          args[0].equalsIgnoreCase("--h")    ||
          args[0].equalsIgnoreCase("-help")  ||
          args[0].equalsIgnoreCase("--help"))  {
        
	JOptionPane.showMessageDialog(new JFrame(),
		"Command line arguments are: -config <configFileName>",
		"Error",
		JOptionPane.ERROR_MESSAGE);
      }
      System.exit(0);
    }
    else if (args.length == 2) {
      if (args[0].equals("-config") == false) {
        JOptionPane.showMessageDialog(new JFrame(),
		"Command line arguments are: -config <configFileName>",
		"Error",
		JOptionPane.ERROR_MESSAGE);
        System.exit(0);
      }
      File file = new File(args[1]);
      if (file.isFile() == false) {
        JOptionPane.showMessageDialog(new JFrame(),
		"\"" + args[1] + "\" is not a file",
		"Error",
		JOptionPane.ERROR_MESSAGE);
         System.exit(0);
      }
    }
    
    // get on with the main program
    final EtViewer viewer = new EtViewer();
    viewer.makeDataDisplayWindow();
    viewer.makeMainWindow(viewer);
    
    // load command-line-specified configuration file
    if (args.length == 2) {
      try {
	EtConfiguration.load(new File(args[1]), viewer);
      }
      catch (IOException ex) {
	JOptionPane.showMessageDialog(new JFrame(),
	      "Cannot load configuration file \"" + args[1] + "\"",
	      "Error",
	      JOptionPane.ERROR_MESSAGE);
      }
    }
  }
  
  
  private void makeDataDisplayWindow() {
    
    dataFrame.setSize(1000,600);
    WindowListener wl = new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
	// iconify this window, don't exit
	dataFrame.setState(Frame.ICONIFIED);
      }
      
      public void windowDeiconified(WindowEvent e) {
        dataFrame.setVisible(true);
      }
    };
    dataFrame.addWindowListener(wl);
    
    ddwPanel.setBackground(EtColors.background);
    ddwPanel.setLayout(new BoxLayout(ddwPanel, BoxLayout.Y_AXIS));
    ddwPanel.setBorder(new EmptyBorder(10,10,10,10));
    ddwPanel.add(tabbedPane);    
      
    // make a some menus
    JMenu     viewMenu, periodMenu;
    JMenuBar  menuBar;
    JMenuItem menuItem;
    
    menuBar = new JMenuBar();
    menuBar.setFont(EtFonts.titleFont);
    menuBar.setBackground(EtColors.background);
    dataFrame.setJMenuBar(menuBar);
    
    // view menu
    viewMenu = new JMenu("View");
    viewMenu.setBackground(EtColors.background);
    menuBar.add(viewMenu);
	    
    // show submenu
    viewMenu.add(systemItem);
    viewMenu.addSeparator();
    
    // menu to tell what data to view & how to view it
    JLabel stations = new JLabel("Stations", JLabel.CENTER);
    stations.setAlignmentX(Component.CENTER_ALIGNMENT);
    stations.setOpaque(true);
    stations.setForeground(EtColors.text);
    stations.setBackground(EtColors.lightBackground);
    stations.setMaximumSize(new Dimension(200, 20));
    viewMenu.add(stations);
    viewMenu.addSeparator();
    viewMenu.add(idleStationsItem);
    viewMenu.add(configurationItem);
    viewMenu.add(attachmentsItem);
    viewMenu.add(rateItem);
    viewMenu.add(eventsItem);
    viewMenu.addSeparator();
    
    JLabel lay = new JLabel("Layout", JLabel.CENTER);
    lay.setAlignmentX(Component.CENTER_ALIGNMENT);
    lay.setOpaque(true);
    lay.setForeground(EtColors.text);
    lay.setBackground(EtColors.lightBackground);
    lay.setMaximumSize(new Dimension(200, 20));
    viewMenu.add(lay);
    viewMenu.addSeparator();
    
    layoutItem.addItemListener(
      new ItemListener() {
	public void itemStateChanged(ItemEvent e) {
	  JCheckBoxMenuItem cb = (JCheckBoxMenuItem)e.getSource();
	  // if changing to a tabbedpane view ...
	  if (cb.isSelected() == true) {
	    // get list of all widgets in panel
	    Component[] comps = ddwPanel.getComponents();
	    // remove them from panel
	    ddwPanel.removeAll();
	    // add widgets to tabbedpane
	    for (int i=0; i<comps.length ; i++) {
	      if (comps[i].getName() != null) {
		tabbedPane.add(comps[i].getName(), comps[i]);
	      }
	    }
	    tabbedPane.updateUI();
	    ddwPanel.add(tabbedPane);
	  }
	  // else changing to everything crammed together in 1 panel ...
	  else {
	    // get list of all widgets in tabbedpane
	    Component[] comps = new Component[tabbedPane.getTabCount()];
	    for (int i=0; i<tabbedPane.getTabCount() ; i++) {
		comps[i] = tabbedPane.getComponentAt(i);
	    }
	    // remove everything from tabbedpane
	    tabbedPane.removeAll();
	    // remove tabbedpane from panel
	    ddwPanel.remove(tabbedPane);
	    // add widgets to panel
	    for (int i=0; i<comps.length ; i++) {
	      comps[i].setVisible(true);
	      ddwPanel.add(comps[i]);
	    }
	  }
	  ddwPanel.updateUI();
	}
      }
    );
    viewMenu.add(layoutItem);
    
    // update period menu
    periodMenu = new JMenu("Updates");
    periodMenu.setBackground(EtColors.background);
    menuBar.add(periodMenu);
    JPanel panel = new JPanel();
    panel.setBackground(EtColors.background);
    JLabel time  = new JLabel("Period in seconds: ");
    time.setOpaque(true);
    time.setBackground(EtColors.background);
    time.setForeground(EtColors.text);
    // default to 5 seconds with 5 spaces, min of 2, and max of 300
    period = new WholeNumberField(5, 5, 2, 300);
    period.setForeground(EtColors.text);
    period.setFont(EtFonts.inputFont);
    // when hitting "enter", make sure its a valid value
    period.addActionListener( new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          WholeNumberField source = (WholeNumberField)e.getSource();
	  source.correctValue();
	}
      }
    );
    // when moving focus out of component, make sure its a valid value
    period.addFocusListener( new FocusAdapter() {
        public void focusLost(FocusEvent e) {
          WholeNumberField source = (WholeNumberField)e.getSource();
	  source.correctValue();
	}
      }
    );
    // when moving mouse out of component, make sure its a valid value
    period.addMouseListener( new MouseAdapter() {
        public void mouseExited(MouseEvent e) {
          WholeNumberField source = (WholeNumberField)e.getSource();
	  source.correctValue();
	}
      }
    );
    panel.add(time);
    panel.add(period);
    periodMenu.add(panel);
    

    Container pane = dataFrame.getContentPane();
    pane.setLayout(new BoxLayout(pane, BoxLayout.Y_AXIS));
    pane.add(ddwPanel);
    dataFrame.setVisible(true);
    return;
  }
  
  
    
  private void makeMainWindow(final EtViewer viewer) throws IOException, UnknownHostException {
            
    // Try finding local subnets. If no native methods, make do without
    String subnets[];
    try {
      subnets = EtNative.findSubnets();
      //subnets = new String[] {"129.57.35.255", "133.201.3.255", "103.1.2.31"};
    }
    catch (Exception x) {
      subnets = new String[] {""};
      System.out.println("Cannot find local subnets");
    }
    catch (Error x) {
      subnets = new String[] {""};
      System.out.println("Cannot find local subnets");
    }

    // main frame	 
    mainFrame.setSize(500,600);
    WindowListener wl = new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
        System.exit(0);
      }
    };
    mainFrame.addWindowListener(wl);
    
    // set look & feel
    String metalLAF   = "javax.swing.plaf.metal.MetalLookAndFeel";
    String motifLAF   = "com.sun.java.swing.plaf.motif.MotifLookAndFeel";
    String windowsLAF = "com.sun.java.swing.plaf.windows.WindowsLookAndFeel";
    try {
      UIManager.setLookAndFeel(metalLAF);
      SwingUtilities.updateComponentTreeUI(mainFrame);
    }
    catch (Exception ex) {
      System.out.println("Whoops, trouble setting Look & Feel");
    }
    
    // put main window into one main panel
    JPanel mainpanel = new JPanel();
    mainpanel.setBackground(EtColors.background);
    mainpanel.setLayout(new BoxLayout(mainpanel, BoxLayout.Y_AXIS));
    mainpanel.setBorder(new EmptyBorder(10,10,10,10));
    
    // setting ET name
    TitledBorder border1 = new TitledBorder(new EmptyBorder(0,0,0,0),
					    "ET Name",
					    TitledBorder.LEFT,
					    TitledBorder.ABOVE_TOP,
					    EtFonts.titleFont,
					    EtColors.title);
    
    JPanel p1 = new JPanel();
    p1.setLayout(new BoxLayout(p1, BoxLayout.X_AXIS));
    p1.setBorder(border1);
    p1.setBackground(EtColors.background);
    p1.setPreferredSize(new Dimension(500, 50));
    p1.setMaximumSize(new Dimension(1000, 100));
    p1.setAlignmentX(Component.LEFT_ALIGNMENT);
    
    cb_etname = new JComboBox(new String[] {""});
    cb_etname.setEditable(true);
    cb_etname.setFont(EtFonts.inputFont);
    cb_etname.setAlignmentX(Component.CENTER_ALIGNMENT);
    cb_etname.addActionListener(
      new ActionListener() {
        public void actionPerformed(ActionEvent e) {
	  JComboBox jcb = (JComboBox)e.getSource();
	  String listitem;
	  String selecteditem = (String)jcb.getSelectedItem();
	  int numitems = jcb.getItemCount();
	  boolean addnewitem = true;
	  for (int i=0; i < numitems; i++) {
	    listitem = (String) jcb.getItemAt(i);
	    if (listitem.equals(selecteditem) == true) {
	      addnewitem = false;
	      break;
            }
	  }
	  if (addnewitem) {
	    jcb.addItem(selecteditem);
	  }
	}
      }
    );
    p1.add(Box.createRigidArea(new Dimension(10,0)));
    p1.add(cb_etname);
    p1.add(Box.createRigidArea(new Dimension(10,0)));
   
    
    // setting ET location
    TitledBorder border2 = new TitledBorder(new EmptyBorder(0,0,0,0),
					    "ET Location",
					    TitledBorder.LEFT,
					    TitledBorder.ABOVE_TOP,
					    EtFonts.titleFont,
					    EtColors.title);
    
    JPanel p2 = new JPanel();
    p2.setLayout(new BoxLayout(p2, BoxLayout.X_AXIS));
    p2.setBorder(border2);
    p2.setBackground(EtColors.background);
    p2.setPreferredSize(new Dimension(500, 50));
    p2.setMaximumSize(new Dimension(1000, 100));
    p2.setAlignmentX(Component.LEFT_ALIGNMENT);
        
    cb_hostname = new JComboBox(new String[] {
    					"local",
					"remote",
					"anywhere",
					InetAddress.getLocalHost().getHostName()
					});
    cb_hostname.setEditable(true);
    cb_hostname.setFont(EtFonts.inputFont);
    cb_hostname.addActionListener(
      new ActionListener() {
        public void actionPerformed(ActionEvent e) {
	    JComboBox jcb = (JComboBox)e.getSource();
	    String listitem;
	    String selecteditem = (String)jcb.getSelectedItem();
	    int numitems = jcb.getItemCount();
	    boolean addnewitem = true;
	    for (int i=0; i < numitems; i++) {
	      listitem = (String) jcb.getItemAt(i);
	      if (listitem.equals(selecteditem) == true) {
	        addnewitem = false;
		break;
              }
	    }
	    if (addnewitem) {
	      jcb.addItem(selecteditem);
	    }
	}
      }
    );
    p2.add(Box.createRigidArea(new Dimension(10,0)));
    p2.add(cb_hostname);
    p2.add(Box.createRigidArea(new Dimension(10,0)));
    

    // panel for ports, ttl, & addresses
    JPanel p3 = new JPanel();
    p3.setLayout(new GridLayout(7,2,5,5));
    p3.setBackground(EtColors.background);
    p3.setPreferredSize(new Dimension(500, 300));
    p3.setMaximumSize(new Dimension(600, 400));
    p3.setBorder(new EmptyBorder(10,10,10,10));
    p3.setAlignmentX(Component.LEFT_ALIGNMENT);
        
    // label for broadcast
    JLabel l4 = new JLabel("Subnet Addresses", JLabel.CENTER);
    l4.setFont(EtFonts.titleFont);
    l4.setForeground(EtColors.text);
    l4.setOpaque(true);
    l4.setBackground(EtColors.background);
    l4.setAlignmentX(Component.LEFT_ALIGNMENT);
    
    // all subnets are to be used by default
    broadcastActivate = new boolean[subnets.length];
    for (int i=0; i<subnets.length; i++) {
      broadcastActivate[i] = true;
    }
    
    // panel for radiobuttons
    JPanel p7 = new JPanel();
    p7.setLayout(new BoxLayout(p7, BoxLayout.X_AXIS));
    p7.setBackground(EtColors.background);
    p7.setBorder(new CompoundBorder(new EtchedBorder(), new EmptyBorder(0,5,0,5)));
    p7.setMaximumSize(new Dimension(250, 50));
    p7.setAlignmentX(Component.LEFT_ALIGNMENT);
    
    // radiobutton for broadcast address activation
    final JRadioButton activate = new JRadioButton("Activated");
    activate.setActionCommand("on");
    activate.setSelected(true);
    activate.setAlignmentX(Component.LEFT_ALIGNMENT);
    activate.setBackground(EtColors.background);

    final JRadioButton deactivate = new JRadioButton("Deactivated");
    deactivate.setActionCommand("off");
    deactivate.setAlignmentX(Component.LEFT_ALIGNMENT);
    deactivate.setBackground(EtColors.background);

    // Group the radio buttons.
    ButtonGroup group1 = new ButtonGroup();
    group1.add(activate);
    group1.add(deactivate);

    // comboBox for broadcast address
    cb_baddress = new JComboBox();
    for (int i=0; i < subnets.length; i++) {
      cb_baddress.addItem(subnets[i]);
    }
    cb_baddress.setEditable(false);
    cb_baddress.setFont(EtFonts.inputFont);
    cb_baddress.setBackground(EtColors.textBackground);
    cb_baddress.setAlignmentX(Component.LEFT_ALIGNMENT);
    cb_baddress.addActionListener(
      new ActionListener() {
        public void actionPerformed(ActionEvent e) {
	    JComboBox jcb = (JComboBox)e.getSource();
	    int selectedIndex = jcb.getSelectedIndex();
	    if (broadcastActivate[selectedIndex]) {
	      activate.setSelected(true);
	    }
	    else {
	      deactivate.setSelected(true);
	    }
	}
      }
    );
    
    // Register a listener for the radio buttons.
    class RadioListener1 implements ActionListener {
        public void actionPerformed(ActionEvent e) {
            if ( e.getActionCommand().equals("on") ) {
	      broadcastActivate[cb_baddress.getSelectedIndex()] = true;
	    }
	    else {
	      broadcastActivate[cb_baddress.getSelectedIndex()] = false;
	    }
        }
    } 
       
    RadioListener1 myListener1 = new RadioListener1();
    activate.addActionListener(myListener1);
    deactivate.addActionListener(myListener1);
    
    p7.add(activate);
    p7.add(deactivate);
        
    // label for multicast
    JLabel l5 = new JLabel("Multicast Addresses", JLabel.CENTER);
    l5.setFont(EtFonts.titleFont);
    l5.setForeground(EtColors.text);
    l5.setOpaque(true);
    l5.setBackground(EtColors.background);
    l5.setAlignmentX(Component.LEFT_ALIGNMENT);
    
    // comboBox for multicast address
    cb_maddress = new JComboBox();
    cb_maddress.setEditable(true);
    cb_maddress.setFont(EtFonts.inputFont);
    cb_maddress.setAlignmentX(Component.LEFT_ALIGNMENT);
    cb_maddress.addActionListener(
      new ActionListener() {
        public void actionPerformed(ActionEvent e) {
	    JComboBox jcb = (JComboBox)e.getSource();
	    String listitem;
	    String selecteditem = (String)jcb.getSelectedItem();
	    int numitems = jcb.getItemCount();
	    boolean addnewitem = true;
	    
	    if (selecteditem == null) {
	      addnewitem = false;
	    }
	    else if (numitems == 0) {
	      addnewitem = true;
	    }
	    else {
	      for (int i=0; i < numitems; i++) {
		listitem = (String) jcb.getItemAt(i);
		if (listitem.equals(selecteditem) == true) {
	          addnewitem = false;
		  break;
        	}
	      }
	    }
	    
	    if (addnewitem) {
	      jcb.addItem(selecteditem);
	    }
	}
      }
    );
    
    // button for multicast address removal
    class ButtonListener implements ActionListener {
        public void actionPerformed(ActionEvent e) {
	    int index = cb_maddress.getSelectedIndex();
	    if (index > -1) {
	        cb_maddress.removeItemAt(index);
	    }
        }
    }    
    
    final JButton remove = new JButton("Remove");
    ButtonListener buttonListener = new ButtonListener();
    remove.addActionListener(buttonListener);
    remove.setAlignmentX(Component.LEFT_ALIGNMENT);
    remove.setBackground(EtColors.background);
    remove.setMaximumSize(new Dimension(250, 50));

    // add to parent panel
    p3.add(l4);
    p3.add(l5);
    p3.add(cb_baddress);
    p3.add(cb_maddress);
    p3.add(p7);
    p3.add(remove);
    // fillers
    p3.add(new JLabel(" "));
    p3.add(new JLabel(" "));
      
    // ports & ttl labels
    JLabel l1 = new JLabel("UDP Port:", JLabel.RIGHT);
    JLabel l2 = new JLabel("TCP Port:", JLabel.RIGHT);
    JLabel l3 = new JLabel("TTL Value:", JLabel.RIGHT);
    l1.setFont(EtFonts.titleFont);
    l2.setFont(EtFonts.titleFont);
    l3.setFont(EtFonts.titleFont);
    l1.setForeground(EtColors.text);
    l2.setForeground(EtColors.text);
    l3.setForeground(EtColors.text);
        
    // text input for udp broad/multicast port number
    udpPort = new WholeNumberField(EtConstants.broadcastPort(), 8, 1024, 65535);
    udpPort.setPreferredSize(new Dimension(150, 25));
    udpPort.setMaximumSize(new Dimension(150, 50));
    udpPort.setAlignmentX(Component.LEFT_ALIGNMENT);
    udpPort.setFont(EtFonts.inputFont);
    // make sure there's a valid value entered
    udpPort.addActionListener( new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          WholeNumberField source = (WholeNumberField)e.getSource();
	  source.correctValue();
	}
      }
    );
    udpPort.addFocusListener( new FocusAdapter() {
        public void focusLost(FocusEvent e) {
          WholeNumberField source = (WholeNumberField)e.getSource();
	  source.correctValue();
	}
      }
    );
    udpPort.addMouseListener( new MouseAdapter() {
        public void mouseExited(MouseEvent e) {
          WholeNumberField source = (WholeNumberField)e.getSource();
	  source.correctValue();
	}
      }
    );
        
    // text input for tcp server port number
    tcpPort = new WholeNumberField(EtConstants.serverPort(), 8, 1024, 65535);
    tcpPort.setFont(EtFonts.inputFont);
    tcpPort.setPreferredSize(new Dimension(150, 25));
    tcpPort.setMaximumSize(new Dimension(150, 50));
    tcpPort.setAlignmentX(Component.LEFT_ALIGNMENT);
    tcpPort.addActionListener( new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          WholeNumberField source = (WholeNumberField)e.getSource();
	  source.correctValue();
	}
      }
    );
    tcpPort.addFocusListener( new FocusAdapter() {
        public void focusLost(FocusEvent e) {
          WholeNumberField source = (WholeNumberField)e.getSource();
	  source.correctValue();
	}
      }
    );
    tcpPort.addMouseListener( new MouseAdapter() {
        public void mouseExited(MouseEvent e) {
          WholeNumberField source = (WholeNumberField)e.getSource();
	  source.correctValue();
	}
      }
    );
    
    // text input for TTL value
    ttl = new WholeNumberField(2, 6, 0, 255);
    ttl.setFont(EtFonts.inputFont);
    ttl.setPreferredSize(new Dimension(150, 25));
    ttl.setMaximumSize(new Dimension(150, 50));
    ttl.setAlignmentX(Component.LEFT_ALIGNMENT);
    ttl.addActionListener( new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          WholeNumberField source = (WholeNumberField)e.getSource();
	  source.correctValue();
	}
      }
    );
    ttl.addFocusListener( new FocusAdapter() {
        public void focusLost(FocusEvent e) {
          WholeNumberField source = (WholeNumberField)e.getSource();
	  source.correctValue();
	}
      }
    );
    ttl.addMouseListener( new MouseAdapter() {
        public void mouseExited(MouseEvent e) {
          WholeNumberField source = (WholeNumberField)e.getSource();
	  source.correctValue();
	}
      }
    );
    
    // add to parent panel
    p3.add(l1);
    p3.add(udpPort);
    p3.add(l2);
    p3.add(tcpPort);
    p3.add(l3);
    p3.add(ttl);
   
    // broad/multicasting or direct connections
    JPanel p9 = new JPanel();
    p9.setLayout(new BoxLayout(p9, BoxLayout.X_AXIS));
    p9.setBackground(EtColors.background);
    p9.setPreferredSize(new Dimension(400, 50));
    p9.setMaximumSize(new Dimension(600, 80));
    p9.setAlignmentX(Component.LEFT_ALIGNMENT);
    
    // comboBox for host name
    cb_cast = new JComboBox(new String[] {
    					"broadcasting",
					"multicasting",
					"broad and multicasting",
					"direct connection",
					});
    cb_cast.setEditable(false);
    cb_cast.setFont(EtFonts.inputFont);
    cb_cast.setBackground(EtColors.textBackground);
    cb_cast.addActionListener(
      new ActionListener() {	
        public void actionPerformed(ActionEvent e) {
	    JComboBox jcb = (JComboBox)e.getSource();
	    String listitem;
	    String selecteditem = (String)jcb.getSelectedItem();
	    
	      if (selecteditem.equals("broadcasting")) {
		cb_baddress.setEnabled(true);
		activate.setEnabled(true);
		deactivate.setEnabled(true);
		udpPort.setEnabled(true);
		
		ttl.setEnabled(false);
		tcpPort.setEnabled(false);
		cb_maddress.setEnabled(false);
		remove.setEnabled(false);
	      }
	      else if (selecteditem.equals("multicasting")) {
		ttl.setEnabled(true);
		udpPort.setEnabled(true);
		cb_maddress.setEnabled(true);
		remove.setEnabled(true);
		
		cb_baddress.setEnabled(false);
		activate.setEnabled(false);
		deactivate.setEnabled(false);
		tcpPort.setEnabled(false);
	      }
	      else if (selecteditem.equals("broad and multicasting")) {
		tcpPort.setEnabled(false);
		
		udpPort.setEnabled(true);
		cb_maddress.setEnabled(true);
		cb_baddress.setEnabled(true);
		activate.setEnabled(true);
		deactivate.setEnabled(true);
		remove.setEnabled(true);
		ttl.setEnabled(true);
	      }
	      else {
		tcpPort.setEnabled(true);
		
		udpPort.setEnabled(false);
		cb_baddress.setEnabled(false);
		cb_maddress.setEnabled(false);
		activate.setEnabled(false);
		deactivate.setEnabled(false);
		remove.setEnabled(false);
		ttl.setEnabled(false);
	      }
	}
      }
    );
    
    // default is broadcasting
    cb_cast.setSelectedIndex(0);
    
    p9.add(Box.createRigidArea(new Dimension(10,0)));
    p9.add(cb_cast);        
    p9.add(Box.createRigidArea(new Dimension(10,0)));
    
    // panel to hold to "Find ET By" stuff
    TitledBorder border3 = new TitledBorder(new EtchedBorder(),
					    "Find ET by",
					    TitledBorder.LEFT,
					    TitledBorder.ABOVE_TOP,
					    EtFonts.titleFont,
					    EtColors.title);
    
    JPanel p10 = new JPanel();
    p10.setLayout(new BoxLayout(p10, BoxLayout.Y_AXIS));
    p10.setBorder(border3);
    p10.setBackground(EtColors.background);
    p10.setMaximumSize(new Dimension(600, 300));
    p10.add(p9);
    p10.add(p3);
    p10.add(Box.createRigidArea(new Dimension(0,10)));
    
    // pack the components
    mainpanel.add(Box.createRigidArea(new Dimension(0,10)));
    mainpanel.add(p1);
    mainpanel.add(Box.createRigidArea(new Dimension(0,10)));
    mainpanel.add(p2);
    mainpanel.add(Box.createRigidArea(new Dimension(0,10)));
    mainpanel.add(p10);
    
    JMenuBar menuBar = new JMenuBar();
    menuBar.setFont(EtFonts.titleFont);
    menuBar.setBackground(EtColors.background);
    mainFrame.setJMenuBar(menuBar);
    
    // file menu
    JMenu fileMenu = new JMenu("File");
    fileMenu.setBackground(EtColors.background);
    menuBar.add(fileMenu);
	 
    // Create a file chooser
    final JFileChooser fc = new JFileChooser(System.getProperty("user.dir"));
    
    // file menu item to save configuration
    JMenuItem menuItem = new JMenuItem("Save Configuration");
    menuItem.setBackground(EtColors.background);
    fileMenu.add(menuItem);
    menuItem.addActionListener(
      new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          File file;
	  if (configurationFile == null) {
            if (fc.showOpenDialog(mainFrame) == JFileChooser.CANCEL_OPTION) {
	      return;
	    }
            file = fc.getSelectedFile();
	  }
	  else {
	    file = configurationFile;
	  }
	  
	  try {
	    EtConfiguration.save(file, viewer, connections);
	  }
	  catch (IOException ex) {
	    JOptionPane.showMessageDialog(new JFrame(),
        	"Cannot write to file \"" + file.getName() + "\"",
        	"Error",
        	JOptionPane.ERROR_MESSAGE);
	    return;
	  }
	  configurationFile = file;
	  
        }
      }    
    );
    
    // file menu item to save configuration
    menuItem = new JMenuItem("Save Configuration As");
    menuItem.setBackground(EtColors.background);
    fileMenu.add(menuItem);
    menuItem.addActionListener(
      new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          int returnVal = fc.showOpenDialog(mainFrame);

          if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fc.getSelectedFile();
	    try {
	      if (file.exists()) {
	        int n = JOptionPane.showConfirmDialog(
                            new JFrame(),
                            "Overwrite existing file?",
                            "WARNING",
                            JOptionPane.YES_NO_OPTION);
		if (n == JOptionPane.NO_OPTION) return;
	      }
	      EtConfiguration.save(file, viewer, connections);
	    }
	    catch (IOException ex) {
	      JOptionPane.showMessageDialog(new JFrame(),
        	  "Cannot write to file \"" + file.getName() + "\"",
        	  "Error",
        	  JOptionPane.ERROR_MESSAGE);
	      return;
	    }
	    configurationFile = file;
          }
        }
      }    
    );
    
    // file menu item to load configuration
    menuItem = new JMenuItem("Load Configuration");
    menuItem.setBackground(EtColors.background);
    fileMenu.add(menuItem);
    menuItem.addActionListener(
      new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          int returnVal = fc.showOpenDialog(mainFrame);

          if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fc.getSelectedFile();
	    try {
	      EtConfiguration.load(file, viewer);
	    }
	    catch (IOException ex) {
	      JOptionPane.showMessageDialog(new JFrame(),
        	  "Cannot load file \"" + file.getName() + "\"",
        	  "Error",
        	  JOptionPane.ERROR_MESSAGE);
	      return;
	    }
          }
        }
      }    
    );
    
    // file menu item to quit
    menuItem = new JMenuItem("Quit");
    menuItem.setBackground(EtColors.background);
    fileMenu.add(menuItem);
    menuItem.addActionListener(
      new ActionListener() {
	public void actionPerformed(ActionEvent e) {
          System.exit(0);
	}    
      }    
    );
    
    // view menu to deiconify data display window
    JMenu viewMenu = new JMenu("View");
    viewMenu.setBackground(EtColors.background);
    menuBar.add(viewMenu);

    menuItem = new JMenuItem("Data Display Window");
    menuItem.setBackground(EtColors.background);
    viewMenu.add(menuItem);
    menuItem.addActionListener(
      new ActionListener() {
	public void actionPerformed(ActionEvent e) {
          dataFrame.setState(Frame.NORMAL);
	}
      }
    );
    
    // connect menu
    JMenu connectMenu = new JMenu("Connections");
    connectMenu.setBackground(EtColors.background);
    menuBar.add(connectMenu);
	 
    menuItem = new JMenuItem("Connect");
    menuItem.setBackground(EtColors.background);
    connectMenu.add(menuItem);
    menuItem.addActionListener(
      new ActionListener() {
	public void actionPerformed(ActionEvent e) {
	  // This callback is executed by the swing graphics
	  // thread. Since making a connection can block for
	  // several seconds (and thus render the GUI frozen),
	  // create another thread to do the work.
	  Thread t = new Thread( new Runnable() {
	    public void run() {
              makeConnection(getEtSystem(), viewer);
	    }
	  });
	  t.start();
	}    
      }
    );
    
    // menu to disconnect existing connections
    disconnectMenu.setBackground(EtColors.background);
    connectMenu.add(disconnectMenu);

    mainFrame.getContentPane().add(mainpanel);
    mainFrame.setVisible(true);
  }
  
  
  // gather data about which ET system and how to connect to it
  public EtSystem getEtSystem() {
    // get ET system and host names
    EtSystem etSys;
    String etsystem = (String)cb_etname.getSelectedItem();
    String hostname = (String)cb_hostname.getSelectedItem();
    if (hostname.equals("local")) {
      hostname = EtConstants.hostLocal();
    } else if (hostname.equals("remote")) {
      hostname = EtConstants.hostRemote();
    } else if (hostname.equals("anywhere")) {
      hostname = EtConstants.hostAnywhere();
    }
    // find out how we're connecting with the ET system
    String how2connect = (String)cb_cast.getSelectedItem();
    if (how2connect.equals("broadcasting")) {
      // find activated subnet broadcast addresses
      int count = 0, nActiveAddrs = 0;
      for (int i=0; i < broadcastActivate.length; i++) {
        if (broadcastActivate[i]) nActiveAddrs++;
      }
      if (nActiveAddrs < 1) {
        return null;
      }
      String[] addresses = new String[nActiveAddrs];
      for (int i=0; i < broadcastActivate.length; i++) {
        if (broadcastActivate[i]) {
	  addresses[count++] = (String) cb_baddress.getItemAt(i);
	}
      }
      // get port #
      int port = udpPort.getValue();
      
      etSys = new EtSystem(etsystem, hostname, addresses, port);
    }
    else if (how2connect.equals("multicasting")) {
      // find multicast addresses
      int count = 0, nAddrs = cb_maddress.getItemCount();
      if (nAddrs < 1) {
        return null;
      }
      String[] addresses = new String[nAddrs];
      for (int i=0; i < nAddrs; i++) {
	addresses[count++] = (String) cb_maddress.getItemAt(i);
      }
      // get port & ttl #s
      int port   = udpPort.getValue();
      int ttlval = ttl.getValue();
      
      etSys = new EtSystem(etsystem, hostname, addresses, port, ttlval);
    }
    else if (how2connect.equals("broad and multicasting")) {
      int count = 0;
      String[] bAddresses = null, mAddresses = null;
      
      // find activated subnet broadcast addresses
      int nActiveAddrs = 0;
      for (int i=0; i < broadcastActivate.length; i++) {
        if (broadcastActivate[i]) nActiveAddrs++;
      }
      if (nActiveAddrs > 0) {
	bAddresses = new String[nActiveAddrs];
	for (int i=0; i < broadcastActivate.length; i++) {
          if (broadcastActivate[i]) {
	    bAddresses[count++] = (String) cb_baddress.getItemAt(i);
	  }
	}
      }
      
      // find multicast addresses
      count = 0;
      int nAddrs = cb_maddress.getItemCount();
      if (nAddrs > 0) {
	mAddresses = new String[nAddrs];
	for (int i=0; i < nAddrs; i++) {
	  mAddresses[count++] = (String) cb_maddress.getItemAt(i);
	}
      }
      if ((bAddresses == null) && (mAddresses == null)) {
        return null;
      }
      
      // get port & ttl #s
      int uPort = udpPort.getValue();
      int tPort = tcpPort.getValue();
      int ttlval  = ttl.getValue();
      
      etSys = new EtSystem(etsystem, hostname, bAddresses, mAddresses,
			   EtConstants.broadAndMulticast(),
			   tPort, uPort, ttlval);
    }
    else if (how2connect.equals("direct connection")) {
      int port = tcpPort.getValue();
      etSys = new EtSystem(etsystem, hostname, port);
    }
    else {
      etSys = null;
    }
    
    return etSys;    
  }
  
  
  // make a connection to an ET system & record it
  public void makeConnection(EtSystem etSys, EtViewer viewer) {
    if (etSys == null) {
      JOptionPane.showMessageDialog(new JFrame(),
          "Bad specification for ET system",
          "Error",
          JOptionPane.ERROR_MESSAGE);
      return;
    }
    // make a connection
    EtConnection myConnection = new EtConnection(etSys);
    try {
      myConnection.connect();
    }
    catch (EtException e) {
      JOptionPane.showMessageDialog(new JFrame(),
          "Cannot find or connect to " + myConnection.getEtName(),
          "Error",
          JOptionPane.ERROR_MESSAGE);
      myConnection = null;
      return;
    }
    catch (UnknownHostException e) {
      JOptionPane.showMessageDialog(new JFrame(),
          myConnection.getHost() + " is an unknown host",
          "Error",
          JOptionPane.ERROR_MESSAGE);
      myConnection = null;
      return;
    }
    catch (IOException e) {
      JOptionPane.showMessageDialog(new JFrame(),
          "Communication problems with "  +
		myConnection.getEtName() + " on " +
		myConnection.getHost(),
          "Error",
          JOptionPane.ERROR_MESSAGE);
      myConnection = null;
      return;
    }
    
    // track connections since you only want to connect once to each system
    // create unique name & enter into Map (HashMap entry)
    if (connections.containsKey(myConnection.getEtName()+myConnection.getHost())) {
      myConnection.disconnect();
      // pop up Dialog box
      JOptionPane.showMessageDialog(new JFrame(),
          "You are already connected to "  +
		myConnection.getEtName() + " on " +
		myConnection.getHost(),
          "ERROR",
          JOptionPane.ERROR_MESSAGE);
      return;
    }    
    
    connections.put(myConnection.getEtName()+myConnection.getHost(), myConnection);
    // start thread to handle this new connection
    EtConnectionThread thread = 
		  new EtConnectionThread(etSys, connections, myConnection, viewer);
    thread.start();
  }    
  
}
