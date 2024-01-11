//--------------------------------------------------- 
// Class: svxAddress
//--------------------------------------------------- 

#ifndef __SVXADDRESS_HH__
#define __SVXADDRESS_HH__

#include <phool.h>
#include <PHObject.h>
#include <PHTimeStamp.h>
#include "SvxParameters.h"
#include <map>
#include <vector>
#include <algorithm>
#include <numeric>
#include <iostream>

/**
 * @brief  svxAdrress class is used to map hardware and software indices
 *
 * @date  Created by Sasha Lebedev <lebedev@iastate.edu>
 *        make it to Singleton  2011.1.18 T.Hachiya
 */
class svxAddress : virtual public PHObject {


public:
  svxAddress();                           // constructor

//  svxAddress(short maptype);                // constructor
  virtual ~svxAddress();                  // destructor

  ////////////////
  // for PHObject
  virtual void Reset(){ std::cout<<"void svxAddress::Reset()"<<std::endl;}; /*Do nothing*/ 
  virtual void identify(std::ostream& os = std::cout) const {
    os<<"This is svxAddress"<<std::endl;
  }
  /// isValid returns non zero if object contains vailid data
  virtual int isValid() const { return 1;}


  ////////////////

  enum SensorType {
    eTypePixel = 1,
    eTypeStrip = 11 
  };

  enum SouthNorth {
    SOUTH = 0,
    NORTH = 1 
  };
  
  void Initialize();
  bool isInitialized() { return m_initialized; }

  PHBoolean Update(PHTimeStamp *Tbeg, PHTimeStamp *Tend);
  PHBoolean Fetch(PHTimeStamp *T);


  PHBoolean commitPixelMap(const char* description, PHTimeStamp* timeBegin, PHTimeStamp* timeEnd);     // Added by T.Hachiya 2011.04.29
  

  void setFetchTime(PHTimeStamp *time) { m_tFetchDB = time; } // this is used in Initialize();

  PHBoolean fetchPixelMap(PHTimeStamp *time);     // Added by T.Hachiya 2011.04.29
  PHBoolean fetchStripixelMap(PHTimeStamp *time); // Added by T.Hachiya 2011.04.29
  void UseDefaults();
  void UseDefaultsPixels();
  void UseDefaultsStripixels();
  void UsePixelHardware();

  void showPixelPacketID();

  // Data member access methods
  void set_Verbose(short a) {Verbose=a;}       
  short get_Verbose() {return Verbose;}


  // Fetch all geometry information from an ASCII file
  PHBoolean FetchFromFile();
  PHBoolean FetchFromFile(std::string filename);

  // Write out hardware/software map to ASCII file
  PHBoolean DumpIntoFile();  
  PHBoolean DumpIntoFile(std::string filename);

  int getChannel0(int sectype, int ix, int iz);
  int getChannel1(int sectype, int ix, int iz);
  int getChannel0(int sectype, int section, int ix, int iz); // added by TH 2011.02.24
  int getChannel1(int sectype, int section, int ix, int iz); // added by TH 2011.02.24
  int getIX0(int sectype, int channel);
  int getIZ0(int sectype, int channel);
  int getSensorIX0(int channel);
  int getSensorIZ0(int channel);
  int getSensorIX0(int lr, int ld, int sn, int channel, int roc, int module);
  int getSensorIZ0(int lr, int ld, int sn, int channel, int roc, int module);
  int getChannelSensor0(int ix, int iz);
  int getChannelSensor0(int lr, int ld, int sn, int ix, int iz, int readout);
  int getROCSensor0(int lr, int ld, int sn, int ix, int iz);
  int getModuleSensor0(int lr, int ld, int sn, int ix, int iz);


  // added by TH 2011.1.21
  int getPixelRocIX0(int rocchan); // ix -> 0-255, exception -1 get ROC_local_ix
  int getPixelRocIZ0(int rocchan); // iz -> 0-31 , exception -1 get ROC_local_iz
  int getPixelSensorIX0(int roc, int rocchan); // ix=0-255, exception -1 get Sensor_local_ix
  int getPixelSensorIZ0(int roc, int rocchan); // iz=0-127, exception -1 get Sensor_local_iz
  int getPixelSensorSection(int iz); // iz=0-127, exception -1
  int getPixelSensorSection(int roc, int rocchan); // iz=0-127, exception -1
  int getPixelChannel(int ix, int iz);

  int getStripSensorSection(int roc); //roc:0-11,exception -1 get SensorSection(L:0,R=1)
  int getStripSensorReadout(int roc); //roc:0-11,exception -1 get SensorReadout(X:0,U=1)
  int getStripSensorHybrid(int roc); //roc:0-11,exception -1 get SensorReadout(hybrid=0,1,2,3 => chip((0,1,2), (3,4,5), (6,7,8), (9,10,11)))
  int getStripSensorChannel(int roc, int rocchan); //roc:0-11,rocchan=0-127, exception -1 get softwarechannel(0-383)
  int getStripChannel(int ix, int iz, int readout);

  // reverse conversion

  // return ROC number on pixel sensor from ix and iz
  int getPixelRocChannel(int ix, int iz); // ix:0-255, iz=0-31, exception -1

  // return channel number on a pixel sensor (covering 0 - 32757) from ix and iz
  int getPixelRocChannelSensor(int ix, int iz); // ix:0-255, iz=0-127, exception -1

  // return ROC number on pixel sensor from sensor number and iz
  int getPixelRocSensor(int sensor, int iz); // sensor:0-3, iz=0-127, exception -1

  // Return tile number (0-15) on ROC in layer 0 or 1. 
  // Independent of ROC, sensor, or ladder index. 
  int getPixelTile(int ix, int iz); // ix: 0-255, iz: 0-31, exception -1.

  int getStripRocChannel(int section, int readout, int channel); //readout:0-1,channel=0-383, exception -1 get rocchan(0-127)
  int getStripRoc(int section, int readout, int channel); //section:0-1, readout:0-1,channel=0-383, exception -1 get roc(0-11)

  bool isSensorType(int sensorType);
  int  getSensorSection(int sensor, int roc, int rocchan); // exception -1;
  int  getSensorReadout(int sensor, int roc);
  int  getLayer(int sensor, int module);
  int  getLadder(int sensor, int module);

private:
  bool isPixelRoc(int roc)            { return (0<=roc&&roc<8); }
  bool isPixelRocChannel(int rocchan) { return (0<=rocchan&&rocchan<8192); } 
  bool isStripRoc(int roc)            { return (0<=roc&&roc<12); }
  bool isStripRocChannel(int rocchan) { return (0<=rocchan&&rocchan<128); }

  bool isStripSensorSection(int section){ return (0<=section&&section<2); }
  bool isStripSensorReadout(int readout){ return (0<=readout&&readout<2); }
  bool isStripSensorChannel(int channel){ return (0<=channel&&channel<384); }

public:

  short get_maptype() {return maptype;}
  void  set_maptype(short a) {maptype=a;}
  void  set_usedatabase(short a) {m_usedatabase=a;}

  // ----------------------
  // to read PRDF 
  int getPixelPacketID(const int module);   // added by TH. 2011.01.18
  int getPixelModuleID(const int packetid); //  convertion matrix in these 2 functions should be read from DB
                                            //  now temporally DB is not used.
  int getStripPacketID(const int module);   //
  int getStripModuleID(const int packetid);   //

  // added by T.Hachiya 2011.11.15
  // ModuleID operation
  static bool isPixelModuleID(const int module);
  int  getPixelSensor(int module, int roc); // exception -1;
  int  getPixelLayer(const int module);     // exception -1;
  int  getPixelLadder(const int module);    // exception -1;
  int  getPixelSide(const int module);      // exception -1;
  int  getPixelModuleID(int layer, int ladder, int sensor); // layer=0/1, ladder=0-4/L0, 0-9/L1, sensor=0-3
  //added by H. Sako
  static int  getPixelModuleIDSouthNorth(const int layer, const int ladder, const int south_north); // layer=0/1, ladder=0-9/L0, 0-19/L1, south=0/north=1
  static int  getPixelSouthNorth(const int module); // module=0-59, south=0/north=1

  static bool isStripModuleID(const int module);
  int  getStripLayer(const int module);     // exception -1
  int  getStripLadder(const int module);    // exception -1
  int  getStripModuleID(int layer, int ladder); // layer=2/3, ladder=0-15/L2, 0-23/L3

  // unitTest is used to confirm if the function works properly
  int unitTest();

  void set_useoldmap(bool t) { if(use_oldmap!=t) {std::cout<<"oldmap is updated. Need to initialize again"<<std::endl;}; 
                               use_oldmap = t; };

private:
  bool unitTestInt(int val, int exp, const char *valstr="", const char *expstr="", const char *headstr="");
  bool unitTestBool(bool val, bool exp, const char *valstr="", const char *expstr="", const char *headstr="");

  bool check_getSensorIX0();
  bool check_getSensorIZ0();
  bool check_getROCSensor0();
  bool check_getModuleSensor0();
  bool check_getChannelSensor0();
  bool check_getPixelChannel();
  bool check_getStripChannel();
  bool check_getPixelModuleID();
  bool check_getPixelLayer();
  bool check_getPixelLadder();
  bool check_getPixelSide();
  bool check_isPixelModuleID();
  bool check_getPixelPacketID();
  bool check_getPixelSensor();
  bool check_getPixelSensorSection();

  bool check_getPixelRocIX0();
  bool check_getPixelRocIZ0();
  bool check_getPixelSensorIX0();
  bool check_getPixelSensorIZ0();

  bool check_getPixelRocChannelSensor();
  bool check_getPixelRocSensor(); 

  bool check_getStripSensorSection();
  bool check_getStripSensorReadout(); 
  bool check_getStripSensorChannel(); 

  bool check_getStripLayer();
  bool check_getStripLadder();
  bool check_getStripPacketID();

  bool check_getStripRocChannel();
  bool check_getStripRoc();

  // ----------------------


private:

  // Verbosity level for this class
  short Verbose;
  short maptype;
  short m_usedatabase; // variable name is slightly changed
  int   m_initialized; // added by T.Hachiya 2011.1.18
  bool use_oldmap;

  PHTimeStamp* m_tFetchDB;
  PHTimeStamp Tsearch;

  std::vector<int> m_aryPixelPacketID; // index=moduleID, content=packetID // map should be changed after run 340200
  std::vector<int> m_aryPixelModule;   // index=packetID, content=moduleID
  /// old map (before run 340200)
  //std::vector<int> m_aryPixelPacketID_old; // index=moduleID, content=packetID
  //std::vector<int> m_aryPixelModule_old;   // index=packetID, content=moduleID

  std::vector<int> m_aryStripPacketID; // index=moduleID, content=packetID
  std::vector<int> m_aryStripModule;   // index=packetID, content=moduleID

  // Hardware/software maps
  std::map<int,int> _svxaddress_sensor1[SVXLAYERNUMBER/2][SVXLADDERSLAYER1*2][SVXSENSORSLAYER1]; // layers, ladders, sensors  
  std::map<int,int> _svxaddress_sensec1[SVXLAYERNUMBER/2][SVXLADDERSLAYER1*2][SVXSENSORSLAYER1][SVXSENSECPIX]; // layers, ladders, sensors, sections
  std::map<int,int> _svxaddress_sensor11[SVXLAYERNUMBER/2][SVXLADDERSLAYER3*2][SVXSENSORSLAYER3]; // layers, ladders, sensors
  std::map<int,int> _svxaddress_sensec11[SVXLAYERNUMBER/2][SVXLADDERSLAYER3*2][SVXSENSORSLAYER3][SVXSENSECSTRIPIX]; // layers, ladders, sensors, sections


  static int m_count;


};

#endif



// 2011.04.29 T.Hachiya  To get values from DB access, m_aryPixelPacketID and m_aryPixelPacketID_old are merged.


