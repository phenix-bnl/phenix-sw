#ifndef __EMCDYNAMICDATA_H__
#define __EMCDYNAMICDATA_H__


// EMCDynamicData.h
// ordered by Edouard Kistenev
// created by Sergei Belikov
// last modifications: 04/20/99
// last revision - 06/11/99 - raw data access was added
//##########################################################################

#ifndef __EMCCONFIGURATIONFILE_H__
#include "emcConfigurationFile.h"
#endif

//_____________________________________________________________________________

/// Describes each FEM settings.		 

struct SuperModule
{
  /** absolute position in the detector.
      Absolute position = (position inside sector: 0-17 for PbSc and 0-31 for PbGl) + 
      (Number of FEM in all previous sectors).
      Range: 0-155 (3x6x6 PbSc + 4x8x2 PbGl) for FEMs attached to SM144 and >155 for reference FEMs if any.
  */
  /// Location of this FEM on the detector
  int absPosition;       
  /// PIN of the attached FEM. Each FEM should be marked by this number.
  int femPin;            
  /// packet number for EventIterator.
  int packet;             
  /// number of towers connected to this FEM.
  int nch;               
  /// beginning of this FEM towers data in data array.
  int startTad;          
  /// list of FEM channels	that should be read out. 
  int* femCh;            
  /// number of references in current SM144.
  int nref;              
  /// addresses of SM references in Reference[] array.
  int* adRef;             
  /// number of references connected to FEM of this SM144.
  int nrefCh;           
  /// list of these references channels connected to FEM that should be read out.
  int* refCh;            
  /// starting address of FEM connected references in Reference[] array.
  int startRad;			   
  /// delay (in clocks) between TAC and Pre sample.
  int tac_pre;           
  /// delay (in clocks) between Post and Pre sample.
  int post_pre;           
};

//_____________________________________________________________________________

/** This structure contains information of one EMCal reference. */

struct Reference
{
  /// Absolute position of the FEM used to measure the signal from this reference ( same as SupermoduleNumber 0-180).
  int absFEM;            
  /// Absolute position in RawData where this reference is stored
  int chan;              
  /// Type of reference
  int type;              
  /// Absolute position of SuperModule which is serviced by this reference (0-180).
  int absPosition;       
  /// Number of active channels in the SuperModule serviced by this reference.
  int nSMch;             
  /// location of the 1-st serviced channel in data arrays.
  int startSMad;   
};

//_____________________________________________________________________________

/** This structure keeps pointers to data arrays. */

struct EmcData
{
  /// Data arrays size.
  int size;             
  /// Array for high gain data.
  float* hg;            
  /// Array for low  gain data.
  float* lg;            
  /// Array for  timing   data.
  float* tac;           
  /// 2D array of raw data. 
  float** rawdata;           
};

/** Parses configuration file and creates storage arrays for only active channels and references data.
    ===============================================================================
    | The purpose of EmcDynamicData class is:                                     |
    | 1. to Parse script file describing EMCalchannels and references connections |
    |    to FEMs;                                                                 |
    | 2. to create storage arrays for only active channels and references data;   |
    | 3. to create routers that explain Event Iterating routines/classes what     |
    |    channels should be read out and where these data should be stored.       |
    |    Actually router creates "connections" between electronics channels and   |
    |    elements of data arrays.                                                 |
    |                                                                             |
    |  To make your own EmcDynamicData object, just do:                           |
    |_____________________________________________________________________________|
    | int status;                                                                 |
    | EmcDynamicData myobject(status,ConfigFileName);//constructor does all       |            
    | if(status) exit(1); //non-zero status means there were script errors        |
    | int* dataMap=myobject.getEmcMap(); // get list of detector cells            |
    | int nchannel=myobject.getEmcSize(); // get size of data arrays              |
    | float* myHighGain=myobject.getEmcHG();//access to EMCal HighGain pre-post   |
    |// the same for getEmcLG, getEmcTAC.                                         |
    |_____________________________________________________________________________|
    | Boolean 'false' as a 2-nd parameter to constructor means that data from FEM |
    | 6x32=192 instead of 6x24=144. It means that you will get data not from      |
    | towers only, but also from channels that are connected to different ASIC    |
    | bias voltage levels.  If you  care about tower signals only, use 'true'     |
    | instead of 'false'                                                          |
    |_____________________________________________________________________________|
    | 3-d parameter in the constructor determins if in addition to amplitudes     |
    | you would like to get array with just raw data: tac, hipost,lopost, hipre,  |
    | lopre. If you set 'true' then get pointer to 2D raw data aray[5][datasize]  |
    | using float** EmcDynamicData::getEmcRaw()                                   |
    |_____________________________________________________________________________|
    | Last parameter in the constructor determins  data mapping style. 'True"     |
    | means that DataMap array will store absolute FEM addresses:                 |
    | address=absFEM*192+ch, where absFEM - FEM# (0-171), ch -channel# (0-191).   |
    |'False' means EMCal towers mapping style: address=tower# (0-24767).          |
    |_____________________________________________________________________________| 
    | Slang in this program:                                                      |
    | abs position-means position in the EMCal detector. For towers it is 0-24767,|
    |         for FEM or reference - this is a position of the supermodule (12x12 |
    | towers) in EMCal : 0-171. EMCal is treated as 8 sectors, PbSc1, ...,PbSc6,  |
    | PgGl1,PbGl2. In this program names of corresponding sectors are: W0,W1,W2,  |
    | W3,E2,E3,E0,E1. Reason - Edouard asked to push PbGl as far as possible :)   |
    =============================================================================*/

class EmcDynamicData
{
protected:
  int line;               // current line in script file
  char* fLine;            // current string in script file
  EmcData Data;			// EMCal channels data structure
  EmcData Refs;			// EMCal references data structure
  int* DataMap;			// pointer to array containing channels abs position 
  Reference* refMap;      // pointer to array of structures describing each reference
  
  bool Style;             // style of data formatting in FEM: TRUE means  
  // each FEM sends data for 24x6=144 channels,
  // FALSE means FEM sends data for 32x6=192 channels,
  // where 144 are real and other are disconnected AMUADC inputs
  bool outputRaw;         // determins if raw data must be put in output arrays
  bool channelMap;        // type of data mapping: channels(false) or towers(true) 
  static bool FEMchErr;
  int curSM;              // current FEM processed by Parser
  
  SuperModule* smMap;     // pointer to array of structures describing SM
  int nSM;                // number of FEM
  
  int Position;           // local position of current FEM
  int Sector;             // sector where current FEM is located .
  // Sector=0-8, 8-th is dummy one, used to 
  // describe FEM or references that are not located 
  // in any SM
  int tacDelay;           // current TAC-Pre delay. Will be default value for all other SM if not specified
  int postDelay;           // current Post-Pre delay. Will be default value for all other SM if not specified
  
  
  static int ScriptErrors;
  
  virtual int FindSector(char *String);
  virtual int ParseFEM(char* String);
  virtual int ParseCHAN(char* String);
  virtual int ParseREF(char* String);
  virtual int ParseDELAY(char* String);
  virtual int ParseDATA(char* String);
  virtual int ParseMAP(char* String);
  virtual void setSMad();
  virtual void setREFtoSM();
  static int cmpSM(const void* s1, const void* s2);
  static int cmpRef(const void* r1, const void* r2);
  static int cmpCh(const void* c1, const void* c2);
  
  
 public:
  static  void MBCleanStr(char* String);
  static  int MBControl(char *String, char*& RestString);
  static  int MBControl(char *String, char*& RestString, const char ** names, int n);
  /// Converts string to upper register
  static char* strupr(char* s);
  /** Class constructor: parses configuration file and creates all arrays for data and addresses storage.
      int& status - gives result of object creation. 
                    status!=0 means there were error(s) found in configuration file.
      char* fileName - name of a configuration file.
      bool GetRaw - if true array for raw data storage is created and 
                    you can get a pointer to this data.
      bool Asic24ch - if true only FEM channels connected to EMC towers
                      will be read out (24 for each ASIC card),
		      if false all FEM channels (32 for each ASIC card) 
		      will be read out. Default is true.
      bool EmcMapStyle - if true the data map  address corresponds to PHENIX
                         tower numeration schematic, if false then it is FEM channel number. 
  */
  EmcDynamicData( int& status, char* fileName, 
		  bool GetRaw=false, 
		  bool Asic24ch=true, 
		  bool EmcMapStyle=true);
  
  /// Same as above, but input is an object (already filled) instead of a filename.
  EmcDynamicData( int& status,
		  emcConfigurationFile& configFile, 
		  bool GetRaw=false, 
		  bool Asic24ch=true, 
		  bool EmcMapStyle=true);
  
  /// Leading routine for parsing of configuration file.
  void ParseConfigurationFile( int& status, 
			       emcConfigurationFile& configFile, 
			       bool GetRaw=false, 
			       bool Asic24ch=true, 
			       bool EmcMapStyle=true);
  
  /// Returns pointer to an array of SuperModule structures. 
  const SuperModule* getSmMap(){return smMap;}
  /// Returns mapping style: true means EMCal style (towers number),
  // false means FEM style (channel numbers).
  bool getMapStyle(){return channelMap;}
  /** Returns output style: true means only those channels 
      that are connected to towers are readout,
      false means that all 32 channels of each ASIC board will be readout */
  bool getASICStyle(){return Style;}
  /// Returns number of SuperModules. 
  int getnSM(){return nSM;}  
  /// Returns pointer to FEM configuration MAP
  const SuperModule* getSMMap(){return smMap;}
  /// Returns pointer to Data map.
  int* getEmcMap(){return DataMap;} 
  /// Returns pointer to High Gain EMCal data.
  float* getEmcHG(){return Data.hg;} 
  /// Returns pointer to Low Gain EMCal data.
  float* getEmcLG(){return Data.lg;}  
  /// Returns pointer to EMCal Timing data.
  float* getEmcTAC(){return Data.tac;}     
  /// Returns data arrays size.
  int getEmcSize(){return Data.size;}       
  /// Gives pointer to raw data structure array. If GetRaw=false then returns NULL.
  float** getEmcRaw(){return Data.rawdata;}
  /// Returns EmcData structure that contains all pointers to data arrays.  
  EmcData* getEmcData(){return &Data;} 
  
  /// Returns pointer to array of Reference structures.		
  const Reference* getRefMap(){return refMap;}
  /// Returns the size of Reference structures array.   
  int getRefSize(){return Refs.size;}            
  /// Returns pointer to references High Gain data.
  float* getRefHG(){return Refs.hg;}          
  /// Returns pointer to references Low Gain data.
  float* getRefLG(){return Refs.lg;}          
  /// Returns pointer to references Timing data.
  float* getRefTAC(){return Refs.tac;}
  /// Returns pointer to references raw data.        
  float** getRefRaw(){return Refs.rawdata;}
  /// Returns references data structure that contains all pointers to references data arrays.       
  EmcData* getRefData(){return &Refs;}
  /// Destructor frees all memory allocated for data and addresses.
  virtual	~EmcDynamicData();
};
#endif


