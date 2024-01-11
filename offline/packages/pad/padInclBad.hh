//--------------------------------------------------- 
// Class: padInclBad
// 
// Created by: David Silvermyr
// 
// Description: 
// This class will take bad channels and bad ROCs from
// a PadCalibrationObject and modify the dPcXRaw tables
//
//---------------------------------------------------

#ifndef __PADINCLBAD_HH__
#define __PADINCLBAD_HH__

#include "phool.h"

#include <set>

class dPadRawWrapper;
class PHTimeStamp;
class PadAddressObject;
class PHCompositeNode;
class PadCalibrationObject;

class padInclBad 
{ 

public:

  padInclBad();                             // constructor
  virtual ~padInclBad();                            // destructor

  // this is where everything happens..
  virtual PHBoolean event(PHCompositeNode *topNode);       
  // runs on an event and updates the raw tables according to the contents of PadCalObj

  // Data member access methods

  // methods for fetching PadCalObj data
  PHBoolean FetchCalDataFromFiles(); // from default ASCII files
  PHBoolean FetchCalDataFromFiles(const char* filebadch, const char* filebadroc); // from specific ASCII files
  PHBoolean FetchCalDataFromObjy(PHTimeStamp &TS); // from Objectivity database

  // fill our badsets with info from the PadCalObj
  PHBoolean FillBadSets();
  PHBoolean FillBadSetsWithROCInfo();
  PHBoolean FillBadSetsWithChInfo();

  // conversion method
  int ROCtypeToPadType(short badroctype, short det, short padz, short padx); 

  // Print out what's going on/has been done
  // general information
  void print();

  // raw data tables to file (mainly for debugging purposes..) 
  PHBoolean printToFile(const int pc = 0, const char *info = "post",
			const int ievt = 0);
  PHBoolean printToFile(const dPadRawWrapper *dPcRaw,
			const int pc = 0, const char *info = "pre",
			const int ievt = 0);

  PHBoolean printBadToFile();

  // set debug level (0 = do not debug)
  void setDebugLevel(int debug) { Debug = debug; }

  // determine whether we should modify the raw data tables with bad 
  // ROC/channel info or not
  void doInclBadROCs() { inclBadROCs=1; }
  void doNotInclBadROCs() { inclBadROCs=0; }
  void doInclBadChs() { inclBadChs=1; }
  void doNotInclBadChs() { inclBadChs=0; }

  // determine whether we should add or remove hot & inactive channels
  void RemoveHotROCs() { rmHotROCs = 1; } 
  void doNotRemoveHotROCs() { rmHotROCs = 0; } 
  void RemoveInactiveROCs() { rmInactROCs = 1; } 
  void doNotRemoveInactiveROCs() { rmInactROCs = 0; } 
  void AddHotROCs() { addHotROCs = 1; } 
  void doNotAddHotROCs() { addHotROCs = 0; } 
  void AddInactiveROCs() { addInactROCs = 1; } 
  void doNotAddInactiveROCs() { addInactROCs = 0; } 

  void RemoveUnSynchROCs() { rmUnSynchROCs = 1; } 
  void doNotRemoveUnSynchROCs() { rmUnSynchROCs = 0; } 
  void AddUnSynchROCs() { addUnSynchROCs = 1; } 
  void doNotAddUnSynchROCs() { addUnSynchROCs = 0; } 

  void RemoveHotChs() { rmHotChs = 1; } 
  void doNotRemoveHotChs() { rmHotChs = 0; } 
  void RemoveInactiveChs() { rmInactChs = 1; } 
  void doNotRemoveInactiveChs() { rmInactChs = 0; } 
  void AddHotChs() { addHotChs = 1; } 
  void doNotAddHotChs() { addHotChs = 0; } 
  void AddInactiveChs() { addInactChs = 1; } 
  void doNotAddInactiveChs() { addInactChs = 0; } 
 
  // get number of included bad ROCs
  int getNumInclBadROCs() { return NumInclBadROCs; }

  // get number of included bad channels
  int getNumInclBadChs() { return NumInclBadChs; }

private:

  // this object holds all calibration info
  PadCalibrationObject* PadCalObj;

  // for translating some indices..
  PadAddressObject* addressObj;

  // Debug flag (0 = do not debug) 
  int Debug;

  // Inclusion flag (0 = do not include bad ROCs or channels respectively) 
  int inclBadROCs,inclBadChs;

  // Flags for whether we should rm all, add all etc.
  short rmHotROCs,rmInactROCs,addHotROCs,addInactROCs;
  short rmUnSynchROCs,addUnSynchROCs;
  short rmHotChs,rmInactChs,addHotChs,addInactChs;

  // Number of included bad ROCs
  int NumInclBadROCs;

  // Number of included bad channels
  int NumInclBadChs;

  // for STL set usage
  class Pad {
  public:
    // default ctor
    Pad() {
      pc = 0; // 0,1,2 for PC1,2,3..
      arm = 0;
      sector = 0;
      side = 0;
      padz = 0;
      padx = 0;
      padtype = 0;
      id = 0;
    }
    // ctor with arguments
    Pad(int p, int a, int se, int si, int px, int pz, int pt, int i) {
      pc = p; // 0,1,2 for PC1,2,3..
      arm = a;
      sector = se;
      side = si;
      padz = pz;
      padx = px;
      padtype = pt;
      id = i;
    }
    ~Pad(){}
    int pc;
    int arm;
    int sector;
    int side;
    int padz;
    int padx;
    int padtype;
    int id;
  };

  class OrderFunc
  {
  public:
    bool operator() (const Pad p1, const Pad p2) const 
    { 
      // return 1 if ptr1 should be before ptr2, else 0
      return ( p1.pc < p2.pc ||
	      (p1.pc == p2.pc && (p1.arm < p2.arm ||
	      (p1.arm == p2.arm && (p1.sector < p2.sector ||
	      (p1.sector == p2.sector && (p1.side < p2.side ||
	      (p1.side == p2.side && (p1.padz < p2.padz ||
	      (p1.padz == p2.padz && (p1.padx < p2.padx) ))))))))) );
    } 
  };
  
  typedef std::set< Pad, OrderFunc > set_type;
  typedef set_type::iterator set_iterator;

  set_type addset; // this is our set of 'bad' pads to be added
  set_type removeset; // this is our set of 'bad' pads to be removed

  set_type ioset; // the set that combines the input pads
  // and the bad pads to an output set

  set_iterator iter; // and the associated iterator
}; 

#endif /* __PADINCLBAD_HH__ */
