//----------------------------------------------------------------------- 
// Class: padEvtToRaw
// 
// Created by: David Silvermyr
// 
// Description: 
// This class reads fired channel information from an Event (the packets
// in an event) and fills the raw data tables.
//
//------------------------------------------------------------------------

#ifndef __PADEVTTORAW_HH__
#define __PADEVTTORAW_HH__

#include <phool.h>

class PHCompositeNode;
class Event;
class PadAddressObject;
class PadCalibrationObject;
class PHTimeStamp;

class padEvtToRaw
{ 

public:
  padEvtToRaw();                             // constructor
  virtual ~padEvtToRaw();                            // destructor


  int FetchCalDataFromPdbCal(PHTimeStamp &TS);
  // this is where everything happens..
  virtual int event(PHCompositeNode* topNode, Event *evt, Event *evt2=0, short evtreal=1, short evt2real=1);
    // where evtreal,evt2real= 0, when we are sending a simulated event
    //               = 1, when we are sending a real event

public:


private:

  PadAddressObject *addressObj;
  PadCalibrationObject *PadCalObj;

}; 

#endif /* __PADEVTTORAW_HH__ */
