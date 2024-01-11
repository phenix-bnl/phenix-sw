#ifndef __EMCFEM_H__
#define __EMCFEM_H__

// EMCalFEE.h Describes class EMCalFEE
// Reads data from file or pool and stores it in EmcDynamicData obect arrays.
// Array FEMstatus keeps information about FEM faults during current event.
// Possible mistakes: FEMstatus[i]=1 if FEM i had not its packet in this event while other had;
//                    FEMstatus[i]=2 if delay between TAC and presamle or between post and presample
//                                   does not equal to expected value
//==================================================================================================
// Created by Sergei Belikov 04/20/99
//-------------------------------------------------------------------------------------------------- 

class Eventiterator;
class Event;
class EmcDynamicData;

/** Min and max values of data for both Low and High gains.
    These limits are used to check if energy outputs are at 
    the saturation or not working at all. */    
struct FEMlimits
{
  /// Minimal acceptable data value.
  int minAmp;
  /// Maximal acceptable data value.
  int maxAmp;
};

/** Keeps numbers of TAC, Pre and Post AMUADC cells.
    that were used to get data for current event.*/  
struct cells
{
  /// Pre sample cell number. 
  int pre;
  /// Post sample cell number. 
  int post;
  /// TAC cell number. 
  int tac;
};

/// Current event parameters. 
struct evtInfo
{  
  /// Length of event data.
  int evtLength;
  /// Event type. Can be Data, BeginRun, EndRun, etc.
  int evtType;
  /// Event number.
  int evtSequence;
  /// Run number.
  int evtRunNumber;
};

/** Reads an Event and extract emcal data from it.
    Does this for channels listed in EmcDynamicData object only.
    In addition this object 
    creates its own arrays that keep information about 
    errors occured during readout.    
*/
class EMCalFEE
{
protected:
  FEMlimits * lim;
  EmcDynamicData* dd;
  int* FEMstatus;
  int* DataErrors;
  int* RefErrors;
  cells* EvtCells;
  evtInfo evtIn;
  //  We keep the pointer to last event read-in
  Event  * event;
public:
  /** Class object constructor. Status returns the result of object creation. 
      If failed then status has notzero value.
  */
  EMCalFEE(EmcDynamicData* d, FEMlimits * l, int& status);
  /// Destructor frees memory allocated for readout status keeping arrays.
  virtual ~EMCalFEE();
  /// Process an external event (usefull in offline)
  virtual int processEvent(Event* evt) ;
  /// Gets next event from Eventiterator. Eventiterator can be File or DDpool Eventiterator.
  virtual int getNextEvent(Eventiterator *it);
  /// Read next event without processing it (used to scan through the input file)
  virtual int readNextEvent(Eventiterator *it);
  /// Process next event (assumed that event is already read-in)
  virtual int convertNextEvent();
  /** Returns pointer to FEM readout status array. 
      Length of this array is equal to the number of
      FEMs. Each element keeps readout status of FEM. It can have the values:

      NO_PACKET - means that there is no packet from this FEM in current event.
                  All data are set to 0.
      WRONG_CELL_NUMBER - delay between pre and post samples or between pre and
                          TAC is not equal to values stored in EmcDynamicData 
			  object (this object gets these delays from configuration file).
      OK - noproblems.
  */
  const int* getFEMstatus(){return FEMstatus;}
  /** Returns pointer to data errors array. 
      Possible errors:
      OK - no errors.
      HG_PRE_TOO_LOW -  high gain Pre sample value is lower then minimal
                        value stored in FEMlimits l structure.
      HG_POST_TOO_LOW - high gain Post sample value is lower then minimal
                        value stored in FEMlimits l structure.
      HG_PRE_TOO_HIGH - high gain Pre sample value is higher then maximal
                        value stored in FEMlimits l structure.
      HG_POST_TOO_HIGH - high gain Post sample value is higher then maximal
                        value stored in FEMlimits l structure.
      LG_PRE_TOO_LOW - low  gain Pre sample value is lower then minimal
                        value stored in FEMlimits l structure.
      LG_POST_TOO_LOW - low gain Post sample value is lower then minimal
                        value stored in FEMlimits l structure.
      LG_PRE_TOO_HIGH - low gain Pre sample value is higher then maximal
                        value stored in FEMlimits l structure.
      LG_POST_TOO_HIGH - low gain Post sample value is higher then maximal
                        value stored in FEMlimits l structure.
			The result in array element can be combination of these errors.
  */	
  int      * getDataErrors(){return DataErrors;}
  /// Returns pointer to references errors array. Error values  are the same as for towers.
  int     * getRefErrors(){return RefErrors;}
  /// Returns pointer to the structure that contains AMU cell numbers for Pre, Post and TAC.  
  const cells* getCells(){return EvtCells;}
  /// returns pointer to Event info structure.
  const evtInfo* getEvtInfo(){return &evtIn;}
  /// Returns event length.
  int getEvtLength(){return evtIn.evtLength;}
  /// Returns event type. 
  int getEvtType(){return evtIn.evtType;}
  /// Returns event number.
  int getEvtSequence(){return evtIn.evtSequence;}
  /// Returns run number.
  int getRunNumber(){return evtIn.evtRunNumber;}
};
#endif
