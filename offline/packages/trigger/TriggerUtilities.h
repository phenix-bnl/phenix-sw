/*  ////////////////////////
// TriggerUtilities:
//    analysis object implementation:
//  general utility object 
// 
//  This object should be used in combination with TriggerHelper
//  for extending TriggerHelper's use.  It's original inception was
//  is so that trigger helper can be run from a prdf file for use 
//  in the online monitoring framework.  It will do this by creating 
//  the TrigLvl1 (2) nodes TriggerHelper needs itself.  
//
//  This only reason this object was made separate and in a separate library
//  from TriggerHelper was to keep the libary containing TriggerHelper
//  "light" :  by itself TrigHelper should not need to link against the
//   database/Pdb cal, or use the database.  
//
////
//   
//    author: jfrantz  
//
//////////////////////////
*/

#ifndef __TriggerUtilities_H
#define __TriggerUtilities_H

#include <string>

class TrigLvl1;
class TrigRunLvl1;
class TrigRunLvl2;
class TriggerHelper;
class Lvl2DecisionOut;
class Event;


class TriggerUtilities
{
///
  public:
  TriggerUtilities();
  virtual ~TriggerUtilities();

  // success 1, failure < 0
  int dbFillTrigRunObjects(int runNumber, TrigRunLvl1* toFill1, TrigRunLvl2* toFill2);
  int fillTrigLvl1(Event * pdrfEvent, TrigLvl1 * tobeFilled);
  int fillLvl2DecisionOut(Event *, Lvl2DecisionOut * tobeFilled);  
  
  TrigRunLvl1 * getNewTrigRunLvl1();
  TrigRunLvl2 * getNewTrigRunLvl2();
  
  // this function gives a triggerHelper object from the input arguments
  // doing database lookups neccessary for trigger helper only if the run
  // number give is different than the last run number given by previous calls to 
  // the same instance of TriggerUtilities. Therefore the instance of TriggerUtilities
  // should be kept persistent across events in order to avoid unneccessary
  // db lookups and object creations
  // 
  // passing a NULL event pointer (the default) will work, but the trigger helper
  // object returned will not be able to access information on which lvl1/2 triggers
  // fired

  TriggerHelper * getTriggerHelper(int runNumber, Event * pdrfEvent = 0);
  
  
 private:
  int send_message(const int severity, const std::string &text);  
  void cleanupTriggerHelper();
  TriggerHelper * _trigHelp;
  int verbosity;
  
};

#endif
