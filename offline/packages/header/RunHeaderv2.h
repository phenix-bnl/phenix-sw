#ifndef RUNHEADERV2_H
#define RUNHEADERV2_H

#include "RunHeader.h"

#include <iostream>
#include <ctime>


///
class RunHeaderv2: public RunHeader
{
 public:
  /// ctor
  RunHeaderv2();

  /// dtor
  virtual ~RunHeaderv2() {}
  
  RunHeaderv2* clone() const { return new RunHeaderv2(*this); }

  /// Clear Event 
  void Reset();

  /** identify Function from PHObject
      @param os Output Stream 
   */
  void identify(std::ostream& os = std::cout) const;

  /** identify the v2 part
      @param os Output Stream 
   */
  void identifyv2(std::ostream& os = std::cout) const;


  /// isValid returns non zero if object contains valid data
  int isValid() const;

   /// get Run Number
   int get_RunNumber() const {return RunNumber;}
   /// set Run Number
   void set_RunNumber(const int run) {RunNumber= run; return;}

   /// get Start Time of run (in unix ticks, use ctime to convert to date string)
   time_t get_TimeStart() const {return TimeStart;}
   /// set Start Time
   void set_TimeStart(const time_t start) {TimeStart = start; return;}

   /// get Time of End Run (in unix ticks)
   time_t get_TimeStop() const {return TimeStop;}
   /// set Time of End Run
   void set_TimeStop(const time_t stop) {TimeStop = stop; return;}

   /// get Current in North Magnet
   int get_currentNorth() const {return currentNorth;}
   /// set Current in North Magnet
   void set_currentNorth(const int icur) {currentNorth = icur; return;}

   /// get Current in South Magnet
   int get_currentSouth() const {return currentSouth;}
   /// set Current in South Magnet
   void set_currentSouth(const int icur) {currentSouth = icur; return;}

   /// get Current in Central Magnet
   int get_currentCentral() const {return currentCentral;}
   /// set Current in Central Magnet
   void set_currentCentral(const int icur) {currentCentral = icur; return;}

 protected:

   int RunNumber;
   time_t TimeStart;
   time_t TimeStop;
   int currentNorth;
   int currentSouth;
   int currentCentral;

 private: // prevent doc++ from showing ClassDef
   ClassDef(RunHeaderv2,1)

};

#endif /* __RUNHEADERV2_H */

