#ifndef RUNHEADER_H
#define RUNHEADER_H

#include <PHObject.h>

#include <ctime>
#include <iostream>

///
class RunHeader: public PHObject
{
 public:

  /// dtor
  virtual ~RunHeader() {}

  virtual RunHeader* clone() const;

  /// Clear Event
  virtual void Reset();

  /** identify Function from PHObject
      @param os Output Stream 
   */
  virtual void identify(std::ostream& os = std::cout) const;

  /// isValid returns non zero if object contains valid data
  virtual int isValid() const;

  /// get Run Number
  virtual int get_RunNumber() const;
  /// set Run Number
  virtual void set_RunNumber(const int run);

  /// get BField (deprecated in v2)
  virtual double get_Bfield() const;
  /// set Bfield (deprecated in v2)
  virtual void set_Bfield(const double rval);

  /// get Start Time of run (in unix ticks, use ctime to convert to date string)
  virtual time_t get_TimeStart() const;
  /// set Start Time
  virtual void set_TimeStart(const time_t ival);

  /// get Time of End Run (in unix ticks)
  virtual time_t get_TimeStop() const;
  /// set Time of End Run
  virtual void set_TimeStop(const time_t ival);

  /// get Current in North Magnet
  virtual int get_currentNorth() const;
  /// set Current in North Magnet
  virtual void set_currentNorth(const int icur);

  /// get Current in South Magnet
  virtual int get_currentSouth() const;
  /// set Current in South Magnet
  virtual void set_currentSouth(const int icur);

  /// get Current in Central Magnet
  virtual int get_currentCentral() const;
  /// set Current in Central Magnet
  virtual void set_currentCentral(const int icur);
  
  /// get Current in Central Magnet
  virtual int get_currentInner() const;
  /// set Current in Central Magnet
  virtual void set_currentInner(const int icur);

  /// switches off the pesky virtual warning messages
  virtual void NoWarning(const int i = 1);

 protected:
  void warning(const char *func) const;
  
 private: // prevent doc++ from showing ClassDef
  ClassDef(RunHeader,1)

};

#endif
