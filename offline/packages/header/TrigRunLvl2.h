#ifndef __TRIGRUNLVL2_H
#define __TRIGRUNLVL2_H

#include "PHObject.h"
#include "phool.h"

#include <iostream>

class TrigRunLvl2: public PHObject
{
 public:
  virtual ~TrigRunLvl2() {}

  virtual TrigRunLvl2* clone() const;

  virtual void Copy(TrigRunLvl2 *);
  virtual void Copy(TObject &object) const { PHOOL_VIRTUAL_WARNING; }
  virtual int isValid() const;
  virtual void identify(std::ostream& os = std::cout) const;
  virtual void Reset();

  // add methods to get the bit information with an arguement
  // also add some specific methods for getting out specific
  // things -- how to value enumerated defined values

  virtual const char * get_lvl2_description() const;
  virtual void set_lvl2_description(const char * name);
  virtual unsigned int get_lvl2_version() const;
  virtual void set_lvl2_version(const unsigned int ival);
  virtual unsigned int get_lvl2_run_enable() const;
  virtual void set_lvl2_run_enable(const unsigned int ival);
  virtual unsigned int get_lvl2_reject_enable() const;
  virtual void set_lvl2_reject_enable(const unsigned int ival);
  virtual unsigned int get_lvl1_lvl2_reject_enable(const unsigned int i) const;
  virtual void set_lvl1_lvl2_reject_enable(const unsigned int ival, const unsigned int i);
  virtual unsigned int get_lvl1_lvl2_force_accept(const unsigned int i) const;
  virtual void set_lvl1_lvl2_force_accept(const unsigned int ival,const unsigned int i);

  virtual const char * get_lvl2_trig_name(const unsigned int i) const;
  virtual void set_lvl2_trig_name(const char *name,const unsigned int i);
  virtual unsigned int get_lvl2_trig_bit(const unsigned int i) const;
  virtual void set_lvl2_trig_bit(const unsigned int ival,const unsigned int i);
  virtual unsigned int get_lvl2_trig_version(const unsigned int i) const;
  virtual void set_lvl2_trig_version(const unsigned int ival, const unsigned int i);
  virtual unsigned int get_lvl2_lvl1_assoc(const unsigned int i, const unsigned int j) const;
  virtual void set_lvl2_lvl1_assoc(const unsigned int ival, const unsigned int i, const unsigned int j);
  virtual unsigned int get_lvl2_lvl1_prescale(const unsigned int i, const unsigned int j) const;
  virtual void set_lvl2_lvl1_prescale(const unsigned int ival, const unsigned int i, const unsigned int j);

  virtual void dump_info(const TrigRunLvl2 *runlevel2) const;
  void ShutUp(const int i = 1);

 private:
  void warning(const char *funcname) const;

  ClassDef (TrigRunLvl2,1) 

};

#endif /*  __TRIGRUNLVL2_H */
