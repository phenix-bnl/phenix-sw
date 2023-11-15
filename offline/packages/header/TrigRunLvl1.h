#ifndef __TRIGRUNLVL1_H
#define __TRIGRUNLVL1_H

#include "PHObject.h"
#include "phool.h"

#include <ctime>
#include <iostream>
#include <string>

class TrigRunLvl1: public PHObject
{
 public:
  virtual ~TrigRunLvl1() {}
  virtual void Copy(TrigRunLvl1 *);
  virtual void Copy(TObject &object) const { PHOOL_VIRTUAL_WARNING; }
  virtual TrigRunLvl1* clone() const;

  virtual int isValid() const;
  virtual void identify(std::ostream& os = std::cout) const;
  virtual void Reset();

  // add methods to get the bit information with an arguement
  // also add some specific methods for getting out specific
  // things -- how to value enumerated defined values

  virtual const char * get_lvl1_trigger_description() const;
  virtual void set_lvl1_trigger_description(const char *name);
  virtual unsigned int get_lvl1_trigger_version() const;
  virtual void set_lvl1_trigger_version(const unsigned int ival);

  virtual const char * get_lvl1_bbcll1_description() const;
  virtual void set_lvl1_bbcll1_description(const char *name);
  virtual unsigned int get_lvl1_bbcll1_version() const;
  virtual void set_lvl1_bbcll1_version(const unsigned int ival);

  virtual const char * get_lvl1_partition_name() const;
  virtual void set_lvl1_partition_name(const char *name);

  virtual const char * get_lvl1_trig_name(const unsigned int i) const;
  virtual const char * get_lvl1_trig_name_bybit(const unsigned int bit) const;
  virtual void set_lvl1_trig_name(const char *name,const unsigned int i);
  virtual const char * get_lvl1_rbit_name(const unsigned int i) const;
  virtual void set_lvl1_rbit_name(const char *name,const unsigned int i);


  virtual unsigned int get_lvl1_trig_bitmask(const unsigned int i) const;
  virtual void set_lvl1_trig_bitmask(const unsigned int ival, const unsigned int i);
  virtual unsigned int get_lvl1_trigger_enable(const unsigned int i) const;
  virtual unsigned int get_lvl1_trigger_enable_bybit(const unsigned int bit) const;
  virtual void set_lvl1_trigger_enable(const unsigned int ival, const unsigned int i);
  virtual unsigned int get_lvl1_trig_bit(const unsigned int i) const;
  virtual unsigned int get_lvl1_trig_index(const unsigned int bit) const;
  virtual void set_lvl1_trig_bit(const unsigned int ival,const unsigned int i);
  virtual unsigned int get_lvl1_trig_scale_down(const unsigned int i) const;
  virtual unsigned int get_lvl1_trig_scale_down_bybit(const unsigned int bit) const;
  virtual void set_lvl1_trig_scale_down(const unsigned int ival,const unsigned int i);
  virtual unsigned int get_lvl1_lvl2_reject_enable(const unsigned int i) const;
  virtual unsigned int get_lvl1_lvl2_reject_enable_bybit(const unsigned int bit) const;
  virtual void set_lvl1_lvl2_reject_enable(const unsigned int ival, const unsigned int i);

  virtual float get_lvl1_trig_rate_begin(const unsigned int i) const;
  virtual void set_lvl1_trig_rate_begin(float val,const unsigned int i);

  virtual void dump_info(const TrigRunLvl1 *runlevel1) const;

  virtual int get_run_number() const;
  virtual void set_run_number(const int runno);
  virtual time_t get_start_time() const;
  virtual void set_start_time(const time_t time);
  void ShutUp(const int i = 1);

 private:
  void warning(const char *funcname) const;

  ClassDef (TrigRunLvl1,1) 

};

#endif /*  __TRIGRUNLVL1_H */
