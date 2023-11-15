#ifndef __TRIGRUNLVL1V3_H
#define __TRIGRUNLVL1V3_H

#include "TrigRunLvl1.h"
#include <ctime>
#include <iostream>
#include <string>

class TrigRunLvl1v3: public TrigRunLvl1
{
 public:
  TrigRunLvl1v3();
  TrigRunLvl1v3(const TrigRunLvl1v3&);

  virtual ~TrigRunLvl1v3() {}

  TrigRunLvl1v3* clone() const { return new TrigRunLvl1v3(*this); }

  int isValid() const;
  void identify(std::ostream& os = std::cout) const;
  void Reset() {return;} // the content should not be resetted event by event

   // *** character string trigger_description
   const char * get_lvl1_trigger_description() const {return trigger_description.c_str();}
   void set_lvl1_trigger_description(const char *name) {trigger_description = name; return;}

   unsigned int get_lvl1_trigger_version() const {return trigger_version;}
   void set_lvl1_trigger_version(const unsigned int ival) {trigger_version = ival; return;}

   // *** character string bbcll1_description
   const char * get_lvl1_bbcll1_description() const {return bbcll1_description.c_str();}
   void set_lvl1_bbcll1_description(const char *name) {bbcll1_description = name; return;}

   unsigned int get_lvl1_bbcll1_version() const {return bbcll1_version;}
   void set_lvl1_bbcll1_version(const unsigned int ival) {bbcll1_version = ival; return;}

   // *** character string parition_name
   const char * get_lvl1_partition_name() const {return partition_name.c_str();}
   void set_lvl1_partition_name(const char *name) {partition_name = name; return;}

   // *** character string lvl1_trigger_names - input is index number
   const char * get_lvl1_trig_name(const unsigned int i) const {return lvl1_trig_name[i].c_str();}
   void set_lvl1_trig_name(const char *name, const unsigned int i) {lvl1_trig_name[i] = name; return;}
   // *** character string lvl1_trigger_names - input is bit number
   const char * get_lvl1_trig_name_bybit(const unsigned int bit) const;

   // *** character string lvl1_rbit_names - input is index number
   const char * get_lvl1_rbit_name(const unsigned int i) const {return lvl1_rbit_name[i].c_str();}
   void set_lvl1_rbit_name(const char *name, const unsigned int i) {lvl1_rbit_name[i] = name; return;}

   // *** get trigger enable (0=no, 1=yes) for that index value - input is index number 
   unsigned int get_lvl1_trigger_enable(const unsigned int i) const;
   // *** get trigger enable (0=no, 1=yes) for that bit value - input is bit value 
   unsigned int get_lvl1_trigger_enable_bybit(const unsigned int bit) const;
   // *** set trigger enable (0=no, 1=yes) for that index value - input is index number 
   void set_lvl1_trigger_enable(const unsigned int ival, const unsigned int i);
   // *** get trigger associated bit value for this index value - input is index number
   unsigned int get_lvl1_trig_bit(const unsigned int i) const {return lvl1_trig_bit[i];}
   void set_lvl1_trig_bit(const unsigned int ival, const unsigned int i) {lvl1_trig_bit[i] = ival; return;}

   // *** get trigger associated index value for this trigger bit value - input is bit number
   unsigned int get_lvl1_trig_index(const unsigned int bit) const;
   // *** get trigger associated bit value for this index value - input is index number
   unsigned int get_lvl1_trig_bitmask(const unsigned int i) const {return lvl1_trig_bitmask[i];}
   void set_lvl1_trig_bitmask(const unsigned int ival, const unsigned int i) {lvl1_trig_bitmask[i] = ival; return;}

   // *** get trigger scale down value for this index value - input is index number
   unsigned int get_lvl1_trig_scale_down(const unsigned int i) const {return lvl1_trig_scale_down[i];}
   void set_lvl1_trig_scale_down(const unsigned int ival, const unsigned int i) {lvl1_trig_scale_down[i] = ival; return;}
   unsigned int get_lvl1_trig_scale_down_bybit(const unsigned int bit) const;
   // *** ask whether lvl2 rejection is on for this lvl1 (0=no,1=yes) for this lvl1 index value - input is index number
   unsigned int get_lvl1_lvl2_reject_enable(const unsigned int i) const;
   // *** ask whether lvl2 rejection is on for this lvl1 (0=no,1=yes) for this lvl1 bit value 
   unsigned int get_lvl1_lvl2_reject_enable_bybit(const unsigned int bit) const;
   // *** set whether lvl2 rejection is on for this lvl1 (0=no,1=yes) for this lvl1 index value - input is index number
   void set_lvl1_lvl2_reject_enable(const unsigned int ival, const unsigned int i);
   // *** print out of entire Run Level1 Trigger Information for Debugging - input is pointer to TrigRunLvl1 object

   // *** get raw scaler rate for this index value - input is index number
   float get_lvl1_trig_rate_begin(const unsigned int i) const {return lvl1_trig_rate_begin[i];}
   void set_lvl1_trig_rate_begin(const float val, const unsigned int i) {lvl1_trig_rate_begin[i] = val;}
   
   time_t get_start_time() const { return start_time; }
   void set_start_time(const time_t val) { start_time = val;}
   
   int get_run_number() const { return run_number; }
   void set_run_number(const int val) { run_number = val;}

  private:
   void copyTo(TrigRunLvl1v3&) const;

 protected:

   void init();

   // could add fields for partition name, bbcll1 name/version
   // probably a good idea for completeness

   // the below assumes a maximum of 32 level1 triggers (ie. one GL1-1 board)

   std::string   trigger_description;          // the phTrigger version (eg. BigPartition)
   unsigned int  trigger_version;              // the phTrigger version number (eg. 80)   

   std::string   bbcll1_description;           // the phTrigger BBCLL1 (eg. BONA_FIDA_QGP)
   unsigned int  bbcll1_version;               // version number (eg. 5)

   std::string   partition_name;               // name of partition run for this PRDF

   std::string   lvl1_trig_name[32];           // list of the 32 level1 trigger names
                                               //          (eg. "BBCLL1>=2")
   std::string   lvl1_rbit_name[130];          // list of the 130 reduced bit names
                                               // this includes 32 RBIB inputs (external)
                                               //               32 LL1  inputs
                                               //               32 Disable Granule bits
                                               //               32 Forced Accept bits
                                               //                2 N/S Beam Status mode bits
   unsigned int   lvl1_trig_enable;            // which level1 triggers were enabled (0/1)

                                               // assuming 32 level1 trigger bits this is a bit map
   unsigned int   lvl1_trig_bit[32];           // which trigger bit is associated with level1
   unsigned int   lvl1_trig_scale_down[32];    // prescale value for each level1 trigger (eg. 39)
   unsigned int   lvl1_trig_bitmask[32];       // bit mask
   unsigned int   lvl1_lvl2_reject_enable;     // was the level2 enabled for this lvl1 trigger (bitwise)

   int run_number;   
   time_t start_time;  // this was exactly when this config data was stored by runcontrol...it may be 
                      // slightly different than the global run start time even though both occur at "StartRun"
   float lvl1_trig_rate_begin[32];       //raw scaler rates for when this config info was stored by r.c.

   ClassDef(TrigRunLvl1v3,1)

};

#endif /*  __TRIGRUNLVL1V3_H */


