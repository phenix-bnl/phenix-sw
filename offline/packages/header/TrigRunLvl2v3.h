#ifndef __TRIGRUNLVL2V3_H
#define __TRIGRUNLVL2V3_H

#include "TrigRunLvl2.h"
#include <iostream>
#include <string>

class TrigRunLvl2v3: public TrigRunLvl2
{
 public:
  TrigRunLvl2v3();
  virtual ~TrigRunLvl2v3() {}
  TrigRunLvl2v3(const TrigRunLvl2v3&);
  
  TrigRunLvl2v3* clone() const { return new TrigRunLvl2v3(*this); }

  int isValid() const;
  void identify(std::ostream& os = std::cout) const;

  // *** char lvl2_description
  const char * get_lvl2_description() const {return lvl2_description.c_str();}
  void set_lvl2_description(const char *name) {lvl2_description = name; return;}

  // *** int level2 trigger version
  unsigned int get_lvl2_version() const {return lvl2_version;}
  void set_lvl2_version(const unsigned int ival) {lvl2_version = ival; return;}

  // *** level2 run enable state (0 = not run, 1 = level2 was running)
  unsigned int get_lvl2_run_enable() const {return lvl2_run_enable;}
  void set_lvl2_run_enable(const unsigned int ival) {lvl2_run_enable = ival; return;}

  // *** level2 reject enable state (0 = not rejecting, 1 = level2 was rejecting)
  unsigned int get_lvl2_reject_enable() const {return lvl2_reject_enable;}
  void set_lvl2_reject_enable(const unsigned int ival) {lvl2_reject_enable = ival; return;}

  // *** ask whether lvl2 rejection is on for this lvl1 (0=no,1=yes) for this lvl1 index value - input is index number
  unsigned int get_lvl1_lvl2_reject_enable(const unsigned int i) const;
  // *** set whether lvl2 rejection is on for this lvl1 (0=no,1=yes) for this lvl1 index value - input is index number
  void set_lvl1_lvl2_reject_enable(const unsigned int ival, const unsigned int i);
  unsigned int get_lvl1_lvl2_force_accept(const unsigned int i) const {return lvl1_lvl2_force_accept[i];}
  void set_lvl1_lvl2_force_accept(const unsigned int ival, const unsigned int i) {lvl1_lvl2_force_accept[i] = ival; return;}

  // *** lvl2_trig_name
  const char * get_lvl2_trig_name(const unsigned int i) const {return lvl2_trig_name[i].c_str();}
  void set_lvl2_trig_name(const char *name, const unsigned int i) {lvl2_trig_name[i] = name; return;}

  unsigned int get_lvl2_trig_bit(const unsigned int i) const {return lvl2_trig_bit[i];}
  void set_lvl2_trig_bit(const unsigned int ival, const unsigned int i) {lvl2_trig_bit[i] = ival; return;}

  unsigned int get_lvl2_trig_version(const unsigned int i) const {return lvl2_trig_version[i];}
  void set_lvl2_trig_version(const unsigned int ival, const unsigned int i) {lvl2_trig_version[i] = ival; return;}

  // *** (yes=1/no=0) on running lvl2 for a given lvl1 - input lvl2 index, lvl1 index
  unsigned int get_lvl2_lvl1_assoc(const unsigned int index_lvl2, const unsigned int index_lvl1) const {return lvl2_lvl1_assoc[index_lvl2][index_lvl1];}
  void set_lvl2_lvl1_assoc(const unsigned int ival, const unsigned int index_lvl2, 
			   const unsigned int index_lvl1) {
    lvl2_lvl1_assoc[index_lvl2][index_lvl1] = ival;
    return;
  }

  // *** prescale on running lvl2 for a given lvl1 - input lvl2 index, lvl1 index
  unsigned int get_lvl2_lvl1_prescale(const unsigned int index_lvl2, const unsigned int index_lvl1) const {return lvl2_lvl1_prescale[index_lvl2][index_lvl1];}
  void set_lvl2_lvl1_prescale(const unsigned int ival, const unsigned int index_lvl2, const unsigned int index_lvl1) {lvl2_lvl1_prescale[index_lvl2][index_lvl1] = ival; return;}
  
 private:
  void copyTo(TrigRunLvl2v3& o) const;
  
 protected:

  // the below fields assume a maximum of 64 level2 trigger algorithms
  // and a maximum of 32 level1 trigger bits (only one GL1-1 board)

  std::string    lvl2_description;           // placeholder for future lvl2 trigger version name
  unsigned int   lvl2_version;               // the CVS tag of the lvl2 trigger version
                                             // this information still needs to be added to Objy
  unsigned int   lvl2_run_enable;            // level2 running or not overall   
  unsigned int   lvl2_reject_enable;         // rejection turned on for at least one lvl1 trigger
  
  unsigned int   lvl1_lvl2_reject_enable;    // each of 32 level1 trigger can be individually 
                                             // enabled with rejection of all of its 
                                             // associated level2 triggers
  unsigned int   lvl1_lvl2_force_accept[32]; // each level1 trigger has a forced accept value
                                             // that automatically accepts the event at
                                             // level2.  This is NOT the same as the level1
                                             // prescale value!  
  
  std::string    lvl2_trig_name[64];         // list of the 64 level2 trigger names 
                                             // (eg. "SingleElectron")
  unsigned int   lvl2_trig_bit[64];          // index in bit map                          
  unsigned int   lvl2_trig_version[64];      // placeholder for future individual versions
  
  unsigned int   lvl2_lvl1_assoc[64][32];    // yes/no on the running of lvl2 on this lvl1
  
  unsigned int   lvl2_lvl1_prescale[64][32]; // prescale for each lvl2 and associated lvl1    
  
  ClassDef(TrigRunLvl2v3,1)

};

#endif /*  __TRIGRUNLVL2V3_H */



