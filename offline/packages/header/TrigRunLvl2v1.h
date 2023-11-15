#ifndef __TRIGRUNLVL2V1_H
#define __TRIGRUNLVL2V1_H

#include "TrigRunLvl2.h"

#include <TString.h>
#include <iostream>


class TrigRunLvl2v1: public TrigRunLvl2
{
 public:
  TrigRunLvl2v1();
  virtual ~TrigRunLvl2v1() {}

  int isValid() const;
  void identify(std::ostream& os = std::cout) const;
  void Reset();

   // *** char lvl2_description
   const char * get_lvl2_description() const {return lvl2_description.Data();}
   void set_lvl2_description(const char *name) {lvl2_description = name; return;}

   unsigned int get_lvl2_version() const {return lvl2_version;}
   void set_lvl2_version(unsigned int ival) {lvl2_version = ival; return;}

   unsigned int get_lvl2_run_enable() const {return lvl2_run_enable;}
   void set_lvl2_run_enable(unsigned int ival) {lvl2_run_enable = ival; return;}

   unsigned int get_lvl2_reject_enable() const {return lvl2_reject_enable;}
   void set_lvl2_reject_enable(unsigned int ival) {lvl2_reject_enable = ival; return;}

   unsigned int get_lvl1_lvl2_reject_enable(unsigned int = 0) const { return lvl1_lvl2_reject_enable; }
   void set_lvl1_lvl2_reject_enable(unsigned int ival, unsigned int =0) { lvl1_lvl2_reject_enable = ival; }

   unsigned int get_lvl1_lvl2_force_accept(unsigned int i) const {return lvl1_lvl2_force_accept[i];}
   void set_lvl1_lvl2_force_accept(unsigned int ival, unsigned int i) {lvl1_lvl2_force_accept[i] = ival; return;}

   // *** lvl2_trig_name
   const char * get_lvl2_trig_name(unsigned int i) const {return lvl2_trig_name[i].Data();}
   void set_lvl2_trig_name(const char *name, unsigned int i) {lvl2_trig_name[i] = name; return;}

   unsigned int get_lvl2_trig_bit(unsigned int i) const {return lvl2_trig_bit[i];}
   void set_lvl2_trig_bit(unsigned int ival, unsigned int i) {lvl2_trig_bit[i] = ival; return;}

   unsigned int get_lvl2_trig_version(unsigned int i) const {return lvl2_trig_version[i];}
   void set_lvl2_trig_version(unsigned int ival, unsigned int i) {lvl2_trig_version[i] = ival; return;}

   unsigned int get_lvl2_lvl1_assoc(unsigned int i, unsigned int =0) const { return lvl2_lvl1_assoc[i]; }
   void set_lvl2_lvl1_assoc(unsigned int ival, unsigned int i,
                            unsigned int = 0) {
     lvl2_lvl1_assoc[i] = ival;
   }

 protected:

   void init();
   // the below fields assume a maximum of 64 level2 trigger algorithms
   // and a maximum of 32 level1 trigger bits (only one GL1-1 board)

   TString        lvl2_description;           // placeholder for future lvl2 trigger version name
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
                                           
   TString        lvl2_trig_name[64];         // list of the 64 level2 trigger names 
                                              // (eg. "SingleElectron")
   unsigned int   lvl2_trig_bit[64];          // index in bit map                          
   unsigned int   lvl2_trig_version[64];      // placeholder for future individual versions

   unsigned int   lvl2_lvl1_assoc[64];        // bit mask for each lvl2 trigger as to which lvl1
                                              // trigger bits it should be run on
   
   ClassDef(TrigRunLvl2v1,1)

};

#endif /*  __TRIGRUNLVL2V1_H */



