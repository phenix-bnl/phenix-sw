//-----------------------------------------------------------------------------
//  Declaration of class PdbMutDallasGlink
//
//  Purpose: Store MuTr Dallas Chips information in Glink
//
//  Author: DongJo Kim(djkim@bnl.gov)  July 18/2001
//-----------------------------------------------------------------------------
#ifndef __PDBMUTDALLASGLINK_HH__
#define __PDBMUTDALLASGLINK_HH__

#include "PdbCalChan.hh"
#include "PHString.h"

#define ARMS_MAX  2
// South Arm has 10 Glink crates, North Arm has 18 crates
//see http://www.phenix.bnl.gov/WWW/muon/mutr_fee/FEE_Overview.doc
static const int MUT_Glinks_MAX = 18; 
// each Glink crate has 12 Dallas chips
static const int DALLAS_Glink_MAX = 12; 

class PdbMutDallasGlink : public PdbCalChan {

public:
  PdbMutDallasGlink();
  ~PdbMutDallasGlink();

  virtual void print() const;

  int getArm       ()     const {return ArmNum;} 
  int getGlink     ()     const {return GlinkNum;} 
  char* get_chipID     (int index)  {return      chipID[index];}
  float get_VDD        (int index) const {return         VDD[index];}
  float get_VAD        (int index) const {return         VAD[index];}
  float get_current    (int index) const {return     current[index];}
  float get_temperature(int index) const {return temperature[index];}

  void setArm           (int temp){ ArmNum=temp;}
  void setGlink         (int temp){ GlinkNum=temp;}
  void set_chipID       (int index,const PHString & chipid) {strcpy(chipID[index],chipid.getString());}
  void set_VDD         (int index,float temp) {        VDD[index]=temp;}
  void set_VAD         (int index,float temp) {        VAD[index]=temp;}
  void set_current     (int index,float temp) {    current[index]=temp;}
  void set_temperature (int index,float temp) {temperature[index]=temp;}


private:
  char chipID[DALLAS_Glink_MAX][17];
  float VDD[DALLAS_Glink_MAX];
  float VAD[DALLAS_Glink_MAX];
  float current[DALLAS_Glink_MAX];
  float temperature[DALLAS_Glink_MAX];
  int ArmNum; 
  int GlinkNum; 

  ClassDef(PdbMutDallasGlink,1);
};

#endif /* __PDBMUTDALLASGLINK_HH__ */
