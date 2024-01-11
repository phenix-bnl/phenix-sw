#ifndef __EMCCALIBTOWERV1_H__
#define __EMCCALIBTOWERV1_H__

#include "EmcCalibTower.h"
#include <iostream>

class dEmcCalibTowerWrapper;
class TClonesArray;

/** (OLD) Kind of interim object between STAF times and Fun4All times.
Relevant object is now emcTowerContainer / emcTowerContent
@ingroup deprecated 
*/

class EmcCalibTowerv1 : public EmcCalibTower
{
 public:
  EmcCalibTowerv1();
  virtual ~EmcCalibTowerv1();

  void Reset();
  int isValid() const;
  void identify(std::ostream &os = std::cout) const;

  void FillFromWrapper(dEmcCalibTowerWrapper *wrap);

  unsigned int get_EmcNTower() const {return  EmcNTower;}
  void set_EmcNTower(const unsigned int ntower) {EmcNTower=ntower; return;}

  int set_TClonesArraySize(const unsigned int ntower);
  void AddEmcTower(const unsigned int itower);

  // get functions for "compressed" index
  short get_arm(const unsigned int itower) const;
  short get_sector(const unsigned int itower) const;
  short get_ind(const unsigned int itower, const short i) const;

  short get_id(const unsigned int iclus) const;
  void set_id(const unsigned int iclus, const short ival);

  short get_type(const unsigned int iclus) const;
  void set_type(const unsigned int iclus, const short ival);


  int get_deadmap(const unsigned int itower) const;
  void set_deadmap(const unsigned int itower, const int ival);

  int get_hwkey(const unsigned int iclus) const;
  void set_hwkey(const unsigned int iclus, const int ival);

  int get_index(const unsigned int itower) const;
  void set_index(const unsigned int itower, const int ival);

  int get_swkey(const unsigned int iclus) const;
  void set_swkey(const unsigned int iclus, const int ival);

  int get_warnmap(const unsigned int itower) const;
  void set_warnmap(const unsigned int itower, const int ival);

  float get_adc(const unsigned int itower) const;
  void set_adc(const unsigned int itower, const float rval);

  float get_ecal(const unsigned int itower) const;
  void set_ecal(const unsigned int itower, const float rval);

  float get_tac(const unsigned int itower) const;
  void set_tac(const unsigned int itower, const float rval);

  float get_tof(const unsigned int itower) const;
  void set_tof(const unsigned int itower, const float rval);

 protected:
  TClonesArray *GetEmcTower() const {return EmcTower;}
  unsigned int EmcNTower;
  TClonesArray *EmcTower;

  ClassDef(EmcCalibTowerv1,1)

};

#endif /*__EMCCALIBTOWERV1_H__*/
