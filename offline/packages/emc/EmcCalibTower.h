#ifndef __EMCCALIBTOWER_H__
#define __EMCCALIBTOWER_H__

#include "phool.h"
#include "PHObject.h"

#include <iostream>

class dEmcCalibTowerWrapper;

// with the following we get the line number of the virtual function we called with PHWHERE
#define EMC_VIRTUAL_WARNING std::cout << PHWHERE << "using virtual function, doing nothing" << std::endl

/** (OLD) Kind of interim object between STAF times and Fun4All times.
Relevant object is now emcTowerContainer / emcTowerContent
@ingroup deprecated 
*/

class EmcCalibTower : public PHObject
{
 public:
  virtual ~EmcCalibTower() {}

  virtual void Reset()
    {
      std::cout << PHWHERE << "ERROR: Reset() not implemented by daughter function" << std::endl;
      return;
    }

  virtual int isValid() const
    {
      std::cout << PHWHERE << "isValid() not implemented by daughter function" << std::endl;
      return 0;
    }

  virtual void identify(std::ostream &os=std::cout) const
    {
      os << "identify yourself: virtual EmcCalibTower object" << std::endl;
      return;
    }


  virtual void FillFromWrapper(dEmcCalibTowerWrapper *wrap) {EMC_VIRTUAL_WARNING; return;}

  virtual unsigned int get_EmcNTower() const {EMC_VIRTUAL_WARNING; return 0;}
  virtual void set_EmcNTower(const unsigned int nclus) {EMC_VIRTUAL_WARNING; return;}

  virtual int set_TClonesArraySize(const unsigned int nclus) {EMC_VIRTUAL_WARNING; return 0;}
  virtual void AddEmcTower(const unsigned int iclus) {EMC_VIRTUAL_WARNING; return;}

  virtual short get_arm(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999;}

  virtual short get_id(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999;}
  virtual void set_id(const unsigned int iclus, const short ival) {EMC_VIRTUAL_WARNING; return;}

  virtual short get_sector(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999;}

  virtual short get_type(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999;}
  virtual void set_type(const unsigned int iclus, const short ival) {EMC_VIRTUAL_WARNING; return;}

  virtual short get_ind(const unsigned int iclus, const short i) const {EMC_VIRTUAL_WARNING; return -9999;}

  virtual int get_deadmap(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999;}
  virtual void set_deadmap(const unsigned int iclus, const int ival) {EMC_VIRTUAL_WARNING; return;}

  virtual int get_hwkey(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999;}
  virtual void set_hwkey(const unsigned int iclus, const int ival) {EMC_VIRTUAL_WARNING; return;}

  virtual int get_index(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999;}
  virtual void set_index(const unsigned int iclus, const int ival) {EMC_VIRTUAL_WARNING; return;}

  virtual int get_swkey(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999;}
  virtual void set_swkey(const unsigned int iclus, const int ival) {EMC_VIRTUAL_WARNING; return;}

  virtual int get_warnmap(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999;}
  virtual void set_warnmap(const unsigned int iclus, const int ival) {EMC_VIRTUAL_WARNING; return;}


  virtual float get_adc(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_adc(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_ecal(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_ecal(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_tac(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_tac(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_tof(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_tof(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  ClassDef(EmcCalibTower,1)

};

#undef EMC_VIRTUAL_WARNING

#endif /*__EMCCALIBTOWER_H__*/
