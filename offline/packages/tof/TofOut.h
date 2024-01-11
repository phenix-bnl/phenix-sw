#ifndef __TOFOUT_H
#define __TOFOUT_H

#include <iostream>
#include "phool.h"
#include "PHObject.h"

class dTofReconstructedWrapper;

// with the following we get the line number of the virtual function we called with PHWHERE
#define TOF_VIRTUAL_WARNING std::cout << PHWHERE << "using virtual function, doing nothing" << std::endl


class TofOut: public PHObject
{

 public:

  TofOut() {}
  virtual ~TofOut() {}

  virtual void identify(std::ostream& os = std::cout) const 
    {
      os << "virtual TofOut object";
      return;
    }

   virtual int isValid() const 
    {
      std::cout << PHWHERE << "isValid() not implemented by daughter class" << std::endl;
      return 0;
    }

   virtual void Reset() 
    {
      std::cout << PHWHERE << "ERROR: Reset() not implemented by daughter class" << std::endl;
      return;
    }

  virtual void  FillFromWrapper(dTofReconstructedWrapper *wrap);

  virtual unsigned int set_TClonesArraySize(const unsigned int nhit) {TOF_VIRTUAL_WARNING; return 0;}
  virtual unsigned int get_TofNHit() const {TOF_VIRTUAL_WARNING; return 0;}
  virtual void set_TofNHit(const unsigned int nhit) {TOF_VIRTUAL_WARNING; return;}

  virtual void AddTofHit(const unsigned int ihit) {TOF_VIRTUAL_WARNING; return;}

 virtual short get_id(const unsigned int ihit) const {TOF_VIRTUAL_WARNING; return 0;}
 virtual void set_id(const unsigned int ihit, const short ival) {TOF_VIRTUAL_WARNING; return;}

 virtual short get_panel(const unsigned int ihit) const {TOF_VIRTUAL_WARNING; return 0;}
 virtual void set_panel(const unsigned int ihit, const short ival) {TOF_VIRTUAL_WARNING; return;}

 virtual short get_sector(const unsigned int ihit) const {TOF_VIRTUAL_WARNING; return 0;}
 virtual void set_sector(const unsigned int ihit, const short ival) {TOF_VIRTUAL_WARNING; return;}

 virtual short get_side(const unsigned int ihit) const {TOF_VIRTUAL_WARNING; return 0;}
 virtual void set_side(const unsigned int ihit, const short ival) {TOF_VIRTUAL_WARNING; return;}

 virtual short get_slat(const unsigned int ihit) const {TOF_VIRTUAL_WARNING; return 0;}
 virtual void set_slat(const unsigned int ihit, const short ival) {TOF_VIRTUAL_WARNING; return;}

 virtual short get_slatid(const unsigned int ihit) const {TOF_VIRTUAL_WARNING; return 0;}
 virtual void set_slatid(const unsigned int ihit, const short ival) {TOF_VIRTUAL_WARNING; return;}


  virtual short get_qvc(const unsigned int ihit, const short i) const {TOF_VIRTUAL_WARNING; return 0;}
  virtual void set_qvc(const unsigned int ihit, const short i, const short ival) {TOF_VIRTUAL_WARNING; return;}

  virtual short get_tvc(const unsigned int ihit, const short i) const {TOF_VIRTUAL_WARNING; return 0;}
  virtual void set_tvc(const unsigned int ihit, const short i, const short ival) {TOF_VIRTUAL_WARNING; return;}

  virtual float get_eloss(const unsigned int ihit) const {TOF_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_eloss(const unsigned int ihit, const float rval) {TOF_VIRTUAL_WARNING; return;}

  virtual float get_eloss_err(const unsigned int ihit) const {TOF_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_eloss_err(const unsigned int ihit, const float rval) {TOF_VIRTUAL_WARNING; return;}

  virtual float get_tof(const unsigned int ihit) const {TOF_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_tof(const unsigned int ihit, const float rval) {TOF_VIRTUAL_WARNING; return;}

  virtual float get_tof_err(const unsigned int ihit) const {TOF_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_tof_err(const unsigned int ihit, const float rval) {TOF_VIRTUAL_WARNING; return;}

  virtual float get_xtof(const unsigned int ihit, const short ival) const {TOF_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_xtof(const unsigned int ihit, const short ival, const float rval) {TOF_VIRTUAL_WARNING; return;}

  virtual float get_xtof_err(const unsigned int ihit, const short ival) const {TOF_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_xtof_err(const unsigned int ihit, const short ival, const float rval) {TOF_VIRTUAL_WARNING; return;}

  virtual float get_tdiff(const unsigned int ihit) const {TOF_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_tdiff(const unsigned int ihit, const float rval) {TOF_VIRTUAL_WARNING; return;}
  
  ClassDef(TofOut,1)

};

#undef TOF_VIRTUAL_WARNING

#endif /* __TOFOUT_H */
