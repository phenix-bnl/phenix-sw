#ifndef __PADSNGLCLUSTER_H
#define __PADSNGLCLUSTER_H

#include "PHObject.h"

class PadSnglCluster: public PHObject
{
 public:
  PadSnglCluster() {}
  virtual ~PadSnglCluster() {}

  virtual short get_arm() const {return -999;}
  virtual short get_cell() const {return -999;}
  virtual short get_id() const {return -999;}
  virtual short get_sector() const {return -999;}
  virtual short get_type() const {return -999;}
  virtual short get_wire() const {return -999;}

  virtual float get_dxyz(const int i) const;
  virtual float get_xyz(const int i) const;

  virtual void set_arm(const short iarm) {return;}
  virtual void set_cell(const short icell) {return;}
  virtual void set_id(const short iid) {return;}
  virtual void set_sector(const short isec) {return;}
  virtual void set_type(const short itype) {return;}
  virtual void set_wire(const short iwire) {return;}
  virtual void set_dxyz(const float rval, const short int i) {return;}
  virtual void set_xyz(const float rval, const short int i) {return;}

 protected:
  void virtual_warning(const char *funcname) const;
  ClassDef(PadSnglCluster,1)
};

#endif
