#ifndef __PADSNGLCLUSTERV2_H
#define __PADSNGLCLUSTERV2_H

#include "PadSnglCluster.h"

class PadSnglClusterv2: public PadSnglCluster
{
 public:
  PadSnglClusterv2();
  virtual ~PadSnglClusterv2() {}

  short get_arm() const;
  short get_cell() const {return cell;}
  short get_id() const {return id;}
  short get_sector() const;
  short get_type() const {return type;}
  short get_wire() const {return wire;}

  float get_dxyz(const int i) const {return dxyz[i];}
  float get_xyz(const int i) const {return padxyz[i];}

  void set_arm(const short iarm);
  void set_cell(const short icell) {cell = icell; return;}
  void set_id(const short iid) {id = iid; return;}
  void set_sector(const short isec);
  void set_type(const short itype) {type = itype; return;}
  void set_wire(const short iwire) {wire = iwire; return;}
  void set_dxyz(const float rval, const short int i) {dxyz[i] = rval; return;}
  void set_xyz(const float rval, const short int i) {padxyz[i] = rval; return;}

 protected:
  short cell;
  short id;
  short index;
  short type;
  short wire;
  float dxyz[3];
  float padxyz[3];

  ClassDef(PadSnglClusterv2,1)
};

#endif
