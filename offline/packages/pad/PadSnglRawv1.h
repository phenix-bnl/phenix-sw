#ifndef __PADSNGLRAWV1_H
#define __PADSNGLRAWV1_H

#include "PadSnglRaw.h"

class PadSnglRawv1: public PadSnglRaw
{
 public:
  PadSnglRawv1();
  virtual ~PadSnglRawv1() {}

  short get_arm() const;
  short get_id() const {return id;}
  short get_padtype() const {return padtype;}
  short get_padx() const {return padx;}
  short get_padz() const {return padz;}
  short get_sector() const;
  short get_side() const;

  void set_arm(const short iarm);
  void set_id(const short iid) {id = iid; return;}
  void set_padtype(const short ipadtype) {padtype = ipadtype; return;}
  void set_padx(const short ipadx) {padx = ipadx; return;}
  void set_padz(const short ipadz) {padz = ipadz; return;}
  void set_sector(const short isec);
  void set_side(const short iside);

 protected:
  short id;
  unsigned short index;
  short padtype;
  short padx;
  short padz;

  ClassDef(PadSnglRawv1,1)
};

#endif
