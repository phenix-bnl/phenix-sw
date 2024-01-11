#ifndef __TECHITMAPENTRY_H_
#define __TECHITMAPENTRY_H_

#include <iostream>

class TecHitMapEntry
{
 public:
  TecHitMapEntry();
  virtual ~TecHitMapEntry() {}

  virtual void identify(std::ostream &os=std::cout) const;

  // Here are the very explicit set routines...
  void set_id(const short int i) {id = i;}
  void set_index(const short int i) {index = i;}
  void set_wire(const short int i) {wire = i;}
  void set_avgtime(const short int i) {avgtime = i;}
  void set_ntimebins(const short int i) {ntimebins = i;}
  void set_charge(const float val) {charge = val;}

  // Here are the very explicit "get" routines...
  short int get_id() const {return id;}
  short int get_index() const {return index;}
  short int get_wire() const {return wire;}
  short int get_avgtime() const {return avgtime;}
  short int get_ntimebins() const {return ntimebins;}
  float get_charge() const {return charge;}

 protected:
  short int id;
  short int index;
  short int wire;
  short int ntimebins;
  short int avgtime;
  float charge;

};

#endif /* TECHITMAPENTRY */
