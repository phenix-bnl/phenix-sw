#ifndef __ACCHITMAPENTRY_H_
#define __ACCHITMAPENTRY_H_

#include <PHObject.h>
#include <iostream>

class AccHitMapEntry : public PHObject
{
 public:
  AccHitMapEntry();
  virtual ~AccHitMapEntry() {}

  virtual void identify(std::ostream &os=std::cout) const;

  // Here are the very explicit set routines...
  //void set_id(const short int i) {id = i;};
  void set_hitid(const short val) {hitid = val;}
  void set_hitconfig(const short val) {hitconfig = val;}
  void set_ph1(const int index, const float val) {if(index >-1 && index <4) ph1[index] = val;}
  void set_ph2(const int index, const float val) {if(index >-1 && index <4) ph2[index] = val;}
  void set_t1(const int index, const float val) {if(index >-1 && index <4) t1[index] = val;}
  void set_t2(const int index, const float val) {if(index >-1 && index <4) t2[index] = val;}

  // Here are the very explicit "get" routines...
  //short int get_id() const {return id;}
  short int get_hitid() const {return hitid;};
  short int get_hitconfig() const {return hitconfig;}
  float get_ph1(const int index) const { if(index >-1 && index <4) return ph1[index]; else return -9999.0; }
  float get_ph2(const int index) const { if(index >-1 && index <4) return ph2[index]; else return -9999.0; }
  float get_t1(const int index) const { if(index >-1 && index <4) return t1[index]; else return -9999.0; }
  float get_t2(const int index) const { if(index >-1 && index <4) return t2[index]; else return -9999.0;
  }
  
 protected:
  //short int id;
  short int hitid;
  short int hitconfig;
  float ph1[4];
  float ph2[4];
  float t1[4];
  float t2[4];

  ClassDef(AccHitMapEntry,1)
};

#endif /* ACCHITMAPENTRY */
