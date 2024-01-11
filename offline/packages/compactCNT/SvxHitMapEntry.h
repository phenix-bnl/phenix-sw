#ifndef __SVXHITMAPENTRY_H_
#define __SVXHITMAPENTRY_H_

#include <iostream>

class SvxHitMapEntry
{
 public:
  SvxHitMapEntry();
  virtual ~SvxHitMapEntry() {}

  virtual void identify(std::ostream &os=std::cout) const;

  void set_id(const short int i) 	{id = i;}
  void set_adcandsize(const short int val) 	{adcandsize = val;}
  void set_ncold(const short int val){ ncold = val; }
  void set_nhot(const short int val){ nhot = val; }
  void set_x(const float val) 		{x = val;}
  void set_y(const float val) 		{y = val;}
  void set_z(const float val) 		{z = val;}

  short int get_id()	const {return id;}
  short int get_adcandsize() 	const {return adcandsize;}
  short int get_ncold() const {return ncold;}
  short int get_nhot() const {return nhot;}

  //---
  float get_x() 	const {return x;}
  float get_y() 	const {return y;}
  float get_z() 	const {return z;}

  //---
/*   short int get_x() 	const {return x;} */
/*   short int get_y() 	const {return y;} */
/*   short int get_z() 	const {return z;} */

 protected:
  short int id;
  short int adcandsize;
  short int ncold;
  short int nhot;
  //---
  float x;
  float y;
  float z;

  //---
/*   short int x; */
/*   short int y; */
/*   short int z; */


};

#endif 
