#ifndef __HBDCELLV2_H_
#define __HBDCELLV2_H_

#include <PHObject.h>
#include <HbdCell.h>

#include <cstring>
class HbdCellv2 : public HbdCell
{

 public:

  HbdCellv2();
  HbdCellv2(HbdCellv2 *cell);  
  virtual ~HbdCellv2() {}

  // The "standard PHObject response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;

  // Set the data members
  void set_padnum (const int val) {padnum = val; return;}
  void set_sector (const int val) {sector = val; return;}
  void set_arm (const int val) {arm = val; return;}
  void set_side (const int val) {side = val; return;}
  void set_secchar (const char *name) {strcpy(secname,name); return;}
  void set_charge (const float val) {charge = val; return;}
  void set_clusterid(const int val)
  {clusterid=val;
  return;
  }
  void set_s0 (const int val) {s0 = val; return;}
  void set_s1 (const int val) {s1 = val; return;}
  void set_s2 (const int val) {s2 = val; return;}


  // Get the data members
  int get_padnum () const { return padnum;}
  int get_sector () const { return sector;}
  int get_arm () const { return arm;}
  int get_side () const { return side;}
  int get_secchar (char *name) const { strcpy(name,secname); return 1;}
  float get_charge () const { return charge;}
  int get_clusterid()const {return clusterid;}
  int get_s0() const {return s0;}
  int get_s1() const {return s1;}
  int get_s2() const {return s2;}


 protected:

  // Data member definition

  int padnum;   	  //pad number
  int sector;		  //sector
  int arm;                //arm
  int side;               //side
  char secname[100];      //sector name
  int s0;
  int s1;
  int s2;
  float charge;          // Charge deposit
  int clusterid;  //cluster id

  ClassDef(HbdCellv2,1)

};

#endif /* __HBDCELLV2_H_ */
