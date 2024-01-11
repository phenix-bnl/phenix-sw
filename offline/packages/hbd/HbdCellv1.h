#ifndef __HBDCELLV1_H_
#define __HBDCELLV1_H_

#include <PHObject.h>
#include <HbdCell.h>

#include <cstring>
class HbdCellv1 : public HbdCell
{

 public:

  HbdCellv1();
  HbdCellv1(HbdCellv1 *cell);  
  virtual ~HbdCellv1() {}

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

  // Get the data members
  int get_padnum () const { return padnum;}
  int get_sector () const { return sector;}
  int get_arm () const { return arm;}
  int get_side () const { return side;}
  int get_secchar (char *name) const { strcpy(name,secname); return 1;}
  float get_charge () const { return charge;}
  int get_clusterid()const {return clusterid;}

 protected:

  // Data member definition

  int padnum;   	  //pad number
  int sector;		  //sector
  int arm;                //arm
  int side;               //side
  char secname[100];      //sector name
  float charge;          // Charge deposit
  int clusterid;  //cluster id

  ClassDef(HbdCellv1,1)

};

#endif /* __HBDCELLV1_H_ */
