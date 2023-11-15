#ifndef __PDBMPCMAP_HH__
#define __PDBMPCMAP_HH__

#include "PdbCalChan.hh"

class PdbMpcMap : public PdbCalChan {
 public:
  PdbMpcMap();
  virtual ~PdbMpcMap(){}
  
  void set_fee576(const short ich) { fee576 = ich; }
  void set_driver(const short idrive) { driver = idrive; }
  void set_gridx(const short ix) { gridx = ix; }
  void set_gridy(const short iy) { gridy = iy; }
  void set_x(const float ix) { x = ix; }
  void set_y(const float iy) { y = iy; }
  void set_z(const float iz) { z = iz; }
  void set(const short ich, const short idrive,
           const short igridx, const short igridy,
           const float ix, const float iy, const float iz) {
    fee576 = ich;
    driver = idrive;
    gridx = igridx;
    gridy = igridy;
    x = ix;
    y = iy;
    z = iz;
  }
  
  short get_fee576() const { return fee576; }
  short get_driver() const { return driver; }
  short get_gridx() const { return gridx; }
  short get_gridy() const { return gridy; }
  float get_x() const { return x; }
  float get_y() const { return y; }
  float get_z() const { return z; }
  
  virtual void Reset();
  virtual void print() const;
  
private:
  short fee576;	// FEE Channel
  short driver;	// Driver Board
  short gridx;	// x grid position
  short gridy;
  float x;	// position of front face of crystal
  float y;	// in PHENIX coords (cm)
  float z;

  ClassDef(PdbMpcMap,1);

};

#endif /* __PDBMPCMAP_HH__ */
