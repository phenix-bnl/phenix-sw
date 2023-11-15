//-----------------------------------------------------------------------------
//
//  Database definition of MPC Leakage Correction
//
//  $Id: PdbMpcLeakage.hh,v 1.1 2009/02/26 00:55:42 chiu Exp $
//-----------------------------------------------------------------------------
#ifndef __PDBMPCLEAKAGE_HH__
#define __PDBMPCLEAKAGE_HH__

#include <PdbCalChan.hh>
#include <iostream>
#include <ctime>
#include <stdint.h>
#include <phool.h>

class PdbMpcLeakage : public PdbCalChan {
public:
  PdbMpcLeakage();
  virtual ~PdbMpcLeakage() {}

  PdbMpcLeakage& operator = (const PdbMpcLeakage &p);

  void setX(uint16_t ix) { x = ix; }
  void setY(uint16_t iy) { y = iy; }
  void setLeakage(uint16_t l) { leakage = l; }
  void setLeakageError(uint16_t e) { dleakage = e; }

  uint16_t getX() const { return x; }
  uint16_t getY() const { return y; }
  uint16_t getLeakage() const { return leakage; }
  uint16_t getLeakageError() const { return dleakage; }

  virtual void reset();

  virtual void print() const;

private:

  uint16_t x;		// x position
  uint16_t y;		// y position
  uint16_t leakage;	// leakage
  uint16_t dleakage;	// leakage uncertainty

  ClassDef(PdbMpcLeakage,1);

};

#endif // __PDBMPCLEAKAGE_HH__ 

