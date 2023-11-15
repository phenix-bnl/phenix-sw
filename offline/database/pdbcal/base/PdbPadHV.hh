//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 2000
//
//  Declaration of class PdbPadHV
//
//  Purpose: Store PC HV status 
//
//  Description:
//
//  Author: silvermy
//-----------------------------------------------------------------------------
#ifndef __PDBPADHV_HH__
#define __PDBPADHV_HH__

#include "PdbCalChan.hh"

class PdbPadHV : public PdbCalChan {
public:
  PdbPadHV();
  virtual ~PdbPadHV();

  virtual void print() const;

  virtual short get_PC() const { return pc;}
  virtual void set_PC(short si) { pc=si;}

  virtual short get_Arm() const { return arm;}
  virtual void set_Arm(short si) { arm=si;}

  virtual bool get_HVStatusSect(short si) const;
  virtual bool set_HVStatusSect(short si, bool b);

  virtual bool set_HVStatusAll(bool b[32]);

  virtual short get_NumOkHVSect() const;
  virtual short get_NumNotOkHVSect() const;

private:
   void zero();  

private:

  short pc; // 0,1,2 for PC1, PC3 and PC3 respectively..
  short arm; // 0 for East and 1 for West

  bool hvstatus[32]; // true means ok, false means not ok

  ClassDef(PdbPadHV,1);
};

#endif /* __PDBPADHV_HH__ */
