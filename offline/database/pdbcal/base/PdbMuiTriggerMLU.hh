//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Declaration of class PdbMuiTriggerMLU
//
//  Purpose: User defined storage class
//
//  Description: A Blue Logic Memory Look-Up module was used to trigger
//               2001 pp events in the MuID.  That map is stored here.
//
//  Author: rjnewby
//-----------------------------------------------------------------------------
#ifndef __PDBMUITRIGGERMLU_HH__
#define __PDBMUITRIGGERMLU_HH__

#include "PdbCalChan.hh"

class PdbMuiTriggerMLU : public PdbCalChan {
public:
	  PdbMuiTriggerMLU();
	  virtual ~PdbMuiTriggerMLU();

    int MLUindex() const { return _MLUindex; }
    int MLUword() const { return _MLUword; }

    void set_MLUindex(int newVal) { _MLUindex = newVal; }
    void set_MLUword(int newVal) { _MLUword = newVal; }
    
	  virtual void print() const;

private:

int _MLUindex;
int _MLUword;

  ClassDef(PdbMuiTriggerMLU,1);
};

#endif /* __PDBMUITRIGGERMLU_HH__ */
