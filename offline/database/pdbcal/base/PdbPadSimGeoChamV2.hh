//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 2000
//
//  Declaration of class PdbPadSimGeoChamV2
//
//  Purpose: Store info of pad chamber sectors into database
//           previously was in padGeometry.txt file
//
//  Author: Debsankar Mukhopadhyay
//----------------------------------------------------------------------------
#ifndef __PDBPADSIMGEOCHAMV2_HH__
#define __PDBPADSIMGEOCHAMV2_HH__

#include "PdbCalChan.hh"

class PdbPadSimGeoChamV2 : public PdbCalChan {

private:

int  pc,arm, sect;
double p0[3],p1[3],p2[3],p3[3];

public:
  PdbPadSimGeoChamV2();
  virtual ~PdbPadSimGeoChamV2();
  PdbPadSimGeoChamV2(const PdbPadSimGeoChamV2 &rhs);

  int  get_pc();
  int  get_arm();
  int  get_sect();
  void set_pc(int ipc);
  void set_arm(int iarm);
  void set_sect(int isect);

  void set_point1(int i,double val);
  void set_point2(int i,double val);
  void set_point3(int i,double val);
  void set_point4(int i,double val);

 double get_point1(int i);
 double get_point2(int i);
 double get_point3(int i);
 double get_point4(int i);

  virtual  void print() const;
  void zero();

  ClassDef(PdbPadSimGeoChamV2,1);
};

#endif /* __PDBPADSIMGEOCHAMV2_HH__ */










