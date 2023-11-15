//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 2000
//
//  Declaration of class PdbPadSimGeoChamV1
//
//  Purpose: Store info of pad chamber sectors into database
//           previously was in padGeometry.txt file
//
//  Author: Indrani Ojha
//----------------------------------------------------------------------------
#ifndef __PDBPADSIMGEOCHAMV1_HH__
#define __PDBPADSIMGEOCHAMV1_HH__

#include "PHPanel.h"
#include "PHPoint.h"
#include "PdbCalChan.hh"

#define CGLMAXSECTPERARM 4
#define PC1MAXSECTPERARM 8

class PdbPadSimGeoChamV1 : public PdbCalChan {

private:

int pc, arm, sect;
PHPanel pc1Sectors[2][PC1MAXSECTPERARM], pc2Sectors[2][CGLMAXSECTPERARM], pc3Sectors[2][CGLMAXSECTPERARM];


public:
  PdbPadSimGeoChamV1();
  virtual ~PdbPadSimGeoChamV1();
  PdbPadSimGeoChamV1(const PdbPadSimGeoChamV1 &rhs);

  int  get_pc();
  int  get_arm();
  int  get_sect();
  void set_pc(int ipc);
  void set_arm(int iarm);
  void set_sect(int isect);

  PHPanel get_pc1Sectors();
  PHPanel get_pc2Sectors();
  PHPanel get_pc3Sectors();

  void set_pc1Sectors(PHPanel pc1SectorfrompadDetGeo);
  void set_pc2Sectors(PHPanel pc2SectorfrompadDetGeo);
  void set_pc3Sectors(PHPanel pc3SectorfrompadDetGeo);

  virtual  void print() const;
  void zero();

  ClassDef(PdbPadSimGeoChamV1,1);
};

#endif /* __PDBPADSIMGEOCHAM_HH__ */










