#ifndef __loadso_cc
#define __loadso_cc
//=============================================================================
{
  // Loading PHOOL libraries
  gSystem->Load("/afs/rhic/phenix/software/sys/i386_redhat61/pro.3//Event/lib/libEvent.so");
  gSystem->Load("libphool.so");
  gSystem->Load("libWrappers.so");
  gSystem->Load("libPdbCal.so");
  gSystem->Load("libPhHistogramFactory.so");
  gSystem->Load("libuti.so");
  gSystem->Load("libdcm.so");
  gSystem->Load("libdgo.so");
  gSystem->Load("libphgeo.so");
  gSystem->Load("libPISARoot.so");
  gSystem->Load("libgea.so");
  // Additions for EMCAL
  gSystem->Load("libemcCalib.so") ;
  gSystem->Load("libemcOM.so") ;

  // Loading subsystem libraries
  gSystem->Load("libbbc.so");
  gSystem->Load("libzdc.so");
  gSystem->Load("libmvd.so");
  gSystem->Load("libpad.so");
  gSystem->Load("libemc.so");
  gSystem->Load("libtof.so");
  gSystem->Load("libcgl.so");
  gSystem->Load("libdch.so");
  gSystem->Load("libcrk.so");
  gSystem->Load("libtec.so");
  gSystem->Load("libmom.so");
  gSystem->Load("libheader.so");
}
//=============================================================================
#endif
//=============================================================================
