//-------------------------------------------------------------------------
// 
// Package: offline/packages/emc
// 
// Copyright (C) PHENIX collaboration, 2000 
//
// Implementation of class : mEmcTOFCorrModule
//
// Ken Oyama, CNS, University of Tokyo.
// H.Torii
//-------------------------------------------------------------------------
#include <stdio.h>
#include <cmath>
#include <cstdlib>
#include <iostream>
#include <fstream>


#include "EmcTOFCorr.hh"
EmcTOFCorr* EmcTOFCorr::_instance = NULL;

//-------------------------------------------------------------------------
EmcTOFCorr::EmcTOFCorr()
{ 
  // Default Constructor
  fVerbose = 0;

  // Initialize run-by-run correction table to have no data.
  _pbsc_nruns = 0;
  _pbgl_nruns = 0;

  // Initialize tower by-tower tables with default value.
  for( int arm=0; arm<2; arm++ )
    for( int sec=0; sec<4; sec++ )
      for( int y=0; y<48; y++ )
        for( int z=0; z<96; z++ )
	  _tbt_corr[arm][sec][y][z] = MEMCTOFCORR_DEF_TBT_CORR;
}

//-------------------------------------------------------------------------
EmcTOFCorr::~EmcTOFCorr()
{


}

//-------------------------------------------------------------------------
EmcTOFCorr* EmcTOFCorr::instance( char *pbsc_file, char *pbgl_file, char *tbt_file )
{ 
  if( _instance )
    delete _instance;
  _instance = new EmcTOFCorr();

  _instance->read_rbrsc_file( pbsc_file);   // Read PbSc run-by-run correction file.
  _instance->read_rbrgl_file( pbgl_file);   // Read PbGl run-by-run correction file.
  _instance->read_tbt_file( tbt_file  );    // Read tower-by-tower correction file.

  return _instance;
}

//-------------------------------------------------------------------------
EmcTOFCorr* EmcTOFCorr::instance()
{ 
  if( !_instance ){
    cerr<<" EmcTOFCorr has not been initialized. Initialized first please"<<endl;
    exit(0);
  }
  return _instance;
}

//-------------------------------------------------------------------------
//
// It reads run-by-run correction data table. Called by instance.
//
void EmcTOFCorr::read_rbrsc_file( char *file)
{
  int suspicious_line = 0;
  float prev_corr = MEMCTOFCORR_DEF_RBR_CORR;

  ifstream fin( file );
  int nruns = 0;
  printf("EmcTOFCorr::read_rbrsc_file: Opening file <%s>\n", file);
  ifstream cfile( file );
  if( cfile == 0 ) {
    printf("EmcTOFCorr::read_rbrsc_file: Error: "
	   "Could not open file <%s>. Do not apply correction.\n", file);
    return;
  }
  while( cfile ){
    int run, ndf, in_range, nent;
    float corr, chi2;
    cfile >> run >> corr >> chi2 >> ndf >> nent >> in_range;

    if( in_range==0 || nent<100 || corr==0.0 ) {
      suspicious_line++;
      continue;
    }
    _pbsc_runs[ nruns ] = run;
    _pbsc_corr[ nruns ] = corr;
    nruns++;
    if( nruns >= MEMCTOFCORR_N_MAX_RUNS ) {
      printf("EmcTOFCorr::read_rbrgl_file: \n");
      printf("                                  Too many lines.\n");
      printf("                                  Probably you increase the maximum run numbers and re-compile me.\n");
      printf("                                  Following lines will be ignored.(%d)\n",MEMCTOFCORR_N_MAX_RUNS -1 );
      break;
    }
  }
  printf("EmcTOFCorr::read_rbrsc_file: %d lines have been read.(%d lines were rejected)\n", nruns,suspicious_line);
  cfile.close();
  _pbsc_nruns = nruns;
}

//-------------------------------------------------------------------------
//
// It reads run-by-run correction data table. Called by instance.
//
void EmcTOFCorr::read_rbrgl_file( char *file)
{
  int suspicious_line = 0;
  float prev_corr = MEMCTOFCORR_DEF_RBR_CORR;
  ifstream fin( file );
  int nruns = 0;
  printf("EmcTOFCorr::read_rbrgl_file: Opening file <%s>\n", file);
  ifstream cfile( file );
  if( cfile == 0 ) {
    printf("EmcTOFCorr::read_rbrgl_file: Error: "
	   "Could not open file <%s>. Do not apply correction.\n", file);
    return;
  }
  while( cfile ){
    int run, ndf, in_range, nent;
    float corr, chi2;
    cfile >> run >> corr >> chi2 >> ndf >> nent >> in_range;

    if( in_range==0 || nent<100 || corr==0.0 ) {
      suspicious_line ++;
      continue;
    }
    _pbgl_runs[ nruns ] = run;
    _pbgl_corr[ nruns ] = corr;
    nruns++;
    if( nruns >= MEMCTOFCORR_N_MAX_RUNS ) {
      printf("EmcTOFCorr::read_rbrgl_file: \n");
      printf("                                  Too many lines.\n");
      printf("                                  Probably you increase the maximum run numbers and re-compile me.\n");
      printf("                                  Following lines will be ignored.(%d)\n",MEMCTOFCORR_N_MAX_RUNS -1 );
      break;
    }
  }
  printf("EmcTOFCorr::read_rbrgl_file: %d lines have been read. (%d lines were rejected)\n", nruns,suspicious_line);
  cfile.close();
  _pbgl_nruns = nruns;
}


//-------------------------------------------------------------------------
void EmcTOFCorr::read_tbt_file( char *file )
//
// It reads tower-by-tower correction data table. Called by instance.
//
{
  int line = 0;
  int reject = 0;
  printf("EmcTOFCorr::read_tbt_file: Opening Tower-by-Tower TOF Correction file <%s>\n", file);
  ifstream cfile( file );
  if( cfile == 0 ) {
    printf("EmcTOFCorr::read_tbt_file: Error: Failed to open file. "
	   "Default value = %f is used for all tower.\n", MEMCTOFCORR_DEF_TBT_CORR );
    return;
  }
  while( cfile ){
    int y,z,s,a;
    float corr, reso;
    float tmp;
    float itmp;
    line++;
    cfile >> y >> z >> s >> a >> corr >> reso;
    cfile >> tmp >> itmp >> itmp >> itmp;
    if( (a<0 || a>1) || (s<0 || s>3) || (y<0 || y>47) || (z<0 || z>95) ) {
      printf("EmcTOFCorr::read_tbt_file: Strange ARM/SEC/Y/Z number at line %d.\n", line );
      reject++;
    } else if (   !((a == 1) && (s < 2)) && ( (y>35) || (z>71) )  ) {
      printf("EmcTOFCorr::read_tbt_file: Hmm. it is PbSc but indexes are strange.... at line %d.\n", line );
      reject++;
    } else {
      _tbt_corr[a][s][y][z] = corr;
    }
  }
  printf("EmcTOFCorr::read_tbt_file: %d lines have been read. (%d lines were rejected.)\n", line, reject );
  cfile.close();
}

//-------------------------------------------------------------------------
//
// Find run number/index in the run-by-run table and return total correction includes tower-by-tower correction.
//
float EmcTOFCorr::get_correction( int run, int arm, int sec, int y, int z )
{
  static int   prev_run = -1;
  static float rbr_corr_pbsc = MEMCTOFCORR_DEF_RBR_CORR;
  static float rbr_corr_pbgl = MEMCTOFCORR_DEF_RBR_CORR;
  float total_corr = 0;
  // If the run changed, re-fill the prev_rbr_corr_pb??.
  if( run != prev_run ) {
    printf("EmcTOFCorr::get_correction: New Run : %d, finding the correction table. \n", run);
    int i;
    for( i = 0 ; i < _pbsc_nruns-1 ; i++) {
      if( _pbsc_runs[i] <= run && _pbsc_runs[i+1] > run ) {
	rbr_corr_pbsc = _pbsc_corr[i];
	break;
      }
    }
    if( i == (_pbsc_nruns - 1) ) rbr_corr_pbsc = _pbsc_corr[i];  // If over run.
    for( i = 0 ; i < _pbgl_nruns-1 ; i++) {
      if( _pbgl_runs[i] <= run && _pbgl_runs[i+1] > run ) {
	rbr_corr_pbgl = _pbgl_corr[i];
	break;
      }
    }
    if( i == (_pbgl_nruns - 1) ) rbr_corr_pbgl = _pbgl_corr[i];  // If over run.
    prev_run = run;
    printf("EmcTOFCorr::get_correction:   New Values : PbSc: %f, PbGl: %f\n",
	   rbr_corr_pbsc, rbr_corr_pbgl);
  }
  if( !((arm == 1) && (sec < 2)) ) { // PbSc
    return rbr_corr_pbsc + _tbt_corr[arm][sec][y][z];
  } else {
    return rbr_corr_pbgl + _tbt_corr[arm][sec][y][z];
  }

}


// EOF
