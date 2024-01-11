// $Id: MILLEPEDE.h,v 1.7 2014/10/01 22:01:38 jinhuang Exp $
#ifndef MILLEPEDE_H
#define MILLEPEDE_H

/*!
   \file    MILLEPEDE.h
   \brief   static variables for C++ to Fortran interface
   \author  Hugo Pereira/Catherine Silvestre
   \version $Revision: 1.7 $
   \date    $Date: 2014/10/01 22:01:38 $
*/

extern "C" {

	//! initialize global alignment parameter arrays
  extern int initgl_(int*,int*,int*,int*);
	
	//! set global alignment parameters limit
  extern int parsig_(int*,float*);
	
	//! add constraint between global aligment parameters
  extern int constf_(float*,float*);
	
	//! initialize fortran output logical units
  extern int initun_(int*,float*, char*, int);
	
	//! set all arrays to zero
  extern int zerloc_(float*,float*);
	
	//! unused
  extern int gener_(float*,float*,float*,float*,float*,float*,float*);
	
	//! track local equations
  extern int equloc_(float*,float*,float*,float*);
	
	//! local track parameters fit
  extern int fitloc_();
	
	//! global alignment parameters fit
  extern int fitglo_(float*);
	
	//! ???
  extern int parglo_(float*);
	
	//! ???
  extern int prtglo_(int*);
	
	//! retrieve error for a given parameter
  extern float errpar_(int*);
	
}

//! static variables for C++ to Fortran interface
namespace MILLEPEDE 
{

	//! verbosity
	enum Verbosity { NONE, SOME, ALOT, MAX };
	
  //! root buffer max size before flushing to file
  static const int BUFFER_SIZE = 32000;
  
  //! max number of detectors
  static const int NPLAN = 8000;
  
  /*! \brief
  	number of parameters/track
    0: x ( perp to the beam horizontal) 
    1: tan(teta_x) ( in Ox,Oz plane)
    2: y ( perp to the beam vertical
    3: tan(teta_y) (in Ox,Oz plane)
  */ 
  static const int NPARTRK=4;	      
  
  /*! number of alignement parameters per plane:
    0: W offset perp to the wire: W+=alpha_W
    1: Z offset along the beam: Z+=alpha_Z
    2: PHI rotational offset, perp to the beam: PHI+=alpha_PHI
  */
  static const int NPARPLAN=3;	
  
  //! max number of global parameters
  static const int NGLB = NPLAN*NPARPLAN;

  //! numerical value for detector_id
  enum enu_detector_id
  {
    kMUTR = 1, kMUID = 2,//
    kFVTX_WEDGE = 11, kFVTX_STATION = 12,//

    kUNKNOWN_DETECTOR = 0
  };
};

//! interface to millepede parameter fix
#define C_PARSIG(A,B) { int i_par(A); float sig(B); parsig_( &i_par, &sig ); }

//! interface to millepede parameter constraints
#define C_CONSTF(A,B) { float* rotation(A);  float sig(B); constf_( rotation, &sig ); }

//! interface to millepede initialization
#define C_INITUN(A,B, C) \
{ \
  int lun(A);  \
  float sig(B); \
  initun_( &lun, &sig, C, strlen(C) ); }

//! interface to millepede matrix reset
#define C_ZERLOC(A,B) { float* dergb( A ); float* derlc( B ); zerloc_( dergb, derlc ); }

//! interface to millepede global parameters printout
#define C_PRTGLO(A)   { int lun( A ); prtglo_( &lun ); }

//! interface to millepede parameters initialization
#define C_INITGL(A,B,C,D) \
{ \
	int n_global( A ); \
 	int n_local( B ); \
	int n_std_dev( C ); \
	int print_flag( D ); \
  std::cout << "C_INITGL - n_global: " << n_global << std::endl; \
  initgl_( &n_global, &n_local, &n_std_dev, &print_flag); \
}        

#endif
