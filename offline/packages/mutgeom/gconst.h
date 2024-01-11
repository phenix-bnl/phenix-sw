// $Id: gconst.h,v 1.9 2007/10/19 02:15:38 phnxbld Exp $ 

#include <iostream>

//! GEANT COMMON block
struct Gconst { 
  float pi;
  float twopi;
  float piby2;
  float degrad;
  float raddeg;
  float clight;
  float big;
  float emass; 

  //! constructor
  Gconst( void ):
    
    // no unit
    pi( 3.14159265358979324 ),	
    twopi( 6.28318530717958648 ),
    piby2( 1.57079632679489662 ),

    // rad/deg
    degrad( 0.0174532925199432958 ),
  
    // deg/rad
    raddeg( 57.2957795130823209 ),
  
    // cm/s
    clight( 29979245800.0 ),	
  
    // no unit
    big( 10000000000.0 ),
  
    // GeV
    emass( 0.0005109990615 )
  {}  
}; 


//! GEANT COMMON block 
#define gconsx gconsx_
extern struct Gconst gconst;

//! GEANT COMMON block
struct Gconsx { 
  
  float emmu;
  float pmass;
  float avo; 
  
  //! constructor
  Gconsx( void ):
   // [GeV]
   emmu( 0.105658387 ),

  // [GeV]
  pmass( 0.9382723128 ),

  // [no unit]
  avo( 0.60221367 )		
  {}
};

//! GEANT COMMON block 
#define gconst gconst_
extern struct Gconsx gconsx;

