#include<TMutCathErrors.h>
#include<PHException.h>

double 
TMutCathErrors::get_error(unsigned short arm, 
			  unsigned short station,
			  unsigned short octant,
			  unsigned short half_octant,
			  unsigned short gap,
			  unsigned short cathode)
{

  // Okay -- so this is crappy code -- we'll fix it later.  Values
  // will come from the database -- currently the below statics 
  // are generated via a macro
  //
  enum {ERRORS_SIZE=6};

  static const boost::array<double,ERRORS_SIZE> south_sta1_errors = 
  {{
    0.0470999,
    0.0419341,
    0.0493112,
    0.0346511,
    0.0496401,
    0.037992
  }};
  
  static const boost::array<double,ERRORS_SIZE> north_sta1_errors = 
  {{
    0.0361763,
    0.0269378,
    0.036904,
    0.0244902,
    0.0465431,
    0.0326659
  }};
  
  static const boost::array<double,ERRORS_SIZE> south_sta2_errors = 
  {{
    0.0221713,
    0.0387942,
    0.0221124,
    0.0399654,
    0.0322391,
    0.0475896
  }};
  
  static const boost::array<double,ERRORS_SIZE> north_sta2_errors = 
  {{
    0.0221713,
    0.0387942,
    0.0221124,
    0.0399654,
    0.0322391,
    0.0475896}};
  
  static const boost::array<double,ERRORS_SIZE> south_sta3_errors = 
  {{
    0.0470999,
    0.0419341,
    0.0493112,
    0.0346511,
    0,0}};
  
  static const boost::array<double,ERRORS_SIZE> north_sta3_errors = 
  {{
    0.0361763,
    0.0269378,
    0.036904,
    0.0244902,
    0,
    0
  }};

  const boost::array<double,ERRORS_SIZE>* errors=0;

  if(arm==MUTOO::South) {
    if(station==MUTOO::Station1) {
      errors = &south_sta1_errors;
    } else if(station == MUTOO::Station2) {
      errors = &south_sta2_errors;
    } else {
      errors = &south_sta3_errors;
    }
  } else {
    if(station==MUTOO::Station1) {
      errors = &north_sta1_errors;
    } else if(station == MUTOO::Station2) {
      errors = &north_sta2_errors;
    } else {
      errors = &north_sta3_errors;
    }
  }

  // Idiot check calculated index and return error
  //
  unsigned short index = gap*2+cathode;
  BOUNDS_CHECK(index,ERRORS_SIZE);
  return (*errors)[index];

}

