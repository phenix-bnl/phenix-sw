// $Id: PHGeant.C,v 1.3 2011/07/18 18:58:44 abhisek Exp $

#include "PHGeant.hh"
#include "cfortran.h"
#include "geant_cfortran.hh"
#include "quest.hh"
#include <iostream>
#include <cstdlib>
#include <string>

using namespace std;

extern "C" {
  extern void zebra_inline_decompress_(int *lun, int *len, char *filename);
  extern int lzinfile_(char *filename, int len);
  extern void lzclose_(void);
  extern void ggclos_(void);
  extern void gphysi_(void ); 
}

// geant parameters
// extern "C" int gslag_(char *option, int* value);
//#define GSLAG( A, B ) { static int value = B; gslag_( A, &value ); }

// PROTOCCALLSFSUB2(GSFLAG, agsflag ,STRING,INT)
// #define GSFLAG(A, I) CCALLSFSUB2(GSFLAG,agsflag,STRING, INT, A, I)

PROTOCCALLSFSUB2(ZEBRA_INIT,zebra_init,INT,INT)
#define ZEBRA_INIT(I1,I2) CCALLSFSUB2(ZEBRA_INIT,zebra_init,INT,INT,I1, I2)

PROTOCCALLSFSUB7(CFOPEN,cfopen,PINT,INT,INT,STRING,INT,STRING,PINT)
#define CFOPEN(A1,A2,A3,A4,A5,A6,A7) \
  CCALLSFSUB7(CFOPEN,cfopen,PINT,INT,INT,STRING,INT,STRING,PINT,A1,A2,A3,A4,A5,A6,A7)

PROTOCCALLSFSUB2(CFCLOS,cfclos,INT,INT)
#define CFCLOS(A1,A2) CCALLSFSUB2(CFCLOS,cfclos,INT,INT,A1,A2)

PROTOCCALLSFSUB2(FZENDI,fzendi,INT,STRING)
#define FZENDI(A1,A2) CCALLSFSUB2(FZENDI,fzendi,INT,STRING,A1,A2)

PROTOCCALLSFSUB3(FZFILE,fzfile,INT,INT,STRING)
#define FZFILE(A1,A2,A3) CCALLSFSUB3(FZFILE,fzfile,INT,INT,STRING,A1,A2,A3)

PROTOCCALLSFSUB2(FZLOGL,fzlogl,INT,INT)
#define FZLOGL(A1,A2) CCALLSFSUB2(FZLOGL,fzlogl,INT,INT,A1,A2)

PROTOCCALLSFSUB3(FZHOOK,fzhook,INT,STRING,INT)
#define FZHOOK(A1,A2,A3) \
  CCALLSFSUB3(FZHOOK,fzhook,INT,STRING,INT,A1,A2,A3)

PROTOCCALLSFSUB6(GFIN,gfin,INT,STRING,INT,PINT,STRING,PINT)
#define GFIN(A1,A2,A3,A4,A5,A6) \
  CCALLSFSUB6(GFIN,gfin,INT,STRING,INT,PINT,STRING,PINT,A1,A2,A3,A4,A5,A6)

// ====================================================================
// AUXILIARY FUNCTIONS
// ====================================================================

//____________________________________________
void stafGzinit() 
{
  // Initialize GEANT common blocks - probably not needed in STAF
  GZINIT();
  
  // Random number seeds - numbers taken directly from pcrninit.F
  Gcflag.nrndm[0] = 9958613;
  Gcflag.nrndm[1] = 7043595;

}

//____________________________________________
PHGeant* PHGeant::_instance(0);
bool PHGeant::_initialized( false );
bool PHGeant::_quiet( false );

//____________________________________________
PHGeant* PHGeant::Geant()
{
  if (!_initialized) {
    cout << "PHGeant::Geant -  not initialized. Use Init() first." << endl;
    return 0;
  } else return _instance;
}


//____________________________________________
void PHGeant::Init( bool quiet )
{	
	if (!_instance) _instance = new PHGeant;  
  if (_initialized) 
  {
		cout << "PHGeand::Init - already initialized." << endl;
    return;
  }
  
  // store quiet flag
  _quiet = quiet;
  
  /* 
  initialize zebra (second argument, 
  if not 0, means quiet mode
  */
  cout << "PHGeant::Init - NWGEAN: " << NWGEAN << endl;
  ZEBRA_INIT( NWGEAN, _quiet );
  stafGzinit();
  
  // update flag
  _initialized = true;
		
}

//____________________________________________
int PHGeant::ReadFile(int &lun, char *filename)
{
		
  char fo_mode[2];
	
  // Added these since cfortran doesn't like literals
  char name[5], option[5];
  int idvers = 0, ier;
  int luncopy;
  int status;
  int len;
  static int lunlast = 0;

  luncopy = lun;
  if(luncopy ==  -1 && lunlast > 0)
  {
	
		// Close the previously opened background file
		Quest.iquest[0] = lunlast;
    strcpy(option, "T");
    FZENDI(lunlast, option);

    if(lunlast == 99) lzclose_();    
    if(lunlast != 99) CFCLOS(lunlast, 0);
         
  }

  // Check if this is a compressed ZEBRA file
  len = strlen(filename);
  if(filename[len - 1] == 'Z' && filename[len - 2] == 'c') 
  {
    if( lzinfile_(filename, len) ) {
      cout << "\n  Cannot find file " << filename << "\n";
      return 0;
    }
		
    //lun = 99;  should load from MuonUtil
    zebra_inline_decompress_(&lun, &len, filename);
    lunlast = lun;
    strcpy(name, "INIT");
    strcpy(option, ( _quiet ? "QI":"I" ) );
    
    GFIN(lun , name, 1, idvers, option, ier);
    ggclos_();
    gphysi_();   
    return 1;
  }

  strncpy(fo_mode, "r", 2);
  CFOPEN(lun, 0, 900, fo_mode, 0, filename, status);
  if (status == 0) {
    Quest.iquest[0] = lun;
    if(luncopy == -1) lunlast = lun;
    strcpy(option, "XIL");
    FZFILE(lun, 0, option);
    FZLOGL(lun, 0);
    strcpy(name, "INIT");
    strcpy(option, ( _quiet ? "QI":"I" ) );
    GFIN(lun , name, 1, idvers, option, ier);
    ggclos_();
    gphysi_();   
    return 1;
  } else return 0;

}


//____________________________________________
void PHGeant::Destroy()
{
  if( _instance ) delete _instance;
  _instance = 0;
  _initialized = false;
}
