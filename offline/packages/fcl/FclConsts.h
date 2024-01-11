
#ifndef __FCLCONSTS__
#define __FCLCONSTS__

const int INIT =-1;
const int POSTTHRESHOLD = 45;
const int ADCOVERFLOW = 4096;
const int ADCZERO = -100;
const int BOARDTOT = 6; // we have 6 possible boards
const int ROWTOT = 12; // total number of rows 
const int COLTOT = 12; // total number of columns
const int ROWUSE = 10; // total number of rows used 
const int COLUSE = 9; // total number of columns used
const int CHANTOT = 144; // 12 x 12 rows and columns
const int FEM_ID_SOUTH = 16001; 
const int FEM_ID_NORTH = 16002; 

const int FCALNORTH = 0;
const int FCALSOUTH = 1;

const int FCALHIGHLOWRUN = 74569;
const int FCALCOSMICRUN = 69946;
const int FCALBASERUN = 69502;

const int FCL_INVALID_INT = -99999;
const int FCL_INVALID_FLOAT = -99999;


#endif
