#ifndef __MPADSLOWSIMMODULE_H__
#define __MPADSLOWSIMMODULE_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"

#include "table_header.h"
#include "dPad23Par.h"
#include "pcghit.h"

#include "dPadGeom.h"
#include "dPadSlowSimPar.h"
#include "dPadRaw.h"
#include "dPadGhitRaw.h"

#include "gsl/gsl_rng.h"

class PHCompositeNode;

class mPadSlowSimModule
{
public:
  mPadSlowSimModule(const int ipc=-1);
  virtual ~mPadSlowSimModule();
  PHBoolean event(PHCompositeNode *);
  int get_pcnumber() const {return pcnumber;}
  void set_pcnumber(const int ipc);

 private:
  int pcnumber;


  long mPadSlowSim_( 
  TABLE_HEAD_ST       *dPadGeom_h,       DPADGEOM_ST         *dPadGeom , 
  TABLE_HEAD_ST        *pcXghit_h,         PCGHIT_ST          *pcXghit , 
  TABLE_HEAD_ST        *dPad23Par_h,        DPAD23PAR_ST          *dPad23Par , 
  TABLE_HEAD_ST *dPadSlowSimPar_h, DPADSLOWSIMPAR_ST   *dPadSlowSimPar , 
  TABLE_HEAD_ST       *dPadXRaw_h,        DPADRAW_ST         *dPadXRaw , 
  TABLE_HEAD_ST   *dPadXGhitRaw_h,    DPADGHITRAW_ST      *dPadXGhitRaw);

  void NumberInPadSpace(int ich, int wire, int cell, int *padx, int *padz, int *sect);
  void lux_pcpix_qresol(int ich) ;
float lux_pcpix_qpix(int ich,float xav,float zav, 
		     float qav,float x,float z);
float lux_pcpix_charge(int ich,float xav,float zav,float qav, 
		       int idp,float xp,float zp);
 float LandauRandom(int ipc);
void lux_pcpix_avdigi(int ich,int isec,int iw, 
		      int nz1,float zzz,float xlen, int hitid) ;
void lux_pcpix_wcoo(int ich,int ix,int nxtot, 
		    int *ncz,float *zzz,float *xlen) ;
 void lux_pcpix_wire(int ich,int *ncx1,int *ncx2) ;
 void lux_pcpix_hitdigi(int ich,int isec,int hitid);

TABLE_HEAD_ST *dPadGeomDigi_h; 
TABLE_HEAD_ST *dPadXRawDigi_h; 
TABLE_HEAD_ST *dPadXGhitRawDigi_h; 
TABLE_HEAD_ST *dPadSlowSimParDigi_h; 
TABLE_HEAD_ST *dPad23ParDigi_h; 
DPADGEOM_ST   *dPadGeomDigi; 
DPADRAW_ST    *dPadXRawDigi; 
DPADGHITRAW_ST *dPadXGhitRawDigi; 
DPADSLOWSIMPAR_ST *dPadSlowSimParDigi; 
DPAD23PAR_ST *dPad23ParDigi; 

int iprintpcdigi;
float pcdigixd[4][3]; 
int ntpmax; 
 
long newPISAdate;  /* PISA updating date, corresponding to  
					pc23par.idatePC23 */ 
 
/* General Pad Chamber Simulation Include File (Pixel Version) */ 
/* .. Maximum number of sectors: 16 */ 
 
/* .. Maximum number of pads in perpendicular to wire  
   direction (X-axis): 40  */ 
 
/* Maximum number of pads in wire direction (Z-axis): 216 */ 
/* Maximum number of wires vertical to  wire direction (X-axis): 116 */ 
 
/* .. Maximum number of tracks traversing pad (IPX,IPZ) sector ISEC: 2 */ 
float qpad[17][41][110][4]; 
int ntrpad[17][41][110][4]; 
int itrpad[17][41][110][4][3]; 
  gsl_rng *rng;

};
#endif /*__MPADSLOWSIMMODULE_H__*/
