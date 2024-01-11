#include <mPadSlowSimModule.h>

#include <dPadGeomWrapper.h>

#include <pcghitWrapper.h>

#include <dPad23ParWrapper.h>

#include <dPadSlowSimParWrapper.h>

#include <dPadRawWrapper.h>

#include <dPadGhitRawWrapper.h>
#include <PHCompositeNode.h>

#include <getClass.h>

#include <iostream>
#include <sstream>

#include <emlLib.h>
#include <gsl/gsl_math.h> 
#include <gsl/gsl_randist.h>

using namespace std;

mPadSlowSimModule::mPadSlowSimModule(const int ipc)
{
  rng = gsl_rng_alloc (gsl_rng_mt19937);
  set_pcnumber(ipc);
  iprintpcdigi = 0;   /* Temporary flag for test-print */
  ntpmax = 2;

  newPISAdate = 19980215;  /* PISA updating date, corresponding to
			      pc23par.idatePC23 */ 
  return ;
}

mPadSlowSimModule::~mPadSlowSimModule()
{
  gsl_rng_free(rng);
  return;
}


// Methods to set and retrieve the pad chamber number
void mPadSlowSimModule::set_pcnumber(const int ipc)
{
  pcnumber = ipc;
  gsl_rng_set(rng, 82276 + ipc);
  return ;
}

PHBoolean
mPadSlowSimModule::event(PHCompositeNode *root) 
{


 // Get out if the user hasn't selected a pad chamber number
 if (pcnumber<0 || pcnumber>2) {
   cout << "ERROR:  pcnumber is not set up for mPadSlowSim\n";
   exit(1);
 }

 ostringstream nodename;

 dPadGeomWrapper *dPadGeomWrap = findNode::getClass<dPadGeomWrapper>(root,"dPadGeom");

 nodename << "pc" << (pcnumber+1) << "ghit";
 pcghitWrapper *pcghitWrap = findNode::getClass<pcghitWrapper>(root,nodename.str().c_str());

 dPad23ParWrapper *dPad23ParWrap = findNode::getClass<dPad23ParWrapper>(root,"dPad23Par");

 dPadSlowSimParWrapper *dPadSlowSimParWrap = findNode::getClass<dPadSlowSimParWrapper>(root,"dPadSlowSimPar");

 nodename.str("");
 nodename << "dPc" << (pcnumber+1) << "Raw";
 dPadRawWrapper *dPadRawWrap = findNode::getClass<dPadRawWrapper>(root,nodename.str().c_str());

 nodename.str("");
 nodename << "dPc" << (pcnumber+1) << "GhitRaw";
 dPadGhitRawWrapper *dPadGhitRawWrap = findNode::getClass<dPadGhitRawWrapper>(root,nodename.str().c_str());

  long result;

  TABLE_HEAD_ST t1;
  DPADGEOM_ST *d1;
  TABLE_HEAD_ST t2;
  PCGHIT_ST *d2;
  TABLE_HEAD_ST t3;
  DPAD23PAR_ST *d3;
  TABLE_HEAD_ST t4;
  DPADSLOWSIMPAR_ST *d4;
  TABLE_HEAD_ST t5;
  DPADRAW_ST *d5;
  TABLE_HEAD_ST t6;
  DPADGHITRAW_ST *d6;

  t1 = dPadGeomWrap->TableHeader();
  d1 = dPadGeomWrap->TableData();
  t2 = pcghitWrap->TableHeader();
  d2 = pcghitWrap->TableData();
  t3 = dPad23ParWrap->TableHeader();
  d3 = dPad23ParWrap->TableData();
  t4 = dPadSlowSimParWrap->TableHeader();
  d4 = dPadSlowSimParWrap->TableData();
  t5 = dPadRawWrap->TableHeader();
  d5 = dPadRawWrap->TableData();
  t6 = dPadGhitRawWrap->TableHeader();
  d6 = dPadGhitRawWrap->TableData();

  result = mPadSlowSim_(
    &t1, d1,
    &t2, d2,
    &t3, d3,
    &t4, d4,
    &t5, d5,
    &t6, d6                              );

  dPadGeomWrap->SetRowCount(t1.nok);
  pcghitWrap->SetRowCount(t2.nok);
  dPad23ParWrap->SetRowCount(t3.nok);
  dPadSlowSimParWrap->SetRowCount(t4.nok);
  dPadRawWrap->SetRowCount(t5.nok);
  dPadGhitRawWrap->SetRowCount(t6.nok);

  if (result == 1) {
    return True;
  } else {
    return False;
  }
}

/*:>-------------------------------------------------------------------- 
**: FILE:       mPadSlowSim.c 
**:<------------------------------------------------------------------*/ 


/* *************************************************************************** 
   *************************************************************************** 
 
 mPadSlowSim.c 
 -------------- 
 
 DESCRIPTION: Contains pixel cluster digitization routines 
 
 AUTHOR/CONTACT: Jeffery Mitchell (BNL) 
 
 ROUTINES IN THIS FILE:  
                        lux_pcpix_qresol, 
			lux_pcpix_qpix, lux_pcpix_charge, 
			lux_pcpix_avdigi, 
			lux_pcpix_xcoo, lux_pcpix_wire, 
                        lux_pcpix_hitdigi,lux_pcpix_digi 
 
 *************************************************************************** 
 ***************************************************************************/ 
 
/*       QPAD(ISEC,IPX,IPZ, side) is the charge  
                          in pad (IPX,IPZ, side) sector ISEC 
       NTRPAD(ISEC,IPX,IPZ,side) is the number of tracks traversing   
	     	    	     pad (IPX,IPZ) sector ISEC 
       ITRPAD(ISEC,IPX,IPZ,side,IT) is the pointer to the pcXpixpads id number  
                               for track IT  in pad (IPX,IPZ) sector ISEC */ 
 
 
//....................................................... 
//... mapping from Wire, Cell to Padx, Padz 
//...   W. Xie  02/27/00 
//.................................................................... 
void mPadSlowSimModule::NumberInPadSpace(int ich, int wire, int cell, int *padx, int *padz, int *sect) 
{ // ich:  PC1/2/3 
  // wire, cell: wire and cell number 
  // padx, padz:  the number of the pad 
  //..................................... 
 
  #define aaa(a,b)  -2*(2*a+b+2+a%3)%3 +a+1 
  #define ppadzf(x,z) (int)(aaa(x,z)-(int)(z+4+x%3)%3+1)/3 
 
  if(ich==0) //... pc1 
  {   
    if(cell<106)  
    { 
      cell = 106 - cell - 1; 
      *sect = 0; 
      padx[0] = ppadzf(wire,cell+1);  padz[0] = cell+2; 
      padx[1] = ppadzf(wire,cell);    padz[1] = cell+1; 
      padx[2] = ppadzf(wire,cell-1);  padz[2] = cell; 
      return; 
    } 
    else if(cell>=106) 
    { 
       cell -= 106; 
       wire = 58 - wire - 1; 
       *sect = 1; 
       padx[0] = ppadzf(wire,cell+1);  padz[0] = cell+2; 
       padx[1] = ppadzf(wire,cell);    padz[1] = cell+1; 
       padx[2] = ppadzf(wire,cell-1);  padz[2] = cell; 
       return; 
    } 
  } 
  else if(ich==1) //...pc2 
  { 
    if(cell<106) 
    { 
       if(wire<58)  
       { 
         cell = 106 - cell - 1; 
	 wire = 58 - wire - 1; 
         *sect = 0; 
         padx[0] = ppadzf(wire,cell+1) + 20;  padz[0] = cell+2; 
         padx[1] = ppadzf(wire,cell) + 20;    padz[1] = cell+1; 
         padx[2] = ppadzf(wire,cell-1) + 20;  padz[2] = cell; 
         return; 
       } 
       else if(wire>=58) 
       { 
         cell = 106 - cell - 1; 
         wire = 116 - wire - 1; 
	 *sect = 0; 
         padx[0] = ppadzf(wire,cell+1);  padz[0] = cell+2; 
         padx[1] = ppadzf(wire,cell);    padz[1] = cell+1; 
         padx[2] = ppadzf(wire,cell-1);  padz[2] = cell; 
         return; 
       }          
    } 
    else if(cell>=106)  
    { 
       if(wire<58)  
       { 
         cell -= 106; 
         *sect = 1; 
         padx[0] = ppadzf(wire,cell+1);  padz[0] = cell+2; 
         padx[1] = ppadzf(wire,cell);    padz[1] = cell+1; 
         padx[2] = ppadzf(wire,cell-1);  padz[2] = cell; 
         return; 
       } 
       else if(wire>=58) 
       { 
         cell -=106; 
         wire -=58; 
         *sect = 1; 
         padx[0] = ppadzf(wire,cell+1) + 20;  padz[0] = cell+2; 
         padx[1] = ppadzf(wire,cell) + 20;    padz[1] = cell+1; 
         padx[2] = ppadzf(wire,cell-1) + 20;  padz[2] = cell; 
         return; 
       }          
    }        
  } 
  else if(ich==2) //...pc3 
  { 
    if(cell<106) 
    { 
       if(wire<58)  
       { 
         cell = 106 - cell - 1; 
         *sect = 0; 
         padx[0] = ppadzf(wire,cell+1);  padz[0] = cell+2; 
         padx[1] = ppadzf(wire,cell);    padz[1] = cell+1; 
         padx[2] = ppadzf(wire,cell-1);  padz[2] = cell; 
         return; 
       } 
       else if(wire>=58) 
       { 
         cell = 106 - cell - 1; 
         wire -= 58; 
         *sect = 0; 
         padx[0] = ppadzf(wire,cell+1) + 20;  padz[0] = cell+2; 
         padx[1] = ppadzf(wire,cell) + 20;    padz[1] = cell+1; 
         padx[2] = ppadzf(wire,cell-1) + 20;  padz[2] = cell; 
         return; 
       }          
    } 
    else if(cell>=106)  
    { 
       if(wire<58)  
       { 
         cell -= 106; 
	 wire = 58 - wire -1; 
         *sect = 1; 
         padx[0] = ppadzf(wire,cell+1) + 20;  padz[0] = cell+2; 
         padx[1] = ppadzf(wire,cell) + 20;    padz[1] = cell+1; 
         padx[2] = ppadzf(wire,cell-1) + 20;  padz[2] = cell; 
         return; 
       } 
       else if(wire>=58) 
       { 
         cell -=106; 
         wire = 116 - wire - 1; 
         *sect = 1; 
         padx[0] = ppadzf(wire,cell+1);  padz[0] = cell+2; 
         padx[1] = ppadzf(wire,cell);    padz[1] = cell+1; 
         padx[2] = ppadzf(wire,cell-1);  padz[2] = cell; 
         return; 
       }          
    }        
  }
  cout << PHWHERE << " Fatal Erro - the code should never get here" << endl;
  exit(1);
} 
 
 
/*************************************************************************** 
 
 LUX_PCPIX_QRESOL 
 ---------------- 
 
 DESCRIPTION: Smear charge on the pad with noise. No exp. data available 
              yet, use an equivalent of approximately 1000 electrons. 
 
 AUTHOR/CONTACT: K.Filimonov, McGill University 
 
 REVISIONS: 
	Date		Author		Description 
 
	02/27/00        W. Xie          Modified for new numbering convention 
	4/16/98         W. Xie          Modified for new numbering convention 
					and new pixel board geometry. 
	3/30/96         J. Mitchell     Converted to C for LUXOR 
	1/03/96         K. Filimonov    Modified for CVS 
	9/22/95         K. Filimonov    Modified for pixel option 
	1/18/95		J. Mitchell	 
	1/12/95		M. Rosati	Original 
 
 INPUT VARIABLES:	ich = charge 
 
 OUTPUT VARIABLES:	 
 
 *************************************************************************** 
 ***************************************************************************/ 
 
void mPadSlowSimModule::lux_pcpix_qresol(int ich)  
 
{ 
     
  float qx[4]; 
  int isec,i; 
  int ipx; 
  float qold,qq,qnew; 
  int ppz; 
  int side_max, sside; 
  int ipx_max; 
 
  /* EXECUTABLE STATEMENTS: */ 
   
  for (i=1; i<=3; i++)  
  { 
    qx[i] = 100.0*dPadSlowSimParDigi[0].qnoise[i-1]; 
  } 
   
  /* loop over sectors */ 
  for (isec=0; isec < dPadGeomDigi[0].npdsec[ich-1]; isec++)  
  { 
     if(ich==1) { 
	  ipx_max = 20; 
          side_max = 2; 
     } else { 
	  ipx_max = 40; 
          side_max = 2; 
     } 
	 
     for(sside=0; sside<side_max; sside++) 
        for(ipx = 0; ipx<ipx_max; ipx++) 
           for(ppz = 0; ppz<108; ppz++) 
           { 
	      qold = qpad[isec][ipx][ppz][sside]; 
	      qq = gsl_ran_gaussian(rng,qx[ich]);
	      qnew = qold+(qq/100.0); 
	      if (qnew<0.0) qnew = 0.0; 
	      qpad[isec][ipx][ppz][sside] = qnew; 
           } 
  }    
     
}    
 
 
/*************************************************************************** 
 *************************************************************************** 
 
 LUX_PCPIX_QPIX 
 -------------- 
 
 DESCRIPTION: The integral function of the surface charge density, used 
              to calculate the charge released on the pixel.  
 
 AUTHOR/CONTACT: K. Filimonov, McGill University 
 
 REVISIONS: 
	Date		Author		Description 
 
	3/30/96         J. Mitchell     Converted to C for LUXOR 
        1/03/96         K. Filimonov    Modified for CVS 
        9/22/95         K. Filimonov    Original 
 
 INPUT VARIABLES:	ich = chamber number 
                  	xav,zav = average avalanche position 
                       qav = anode charge 
                       x,z =  coordinates of the corners of rectangular pixel 
 
 OUTPUT VARIABLES:	 
 
 *************************************************************************** 
 ***************************************************************************/ 
 
float mPadSlowSimModule::lux_pcpix_qpix(int ich,float xav,float zav, 
		     float qav,float x,float z)  
 
{ 
     
  float rK1,rK2,zL,xL; 
  float q1,q2,qpix; 
 
  /*  The value of parameter for emperical formula (to take into  
      account field wires) is found using GARFIELD for three pad  
      chambers: */ 
 
  float rK3[4]= 
  { 
    0.0,0.5279,0.6367,0.6383 
  }; 
     
 
  /* EXECUTABLE STATEMENTS: */ 
       
  zL = 2.0*(zav-z)/dPadGeomDigi[0].pdgas[ich-1]; 
  xL = 2.0*(xav-x)/dPadGeomDigi[0].pdgas[ich-1]; 
   
  rK2 = M_PI*(1-0.5*sqrt(rK3[ich]))/2.0; 
  rK1 = rK2*sqrt(rK3[ich])/(4*atan(sqrt(rK3[ich]))); 
 
  q1 = rK1/(rK2*sqrt(rK3[ich]))*atan(sqrt(rK3[ich])*tanh(rK2*zL)); 
  q2 = rK1/(rK2*sqrt(rK3[ich]))*atan(sqrt(rK3[ich])*tanh(rK2*xL)); 
 
  qpix = 2.0*qav*(q1*q2); 
     
  return(qpix); 
     
}   /* end lux_pcpix_qpix */ 
 
 
/*************************************************************************** 
 *************************************************************************** 
 
 LUX_PCPIX_CHARGE 
 ---------------- 
 
 DESCRIPTION: Computes the amount of charge collected by individual pixel 
              The induced charge distribution is given by single parameter  
              empirical formula (from Bo Yu's thesis), adjusted to take into 
              account field wires.   
 
 AUTHOR/CONTACT: K. Filimonov, McGill University 
 
 REVISIONS: 
	Date		Author		Description 
 
	3/30/96         J. Mitchell     Converted to C for LUXOR 
        1/03/96         K. Filimonov    Modified for CVS 
        9/22/95         K. Filimonov    Original 
 
 INPUT VARIABLES:	ich = chamber number 
                  	xav,zav = average avalanche position 
                       qav = anode charge 
                       idp = id of pixel. idp = 1,3 for side pixels 
                                              = 2 for center pixel 
                       xp,zp =  pixel coordinates 
 
 OUTPUT VARIABLES:	 
 
 INTERNAL VARIABLES:	 
 
 *************************************************************************** 
 ***************************************************************************/ 
 
float mPadSlowSimModule::lux_pcpix_charge(int ich,float xav,float zav,float qav, 
		       int idp,float xp,float zp)  
 
{ 
   
  float x0,x1,z0,z1,q; 
     
  /* EXECUTABLE STATEMENTS: */ 
 
  z0 = zp-dPadGeomDigi[0].pxlen[ich-1]/2.0; 
  z1 = zp+dPadGeomDigi[0].pxlen[ich-1]/2.0; 
   
  if (idp == 2)  
    { 
      x0 = xp-dPadGeomDigi[0].wcent[ich-1]/2.0; 
      x1 = xp+dPadGeomDigi[0].wcent[ich-1]/2.0; 
    } 
  else if (idp==1 || idp==3)  
    { 
      x0 = xp-dPadGeomDigi[0].wside[ich-1]/2.0; 
      x1 = xp+dPadGeomDigi[0].wside[ich-1]/2.0; 
    } 
  else  
    { 
      printf("LUX_PCPIX_CHARGE-E: idp is not 1,2,or 3! \n");
      /* CFM November 29, 2001  x0, x1 are uninitialized in this case */
      /* Should really throw an exception, but will set the variables for now */
      x0 = 0.0;
      x1 = 0.0;
    } 
   
  q = lux_pcpix_qpix(ich,xav,zav,qav,x1,z1)+ 
    lux_pcpix_qpix(ich,xav,zav,qav,x0,z0)- 
    lux_pcpix_qpix(ich,xav,zav,qav,x1,z0)- 
    lux_pcpix_qpix(ich,xav,zav,qav,x0,z1); 
     
  return(q); 
     
}   /* end lux_pcpix_charge */ 
 
 
/*************************************************************************** 
 *************************************************************************** 
 LandauRandom 
 -------------- 
 
 DESCRIPTION: Returns random numbers of the approxiamte Landau distribution 
 
 AUTHOR/CONTACT: W. Xie,  Weizmann Inst. Sci (6/27/1998) 
 
 *************************************************************************** 
 ***************************************************************************/ 
 
float mPadSlowSimModule::LandauRandom(int ipc) 
     /* generate random number of function in Slow simulator. See my notebook:Jun.27 */ 
{ 
  static float p[2]={0.383,1.004}; /* to be adjusted according to experiment */ 
  float Hvalue, x1, rnd1, rnd2, Displace = 5; 
  float Hmax =2/sqrt(2*M_PI)*exp(0.5*Displace); 
  long irandom; 
 
  irandom = dPadSlowSimParDigi[0].randseed[ipc]; 
 
  while(1) 
  { 
   rnd1 = gsl_rng_uniform_pos(rng);
   rnd2 = gsl_rng_uniform_pos(rng); 
   x1 = -2*log(rnd1); 
   if(x1>1e100) continue;  /* prevent overflow */ 
   Hvalue =2.0/sqrt(2*M_PI)*exp(Displace/2)*exp(-0.5*exp(-x1+Displace)); 
   if(Hmax*rnd2 <= Hvalue) return (x1-Displace)*p[0]+p[1]; 
  }      
 
  dPadSlowSimParDigi[0].randseed[ipc] = irandom; 
 
} 
 
/*************************************************************************** 
 
 LUX_PCPIX_AVDIGI 
 ---------------- 
 
 DESCRIPTION: Returns the charge on the pads 
 
 AUTHOR/CONTACT: K. Filimonov, McGill University 
 
 REVISIONS: 
       Date            Author          Description 
 
       02/27/00        W. Xie          Modified corresponding to new numbering 
       4/16/98         W. Xie          Modified corresponding to new numbering 
				       convention and PC2/3 PISA geometry  
				       (split at Z=0) 
       3/28/96         J. Mitchell     Converted to C for LUXOR 
       1/03/96         K. Filimonov    Modified for CVS 
       22/9/95         K. Filimonov    Original 
 
 INPUT VARIABLES:      ich = chamber number 
                       isec = sector number 
                       iw = wire number 
                       nz1 = pad (cell) number in Z 
                       zzz = average z-position of track in pad volume 
                       xlen = length of track in pad volume 
		       hitid = GEANT hit id of current track 
 
 *************************************************************************** 
 ***************************************************************************/ 
 
void mPadSlowSimModule::lux_pcpix_avdigi(int ich,int isec,int iw, 
		      int nz1,float zzz,float xlen, int hitid)  
 
{ 
 
  float ratio,xav,zav,qav; 
  float xp0,zp,xp[4]; 
  int i,j,istart,istop,jstart,jstop; 
  int jh,iflag; 
  int nhits; 
  int ncelhalfsect = (dPadGeomDigi[0].npdz[ich-1]-4)/2; 
			/* number of pixel within half section */ 
  float qind,qpix,qtot; 
 
  int pdx1, pdz1,pdx2,pdz2,pdx3,pdz3; 
  int ipix, ipadx, ipadz; /* ipadz is the cell no. along wire,  
			     ipadx is the one vertical to wire */ 
   
  int pxx[3], pzz[3], ipc = ich-1, sside; 
 
  /* EXECUTABLE STATEMENTS: */ 
     
  /* Avalanche coordinates: */ 
     
  zav = zzz;		/* Z-coordinate of the avalanche */ 
  xav = iw*dPadGeomDigi[0].aasep[ich-1]+ 
    dPadGeomDigi[0].aasep[ich-1]/2.- 
    dPadGeomDigi[0].clsep[ich-1]/2.+ 
    dPadGeomDigi[0].pdxoff[ich-1]; 
 
  /* Simulate the Landau shape of anode charge distribution: */ 
 
  qtot = LandauRandom(ich-1); 
 
  /* PN added %f to make insure++ happy */     
  if (iprintpcdigi > 0) printf("  Landau charge: %f\n",qtot); 
 
  ratio = xlen/dPadGeomDigi[0].pdgas[ich-1];   /* attenuation factor */ 
  qav = ratio*qtot;	/* Avalanche size for given track length */ 
 
  if (iprintpcdigi > 0)  
    { 
      printf("Wire #%d, Q=%f, Z=%f, <cell %d>, X=%f <cell %d> \n", 
	     iw,qav,zav,nz1,xav,iw); 
      printf("  Active track length: %f \n",xlen); 
    } 
     
  /* Redefine xav, zav from local coordinates to pixel coordinates  */ 
 
  xav -= dPadGeomDigi[0].pdxoff[ich-1]; 
  zav -= dPadGeomDigi[0].pdzoff[ich-1]; 
 
  /* Calculate the induced charge in a hit cell and 8 neighboring cells: 
     ________ 
     |__|__|__|    
     |__|XX|__|   (collect more than 40% of the anode charge) 
     |__|__|__|    
          
 
     Go over 9x3=27 pixels... 
 
     Check the boundaries in Z-direction: */ 
 
  if (nz1 > dPadGeomDigi[0].npdz[ich-1]-5 )  
    { 
      printf("LUX_PCPIX_AVDIGI-W: Hit is out of bounds! "); 
      printf("ich,nz1= %d %d \n",ich,nz1); 
      printf("reset to %d \n",dPadGeomDigi[0].npdz[ich-1]-5); 
      nz1 = dPadGeomDigi[0].npdz[ich-1]-5; 
      istart = nz1-1; 
      istop = nz1; 
    } 
  else if (nz1 == 0)  
    { 
      istart = nz1; 
      istop = nz1+1; 
    } 
  else if (nz1 == dPadGeomDigi[0].npdz[ich-1]-5)  
    { 
      istart = nz1-1; 
      istop = nz1; 
    } 
  else if (dPad23ParDigi[0].idatePC23 >= newPISAdate && (ich == 2 || ich == 3))   
    { 
	   /* boundary of 1st half sector along wire for PC2/3 at Z=0 */ 
	   if(nz1 == ncelhalfsect-1) 
           { 
               istart = nz1-1; 
               istop = nz1; 
           } 
           else if(nz1 == ncelhalfsect) 
	  /* boundary of 2nd  half sector along wire for PC2/3 at Z=0 */ 
           { 
              istart = nz1; 
              istop = nz1+1; 
           } 
  	   else  
    	   { 
      	     istart = nz1-1; 
      	     istop = nz1+1; 
    	   } 
    } 
  else  
    { 
      istart = nz1-1; 
      istop = nz1+1; 
    } 
   
  /* Check the boundaries in X-direction: */ 
  if (iw >= dPadGeomDigi[0].npdwr[ich-1])  
    { 
      if (iprintpcdigi > 0)  
	{ 
	  printf("LUX_PCPIX_AVDIGI-W: Hit is out of bounds! "); 
	  printf("ich,iw= %d %d \n",ich,iw); 
	  printf("reset to %d \n",dPadGeomDigi[0].npdwr[ich-1] - 1); 
	} 
      iw = dPadGeomDigi[0].npdwr[ich-1] - 1 ; 
      jstart = iw-1; 
      jstop = iw; 
    } 
  else if (iw == 0)  
    { 
      jstart = iw; 
      jstop = iw+1; 
    } 
  else if (iw == dPadGeomDigi[0].npdwr[ich-1]-1)  
    { 
      jstart = iw-1; 
      jstop = iw; 
    } 
  else  
    { 
      jstart = iw-1; 
      jstop = iw+1; 
    } 
     
  /* Reset total induced charge for this avalanche... It's calculated 
     for control purpose only, may be omitted in the future. The total  
     induced charge on the pad cathode should be about 40% (not 50% because 
     pixels are separated by 250 microns and we suppose we don't collect 
     any charge in there). */ 
	    
  qind = 0.0; 
   
  /* Start imaging the anode charge on the pixel cathode... */ 
   
  /* loop over wires */ 
  for (j=jstart; j<=jstop; j++)  
    { 
	   
      /* X-coordinate of the center of the cell: */ 
           
      xp0 = j*dPadGeomDigi[0].aasep[ich-1]+ 
	(dPadGeomDigi[0].wside[ich-1]+ 
	 dPadGeomDigi[0].wcent[ich-1]/2.0+ 
	 dPadGeomDigi[0].pxsep[ich-1]); 
 
      /* X-coordinates of the centers of three pixels: */ 
 
      xp[1] = xp0-(dPadGeomDigi[0].wcent[ich-1]/2.+ 
		   dPadGeomDigi[0].wside[ich-1]/2.+ 
		   dPadGeomDigi[0].pxsep[ich-1]); 
      xp[2] = xp0; 
      xp[3] = xp0+(dPadGeomDigi[0].wcent[ich-1]/2.+ 
		   dPadGeomDigi[0].wside[ich-1]/2.+ 
		   dPadGeomDigi[0].pxsep[ich-1]); 
	   
      /* loop over pixels along the wire */ 
      for (i=istart; i<=istop; i++)  
	{ 
  
	  /* Z-coordinate of the pixel: */ 
 
           if(dPad23ParDigi[0].idatePC23 >= newPISAdate && 
		   i >= (dPadGeomDigi[0].npdz[ich-1]-4)/2)  
	         zp = i*(dPadGeomDigi[0].pxlen[ich-1]+ 
		      dPadGeomDigi[0].pxsep[ich-1])+ 
	              dPadGeomDigi[0].pxlen[ich-1]/2 + 
	              dPadGeomDigi[0].zgap[ich-1]; 
	   else 
	     /* for old PISA file or when i<(dPadGeomDigi[0].npdz[ich-1]-4)/2 */ 
	         zp = i*(dPadGeomDigi[0].pxlen[ich-1]+ 
		      dPadGeomDigi[0].pxsep[ich-1])+ 
   	              dPadGeomDigi[0].pxlen[ich-1]/2; 
 
        //.............................................. 
	//.. start mapping from wire, cell to padx, padz 
        //.............................................. 
          NumberInPadSpace(ipc, j, i, pxx, pzz, &sside); 
 
          pdz1 = pzz[2]; 
          pdx1 = pxx[2]; 
 
          pdz2 = pzz[1]; 
          pdx2 = pxx[1]; 
 
          pdz3 = pzz[0]; 
          pdx3 = pxx[0]; 
 
	//.............................................. 
     
          for(ipix=1; ipix<=3; ipix++) 
	  { 
	    qpix = lux_pcpix_charge(ich,xav,zav,qav, 
				    ipix,xp[ipix],zp); 
	    if(ipix == 1)  
	      { 
		ipadz = pdz1;  
		ipadx = pdx1; 
	      } 
	    else if(ipix == 2) 
	      { 
		ipadz = pdz2; 
		ipadx = pdx2; 
	      } 
	    else if(ipix == 3) 
	      { 
		ipadz = pdz3; 
		ipadx = pdx3; 
	      } 
	    else 
	      {
		cout << PHWHERE << "invalid ipix: " << ipix
		     << ", fix it! exiting now" << endl;
		exit(1);
	      }
	    qpad[isec][ipadx][ipadz][sside] += qpix;  
 
	    qind += qpix; /*Total induced charge (check-up)*/ 
	    nhits = ntrpad[isec][ipadx][ipadz][sside]; 
	    iflag = 0; 
	    if (nhits >= 3) 
      	    { 
	       iflag = 1; 
	       nhits = 3; 
	    } 
	    for (jh=0; jh<nhits; jh++) 
	    { 
	      if (itrpad[isec][ipadx][ipadz][sside][jh] == hitid) 
	      iflag = 1; 
	    } 
	    if (iflag == 0) 
	    { 
	      itrpad[isec][ipadx][ipadz][sside][nhits] = hitid; 
      	      ntrpad[isec][ipadx][ipadz][sside]++; 
	    } 
	  } 
	}   /* i=istart,istop -> Z-direction */ 
    }   /* j=jstart,jstop -> X-direction */ 
     
}   /* end lux_pcpix_avdigi */ 
 
 
/*************************************************************************** 
 *************************************************************************** 
 
 LUX_PCPIX_WCOO 
 -------------- 
 
 DESCRIPTION: Calculates the cell number for the current hit 
 
 AUTHOR/CONTACT: Kirill Filimonov, McGill University 
 
 REVISIONS: 
	Date		Author		Description 
 
	4/17/98         W. Xie          Modeified for new numbering convention 
					and PC2/3 PISA geometry (split at Z=0) 
	3/28/96         J. Mitchell     Converted to C for LUXOR 
        1/03/96         K. Filimonov    Modified for CVS 
        9/22/95         K. Filimonov    Modified for pixel option 
	4/03/95		J. Mitchell	Modified for tracking shell 
	1/12/95		M. Rosati	Original 
 
 INPUT VARIABLES:	ich = chamber number 
                        xd = hit coordinates (global) 
                        ix = input wire number 
                        nxtot = total number of wires in range 
 
 OUTPUT VARIABLES:	ncz = pad (cell) number in Z 
                        zzz = average z position of track entry/exit 
                        xlen = length of track through gas volume 
 
 *************************************************************************** 
 ***************************************************************************/ 
 
void mPadSlowSimModule::lux_pcpix_wcoo(int ich,int ix,int nxtot, 
		    int *ncz,float *zzz,float *xlen)  
 
{ 
     
  int ind1,ind2; 
  int ncelhalfsect = (dPadGeomDigi[0].npdz[ich-1]-4)/2; 
		      /* number of pixel within half section */ 
  float tanzx,tanyx; 
  float xright,xleft; 
  float x1,y1,z1,x2,y2,z2; 
  float zcell,zOffLocal; 
 
  /* EXECUTABLE STATEMENTS: */ 
 
  if (nxtot == 1)  
    { 
      z1=pcdigixd[3][1]; 
      z2=pcdigixd[3][2]; 
      *zzz=(z1+z2)*0.5; 
      *xlen=sqrt((pcdigixd[1][1]-pcdigixd[1][2])* 
		 (pcdigixd[1][1]-pcdigixd[1][2])+ 
		 (pcdigixd[2][1]-pcdigixd[2][2])* 
		 (pcdigixd[2][1]-pcdigixd[2][2])+ 
		 (pcdigixd[3][1]-pcdigixd[3][2])* 
		 (pcdigixd[3][1]-pcdigixd[3][2])); 
    } 
  else  
    { 
      if (pcdigixd[1][2] > pcdigixd[1][1])  
	{ 
	  ind1=1; 
	  ind2=2; 
	} 
      else 
	{ 
	  ind1=2; 
	  ind2=1; 
	} 
      tanyx=(pcdigixd[2][ind1]-pcdigixd[2][ind2])/ 
	(pcdigixd[1][ind1]-pcdigixd[1][ind2]); 
      tanzx=(pcdigixd[3][ind1]-pcdigixd[3][ind2])/ 
	(pcdigixd[1][ind1]-pcdigixd[1][ind2]); 
 
      xleft = ix*dPadGeomDigi[0].aasep[ich-1]+ 
	dPadGeomDigi[0].pdxoff[ich-1]; 
      xright = (ix+1)*dPadGeomDigi[0].aasep[ich-1]+ 
	dPadGeomDigi[0].pdxoff[ich-1]; 
  
      if (pcdigixd[1][ind1] < xleft)  
	{ 
	  x1 = xleft; 
	  y1 = pcdigixd[2][ind1]+(xleft-pcdigixd[1][ind1])*tanyx; 
	  z1 = pcdigixd[3][ind1]+(xleft-pcdigixd[1][ind1])*tanzx; 
	} 
      else  
	{ 
	  x1 = pcdigixd[1][ind1]; 
	  y1 = pcdigixd[2][ind1]; 
	  z1 = pcdigixd[3][ind1]; 
	} 
      if (pcdigixd[1][ind2] > xright)  
	{ 
	  x2 = xright; 
	  y2 = pcdigixd[2][ind2]+(xright-pcdigixd[1][ind2])*tanyx; 
	  z2 = pcdigixd[3][ind2]+(xright-pcdigixd[1][ind2])*tanzx; 
	} 
      else  
	{ 
	  x2 = pcdigixd[1][ind2]; 
	  y2 = pcdigixd[2][ind2]; 
	  z2 = pcdigixd[3][ind2]; 
	} 
      *zzz = (z1+z2)*0.5; 
      *xlen = sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2)+ 
		   (z1-z2)*(z1-z2)); 
    } 
 
   if(dPad23ParDigi[0].idatePC23 >= newPISAdate && 
      (ich == 2 || ich == 3)) /*PC23 split at Z=0 this day*/ 
   { 
       zOffLocal = (dPadGeomDigi[0].pdzoff[ich-1] +  
	     dPadGeomDigi[0].zgap[ich-1]/2)/2;/* Z offset in local gas volume*/ 
                                              /* less than 0. */ 
       if(pcdigixd[3][0] < 0) 
       { 
            zcell = (*zzz-zOffLocal)/ 
	       (dPadGeomDigi[0].pxlen[ich-1]+dPadGeomDigi[0].pxsep[ich-1]); 
            *ncz = (int) zcell; 
	    /*----------------------------------------------------- 
	     change to GLOBAL for the use of lux_pcpix_avdigi */ 
	    *zzz += zOffLocal - dPadGeomDigi[0].zgap[ich-1]/2; 
							     
            if(*ncz >= ncelhalfsect) *ncz=ncelhalfsect-1; 
       } 
       else 
       { 
            zcell = ncelhalfsect + (*zzz-zOffLocal)/ 
	       (dPadGeomDigi[0].pxlen[ich-1]+dPadGeomDigi[0].pxsep[ich-1]); 
            *ncz=(int) zcell; 
	    *zzz -= zOffLocal - dPadGeomDigi[0].zgap[ich-1]/2;  
            if(*ncz < ncelhalfsect) *ncz=ncelhalfsect; 
       } 
   } 
   else  /* for PC1 and PC23 without split at Z = 0 */ 
   { 
       zcell = (*zzz-dPadGeomDigi[0].pdzoff[ich-1])/ 
               (dPadGeomDigi[0].pxlen[ich-1]+dPadGeomDigi[0].pxsep[ich-1]); 
       *ncz=(int) zcell; 
   } 
 
  if (*ncz > dPadGeomDigi[0].npdz[ich-1]-5)  
    { 
/*  
      printf("LUX_PC_WCOO-W: cell number in Z too large: %d \n",*ncz); 
      printf("chamber #, entry/exit Z-coordinates %d %f %f \n", 
	     ich,z1,z2); 
      printf("entry/exit X-coordinates %f %f \n",x1,x2); 
      printf("xleft,xright= %f %f \n",xleft,xright); 
      printf("ix = %d \n",ix); 
      printf("zcell = %f \n",zcell); 
      printf("zzz,xlen = %f %f \n",zzz,xlen); 
      printf("reset to %d \n",dPadGeomDigi[0].npdz[ich-1]-5); 
*/  
      *ncz = dPadGeomDigi[0].npdz[ich-1]-5; 
    } 
   
  if (*ncz < 0)  
    { 
/* 
      printf("LUX_PC_WCOO-W: cell number in Z < 0 \n"); 
      printf("chamber #, entry/exit Z-coordinates %d %f %f \n", 
	     ich,z1,z2); 
      printf("reset to 0 \n"); 
*/ 
      *ncz = 0; 
    } 
 
}   /* end lux_pcpix_wcoo */ 
 
 
/*************************************************************************** 
 *************************************************************************** 
 
 LUX_PCPIX_WIRE 
 -------------- 
 
 DESCRIPTION: Calculates the wire number range from the coordinates 
 
       AUTHOR/CONTACT: K. Filimonov, McGill University 
 
 REVISIONS: 
	Date		Author		Description 
 
	4/17/98         W. Xie          Modified for new numbering convention. 
			       	        and PC2/3 PISA geometry (split at Z=0) 
	3/28/96         J. Mitchell     Converted to C for LUXOR 
        1/03/96         K. Filimonov    Modified for CVS 
        9/22/95         K. Filimonov    Modified for pixel option 
	4/03/95		J. Mitchell	Modified for tracking shell 
        1/12/95		M. Rosati	Original 
 
 INPUT VARIABLES:	ich = chamber number 
                        xd  = coordinates (global) 
 
 OUTPUT VARIABLES:	ncx1,ncx2 = wire numbers for these coordinates 
 
 *************************************************************************** 
 ***************************************************************************/ 
 
void mPadSlowSimModule::lux_pcpix_wire(int ich,int *ncx1,int *ncx2)  
 
{ 
     
  float xcell,xcell2; 
  float x1,x2; 
   
  /* EXECUTABLE STATEMENTS: */ 
   
  /* Determine the hit cell: */ 
   
  x1=pcdigixd[1][1];   /* entry coordinates */ 
  x2=pcdigixd[1][2];   /* exit coordinates */ 
        
  xcell = ((x1-dPadGeomDigi[0].pdxoff[ich-1])+ 
	   dPadGeomDigi[0].clsep[ich-1]/2.0)/ 
            dPadGeomDigi[0].aasep[ich-1]; 
  xcell2 = ((x2-dPadGeomDigi[0].pdxoff[ich-1])+ 
	    dPadGeomDigi[0].clsep[ich-1]/2.)/ 
    dPadGeomDigi[0].aasep[ich-1]; 
 
  if (xcell < xcell2)  
    { 
      *ncx1 = (int) xcell; 
      *ncx2 = (int) xcell2; 
    } 
  else  
    { 
      *ncx1 = (int) xcell2; 
      *ncx2 = (int) xcell; 
    } 
 
  if (iprintpcdigi > 0)  
    { 
      if (*ncx1 < 0)  
	{ 
	  printf("LUX_PCPIX_WIRE-W: wire number less than 0: %d \n", 
		 *ncx1); 
	  printf("reset to 0 \n"); 
	  printf("chamber #, entry/exit X-coordinates %d %f %f \n", 
		 ich,x1,x2); 
	  *ncx1=0; 
	} 
      if (*ncx2 < 0)  
	{ 
	  printf("LUX_PCPIX_WIRE-W:wire number less than 1: %d \n", 
		 *ncx2); 
	  printf("reset to 0 \n"); 
	  printf("chamber #, entry/exit coordinates %d %f %f \n", 
		 ich,x1,x2); 
	  *ncx2=0; 
	} 
	   
      if (*ncx1 > dPadGeomDigi[0].npdwr[ich-1] - 1)  
	{ 
	  printf("LUX_PCPIX_WIRE-W:wire number greater than max: %d \n", 
		 *ncx1); 
	  printf("reset to %d \n",dPadGeomDigi[0].npdwr[ich-1]-1); 
	  printf("chamber #, entry/exit X-coordinates %d %f %f \n", 
		 ich,x1,x2); 
	  *ncx1 = dPadGeomDigi[0].npdwr[ich-1] - 1; 
	} 
      if (*ncx2 > dPadGeomDigi[0].npdwr[ich-1]-1)  
	{ 
	  printf("LUX_PCPIX_WIRE-W:wire number greater "); 
	  printf("than max: %d \n",*ncx2); 
	  printf("reset to %d \n",dPadGeomDigi[0].npdwr[ich-1]-1); 
	  printf("chamber #, entry/exit X-coordinates %d %f %f \n", 
		 ich,x1,x2); 
	  *ncx2 = dPadGeomDigi[0].npdwr[ich-1] - 1; 
	} 
    }   /* iprintpcdigi > 0 */ 
     
  if (iprintpcdigi > 0)  
    printf("  Wire # at entry/exit: %d %d \n",*ncx1,*ncx2); 
     
}   /* end lux_pcpix_wire */ 
 
 
/**************************************************************************** 
 *************************************************************************** 
 
 LUX_PCPIX_HITDIGI 
 ----------------- 
 
 DESCRIPTION: General subroutine for digitization of GEANT hits for any of 
       three pad chambers (pixel option). 
 
 AUTHOR/CONTACT: K. Filimonov, McGill University 
 
 REVISIONS: 
	Date		Author		Description 
 
        3/28/96         J.T. Mitchell   Converted to C for LUXOR 
        1/03/96         K. Filimonov    Modified for CVS 
        9/22/95		K. Filimonov	Original 
 
 INPUT VARIABLES:	ich = chamber number 
                        isec = sector number 
                        xd(4,3) = entrance and exit coordinates (global) 
                        hitid = hit number 
 
 OUTPUT VARIABLES:    
 
 DATA STRUCTURES USED:	NONE 
 
 INCLUDES USED: PCPIXGEO, PCPIXDIGI 
 
 INTERNAL VARIABLES:	 
 
 *************************************************************************** 
 ***************************************************************************/ 
 
void mPadSlowSimModule::lux_pcpix_hitdigi(int ich,int isec,int hitid) 
 
{ 
     
  int nx1,nx2,nxtot,iw,nz1,i; 
  float zzz,xlen; 
 
  /*.. Define which wires were hit: */ 
   
  lux_pcpix_wire(ich,&nx1,&nx2); 
     
  /* nxtot is the total number of wires hit */ 
  nxtot = nx2-nx1+1; 
     
  /* loop over the wires */ 
  for (i=nx1; i<=nx2; i++)  
    { 
      iw = i; 
	   
      /* Calculate the cell number for the current hit: */ 
       
      lux_pcpix_wcoo(ich,iw,nxtot,&nz1,&zzz,&xlen); 
       
      /* Digitize the avalanche and fill out the PIXDIGI.h stuff: */ 
       
      lux_pcpix_avdigi(ich,isec,iw,nz1,zzz,xlen,hitid); 
	   
    }   /* i=nx1,nx2 */  
     
}   /* end lux_pcpix_hitdigi */ 
 
/************************************************************************ 
  Main pcSlowSim Module Switchyard 
  ***********************************************************************/ 
 
long mPadSlowSimModule::mPadSlowSim_( 
  TABLE_HEAD_ST       *dPadGeom_h,       DPADGEOM_ST         *dPadGeom , 
  TABLE_HEAD_ST        *pcXghit_h,         PCGHIT_ST          *pcXghit , 
  TABLE_HEAD_ST        *dPad23Par_h,        DPAD23PAR_ST          *dPad23Par , 
  TABLE_HEAD_ST *dPadSlowSimPar_h, DPADSLOWSIMPAR_ST   *dPadSlowSimPar , 
  TABLE_HEAD_ST       *dPadXRaw_h,        DPADRAW_ST         *dPadXRaw , 
  TABLE_HEAD_ST   *dPadXGhitRaw_h,    DPADGHITRAW_ST      *dPadXGhitRaw) 
{ 
/*:>-------------------------------------------------------------------- 
**: ROUTINE:    mPadSlowSim_ 
**: DESCRIPTION: Pixel Pad Chamber Slow Detector Response Simulation 
**: ARGUMENTS: 
**:       IN: 
**:           dPadGeom    - Simple geometry parameters for the pad chambers 
**:          dPadGeom_h   - header Structure for dPadGeom 
**:            pcXghit    - PC GEANT hit information 
**:           pcXghit_h   - header Structure for pcXghit 
**:            dPad23Par    - PC2/3 geometry updating date (Z=0 split). 
**:           dPad23Par_h   - header Structure for dPad23Par 
**:    INOUT: 
**:     dPadSlowSimPar    - Input parameters for the pcSlowSim module 
**:    dPadSlowSimPar_h   - header Structure for dPadSlowSimPar 
**:      OUT: 
**:          dPadXRaw     - PC hit pads information 
**:         dPadXRaw_h    - header Structure for dPadXRaw 
**:      dPadXGhitRaw     - Relation between pcXghit and dPadXRaw 
**:     dPadXGhitRaw_h    - header Structure for dPadXGhitRaw 
**: RETURNS:    STAF Condition Value 
**:>------------------------------------------------------------------*/ 
 
 
  /*************************************************************************** 
  *************************************************************************** 
 
  mPadSlowSimModule 
  ----------- 
 
  DESCRIPTION: Main digitization of hits for pixel pad chambers. 
   
  AUTHOR/CONTACT: K. Filimonov, McGill University 
  CONTACT: Jeffery T. Mitchell (BNL) 
 
  REVISIONS: 
  Date		Author		Description 
   
  10/12/98        W. Xie          convert 2 numbering convention with each other 
  4/17/98         W. Xie          Modified for new numbering convention 
			       	  and PC2/3 PISA geometry (split at Z=0) 
  6/12/97         J. Mitchell     Converted to STAF 
  3/28/96         J. Mitchell     Converted to C for LUXOR 
  1/03/96         K. Filimonov    Modified for CVS 
  1/12/95         M. Rosati       Original 
  9/22/95	  K. Filimonov    Modified for pixel option, fixed 
  3/31/95         J. Mitchell     Modified for tracking shell 
 
  *************************************************************************** 
  ***************************************************************************/ 
 
  int ich,isect,ihit; 
  int iarm,ipads,ipc,ipcraw,ipcgr; 
  int hitid,jsect,sectarm; 
  int is,ipx,ih; 
  int ngr; 
  int ppz; 
  int side_max, sside, ipx_max; 
 
  /* Executable statements */ 
 
  /* Set up local pointers to be used within this module */ 
  dPadGeomDigi_h = dPadGeom_h; 
  dPadXRawDigi_h = dPadXRaw_h; 
  dPadXGhitRawDigi_h = dPadXGhitRaw_h; 
  dPadSlowSimParDigi_h = dPadSlowSimPar_h; 
  dPad23ParDigi_h = dPad23Par_h; 
  dPadGeomDigi = dPadGeom; 
  dPadSlowSimParDigi = dPadSlowSimPar; 
  dPadXRawDigi = dPadXRaw; 
  dPadXGhitRawDigi = dPadXGhitRaw; 
  dPad23ParDigi = dPad23Par; 
 
 
  iprintpcdigi = 0; 
  if (dPadSlowSimParDigi[0].verbose != 0) iprintpcdigi = 1; 
  if (iprintpcdigi > 0)  
    { 
      printf("\n"); 
      printf("************************************************* \n"); 
      printf("*     E N T E R  PC  D I G I T I Z A T I O N    * \n"); 
      printf("************************************************* \n"); 
      printf("/n"); 
    } 
 
  ipc = dPadSlowSimParDigi[0].pcnumber; 
 
  ich = ipc + 1; 
 
  /* With this filling, the id number is related to the  
     (sector,padx,padz) address by: 
     id = (sector-1)*(dPadGeomDigi[0].npdx*dPadGeomDigi[0].npdch)+ 
     (ipadx-1)*(dPadGeomDigi[0].npdz)+(ipadz)  */ 
 
  /* loop over sectors */ 
  for (is=0; is<dPadGeomDigi[0].npdsec[ich-1]; is++)  
  { 
     if(ich==1) {  
	  ipx_max = 20; 
          side_max = 2; 
     } else { 
	  ipx_max = 40; 
          side_max = 2; 
     } 
	 
     for(sside=0; sside<side_max; sside++) 
        for(ipx = 0; ipx<ipx_max; ipx++) 
           for(ppz = 0; ppz<108; ppz++) 
     	   { 
	      qpad[is][ipx][ppz][sside] = 0; 
	      ntrpad[is][ipx][ppz][sside] = 0; 
	      for (ih=0; ih<=ntpmax; ih++) 
	      { 
	        itrpad[is][ipx][ppz][sside][ih] = 0; 
	      } 
	   } 
  } 
  if (iprintpcdigi > 0)  
    printf("pcSlowSim-I: PC%d: Total number of hits = %ld\n", 
	   ipc,pcXghit_h->nok); 
 
  for (ipads=0; ipads<dPadXGhitRawDigi_h->maxlen; ipads++) 
    { 
      dPadXGhitRawDigi[ipads].ghitid = 0; 
      dPadXGhitRawDigi[ipads].rawid = 0; 
    } 
 
  /* loop over PCX GEANT hits */ 
  for (ihit=0; ihit<pcXghit_h->nok; ihit++)  
    { 
      /* Fill variables for lux_pcpix_hitdigi */ 
      isect = pcXghit[ihit].sector ; 
      iarm = pcXghit[ihit].arm ; 
 
      pcdigixd[1][0] = pcXghit[ihit].xyzinglo[0]; /* global X */ 
      pcdigixd[2][0] = pcXghit[ihit].xyzinglo[1]; /* global Y */ 
      pcdigixd[3][0] = pcXghit[ihit].xyzinglo[2]; /* global Z */ 
 
      pcdigixd[1][1] = pcXghit[ihit].xyzinloc[0]; 
      pcdigixd[2][1] = pcXghit[ihit].xyzinloc[1]; 
      pcdigixd[3][1] = pcXghit[ihit].xyzinloc[2]; 
      pcdigixd[1][2] = pcXghit[ihit].xyzoutloc[0]; 
      pcdigixd[2][2] = pcXghit[ihit].xyzoutloc[1]; 
      pcdigixd[3][2] = pcXghit[ihit].xyzoutloc[2]; 
      hitid = pcXghit[ihit].id; 
 
      if (iprintpcdigi > 0)  
	{ 
	  printf("\n"); 
	  printf(" HIT # %d \n",hitid); 
	  printf(" Sector %d \n",isect); 
	  printf(" Entry coordinates: %f %f %f \n", 
		 pcdigixd[1][1],pcdigixd[2][1],pcdigixd[3][1]); 
	  printf(" Exit  coordinates: %f %f %f \n", 
		 pcdigixd[1][2],pcdigixd[2][2],pcdigixd[3][2]); 
	} 
 
      /* Digitize it */ 
      lux_pcpix_hitdigi(ich,isect,hitid); 
 
    }   /* ihit=0,pcXghit_h->nok */ 
   
  /* Smear charge on the pad with noise */ 
  lux_pcpix_qresol(ich); 
 
  /* Now fill the dPadRaw table if the charge on the pad is above threshold */ 
  /* loop over sectors */ 
  for (is=0; is<dPadGeomDigi[0].npdsec[ich-1]; is++)  
  { 
     if(ich==1) {//... PC1 
	  ipx_max = 20; 
          side_max = 2; 
     } else { 
          side_max = 2; 
	  ipx_max = 40; 
     } 
	
     for(sside=0; sside<side_max; sside++) 
        for(ipx = 0; ipx<ipx_max; ipx++) 
           for(ppz = 0; ppz<108; ppz++) 
	   { 
              if(qpad[is][ipx][ppz][sside] > dPadSlowSimPar[0].threshold[ich-1]) 
              { 
                  sectarm = dPadGeomDigi[0].sectperarm[ich-1]; 
                  iarm = 0; 
                  if (is>=sectarm) iarm=1; 
                  if (iarm == 1) 
                  { 
                      jsect = is - sectarm ; 
                  } 
                  else if (iarm == 0) 
                  { 
                      jsect = sectarm - is - 1 ; 
                  } 
		  else 
                  {
		    printf("mPadSlowSimModule: iarm is illegal %d \n", iarm);
		    /* CFM November 29, 2001 jsect uninitialized in this case */
		    /* Should really throw an exception, but will set the variable for now */
		    jsect = 0;

		  } 

                  ipcraw = dPadXRawDigi_h->nok; 
                  dPadXRawDigi[ipcraw].id = ipcraw; 
                  dPadXRawDigi[ipcraw].arm = iarm; 
                  dPadXRawDigi[ipcraw].sector = jsect; 
                  dPadXRawDigi[ipcraw].side = sside; 
                  dPadXRawDigi[ipcraw].padx = ipx; 
                  dPadXRawDigi[ipcraw].padz = ppz; 
		  dPadXRawDigi[ipcraw].padtype = 0; /* Line added on Dec. 31, 2003 (CFM), valgrind error in PadRec */ 
 
		  /* 
		  printf("%d %d %d %d %d %d %d\n",ipcraw,iarm,jsect,sside,ipx,ppz,ich); 
		  */ 
	 
	          dPadXRawDigi_h->nok++; 
 
		  /* Fill the dPadXGhitRaw structure for each GEANT track 
		     traverersing the pad */ 
		  ipcgr = dPadXGhitRaw_h->nok; 
		  ngr = ntrpad[is][ipx][ppz][sside]; 
		  if (ngr > 3) ngr = 3;   /* protection */ 
		  for (ih=0; ih<ngr; ih++) 
		  { 
		      dPadXGhitRaw[ipcgr].ghitid =  
			itrpad[is][ipx][ppz][sside][ih]; 
		      dPadXGhitRaw[ipcgr].rawid = ipcraw; 
		      dPadXGhitRaw_h->nok++; 
		  }   
              } 
	   }   
    }   
 
  /* Check on the size of the output tables */ 
  if ((dPadSlowSimPar_h->maxlen <= 0) || 
      (dPadSlowSimPar_h->nok > dPadSlowSimPar_h->maxlen)) 
    { 
      printf("pcSlowSim-F: Too many dPadSlowSimPar rows: %ld\n", 
	     dPadSlowSimPar_h->nok); 
      return STAFCV_BAD; 
    } 

  /* printf(" mPadSlowSimModule.c: maxlen = %d, nok = %d\n",dPadXRaw_h->maxlen,dPadXRaw_h->nok); */

  if ((dPadXRaw_h->maxlen <= 0) || 
      (dPadXRaw_h->nok > dPadXRaw_h->maxlen)) 
    { 
      printf("pcSlowSim-F: Too many dPadXRaw rows: %ld\n", 
	     dPadXRaw_h->nok); 
      return STAFCV_BAD; 
    } 
  if ((dPadXGhitRaw_h->maxlen <= 0) || 
      (dPadXGhitRaw_h->nok > dPadXGhitRaw_h->maxlen)) 
    { 
      printf("pcSlowSim-F: Too many dPadXGhitRaw rows: %ld\n", 
	     dPadXGhitRaw_h->nok); 
      return STAFCV_BAD; 
    } 
 
  return STAFCV_OK; 
 
}   /* end mPadSlowSimModule module */
