
#include "mDchDCM.h"
#include "DchSubs_DCM.h"
#include "emlLib.h"
#include <iostream>

using namespace std;

unsigned long int 
DchDCMPACK(unsigned long top, unsigned long chn, 
	   unsigned long slc, unsigned long tdc)
{
  // This routine produces the DCM 32-bit word according to the
  // documentation from Chi and Jamie
  // http://www.nevis.columbia.edu/~nagle/PHENIX/dc_format_pub.htm
  unsigned long int sum=0x0;
  sum = sum | (top*0x80000000);
  sum = sum | (chn*0x01000000);
  sum = sum | (slc*0x00100000);
  sum = sum | (tdc&0x000FFFFF);

  return sum;
}

unsigned long int 
DchDCMid(short arm,short side,short keystone,short pair)
{
  // The subsystem ID as defined by J.Nagle at
  // http://www.nevis.columbia.edu/~nagle/PHENIX/Data_Formats/PHENIX_Formats/header_words_pub.htm

  unsigned long int packetID;

  packetID=Dch_ID_DCM*1000+80*arm+40*side+2*keystone+pair+1;
  return(packetID);
}

/**

This routine takes as its input the dDchFEM table and produce
             three output DCM data streams depending on the mode of 
             operation of the DCM:
             scheme 0: FPGA and DSP pass-through
             scheme 1: FPGA zero-suppression (throw out all 0 words)
                       DSP - pass-through
             scheme 2: FPGA zero-suppression (throw out all 0 words)
                       DSP - further suppression or data decoding?? 
                             to be decided at a later time
                       the code presently returns error for scheme=>2

  @author Julia Velkovska \URL{mailto:velkovska@skipper.physics.sunysb.edu}
  and Thomas K. Hemmick \URL{mailto:hemmick@skipper.physics.sunysb.edu}
  @version 1.0

 ARGUMENTS:
 IN:
 dDchFEM    - FEM table - a block of 972 20-bit words
 dDchFEM_h   - header Structure for dDchFEM
 dDchDCMPar    - this table passes to us the DCM scheme parameter
 dDchDCMPar_h   - header Structure for dDchDCMPar
 INOUT:
 OUT:
 dDchDCM    - DCM table a block of <=971 32-bit words
 note: 971 not 972 , because the CAV1=0xFFFFF is not
 in the DCM output 
 dDchDCM_h   - header Structure for dDchDCM
 RETURNS:    STAF Condition Value
 
*/

long 
mDchDCM_(
	 TABLE_HEAD_ST        *dDchFEM_h,        DDCHFEM_ST          *dDchFEM ,
	 TABLE_HEAD_ST     *dDchDCMPar_h,     DDCHDCMPAR_ST       *dDchDCMPar ,
	 TABLE_HEAD_ST        *dDchDCM_h,        DDCHDCM_ST          *dDchDCM )
{
  unsigned long readout,DCMwcnt,j;
  unsigned long ID,ADDR,idummy;
  short arm;
  short side;
  short keystone;
  short pair;
  short scheme = dDchDCMPar->scheme;
  short maxkeystone=20;
  short i,chan;
  
#ifndef IDDCH_DCM0
#define IDDCH_DCM0 403
#endif
#ifndef IDDCH_DCM1
#define IDDCH_DCM1 503
#endif
#ifndef IDDCH_DCM2
#define IDDCH_DCM2 603
#endif
  
  for (i=0; i<dDchFEM_h->nok; i++) 
    {
      dDchDCM[i].nWord=0;
      //Let's fill in the hardware address and the idDCM      
      ID=dDchFEM[i].det;
      switch(ID)
	{
	case 0xDC000:
	  arm=0;
	  break;
	case 0xDC001:
	  arm=1;
	  break;
	case 0x00300:
	  arm=0;
	  break;
	case 0x00301:
	  arm=1;
	  break;
	default:
	  cout<< " wrong detector ID "<< hex << ID <<dec << endl;
	  return STAFCV_BAD;
	  break;
	}
      
      ADDR=dDchFEM[i].adr;
      idummy=ADDR&0xf0000;
      if(idummy!=0x0000)
	{
	  cout<< " wrong module address "<< hex <<ADDR <<" "<<idummy<<dec<< endl;
	  return STAFCV_BAD;
	}
      idummy=ADDR&0x1000;
      switch(idummy)
	{
	case 0x0:
	  side=0;
	  break;
	case 0x1000:
	  side=1;
	  break;
	default:
	  cout<< " wrong module address" <<endl;
	  return STAFCV_BAD;
	  break;
	}
      
      idummy=ADDR&0x0ff0;
      idummy>>=4;
      keystone=idummy;
      if(keystone>=maxkeystone)
	{
	  cout<< " wrong module address" <<endl;
	  return STAFCV_BAD;
	}
      idummy=ADDR&0x0f;
      switch(idummy)
	{
	case 0:
	  pair=0;
	  break;
	case 1:
	  pair=1;
	  break;
	default:
	  cout<< " wrong module address" <<endl;
	  return STAFCV_BAD;
	  break;
	}
// now we have given the proper hardware address 
// we can get the packetID     
      dDchDCM[i].packetID=DchDCMid(arm,side,keystone,pair);
// First copy the header words w/ proper tags:
      dDchDCM[i].DCM[0] = DchDCMPACK(1,0,0,dDchFEM[i].det);
      dDchDCM[i].DCM[1] = DchDCMPACK(1,0,1,dDchFEM[i].Ecounter);
      dDchDCM[i].DCM[2] = DchDCMPACK(1,0,2,dDchFEM[i].adr);
      dDchDCM[i].DCM[3] = DchDCMPACK(1,0,3,dDchFEM[i].Flag);
      dDchDCM[i].DCM[4] = DchDCMPACK(1,0,4,dDchFEM[i].Bcounter);
      DCMwcnt=5;
// depending on the DCM modes of operation , we have different schemes
      switch(scheme)
	{
	case 0: /* pass through */
// loop over the FEM table , fill in the upper 12 bits on each 32-bit DCM word
	  dDchDCM[i].scheme=IDDCH_DCM0;
	  j=0;
	  for (chan=0; chan<80; chan++) 
	    {
	      for (readout=0; readout<12; readout++) 
		{
		  dDchDCM[i].DCM[DCMwcnt] = DchDCMPACK(0,chan,readout,
			        		  dDchFEM[i].Word[j]);
		  DCMwcnt++;
		  j++;
		}
	    }
	  break;
	case 1: /* zero-suppression - throw out all 0 FEM words */
	  dDchDCM[i].scheme=IDDCH_DCM1;
	  j=0;
	  for (chan=0; chan<80; chan++) 
	    {
	      for (readout=0; readout<12; readout++) 
		{
		  if(dDchFEM[i].Word[j]&0xFFFFF)
		    {
		      dDchDCM[i].DCM[DCMwcnt] = DchDCMPACK(0,chan,readout,
						      dDchFEM[i].Word[j]);
		      DCMwcnt++;
		    }
		  j++;
		}
	    }
	  break;
	case 2:
	  dDchDCM[i].scheme=IDDCH_DCM2;
	  return STAFCV_BAD;
	  break;
	default:
	  cout<<" Undefined DCM scheme "<< endl;
	  return STAFCV_BAD;
	  break;
	}
// Now append all the trailing words.
      dDchDCM[i].DCM[DCMwcnt] = DchDCMPACK(1,1,0,dDchFEM[i].usr1);
      DCMwcnt++;
      dDchDCM[i].DCM[DCMwcnt] = DchDCMPACK(1,1,1,dDchFEM[i].usr2);
      DCMwcnt++;
      dDchDCM[i].DCM[DCMwcnt] = DchDCMPACK(1,1,2,dDchFEM[i].usr3);
      DCMwcnt++;
      dDchDCM[i].DCM[DCMwcnt] = DchDCMPACK(1,1,3,dDchFEM[i].usr4);
      DCMwcnt++;
      dDchDCM[i].DCM[DCMwcnt] = DchDCMPACK(1,1,4,dDchFEM[i].parity);
      DCMwcnt++;
      dDchDCM[i].DCM[DCMwcnt] = DchDCMPACK(1,127,15,dDchFEM[i].CAV2);
      DCMwcnt++;
      dDchDCM[i].nWord=DCMwcnt;
      dDchDCM_h->nok++;    
    }

  return STAFCV_OK;
}







