#include <iostream>
#include "PISAEventHeader.h"
//
// PISAEventHeader.cc
//
ClassImp(PISAEventHeader)

// Initialize static data member
PISAEventHeader PISAEventHeader::EventHeader;

//_______________________________________________________
PISAEventHeader::PISAEventHeader( void ):
  idrun(0),
  ntru_evt(0),
  nsub_evt(0),
  readout(0),
  idevt(0),
  nptls(0),
  ipopsub(0),
  nsubevent(0),
  bimevt1000(0),
  atarg(0),
  ztarg(0),
  aproj(0),
  zproj(0),
  roots1000(0),
  bmin1000(0),
  bmax1000(0),
  t0femto(0),
  igdate(0),
  igtime(0),
  isq_start(0),
  itime_evt(0),
  event_code(0),
  nthrow(0),
  naccept(0)
{
  
  // initialize arrays
  for( unsigned int i = 0; i<3; i++ ) { xyz1000[i] = 0; }
  for( unsigned int i = 0; i<2; i++ ) { nrndm[i] = 0; }
  for( unsigned int i = 0; i< ARRAY_SIZE; i++ )
  {
    eventInt[i] = 0;
    eventFloat[i] = 0;
  }
  
}

//_______________________________________________________
PISAEventHeader::~PISAEventHeader( void )
{}

//_______________________________________________________
void PISAEventHeader::Set(int i[], float f[])
{
  
  // Event codes from PISA
  enum {
    HIJING = 5,
    PYTHIA  = 7,
    RV_PHI  = 101,
    RV_JPSI = 102,
    RV_CHI  = 103
  };

  idrun      = i[4];
  ntru_evt   = i[5];
  idevt      = i[6];
  nsub_evt   = i[7];

  readout    = i[8];
  nptls      = i[9];
  ipopsub    = i[10];

  // default value
  nsubevent = 1;  
  if(ipopsub > 0 && nptls > 0) 
  {
    nsubevent = nptls/ipopsub + 1;
    
    // check if nptls exactly divisible
    if(nptls%ipopsub == 0) nsubevent = nsubevent - 1;         
    
  }  // check if nptls and ipopsub have been set in PISA
  
  bimevt1000 = i[11];
  atarg      = i[12];
  ztarg      = i[13];
  aproj      = i[14];
  zproj      = i[15];
  roots1000  = i[16];
  bmin1000   = i[17];
  bmax1000   = i[18];
  t0femto    = i[19];
  xyz1000[0] = i[20];
  xyz1000[1] = i[21];
  xyz1000[2] = i[22];
  
  // date/time
  igdate     = i[23];
  igtime     = i[24];
  
  nrndm[0]   = i[25];
  nrndm[1]   = i[26];
  isq_start  = i[27];
  itime_evt  = i[28];
  event_code = i[29];

  //
  // Event generator specific information
  //

  if(event_code == RV_JPSI ||
     event_code == RV_PHI  ||
     event_code == RV_CHI) {
    //
    // Vector meson generators
    //
    nthrow     = i[30];
    naccept    = i[31];
  } // check on Vector meson event code

  if(event_code == PYTHIA) {
    //
    // Pythia information requested by the spin group
    // This information is transferred in e_put_dst 
    //
    eventInt[0]   = i[30]; // pyth_proc_id

    eventFloat[0] = f[31]; // pyth_bjork(1)
    eventFloat[1] = f[32]; // pyth_bjork(2)
    eventFloat[2] = f[33]; // pyth_partstu(1)
    eventFloat[3] = f[34]; // pyth_partstu(2)
    eventFloat[4] = f[35]; // pyth_partstu(3)
    eventFloat[5] = f[36]; // pyth_qsqr
    eventFloat[6] = f[37]; // pyth_ptrans

    //
    // Intermediate particles
    //
    for (Int_t kpart=0; kpart<4; kpart++) {
      eventInt[1 + kpart]      = i[38 + kpart*5];     // intr_part_id
      eventFloat[7 + kpart*4]  = f[39 + kpart*5];     // intr_part_p1
      eventFloat[8 + kpart*4]  = f[40 + kpart*5];     // intr_part_p2
      eventFloat[9 + kpart*4]  = f[41 + kpart*5];     // intr_part_p3
      eventFloat[10 + kpart*4] = f[42 + kpart*5];     // intr_part_p4
    } // loop over intermediate particles
  } // Check on PYTHIA event code

  eventInt[10] = i[70];    // iEastWest
  for (Int_t iLoop=0; iLoop<3; iLoop++) {
    eventFloat[71+iLoop] = f[71+iLoop];  // shiftEast
    eventFloat[74+iLoop] = f[74+iLoop];  // shiftWest
    eventFloat[77+iLoop] = f[77+iLoop];  // rotateEast
    eventFloat[80+iLoop] = f[80+iLoop];  // roateWest
  }
  
  eventInt[11] = i[89];  // mapFile choice
  eventFloat[90] = f[90];  // fscale factor
  eventFloat[91] = f[91];  // R cut off in gustep
  eventFloat[92] = f[92];  // Z cut off in gustep
  eventFloat[93] = f[93];  // momentum cut off in gustep
  eventFloat[94] = f[94];  // reaction plane angle in degrees (0 to 360)

  eventInt[99] = i[99];

  eventInt[12] = i[0];  // identification of event input file
  eventInt[13] = i[1];  // identification of PISA hits output file
  eventInt[14] = i[2];  // identification of PISA hits project
  eventInt[15] = i[3];  // identification of version number 

  if(event_code == HIJING) {
    eventInt[16] = i[30];  // number of binary collisions in HIJING1.37 (September 5, 2002)
    std::cout << "\n Number of HIJING binary collisions = " << i[30] << std::endl;

  } // check for HIJING event generator

}
