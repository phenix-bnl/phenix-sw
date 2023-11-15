#include <iostream>
#include <cstdlib>
#include "PISAEvent.h"
#include "MpcPISAHit.h"

using namespace std;

//________________________________________________________________
void encodeRootEvntMpc(
  int kentries, int iData[], float fData[], 
  int isubevent, PISAEvent *pisaevent)
{

  assert( pisaevent->GetStartFlag() != 2 );

/*
  cout 
    << "encodeRootEvntMpc - kentries: " << kentries
    << " isubevent: " << isubevent << endl;
*/

  Int_t mctrack = -1;  // Will change in Off-Line
  Int_t nfile = -1;    // Will change in Off-Line

  Int_t p = 0;
  Int_t arm = -1;
  Float_t xx_mpc;
  Float_t yy_mpc;
  Float_t zz_mpc;

  // Loop over hits
  for (int k=0; k<kentries; k++) 
  {

    Int_t track   = iData[p++];     // track in subevent
    xx_mpc        = fData[p++];     // X of hit
    yy_mpc        = fData[p++];     // Y of hit
    zz_mpc        = fData[p++];     // Z of hit
     
    if ( zz_mpc>0. ) arm = 1;	// north arm
    else             arm = 0;	// south arm

    // p++;  // Skip particle ID output (could do trtrno redundancy check)
    Float_t   tofg_mpc     = fData[p++]; // tofg of hit
    Float_t Xe_mpc         = fData[p++]; // X of incoming particle
    Float_t Ye_mpc         = fData[p++]; // Y of incoming particle
    Float_t Pmom_mpc       = fData[p++]; // Mom of incoming particle
    Float_t P_id_mpc       = fData[p++]; // Pid of incoming particle
    Float_t PNum_mpc       = fData[p++]; // Track num of incoming particle
    Int_t tower_id         = static_cast<Int_t>(fData[p++]); // Simulation Tower Number
    Float_t dedx_mpc       = fData[p++]; // dedx of hit
 
    // Original subevent output, kept for future use
    Int_t towers_output_flag = 0;

    if ( !towers_output_flag ) 
    {  
      // write out each hit
      pisaevent->AddMpcHit(
        xx_mpc, yy_mpc, zz_mpc,
        dedx_mpc, Xe_mpc, Ye_mpc,
        Pmom_mpc, P_id_mpc, PNum_mpc,
        track, arm, tower_id,
        tofg_mpc, isubevent,  mctrack, nfile );
    }
    
  }
  
  return;
}
