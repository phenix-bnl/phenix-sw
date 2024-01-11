#ifndef __mEmcEmbedConst_h__
#define __mEmcEmbedConst_h__

#include <vector>
#include "TObject.h"
#include "TRandom2.h"

static Int_t init_done = 0 ;
static Int_t kVerbose = 0 ;
static Bool_t fakeEmbed = false ;
static PHString gOutputFileNamePbSc = "" ;
static PHString gOutputFileNamePbGl = "" ;
static Bool_t fOutputPbGlCustomFile = false ;
static Double_t PercentageOfSimEnergySmearing = 0. ;

#ifdef MAIN
// some global counters

Int_t eventNumMerge = 0; // Merged event counter
Int_t eventNumReal = 0;  // Real event counter
Int_t eventNumSim = 0;   // Simul event counter
Int_t SimulEvents = 0;   // total number of events in simulated file
Int_t SimEventOffset = 0; // Offset were to start the scanning of the Simulated DST

Int_t nemcclusReal = 0    ; 
Int_t nemcclusRealpbsc = 0  ; 
Int_t nemcclusRealpbgl = 0 ; 
Int_t nemcclusSim = 0    ;
Int_t nemcclusSimpbsc = 0 ; 
Int_t nemcclusSimpbgl = 0 ; 
Int_t nemcclus = 0 ; 
Int_t nemccluspbsc_reclust  = 0; 
Int_t nemccluspbgl_reclust = 0 ; 
Int_t nemccluspbsc_sum  = 0; 
Int_t nemccluspbgl_sum = 0 ; 

Int_t rejectedReal = 0 ;
Int_t rejectedSimul = 0 ; 
TRandom2 *rand_phi ;         // Run-1 phi-correction hack (DdE)
Int_t rejectedSimulphi = 0 ; // Run-1phi-correction hack (DdE)

//
std::vector<int> used_simul_evts ;
Float_t bbc_z_exp = 0. ;  // Run-1 exp. vertex (def. global to be compared with simul vtx)
Float_t vtx_z_exp = 0. ;  // Run-2 exp. vertex (def. global to be compared with simul vtx)
Float_t bbc_t0_exp = 0 ;  // def. global to propagate it inside process_event

Float_t xclusSim = 0.;
Float_t yclusSim = 0.;
Float_t zclusSim = 0.;

#else
// some global counters

extern Int_t eventNumMerge = 0; // Merged event counter
extern Int_t eventNumReal = 0;  // Real event counter
extern Int_t eventNumSim = 0;   // Simul event counter
extern Int_t SimulEvents = 0;   // total number of events in simulated file
extern Int_t SimEventOffset = 0; // Offset were to start the scanning of the Simulated DST

extern Int_t nemcclusReal = 0    ; 
extern Int_t nemcclusRealpbsc = 0  ; 
extern Int_t nemcclusRealpbgl = 0 ; 
extern Int_t nemcclusSim = 0    ;
extern Int_t nemcclusSimpbsc = 0 ; 
extern Int_t nemcclusSimpbgl = 0 ; 
extern Int_t nemcclus = 0 ; 
extern Int_t nemccluspbsc_reclust  = 0; 
extern Int_t nemccluspbgl_reclust = 0 ; 
extern Int_t nemccluspbsc_sum  = 0; 
extern Int_t nemccluspbgl_sum = 0 ; 

extern Int_t rejectedReal = 0 ;
extern Int_t rejectedSimul = 0 ; 
extern TRandom2 *rand_phi ;         // Run-1 phi-correction hack (DdE)
extern Int_t rejectedSimulphi = 0 ; // Run-1 phi-correction hack (DdE)

//
extern std::vector<Int_t> used_simul_evts ;
extern Float_t bbc_z_exp = 0. ;  // Run-1 exp. vertex (def. global to be compared with simul vtx)
extern Float_t vtx_z_exp = 0. ;  // Run-2 exp. vertex (def. global to be compared with simul vtx)
Float_t bbc_t0_exp = 0 ;  // def. global to propagate it inside process_event

extern Float_t xclusSim = 0.;
extern Float_t yclusSim = 0.;
extern Float_t zclusSim = 0.;

#endif


#endif //__mEmcEmbedConst_h__
