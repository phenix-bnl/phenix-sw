#include "PISAEvent.h"
#include "KinPISAHit.h"
#include "PriPISAHit.h"
#include "AerPISAHit.h"
#include "BbcPISAHit.h"
#include "BbcPISAPara.h"
#include "ZdcPISAHit.h"
#include "DchPISAHit.h"
#include "DchPISAPara.h"
#include "PadPISAHit.h"
#include "PadPISAPara.h"
#include "SvxPISAHit.h"
#include "SvxPISAParav1.h"
#include "FclPISAHit.h"
#include "TfwPISAHit.h"
#include "RxnPISAHit.h"
#include "HbdPISAHit.h"
#include "CrkPISAHit.h"
#include "CtrPISAHit.h"
#include "TecPISAHit.h"
#include "TofPISAHit.h"
#include "TofPISAPara.h"
#include "EmcPISAHit.h"
#include "EmcPISAPara.h"
#include "MutPISAHit.h"
#include "MutPISAPara.h"
#include "MuiPISAHit.h"
#include "MuiPISAPara.h"
#include "MuPCPISAHit.h"
#include "RLTPISAHit.h"
#include "NCCPISAHit.h"
#include "MPCEXABSPISAHit.h"
#include "MPCFPLTPISAHit.h"
#include "MPCEXEntryPISAHit.h"
#include "VncPISAHit.h"
#include "MpcPISAHit.h"

/////////////////////////////////////////////////////////////////////////
//                                                                     //
// PISAEvent.cc                                                        //
//                                                                     //
// Implementation of PISA event header, event, and PISA hits in event  //
// Paramater tables are stored before the first event data tables      //
//                                                                     //
// Modified by V. L. Rykov 03-Sep-2003: Extended SVX hit set           //
// Modified by V. Dzhordzhadze Aug-10-2004:  for NCC (Nosecone Cal.    //
// Modified by M. Chiu 28-Jul-2005:  for MPC (Muon Piston Cal)         //
// 26 Jul 2006 Hubert van Hecke: added svx global in/out coordinates   //
// Modified by M. Chiu 26-Feb-2015:  for VNC (Veto Nosecone Cal)       //
// """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" //
/////////////////////////////////////////////////////////////////////////

ClassImp(PISAEvent)

//________________________________________________________
PISAEvent::PISAEvent()
{
  
  //std::cout << "PISAEvent::PISAEvent." << std::endl;
  
  // Creates a PISA Event
  // all TClonesArray *must be* stored in _arrays vector, so that they are easily deleted
  // in PISAEvent destructor.
  
  _arrays.push_back( fAerHits = new TClonesArray("AerPISAHit", 1000) );
  fAerNhit = 0;

  _arrays.push_back( fBbcHits = new TClonesArray("BbcPISAHit", 1000) );
  _arrays.push_back( fBbcParas = new TClonesArray("BbcPISAPara", 1000) );
  fBbcNhit = 0;
  fBbcNpara = 0;
 
  _arrays.push_back( fCrkHits = new TClonesArray("CrkPISAHit", 1000) );
  fCrkNhit = 0;
  
  _arrays.push_back( fCtrHits = new TClonesArray("CtrPISAHit", 1000) );
  fCtrNhit = 0;

  _arrays.push_back( fDchHits = new TClonesArray("DchPISAHit", 1000) );
  _arrays.push_back( fDchParas = new TClonesArray("DchPISAPara", 1000) );
  fDchNhit = 0;
  fDchNpara = 0;

  _arrays.push_back( fEmcHits = new TClonesArray("EmcPISAHit", 1000) );
  _arrays.push_back( fEmcParas = new TClonesArray("EmcPISAPara", 1000) );
  fEmcNhit = 0;
  fEmcNpara = 0;
  
  _arrays.push_back( fFclHits = new TClonesArray("FclPISAHit", 1000) );
  fFclNhit = 0;

  _arrays.push_back( fHbdHits = new TClonesArray("HbdPISAHit", 1000) );
  fHbdNhit = 0;

  _arrays.push_back( fKinHits = new TClonesArray("KinPISAHit", 1000) );
  fKinNhit = 0;
  
  _arrays.push_back( fVncHits = new TClonesArray("VncPISAHit", 1000) );
  fVncNhit = 0;

  _arrays.push_back( fMpcHits = new TClonesArray("MpcPISAHit", 1000) );
  fMpcNhit = 0;

  _arrays.push_back( fMuiHits = new TClonesArray("MuiPISAHit", 1000) );
  _arrays.push_back( fMuiParas = new TClonesArray("MuiPISAPara", 1000) );
  fMuiNhit = 0;
  fMuiNpara = 0;

  _arrays.push_back( fMuPCHits = new TClonesArray("MuPCPISAHit", 1000) );
  fMuPCNhit = 0;
     
  _arrays.push_back( fMutHits = new TClonesArray("MutPISAHit", 1000) );
  _arrays.push_back( fMutParas = new TClonesArray("MutPISAPara", 1000) );
  fMutNhit = 0;
  fMutNpara = 0;
  
  _arrays.push_back( fNCCHits = new TClonesArray("NCCPISAHit", 1000) );		
  fNCCNhit = 0;		

  _arrays.push_back( fMPCEXABSHits = new TClonesArray("MPCEXABSPISAHit", 1000) );		
  fMPCEXABSNhit = 0;		
  fMPCEXABSNpara = 0;		

  _arrays.push_back( fMPCFPLTHits = new TClonesArray("MPCFPLTPISAHit", 1000) );		
  fMPCFPLTNhit = 0;		
  fMPCFPLTNpara = 0;		

  _arrays.push_back( fMPCEXEntryHits = new TClonesArray("MPCEXEntryPISAHit", 1000) );		
  fMPCEXEntryNhit = 0;		
  fMPCEXEntryNpara = 0;		

  _arrays.push_back( fPadHits = new TClonesArray("PadPISAHit", 1000) );
  _arrays.push_back( fPadParas = new TClonesArray("PadPISAPara", 1000) );
  fPadNhit = 0;
  fPadNpara = 0;
 
  _arrays.push_back( fPriHits = new TClonesArray("PriPISAHit", 1000) );
  fPriNhit = 0;
    
  _arrays.push_back( frltHits = new TClonesArray("RLTPISAHit", 1000) );
  frltNhit = 0;

  _arrays.push_back( fRxnHits = new TClonesArray("RxnPISAHit", 1000) );
  fRxnNhit = 0;

  _arrays.push_back( fSvxHits = new TClonesArray("SvxPISAHit", 1000) );
  _arrays.push_back( fSvxParas = new TClonesArray("SvxPISAParav1", 1000) );
  fSvxNhit = 0;
  fSvxNpara = 0;
  
  _arrays.push_back( fTecHits = new TClonesArray("TecPISAHit", 1000) );
  fTecNhit = 0;

  _arrays.push_back( fTofHits = new TClonesArray("TofPISAHit", 1000) );
  _arrays.push_back( fTofParas = new TClonesArray("TofPISAPara", 1000) );
  fTofNhit = 0;
  fTofNpara = 0;
  
  _arrays.push_back( fTfwHits = new TClonesArray("TfwPISAHit", 1000) );
  fTfwNhit = 0;

  _arrays.push_back( fZdcHits = new TClonesArray("ZdcPISAHit", 1000) );
  fZdcNhit = 0;  
}

//____________________________________________________
PISAEvent::~PISAEvent()
{

  //std::cout << "PISAEvent::~PISAEvent." << std::endl;
  for( std::vector<TClonesArray*>::iterator iter = _arrays.begin(); iter != _arrays.end(); iter++ )
  { delete *iter; }
  _arrays.clear();
  
}

//____________________________________________________
void PISAEvent::SetHeader(int i[], float f[]) 
{
  //
  // SetHeader is called *after* Para banks are filled
  // Need to change this order in e_put_dst
  // Also must clear out the Para banks after the first event
  //

   fEvtHdr.Set(i, f);

   fKinNhit = 0;
   fPriNhit = 0;
   fBbcNhit = 0;
   fZdcNhit = 0;
   fDchNhit = 0;
   fPadNhit = 0;
   fSvxNhit = 0;
   fMuPCNhit = 0;
   fFclNhit = 0;
   fAerNhit = 0;
   fTfwNhit = 0;
   fRxnNhit = 0;
   fHbdNhit = 0;
   fCrkNhit = 0;
   fCtrNhit = 0;
   fTecNhit = 0;
   fTofNhit = 0;
   fEmcNhit = 0;
   fMutNhit = 0;
   fMuiNhit = 0;
   frltNhit = 0;
   fNCCNhit = 0;
   fMPCEXABSNhit = 0;
   fMPCFPLTNhit = 0;
   fMPCEXEntryNhit = 0;
   fVncNhit = 0;
   fMpcNhit = 0;
}

//____________________________________________________
void PISAEvent::AddKinHit(
  Int_t argtrue_track, Int_t argisubevent,  Int_t argntrack,
  Int_t argidpart, Float_t argptot, Float_t argpthet, Float_t argpphi,
  Float_t argr_vertex, Float_t argz_vertex, Float_t argth_vertx,
  Float_t argph_vertx, Int_t argitparent,   Int_t argidparent,
  Int_t argnfile)
{
  //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &KinPISAhits = *fKinHits;  // set up a reference variable to be what is in fHits
  new(KinPISAhits[fKinNhit++]) KinPISAHit(argtrue_track, argisubevent, argntrack, argidpart,
					  argptot,       argpthet,     argpphi,
					  argr_vertex,   argz_vertex,  argth_vertx,
					  argph_vertx,   argitparent,  argidparent,
					  argnfile);

}

//____________________________________________________
void PISAEvent::PrintKinHits( void ) const
{
  
  for( int khit=0; khit < GetKinNhit(); khit++)
  {
    
    KinPISAHit &hit = *(KinPISAHit*)GetKinHits()->UncheckedAt(khit);
    std::cout << "PISAEvent::PrintKinHits -" 
      << " index: " << khit 
      << " rvertex: " << hit.GetRvertex()
      << " zvertex: " << hit.GetZvertex()
      << " ThVertex: " << hit.GetThvertx()
      << " PhVertex: " << hit.GetPhvertx()
      << "  (" << hit.GetXvertex() << "," << hit.GetYvertex() << "," << hit.GetZvertex() << ")"
      << "  (" << hit.GetPx() << "," << hit.GetPy() << "," << hit.GetPz() << ")"
      << std::endl;
    
  }
  std::cout << std::endl;
  
}

//____________________________________________________
void PISAEvent::AddPriHit(Int_t true_track, Int_t isubevent,  Int_t ntrack, Int_t idpart, Float_t px, Float_t py, Float_t pz, Int_t evttrack, Int_t nfile)
{
  //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &PriPISAhits = *fPriHits;  // set up a reference variable to be what is in fHits
  new(PriPISAhits[fPriNhit++]) PriPISAHit(true_track, isubevent, ntrack, idpart,
					  px, py, pz, evttrack, nfile);
}

//____________________________________________________
void PISAEvent::AddBbcHit(Float_t argx,   Float_t argy,   Float_t argz,
			  Float_t argpx,  Float_t argpy,  Float_t argpz,
			  Float_t argdel, Float_t argtof, Float_t arglen,
			  Short_t argpmt, Short_t argpid, Int_t argtrack, Int_t argisubevent,
                          Int_t argmctrack, Int_t argnfile)
{
  //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &BbcPISAhits = *fBbcHits;  // set up a reference variable to be what is in fHits
  new(BbcPISAhits[fBbcNhit++]) BbcPISAHit(argx, argy, argz, argpx, argpy, argpz, argdel,
				          argtof,arglen, argpmt, argpid, argtrack, argisubevent,
					  argmctrack, argnfile);
}

//____________________________________________________
void PISAEvent::AddBbcPara(Int_t i[], Float_t f[])
{
  //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &BbcPISAparas = *fBbcParas;  // set up a reference variable to be what is in fParas
  new(BbcPISAparas[fBbcNpara++]) BbcPISAPara(i, f);

  return;
}

//____________________________________________________
void PISAEvent::AddZdcHit(Float_t argxm,   Float_t argym,   Float_t argzm,
			  Float_t argpxm,  Float_t argpym,  Float_t argpzm,
			  Float_t argdele, Float_t argtof,
			  Int_t argpid, Int_t argdir, Int_t argmod, 
			  Int_t argtrack, Int_t argisubevent,
                          Int_t argmctrack, Int_t argnfile)
{
  //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &ZdcPISAhits = *fZdcHits;  // set up a reference variable to be what is in fHits
  new(ZdcPISAhits[fZdcNhit++]) ZdcPISAHit(argxm, argym, argzm, argpxm, argpym, argpzm, argdele,
				          argtof,argpid, argdir, argmod, 
                                          argtrack, argisubevent,argmctrack, argnfile);
}

//____________________________________________________
void PISAEvent::AddDchHit(Float_t argxyzinloc[], Float_t argtof, Float_t argxyzoutloc[],
			  Int_t argplane,        Int_t argcell,  Float_t argxyzinglo[],
			  Float_t argpathLength, Int_t argtrack, Int_t argisubevent,
			  Int_t argiArm, Int_t argid, Int_t argmctrack, Int_t argnfile)
{
  //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &DchPISAhits = *fDchHits;  // set up a reference variable to be what is in fHits
  new(DchPISAhits[fDchNhit++]) DchPISAHit(argxyzinloc,    argtof,   argxyzoutloc,
					  argplane,       argcell,  argxyzinglo,
					  argpathLength,  argtrack, argisubevent,
					  argiArm, argid, argmctrack, argnfile);
}

//____________________________________________________
void PISAEvent::AddDchPara(Int_t i[], Float_t f[])
{
  //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &DchPISAparas = *fDchParas;  // set up a reference variable to be what is in fParas
  new(DchPISAparas[fDchNpara++]) DchPISAPara(i, f);
  return;
}

//____________________________________________________
void PISAEvent::AddPadHit(Float_t xyzinloc[], Float_t xyzoutloc[], Float_t xyzinglo[],
			  Float_t tof, Float_t dedx, Float_t pathLength, Int_t track,
			  Int_t arm, Int_t sector, Int_t id, Int_t ipc,
			  Int_t isubevent,  Int_t mctrack, Int_t nfile)
{
  //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &PadPISAhits = *fPadHits;  // set up a reference variable to be what is in fHits
  new(PadPISAhits[fPadNhit++]) PadPISAHit(xyzinloc, xyzoutloc, xyzinglo,
					  tof, dedx, pathLength, track, arm, sector, id, ipc,
					  isubevent,  mctrack, nfile );
}

//____________________________________________________
void PISAEvent::AddMuPCHit(Float_t xyzinloc[], Float_t xyzoutloc[], Float_t xyzinglo[], Float_t tof, Float_t dedx, Float_t pathLength, Int_t track,
			  Int_t arm, Int_t id, Int_t ipc,
			  Int_t isubevent,  Int_t mctrack, Int_t nfile)
{
  //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &MuPCPISAhits = *fMuPCHits; 
  new(MuPCPISAhits[fMuPCNhit++]) MuPCPISAHit(xyzinloc, xyzoutloc, xyzinglo,
					  tof, dedx, pathLength, track, arm, 
					  id, ipc,
					  isubevent,  mctrack, nfile );
}


//____________________________________________________
void PISAEvent::AddSvxHit(Float_t xyzglobal[], Float_t pmomxyz[],
			  Float_t dele, Float_t timeOfFlight,
			  Float_t xyzlocalIn[],  Float_t xyzlocalOut[],
			  Float_t xyzglobalIn[], Float_t xyzglobalOut[],
			  Int_t hitVolume[],
			  Int_t track, Int_t layer, Int_t siliID,
			  Int_t isubevent,  Int_t mctrack, Int_t nfile)
{
  //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &SvxPISAhits = *fSvxHits;  // set up a reference variable to be what is in fHits
  new(SvxPISAhits[fSvxNhit++]) SvxPISAHit(xyzglobal, pmomxyz, dele,
					  timeOfFlight,
					  xyzlocalIn,  xyzlocalOut,
					  xyzglobalIn, xyzglobalOut,
					  hitVolume,
					  track, layer, siliID,
					  isubevent,  mctrack, nfile );
}

//____________________________________________________
void PISAEvent::AddSvxPara(Int_t i[], Float_t f[])
{
  TClonesArray &SvxPISAparas = *fSvxParas;  // set up a reference variable to be what is in fParas
  new(SvxPISAparas[fSvxNpara++]) SvxPISAParav1(i, f);
  return;
}

//____________________________________________________
void PISAEvent::AddFclHit(Float_t xyzglobal[], Float_t pmomxyz[],
			  Float_t dele, Int_t track, Int_t layer, Int_t fclID,
			  Int_t isubevent,  Int_t mctrack, Int_t nfile)
{
  //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &FclPISAhits = *fFclHits;  // set up a reference variable to be what is in fHits
  new(FclPISAhits[fFclNhit++]) FclPISAHit(xyzglobal, pmomxyz, dele, track, layer, fclID,
					  isubevent,  mctrack, nfile );
}

//____________________________________________________
void PISAEvent::AddAerHit(Float_t xyzglobal[], Float_t pmomxyz[],
			  Float_t dele, Int_t track, Int_t layer, Int_t aerID,
			  Float_t pathLength, Float_t tof, Float_t stepLength,
			  Float_t etot, Float_t charge, Float_t momentum,
			  Float_t vertxyz[], Int_t isubevent,  Int_t mctrack, 
			  Int_t nfile)
{
  //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &AerPISAhits = *fAerHits;  // set up a reference variable to be what is in fHits
  new(AerPISAhits[fAerNhit++]) AerPISAHit(xyzglobal, pmomxyz, dele, track, layer, aerID,
					  pathLength, tof, stepLength, etot, charge, momentum,
					  vertxyz, isubevent,  mctrack, nfile );
}

//____________________________________________________
void PISAEvent::AddTfwHit(Float_t xyzinloc[], Float_t xyzoutloc[], Float_t xyzinglo[],
			  Float_t tof, Float_t dedx, Float_t pathLength, Int_t track,
			  Int_t sector, Int_t id,
			  Int_t isubevent,  Int_t mctrack, Int_t nfile)
{
  //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &TfwPISAhits = *fTfwHits;  // set up a reference variable to be what is in fHits
  new(TfwPISAhits[fTfwNhit++]) TfwPISAHit(xyzinloc, xyzoutloc, xyzinglo,
					  tof, dedx, pathLength, track, sector, id,
					  isubevent,  mctrack, nfile );
}

//____________________________________________________
void PISAEvent::AddRxnHit(Float_t xyzinloc[], Float_t xyzoutloc[], Float_t xyzinglo[], Float_t pmomxyz[],
			  Float_t tof, Float_t dedx, Float_t pathLength, Int_t track,
			  Int_t sector, Int_t id,
			  Int_t isubevent,  Int_t mctrack, Int_t nfile)
{
  //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &RxnPISAhits = *fRxnHits;  // set up a reference variable to be what is in fHits
  new(RxnPISAhits[fRxnNhit++]) RxnPISAHit(xyzinloc, xyzoutloc, xyzinglo, pmomxyz,
					  tof, dedx, pathLength, track, sector, id,
					  isubevent,  mctrack, nfile );
}

//____________________________________________________
void PISAEvent::AddHbdHit(Float_t xyzin[], Float_t pxyz[],
			  Float_t tof, Int_t hbdID, Int_t track,
                          Float_t xyzout[], Float_t dele, Float_t pathLength,
			  Int_t detector,  Int_t sector, Int_t padrow, Int_t detflag,
			  Int_t isubevent,  Int_t mctrack, Int_t nfile)
{
  //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &HbdPISAhits = *fHbdHits;  // set up a reference variable to be what is in fHits
  new(HbdPISAhits[fHbdNhit++]) HbdPISAHit(xyzin, pxyz, tof, hbdID, track,
                                          xyzout, dele, pathLength,
					  detector, sector, padrow, detflag,
					  isubevent,  mctrack, nfile );
}

//____________________________________________________
void PISAEvent::AddCrkHit(Short_t argpmt, Float_t argx, Float_t argy, Float_t argz, 
			  Float_t argtof, Float_t argpx, Float_t argpy, Float_t argpz, Short_t argpid, 
			  Int_t argtra, Int_t argparent, Int_t argnbf, Int_t argbi1, Int_t argbi2, 
			  Float_t argbp1, Float_t argbp2, Int_t argmctrack, Int_t argnfile,
			  Int_t argisubevent)
{
  //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &CrkPISAhits = *fCrkHits;  // set up a reference variable to be what is in fHits
  new(CrkPISAhits[fCrkNhit++]) CrkPISAHit(argpmt, argx, argy, argz, argtof, argpx, 
					  argpy, argpz, argpid, argtra,  argparent, argnbf,
					  argbi1, argbi2, argbp1, argbp2,
					  argmctrack, argnfile, argisubevent);
}

//____________________________________________________
void PISAEvent::AddCtrHit(Int_t argdetector, Float_t argx, Float_t argy, Float_t argz, 
			  Short_t argpid, Int_t argitra, Float_t argpvx, Float_t argpvy,
			  Float_t argpvz, Float_t argvx, Float_t argvy, Float_t argvz,
			  Int_t argmctrack, Int_t argnfile, Int_t argisubevent)
{
 //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &CtrPISAhits = *fCtrHits;  // set up a reference variable to be what is in fHits
  new(CtrPISAhits[fCtrNhit++]) CtrPISAHit(argdetector,  argx,  argy,  argz, argpid, argitra,  
					  argpvx,  argpvy,  argpvz, argvx, argvy,  argvz,
					  argmctrack, argnfile, argisubevent);  

}

//____________________________________________________
void PISAEvent::AddTecHit(Int_t argmctrack, Float_t argxyzinloc[], Float_t argxyzoutloc[],
			   Float_t argtof, Int_t argsector, Float_t argdedx, Float_t argxyzinglo[],
			   Int_t argiArm, Int_t argnfile, Int_t argid, Int_t argplane,
			   Int_t argisubevent, Int_t argtrack )
 
{
   //
   // Taken from Event class in ROOT
   // Using "new with placement" to save time
   //
   TClonesArray &TecPISAhits = *fTecHits;  // set up a reference variable to be what is in fHits
   new(TecPISAhits[fTecNhit++]) TecPISAHit(argmctrack, argxyzinloc, argxyzoutloc, 
					   argtof, argsector, argdedx, argxyzinglo, argiArm,
					   argnfile, argid, argplane, argisubevent, argtrack );
}

//____________________________________________________
void PISAEvent::AddTofHit(Int_t argsubvol, Int_t argpanel, Int_t argcolumn,
			  Int_t argpslat, Int_t argslat_seq, Int_t argpartl, Float_t argxm, Float_t argym,
			  Float_t argzm,  Float_t argpos_hit_slat, Float_t argpxm, Float_t argpym,
			  Float_t argpzm, Float_t argtof, Float_t argdele, Int_t argtrack, 
			  Int_t argmctrack, Int_t argisubevent, Int_t argnfile)
{
  //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &TofPISAhits = *fTofHits;  // set up a reference variable to be what is in fHits
  new(TofPISAhits[fTofNhit++]) TofPISAHit(argsubvol, argpanel, argcolumn, argpslat,
					  argslat_seq, argpartl, argxm, argym, argzm, argpos_hit_slat,
					  argpxm, argpym, argpzm, argtof, argdele, argtrack,
					  argmctrack, argisubevent, argnfile);

}

//____________________________________________________
void PISAEvent::AddTofPara(Int_t i[], Float_t f[])
{
  //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &TofPISAparas = *fTofParas;  // set up a reference variable to be what is in fParas
  new(TofPISAparas[fTofNpara++]) TofPISAPara(i, f);
  return;
}

//____________________________________________________
void PISAEvent::AddEmcHit(Int_t argi1, Float_t argdele, Float_t argposx, Float_t argposy,
			  Float_t argposz, Float_t argtof, Int_t argindex1, Int_t argindex2, 
			  Int_t argnumed, Int_t argadd1, Int_t argadd2, Int_t argtrack, 
			  Int_t argmctrack, Int_t argisubevent, Int_t argnfile)
{
  //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &EmcPISAhits = *fEmcHits;  // set up a reference variable to be what is in fHits
  new(EmcPISAhits[fEmcNhit++]) EmcPISAHit(argi1, argdele, argposx, argposy,
					  argposz, argtof, argindex1, argindex2, 
					  argnumed, argadd1, argadd2, argtrack, 
					  argmctrack, argisubevent, argnfile);
}

//____________________________________________________
void PISAEvent::AddEmcPara(const Float_t f[])
{
  //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &EmcPISAparas = *fEmcParas;  // set up a reference variable to be what is in fParas
  new(EmcPISAparas[fEmcNpara++]) EmcPISAPara(f);
  return;
}

//____________________________________________________
void PISAEvent::AddMutHit(Int_t argtrack, Int_t argplane, Short_t argpid, Float_t argt, 
			  Float_t arge, Float_t argx, Float_t argy, Float_t argz, Float_t argpx,
			  Float_t argpy, Float_t argpz, Int_t argmctrack, Int_t argnfile,
			  Int_t argisubevent)
{
  //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &MutPISAhits = *fMutHits;  // set up a reference variable to be what is in fHits
  new(MutPISAhits[fMutNhit++]) MutPISAHit(argtrack, argplane, argpid, argt,
					  arge, argx, argy, argz, argpx, argpy, argpz,
					  argmctrack, argnfile, argisubevent);

}

//____________________________________________________
void PISAEvent::AddMutPara(Int_t i[], Float_t f[])
{

  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  TClonesArray &MutPISAparas = *fMutParas;  // set up a reference variable to be what is in fParas
  new(MutPISAparas[fMutNpara++]) MutPISAPara(i, f);

}

//____________________________________________________
void PISAEvent::AddMuiHit(Int_t argitrksub, Int_t argplane_num, 
			  Int_t argtrk_id, Float_t argtof, Float_t argde, Float_t argrhit[],
			  Float_t argphit[], Int_t argmctrack, Int_t argnfile, Int_t argisubevent)
{
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  TClonesArray &MuiPISAhits = *fMuiHits;  // set up a reference variable to be what is in fHits
  new(MuiPISAhits[fMuiNhit++]) MuiPISAHit(argitrksub, argplane_num,
					  argtrk_id, argtof, argde, argrhit, argphit,
					  argmctrack, argnfile, argisubevent);
}

//____________________________________________________
void PISAEvent::AddMuiPara(Int_t i[], Float_t f[])
{
  TClonesArray &MuiPISAparas = *fMuiParas;  // set up a reference variable to be what is in fParas
  new(MuiPISAparas[fMuiNpara++]) MuiPISAPara(i, f);

}

//____________________________________________________
void PISAEvent::AddrltHit(Float_t xyzinloc[], Float_t xyzoutloc[], Float_t xyzinglo[], Float_t tof, Float_t dedx, Float_t pathLength, Int_t track,
			  Int_t id, Int_t irpc,
			  Int_t isubevent,  Int_t mctrack, Int_t nfile)
{

  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  TClonesArray &RLTPISAhits = *frltHits; 
  new(RLTPISAhits[frltNhit++]) RLTPISAHit(xyzinloc, xyzoutloc, xyzinglo,
					  tof, dedx, pathLength, track,
					  id, irpc,
					  isubevent,  mctrack, nfile );
}

//____________________________________________________
void PISAEvent::AddNCCHit(Int_t evnt, Int_t incc,
                          Int_t twr_id, Int_t sen_id,
                          Float_t tof, Float_t dedx,
                          Int_t isubevent,  Int_t track, Int_t nfile)
{
  //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &NCCPISAhits = *fNCCHits; 
  new(NCCPISAhits[fNCCNhit++]) NCCPISAHit(evnt, incc, twr_id, sen_id, tof, dedx,
					  isubevent, track, nfile );
}

//____________________________________________________
void PISAEvent::AddMPCEXABSHit(Int_t evnt, Int_t incc,
                          Float_t dedx,
                          Int_t isubevent,  Int_t track, Int_t nfile)
{
  //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &MPCEXABSPISAhits = *fMPCEXABSHits; 
  new(MPCEXABSPISAhits[fMPCEXABSNhit++]) MPCEXABSPISAHit(evnt, incc, dedx,
					  isubevent, track, nfile );
}

//____________________________________________________
void PISAEvent::AddMPCFPLTHit(Int_t evnt, Int_t incc,
                          Float_t dedx,
                          Int_t isubevent,  Int_t track, Int_t nfile)
{
  //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &MPCFPLTPISAhits = *fMPCFPLTHits; 
  new(MPCFPLTPISAhits[fMPCFPLTNhit++]) MPCFPLTPISAHit(evnt, incc, dedx,
					  isubevent, track, nfile );
}

//____________________________________________________
void PISAEvent::AddMPCEXEntryHit(Float_t vx, Float_t vy, Float_t vz,
			  Float_t px, Float_t py, Float_t pz,
                          Int_t isubevent,  Int_t track, Int_t nfile)
{
  //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &MPCEXEntryPISAhits = *fMPCEXEntryHits; 
  new(MPCEXEntryPISAhits[fMPCEXEntryNhit++]) MPCEXEntryPISAHit(vx, vy, vz,
							       px, py, pz,
							       isubevent, track, nfile );
}

//____________________________________________________
void PISAEvent::AddVncHit(Float_t xx, Float_t yy, Float_t zz, 
                          Float_t dedx, Float_t Xe, Float_t Ye,
                          Float_t Pmom, Float_t P_id, Float_t PNum,
			  Int_t track,  Int_t arm, Int_t itowr, Float_t itof,
			  Int_t isubevent,  Int_t mctrack, Int_t nfile)
{
  //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &VncPISAhits = *fVncHits; 
  new(VncPISAhits[fVncNhit++]) VncPISAHit(xx, yy, zz, dedx, Xe, Ye, Pmom,
					  P_id, PNum, track, arm, itowr, itof,
					  isubevent,  mctrack, nfile );

}

//____________________________________________________
void PISAEvent::AddMpcHit(Float_t xx, Float_t yy, Float_t zz, 
                          Float_t dedx, Float_t Xe, Float_t Ye,
                          Float_t Pmom, Float_t P_id, Float_t PNum,
			  Int_t track,  Int_t arm, Int_t itowr, Float_t itof,
			  Int_t isubevent,  Int_t mctrack, Int_t nfile)
{
  //
  // Taken from Event class in ROOT
  // Using "new with placement" to save time
  //
  TClonesArray &MpcPISAhits = *fMpcHits; 
  new(MpcPISAhits[fMpcNhit++]) MpcPISAHit(xx, yy, zz, dedx, Xe, Ye, Pmom,
					  P_id, PNum, track, arm, itowr, itof,
					  isubevent,  mctrack, nfile );

}

//____________________________________________________________________
void PISAEvent::Clear(Option_t *option)
{
  fAerHits->Clear(option);
  fBbcHits->Clear(option);
  fCrkHits->Clear(option);
  fCtrHits->Clear(option);
  fDchHits->Clear(option);
  fEmcHits->Clear(option);
  fFclHits->Clear(option);
  fHbdHits->Clear(option);
  fKinHits->Clear(option);
  fVncHits->Clear(option);
  fMpcHits->Clear(option);
  fMuiHits->Clear(option);
  fMuPCHits->Clear(option);
  fMutHits->Clear(option);
  fNCCHits->Clear(option);
  fMPCEXABSHits->Clear(option);
  fMPCFPLTHits->Clear(option);
  fMPCEXEntryHits->Clear(option);
  fPadHits->Clear(option);
  frltHits->Clear(option);
  fRxnHits->Clear(option);
  fSvxHits->Clear(option);
  fTecHits->Clear(option);
  fTfwHits->Clear(option);
  fTofHits->Clear(option);
  fZdcHits->Clear(option);
}
