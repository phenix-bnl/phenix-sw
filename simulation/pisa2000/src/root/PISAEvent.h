#ifndef _PISAEVENT_
#define _PISAEVENT_

// $Id: PISAEvent.h,v 1.43 2018/07/05 16:50:13 lajoie Exp $

/*!
  \file  PISAEvent.h
  \brief Description of the PISA event header, event, and PISA hits in event
  \author  C. Maguire
  \version $Revision: 1.43 $
  \date    $Date: 2018/07/05 16:50:13 $
*/

#include <vector>

#ifndef __CINT__
#include<boost/weak_ptr.hpp>
#endif

#include "TTree.h"
#include "TObject.h"
#include "TClonesArray.h"
#include "TH1.h"
#include "PISAEventHeader.h"

//! Description of the PISA event header, event, and PISA hits in event
/*!                                                                                                                                            
Description of the PISA event header, event, and PISA hits in event  
Used for all subsystems.<br>
Each subsystem is described by a set of variables:
<ul>
<li>The number of hits in the event for this detector</li>
<li>A <i>local</i> array to store the detector hits in the event (or sub-event)</li>
<li>A <i>static</i> array to store the detector hits in the event (possibly merging the arrays for different subevents)</li>
<li>Optionally the number of parameter objects that describe this detector</li>
<li>Optionally a <i>local</i> array of parameter objects for this detector</li>
<li>Optionally a <i>global</i> array of parameter objects for this detector</li>
</ul>
Not all detectors have parameter objects. Not all parameter objects of the detectors are actually
filled, or used.
*/

class PISAEvent : public TObject 
{
  
  private:
  
  //! event header
  PISAEventHeader fEvtHdr;
  
  //! used to detect the last event to be processed
  Int_t end_flag;
  
  //! used to perform dedicated actions when processing the event 
  /*! 0 for subevent, 1 for full event output. Other values are ignored */
  Int_t startflag;  

  //!@name KIN
  //@{
  //!  number of KIN hits
  Int_t fKinNhit;
  
  //! array of KIN hits
  TClonesArray *fKinHits;
  
  //@}
  
  //!@name PRI
  //@{
  
  //! number of PRI hits
  Int_t fPriNhit;

  //! array of PRI hits
  TClonesArray *fPriHits;

  //@}
  
  //!@name BBC
  //@{
  //! number of BBC hits
  Int_t fBbcNhit;

  //! array of BBC hits
  TClonesArray *fBbcHits;

  //! number of BBC parameter objects
  Int_t fBbcNpara;
  
  //! array of BBC parameter objects
  TClonesArray *fBbcParas;
  
  //@}
  
  //!@name ZDC
  //@{
  //! number of ZDC hits
  Int_t fZdcNhit;

  //! array of ZDC hits
  TClonesArray *fZdcHits;

  //@}
  
  //!@name DCH (drift chambers)
  //@{
  //! number of DCH hits
  Int_t fDchNhit;

  //! array of DCH hits
  TClonesArray *fDchHits;
  
  //! number of DCH parameter objects
  Int_t fDchNpara;

  //! array of DCH parameter objects
  TClonesArray *fDchParas;

  //@}
  
  //!@name PAD
  //@{
  //! number of PAD hits
  Int_t fPadNhit;

  //! array of PAD hits
  TClonesArray *fPadHits;
  
  //! number of PAD parameter objects
  Int_t fPadNpara;
  
  //! array of PAD parameter objects
  TClonesArray *fPadParas;
  
  //@}
  
  //!@name SVX
  //@{
  //! number of SVX hits
  Int_t fSvxNhit;

  //! array of SVX hits
  TClonesArray *fSvxHits;
  
  //! number of SVX parameter objects
  Int_t fSvxNpara;

  //! array of SVX parameter objects
  TClonesArray *fSvxParas;

  //@}
  
  //!@name MuPC (muon pad chambers)
  //@{
  //! number of MuPC hits
  Int_t fMuPCNhit;

  //! array of MuPC hits
  TClonesArray *fMuPCHits;

  //@}
  
  //!@name Rxn (reaction plane detector)
  //@{
  //! number of RXN hits
  Int_t fRxnNhit;

  //! array of RXN hits
  TClonesArray *fRxnHits;

  //@}
  
  //!@name HBD
  //@{
  //! number of HBD hits
  Int_t fHbdNhit;

  //! array of HBD hits
  TClonesArray *fHbdHits;

  //@}
  
  //!@name FCL (forward calorimeter
  //@{
  //! number of FCL hits
  Int_t fFclNhit;

  //! array of FCL hits
  TClonesArray *fFclHits;

  //@}
  
  //!@name AER (aerogel detector)
  //@{
  //! number of AER hits
  Int_t fAerNhit;

  //! array of AER hits
  TClonesArray *fAerHits;

  //@}
  
  //!@name TFW (Time of Flight West)
  //@{
  //! number of TFW hits
  Int_t fTfwNhit;

  //! array of TFW hits
  TClonesArray *fTfwHits;

  //@}
  
  //!@name CRK (RICH detector)
  //@{
  //! number of CRK hits
  Int_t fCrkNhit;

  //! array of CRK hits
  TClonesArray *fCrkHits;
  
  //! number of Ctr (internal crk dev object)  
  Int_t fCtrNhit;

  //! array of Ctr hits
  TClonesArray *fCtrHits;

  //@}
  
  //!@name TEC (Time Expansion Chamber)
  //@{
  //! number of TEC hits
  Int_t fTecNhit;

  //! array of TEC hits
  TClonesArray *fTecHits;

  //@}
  
  //!@name TOF (Time Of Flight)
  //@{
  //! number of TOF hits
  Int_t fTofNhit;

  //! array of TOF hits
  TClonesArray *fTofHits;
  
  //! number of TOF parameter objects
  Int_t fTofNpara;

  //! array of TOF parameter objects
  TClonesArray *fTofParas;

  //@}
  
  //!@name EMC (Electro-Magnetic Calorimeter
  //@{
  //! number of EMC hits
  Int_t fEmcNhit;

  //! array of EMC hits
  TClonesArray *fEmcHits;

  //! number of EMC parameter objects
  Int_t fEmcNpara;

  //! array of EMC parameter objects
  TClonesArray *fEmcParas;
  
  //!@name MUT (muon tracker)
  //@{
  //! number of MUT hits
  Int_t fMutNhit;

  //! array of MUT hits
  TClonesArray *fMutHits;
  
  //! number of MUT parameter objects
  Int_t fMutNpara;

  //! array of MUT parameter objects
  TClonesArray *fMutParas;

  //@}
  
  //!@name MUI (muon identifier)
  //@{
  //! number of MUI hits
  Int_t fMuiNhit;

  //! array of MUI hits
  TClonesArray *fMuiHits;
  
  //! number of MUI parameter objects
  Int_t fMuiNpara;

  //! array of MUI parameter objects
  TClonesArray *fMuiParas;

  //@}
  
  //!@name RLT (Relative Luminosity Telescope)
  //@{
  
  //! number of RLT hits
  Int_t frltNhit;

  //! array of RLT hits
  TClonesArray *frltHits;
  
  //! array of RLT parameter objects
  Int_t frltNpara;
  
  //@}
  
  //!@name NCC (Nosecone Calorimeter)
  //@{
  
  //! number of NCC hits
  Int_t fNCCNhit;

  //! array of NCC hits
  TClonesArray *fNCCHits;

  //! number of NCC parameter objects
  Int_t fNCCNpara;

  //@}

  //!@name MPCEXABS (MPC-EX Absorber)
  //@{
  
  //! number of absorber hits
  Int_t fMPCEXABSNhit;

  //! array of NCC hits
  TClonesArray *fMPCEXABSHits;

  //! number of NCC parameter objects
  Int_t fMPCEXABSNpara;

  //!@name MPCFPLT (MPC front plate)
  //@{
  
  //! number of absorber hits
  Int_t fMPCFPLTNhit;

  //! array of NCC hits
  TClonesArray *fMPCFPLTHits;

  //! number of NCC parameter objects
  Int_t fMPCFPLTNpara;

  //!@name MPCEXEntry (MPC-EX Entry Particles)
  //@{
  
  //! number of absorber hits
  Int_t fMPCEXEntryNhit;

  //! array of NCC hits
  TClonesArray *fMPCEXEntryHits;

  //! number of NCC parameter objects
  Int_t fMPCEXEntryNpara;

  //@}
  
  //!@name VNC (Veto Nosecone Calorimeter)
  //@{
  
  //! number of VNC hits
  Int_t fVncNhit;

  //! array of VNC hits
  TClonesArray *fVncHits;
  
  //! number of VNC parameter objects
  Int_t fVncNpara;
  
  //@}
  
  //!@name MPC (Muon Piston Calorimeter)
  //@{
  
  //! number of MPC hits
  Int_t fMpcNhit;

  //! array of MPC hits
  TClonesArray *fMpcHits;
  
  //! number of MPC parameter objects
  Int_t fMpcNpara;
  
  //@}
  
  //! stores all TClonesARray pointer in a single vector, to make sure they are all deleted in destructor
  /*! this is hidden from root interpreter, by means of the //! comments after the declaration */
  std::vector<TClonesArray*> _arrays; //!
  
  
  public:
  
  #ifndef __CINT__
  //! typedef for shared_pointer to PISAEvent
  typedef boost::shared_ptr<PISAEvent> pointer;
  #endif
  
  //! default constructor
  PISAEvent();
  
  //! destructor
  virtual ~PISAEvent();
  
  void SetEndFlag(const Int_t flag) 
  { end_flag = flag; }
  
  Int_t GetEndFlag() const 
  { return end_flag; }
  
  void Clear(Option_t *option ="");  
  void SetHeader(int i[], float f[]);
  
  PISAEventHeader *GetHeader() 
  { return &fEvtHdr; }
  
  void SetStartFlag(const Int_t flag) 
  { startflag = flag; }
  
  Int_t GetStartFlag()const 
  {return startflag; }
  
  //! KIN
  Int_t GetKinNhit() const 
  { return fKinNhit; }
  
  void AddKinHit(
    Int_t true_track, Int_t subevent, Int_t ntrack,
    Int_t idpart, Float_t ptot, Float_t pthet,
    Float_t pphi, Float_t r_vertex, Float_t z_vertex,
    Float_t th_vertx, Float_t ph_vertx, Int_t itparent,
    Int_t idparent, Int_t nfile);
  
  TClonesArray *GetKinHits() const 
  { return fKinHits; }
  
  //! print kin hist
  void PrintKinHits( void ) const;
  
  //! PRI
  Int_t GetPriNhit() const
  { return fPriNhit; }
  
  void AddPriHit(
    Int_t true_track, Int_t subevent, Int_t ntrack, Int_t idpart,
    Float_t px, Float_t py, Float_t pz, Int_t evttrack, Int_t nfile);
  
  TClonesArray *GetPriHits() const 
  { return fPriHits; }
  
  //! BBC
  Int_t GetBbcNhit() const 
  { return fBbcNhit; }
  
  void AddBbcHit(
    Float_t x, Float_t y, Float_t z,
    Float_t px, Float_t py, Float_t pz,
    Float_t del, Float_t tof, Float_t len,
    Short_t pmt, Short_t pid, Int_t track, Int_t isubevent,
    Int_t mctrack, Int_t nfile);
  
  TClonesArray *GetBbcHits() const
  { return fBbcHits; }
  
  void AddBbcPara(Int_t i[], Float_t f[]);
  
  //! ZDC
  Int_t GetZdcNhit() const 
  { return fZdcNhit; }
  
  void AddZdcHit(
    Float_t xm, Float_t ym, Float_t zm,
    Float_t pxm, Float_t pym, Float_t pzm,
    Float_t dele, Float_t tof,
    Int_t pid, Int_t dir, Int_t mod, 
    Int_t track, Int_t isubevent,
    Int_t mctrack, Int_t nfile);
  TClonesArray *GetZdcHits() const 
  { return fZdcHits; }
  
  
  //! DCH
  Int_t GetDchNhit() const 
  { return fDchNhit; }
  
  void AddDchHit(
    Float_t argxyzinloc[], Float_t argtof, Float_t argxyzoutloc[],
    Int_t argplane, Int_t argcell, Float_t argxyzinglo[],
    Float_t argpathLength, Int_t argtrack, Int_t argisubevent,
    Int_t argiArm, Int_t argid, Int_t argmctrack, Int_t nfile);
  
  TClonesArray *GetDchHits() const 
  { return fDchHits; }
  
  void AddDchPara(Int_t i[], Float_t f[]);
  
  //! PAD
  Int_t GetPadNhit() const 
  { return fPadNhit; }
  
  void AddPadHit(
    Float_t xyzinloc[], Float_t xyzoutloc[], Float_t xyzinglo[],
    Float_t tof, Float_t dedx, Float_t pathLength, Int_t track,
    Int_t arm, Int_t sector, Int_t id, Int_t ipc,
    Int_t isubevent, Int_t mctrack, Int_t nfile);
  
  TClonesArray *GetPadHits() const 
  { return fPadHits; }
  
  //! MUPC
  Int_t GetMuPCNhit() const 
  { return fMuPCNhit; }
  
  void AddMuPCHit(
    Float_t xyzinloc[], Float_t xyzoutloc[], Float_t xyzinglo[],
    Float_t tof, Float_t dedx, Float_t pathLength, Int_t track,
    Int_t arm, Int_t id, Int_t ipc,
    Int_t isubevent, Int_t mctrack, Int_t nfile);
  
  TClonesArray *GetMuPCHits() const 
  { return fMuPCHits; }
  
  
  //! RLT
  Int_t GetrltNhit() const
  { return frltNhit; }
  
  void AddrltHit(
    Float_t xyzinloc[], Float_t xyzoutloc[], Float_t xyzinglo[],
    Float_t tof, Float_t dedx, Float_t pathLength, Int_t track,
    Int_t id, Int_t irpc,
    Int_t isubevent, Int_t mctrack, Int_t nfile);
  
  Int_t GetrltNpara() const 
  { return frltNpara; }
  
  TClonesArray *GetrltHits() const 
  { return frltHits; }
 
  //! NCC
  Int_t GetNCCNhit() const 
  { return fNCCNhit; }
  
  void AddNCCHit(
    Int_t evnt, Int_t incc,
    Int_t twr_id, Int_t sen_id,
    Float_t tof, Float_t dedx,
    Int_t isubevent, Int_t track, Int_t nfile);
  
  Int_t GetNCCNpara() const 
  { return fNCCNpara; }
  
  TClonesArray *GetNCCHits() const 
  { return fNCCHits; }
  
  //! MPC-EX Absorber
  Int_t GetMPCEXABSNhit() const 
  { return fMPCEXABSNhit; }
  
  void AddMPCEXABSHit(
    Int_t evnt, Int_t incc,
    Float_t dedx,
    Int_t isubevent, Int_t track, Int_t nfile);

  Int_t GetMPCEXABSNpara() const 
  { return fMPCEXABSNpara; }
  
  TClonesArray *GetMPCEXABSHits() const 
  { return fMPCEXABSHits; }

  //! MPC front plate
  Int_t GetMPCFPLTNhit() const 
  { return fMPCFPLTNhit; }
  
  void AddMPCFPLTHit(
    Int_t evnt, Int_t incc,
    Float_t dedx,
    Int_t isubevent, Int_t track, Int_t nfile);

  Int_t GetMPCFPLTNpara() const 
  { return fMPCFPLTNpara; }
  
  TClonesArray *GetMPCFPLTHits() const 
  { return fMPCFPLTHits; }

  //! MPC-EX Entry Hits
  Int_t GetMPCEXEntryNhit() const 
  { return fMPCEXEntryNhit; }
  
  void AddMPCEXEntryHit(
    Float_t vx, Float_t vy, Float_t vz,
    Float_t px, Float_t py, Float_t pz,
    Int_t isubevent,  Int_t track, Int_t nfile);

  Int_t GetMPCEXEntryNpara() const 
  { return fMPCEXEntryNpara; }
  
  TClonesArray *GetMPCEXEntryHits() const 
  { return fMPCEXEntryHits; }

  //! VNC
  Int_t GetVncNhit() const 
  { return fVncNhit; }
  
  void AddVncHit(
    Float_t xx, Float_t yy, Float_t zz,
    Float_t dedx, Float_t Xe, Float_t Ye,
    Float_t Pmom, Float_t P_id, Float_t PNum,
    Int_t track, Int_t arm, Int_t impc, Float_t itof,
    Int_t isubevent, Int_t mctrack, Int_t nfile);
  
  Int_t GetVncNpara() const 
  { return fVncNpara; }
  
  TClonesArray *GetVncHits() const 
  { return fVncHits; }
  
  //! MPC
  Int_t GetMpcNhit() const 
  { return fMpcNhit; }
  
  void AddMpcHit(
    Float_t xx, Float_t yy, Float_t zz,
    Float_t dedx, Float_t Xe, Float_t Ye,
    Float_t Pmom, Float_t P_id, Float_t PNum,
    Int_t track, Int_t arm, Int_t impc, Float_t itof,
    Int_t isubevent, Int_t mctrack, Int_t nfile);
  
  Int_t GetMpcNpara() const 
  { return fMpcNpara; }
  
  TClonesArray *GetMpcHits() const 
  { return fMpcHits; }
  
  //! SVX
  Int_t GetSvxNhit() const 
  { return fSvxNhit; }
  
  void AddSvxHit(
    Float_t xyzglobal[], Float_t pmomxyz[],
    Float_t dele, Float_t timeOfFlight,
    Float_t xyzlocalIn[], Float_t xyzlocalOut[],
    Float_t xyzglobalIn[], Float_t xyzglobalOut[],
    Int_t hitVolume[],
    Int_t track, Int_t layer, Int_t siliID,
    Int_t isubevent, Int_t mctrack, Int_t nfile);
  
  TClonesArray *GetSvxHits() const 
  { return fSvxHits; }
  
  void AddSvxPara(Int_t i[], Float_t f[]);
  
  //! Fcl
  Int_t GetFclNhit() const 
  { return fFclNhit; }
  
  void AddFclHit(
    Float_t xyzglobal[], Float_t pmomxyz[],
    Float_t dele, Int_t track, Int_t layer, Int_t fclID,
    Int_t isubevent, Int_t mctrack, Int_t nfile);
  
  TClonesArray *GetFclHits() const 
  { return fFclHits; }
  
  //! AER
  Int_t GetAerNhit() const 
  { return fAerNhit; }
  
  void AddAerHit(Float_t xyzglobal[], Float_t pmomxyz[],
    Float_t dele, Int_t track, Int_t layer, Int_t aerID,
    Float_t pathLength, Float_t tof, Float_t stepLength,
    Float_t etot, Float_t charge, Float_t momentum,
    Float_t vertxyz[], Int_t isubevent, Int_t mctrack, 
    Int_t nfile);
  
  TClonesArray *GetAerHits() const 
  { return fAerHits; }
  
  //! TFW
  Int_t GetTfwNhit() const 
  { return fTfwNhit; }
  
  void AddTfwHit(Float_t xyzinloc[], Float_t xyzoutloc[], Float_t xyzinglo[],
    Float_t tof, Float_t dedx, Float_t pathLength, Int_t track,
    Int_t sector, Int_t id,
    Int_t isubevent, Int_t mctrack, Int_t nfile);
  
  
  TClonesArray *GetTfwHits() const 
  { return fTfwHits; }
  
  //! RXN
  Int_t GetRxnNhit() const
  { return fRxnNhit; }
  
  void AddRxnHit(
    Float_t xyzinloc[], Float_t xyzoutloc[], Float_t xyzinglo[], Float_t pmomxyz[],
    Float_t tof, Float_t dedx, Float_t pathLength, Int_t track,
    Int_t sector, Int_t id,
    Int_t isubevent, Int_t mctrack, Int_t nfile);
  
  TClonesArray *GetRxnHits() const 
  { return fRxnHits; }
  
  //! HBD
  Int_t GetHbdNhit() const 
  { return fHbdNhit; }
  
  void AddHbdHit(
    Float_t xyzin[], Float_t pxyz[],
    Float_t tof, Int_t hbdID, Int_t track,
    Float_t xyzout[], Float_t dele, Float_t pathLength,
    Int_t detector, Int_t sector, Int_t padrow, Int_t detflag,
    Int_t isubevent, Int_t mctrack, Int_t nfile);
  
  TClonesArray *GetHbdHits() const 
  { return fHbdHits; }
  
  //! CRK
  Int_t GetCrkNhit() const 
  { return fCrkNhit; }
  
  void AddCrkHit(
    Short_t pmt, Float_t x, Float_t y, Float_t z, Float_t tof, Float_t px, 
    Float_t py, Float_t pz, Short_t pid, Int_t tra, Int_t parent, Int_t nbf,
    Int_t bi1, Int_t bi2, Float_t bp1, Float_t bp2, Int_t mctrack,
    Int_t nfile, Int_t isubevent);
  
  TClonesArray *GetCrkHits() const 
  { return fCrkHits; }
  
  //! Ctr
  Int_t GetCtrNhit() const 
  { return fCtrNhit; }
  
  void AddCtrHit(
    Int_t detector, Float_t x, Float_t y, Float_t z, Short_t pid, 
    Int_t itra, Float_t pvx, Float_t pvy, Float_t pvz, Float_t vx, Float_t vy, 
    Float_t vz, Int_t mctrack, Int_t nfile, Int_t isubevent);
  
  TClonesArray *GetCtrHits() const 
  { return fCtrHits; }
  
  //! TEC
  Int_t GetTecNhit() const 
  { return fTecNhit; }
  
  void AddTecHit(
    Int_t mctrack, Float_t xyzinloc[], Float_t xyzoutloc [], 
    Float_t tof, Int_t sector, Float_t dedx, Float_t xyzinglo[],
    Int_t iArm, Int_t nfile, Int_t id, Int_t plane, Int_t isubevent,
    Int_t track);
  TClonesArray *GetTecHits() const 
  { return fTecHits; }
  
  //! TOF
  Int_t GetTofNhit() const 
  { return fTofNhit; }
  
  void AddTofHit(
    Int_t subvol, Int_t panel, Int_t column, Int_t pslat,
    Int_t slat_seq, Int_t partl, Float_t xm, Float_t ym, Float_t zm,
    Float_t pos_hit_slat, Float_t pxm, Float_t pym, Float_t pzm, Float_t tof,
    Float_t dele, Int_t track, Int_t mctrack, Int_t isubevent, Int_t nfile);
  
  TClonesArray *GetTofHits() const 
  { return fTofHits; }
  
  void AddTofPara(Int_t i[], Float_t f[]);
  
  //! EMC
  Int_t GetEmcNhit() const 
  { return fEmcNhit; }
  
  void AddEmcHit(
    Int_t argi1, Float_t argdele, Float_t argposx, Float_t argposy,
    Float_t argposz, Float_t argtof, Int_t argindex1, Int_t argindex2, 
    Int_t argnumed, Int_t argadd1, Int_t argadd2, Int_t argtrack, 
    Int_t argmctrack, Int_t argisubevent, Int_t argnfile);
  
  Int_t GetEmcNpara() const 
  { return fEmcNpara; }
  
  void AddEmcPara(const Float_t f[]);
  
  TClonesArray *GetEmcHits() const 
  { return fEmcHits; }
  
  TClonesArray *GetEmcParas() const
  { return fEmcParas; }
  
  //! Mut
  Int_t GetMutNhit() const 
  { return fMutNhit; }
  
  void AddMutHit(
    Int_t track, Int_t plane, Short_t pid, Float_t t, 
    Float_t e, Float_t x, Float_t y, Float_t z, Float_t px,
    Float_t py, Float_t pz, Int_t mctrack, Int_t nfile, Int_t isubevent); 
  
  TClonesArray *GetMutHits() const 
  { return fMutHits; }
  
  Int_t GetMutNpara() const 
  { return fMutNpara; }  
  
  void AddMutPara(Int_t i[], Float_t f[]);
  
  TClonesArray *GetMutParas() const 
  { return fMutParas; }
  
  //! Mui
  Int_t GetMuiNhit() const 
  { return fMuiNhit; }
  
  void AddMuiHit(Int_t itrksub, Int_t plane_num, Int_t trk_id, Float_t tof, 
    Float_t de, Float_t rhit[],Float_t phit[],Int_t mctrack, Int_t nfile, 
    Int_t isubevent);
  
  TClonesArray *GetMuiHits() const 
  { return fMuiHits; }
  
  void AddMuiPara(Int_t i[], Float_t f[]);
  
  
  ClassDef(PISAEvent,1)
    
};

#endif

