#ifndef _MPCPISAHIT_
#define _MPCPISAHIT_

// $Id: MpcPISAHit.h,v 1.8 2011/02/14 05:27:50 bbannier Exp $

/*!
  \file  MpcPISAHit.h
  \brief container for muon piston calorimeter pisa hits
  \author  M. Chiu
  \version $Revision: 1.8 $
  \date    $Date: 2011/02/14 05:27:50 $
*/

#include <TObject.h>
#include <vector>

//! container for muon piston calorimeter pisa hits
class MpcPISAHit : public TObject 
{

private:
  
  Int_t   id;
  Int_t   mctrack;
  
  // x of hit in mpc
  Float_t xx_mpc;	
  
  // y of hit in mpc
  Float_t yy_mpc;	
  
  // z of hit in mpc
  Float_t zz_mpc;	
  
  // tof of hit in mpc
  Float_t tofg_mpc;  
  
  // edep in mpc
  Float_t dedx_mpc;
  
  // X location of parent on front face of MPC
  Float_t Xe_mpc; 	
  
  // Y location of parent on front face of MPC
  Float_t Ye_mpc; 	  
  
  // Parent Mom in MPC
  Float_t Pmom_mpc; 
  
  // Parent ID
  Float_t P_id_mpc;	
  
  // particle track number?
  Float_t PNum_mpc;	
  
  // track in subevent
  Int_t   track;        
  
  // 0 = south, 1 = north
  Int_t   arm;          
  
  // simulation tower id
  Int_t   tower;        
  
  // subevent number
  Int_t   isubevent;    
  Int_t   nfile;
    
  //! static interface
  static std::vector<MpcPISAHit> _hits;
  

  public:

  //! constructor
  MpcPISAHit(
    Float_t xx_mpc = 0, 
    Float_t yy_mpc = 0, 
    Float_t zz_mpc = 0, 
    Float_t dedx_mpc = 0,
    Float_t Xe_mpc = 0, 
    Float_t Ye_mpc = 0, 
    Float_t Pmom_mpc = 0, 
    Float_t P_id_mpc = 0, 
    Float_t PNum_mpc = 0,
    Int_t track = 0, 
    Int_t arm = 0,  
    Int_t itow = 0, 
    Float_t arg_tofg = 0,
    Int_t isubevent = 0,  
    Int_t mctrack = 0, 
    Int_t nfile = 0); 
  
  //! destructor
  virtual ~MpcPISAHit() 
  {}
  
  //!@name static interface 
  //@{
  
  static Int_t GetMpcCount() 
  {return _hits.size(); }
  
  //! push hit
  static void AddHit( const MpcPISAHit& hit ) 
  { _hits.push_back( hit ); }
  
  static MpcPISAHit *GetMpcHitEvt() 
  { return _hits.empty() ? 0: &_hits[0]; }
    
  static void MpcClear()
  { _hits.clear(); }
  
  //@}
  
  void Print(Option_t *opt = "") const;
   
  void SetIarm(const Int_t iarm) 
  { arm = iarm; }
  
  Int_t GetIarm() const 
  { return arm; }
  
  Int_t GetIsubevent() const 
  { return isubevent; }
  
  Int_t GetNtrack() const 
  { return track; }
  
  void SetId(const Int_t pcID) 
  { id = pcID; }
  
  Int_t GetId() const 
  { return id; }
  
  void SetMctrack(const Int_t ntrack) 
  { mctrack = ntrack; } 
  
  Int_t GetMctrack() const
  { return mctrack; } 
  
  Int_t GetTowerID() const 
  { return tower; }
  
  Float_t GetXin() const 
  { return xx_mpc; } 
  
  Float_t GetYin() const 
  { return yy_mpc; }
  
  Float_t GetZin() const 
  { return zz_mpc; }
  
  Float_t GetTofg() const 
  { return tofg_mpc; }
  
  Float_t GetDedx() const 
  { return dedx_mpc; }
  
  Float_t GetXe() const 
  { return Xe_mpc; }
  
  Float_t GetYe() const
  { return Ye_mpc; }
  
  Float_t GetPmom() const 
  { return Pmom_mpc; }
  
  Float_t GetP_id() const 
  { return P_id_mpc; }
  
  Float_t GetPNum() const 
  { return PNum_mpc; }

  Int_t GetNfile() const 
  { return nfile; }
  
  void SetNfile(const Int_t file) 
  { nfile = file; }

  ClassDef(MpcPISAHit,1)  
};

#endif // _MPCPISAHIT_

