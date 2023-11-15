#ifndef _VNCPISAHIT_
#define _VNCPISAHIT_

// $Id: VncPISAHit.h,v 1.2 2015/03/19 16:57:01 chiu Exp $

/*!
  \file  VncPISAHit.h
  \brief container for Veto Nosecone Calorimeter pisa hits
  \author  M. Chiu
  \version $Revision: 1.2 $
  \date    $Date: 2015/03/19 16:57:01 $
*/

#include <TObject.h>
#include <vector>

//! container for muon piston calorimeter pisa hits
class VncPISAHit : public TObject 
{

private:
  
  Int_t   id;
  Int_t   mctrack;
  
  // x of hit in vnc
  Float_t xx_vnc;	
  
  // y of hit in vnc
  Float_t yy_vnc;	
  
  // z of hit in vnc
  Float_t zz_vnc;	
  
  // tof of hit in vnc
  Float_t tofg_vnc;  
  
  // edep in vnc
  Float_t dedx_vnc;
  
  // X location of parent on front face of MPC
  Float_t Xe_vnc; 	
  
  // Y location of parent on front face of MPC
  Float_t Ye_vnc; 	  
  
  // Parent Mom in MPC
  Float_t Pmom_vnc; 
  
  // Parent ID
  Float_t P_id_vnc;	
  
  // particle track number?
  Float_t PNum_vnc;	
  
  // track in subevent
  Int_t   track;        
  
  // 0 = south, 1 = north
  Int_t   arm;          
  
  // simulation layer id
  Int_t   layer;        
  
  // subevent number
  Int_t   isubevent;    
  Int_t   nfile;
    
  //! static interface
  static std::vector<VncPISAHit> _hits;
  

  public:

  //! constructor
  VncPISAHit(
    Float_t xx_vnc = 0, 
    Float_t yy_vnc = 0, 
    Float_t zz_vnc = 0, 
    Float_t dedx_vnc = 0,
    Float_t Xe_vnc = 0, 
    Float_t Ye_vnc = 0, 
    Float_t Pmom_vnc = 0, 
    Float_t P_id_vnc = 0, 
    Float_t PNum_vnc = 0,
    Int_t track = 0, 
    Int_t arm = 0,  
    Int_t itow = 0, 
    Float_t arg_tofg = 0,
    Int_t isubevent = 0,  
    Int_t mctrack = 0, 
    Int_t nfile = 0); 
  
  //! destructor
  virtual ~VncPISAHit() 
  {}
  
  //!@name static interface 
  //@{
  
  static Int_t GetVncCount() 
  {return _hits.size(); }
  
  //! push hit
  static void AddHit( const VncPISAHit& hit ) 
  { _hits.push_back( hit ); }
  
  static VncPISAHit *GetVncHitEvt() 
  { return _hits.empty() ? 0: &_hits[0]; }
    
  static void VncClear()
  { _hits.clear(); }
  
  //@}
  
  void Print(Option_t *opt = "") const;
   
  void SetArm(const Int_t iarm) 
  { arm = iarm; }
  
  Int_t GetArm() const 
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
  
  Int_t GetLayer() const 
  { return layer; }
  
  Float_t GetXin() const 
  { return xx_vnc; } 
  
  Float_t GetYin() const 
  { return yy_vnc; }
  
  Float_t GetZin() const 
  { return zz_vnc; }
  
  Float_t GetTofg() const 
  { return tofg_vnc; }
  
  Float_t GetDedx() const 
  { return dedx_vnc; }
  
  Float_t GetXe() const 
  { return Xe_vnc; }
  
  Float_t GetYe() const
  { return Ye_vnc; }
  
  Float_t GetPmom() const 
  { return Pmom_vnc; }
  
  Float_t GetP_id() const 
  { return P_id_vnc; }
  
  Float_t GetPNum() const 
  { return PNum_vnc; }

  Int_t GetNfile() const 
  { return nfile; }
  
  void SetNfile(const Int_t file) 
  { nfile = file; }

  ClassDef(VncPISAHit,1)  
};

#endif // _VNCPISAHIT_

