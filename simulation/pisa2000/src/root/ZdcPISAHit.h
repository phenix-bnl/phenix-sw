#ifndef _ZdcPISAHit_
#define _ZdcPISAHit_

// $Id: ZdcPISAHit.h,v 1.4 2007/11/13 22:27:49 hpereira Exp $

/*!
\file  ZdcPISAHit.h
\brief container for zero degree calorimeter pisa hits
\author  T. K. Ghosh
\version $Revision: 1.4 $
\date    $Date: 2007/11/13 22:27:49 $
*/

#include <TObject.h>
#include <vector>

//! container for zero degree calorimeter pisa hits
class ZdcPISAHit : public TObject 
{
  
  private:
  Float_t pos_m[3];
  
  //! energy loss in cherenkov fiber
  Float_t dele;		
  Float_t p_m[3];
  
  //! time of flight (ns)
  Float_t tof;		
  
  //! geant particle id
  Int_t partl;		
  
  //! north (1) or south (0)
  Int_t north_south;
  
  //! module
  Int_t module;	
  
  //! track in subevent (in PISA)
  Int_t track;		
  
  //! subevent number (in PISA)
  Int_t isubevent;
  
  //! unique track number (set off-line)
  Int_t mctrack;	
  
  //! file number (depends on MERGE, set off-line)
  Int_t nfile;		
  
  //! static interface
  static std::vector<ZdcPISAHit> _hits;
  
  public:
  
  //! constructor
  ZdcPISAHit(
    Float_t xm = 0,
    Float_t ym = 0,
    Float_t zm = 0,
    Float_t pxm = 0,
    Float_t pym = 0,
    Float_t pzm = 0,
    Float_t dele = 0,
    Float_t tof = 0, 
    Int_t   pid = 0,
    Int_t   dir = 0,
    Int_t   mod = 0,
    Int_t   track = 0,
    Int_t   isubevent = 0,
    Int_t   mctrack = 0,
    Int_t   nfile = 0);
  
  //! destructor
  virtual ~ZdcPISAHit() 
  {}
  
  
  //!@name static interface 
  //@{
  
  static Int_t GetZdcCount() 
  {return _hits.size(); }
  
  //! push hit
  static void AddHit( const ZdcPISAHit& hit ) 
  { _hits.push_back( hit ); }
  
  static ZdcPISAHit *GetZdcHitEvt() 
  { return _hits.empty() ? 0: &_hits[0]; }
  
  static void ZdcClear()
  { _hits.clear(); }
  
  //@}
  
  Float_t GetXm() const 
  { return pos_m[0]; }
  
  Float_t GetYm() const 
  { return pos_m[1]; }
  
  Float_t GetZm() const 
  { return pos_m[2]; }
  
  Float_t GetDele() const 
  { return dele; }
  
  Float_t GetPxm() const
  { return p_m[0]; }
  
  Float_t GetPym() const
  { return p_m[1]; }
  
  Float_t GetPzm() const 
  { return p_m[2]; }
  
  Float_t GetTof() const 
  { return tof; }
  
  Int_t GetPartl() const 
  { return partl; }
  
  Int_t GetDirection() const 
  { return north_south; }
  
  Int_t GetNmodule() const
  { return module; }
  
  Int_t GetNtrack() const 
  { return track; }
  
  void SetMctrack(const Int_t itrack) 
  { mctrack = itrack; }
  
  Int_t GetMctrack() const 
  { return mctrack; }
  
  Int_t GetIsubevent() const 
  { return isubevent; }
  
  Int_t GetNfile() const 
  { return nfile; }
  
  void SetNfile(const Int_t file) 
  { nfile = file; }
  
  ClassDef(ZdcPISAHit,1)  
    
};

#endif
