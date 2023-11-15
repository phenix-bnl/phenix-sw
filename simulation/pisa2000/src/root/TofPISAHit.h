#ifndef _TofPISAHit_
#define _TofPISAHit_

// $Id: TofPISAHit.h,v 1.3 2007/11/13 22:27:49 hpereira Exp $

/*!
\file  TofPISAHit.h
\brief container for time of flight pisa hits
\author  T. K. Ghosh
\version $Revision: 1.3 $
\date    $Date: 2007/11/13 22:27:49 $
*/

#include <TObject.h>
#include <vector>


//! container for time of flight pisa hits
class TofPISAHit : public TObject 
{
  
  private:
  Int_t subvol;   
  Int_t panel ;
  Int_t column;
  Int_t pslat;
  Int_t slat_seq;
  Int_t partl;
  Float_t pos_m[3];
  Float_t pos_hit_slat;
  Float_t p_m[3];
  Float_t tof;
  Float_t dele;
  Int_t   track;
  Int_t   mctrack;
  Int_t   isubevent;
  Int_t   nfile;
  
  
  //! static interface
  static std::vector<TofPISAHit> _hits;
  
  public:
  
  //! constructor
  TofPISAHit(
    Int_t subvol = 0, 
    Int_t panel = 0, 
    Int_t column = 0, 
    Int_t pslat = 0, 
    Int_t  slat_seq = 0, 
    Int_t partl = 0, 
    Float_t xm = 0, 
    Float_t ym = 0, 
    Float_t zm = 0,
    Float_t  pos_hit_slat = 0, 
    Float_t pxm = 0, 
    Float_t pym = 0, 
    Float_t pzm = 0, 
    Float_t tof = 0, 
    Float_t dele = 0, 
    Int_t track = 0, 
    Int_t mctrack = 0, 
    Int_t isubevent = 0,
    Int_t nfile = 0);  
  
  //! destructor
  virtual ~TofPISAHit() 
  {}
  
  //!@name static interface 
  //@{
  
  static Int_t GetTofCount() 
  {return _hits.size(); }
  
  //! push hit
  static void AddHit( const TofPISAHit& hit ) 
  { _hits.push_back( hit ); }
  
  static TofPISAHit *GetTofHitEvt() 
  { return _hits.empty() ? 0: &_hits[0]; }
  
  static void TofClear()
  { _hits.clear(); }
  
  //@}
  
  Int_t GetSubvol() const 
  { return subvol; }
  
  Int_t GetPanel() const 
  { return panel; }
  
  Int_t GetColumn() const 
  { return column; }
  
  Int_t GetPslat() const 
  { return pslat; }
  
  Int_t GetSlat_seq() const 
  { return slat_seq; }
  
  Int_t GetPartl() const 
  { return partl; }
  
  Float_t GetXm() const
  { return pos_m[0]; }
  
  Float_t GetYm() const 
  { return pos_m[1]; }
  
  Float_t GetZm() const 
  { return pos_m[2]; }
  
  Float_t GetPos_hit_slat() const 
  { return pos_hit_slat; }
  
  Float_t GetPxm() const 
  { return p_m[0]; }
  
  Float_t GetPym() const 
  { return p_m[1]; }
  
  Float_t GetPzm() const
  { return p_m[2]; }
  
  Float_t GetTof() const 
  { return tof; }
  
  Float_t GetDele() const
  { return dele; }
  
  Int_t GetMctrack() const
  { return mctrack; }
  
  void SetMctrack(const Int_t ntrack) 
  { mctrack = ntrack; }
  
  Int_t GetNtrack() const 
  { return track; }
  
  Int_t GetIsubevent() const 
  { return isubevent; }
  
  Int_t GetNfile() const 
  { return nfile; }
  
  void SetNfile(const Int_t file) 
  { nfile = file; }
  
  ClassDef(TofPISAHit,1) 
};

#endif
