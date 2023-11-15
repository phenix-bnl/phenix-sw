#ifndef _CrkPISAHit_
#define _CrkPISAHit_

// $Id: CrkPISAHit.h,v 1.3 2007/11/13 22:27:43 hpereira Exp $

/*!
\file  CrkPISAHit.h
\brief container for RICH pisa hits
\author  T. K. Ghosh
\version $Revision: 1.3 $
\date    $Date: 2007/11/13 22:27:43 $
*/

#include <TObject.h>
#include <vector>

//! container for RICH pisa hits
class CrkPISAHit : public TObject 
{
  
  private:
  
  Short_t pmt   ;
  Float_t pos[3] ;
  Float_t tof  ;
  Float_t mom[3];
  Short_t pid ;
  Int_t tra   ;
  Int_t parent ;
  Int_t nbf   ;
  Int_t bi1 ;
  Int_t bi2  ;
  Float_t bp1  ;
  Float_t bp2  ;
  Int_t mctrack;
  Int_t nfile;
  Int_t isubevent;
  
  //! static interface
  static std::vector<CrkPISAHit> _hits;
  
  public:
  
  //! constructor
  CrkPISAHit(
    Short_t pmt = 0, 
    Float_t x = 0, 
    Float_t y = 0, 
    Float_t z = 0, 
    Float_t tof = 0, 
    Float_t px = 0, 
    Float_t py = 0, 
    Float_t pz = 0, 
    Short_t pid = 0, 
    Int_t tra = 0, 
    Int_t parent = 0, 
    Int_t nbf = 0,
    Int_t bi1 = 0, 
    Int_t bi2 = 0, 
    Float_t bp1 = 0, 
    Float_t bp2 = 0, 
    Int_t mctrack = 0,
    Int_t nfile = 0, 
    Int_t isubevent = 0);   
  
  //! destructor
  virtual ~CrkPISAHit() 
  {}
  //!@name static interface 
  //@{
  
  static Int_t GetCrkCount() 
  {return _hits.size(); }
  
  //! push hit
  static void AddHit( const CrkPISAHit& hit ) 
  { _hits.push_back( hit ); }
  
  static CrkPISAHit *GetCrkHitEvt() 
  { return _hits.empty() ? 0: &_hits[0]; }
    
  static void CrkClear()
  { _hits.clear(); }
  
  //@}    
    
  Short_t GetPmt() const 
  { return pmt; }
  
  Float_t GetX() const 
  { return pos[0]; }
  
  Float_t GetY() const 
  { return pos[1]; }
  
  Float_t GetZ() const 
  { return pos[2]; }
  
  Float_t GetTof() const 
  { return tof; }
  
  Float_t GetPx() const 
  { return mom[0]; }
  
  Float_t GetPy() const 
  { return mom[1]; }
  
  Float_t GetPz() const 
  { return mom[2]; }
  
  Short_t GetPid() const 
  { return pid; }
  
  Int_t GetNtrack() const 
  {return tra;}
  
  Int_t GetParent() const 
  { return parent; }
  
  Int_t GetNbf() const 
  { return nbf; }
  
  Int_t GetBi1() const 
  { return bi1; }
  
  Int_t GetBi2() const 
  { return bi2; }
  
  Float_t GetBp1() const 
  { return bp1; }
  
  Float_t GetBp2() const 
  { return bp2; }
  
  Int_t GetMctrack() const
  { return mctrack; }
  
  void SetMctrack(const Int_t ntrack) 
  { mctrack =  ntrack  ; }
  
  Int_t GetIsubevent() const 
  { return isubevent; }
  
  Int_t GetNfile() const 
  { return nfile; }
  
  void SetNfile(const Int_t file) 
  { nfile = file; }
  
  ClassDef(CrkPISAHit,1)  
    
};

#endif
