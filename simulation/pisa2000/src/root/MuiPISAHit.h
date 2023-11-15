#ifndef _MuiPISAHit_
#define _MuiPISAHit_

// $Id: MuiPISAHit.h,v 1.3 2007/11/13 22:27:45 hpereira Exp $

/*!
  \file  MuiPISAHit.h
  \brief container for pisa hits
  \author  T. K. Ghosh
  \version $Revision: 1.3 $
  \date    $Date: 2007/11/13 22:27:45 $
*/

#include <vector>
#include <TObject.h>

//! container for pisa hits
class MuiPISAHit : public TObject 
{

  private:

  //! track in subevent number
  Int_t itrksub   ;  
  Int_t plane_num ;
  Int_t trk_id    ;
  Float_t tof     ;
  Float_t de      ;
  Float_t rhit[3] ;
  Float_t phit[3];
  
  //! unique track number assigned in PHOOL
  Int_t mctrack ;    
  Int_t nfile ;
  Int_t isubevent ;
  
  //! singleton
  static std::vector<MuiPISAHit> _hits;  

  public:

  //! default constructor
  MuiPISAHit( void );
  
  //! constructor
  MuiPISAHit(
    Int_t itrksub, 
    Int_t plane_num, 
    Int_t trk_id, 
    Float_t tof, 
    Float_t de, 
    Float_t rhit[],
    Float_t phit[],
    Int_t mctrack, 
    Int_t nfile, 
    Int_t isubevent);    

  //! destructor
  virtual ~MuiPISAHit() 
  { }

  //! @name static hits container interface
  //@{
  
  //! number of hits
  static Int_t GetMuiCount() 
  {return _hits.size(); }
    
  //! push hit
  static void AddHit( const MuiPISAHit& hit )
  { _hits.push_back( hit ); }
  
  //! events hits
  static MuiPISAHit *GetMuiHitEvt() 
  { return (_hits.empty()) ? 0:&_hits[0]; }
    
  //! delete hits
  static void MuiClear()
  { _hits.clear(); }

  //@}
  
  Int_t GetNtrack() const
  { return itrksub;}
  
  Int_t GetPlane_num() const 
  { return plane_num;}
  
  Int_t GetTrk_id() const 
  { return trk_id;}
  
  Float_t GetTof() const 
  { return tof; } 
  
  Float_t GetDe() const 
  { return de; }
  
  Float_t GetRhit1() const
  { return rhit[0]; }
  
  Float_t GetRhit2() const 
  { return rhit[1]; }
  
  Float_t GetRhit3() const 
  { return rhit[2]; }
  
  Float_t GetPhit1() const 
  { return phit[0]; }
  
  Float_t GetPhit2() const 
  { return phit[1]; }
  
  Float_t GetPhit3() const 
  { return phit[2]; }
  
  Int_t GetMctrack() const 
  { return mctrack; }
  
  void SetMctrack(const Int_t ntrack) 
  { mctrack = ntrack  ; }
  
  Int_t GetNfile() const 
  { return nfile; }
  
  void SetNfile(const Int_t file) 
  { nfile = file; }   
  
  Int_t GetIsubevent() const 
  { return isubevent; }
  
  ClassDef(MuiPISAHit,1) 
};


#endif
