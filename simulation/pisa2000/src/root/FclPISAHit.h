#ifndef _FclPISAHit_
#define _FclPISAHit_

// $Id: FclPISAHit.h,v 1.3 2007/11/13 22:27:44 hpereira Exp $

/*!
\file  FclPISAHit.h
\brief container for Forward calorimeter pisa hits
\author  T. K. Ghosh
\version $Revision: 1.3 $
\date    $Date: 2007/11/13 22:27:44 $
*/

#include <TObject.h>
#include <vector>

//! container for forward calorimeter pisa hits
class FclPISAHit : public TObject 
{
  
  private:
  
  //! Track number filled after read of PISA hits files
  Int_t   mctrack; 
  
  //! GEANT particle ID assigned from siliID
  Int_t   idPart;  
  Float_t xyzglobal[3] ;
  Float_t dele; 
  Float_t pmomxyz[3];
  
  //! track in subevent
  Int_t   track;  
  
  //! layer (*not* counting from 0!!)
  Int_t   layer;   
  Int_t   isubevent; 
  Int_t   nfile;
  
  //! static interface
  static std::vector<FclPISAHit> _hits;
  
  public:
  
  //! default constructor
  FclPISAHit();
  
  //! constructor
  FclPISAHit(
    Float_t xyzglobal[], Float_t pmomxyz[],
    Float_t dele, Int_t track, Int_t layer, Int_t siliID,
    Int_t isubevent,  Int_t mctrack, Int_t nfile); 
  
  //! destructor
  virtual ~FclPISAHit() 
  {}
  
  
  //!@name static interface 
  //@{
  
  static Int_t GetFclCount() 
  {return _hits.size(); }
  
  //! push hit
  static void AddHit( const FclPISAHit& hit ) 
  { _hits.push_back( hit ); }
  
  static FclPISAHit *GetFclHitEvt() 
  { return _hits.empty() ? 0: &_hits[0]; }
    
  static void FclClear()
  { _hits.clear(); }
  
  //@}
  
  Int_t GetIsubevent() const
  { return isubevent; }
  
  Int_t GetNtrack() const 
  { return track; }
  
  void SetIdPart(const Int_t siliID) 
  { idPart = siliID; }
  
  Int_t GetIdPart() const
  { return idPart; }
  
  void SetMctrack(const Int_t ntrack) 
  { mctrack = ntrack; }
  
  Int_t GetMctrack() const 
  { return mctrack; }
  
  Int_t GetLayer() const
  { return layer; }
  
  Float_t GetX() const
  { return xyzglobal[0]; }
  
  Float_t GetY() const 
  { return xyzglobal[1]; }
  
  Float_t GetZ() const
  { return xyzglobal[2]; }
  
  Float_t GetDele() const
  { return dele; }
  
  Float_t GetPx() const 
  { return pmomxyz[0]; }
  
  Float_t GetPy() const 
  { return pmomxyz[1]; }
  
  Float_t GetPz() const 
  { return pmomxyz[2]; }
  
  Int_t GetNfile() const
  { return nfile; }
  
  void SetNfile(const Int_t file) 
  { nfile = file; }
  
  ClassDef(FclPISAHit,1)  
};

#endif
