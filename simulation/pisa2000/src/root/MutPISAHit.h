#ifndef _MutPISAHit_
#define _MutPISAHit_
// $Id: MutPISAHit.h,v 1.4 2007/11/13 22:27:46 hpereira Exp $

/*!
  \file  MutPISAHit.h
  \brief container for MuTR pisa hits
  \author  T. K. Ghosh
  \version $Revision: 1.4 $
  \date    $Date: 2007/11/13 22:27:46 $
*/

#include <vector>
#include <TObject.h>
#include <TClonesArray.h>
#include <TH1.h>
#include <TMath.h>

//! container for mutr MuTr hits
class MutPISAHit : public TObject 
{
  
  private:
  
  Int_t track ;
  Int_t plane ;
  Short_t pid ;
  Float_t t ;
  Float_t e ;
  Float_t pos[3] ;
  Float_t mom[3] ;
  Int_t mctrack ;
  Int_t nfile ;
  Int_t isubevent ;
  
  //! singleton
  static std::vector<MutPISAHit> _hits;  
    
  public:

  //! filled constructor
  MutPISAHit(
    Int_t track = 0, 
    Int_t plane = 0,
    Short_t pid = 0, 
    Float_t t = 0, 
    Float_t e = 0, 
    Float_t x = 0, 
    Float_t y = 0, 
    Float_t z = 0, 
    Float_t px = 0,
    Float_t py = 0, 
    Float_t pz = 0, 
    Int_t mctrack = 0, 
    Int_t nfile = 0, 
    Int_t isubevent = 0);
  
  //! destructor
  virtual ~MutPISAHit() 
  {}

  //! @name static interface
  //@{
  
  //! number of hits
  static Int_t  GetMutCount() 
  {return _hits.size(); }
  
  //! push hit
  static void AddHit( const MutPISAHit& hit )
  { _hits.push_back( hit ); }
  
  //! events hits
  static MutPISAHit *GetMutHitEvt() 
  { return (_hits.empty()) ? 0:&_hits[0]; }
    
  //! delete hits
  static void MutClear()
  { _hits.clear(); }

  //@}
  
  //! track id
  Int_t GetNtrack() const 
  { return track; }
  
  //! detector id
  Int_t GetPlane() const 
  { return plane; }
  
  //! particle id
  Short_t GetPid() const 
  { return pid; } 
  
  //! time
  Float_t GetT() const 
  { return t; } 
  
  //! energy
  Float_t GetE() const 
  { return e; } 
  
  //! position
  Float_t GetX() const 
  { return pos[0]; } 
  
  //! position
  Float_t GetY() const 
  { return pos[1]; } 
  
  //! position
  Float_t GetZ() const 
  { return pos[2]; } 
  
  //! momentum
  Float_t GetPx() const 
  { return mom[0]; } 
  
  //! momentum
  Float_t GetPy() const 
  { return mom[1]; } 
  
  //! momentum
  Float_t GetPz() const 
  { return mom[2]; } 
  
  //! mc track id
  Int_t GetMctrack() const 
  { return mctrack; } 
  
  //! mc track id
  void SetMctrack(const Int_t ntrack)
  { mctrack = ntrack  ; } 
  
  //! file id
  Int_t GetNfile() const 
  { return nfile; } 
  
  //! file id
  void SetNfile(const Int_t file) 
  { nfile = file; }    
  
  //! subevent id
  Int_t GetIsubevent() const 
  { return isubevent; } 
  
  ClassDef(MutPISAHit,1)
};


#endif
