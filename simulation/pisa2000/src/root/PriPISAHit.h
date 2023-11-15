#ifndef _PRIPISAHIT_
#define _PRIPISAHIT_

// $Id: PriPISAHit.h,v 1.3 2007/11/13 22:27:47 hpereira Exp $

/*!
  \file  PriPISAHit.h
  \brief container for primary particles
  \author  T. K. Ghosh
  \version $Revision: 1.3 $
  \date    $Date: 2007/11/13 22:27:47 $
*/

#include <TObject.h>
#include <vector>

//! container for primary particles
class PriPISAHit : public TObject 
{
  private:
  
  //! track number in event file
  Int_t evttrack;   
  
  //! particle ID number
  Int_t idpart;     
  
  //! X component of momentum
  Float_t px;       
  
  //! Y component of momentum
  Float_t py;       
  
  //! Z component of momentum
  Float_t pz;       
  
  //! track number in PISA subevent
  Int_t ntrack   ;  
  
  //! subevent in PISA
  Int_t isubevent; 
  
  //! unique track number created in Off-Line
  Int_t true_track;
  
  //! MERGE file number
  Int_t nfile;      
   
  //! static interface
  static std::vector<PriPISAHit> _hits;
  

  public:

  //! constructor
  PriPISAHit(
    Int_t true_track = 0, 
    Int_t isubevent = 0,  
    Int_t ntrack = 0, 
    Int_t idpart = 0, 
    Float_t px = 0, 
    Float_t py = 0, 
    Float_t pz = 0, 
    Int_t evttrack = 0,
    Int_t nfile = 0);
  
  //! destructor
  virtual ~PriPISAHit() 
  {}

  //!@name static interface 
  //@{
  
  static Int_t GetPriCount() 
  {return _hits.size(); }
  
  //! push hit
  static void AddHit( const PriPISAHit& hit ) 
  { _hits.push_back( hit ); }
  
  static PriPISAHit *GetPriHitEvt() 
  { return _hits.empty() ? 0: &_hits[0]; }
    
  static void PriClear()
  { _hits.clear(); }
  
  //@}
  
  void SetMctrack(const Int_t track) 
  { SetTrue_track( track ); }
  
  void SetTrue_track(const Int_t track) 
  {true_track = track; }
  
  Int_t GetTrue_track() const 
  {return true_track; }
  
  Int_t GetIsubevent() const
  {return isubevent; }
  
  Int_t GetNtrack() const 
  {return ntrack; }
  
  Int_t GetIdpart() const 
  {return idpart; }
  
  Int_t GetNfile() const 
  {return nfile; }
  
  void SetNfile(const Int_t kfile) 
  {nfile = kfile; }
  
  Float_t GetPx() const 
  { return px; }
  
  Float_t GetPy() const 
  { return py; }
  
  Float_t GetPz() const
  { return pz; }
  
  Int_t GetEvttrack() const 
  { return evttrack; }
  
  ClassDef(PriPISAHit, 1)
};

#endif



