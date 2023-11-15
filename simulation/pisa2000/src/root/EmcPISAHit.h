#ifndef _EmcPISAHit_
#define _EmcPISAHit_

// $Id: EmcPISAHit.h,v 1.3 2007/11/13 22:27:44 hpereira Exp $

/*!
  \file  EmcPISAHit.h
  \brief container for EMCal pisa hits
  \author  T. K. Ghosh
  \version $Revision: 1.3 $
  \date    $Date: 2007/11/13 22:27:44 $
*/

#include <TObject.h>
#include <vector>

//! container for EMCal pisa hits
class EmcPISAHit : public TObject 
{
  
  private:
  
  //! detector counter
  Int_t i1;            
  
  //! energy loss in scintillator layer or Cerenkov (GeV)
  Float_t dele;        
  //! position along East (-) West (+) axis (cm)
  Float_t posx;        
  
  //! position along Down (-) Up (+) axis (cm)
  Float_t posy;        
  
  //! position along South (-) North (+) axis (cm)
  Float_t posz;       
  
  //! time of flight (ns)
  Float_t tof;        
  
  //! super-module pointer
  Int_t index1;       
  
  //! cell pointer
  Int_t index2;       
  
  //! medium number
  Int_t numed;        
  
  //! first space for additional variable (use for wall now)
  Int_t add1;         
  
  //! second space for additional variable (use for itype now)
  Int_t add2;         
  
  //! track in subevent (in PISA)
  Int_t track;         
  
  //! subevent number (in PISA)
  Int_t isubevent;    
  
  //! unique track number (set off-line)
  Int_t mctrack;      
  
  //! file number (depends on MERGE, set off-line)
  Int_t nfile;         

  //! static interface
  static std::vector<EmcPISAHit> _hits;
  
 public:

  //! default constructor
  EmcPISAHit(
    Int_t argi1 = 0, 
    Float_t argdele = 0, 
    Float_t argposx = 0,
    Float_t argposy = 0,
    Float_t argposz = 0, 
    Float_t argtof = 0, 
    Int_t argindex1 = 0, 
    Int_t argindex2 = 0, 
    Int_t argnumed = 0,
    Int_t argadd1 = 0,
    Int_t argadd2 = 0,
    Int_t argtrack = 0, 
    Int_t argmctrack = 0,
    Int_t argisubevent = 0,
    Int_t argnfile = 0); 

  //! destructor
  virtual ~EmcPISAHit() 
  {}
  

  //!@name static interface 
  //@{
  
  static Int_t GetEmcCount() 
  {return _hits.size(); }
  
  //! push hit
  static void AddHit( const EmcPISAHit& hit ) 
  { _hits.push_back( hit ); }
  
  static EmcPISAHit *GetEmcHitEvt() 
  { return _hits.empty() ? 0: &_hits[0]; }
    
  static void EmcClear()
  { _hits.clear(); }
  
  //@}
  
  Int_t GetI1() const 
  { return i1; }
  
  Float_t GetDele() const 
  { return dele; }
  
  Float_t GetPosx() const 
  { return posx; }
  
  Float_t GetPosy() const 
  { return posy; }
  
  Float_t GetPosz() const 
  { return posz; }
  
  Float_t GetTof() const 
  { return tof; }
  
  Int_t GetWall() const
  { return add1; }
  
  Int_t GetItype() const 
  { return add2; }
  
  Int_t GetIndex1() const
  { return index1; }
  
  Int_t GetIndex2() const 
  { return index2; }
  
  Int_t GetNumed() const
  { return numed; }
  
  Int_t GetIsubevent() const 
  { return isubevent; }
  
  Int_t GetNtrack() const 
  { return track; }
  
  void SetMctrack(const Int_t ntrack) 
  { mctrack = ntrack; }
  
  Int_t GetMctrack() const 
  { return mctrack; }
  
  Int_t GetNfile() const 
  { return nfile; }
  
  void SetNfile(const Int_t file) 
  { nfile = file; }

  ClassDef(EmcPISAHit,1)  // An EMCal hit instance

};

#endif




