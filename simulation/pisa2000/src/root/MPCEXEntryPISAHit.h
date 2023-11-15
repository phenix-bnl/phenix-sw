#ifndef _MPCEXEntryPISAHit_
#define _MPCEXEntryPISAHit_

#include <vector>
#include <TObject.h>
#include <cassert>

//! container for pisa hits
class MPCEXEntryPISAHit : public TObject 
{
  
  private:
  Float_t vx; 
  Float_t vy; 
  Float_t vz; 
  Float_t px; 
  Float_t py; 
  Float_t pz; 
  
  //! track in subevent
  Int_t track;  
  Int_t isubevent; 
  Int_t nfile;

  Int_t mctrack; 
  
  //!@name static interface
  //@{
  
  //! static container
  static std::vector<MPCEXEntryPISAHit> _hits;    
  
  //! number of counts
  static int _count;   
    
  //@}
  
  public:

  //! constructor
  MPCEXEntryPISAHit(
    Float_t vx = 0.0,
    Float_t vy = 0.0,
    Float_t vz = 0.0,
    Float_t px = 0.0,
    Float_t py = 0.0,
    Float_t pz = 0.0,
    Int_t isubevent = 0, 
    Int_t track = 0, 
    Int_t nfile = 0); 
  
  //! destructor
  virtual ~MPCEXEntryPISAHit() 
  {}
  
  //!@name static interface
  //@{
    
  //! set array size
  static void SetMPCEXEntryCount( int count ) 
  {
    _hits.clear();
    _count = count;
    if( _count )
    { _hits.resize( _count ); }
  }
  
  //! assign first arm hit
  static void SetMPCEXEntryHitEvt( int index, const MPCEXEntryPISAHit& ref) 
  {
    assert( index < _count );
    _hits[index] = ref;
  }

  //! total size
  static Int_t GetMPCEXEntryCount()
  {return _hits.size(); }

  //! first hit
  static MPCEXEntryPISAHit *GetMPCEXEntryHitEvt() 
  { return (_hits.empty()) ? 0:&_hits[0]; } 
  
  //! clear array
  static void MPCEXEntryClear()
  { 
    _hits.clear(); 
    _count = 0;
  }
  
  //@}
    
  Int_t GetIsubevent() const 
  { return isubevent; }
  
  Int_t GetNtrack() const 
  { return track; } 
    
  void SetMctrack(const Int_t ntrack) 
  { mctrack = ntrack; } 
  
  Int_t GetMctrack() const 
  { return mctrack; } 

  Float_t GetVx() const
  { return vx; }

  Float_t GetVy() const
  { return vy; }

  Float_t GetVz() const
  { return vz; }

  Float_t GetPx() const
  { return px; }

  Float_t GetPy() const
  { return py; }

  Float_t GetPz() const
  { return pz; }
  
  Int_t GetNfile() const 
  { return nfile; }
  
  void SetNfile(const Int_t file) 
  { nfile = file; }
  
  ClassDef(MPCEXEntryPISAHit,1)  
};

#endif // _MPCEXEntryPISAHit_

