#ifndef _MPCEXABSPISAHit_
#define _MPCEXABSPISAHit_

#include <vector>
#include <TObject.h>
#include <cassert>

//! container for pisa hits
class MPCEXABSPISAHit : public TObject 
{
  
  private:
  Int_t evnt;
  Int_t id;
  Int_t arm;    
  Int_t mctrack;
  Float_t dedx; 
  
  //! track in subevent
  Int_t track;  
  Int_t incc; 
  Int_t isubevent; 
  Int_t nfile;
  
  //!@name static interface
  //@{
  
  //! static container
  static std::vector<MPCEXABSPISAHit> _hits;    
  
  //! number of counts in 1st arm
  static int _ncc1_count;   
  
  //! number of counts in 2nd arm
  static int _ncc2_count;     
  
  //@}
  
  public:

  //! constructor
  MPCEXABSPISAHit(
    Int_t evnt = 0,
    Int_t incc = 0, 
    Float_t dedx = 0,
    Int_t isubevent = 0, 
    Int_t track = 0, 
    Int_t nfile = 0 ); 
  
  //! destructor
  virtual ~MPCEXABSPISAHit() 
  {}
  
  //!@name static interface
  //@{
    
  //! set array size
  static void SetMPCEXABSCounts( int ncc1_count, int ncc2_count ) 
  {
    _hits.clear();
    _ncc1_count = ncc1_count;
    _ncc2_count = ncc2_count;
    if( ncc1_count + ncc2_count )
    { _hits.resize( ncc1_count + ncc2_count ); }
  }
  
  //! assign first arm hit
  static void SetMPCEXABS1HitEvt( int index, const MPCEXABSPISAHit& ref) 
  {
    assert( index < _ncc1_count );
    _hits[index] = ref;
  }

  //! assign second arm hit
  static void SetMPCEXABS2HitEvt( int index, const MPCEXABSPISAHit& ref) 
  {
    assert( index < _ncc2_count );
    _hits[_ncc1_count + index] = ref;
  }

  //! total size
  static Int_t GetMPCEXABSCount()
  {return _hits.size(); }

  //! counts in first arm
  static int GetMPCEXABS1Count() 
  {return _ncc1_count; }
    
  //! counts in second arm
  static int GetMPCEXABS2Count() 
  {return _ncc2_count; }

  //! first hit
  static MPCEXABSPISAHit *GetMPCEXABSHitEvt() 
  { return (_hits.empty()) ? 0:&_hits[0]; } 
  

  //! pointer to first hit in 1st arm
  static MPCEXABSPISAHit *GetMPCEXABS1HitEvt() 
  { return (_hits.empty()) ? 0:&_hits[0]; } 
  
  //! pointer to first hit in 2nd arm
  static MPCEXABSPISAHit *GetMPCEXABS2HitEvt() 
  { return ( static_cast<int>(_hits.size()) > _ncc1_count) ? &_hits[_ncc1_count]:0; } 
        
  //! clear array
  static void MPCEXABSClear()
  { 
    _hits.clear(); 
    _ncc1_count = 0;
    _ncc2_count = 0;
  }
  
  //@}
  
  void SetIarm(const Int_t iarm) 
  { arm = iarm; }
  
  Int_t GetIarm() const 
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
  
  Int_t GetIncc() const 
  { return incc; }
  
  Int_t GetNEvent() const 
  {return evnt; }
    
  Float_t GetDedx() const
  { return dedx; }
  
  Int_t GetNfile() const 
  { return nfile; }
  
  void SetNfile(const Int_t file) 
  { nfile = file; }
  
  ClassDef(MPCEXABSPISAHit,1)  
};

#endif // _MPCEXABSPISAHit_

