#ifndef _NCCPISAHit_
#define _NCCPISAHit_
// $Id: NCCPISAHit.h,v 1.11 2009/06/25 21:52:50 pinkenbu Exp $

/*!
  \file  MutPISAHit.h
  \brief container for pisa hits
  \author  Vasily Dzhordzhadze, H. Pereira
  \version $Revision: 1.11 $
  \date    $Date: 2009/06/25 21:52:50 $
*/

#include <vector>
#include <TObject.h>
#include <cassert>

//! container for pisa hits
class NCCPISAHit : public TObject 
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
  Int_t twrid;  
  Int_t senid;
  Float_t tof;
  Int_t isubevent; 
  Int_t nfile;
  
  //!@name static interface
  //@{
  
  //! static container
  static std::vector<NCCPISAHit> _hits;    
  
  //! number of counts in 1st arm
  static int _ncc1_count;   
  
  //! number of counts in 2nd arm
  static int _ncc2_count;     
  
  //@}
  
  public:

  //! constructor
  NCCPISAHit(
    Int_t evnt = 0,
    Int_t incc = 0, 
    Int_t twrid = 0, 
    Int_t senid = 0, 
    Float_t tof = 0, 
    Float_t dedx = 0,
    Int_t isubevent = 0, 
    Int_t track = 0, 
    Int_t nfile = 0 ); 
  
  //! destructor
  virtual ~NCCPISAHit() 
  {}
  
  //!@name static interface
  //@{
    
  //! set array size
  static void SetNCCCounts( int ncc1_count, int ncc2_count ) 
  {
    _hits.clear();
    _ncc1_count = ncc1_count;
    _ncc2_count = ncc2_count;
    if( ncc1_count + ncc2_count )
    { _hits.resize( ncc1_count + ncc2_count ); }
  }
  
  //! assign first arm hit
  static void SetNCC1HitEvt( int index, const NCCPISAHit& ref) 
  {
    assert( index < _ncc1_count );
    _hits[index] = ref;
  }

  //! assign second arm hit
  static void SetNCC2HitEvt( int index, const NCCPISAHit& ref) 
  {
    assert( index < _ncc2_count );
    _hits[_ncc1_count + index] = ref;
  }

  //! total size
  static Int_t GetNCCCount()
  {return _hits.size(); }

  //! counts in first arm
  static int GetNCC1Count() 
  {return _ncc1_count; }
    
  //! counts in second arm
  static int GetNCC2Count() 
  {return _ncc2_count; }

  //! first hit
  static NCCPISAHit *GetNCCHitEvt() 
  { return (_hits.empty()) ? 0:&_hits[0]; } 
  

  //! pointer to first hit in 1st arm
  static NCCPISAHit *GetNCC1HitEvt() 
  { return (_hits.empty()) ? 0:&_hits[0]; } 
  
  //! pointer to first hit in 2nd arm
  static NCCPISAHit *GetNCC2HitEvt() 
  { return ( static_cast<int>(_hits.size()) > _ncc1_count) ? &_hits[_ncc1_count]:0; } 
        
  //! clear array
  static void NCCClear()
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
  
  Int_t GetSENID() const 
  { return senid; }
  
  Int_t GetTWRID() const 
  { return twrid; }
  
  Float_t GetTOFIN() const 
  { return tof; }
  
  Float_t GetDedx() const
  { return dedx; }
  
  Int_t GetNfile() const 
  { return nfile; }
  
  void SetNfile(const Int_t file) 
  { nfile = file; }
  
  ClassDef(NCCPISAHit,1)  
};

#endif // _NCCPISAHit_

