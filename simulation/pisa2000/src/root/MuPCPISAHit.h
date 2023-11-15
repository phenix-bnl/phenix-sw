#ifndef _MuPCPISAHit_
#define _MuPCPISAHit_

// $Id: MuPCPISAHit.h,v 1.4 2009/06/25 21:52:50 pinkenbu Exp $

/*!
\file  MuPCPISAHit.h
\brief container for muon pad chambers pisa hits
\author  W. Xie
\version $Revision: 1.4 $
\date    $Date: 2009/06/25 21:52:50 $
*/

#include <TObject.h>
#include <vector>
#include <cassert>

//! container for muon pad chambers pisa hits
class MuPCPISAHit : public TObject 
{
  
  private:
  Int_t   id;
  Int_t   mctrack;
  Float_t xyzinloc[3] ;
  Float_t xyzoutloc[3];
  Float_t tof ;
  Float_t dedx; 
  Float_t xyzinglo[3];
  Float_t pathLength;
  
  //! track in subevent
  Int_t   track;   
  Int_t   arm;
  
  //! which of PC1, PC2, or PC3 (*not* counting from 0!!)
  Int_t   ipc;     
  Int_t   isubevent; 
  Int_t   nfile;
  
  //!@name static interface
  //@{
  
  //! static container
  static std::vector<MuPCPISAHit> _hits;    
  
  //! number of counts in first pad chamber
  static int _mupc1_count;   
  
  //! number of counts in 2nd pad chamber
  static int _mupc2_count;     
  
  //! number of counts in 3rd pad chamber
  static int _mupc3_count;     
  
  //@}
  
  public:
  
  //! default constructor
  MuPCPISAHit();
  
  //! constructor
  MuPCPISAHit(
    Float_t xyzinloc[], Float_t xyzoutloc[], Float_t xyzinglo[],
    Float_t tof, Float_t dedx, Float_t pathLength, Int_t track,
    Int_t arm, Int_t id, Int_t ipc,
    Int_t isubevent,  Int_t mctrack, Int_t nfile); 
  
  //! destructor
  virtual ~MuPCPISAHit() 
  {}
  
  //!@name static interface
  //@{
  
  //! set array size
  static void SetMuPCCounts( int mupc1_count, int mupc2_count, int mupc3_count ) 
  {
    _hits.clear();
    _mupc1_count = mupc1_count;
    _mupc2_count = mupc2_count;
    _mupc3_count = mupc3_count;
    if( mupc1_count + mupc2_count + mupc3_count )
    { _hits.resize( mupc1_count + mupc2_count + mupc3_count ); }
  }
  
  //! assign first chamber hit
  static void SetMuPC1HitEvt( int index, const MuPCPISAHit& ref) 
  {
    assert( index < _mupc1_count );
    _hits[index] = ref;
  }
  
  //! assign second chamber hit
  static void SetMuPC2HitEvt( int index, const MuPCPISAHit& ref) 
  {
    assert( index < _mupc2_count );
    _hits[_mupc1_count + index] = ref;
  }
  
  //! assign third chamber hit
  static void SetMuPC3HitEvt( int index, const MuPCPISAHit& ref) 
  {
    assert( index < _mupc3_count );
    _hits[_mupc1_count + _mupc2_count + index] = ref;
  }
  
  //! total size
  static Int_t GetMuPCCount()
  {return _hits.size(); }
  
  //! counts in first chamber
  static int GetMuPC1Count() 
  {return _mupc1_count; }
  
  //! counts in second chamber
  static int GetMuPC2Count() 
  {return _mupc2_count; }
  
  //! counts in third chamber
  static int GetMuPC3Count() 
  {return _mupc3_count; }
  
  //! first hit
  static MuPCPISAHit *GetMuPCHitEvt() 
  { return (_hits.empty()) ? 0:&_hits[0]; } 
  
  //! pointer to first hit in 1st chamber
  static MuPCPISAHit *GetMuPC1HitEvt() 
  { return (_hits.empty()) ? 0:&_hits[0]; } 
  
  //! pointer to first hit in 2nd chamber
  static MuPCPISAHit *GetMuPC2HitEvt() 
  { return ( static_cast<int>(_hits.size()) > _mupc1_count) ? &_hits[_mupc1_count]:0; } 
  
  //! pointer to first hit in third chamber
  static MuPCPISAHit *GetMuPC3HitEvt() 
  { return ( static_cast<int>(_hits.size()) > _mupc1_count + _mupc2_count ) ? &_hits[_mupc1_count + _mupc2_count]:0; } 
  
  //! clear array
  static void MuPCClear()
  { 
    _hits.clear(); 
    _mupc1_count = 0;
    _mupc2_count = 0;
    _mupc3_count = 0;
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
  
  Int_t GetIpc() const 
  { return ipc; }
  
  Float_t GetXin() const 
  { return xyzinloc[0]; }
  
  Float_t GetYin() const 
  { return xyzinloc[1]; }
  
  Float_t GetZin() const 
  { return xyzinloc[2]; }
  
  Float_t GetXout() const 
  { return xyzoutloc[0]; }
  
  Float_t GetYout() const 
  { return xyzoutloc[1]; }
  
  Float_t GetZout() const
  { return xyzoutloc[2]; }
  
  Float_t GetTof() const 
  { return tof; }
  
  Float_t GetDedx() const 
  { return dedx; }
  
  Float_t GetXing() const 
  { return xyzinglo[0]; }
  
  Float_t GetYing() const 
  { return xyzinglo[1]; }
  
  Float_t GetZing() const 
  { return xyzinglo[2]; }
  
  Float_t GetPathLength() const 
  { return pathLength; }
  
  Int_t GetNfile() const 
  { return nfile; }
  
  void SetNfile(const Int_t file) 
  { nfile = file; }
  
  ClassDef(MuPCPISAHit,1)  
};

#endif
