#ifndef _RLTPISAHit_
#define _RLTPISAHit_
// $Id: RLTPISAHit.h,v 1.3 2009/06/25 21:52:50 pinkenbu Exp $

/*!
\file  RLTPISAHit.h
\brief container for relative luminosity telescope pisa hits
\author  T. K. Ghosh
\version $Revision: 1.3 $
\date    $Date: 2009/06/25 21:52:50 $
*/


#include <TObject.h>
#include <vector>
#include <cassert>

//! container for relative luminosity telescope pisa hits
class RLTPISAHit : public TObject 
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
  
  // track in subevent
  Int_t   track;   
  
  // which of RPC1, RPC2, RPC3 (*not* counting from 0!!)
  Int_t irpc;    
  Int_t isubevent; 
  Int_t nfile;
  
  //!@name static interface
  //@{
  
  //! static container
  static std::vector<RLTPISAHit> _hits;    
  
  //! number of counts in first pad chamber
  static int _rpc1_count;   
  
  //! number of counts in 2nd pad chamber
  static int _rpc2_count;     
  
  //! number of counts in 3rd pad chamber
  static int _rpc3_count;     
  
  //@}
  
  public:
  
  //! default constructor
  RLTPISAHit();  
  
  //! constructor
  RLTPISAHit(
    Float_t xyzinloc[], Float_t xyzoutloc[], Float_t xyzinglo[],
    Float_t tof, Float_t dedx, Float_t pathLength, Int_t track,
    Int_t id, Int_t irpc,
    Int_t isubevent,  Int_t mctrack, Int_t nfile); 
  
  //! destructor
  virtual ~RLTPISAHit() 
  {}
  
  //!@name static interface
  //@{
  
  //! set array size
  static void SetrltCounts( int rpc1_count, int rpc2_count, int rpc3_count ) 
  {
    _hits.clear();
    _rpc1_count = rpc1_count;
    _rpc2_count = rpc2_count;
    _rpc3_count = rpc3_count;
    if( rpc1_count + rpc2_count + rpc3_count )
    { _hits.resize( rpc1_count + rpc2_count + rpc3_count ); }
  }
  
  //! assign first chamber hit
  static void SetrltRPC1HitEvt( int index, const RLTPISAHit& ref) 
  {
    assert( index < _rpc1_count );
    _hits[index] = ref;
  }
  
  //! assign second chamber hit
  static void SetrltRPC2HitEvt( int index, const RLTPISAHit& ref) 
  {
    assert( index < _rpc2_count );
    _hits[_rpc1_count + index] = ref;
  }
  
  //! assign third chamber hit
  static void SetrltRPC3HitEvt( int index, const RLTPISAHit& ref) 
  {
    assert( index < _rpc3_count );
    _hits[_rpc1_count + _rpc2_count + index] = ref;
  }
  
  //! total size
  static Int_t GetrltCount()
  {return _hits.size(); }
  
  //! counts in first chamber
  static int GetrltRPC1Count() 
  {return _rpc1_count; }
  
  //! counts in second chamber
  static int GetrltRPC2Count() 
  {return _rpc2_count; }
  
  //! counts in third chamber
  static int GetrltRPC3Count() 
  {return _rpc3_count; }
  
  //! first hit
  static RLTPISAHit *GetrltHitEvt() 
  { return (_hits.empty()) ? 0:&_hits[0]; } 
  
  //! pointer to first hit in 1st chamber
  static RLTPISAHit *GetrltRPC1HitEvt() 
  { return (_hits.empty()) ? 0:&_hits[0]; } 
  
  //! pointer to first hit in 2nd chamber
  static RLTPISAHit *GetrltRPC2HitEvt() 
  { return ( static_cast<int>(_hits.size()) > _rpc1_count) ? &_hits[_rpc1_count]:0; } 
  
  //! pointer to first hit in third chamber
  static RLTPISAHit *GetrltRPC3HitEvt() 
  { return ( static_cast<int>(_hits.size()) > _rpc1_count + _rpc2_count ) ? &_hits[_rpc1_count + _rpc2_count]:0; } 
  
  //! clear array
  static void RLTClear()
  { 
    _hits.clear(); 
    _rpc1_count = 0;
    _rpc2_count = 0;
    _rpc3_count = 0;
  }
  
  //@}  
  
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
  
  Int_t GetIrpc() const 
  { return irpc; }
  
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
  
  ClassDef(RLTPISAHit,1)  
};

#endif // _RLTPISAHit_
