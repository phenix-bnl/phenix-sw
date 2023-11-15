#ifndef _PadPISAHit_
#define _PadPISAHit_

// $Id: PadPISAHit.h,v 1.6 2009/06/25 21:52:50 pinkenbu Exp $

/*!
\file  PadPISAHit.h
\brief container for pad chambers pisa hits
\author  T. K. Ghosh
\version $Revision: 1.6 $
\date    $Date: 2009/06/25 21:52:50 $
*/

#include <TObject.h>
#include <vector>
#include <cassert>

//! container for pad chambers pisa hits
class PadPISAHit : public TObject 
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
  Int_t   sector;
  
  //! which of PC1, PC2, or PC3 (*not* counting from 0!!)
  Int_t   ipc;    
  
  Int_t   isubevent; 
  Int_t   nfile;
  
  //!@name static interface
  //@{
  
  //! static container
  static std::vector<PadPISAHit> _hits;    
  
  //! number of counts in first pad chamber
  static int _pc1_count;   
  
  //! number of counts in 2nd pad chamber
  static int _pc2_count;     
  
  //! number of counts in 3rd pad chamber
  static int _pc3_count;     
  
  //@}
  
  public:
  
  //! default constructor  
  PadPISAHit();
  
  //! constructor
  PadPISAHit(
    Float_t xyzinloc[], Float_t xyzoutloc[], Float_t xyzinglo[],
    Float_t tof, Float_t dedx, Float_t pathLength, Int_t track,
    Int_t arm, Int_t sector, Int_t id, Int_t ipc,
    Int_t isubevent,  Int_t mctrack, Int_t nfile); 
  
  //! destructor
  virtual ~PadPISAHit() { }
  
  
  //!@name static interface
  //@{
  
  //! set array size
  static void SetPadCounts( int pc1_count, int pc2_count, int pc3_count ) 
  {
    _hits.clear();
    _pc1_count = pc1_count;
    _pc2_count = pc2_count;
    _pc3_count = pc3_count;
    
    if( pc1_count + pc2_count + pc3_count > 0 )
    { _hits.resize( pc1_count + pc2_count + pc3_count ); }
    
  }
  
  //! assign first chamber hit
  static void SetPC1HitEvt( int index, const PadPISAHit& ref) 
  {
    assert( index < _pc1_count );
    _hits[index] = ref;
  }
  
  //! assign second chamber hit
  static void SetPC2HitEvt( int index, const PadPISAHit& ref) 
  {
    assert( index < _pc2_count );
    _hits[_pc1_count + index] = ref;
  }
  
  //! assign third chamber hit
  static void SetPC3HitEvt( int index, const PadPISAHit& ref) 
  {
    assert( index < _pc3_count );
    _hits[_pc1_count + _pc2_count + index] = ref;
  }
  
  //! total size
  static Int_t GetPadCount()
  {return _hits.size(); }
  
  //! counts in first chamber
  static int GetPC1Count() 
  {return _pc1_count; }
  
  //! counts in second chamber
  static int GetPC2Count() 
  {return _pc2_count; }
  
  //! counts in third chamber
  static int GetPC3Count() 
  {return _pc3_count; }
  
  //! first hit
  static PadPISAHit *GetPadHitEvt() 
  { return (_hits.empty()) ? 0:&_hits[0]; } 
  
  //! pointer to first hit in 1st chamber
  static PadPISAHit *GetPC1HitEvt() 
  { return (_hits.empty()) ? 0:&_hits[0]; } 
  
  //! pointer to first hit in 2nd chamber
  static PadPISAHit *GetPC2HitEvt() 
  { return ( static_cast<int>(_hits.size()) > _pc1_count) ? &_hits[_pc1_count]:0; } 
  
  //! pointer to first hit in third chamber
  static PadPISAHit *GetPC3HitEvt() 
  { return ( static_cast<int>(_hits.size()) > _pc1_count + _pc2_count ) ? &_hits[_pc1_count + _pc2_count]:0; } 
  
  //! clear array
  static void PadClear()
  { 
    _hits.clear(); 
    _pc1_count = 0;
    _pc2_count = 0;
    _pc3_count = 0;
  }
  
  //@}
  
  
  void SetSector(const Int_t isector) 
  { sector = isector; }
  
  Int_t GetSector() const 
  { return sector; }
  
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
  
  Float_t get_xyzinloc(const int i) const
  {return xyzinloc[i];}
  
  Float_t GetXin() const 
  { return xyzinloc[0]; }
  
  Float_t GetYin() const 
  { return xyzinloc[1]; }
  
  Float_t GetZin() const 
  { return xyzinloc[2]; }
  
  Float_t get_xyzoutloc(const int i) const 
  {return xyzoutloc[i];}
  
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
  
  Float_t get_xyzinglo(const int i) const 
  {return xyzinglo[i];}
  
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
  
  ClassDef(PadPISAHit,1)
};

#endif
