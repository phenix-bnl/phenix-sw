#ifndef _KINPISAHIT_
#define _KINPISAHIT_

// $Id: KinPISAHit.h,v 1.9 2012/09/12 07:24:23 bbannier Exp $

/*!
\file  KinPISAHit.h
KIN structure keeps track of all track kinematical variables, and ancestry information. 
True_track is the unique index used to identify tracks in a given event from PISA file.
The parent true_track id is only valid if > 0. (positive)
If non-positive, the track is a primary track.
If negative the indicated number refer to one of the daughter of the current track.
A single true_track might appear multiple times in the KIN table, with different _negative_ true_track daughter ids.
\author  T. K. Ghosh, H. Pereira
\version $Revision: 1.9 $
\date    $Date: 2012/09/12 07:24:23 $
*/

#include <iostream>
#include <vector>
#include <TObject.h>
#include <cassert>

//! container for pisa hits
/*!
KIN structure keeps track of all track kinematical variables, and ancestry information. 
True_track is the unique index used to identify tracks in a given event from PISA file.
The parent true_track id is only valid if > 0. (positive)
If non-positive, the track is a primary track.
If negative the indicated number refer to one of the daughter of the current track.
A single true_track might appear multiple times in the KIN table, with different _negative_ true_track daughter ids.
*/
class KinPISAHit : public TObject 
{
    
  public:
  
  //! constructor
  KinPISAHit(
    Int_t true_track = 0, 
    Int_t isubevent = 0,  
    Int_t ntrack = 0, 
    Int_t idpart = 0,
    Float_t ptot = 0,     
    Float_t pthet = 0,    
    Float_t pphi = 0,
    Float_t r_vertex = 0, 
    Float_t z_vertex = 0, 
    Float_t th_vertx = 0,
    Float_t ph_vertx = 0,
    Int_t itparent = 0,   
    Int_t idparent = 0,
    Int_t nfile = 0);
  
  virtual ~KinPISAHit()
  {}
  
  //!@name static interface
  //@{
  
  //! number of kin hits
  static Int_t GetKinCount() 
  {return _KinHitEvt.size(); }
 
  //! kin hits
  static KinPISAHit *GetKinHitEvt() 
  {return _KinHitEvt.empty() ? 0:&_KinHitEvt[0]; }

  //! add hit
  static void AddKinHit( const KinPISAHit &ref )
  { _KinHitEvt.push_back( ref ); }

  //! find hit matching true_track id
  static KinPISAHit* Find( int true_track );
    
  //! find hit matching primary ancestor of true_track id
  static KinPISAHit* FindOrigin( int true_track );
  
  //! number of kin tracks
  static Int_t GetKinMaxTrack() 
  { return _KinTrkIndex.size(); }
   
  //! number of kin tracks
  static void SetKinMaxTrack(Int_t ktrack) 
  {
    
    _KinTrkIndex.clear(); 
    _KinTrkIndex.resize( ktrack );
    
    _KinTrkOrigin.clear(); 
    _KinTrkOrigin.resize( ktrack );
    
  }
     
  //! kin track index
  static Int_t *GetKinTrkIndex() 
  {return _KinTrkIndex.empty() ? 0 : &_KinTrkIndex[0] ; }
  
  //! kin track index
  static void SetKinTrkIndex( unsigned int index, const Int_t& ref) 
  { 
    assert( index <  _KinTrkIndex.size() );
    _KinTrkIndex[index] = ref; 
  }
  
  //! kin track parent index
  static Int_t *GetKinTrkOrigin() 
  {return _KinTrkOrigin.empty() ? 0 : &_KinTrkOrigin[0] ; }
    
  //! kin track parent index
  static void  SetKinTrkOrigin( unsigned int index, const Int_t &ref) 
  { 
    assert( index <  _KinTrkOrigin.size() );
    _KinTrkOrigin[index] = ref; 
  }
  
  //! clear
  static void KinClear()
  { 
    _KinHitEvt.clear();
    _KinTrkIndex.clear();
    _KinTrkOrigin.clear();
  }
  
  //! print _KinTrkIndex and _KinTrkOrigin association arrays
  static void PrintAssociations( void );
  
  //@}
  
  //! "event" track ID. unique within given file and subevent
  void SetEvttrack(const Int_t track) 
  {evttrack = track; }
  
  //! "event" track ID. unique within given file and subevent
  Int_t GetEvttrack() const 
  {return evttrack; }
  
  //! true track id (unique within given event (including multiple files and multiple sub-events)
  void SetTrue_track(const Int_t track) 
  {true_track = track; }
  
  //! true track id (unique within given event (including multiple files and multiple sub-events)
  Int_t GetTrue_track() const 
  {return true_track; }
  
  //! parent true track id
  void SetItparent(const Int_t track) 
  {itparent = track; }
  
  //! subevent id
  Int_t GetIsubevent() const 
  {return isubevent; }
  
  //! geant track id (it is unique within a given file and subevent)
  Int_t GetNtrack() const 
  {return ntrack; }
  
  //! particle id
  Int_t GetIdpart() const 
  {return idpart; }
  
  //!@name momentum at track origin
  //@{
  
  //! convenience function to get momentum along x
  Float_t GetPx() const;
  
  //! convenience function to get momentum along y
  Float_t GetPy() const;

  //! convenience function to get momentum along z
  Float_t GetPz() const;

  //! total momentum
  Float_t GetPtot() const 
  {return ptot; }
  
  //! momentum theta angle (degrees)
  Float_t GetPthet() const
  {return pthet; }
  
  //! momentum azimuthat angle
  Float_t GetPhi() const 
  {return pphi; }
  
  //@}

  //!@name position at track origin (vertex)
  //@{
  
  //! convenience function to get position along x
  Float_t GetXvertex() const;
  
  //! convenience function to get position along y
  Float_t GetYvertex() const;

  //! position along z
  Float_t GetZvertex() const 
  {return z_vertex; }

  //! radial position
  Float_t GetRvertex() const 
  {return r_vertex; }
  
  //! vertex theta angle (with respect to z = 0 and z axis)
  Float_t GetThvertx() const 
  {return th_vertx; }
  
  //! azimuthal position
  Float_t GetPhvertx() const 
  {return ph_vertx; }
  
  //@}
  //! return true if hit corresponds to a primary particle
  bool IsPrimary() const
  { return GetItparent() <= 0; }
  
  //! parent true track id
  Int_t GetItparent() const
  {return itparent; }
  
  //! parent particle id
  Int_t GetIdparent() const 
  {return idparent; }
  
  //! file id
  Int_t GetNfile() const 
  {return nfile; }
  
  //! file id
  void SetNfile(const Int_t kfile) 
  {nfile = kfile; }
  
  private:
  
  // true (unique, event wise) track id
  Int_t true_track;
  
  // sub event number
  Int_t isubevent;
  
  // geant (unique, sub-event wise) track id
  Int_t ntrack;
  
  //! track number ancestor in primary event
  Int_t evttrack;
  
  //! particle id
  Int_t idpart;
  
  //! total momentum
  Float_t ptot;
  
  //! momentum theta angle
  Float_t pthet;
  
  //! momentum phi angle
  Float_t pphi;
  
  //! vertex r position
  Float_t r_vertex;
  
  //! vertex z position
  Float_t z_vertex;
  
  //! vertex theta angle (!)
  Float_t th_vertx;
  
  //! vertex phi angle
  Float_t ph_vertx;
  
  //! parent true track id
  Int_t itparent;
  
  //! parent particle id
  Int_t idparent;
  
  //! file index
  Int_t nfile;
  
  //!@name static interface
  //@{
  
  //! Class global giving start pointer address
  static std::vector<KinPISAHit> _KinHitEvt;
      
  //! Class global giving indicies for each track number
  static std::vector<Int_t> _KinTrkIndex; 
  
  //! Class global giving indicies for each track number at origin
  static std::vector<Int_t> _KinTrkOrigin; 
  
  //@}
  
      
  //! print hit
  friend std::ostream& operator << (std::ostream& out, const KinPISAHit& );

  ClassDef(KinPISAHit, 1) 
      

};

#endif


