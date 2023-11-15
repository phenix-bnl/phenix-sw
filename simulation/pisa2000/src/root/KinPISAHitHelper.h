#ifndef _KINPISAHITHELPER_
#define _KINPISAHITHELPER_

// $Id: KinPISAHitHelper.h,v 1.2 2009/08/21 22:34:53 hpereira Exp $

/*!
\file  KinPISAHitHelper.h
\brief Helper class used to associate unique track id for full pisa event, and keep track of ancestry information
\author  H. Pereira
\version $Revision: 1.2 $
\date    $Date: 2009/08/21 22:34:53 $
*/

#include <cassert>
#include <set> 
#include <TClonesArray.h>
#include <KinPISAHit.h>

class KinPISAHitHelper
{
  
  public: 
  
  //! constructor
  KinPISAHitHelper( void );
  
  //! reset (set _true_track_id to 0). Must be called at beginning of every new event.
  void reset( void );
  
  //! add kin hits of current events to array
  /*! 
  - Set "true track" to existing pisa hits (that is: the event-wise track unique id);
  - Update track parents accordingly
  - store resulting hits in KinPISAHits
  */
  void add( TClonesArray&, int, int );
  
  //! perform true track associations and finish pisa hit filling
  void associate( void ) const;
    
  //! find true track matching file, subevent and geant track id
  int findTrueTrack( int file, int event, int track ) const;
  
  private:
  
  //! needed to identify unique tracks
  /*! it is based on geant id, subevent id, andfile id*/
  class SmallerTrackFTor
  {
    
    public:
    
    //! predicate
    bool operator () ( KinPISAHit* first, KinPISAHit* second ) const
    {
      if( first->GetNfile() != second->GetNfile() ) return first->GetNfile() < second->GetNfile();
      if( first->GetIsubevent() != second->GetIsubevent() ) return first->GetIsubevent() < second->GetIsubevent();
      return first->GetNtrack() < second->GetNtrack();
    }
  
  };
  
  //! check if two hits have the same contents
  class SameContentsFTor
  {
    
    public:
    
    //! predicate
    bool operator () ( KinPISAHit*, KinPISAHit* ) const;
    
  };
  
  //! check if two hits have the same contents
  class IsParentFTor
  {
    
    public:
    
    //! constructor
    IsParentFTor( KinPISAHit* child ):
      _child( child )
    { assert( child ); }
    
    //! return true if current hit is parent of child
    bool operator() ( KinPISAHit* hit ) const
    {
      return 
        _child->GetNfile() == hit->GetNfile() &&
        _child->GetIsubevent() == hit->GetIsubevent() &&
        abs( _child->GetItparent() ) == hit->GetNtrack();
    }
    
    private:
    
    //! child
    KinPISAHit* _child; 
    
  };
  
  //! hit set
  typedef std::set<KinPISAHit*, SmallerTrackFTor > KinPISAHitSet;
  
  //! true track running index
  int _true_track_id;

  //! stores file event and id needed to uniquely identify a geant track
  class GeantId
  {
    public:
    
    //! constructor
    GeantId( int file = 0, int event = 0, int track = 0 ):
      _file( file ), 
      _event( event ),
      _track( track )
    {}
      
    //! equal to operator
    bool operator == (const GeantId& id ) const
    { 
      return 
        _file == id._file && 
        _event == id._event &&
        _track == id._track;
    }
  
    //! less than operator
    bool operator < (const GeantId& id ) const
    { 
      if( _track != id._track ) return _track < id._track;
      if( _event != id._event ) return _event < id._event;
      return _file < id._file;
    }
    
    private:
    
    int _file;
    int _event;
    int _track;
    
  };
  
  //! map geant ID and true track
  /*! this is used to speed up matching of true track to various detector hits */
  typedef std::map< GeantId, int > TrueTrackMap;
  TrueTrackMap _true_track_map;
  
};

#endif

