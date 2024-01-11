// $Id: Muon3DDisplay.h,v 1.8 2011/02/15 17:34:51 youzy Exp $

/*!
  \file Muon3DDisplay.h
  \ingroup supermodules
  \brief muon arms 3D event display supermodule.
  Displays detectors, mc tracks,
  tracks, stubs, roads, mutr/muid clusters, on user request
  \author Hugo Pereira
  \version $Revision: 1.8 $
  \date $Date: 2011/02/15 17:34:51 $
*/

#ifndef __MUON3DDISPLAY_H__
#define __MUON3DDISPLAY_H__

#include<SubsysReco.h>

#include<TROOT.h>
#include<TCanvas.h>
#include<TNode.h>
#include<TView.h>
#include<TGButton.h>

#include<map>
#include<list>
#include<string>

#include <TMutIndex.h>
#include <TMuiIndex.h>

#include <TRpcStrip3D.h>
#include <TRpcIndex.h>

#include <TFvtxIndex.h>

#ifndef __CINT__
#include <TMuiRoadMapO.h>
#include <PHTimeServer.h>
#endif

#include <TText.h>

class PHCompositeNode;

// generic 3D objects
class PHObj3D;

/*!
  \ingroup supermodules
  \brief   mutoo/muioo 3D event display supermodule.
  Displays Mutr/Muid detectors, mc tracks,
  tracks, stubs, roads, mutr/muid clusters, on user request
*/
class Muon3DDisplay: public SubsysReco
{
 public:

  //! constructor
  Muon3DDisplay(const char* name = "MUON3DDISPLAY");

  //! destructor
  virtual ~Muon3DDisplay()
  {
    clear();
    if( _rot_node ) SafeDelete( _rot_node );
    if( _main_node ) SafeDelete( _main_node );
    if( _view ) SafeDelete( _view );
    if( _cv ) SafeDelete( _cv );
  }

  //! global initialization
  int Init(PHCompositeNode *topNode)
  {
    _init_display( );
    return 0;
  }

  //! event by event processing
  int process_event(PHCompositeNode *topNode);

  //! terminate
  int End(PHCompositeNode *topNode);

  //! display commands
  void help( const char* module_name = "muon_display" );

  //!@names drawing flags
  //@{

  //! auto_draw
  void set_auto_draw( const bool& value )
  { _auto_draw = value; }

  //! event header
  void set_draw_header( const bool& value )
  { _draw_header = value; }

  //! mut detectors
  void set_draw_mut_detectors( const bool& value )
  { _draw_mut_detectors = value; }

  //! tracks
  void set_draw_tracks( const bool& value )
  { _tracks.set_enabled( value ); }

  //! MC tracs
  void set_draw_mc_tracks( const bool& value )
  { _mc_tracks.set_enabled( value ); }

  //! stubs
  void set_draw_mut_stubs( const bool& value )
  { _mut_stubs.set_enabled( value ); }

  //! coordinates
  void set_draw_mut_coords( const bool& value )
  { _mut_coords.set_enabled( value ); }

  //! mui detectors
  void set_draw_mui_detectors( const bool& value )
  { _draw_mui_detectors = value; }

  //! mui roads
  void set_draw_mui_roads( const bool& value )
  { _mui_roads.set_enabled( value ); }

  //! mui clusters
  void set_draw_mui_clusters( const bool& value )
  { _mui_clusters.set_enabled( value ); }

  //! rpc detectors
  void set_draw_rpc_detectors( const bool& value )
  { _draw_rpc_detectors = value; }

#ifdef __USE_RPC__
  //! rpc mc hits
  void set_draw_rpc_mc_hits( const bool& value )
  { _rpc_mc_hits.set_enabled( value ); }
#endif

  //! rpc coordinates
  void set_draw_rpc_coords( const bool& value )
  { _rpc_hits.set_enabled( value ); }
  
  //! fvtx detector
  void set_draw_fvtx_detectors( const bool& value )
  { _draw_fvtx_detectors = value; }

  //! fvtx detectors
  void set_draw_fvtx_columns( const bool& value )
  { _draw_fvtx_columns = value; }

  //! fvtx mc hits
  void set_draw_fvtx_mc_hits( const bool& value )
  { _fvtx_mc_hits.set_enabled( value ); }

  //! fvtx mc tracks
  void set_draw_fvtx_mc_tracks( const bool& value )
  { _fvtx_mc_tracks.set_enabled( value ); }

  //! fvtx tracks
  void set_draw_fvtx_tracks( const bool& value )
  { _fvtx_tracks.set_enabled( value ); }

  //! delay (seconds) after drawing an event and before processing next event
  void set_delay( const unsigned int& value )
  { _delay = value; }

  //@}

  //!@name accessors
  //@{

  //! access const reference to main node
  const TNode& get_main_node( void ) const
  { return *_main_node; }

  //! access const reference to rotated node
  const TNode& get_rot_node( void ) const
  { return *_rot_node; }

  //! access const reference to rotated node
  const TView& get_view( void ) const
  { return *_view; }

  //@}

  //!@name drawing
  //@{

  //! draw event header
  void draw_header( void );

  //! draw detectors
  void draw_mut_detectors(
    const int& arm = -1,
    const int& station = -1,
    const int& octant = -1,
    const int& half = -1,
    const int& gap = -1
  );

  //! draw detectors
  void draw_mut_wires(
    const int& arm = -1,
    const int& station = -1,
    const int& octant = -1,
    const int& half = -1,
    const int& gap = -1,
    const unsigned int& wire = 0
  );

  //! draw muon tracker anode cards
  void draw_mut_anode_card(
    const int& arm,
    const int& station,
    const int& octant,
    const int& half,
    const int& gap,
    const unsigned int& card
  );

  //! draw muon tracker disabled anode cards
  void draw_mut_disabled_anodes(
    const int& arm = -1,
    const int& station = -1,
    const int& octant = -1,
    const int& half = -1,
    const int& gap = -1
  );

  //! draw muon tracker anode by name
  void draw_mut_anode_card( const char* name );

  //! draw detectors
  void draw_mui_detectors(
    const int& arm = -1,
    const int& plane = -1,
    const int& panel = -1
  );

  //! draw detectors
  void draw_rpc_detectors(
    const int& arm     = -1,
    const int& station = -1,
    const int& octant  = -1,
    const int& halfoct = -1,
    const int& radseg  = -1,
    const int& strip   = -1);

  //! draw detectors
  void draw_fvtx_detectors(
    const int& arm = -1,
    const int& cage = -1, 
    const int& station = -1,
    const int& sector = -1);

  //! draw detectors
  void draw_fvtx_columns(
    const int& arm = -1,
    const int& cage = -1,
    const int& station = -1,
    const int& sector = -1,
    const int& column = -1 );

  //! draw coordinates associated to current event and drawn detectors
  void draw_mut_coords( void );

  //! draw tracks associated to current event
  void draw_tracks( void );

  //! draw mc tracks associated to current event
  void draw_mc_tracks( void );

#ifdef __USE_RPC__
  //! draw rpc mc hits associated to current event
  void draw_rpc_mc_hits( void );
#endif

  //! draw rpc coords associated to current event
  void draw_rpc_coords( void );
  
  //! draw stubs associated to current event and drawn detectors
  void draw_mut_stubs( void );

  //! draw muid clusters associated to current event and drawn detectors
  void draw_mui_clusters( void );

  //! draw muid roads associated to current event
  void draw_mui_roads( void );

  //! draw forward vertex mc hits associated to mc track
  void draw_fvtx_mc_hits( void );

  //! draw fvtx mc tracks associated to current event
  void draw_fvtx_mc_tracks( void );

  //! draw fvtx tracks associated to current event
  void draw_fvtx_tracks( void );

  //! clear detectors
  void clear( void );

  //! clear drawn detectors
  void clear_mut_detectors( void );

  //! clear drawn detectors
  void clear_mui_detectors( void );

  //! clear drawn detectors
  void clear_rpc_detectors( void );
  
  //! clear drawn detectors
  void clear_fvtx_detectors( void );

  //! clear drawn detectors
  void clear_fvtx_columns( void );

  // clear mut wires
  void clear_mut_wires( void )
  { _clear_list( _mut_wires ); }

  // clear mut anode cards
  void clear_mut_anode_cards( void )
  { _clear_list( _mut_anode_cards ); }

  //! clear drawn coordinates
  void clear_mut_coords( void )
  { _clear_list( _mut_coords ); }

  //! clear drawn tracks
  void clear_tracks( void )
  { _clear_list( _tracks ); }

  //! clear drawn MC tracks
  void clear_mc_tracks( void )
  { _clear_list( _mc_tracks ); }

#ifdef __USE_RPC__
  //! clear drawn rpc MC hits
  void clear_rpc_mc_hits( void )
  { _clear_list( _rpc_mc_hits ); }
#endif

  //! clear drawn rpc MC hits
  void clear_rpc_coords( void )
  { _clear_list( _rpc_hits ); }
  
  //! clear drawn stubs
  void clear_mut_stubs( void )
  { _clear_list( _mut_stubs ); }

  //! clear drawn muid clusters
  void clear_mui_clusters( void )
  { _clear_list( _mui_clusters ); }

  //! clear drawn muid roads
  void clear_mui_roads( void )
  { _clear_list( _mui_roads ); }

  //! clear drawn forward vertex mc hits
  void clear_fvtx_mc_hits( void )
  { _clear_list( _fvtx_mc_hits ); }

  //! clear drawn forward vertex mc trackss
  void clear_fvtx_mc_tracks( void )
  { _clear_list( _fvtx_mc_tracks ); }

  //! clear drawn forward vertex trackss
  void clear_fvtx_tracks( void )
  { _clear_list( _fvtx_tracks ); }

  //! update display
  void update( void );

  //@}

  //!@name menu
  //@{

  //! build menu
  void menu( void );

  //! enable/disable auto draw
  void toggle_auto_draw( void )
  { set_auto_draw( _is_down( (TGCheckButton*) gTQSender ) ); }

  //! draw/hide mut detectors
  void toggle_mut_detectors( void )
  {
    set_draw_mut_detectors( _is_down( (TGCheckButton*) gTQSender ) );
    if( _draw_mut_detectors ) draw_mut_detectors();
    else clear_mut_detectors();
  }

  //! draw/hide mut tracks
  void toggle_tracks( void )
  {
    set_draw_tracks( _is_down( (TGCheckButton*) gTQSender ) );
    if( _tracks.enabled() ) draw_tracks();
    else clear_tracks();
  }

  //! draw/hide mc tracks
  void toggle_mc_tracks( void )
  {
    set_draw_mc_tracks( _is_down( (TGCheckButton*) gTQSender ) );
    if( _mc_tracks.enabled() ) draw_mc_tracks();
    else clear_mc_tracks();
  }

  //! draw/hide mut stubs
  void toggle_mut_stubs( void )
  {
    set_draw_mut_stubs( _is_down( (TGCheckButton*) gTQSender ) );
    if( _mut_stubs.enabled() ) draw_mut_stubs();
    else clear_mut_stubs();
  }

  //! draw/hide mut coordinates
  void toggle_mut_coords( void )
  {
    set_draw_mut_coords( _is_down( (TGCheckButton*) gTQSender ) );
    if( _mut_coords.enabled() ) draw_mut_coords();
    else clear_mut_coords();
  }

  //! draw/hide mui detectors
  void toggle_mui_detectors( void )
  {
    set_draw_mui_detectors( _is_down( (TGCheckButton*) gTQSender ) );
    if( _draw_mui_detectors ) draw_mui_detectors();
    else clear_mui_detectors();
  }

  //! draw/hide mui roads
  void toggle_mui_roads( void )
  {
    set_draw_mui_roads( _is_down( (TGCheckButton*) gTQSender ) );
    if( _mui_roads.enabled() ) draw_mui_roads();
    else clear_mui_roads();
  }

  //! draw/hide mui clusters
  void toggle_mui_clusters( void )
  {
    set_draw_mui_clusters( _is_down( (TGCheckButton*) gTQSender ) );
    if( _mui_clusters.enabled() ) draw_mui_clusters();
    else clear_mui_clusters();
  }

  //! draw/hide rpc detectors
  void toggle_rpc_detectors( void )
  {
    set_draw_rpc_detectors( _is_down( (TGCheckButton*) gTQSender ) );
    if( _draw_rpc_detectors ) draw_rpc_detectors();
    else clear_rpc_detectors();
  }

#ifdef __USE_RPC__
  //! draw/hide rpc mc hits
  void toggle_rpc_mc_hits( void )
  {
    set_draw_rpc_mc_hits( _is_down( (TGCheckButton*) gTQSender ) );
    if( _rpc_mc_hits.enabled() ) draw_rpc_mc_hits();
    else clear_rpc_mc_hits();
  }
#endif

  //! draw/hide rpc clusters
  void toggle_rpc_coords( void )
  {
    set_draw_rpc_coords( _is_down( (TGCheckButton*) gTQSender ) );
    if( _rpc_hits.enabled() ) draw_rpc_coords();
    else clear_rpc_coords();
  }

  //! draw/hide fvtx detectors
  void toggle_fvtx_detectors( void )
  {
    set_draw_fvtx_detectors( _is_down( (TGCheckButton*) gTQSender ) );
    if( _draw_fvtx_detectors ) draw_fvtx_detectors();
    else clear_fvtx_detectors();
  }


  //! draw/hide fvtx detectors
  void toggle_fvtx_columns( void )
  {
    set_draw_fvtx_columns( _is_down( (TGCheckButton*) gTQSender ) );
    if( _draw_fvtx_columns ) draw_fvtx_columns();
    else clear_fvtx_columns();
  }

  //! draw/hide fvtx mc hits
  void toggle_fvtx_mc_hits( void )
  {
    set_draw_fvtx_mc_hits( _is_down( (TGCheckButton*) gTQSender ) );
    if( _fvtx_mc_hits.enabled() ) draw_fvtx_mc_hits();
    else clear_fvtx_mc_hits();
  }

  //@}

  protected:

  //! initialise canvas
  void _init_display( void );

  //! create all mutr gaps
  void _init_detectors( void );

  //! clear PHObj3D list
  void _clear_list( std::list< PHObj3D* >& list );

  //! returns true if arg is a check button and is selected
  static bool _is_down( TGCheckButton* btn )
  { return ( btn && btn->GetState() == kButtonDown ); }

  #ifndef __CINT__

  //! returns true if road passes the level2 cuts on number of gaps, depth and slope
  static bool _is_l2_candidate( const TMuiRoadMapO::value_type & road );

  //! calculate road slope
  static double _get_slope( const TMuiRoadMapO::value_type &road );

  //! calculate number of hit gaps (vert+horiz. Max should be 10)
  static unsigned int _get_n_gaps( const TMuiRoadMapO::value_type &road );

  //! module timer
  PHTimeServer::timer _timer;

  #endif

  //! top node
  PHCompositeNode* _top_node;

  //! detector initialization done
  bool _detector_init;

  //! auto drawing at new event
  bool _auto_draw;

  //! event header
  bool _draw_header;

  //! mutr detectors
  bool _draw_mut_detectors;

  //! muid detectors
  bool _draw_mui_detectors;

  //! rpc detectors
  bool _draw_rpc_detectors;
  
  //! fvtx detectors
  bool _draw_fvtx_detectors;

  //! fvtx columns
  bool _draw_fvtx_columns;
  
  //! root canvas
  TCanvas* _cv;

  //! root view
  TView* _view;

  //! pointer to 3D Display Main Node
  TNode* _main_node;

  //! pointer to 3D Display Rotated Node (z horizontal)
  TNode* _rot_node;

  //! delay (sleep, in seconds) after drawing an event
  unsigned int _delay;

  //! header text object
  TText* _header_text;
  
  //!@name 3D object maps
  //@{

  //! shortcut to access iterator over gap pairs
  typedef std::map< TMutIndex, PHObj3D* >::iterator gap_iterator;

  //! shortcut to access iterator over muid panel pairs
  typedef std::map< TMuiIndex, PHObj3D* >::iterator panel_iterator;

  //! shortcut to access iterator over rpc station pairs
  typedef std::map< TRpcIndex, PHObj3D* >::iterator rpc_station_iterator;
  
  //! shortcut to access iterator over fvtx sector pairs
  typedef std::map< TFvtxIndex, PHObj3D* >::iterator fvtx_sector_iterator;

  //! shortcut to access iterator over fvtx column pairs
  typedef std::map< TFvtxIndex, PHObj3D* >::iterator fvtx_column_iterator;

  //! map of mutr gaps
  std::map< TMutIndex, PHObj3D* > _mut_gaps;

  //! map of panel objects
  std::map< TMuiIndex, PHObj3D* > _muid_panels;

  //! map of rpc station objects
  std::map< TRpcIndex, PHObj3D* > _rpc_stations;

  //! map of forward vertex sectors objects
  std::map< TFvtxIndex, PHObj3D* > _fvtx_sectors;

  //! map of forward vertex columns objects
  std::map< TFvtxIndex, PHObj3D* > _fvtx_columns;

  //! 3D objects storage list
  class PHObj3DList: public std::list< PHObj3D* >
  {
    public:
    
    //! constructor
    PHObj3DList( void ):
      _enabled( false )
      {}
    
    //! true if list is to be drawn
    void set_enabled( const bool& value )
    { _enabled = value; }
        
    //! true if list is to be drawn
    const bool& enabled( void ) const
    { return _enabled; }
    
    private:
    
    //! true if list is to be drawn
    bool _enabled;
    
  };
  
  //! list of mutr wires
  PHObj3DList _mut_wires;

  //! list of mutr wires
  PHObj3DList _mut_anode_cards;
  
  //! list of mc tracks
  PHObj3DList _mc_tracks;

#ifdef __USE_RPC__
  //! list of rpc roads
  PHObj3DList _rpc_mc_hits;
#endif

  //! list of rpc roads
  PHObj3DList _rpc_hits;
  
  //! list of fvtx mc hits
  PHObj3DList _fvtx_mc_hits;

  //! list of fvtx mc tracks 
  PHObj3DList _fvtx_mc_tracks;

  //! list of fvtx tracks 
  PHObj3DList _fvtx_tracks;

  //! list of tracks
  PHObj3DList _tracks;

  //! list of coordinates
  PHObj3DList _mut_coords;

  //! list of stubs
  PHObj3DList _mut_stubs;

  //! list of muid clusters
  PHObj3DList _mui_clusters;

  //! list of muid roads
  PHObj3DList _mui_roads;

  //@}

};

#endif /* __MUONDISPAY_H__ */
