/*!
  \file Muon3DDisplay.cxx
  \brief mutoo 3D display supermodule
  \author Hugo Pereira
  \version $Revision: 1.13 $
  \date $Date: 2012/01/24 15:16:40 $
*/

#include <EventHeader.h>

#include <MutAnodeMap.h>
#include <MuonUtil.h>

#include <TMutGeo.h>
#include <TMutAnode3D.h>
#include <TMutGap3D.h>
#include <TMutMCTrack3D.h>
#include <TMutTrack3D.h>
#include <TMutCoord3D.h>
#include <TMutStub3D.h>
#include <TMutWire3D.h>

#include <TMuiPanel3D.h>
#include <TMuiCluster3D.h>
#include <TMuiRoad3D.h>

#include <TRpcHitMap.h>

#ifdef __USE_RPC__
  #include <TRpcClusMap.h>
  #include <TRpcCoordMap.h>
  #include <TRpcHitMap.h>
  #include <TRpcMCHitMap.h>
  #include <TRpcPad3DType1.h>
  #include <TRpcPad3DType2.h>
  #include <TRpcStation3DType1.h>
  #include <TRpcStation3DType2.h>
#endif

#include <FVTXOO.h>
#include <FvtxGeom.h>
#include <TFvtxSector3D.h>
#include <TFvtxColumn3D.h>
#include <TFvtxMCHit3D.h>
#include <TFvtxMCTrack3D.h>
#include <TFvtxTrack3D.h>
#include <TFvtxTrkMap.h>

#include <iostream>
#include <TBRIK.h>
#include <TMutMCTrkMap.h>
#include <TMutTrkMap.h>
#include <TMutCoordMap.h>
#include <TMutStubMap.h>
#include <TMuiRoadMapO.h>
#include <TMutFitPar.hh>
#include <recoConsts.h>
#include <PHTimer.h>

#include <Muon3DDisplay.h>

#include <RVersion.h> // root version macro

using namespace std;

//____________________________________________________________________________
Muon3DDisplay::Muon3DDisplay( const char* name ) :
  SubsysReco(name),
  _timer( PHTimeServer::get()->insert_new(name) ),
  _top_node(0),
  _detector_init( false ),
  _auto_draw( false ),
  _draw_header( false ),
  _draw_mut_detectors( false ),
  _draw_mui_detectors( false ),
  _draw_rpc_detectors( false ),
  _draw_fvtx_detectors( false ),
  _draw_fvtx_columns( false ),
  _cv( 0 ),
  _view( 0 ),
  _main_node( 0 ),
  _rot_node( 0 ),
  _delay( 0 ),
  _header_text( 0 )
{}

//____________________________________________________________________________
int Muon3DDisplay::process_event(PHCompositeNode *top_node)
{
  _timer.get()->restart();
  _top_node = top_node;

  //! book all gap 3D objects
  if( ! _detector_init ) {
    _init_detectors( );
    _detector_init = true;

    if( _auto_draw )
    {
      if( _draw_mut_detectors ) draw_mut_detectors();
      if( _draw_mui_detectors ) draw_mui_detectors();
      if( _draw_rpc_detectors ) draw_rpc_detectors();
      if( _draw_fvtx_detectors ) draw_fvtx_detectors();
    }

  }

  clear_mut_wires();
  clear_mut_anode_cards();
  clear_mc_tracks();
  clear_tracks();
  clear_mut_coords();
  clear_mut_stubs();
  clear_mui_clusters();
  clear_mui_roads();
#ifdef __USE_RPC__
  clear_rpc_mc_hits();
#endif
  clear_rpc_coords();
  clear_fvtx_mc_hits();
  clear_fvtx_mc_tracks();
  clear_fvtx_tracks();

  if( _auto_draw )
  {

    if( _mut_coords.enabled() ) draw_mut_coords();
    if( _mut_stubs.enabled() ) draw_mut_stubs();
    if( _tracks.enabled() ) draw_tracks();
    if( _mc_tracks.enabled() ) draw_mc_tracks();

    if( _mui_clusters.enabled() ) draw_mui_clusters();
    if( _mui_roads.enabled() ) draw_mui_roads();

#ifdef __USE_RPC__
    if( _rpc_mc_hits.enabled() ) draw_rpc_mc_hits();
#endif
    if( _rpc_hits.enabled() ) draw_rpc_coords();
    if( _fvtx_mc_hits.enabled() ) draw_fvtx_mc_hits();
    if( _fvtx_mc_tracks.enabled() ) draw_fvtx_mc_tracks();
    if( _fvtx_tracks.enabled() ) draw_fvtx_tracks();
    if( _draw_header ) draw_header();

  }

  // wait
  if( _delay ) sleep( _delay );

  _timer.get()->stop();
  return 0;
}

//____________________________________________________________________________
int Muon3DDisplay::End(PHCompositeNode* top_node)
{
  _timer.get()->print_stat();
  return 0;
}

//____________________________________________________________________________
void Muon3DDisplay::help( const char* module_name )
{

  cout << "Muon3DDisplay Visualize Commands" << endl;
  cout << "--------------------------------" << endl;
  cout << "Note: for each index, -1 means all" << endl;
  cout << module_name << "->draw_mut_detectors( arm, station, octant, half, gap )" << endl;
  cout << module_name << "->draw_mut_wires( arm, station, octant, half, gap, index )" << endl;
  cout << module_name << "->draw_mut_anode_card( arm, station, octant, half, gap, index )" << endl;
  cout << module_name << "->draw_mc_tracks()" << endl;
  cout << module_name << "->draw_tracks()" << endl;
  cout << module_name << "->draw_mut_coords()" << endl;
  cout << module_name << "->draw_mut_stubs()" << endl;
  cout << endl;
  cout << module_name << "->draw_mui_detectors( arm, plane, panel )" << endl;
  cout << module_name << "->draw_mui_roads()" << endl;
  cout << module_name << "->draw_mui_clusters()" << endl;
  cout << endl;
  cout << module_name << "->draw_rpc_detectors( arm, station, octant, halfoctant, radialsegment, strip )" << endl;
#ifdef __USE_RPC__
  cout << module_name << "->draw_rpc_mc_hits()" << endl;
#endif
  cout << module_name << "->draw_rpc_coords()" << endl;
  cout << endl;
  cout << module_name << "->draw_fvtx_detectors( arm, cage, station, sector )" << endl;
  cout << module_name << "->draw_fvtx_columns( arm, cage, station, sector, column )" << endl;
  cout << module_name << "->draw_fvtx_mc_hits()" << endl;
  cout << module_name << "->draw_fvtx_mc_tracks()" << endl;
  cout << module_name << "->draw_fvtx_tracks()" << endl;
  cout << endl;
  cout << module_name << "->clear(): remove everything." << endl;
  cout << module_name << "->clear_mut_detectors()" << endl;
  cout << module_name << "->clear_mut_wires()" << endl;
  cout << module_name << "->clear_mut_anode_cards()" << endl;
  cout << module_name << "->clear_mc_tracks()" << endl;
  cout << module_name << "->clear_tracks()" << endl;
  cout << module_name << "->clear_mut_coords()" << endl;
  cout << module_name << "->clear_mut_stubs()" << endl;
  cout << endl;
  cout << module_name << "->clear_mui_detectors()" << endl;
  cout << module_name << "->clear_mui_roads()" << endl;
  cout << module_name << "->clear_mui_clusters()" << endl;
  cout << endl;
  cout << module_name << "->clear_rpc_detectors()" << endl;
#ifdef __USE_RPC__
  cout << module_name << "->clear_rpc_mc_hits()" << endl;
#endif
  cout << module_name << "->clear_rpc_coords()" << endl;
  cout << endl;
  cout << module_name << "->clear_fvtx_detectors()" << endl;
  cout << module_name << "->clear_fvtx_columns()" << endl;
  cout << module_name << "->clear_fvtx_mc_hits()" << endl;
  cout << module_name << "->clear_fvtx_mc_tracks()" << endl;
  cout << module_name << "->clear_fvtx_tracks()" << endl;
  cout << endl;
  cout << module_name << "->menu()" << endl;
  cout << module_name << "->set_auto_draw( bool )" << endl;
  cout << "--------------------------------------------------------------------------------" << endl;
}

//____________________________________________________________________________
void Muon3DDisplay::draw_header( void )
{

  MUTOO::PRINT( cout, "Muon3DDisplay::draw_header" );

  // retrieve run number
  int run_number = recoConsts::instance()->get_IntFlag("RUNNUMBER", 0);

  // retrieve event number
  int event_number( -1 );
  try {
    EventHeader* evt = TMutNode<EventHeader>::find_io_node(_top_node,"EventHeader");
    event_number = evt->get_EvtSequence();
  } catch (exception& e) { }

  ostringstream what;
  what << "run: " << run_number << " event: " << event_number;

  _cv->cd();

  if( _header_text ) SafeDelete( _header_text );
  _header_text = new TText();
  _header_text->DrawTextNDC( 0.1, 0.1, what.str().c_str() );

  update();

}

//____________________________________________________________________________
void Muon3DDisplay::draw_mut_detectors(
  const int& arm_,
  const int& station_,
  const int& octant_,
  const int& half_,
  const int& gap_
)
{

  // make sure we are in the correct canvas
  _cv->cd();

  try {

    for( int arm=0; arm < MUTOO::MAX_ARM; arm++ )
    if( arm_ < 0 || arm_ == arm )
    for( int station = 0; station < MUTOO::MAX_STATION; station++ )
    if( station_ < 0 || station_ == station )
    for( int octant = 0; octant < MUTOO::MAX_OCTANT; octant++ )
    if( octant_ < 0 || octant_ == octant )
    for( int half = 0; half < MUTOO::MAX_HALF_OCTANT; half++ )
    if( half_ < 0 || half_ == half )
    for( int gap = 0; gap < (( station == MUTOO::Station3 ) ? 2:3 ); gap++ )
    if( gap_ < 0 || gap_ == gap ) {
      TMutIndex index( arm, station, octant, half, gap );
      gap_iterator It( _mut_gaps.find( index ) );
      if( It != _mut_gaps.end() && It->second && !It->second->drawn() ) {
//        It->second->set_line_color(18);  //mjl
        It->second->set_line_color(1);
        It->second->draw();
      }
    }

  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }
  _draw_mut_detectors = true;
  update();

  return;
}

//____________________________________________________________________________
void Muon3DDisplay::draw_mut_wires(
  const int& arm_,
  const int& station_,
  const int& octant_,
  const int& half_,
  const int& gap_,
  const unsigned int& wire_
  )
{

  // make sure we are in the correct canvas
  _cv->cd();

  try {

    for( int arm=0; arm < MUTOO::MAX_ARM; arm++ )
    if( arm_ < 0 || arm_ == arm )
    for( int station = 0; station < MUTOO::MAX_STATION; station++ )
    if( station_ < 0 || station_ == station )
    for( int octant = 0; octant < MUTOO::MAX_OCTANT; octant++ )
    if( octant_ < 0 || octant_ == octant )
    for( int half = 0; half < MUTOO::MAX_HALF_OCTANT; half++ )
    if( half_ < 0 || half_ == half )
    for( int gap = 0; gap < (( station == MUTOO::Station3 ) ? 2:3 ); gap++ )
    if( gap_ < 0 || gap_ == gap )
    {
      // create wire object
      TMutIndex index( arm, station, octant, half, gap );
      TMutWire3D* mut_wire = new TMutWire3D( index, wire_, _rot_node );
      mut_wire->draw( );
      _mut_wires.push_back( mut_wire );
    }
  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }

  update();

}

//____________________________________________________________________________
void Muon3DDisplay::draw_mut_disabled_anodes(
  const int& arm_,
  const int& station_,
  const int& octant_,
  const int& half_,
  const int& gap_
  )
{

  // make sure we are in the correct canvas
  _cv->cd();

  try {

    for( int arm=0; arm < MUTOO::MAX_ARM; arm++ )
    if( arm_ < 0 || arm_ == arm )
    for( int station = 0; station < MUTOO::MAX_STATION; station++ )
    if( station_ < 0 || station_ == station )
    for( int octant = 0; octant < MUTOO::MAX_OCTANT; octant++ )
    if( octant_ < 0 || octant_ == octant )
    for( int half = 0; half < MUTOO::MAX_HALF_OCTANT; half++ )
    if( half_ < 0 || half_ == half )
    for( int gap = 0; gap < (( station == MUTOO::Station3 ) ? 2:3 ); gap++ )
    if( gap_ < 0 || gap_ == gap )
    {

      // retrieve disabled wire cards
      const set<int>& disabled_anodes( TMutGeo::get_disabled_anode_cards( arm, station, octant, half, gap ) );
      for( set<int>::const_iterator iter = disabled_anodes.begin(); iter != disabled_anodes.end(); iter++ )
      {
        // create wire object
        TMutIndex index( arm, station, octant, half, gap );
        TMutAnode3D* mut_anode = new TMutAnode3D( index, *iter, _rot_node );
        mut_anode->set_line_color(2);
        mut_anode->draw( );
        _mut_anode_cards.push_back( mut_anode );
      }
    }

  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }

  update();

}

//____________________________________________________________________________
void Muon3DDisplay::draw_mut_anode_card( const char* name )
{

  // check name and convert to string
  if( !name ) return;
  string name_string( name );

  // make sure we are in the correct canvas
  _cv->cd();

  try {

    // loop over all possible cards find matching name
    // this is a copy of the code in mutgeom/MutArm::disable_anodes
    // this should be put in some independent piece of code
    MutAnodeMap::CardSet cards( MutAnodeMap::find_cards( name_string, false ) );
    for( MutAnodeMap::CardSet::const_iterator iter = cards.begin(); iter != cards.end(); iter++ )
    {
      for( int half = 0; half < MUTOO::MAX_HALF_OCTANT; half++ )
      {
        TMutIndex index( iter->arm(), iter->station(), iter->octant(), half, iter->gap() );
        TMutAnode3D* mut_anode = new TMutAnode3D( index, iter->card(), _rot_node );
        mut_anode->set_line_color(4);
        mut_anode->draw( );
        _mut_anode_cards.push_back( mut_anode );
      }
    }

  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }

  update();

}
//____________________________________________________________________________
void Muon3DDisplay::draw_mut_anode_card(
  const int& arm,
  const int& station,
  const int& octant,
  const int& half,
  const int& gap,
  const unsigned int& card
  )
{

  // make sure we are in the correct canvas
  _cv->cd();

  try {

    // create wire object
    TMutIndex index( arm, station, octant, half, gap );
    TMutAnode3D* mut_anode = new TMutAnode3D( index, card, _rot_node );
    mut_anode->draw( );
    _mut_anode_cards.push_back( mut_anode );

  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }

  update();

}

//___________________________________
void Muon3DDisplay::draw_mui_detectors(
  const int& arm_,
  const int& plane_,
  const int& panel_
)
{

  // make sure we are in the correct canvas
  _cv->cd();

  try {

    unsigned int n_detectors_drawn( 0 );

    for( int arm=0; arm < MUIOO::MAX_ARM; arm++ )
    if( arm_ < 0 || arm_ == arm )
    for( int plane = 0; plane < MUIOO::MAX_PLANE; plane++ )
    if( plane_ < 0 || plane_ == plane )
    for( int panel = 0; panel < MUIOO::MAX_PANEL; panel++ )
    if( panel_ < 0 || panel_ == panel ) {
      TMuiIndex index( arm, plane, panel );
      panel_iterator It( _muid_panels.find( index ) );
      if( It != _muid_panels.end() && It->second && !It->second->drawn() ) {
//        It->second->set_line_color(18);  //mjl
        It->second->set_line_color(1);  //mjl
        It->second->draw( );
        n_detectors_drawn++;
      }
    }

  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }

  _draw_mui_detectors = true;
  update();

  return;
}

//___________________________________
void Muon3DDisplay::draw_rpc_detectors(const int& arm_,   const int& station_,
				       const int& octant_,const int& halfoct_,
				       const int& radseg_,const int& strip_)
{

  // make sure we are in the correct canvas
  _cv->cd();

  recoConsts *myrc = recoConsts::instance();

  int fThisRPCGeom = 1;//Prototype is default
  if(myrc->FlagExist("RpcGeomType")) {
    if(myrc->get_IntFlag("RpcGeomType")==0) {
      cout << "UNKNOWN RPC GEOMETRY" << endl; return; }
    if(myrc->get_IntFlag("RpcGeomType")==1) { fThisRPCGeom=1; }
    if(myrc->get_IntFlag("RpcGeomType")==2) { fThisRPCGeom=2; }
    if(myrc->get_IntFlag("RpcGeomType")==3) { fThisRPCGeom=3; } }

  //This defines (more or less) the outer frame for the RPC
  //It needs moving to RPCFULLGEOM
  //[station][radial segments][4 sides][2 pairs]
  int st_pairs[3][3][4][2] = {
    {{{36,59},{36,41},{59,54},{41,54}}, //rad seg 0  
     {{36,59},{36,41},{59,55},{41,55}}, //rad seg 1  station 1
     {{-1,-1},{-1,-1},{-1,-1},{-1,-1}}},//rad seg 2  
    {{{-1,-1},{-1,-1},{-1,-1},{-1,-1}}, //rad seg 0  
     {{-1,-1},{-1,-1},{-1,-1},{-1,-1}}, //rad seg 1  station 2
     {{-1,-1},{-1,-1},{-1,-1},{-1,-1}}},//rad seg 2  
    {{{34,61},{34,39},{61,55},{39,55}}, //rad seg 0  (40 not 39)
     {{33,61},{33,37},{61,57},{37,57}}, //rad seg 1  station 3
     {{33,62},{33,36},{62,26},{36,26}}}};//rad seg 2  
  
  int is_pairs[4][2] = {{ 1, 1},{ 1, 0},{ 1, 0},{ 0, 0}}; //same for all rad seg and stations

  try {
    if(fThisRPCGeom==1) {
      std::cout << "drawing rpc - only the ones in the prototype" << std::endl; }
    else if(fThisRPCGeom==2) {
      std::cout << "drawing rpc - (version 2) full geom" << std::endl; }
    else if(fThisRPCGeom==3) {
      std::cout << "drawing rpc - (version 3) final geom" << std::endl; }
    for(int arm=0 ; arm<2 ; arm++) {
      if(arm!=0 && fThisRPCGeom==1) { continue; }
      if(arm_!=arm && arm_>=0) { continue; }
      for(int station=0 ; station<3; station++) {
	if(station==0 && fThisRPCGeom==1) { continue; }
	if(station_!=station && station_>=0) { continue; }
	for(int octant=0 ; octant<8 ; octant++) {
	  if(octant!=4 && fThisRPCGeom==1) { continue; }
	  if(octant_!=octant && octant_>=0) { continue; }
	  for(int halfoct=0 ; halfoct<2 ; halfoct++) {
	    if(halfoct!=0 && fThisRPCGeom==1) { continue; }
	    if(halfoct_!=halfoct && halfoct_>=0) { continue; }
	    for(int radseg=0 ; radseg<3 ; radseg++) {
	      if(radseg_!=radseg && radseg_>=0) { continue; }
	      for(int strip=0 ; strip<64 ; strip++) {
		if(strip_!=strip && strip_>=0) { continue; }
		TRpcIndex index( arm, station ,octant, halfoct, radseg, strip);
		rpc_station_iterator It( _rpc_stations.find( index ) );
		if(arm==arm_ && station==station_ && octant==octant_ &&
		   halfoct==halfoct_ && radseg==radseg_ && strip==strip_) {
		  if(It != _rpc_stations.end() && It->second) {
		    It->second->set_line_color(2);
		    It->second->draw( );} }
		if(fThisRPCGeom==1 && It != _rpc_stations.end()&& It->second) {
		  //this would draw all strips and takes forever
		  It->second->draw( );}
	      } //strip
	      if(fThisRPCGeom>1) {
		for(int iside=0 ; iside<4 ; iside++) { 
		  TRpcIndex index( arm, station, octant, halfoct, radseg,
				   st_pairs[station][radseg][iside][0]*1e6+st_pairs[station][radseg][iside][1]);
	    
		  index.setstrip1(st_pairs[station][radseg][iside][0],is_pairs[iside][0]);
		  index.setstrip2(st_pairs[station][radseg][iside][1],is_pairs[iside][1]);
		  
		  rpc_station_iterator It( _rpc_stations.find( index ) );
		  if(It != _rpc_stations.end() && It->second) { It->second->draw( ); }
		} }
	      
	      //cout << arm << " " << station << " " << octant << " "
	      //   << halfoct << " " << radseg << endl;
	    } //radseg
	  } //halfoct
	} //octant
      } //station
    } //arm
  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }
  
  update();
  
  return;
}

//___________________________________
void Muon3DDisplay::draw_fvtx_detectors( const int& arm_, const int& cage_, const int& station_, const int& sector_ )
{

  // make sure we are in the correct canvas
  _cv->cd();

  try {

    unsigned int n_detectors_drawn( 0 );

    for( int arm=0; arm < FVTXOO::MAX_ARM; arm++ )
    if( arm_ < 0 || arm_ == arm )
    for( int cage=0; cage < FVTXOO::MAX_CAGE; cage++ )
    if( cage_ < 0 || cage_ == cage )
    for( int station = 0; station < FVTXOO::MAX_STATION; station++ )
    if( station_ < 0 || station_ == station )
    for( int sector = 0; sector < FVTXOO::MAX_SECTOR; sector++ )
    if( sector_ < 0 || sector_ == sector )
    {
      TFvtxIndex index( arm, cage, station, sector, 0 );
      fvtx_sector_iterator It( _fvtx_sectors.find( index ) );
      if( It != _fvtx_sectors.end() && It->second && !It->second->drawn() )
      {
        It->second->draw( );
        n_detectors_drawn++;
      }
    }

  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }

  update();

  return;
}

//___________________________________
void Muon3DDisplay::draw_fvtx_columns(
    const int& arm_, const int& cage_, const int& station_, const int& sector_, const int& column_ )
{

  // make sure we are in the correct canvas
  _cv->cd();

  try {

    unsigned int n_detectors_drawn( 0 );

    for( int arm=0; arm < FVTXOO::MAX_ARM; arm++ )
    if( arm_ < 0 || arm_ == arm )
    for( int cage=0; cage < FVTXOO::MAX_CAGE; cage++ )
    if( cage_ < 0 || cage_ == cage )
    for( int station = 0; station < FVTXOO::MAX_STATION; station++ )
    if( station_ < 0 || station_ == station )
    for( int sector = 0; sector < FVTXOO::MAX_SECTOR; sector++ )
    if( sector_ < 0 || sector_ == sector )
    for( int column = 0; column < FVTXOO::MAX_COLUMN; column++ )
    if( column_ < 0 || column_ == column )
    {
      TFvtxIndex index( arm, cage, station, sector, column );
      fvtx_column_iterator It( _fvtx_columns.find( index ) );
      if( It != _fvtx_columns.end() && It->second && !It->second->drawn() )
      {
        It->second->draw( );
        n_detectors_drawn++;
      }
    }

  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }

  update();

  return;
}

//___________________________________
void Muon3DDisplay::draw_mut_coords( void )
{

  // check top_node
  if( !_top_node ) return;

  // make sure we are in the correct canvas
  _cv->cd();

  try {
    // retrieve coord map
    TMutCoordMap* coord_map = TMutNode<TMutCoordMap>::find_node( _top_node,"TMutCoordMap" );
    unsigned int n_mut_coords_drawn( 0 );
    if( !coord_map ) return;

    // loop over coordinates
    TMutCoordMap::iterator coord_iter = coord_map->range();
    while( TMutCoordMap::pointer coord_ptr = coord_iter.next() )
    {

      // build index
      TMutIndex index(
        coord_ptr->get()->get_arm(),
        coord_ptr->get()->get_station(),
        coord_ptr->get()->get_octant(),
        coord_ptr->get()->get_half_octant(),
        coord_ptr->get()->get_gap()
      );

      // retrieve associated detector, check if drawn
      gap_iterator It( _mut_gaps.find( index ) );
      if( It == _mut_gaps.end() || !( It->second && It->second->drawn() ) ) continue;

      // build and draw Coord3D object
      TMutCoord3D *coord = new TMutCoord3D( _rot_node, *coord_ptr);
      //coord->set_line_color(33);
      coord->set_line_color(2);
      coord->draw();
      if( coord->drawn() ) n_mut_coords_drawn++;
      _mut_coords.push_back( coord );

    }

    cout << "Muon3DDisplay::draw_mut_coords - " << n_mut_coords_drawn << " coords drawn." << endl;

  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }

  _mut_coords.set_enabled( true );
  update();
  return;
}

//___________________________________
void Muon3DDisplay::draw_mut_stubs( void )
{

  // check top_node
  if( !_top_node ) return;

  // make sure we are in the correct canvas
  _cv->cd();

  try {
    // retrieve stup map
    TMutStubMap* stub_map = TMutNode<TMutStubMap>::find_node( _top_node,"TMutStubMap" );
    unsigned int n_mut_stubs_drawn( 0 );
    if( !stub_map ) return;

    // loop stubs
    TMutStubMap::iterator stub_iter = stub_map->range();
    while( TMutStubMap::pointer stub_ptr = stub_iter.next() ) {

      // check at least one drawn gap matches stub
      bool found_gap( false );
      for( int gap = 0; gap < ((stub_ptr->get()->get_station() == MUTOO::Station3 ) ? 2:3 ); gap++ ) {

        // build index
        TMutIndex index(
          stub_ptr->get()->get_arm(),
          stub_ptr->get()->get_station(),
          stub_ptr->get()->get_octant(),
          stub_ptr->get()->get_half_octant(),
          gap
        );

        // retrieve associated detector, check if drawn
        gap_iterator It( _mut_gaps.find( index ) );
        if( It != _mut_gaps.end() && It->second && It->second->drawn() ) {
          found_gap = true;
          break;
        }
      }

      if( !found_gap ) continue;

      // build and draw 3D object
      TMutStub3D *stub = new TMutStub3D( _rot_node, *stub_ptr);
      stub->draw();
      if( stub->drawn() ) n_mut_stubs_drawn++;
      _mut_stubs.push_back( stub );
    }

    cout << "Muon3DDisplay::draw_mut_stubs - " << n_mut_stubs_drawn << " stubs drawn." << endl;

  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }

  _mut_stubs.set_enabled( true );
  update();
  return;
}

//___________________________________
void Muon3DDisplay::draw_mui_clusters( void )
{

  // check top_node
  if( !_top_node ) return;

  // make sure we are in the correct canvas
  _cv->cd();

  try {
    // retrieve coord map
    TMuiClusterMapO* cluster_map = TMutNode<TMuiClusterMapO>::find_node( _top_node,"TMuiClusterMapO" );
    unsigned int n_clusters_drawn( 0 );
    if( !cluster_map ) return;

    // loop over coordinates
    TMuiClusterMapO::iterator cluster_iter = cluster_map->range();
    while( TMuiClusterMapO::pointer cluster_ptr = cluster_iter.next() ) {

      // build index
      TMuiIndex index(
        cluster_ptr->get()->get_arm(),
        cluster_ptr->get()->get_plane(),
        cluster_ptr->get()->get_panel()
      );

      // retrieve associated detector, check if drawn
      panel_iterator It( _muid_panels.find( index ) );
      if( It == _muid_panels.end() || !( It->second && It->second->drawn() ) )
      continue;

      // build and draw 3D object
      TMuiCluster3D *cluster = new TMuiCluster3D( _rot_node, *cluster_ptr);
      //cluster->set_line_color(38);  // mjl
      cluster->set_line_color(2);  // mjl
      cluster->draw();
      if( cluster->drawn() ) n_clusters_drawn++;
      _mui_clusters.push_back( cluster );

    }

    cout << "Muon3DDisplay::draw_mui_clusters - " << n_clusters_drawn << " mui clusters drawn." << endl;

  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }

  _mui_clusters.set_enabled( true );
  update();
  return;
}

//___________________________________
void Muon3DDisplay::draw_mui_roads( void )
{

  // check top_node
  if( !_top_node ) return;

  // make sure we are in the correct canvas
  _cv->cd();

  try {
    // retrieve coord map
    TMuiRoadMapO* road_map = TMutNode<TMuiRoadMapO>::find_node( _top_node,"TMuiRoadMapO" );
    unsigned int n_roads_drawn( 0 );
    if( !road_map ) return;

    // loop over coordinates
    TMuiRoadMapO::iterator road_iter = road_map->range();
    while( TMuiRoadMapO::pointer road_ptr = road_iter.next() ) {

      // build and draw TMuiRoad3D object
      TMuiRoad3D *road = new TMuiRoad3D( _rot_node, *road_ptr);
      road->set_line_color( 6 );
      road->set_line_width( 2 );

//       if( _is_l2_candidate( *road_ptr ) ) {
//         road->set_line_color( 5 );
//         road->set_line_width( 2 );
//       }

      road->draw();
      if( road->drawn() ) n_roads_drawn++;
      _mui_roads.push_back( road );
    }

    cout << "Muon3DDisplay::draw_mui_roads - " << n_roads_drawn << " mui roads drawn." << endl;

  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }

  _mui_roads.set_enabled( true );
  update();
  return;
}


//___________________________________
void Muon3DDisplay::draw_tracks( void )
{

  // check top_node
  if( !_top_node ) return;

  // make sure we are in the correct canvas
  _cv->cd();

  try {

    // retrieve track map
    TMutTrkMap* trk_map = TMutNode<TMutTrkMap>::find_node( _top_node,"TMutTrkMap" );
    if( !trk_map ) return;

    // loop over tracks
    unsigned int n_tracks_drawn( 0 );
    TMutTrkMap::iterator trk_iter = trk_map->range();
    while( TMutTrkMap::pointer  trk_ptr = trk_iter.next() )
    {

      // remove ghost tracks and badly reconstructed tracks
      if( trk_ptr->get()->get_ghost() || !trk_ptr->get()->get_reco_success() ) continue;

      // this is experimental code.
      // not everything has been committed to CVS.
      // if( !trk_ptr->get()->get_status( TMutTrk::BOTH_ARMS ) ) continue;

      // print track parameters at vertex
      if( true )
      {
        TMutTrkPar trk_par_vtx( *trk_ptr->get()->get_trk_par_vtx() );
        // dump vtx track parameters
        cout << " track parameters vertex = {";
        cout << setw(5) << setprecision(3) << setiosflags(ios::showpoint) << setiosflags(ios::fixed);
        cout << trk_par_vtx.get_x() << ", ";
        cout << trk_par_vtx.get_y() << ", ";
        cout << trk_par_vtx.get_px() << ", ";
        cout << trk_par_vtx.get_py() << ", ";
        cout << trk_par_vtx.get_pz() << "}" << std::endl;
        cout << " ptot: = " << trk_par_vtx.get_ptot() << "	 z reference: " << trk_par_vtx.get_z() << std::endl;
      }

      // build and draw track3D object
      TMutTrack3D *track = new TMutTrack3D( _rot_node, *trk_ptr );

      // set special colors for tracks that span along both arms
      track->set_line_color(4);
      track->set_line_width(2);
      track->draw();
      if( track->drawn() ) n_tracks_drawn++;
      _tracks.push_back( track );

    }

    cout << "Muon3DDisplay::draw_tracks - " << n_tracks_drawn << " tracks drawn." << endl;

  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }

  _tracks.set_enabled( true );
  update();
  return;
}

//___________________________________
void Muon3DDisplay::draw_mc_tracks( void )
{

  // check top_node
  if( !_top_node ) return;

  // make sure we are in the correct canvas
  _cv->cd();

  try {
    // retrieve track map
    TMutMCTrkMap* mc_trk_map = MuonUtil::find_node<TMutMCTrkMap>("TMutMCTrkMap" );
    if( !mc_trk_map ) return;

    // loop over tracks
    unsigned int n_mc_tracks_drawn( 0 );
    TMutMCTrkMap::iterator mc_trk_iter = mc_trk_map->range();
    while( TMutMCTrkMap::pointer  mc_trk_ptr = mc_trk_iter.next() )
    {

      // check track is reconstructible (needs one hit in at least one mutr station
      if( !(
        mc_trk_ptr->get()->has_hits( MUTOO::Station1 ) ||
        mc_trk_ptr->get()->has_hits( MUTOO::Station2 ) ||
        mc_trk_ptr->get()->has_hits( MUTOO::Station3 ) ) ) continue;


      // build and draw MCTrack3D object
      TMutMCTrack3D *mc_track = new TMutMCTrack3D( _rot_node, *mc_trk_ptr  );

      mc_track->draw();
      if( mc_track->drawn() ) n_mc_tracks_drawn++;
      _mc_tracks.push_back( mc_track );

    }

    cout << "Muon3DDisplay::draw_mc_tracks - " << n_mc_tracks_drawn << " mc tracks drawn." << endl;

  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }

  _mc_tracks.set_enabled( true );
  update();
  return;
}

#ifdef __USE_RPC__
//___________________________________
void Muon3DDisplay::draw_rpc_mc_hits( void )
{

  // check top_node
  if( !_top_node ) return;

  // make sure we are in the correct canvas
  _cv->cd();

  try {
    // retrieve track map
    TMutMCTrkMap* mc_trk_map = MuonUtil::find_node<TMutMCTrkMap>("TMutMCTrkMap" );
    if( !mc_trk_map ) return;

    // loop over tracks
    unsigned int n_rpc_mc_hits_drawn( 0 );
    TMutMCTrkMap::iterator mc_trk_iter = mc_trk_map->range();
    while( TMutMCTrkMap::pointer  mc_trk_ptr = mc_trk_iter.next() )
    {

      // get associated TRpcMCHits
      TRpcMCHitMap::key_iterator hit_iter( mc_trk_ptr->get()->get_associated<TRpcMCHit>() );
      while( TRpcMCHitMap::pointer hit_ptr = hit_iter.next() )
      {
        // get pads associated to MC hits
        TRpcMCHit::pad_list pads( hit_ptr->get()->get_pad_list() );
        for( TRpcMCHit::pad_iterator pad_iter = pads.begin(); pad_iter!=pads.end(); pad_iter++ )
        {

          PHObj3D *pad = ( hit_ptr->get()->get_station() == RPCOO::Station3 ) ?
              (PHObj3D*) new TRpcPad3DType2( hit_ptr->get()->get_arm(), hit_ptr->get()->get_station(), (*pad_iter)->get_pad(), _rot_node  ):
              (PHObj3D*) new TRpcPad3DType1( hit_ptr->get()->get_arm(), hit_ptr->get()->get_station(), (*pad_iter)->get_pad(), _rot_node  );
          pad->draw();
          _rpc_mc_hits.push_back( pad );
          n_rpc_mc_hits_drawn++;
        }
      }

    }
    cout << "Muon3DDisplay::draw_rpc_mc_hits - " << n_rpc_mc_hits_drawn << " hits drawn." << endl;

  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }

  _rpc_mc_hits.set_enabled( true );
  update();
  return;
}
#endif

//___________________________________
void Muon3DDisplay::draw_rpc_coords()
{

  //#ifdef __USE_RPC__
  recoConsts *myrc = recoConsts::instance();
  
  int fThisRPCGeom = 1;//Prototype is default
  if(myrc->FlagExist("RpcGeomType")) {
    if(myrc->get_IntFlag("RpcGeomType")==0) {
      std::cout << "UNKNOWN RPC GEOMETRY" << std::endl;
      return; }
    if(myrc->get_IntFlag("RpcGeomType")==1) { fThisRPCGeom=1; }
    if(myrc->get_IntFlag("RpcGeomType")==2) { fThisRPCGeom=2; }
    if(myrc->get_IntFlag("RpcGeomType")==3) { fThisRPCGeom=3; } }

  if(fThisRPCGeom==1) {
    std::cout << "Muon3DDisplay::draw_rpc_coords - DRAWING UNDER THE ASSUMPTION THAT THIS IS:" << std::endl
	      << "        ***** PROTOTYPE DATA *****        " << std::endl
	      << "******************************************" << std::endl;}

  // check top_node
  if( !_top_node ) return;

  // make sure we are in the correct canvas
  _cv->cd();

  try {
    // retrieve track map
    TRpcHitMap* hit_map = TMutNode<TRpcHitMap>::find_node( _top_node,"TRpcHitMap" );
    if( !hit_map ) return;
    std::cout << "Found RPC Coords" << std::endl;
    
    // loop over tracks
    unsigned int n_rpc_hits_drawn( 0 );
    TRpcHitMap::iterator hit_iter = hit_map->range();
    while( TRpcHitMap::pointer hit_ptr = hit_iter.next() )
      {
	int thisarm     = hit_ptr->get()->get_arm();
	int thisstation = hit_ptr->get()->get_station();
	int thisoctant  = hit_ptr->get()->get_octant();
	int thishalfoct = hit_ptr->get()->get_half_octant();
	int thisradseg  = hit_ptr->get()->get_rseg();
	int thisstrip   = hit_ptr->get()->get_strip();

	if(fThisRPCGeom==1) {//make sure that the data is correct
	  thisarm=0;
	  thisoctant=4;
	  thishalfoct=0; }

	TRpcIndex index(thisarm,thisstation,thisoctant,thishalfoct,
			thisradseg,thisstrip);

	PHObj3D *strip_3d = (PHObj3D*)new TRpcStrip3D( index, _rot_node  );
	strip_3d->set_line_color(2);
	strip_3d->set_line_width(3);
	strip_3d->draw();
	_rpc_hits.push_back( strip_3d );
	//delete strip_3d;
	n_rpc_hits_drawn++;}

    cout << "Muon3DDisplay::draw_rpc_coords - " << n_rpc_hits_drawn << " coords drawn." << endl;

  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }

  _rpc_hits.set_enabled( true );
  update();
  return;
}

//___________________________________
void Muon3DDisplay::draw_fvtx_mc_hits( void )
{

  // check top_node
  if( !_top_node ) return;

  // make sure we are in the correct canvas
  _cv->cd();

  clear_fvtx_detectors();

  try {
    // retrieve track map
    TMutMCTrkMap* mc_trk_map = MuonUtil::find_node<TMutMCTrkMap>("TMutMCTrkMap" );
    if( !mc_trk_map ) return;

    // loop over tracks
    unsigned int n_fvtx_mc_hits_drawn( 0 );
    TMutMCTrkMap::iterator mc_trk_iter = mc_trk_map->range();
    while( TMutMCTrkMap::pointer  mc_trk_ptr = mc_trk_iter.next() )
    {

      // get associated TFvtxMCHits
      TFvtxMCHitMap::key_iterator hit_iter( mc_trk_ptr->get()->get_associated<TFvtxMCHit>() );

      while( TFvtxMCHitMap::pointer hit_ptr = hit_iter.next() )
      {
        PHObj3D* mc_hit = new TFvtxMCHit3D( _rot_node, *hit_ptr );
        mc_hit->draw();
        _fvtx_mc_hits.push_back( mc_hit );
        n_fvtx_mc_hits_drawn++;

        draw_fvtx_detectors(hit_ptr->get()->get_arm(), hit_ptr->get()->get_cage(), hit_ptr->get()->get_station(), hit_ptr->get()->get_sector());

      }

    }
    //cout << "Muon3DDisplay::draw_fvtx_mc_hits - " << n_fvtx_mc_hits_drawn << " hits drawn." << endl;

  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }
  _fvtx_mc_hits.set_enabled( true );
  update();
  return;
}

//___________________________________
void Muon3DDisplay::draw_fvtx_mc_tracks( void )
{

  // check top_node
  if( !_top_node ) return;

  // make sure we are in the correct canvas
  _cv->cd();

  try {
    // retrieve track map
    TMutMCTrkMap* mc_trk_map = MuonUtil::find_node<TMutMCTrkMap>("TMutMCTrkMap" );
    if( !mc_trk_map ) return;

    // loop over tracks
    unsigned int n_fvtx_mc_tracks_drawn( 0 );
    TMutMCTrkMap::iterator mc_trk_iter = mc_trk_map->range();
    // cout << "mc trk map size " << mc_trk_iter.count() << endl;
    while( TMutMCTrkMap::pointer  mc_trk_ptr = mc_trk_iter.next() )
    {
/*
      // check track is reconstructible (needs one hit in at least one mutr station
      if( !(
        mc_trk_ptr->get()->has_hits( MUTOO::Station1 ) ||
        mc_trk_ptr->get()->has_hits( MUTOO::Station2 ) ||
        mc_trk_ptr->get()->has_hits( MUTOO::Station3 ) ) ) continue;
*/

      // build and draw MCTrack3D object
      TFvtxMCTrack3D *mc_track = new TFvtxMCTrack3D( _rot_node, *mc_trk_ptr  );

      mc_track->draw();
      if( mc_track->drawn() ) n_fvtx_mc_tracks_drawn++;
      _fvtx_mc_tracks.push_back( mc_track );

    }

    //cout << "Muon3DDisplay::draw_fvtx_mc_tracks - " << n_fvtx_mc_tracks_drawn << " fvtx mc tracks drawn." << endl;
  
  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }
  
  _fvtx_mc_tracks.set_enabled( true );
  update();
  return;
}

//___________________________________
void Muon3DDisplay::draw_fvtx_tracks( void )
{

  // check top_node
  if( !_top_node ) return;

  // make sure we are in the correct canvas
  _cv->cd();

  try {

    // retrieve track map
    TFvtxTrkMap* trk_map = TMutNode<TFvtxTrkMap>::find_node( _top_node,"TFvtxTrkMap" );
    if( !trk_map ) return;

    // loop over tracks
    unsigned int n_fvtx_tracks_drawn( 0 );
    TFvtxTrkMap::iterator trk_iter = trk_map->range();
    //cout << "fvtx trk map size " << trk_map->count() << endl;
    while( TFvtxTrkMap::pointer  trk_ptr = trk_iter.next() )
    {

      // remove ghost tracks and badly reconstructed tracks
      if( trk_ptr->get()->get_ghost() || !trk_ptr->get()->get_reco_success() ) continue;

      // print track parameters at vertex
      if( false )
      {
        TMutTrkPar trk_par_vtx( *trk_ptr->get()->get_trk_par_vtx() );
        // dump vtx track parameters
        cout << " track parameters vertex = {";
        cout << setw(5) << setprecision(3) << setiosflags(ios::showpoint) << setiosflags(ios::fixed);
        cout << trk_par_vtx.get_x() << ", ";
        cout << trk_par_vtx.get_y() << ", ";
        cout << trk_par_vtx.get_px() << ", ";
        cout << trk_par_vtx.get_py() << ", ";
        cout << trk_par_vtx.get_pz() << "}" << std::endl;
        cout << " ptot: = " << trk_par_vtx.get_ptot() << "       z reference: " << trk_par_vtx.get_z() << std::endl;
      }

      // build and draw track3D object
      TFvtxTrack3D *track = new TFvtxTrack3D( _rot_node, *trk_ptr );

      // set special colors for tracks that span along both arms
      track->set_line_color(6); 
      track->set_line_width(1);
      track->draw();
      if( track->drawn() ) n_fvtx_tracks_drawn++;
      _fvtx_tracks.push_back( track );

    }

    //cout << "Muon3DDisplay::draw_fvtx_tracks - " << n_fvtx_tracks_drawn << " fvtx tracks drawn." << endl; 

  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }

  _fvtx_tracks.set_enabled( true ); 
  update();
  return;
}

//___________________________________
void Muon3DDisplay::clear( void )
{

  // clear lists
  clear_mc_tracks();
  clear_tracks();
  clear_mut_coords();
  clear_mut_stubs();
  clear_mui_clusters();
  clear_mui_roads();
#ifdef __USE_RPC__
  clear_rpc_mc_hits();
#endif
  clear_rpc_coords();
  clear_rpc_detectors();
  clear_mut_detectors();
  clear_mui_detectors();
  clear_fvtx_detectors();
  clear_fvtx_columns();
  clear_fvtx_mc_hits();
  clear_fvtx_mc_tracks();
  clear_fvtx_tracks();
  update();

}

//___________________________________
void Muon3DDisplay::clear_mut_detectors( void )
{

  // clear list of detectors
  for( gap_iterator It = _mut_gaps.begin(); It != _mut_gaps.end(); It++ )
  if( It->second ) It->second->hide();
  update();

}

//___________________________________
void Muon3DDisplay::clear_mui_detectors( void )
{

  // clear list of detectors
  for( panel_iterator It = _muid_panels.begin(); It != _muid_panels.end(); It++ )
  if( It->second ) It->second->hide();
  update();

}

//___________________________________
void Muon3DDisplay::clear_rpc_detectors( void )
{

  // clear list of detectors
  for( rpc_station_iterator It = _rpc_stations.begin(); It != _rpc_stations.end(); It++ )
  if( It->second ) It->second->hide();
  update();

}

//___________________________________
void Muon3DDisplay::clear_fvtx_detectors( void )
{

  // clear list of detectors
  for( fvtx_sector_iterator It = _fvtx_sectors.begin(); It != _fvtx_sectors.end(); It++ )
  if( It->second ) It->second->hide();
  update();

}

//___________________________________
void Muon3DDisplay::clear_fvtx_columns( void )
{

  // clear list of detectors
  for( fvtx_column_iterator It = _fvtx_columns.begin(); It != _fvtx_columns.end(); It++ )
  if( It->second ) It->second->hide();
  update();

}

//___________________________________
void Muon3DDisplay::update( void )
{

  // make sure we are in the correct canvas
  _cv->cd();
  if( _main_node ) _main_node->Draw();
  if( _cv ) _cv->Update();
}

//___________________________________
void Muon3DDisplay::menu( void )
{
  TGMainFrame* main_frame = new TGMainFrame( gClient->GetRoot(), 100, 300 );

  TGLayoutHints *hint = new TGLayoutHints( kLHintsExpandX, 0,1,0,1);
  TGCheckButton *button;

  TGVerticalFrame* menu_frame = new TGVerticalFrame( main_frame, 100, 300 );
  button = new TGCheckButton( menu_frame, "MUTR &detectors" );
  button->SetState( (EButtonState)_draw_mut_detectors );
  button->Connect("Clicked()","Muon3DDisplay",this,"toggle_mut_detectors()" );
  menu_frame->AddFrame( button, hint );

  button = new TGCheckButton( menu_frame, "MUTR &tracks" );
  button->SetState( (EButtonState) _tracks.enabled() );
  button->Connect("Clicked()","Muon3DDisplay",this,"toggle_tracks()" );
  menu_frame->AddFrame( button, hint );

  button = new TGCheckButton( menu_frame, "MUTR &stubs" );
  button->SetState( (EButtonState)_mut_stubs.enabled() );
  button->Connect("Clicked()","Muon3DDisplay",this,"toggle_mut_stubs()" );
  menu_frame->AddFrame( button, hint );

  button = new TGCheckButton( menu_frame, "MUTR &coordinates" );
  button->SetState( (EButtonState)_mut_coords.enabled() );
  button->Connect("Clicked()","Muon3DDisplay",this,"toggle_mut_coords()" );
  menu_frame->AddFrame( button, hint );

  button = new TGCheckButton( menu_frame, "MUID d&etectors" );
  button->SetState( (EButtonState)_draw_mui_detectors );
  button->Connect("Clicked()","Muon3DDisplay",this,"toggle_mui_detectors()" );
  menu_frame->AddFrame( button, hint );

  button = new TGCheckButton( menu_frame, "MUID &roads" );
  button->SetState( (EButtonState)_mui_roads.enabled() );
  button->Connect("Clicked()","Muon3DDisplay",this,"toggle_mui_roads()" );
  menu_frame->AddFrame( button, hint );

  button = new TGCheckButton( menu_frame, "MUID c&lusters" );
  button->SetState( (EButtonState)_mui_clusters.enabled() );
  button->Connect("Clicked()","Muon3DDisplay",this,"toggle_mui_clusters()" );
  menu_frame->AddFrame( button, hint );

  button = new TGCheckButton( menu_frame, "MC tr&acks" );
  button->SetState( (EButtonState)_mc_tracks.enabled() );
  button->Connect("Clicked()","Muon3DDisplay",this,"toggle_mc_tracks()" );
  menu_frame->AddFrame( button, hint );

  button = new TGCheckButton( menu_frame, "RPC d&etectors" );
  button->SetState( (EButtonState)_draw_rpc_detectors );
  button->Connect("Clicked()","Muon3DDisplay",this,"toggle_rpc_detectors()" );
  menu_frame->AddFrame( button, hint );

#ifdef __USE_RPC__
  button = new TGCheckButton( menu_frame, "RPC &mc hits" );
  button->SetState( (EButtonState)_rpc_mc_hits.enabled() );
  button->Connect("Clicked()","Muon3DDisplay",this,"toggle_rpc_mc_hits()" );
  menu_frame->AddFrame( button, hint );
#endif

  button = new TGCheckButton( menu_frame, "RPC c&oords" );
  button->SetState( (EButtonState)_rpc_hits.enabled() );
  button->Connect("Clicked()","Muon3DDisplay",this,"toggle_rpc_coords()" );
  menu_frame->AddFrame( button, hint );

  button = new TGCheckButton( menu_frame, "F&VTX detectors" );
  button->SetState( (EButtonState)_draw_fvtx_detectors );
  button->Connect("Clicked()","Muon3DDisplay",this,"toggle_fvtx_detectors()" );
  menu_frame->AddFrame( button, hint );

  button = new TGCheckButton( menu_frame, "FVTX colu&mns" );
  button->SetState( (EButtonState)_draw_fvtx_columns );
  button->Connect("Clicked()","Muon3DDisplay",this,"toggle_fvtx_columns()" );
  menu_frame->AddFrame( button, hint );

  button = new TGCheckButton( menu_frame, "FVTX MC hits" );
  button->SetState( (EButtonState) _fvtx_mc_hits.enabled() );
  button->Connect("Clicked()","Muon3DDisplay",this,"toggle_fvtx_mc_hits()" );
  menu_frame->AddFrame( button, hint );

  button = new TGCheckButton( menu_frame, "&auto draw" );
  button->SetState( (EButtonState)_auto_draw );
  button->Connect("Clicked()","Muon3DDisplay",this,"toggle_auto_draw()" );
  menu_frame->AddFrame( button, hint );

  menu_frame->MapSubwindows();
  menu_frame->Resize(menu_frame->GetDefaultSize());
  menu_frame->MapWindow();

  main_frame->SetWindowName( "menu" );
  main_frame->MapSubwindows();
  main_frame->Resize(menu_frame->GetDefaultSize());
  main_frame->MapWindow();

  return;
}

//___________________________________
void Muon3DDisplay::_init_display( void )
{

  gROOT->SetStyle("Plain");

  // Create canvas
  _cv = new TCanvas("cv","phenix muon tracker",200,10,1000,500);
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,15,8)
  _view = TView::CreateView(2);
#else
  _view = new TView(2);
#endif

  // Create Main shape/node
  TBRIK* mainShape = new TBRIK( "mainS", "mainS", "void", 0.1, 0.1, 0.1 );
  _main_node  = new TNode( "mainN", "mainN", mainShape, 0, 0, 0 );
  _main_node->SetLineColor( 0 );
  _main_node->cd();

  // Create rotated shape/node
  TBRIK* rot_shape = new TBRIK( "rotS", "rotS", "void", 0.1, 0.1, 0.1 );
  _rot_node  = new TNode( "rotN", "rotN", rot_shape, 0, 0, 0 );

  double rot[] = {
     0, 1, 0,
     0, 0, 1,
     1, 0, 0 };

  TRotMatrix *rotM = new TRotMatrix( "rotM", "rotM", rot );
  _rot_node->SetMatrix( rotM );
  _rot_node->SetLineColor( 0 );
  _rot_node->cd();

  return;

}

//___________________________________
void Muon3DDisplay::_init_detectors( void )
{
  MUTOO::PRINT( cout, "Muon3DDisplay::_init_detectors" );

  //! initialize mutr detectors
  try {
    for( int arm=0; arm < MUTOO::MAX_ARM; arm++ )
    for( int station = 0; station < MUTOO::MAX_STATION; station++ )
    for( int octant = 0; octant < MUTOO::MAX_OCTANT; octant++ )
    for( int half = 0; half < MUTOO::MAX_HALF_OCTANT; half++ )
    for( int gap = 0; gap < (( station == MUTOO::Station3 ) ? 2:3 ); gap++ )
    {
      TMutIndex index( arm, station, octant, half, gap );
      _mut_gaps.insert( make_pair( index, new TMutGap3D( index, _rot_node ) ) );
    }

    cout << _mut_gaps.size() << " gaps created." << endl;
  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }

  // initialize muid detectors
  try {

    for( int arm=0; arm < MUIOO::MAX_ARM; arm++ )
    for( int plane = 0; plane < MUIOO::MAX_PLANE; plane++ )
    for( int panel = 0; panel < MUIOO::MAX_PANEL; panel++ )
    {
      TMuiIndex index( arm, plane, panel );
      _muid_panels.insert( make_pair( index, new TMuiPanel3D( index, _rot_node ) ) );
    }

    cout << _muid_panels.size() << " muid panels created." << endl;
  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }

  
  recoConsts *myrc = recoConsts::instance();
  
  int fThisRPCGeom = 1;//Prototype is default
  if(myrc->FlagExist("RpcGeomType")) {
    if(myrc->get_IntFlag("RpcGeomType")==0) {
      std::cout << "UNKNOWN RPC GEOMETRY" << std::endl;
      return; }
    if(myrc->get_IntFlag("RpcGeomType")==1) { fThisRPCGeom=1; }
    if(myrc->get_IntFlag("RpcGeomType")==2) { fThisRPCGeom=2; }
    if(myrc->get_IntFlag("RpcGeomType")==3) { fThisRPCGeom=3; } }
  
  //This defines (more or less) the outer frame for the RPC
  //It needs moving to RPCFULLGEOM
  //[station][radial segments][4 sides][2 pairs]
  int st_pairs[3][3][4][2] = {
    {{{36,59},{36,41},{59,54},{41,54}}, //rad seg 0  
     {{36,59},{36,41},{59,55},{41,55}}, //rad seg 1  station 1
     {{-1,-1},{-1,-1},{-1,-1},{-1,-1}}},//rad seg 2  
    {{{-1,-1},{-1,-1},{-1,-1},{-1,-1}}, //rad seg 0  
     {{-1,-1},{-1,-1},{-1,-1},{-1,-1}}, //rad seg 1  station 2
     {{-1,-1},{-1,-1},{-1,-1},{-1,-1}}},//rad seg 2  
    {{{34,61},{34,39},{61,55},{39,55}}, //rad seg 0  
     {{33,61},{33,37},{61,57},{37,57}}, //rad seg 1  station 3
     {{33,62},{33,36},{62,26},{36,26}}}};//rad seg 2  
  
  int is_pairs[4][2] = {{ 1, 1},{ 1, 0},{ 1, 0},{ 0, 0}}; //same for all rad seg and stations
  
  // initialize rpc detectors
  try {
    for(int arm=0 ; arm<2 ; arm++) {
      if(arm!=0 && fThisRPCGeom==1) { continue; }
      for(int station=0 ; station<3; station++) {
	if(station==0 && fThisRPCGeom==1) { continue; }
	for(int octant=0 ; octant<8 ; octant++) {
	  if(octant!=4 && fThisRPCGeom==1) { continue; }
	  for(int halfoct=0 ; halfoct<2 ; halfoct++) {
	    if(halfoct!=0 && fThisRPCGeom==1) { continue; }
	    for(int radseg=0 ; radseg<3 ; radseg++) {
	      for(int strip=0 ; strip<64 ; strip++) {
		TRpcIndex index( arm, station ,octant, halfoct, radseg, strip);
		PHObj3D *strip_3d = new TRpcStrip3D( index, _rot_node);
		strip_3d->set_line_color(17);
		_rpc_stations.insert( make_pair( index, strip_3d ) );
		/*delete strip_3d; */}
	      
	      for(int iside=0 ; iside<4 ; iside++) {
		TRpcIndex index( arm, station, octant, halfoct, radseg,
				 st_pairs[station][radseg][iside][0]*1e6+st_pairs[station][radseg][iside][1]);
		index.setstrip1(st_pairs[station][radseg][iside][0],is_pairs[iside][0]);
		index.setstrip2(st_pairs[station][radseg][iside][1],is_pairs[iside][1]);
		PHObj3D *strip_3d = new TRpcStrip3D( index, _rot_node);
		strip_3d->set_line_color(600);//blue
		strip_3d->set_line_width(2);
		_rpc_stations.insert( make_pair( index, strip_3d ) );
		/*delete strip_3d;*/ }
	      
	      
	    } //radseg
	  } //halfoct
	} //octant
      } //station
    } //arm
    
    cout << _rpc_stations.size() << " rpc stations created." << endl;
  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }
/*
  // initialize fvtx detectors
  try {

    for( int arm=0; arm < FVTXOO::MAX_ARM; arm++ )
    for( int cage = 0; cage < FVTXOO::MAX_CAGE; cage++ )
    for( int station = 0; station < FVTXOO::MAX_STATION; station++ )
    for( int sector = 0; sector < FVTXOO::MAX_SECTOR; sector++ )
    {
      TFvtxIndex index( arm, cage, station, sector );
      _fvtx_sectors.insert( make_pair( index, new TFvtxSector3D( index, _rot_node ) ) );
    }

    cout << _fvtx_sectors.size() << " fvtx sectors created." << endl;
  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }

  // initialize fvtx columns
  try {

    for( int arm=0; arm < FVTXOO::MAX_ARM; arm++ )
    for( int cage = 0; cage < FVTXOO::MAX_CAGE; cage++ )
    for( int station = 0; station < FVTXOO::MAX_STATION; station++ )
    for( int sector = 0; sector < FVTXOO::MAX_SECTOR; sector++ )
    for( int column = 0; column < FVTXOO::MAX_COLUMN; column++ )
    {
      TFvtxIndex index( arm, cage, station, sector, column );
      TFvtxColumn3D *column3d = new TFvtxColumn3D( index, _rot_node );
      column3d->set_draw_strips( false );
      _fvtx_columns.insert( make_pair( index, column3d ) );
    }

    cout << _fvtx_columns.size() << " fvtx columns created." << endl;
  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }

  MUTOO::PRINT( cout, " ** " );
*/
}

//___________________________________
void Muon3DDisplay::_clear_list( list<PHObj3D*>& objs )
{

  // clear list of tracks
  for( list<PHObj3D*>::iterator It = objs.begin(); It != objs.end(); It++ )
  if( *It ) {
    (*It)->hide();
    SafeDelete( *It );
  }
  objs.clear();
  update();
}

//__________________________________________________
bool Muon3DDisplay::_is_l2_candidate( const TMuiRoadMapO::value_type& road )
{
  // check road is gold
  if( !road.get()->get_golden() ) return false;

  static const unsigned int min_depth( 4 );  // deep
  static const unsigned int min_n_gaps( 8 );
  static const double min_slope( 0.212557 );

  // check road depth is big enough
  if( road.get()->get_depth() < min_depth ) return false;

  // check road number of gaps is big enough
  if( _get_n_gaps( road ) < min_n_gaps ) return false;

  // check road slope is big enough
  if( _get_slope( road ) < min_slope ) return false;

  return true;

}

//__________________________________________________
double Muon3DDisplay::_get_slope( const TMuiRoadMapO::value_type& road )
{
  const TMutFitPar* fit_par( road.get()->get_const_fitpar() );
  if( !fit_par )
  {
    cout << "Muon3DDisplay::_get_slope - invalid fit parameters." << endl;
    return 0;
  }

  return sqrt( fit_par->get_dxdz()*fit_par->get_dxdz() + fit_par->get_dydz()*fit_par->get_dydz() );

}

//__________________________________________________
unsigned int Muon3DDisplay::_get_n_gaps( const TMuiRoadMapO::value_type& road )
{
  unsigned int bit( road.get()->get_gapbit() );
  unsigned int out(0);
  for( unsigned int i=0; i<10; i++ ) if( bit & (1<<i) ) out++;
  return out;
}
