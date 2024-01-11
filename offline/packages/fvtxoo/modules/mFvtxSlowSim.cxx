// $Id: mFvtxSlowSim.cxx,v 1.41 2016/03/20 23:37:13 shlim Exp $

/*!
  \file mFvtxSlowSim.cxx
  \brief Forward Silicon vertex simulation module. Generate TFvtxMCHit from pisa hits.
  \author Tatia Engelmore, H. Pereira Da Costa
  \version $Revision: 1.41 $
  \date $Date: 2016/03/20 23:37:13 $
*/

#include <FVTXGEOM.h>
#include <FvtxGeom.h>
#include <PHTFileServer.h>
#include <SvxSnglPisaHit.h>
#include <rootAnc.h> 

#include <TFvtxPisaHitMap.h>
#include <TMCPrimaryMap.h>
#include <TMutMCTrkMap.h>

#include "mFvtxSlowSim.h"
#include "mFvtxSlowSimPar.h"

using namespace std;

//__________________________________________________________________
mFvtxSlowSim::mFvtxSlowSim():
  _timer( PHTimeServer::get()->insert_new("mFvtxSlowSim") ),
  _errors_z(0),
  _errors_id(0),
  _pisa_tree(0),
  _evt(0)
{
  _total_pisa_hits_arm.assign(0);
  _accepted_pisa_hits_arm.assign(0);
  _total_pisa_hits_cage.assign(0);
  _accepted_pisa_hits_cage.assign(0);
  _total_pisa_hits_station.assign(0);
  _accepted_pisa_hits_station.assign(0);

  FVTXOO::TRACE("initializing module mFvtxSlowSim");
}

//_____________________________________________
void mFvtxSlowSim::print_summary( ostream& out )
{
  FVTXOO::PRINT( out, "mFvtxSlowSim::print_summary" );
  for( int arm=0; arm < FVTXOO::MAX_ARM; arm++ )
  {
    out << "arm=" << arm << " total=" << _total_pisa_hits_arm[arm] << " accepted=" << _accepted_pisa_hits_arm[arm] << endl;
    
    for( int station=0; station < FVTXOO::MAX_STATION; station++ )
    out << "arm= " << arm << " station= " << station
      << " total= " << _total_pisa_hits_station[ mFvtxSlowSimPar::get_acceptance_index( arm, station ) ]
      << " accepted= " << _accepted_pisa_hits_station[ mFvtxSlowSimPar::get_acceptance_index( arm, station ) ]
      << endl;
  }
    
  if( _errors_z ) out << " inconsistent z errors= " << _errors_z << endl;
  if( _errors_id ) out << " inconsistent id errors= " << _errors_id << endl;

//   // dump PISA hits geometry
//   if( !_geometry.empty() )
//   {
//     for( GeometryMap::const_iterator iter = _geometry.begin(); iter != _geometry.end(); iter++ )
//     out << "volume id: " << iter->first << " geometry: " << iter->second << endl;
//   }
  
  FVTXOO::PRINT( out, "**" );
}


//__________________________________________________________________
PHBoolean mFvtxSlowSim::event(PHCompositeNode* top_node)
{
  _timer.get()->restart();
  try {

    set_interface_ptrs(top_node);
    _pisa_hit_map->clear();
    _mc_hit_map->clear();
    simulator_loop();

  } catch(std::exception& e) {
    FVTXOO::TRACE(e.what());
    return False;
  }

  // If verbose dump the contents of trks and primary
  _timer.get()->stop();
  if(_mod_par->get_verbosity() >= FVTXOO::ALOT) {
    _pisa_hit_map->print();
    _mc_hit_map->print();
  }

  return True;
}

//__________________________________________________________________
void mFvtxSlowSim::simulator_loop()
{
  //loop over hits, pull out info from SvxPisaHit object
  if( _mod_par->get_verbosity() >= FVTXOO::SOME )
    cout << "mFvtxSlowSim::simulator_loop - nhit: " << _svx->GetnHit() << endl;

  for(int hit_id = 0; hit_id < _svx->GetnHit(); hit_id++)
  {
    
    TFvtxPisaHitMap::iterator pisa_hit_iter( _pisa_hit_map->insert_new() );
    pisa_hit_iter->get()->set_pisa_hit( _svx->GetHit(hit_id) );
    associate_mc_trk( pisa_hit_iter.current(), _svx->GetHit(hit_id)->GetMctrack() );

    // all hits with volume id <= 4 belong to barrel
    // store them in pisa_hit_map
    if( _svx->GetHit(hit_id)->GetLayer()<=4 ) continue;

    //get position
    PHPoint global( PHPoint(
      _svx->GetHit(hit_id)->GetXGlobal(),
      _svx->GetHit(hit_id)->GetYGlobal(),
      _svx->GetHit(hit_id)->GetZGlobal() ) );

    // increment total number of pisa hits
    _total_pisa_hits_arm[ (global.getZ()>0) ? FVTXOO::North:FVTXOO::South ] ++;

    // dump pisa hits
    if( _mod_par->get_verbosity() >= FVTXOO::ALOT )
    cout << "mFvtxSlowSim::simulator_loop - new pisa hit" << PisaHitWrapper( _svx, hit_id ) << endl;

    // retrieve detector ID
    // note that two methods are used, to be able to perform some geometry consistency checks.
    bool volume_valid( false );
    bool position_valid( false );
    TFvtxIndex volume_index( get_index_from_volume_id( hit_id, volume_valid ) );
    TFvtxIndex position_index;

    if (!_mod_par->get_use_volume_id() || _mod_par->get_check_consistency()) // fix
      {

        static bool once = true;

        if (once)
          {
            std::cout << "mFvtxSlowSim::simulator_loop - WARNING -"
                <<"will calculate hit index from hit position. This lead to unnecessary CPU cost."<<std::endl;

            once = false;
          }

        position_index=get_index_from_position( hit_id, position_valid );
      }

    // use the volume index to build the MC hit
    TFvtxIndex index( _mod_par->get_use_volume_id() ? volume_index:position_index );
    bool valid(  _mod_par->get_use_volume_id() ? volume_valid:position_valid );
    if( !valid )
    {
      if( _mod_par->get_verbosity() >= FVTXOO::NONE )
      cout << "mFvtxSlowSim::simulator_loop - no detector matching PISA hit "
        << PisaHitWrapper( _svx, hit_id )
        << endl;
      continue;
    }
 
    if( _mod_par->get_verbosity() >= FVTXOO::SOME )
      std::cout << "MC hit arm " << index.arm() << " cage " << index.cage() << " station " << index.station() << " sector " << index.sector() << " column " << index.column() << std::endl;

    // check consistency
    if( _mod_par->get_check_consistency() )

      {

        static bool once = true;

        if (once)
          {
            std::cout << "mFvtxSlowSim::simulator_loop - WARNING -"
                <<"Will check consistency between PISA digitalization and geometry matching. "
                <<"This would yield error messages if PISA and Reco geometry are not identical, "
                <<"e.g. when PISA geometry is deliberately mis-aligned to mimic real data misaglinment"<<std::endl;

            once = false;
          }

        check_consistency( hit_id, volume_index, position_index );
      }


    // increment total number of pisa hits
    _total_pisa_hits_station[ mFvtxSlowSimPar::get_acceptance_index( index.arm(), index.station() ) ] ++;

    // retrieve sector associated to index
    FvtxSector *sector = FvtxGeom::get_arm( index.arm() )
      ->get_cage( index.cage() )
      ->get_station( index.station() )
      ->get_sector( index.sector() );

    // try cast SvxSinglePisaHit
    SvxSnglPisaHit* svx_hit = _svx->GetHit( hit_id );

    // get global in and out points
    //PHPoint global_in( svx_hit->GetXGlobalIn(), svx_hit->GetYGlobalIn(), svx_hit->GetZGlobalIn() );
    //PHPoint global_out( svx_hit->GetXGlobalOut(), svx_hit->GetYGlobalOut(), svx_hit->GetZGlobalOut() );
    PHPoint local_in( svx_hit->GetXLocalIn(), svx_hit->GetYLocalIn(), svx_hit->GetZLocalIn() );
    PHPoint local_out( svx_hit->GetXLocalOut(), svx_hit->GetYLocalOut(), svx_hit->GetZLocalOut() );
    PHPoint local( PHPoint(
			   (_svx->GetHit(hit_id))->GetXLocalIn(),
			   (_svx->GetHit(hit_id))->GetYLocalIn(),
			   (_svx->GetHit(hit_id))->GetZLocalIn() ) );

	
    //cout << _svx->GetHit(hit_id)->GetXGlobalIn() << "  " << _svx->GetHit(hit_id)->GetYGlobalIn() << "  " << _svx->GetHit(hit_id)->GetZGlobalIn() << "  " << endl; 
    //cout << _svx->GetHit(hit_id)->GetXGlobalOut() << "  " << _svx->GetHit(hit_id)->GetYGlobalOut() << "  " << _svx->GetHit(hit_id)->GetZGlobalOut() << "  " << endl;

    // find matching strip
    /*
      loop over columns.
      if a matching strip is found, store it,
      assign column to index
      and break
    */
    bool found_column( false );
    //for(unsigned int column_id = 0; column_id < FVTXGEOM::NumberOfColumns; column_id++)
    //{
    FvtxColumn* column =  sector->get_column(index.column());
    //if(!column->contains(local)) continue;
    found_column = true;
      //index.set_column( column_id );

      // get strips and relative path length
      typedef map< unsigned int, double > StripMap;
      StripMap strip_map;

      if( _mod_par->get_do_clusters() )
      {

        /*
          look for strip indices mathing entrance and exit point of the track par
          and corresponding path length ratio; add to MC strip
        */
	
	//std::cout << "Local In/Out: ("<<local_in.getX()<<","<<local_in.getY()<<","<<local_in.getZ()<<")  ("
	//	  <<local_out.getX() <<","<<local_out.getY()<<","<<local_out.getZ() << ")" << std::endl;

				double tmp_hit_in_r = local_in.getZ() + (column->get_outer_radius() - column->get_inner_radius())/2.0;
				double tmp_hit_out_r = local_out.getZ() + (column->get_outer_radius() - column->get_inner_radius())/2.0;

				// just in case of swap
				double hit_in_r = TMath::Min(tmp_hit_in_r, tmp_hit_out_r);
				double hit_out_r = TMath::Max(tmp_hit_in_r, tmp_hit_out_r);

        list<FvtxStrip*> strips( column->find_strips( local_in, local_out ) );
        for( list<FvtxStrip*>::iterator iter = strips.begin(); iter != strips.end(); iter++ )
        {

					// calculate path length in strip
					// temporary fix (assume main path is along direction of width)
					double strip_begin_r = ((*iter)->get_strip_index())*((*iter)->get_width());
					double strip_end_r = ((*iter)->get_strip_index() + 1)*((*iter)->get_width());

					double path_length = (TMath::Min(hit_out_r,strip_end_r) - TMath::Max(hit_in_r,strip_begin_r))/(hit_out_r - hit_in_r);

					//cout << "Strip begin/end r: " << strip_begin_r << " - " << strip_end_r << ", Path length: " << path_length << endl;

          // calculate path length in strip
          //double path_length = (*iter)->get_path_length( local_in, local_out );
          strip_map.insert( make_pair( (*iter)->get_strip_index(), path_length ) );
        }

      } else {

        /*
          do not create clusters. Only add the strip corresponding to the
          point in the middle of the column
        */

        FvtxStrip* strip( column->find_strip( local ) );
        if( strip ) strip_map.insert( make_pair( strip->get_strip_index(), 1 ) );

      }


      // check that some fired strips have been found
      if( strip_map.empty() ) 
      {
        if( _mod_par->get_verbosity() >= FVTXOO::SOME )
        cout << "mFvtxSlowSim::simulator_loop - no strip matching PISA hit "
          << PisaHitWrapper( _svx, hit_id )
          << endl;
        continue;
      }


      // allocate new MC hit
      TFvtxMCHitMap::iterator mc_hit_iter( _mc_hit_map->insert_new(
          index.arm(),
          index.cage(),
          index.station(),
          index.sector(),
          index.column() ));

      // store MC hit position
      mc_hit_iter->get()->set_x( global.getX() );
      mc_hit_iter->get()->set_y( global.getY() );
      mc_hit_iter->get()->set_z( global.getZ() );

      //still need to set momenta - get that from PISA hit
      mc_hit_iter->get()->set_px( _svx->GetHit(hit_id)->GetPmomX() );
      mc_hit_iter->get()->set_py( _svx->GetHit(hit_id)->GetPmomY() );
      mc_hit_iter->get()->set_pz( _svx->GetHit(hit_id)->GetPmomZ() );
      mc_hit_iter->get()->set_tof( _svx->GetHit(hit_id)->GetTof() );

      //get energy loss
      static const float econv = 2.2e8;
      mc_hit_iter->get()->set_eloss( econv*_svx->GetHit(hit_id)->GetDele() );

      // fill track ID
      mc_hit_iter->get()->set_track_id( _svx->GetHit(hit_id)->GetMctrack() );

      // add strips to the mc_hit
      // the strip charge is calculated from the strip_map fraction 
      // and the total energy loss
      for( StripMap::iterator iter = strip_map.begin(); iter != strip_map.end(); iter++ )
      mc_hit_iter->get()->add_strip( iter->first, iter->second*mc_hit_iter->get()->get_eloss() );

      // associate to MC trk
      associate_mc_trk( mc_hit_iter.current() );

      // increment counter
      _accepted_pisa_hits_arm[ index.arm() ]++;
      _accepted_pisa_hits_station[ mFvtxSlowSimPar::get_acceptance_index( index.arm(), index.station() ) ]++;

      //}

    if( !found_column )// && _mod_par->get_verbosity() >= FVTXOO::SOME )
    cout << "mFvtxSlowSim::simulator_loop - no column matching PISA hit "
      << PisaHitWrapper( _svx, hit_id )
      << endl;

    // fill evaluation tree
    if( _mod_par->get_do_evaluation() ) fill_evaluation_tree( hit_id, index, true );

  }

   TFvtxMCHitMap::iterator mc_hit_iter = _mc_hit_map->range();
   while (TFvtxMCHitMap::pointer mc_hit_ptr = mc_hit_iter.next())
   {
     associate_child_mc_trk( mc_hit_ptr );
   }

}

//__________________________________________________________________
void mFvtxSlowSim::set_interface_ptrs(PHCompositeNode* top_node)
{

  // parameter module
  _mod_par = TMutNode<mFvtxSlowSimPar>::find_node(top_node,"mFvtxSlowSimPar");

  // object maps
  _pisa_hit_map = TMutNode<TFvtxPisaHitMap>::find_node( top_node, "TFvtxPisaHitMap" );
  _mc_hit_map = TMutNode<TFvtxMCHitMap>::find_node(top_node, "TFvtxMCHitMap");
  _mc_trk_map = TMutNode<TMutMCTrkMap>::find_node(top_node, "TMutMCTrkMap");

  // pisa node
  _svx = TMutNode<SvxPisaHit>::find_io_node(top_node,"SvxPisaHit");

}

//___________________________________________________
TFvtxIndex mFvtxSlowSim::get_index_from_position( const int& hit_id, bool& valid ) const
{

  valid = false;

  //get position
  double x = _svx->GetHit(hit_id)->GetXGlobal();
  double y = _svx->GetHit(hit_id)->GetYGlobal();
  double z = _svx->GetHit(hit_id)->GetZGlobal();
  //std::cout << x  << " " << y << " " << z << std::endl;
  
  // running index
  TFvtxIndex index;

  //set arm by sign of z
  index.set_arm( (z < 0) ? FVTXOO::South : FVTXOO::North );

  //get station by checking which one the z is closest to
  //this only works for the north arm.
  bool found_station( false );
  for(unsigned int cage_id = 0; cage_id < FVTXGEOM::NumberOfCages; cage_id++)
  {
    for(unsigned int station_id = 0; station_id < FVTXGEOM::NumberOfStations; station_id++) 
    {
      double station_z = FvtxGeom::get_arm(index.arm())->get_cage(cage_id)->get_station(station_id)->get_z();
      if( fabs( station_z - z ) > 0.10*fabs(station_z) ) continue;
      index.set_cage( cage_id );
      index.set_station( station_id );
      found_station = true;
    }
  }

  if( !found_station ) return index;

  // retrieve station object, and do similar to in mFvtxSlowSim
  FvtxStation* station_ptr = FvtxGeom::get_arm(index.arm())->get_cage(index.cage())->get_station(index.station());

  //loop over sector/plane/radius/column and see which columns contain the PISA point - if it contains it,
  double dz_min = -1;
  list<FvtxSector*> sectors( station_ptr->find_sectors(PHPoint(x, y, z)) );
  for(list<FvtxSector*>::const_iterator sector_itr = sectors.begin(); sector_itr != sectors.end(); sector_itr++)
  for(unsigned int column_id = 0; column_id < FVTXGEOM::NumberOfColumns; column_id++)
  {
    FvtxColumn* column = (*sector_itr)->get_column(column_id);
    if(!column->contains(PHPoint(x, y, z))) continue;

    // check the z difference. keep the closest
    double dz = fabs( column->get_z() - z );
    if( dz_min > 0 && dz > dz_min ) continue;
    dz_min = dz;
    valid = true;

    // assign sector plane and radius
    index.set_sector( (*sector_itr)->index().sector() );
  }

  return index;

}

//___________________________________________________
TFvtxIndex mFvtxSlowSim::get_index_from_volume_id( const int& hit_id, bool& valid ) const
{

  //std::cout << "In get_index_from_volume_id " << std::endl;
  
  // running index
  TFvtxIndex index;
  valid = false;

  FvtxGeom::get_arm(0); // to create Fvtx Geom
  //cout << _svx->GetHit(hit_id)->GetLayer() << " " << _svx->GetHit(hit_id)->GetHitVolume(4) << " " << _svx->GetHit(hit_id)->GetHitVolume(5) << endl;
  // get arm from layer
  /*
    south arm correspond to layers fro 5 to 8
    north arm correspond to layers from 9 to 12
  */
  int layer = _svx->GetHit(hit_id)->GetLayer();
  if (layer <= 4) 
  { 
    valid = false;
    return index;
  }

  if( _mod_par->get_verbosity() >= FVTXOO::SOME )
    std::cout << "TFvtxIndex mFvtxSlowSim::get_index_from_volume_id() layer = " << layer << std::endl;

  index.set_arm( (layer < 13 ? FVTXOO::South : FVTXOO::North ) );
  index.set_cage( layer % 2 == 0 ? 1 : 0 );

  // get station from layer
  /*
    for south arm, station = (12 - layer)/2
    for north arm, station = (layer - 13)/2
  */
  index.set_station( index.arm() == FVTXOO::South ? (12-layer)/2 : (layer-13)/2 );

  // get sector
  // numbering starts from 1 in pisa, and 0 in offline.
  int sector_id = _svx->GetHit(hit_id)->GetHitVolume(6);
  //These two lines need to be removed to be consistent with new FvtxGeom.cxx (9/26/12)
  //if ( index.arm() == 0 ) sector_id = 24-sector_id;
  //else sector_id --;
  sector_id --;

  index.set_sector( sector_id );

  int column_id = 0;
  column_id = _svx->GetHit(hit_id)->GetHitVolume(8)-1;
  if ( index.station() == 0 ) column_id -= 2;
  index.set_column( column_id );

  if( _mod_par->get_verbosity() >= FVTXOO::SOME )
  {
    cout << "volume id ";
    for (int i = 0; i < 9; i++) cout << _svx->GetHit(hit_id)->GetHitVolume(i) << " ";
    cout << endl;
    cout << "arm " << index.arm() << " cage " << index.cage() << " station " << index.station() << " sector " << index.sector() << " column " << index.column() << endl;
  }

  valid = index.check();
  return index;

}

//______________________________________________________________________
void mFvtxSlowSim::associate_mc_trk(TFvtxMCHitMap::pointer mc_hit_ptr)
{
  // Get an iterator to all TFvtxMCTrk in map
  TMutMCTrkMap::iterator mc_trk_iter = _mc_trk_map->range();
  while(TMutMCTrkMap::pointer mc_trk_ptr = mc_trk_iter.next())
  {
    if(mc_trk_ptr->get()->get_track_id() == mc_hit_ptr->get()->get_track_id()) 
    {

      // Track ids match so we make association and return
      PHKey::associate(mc_hit_ptr, mc_trk_ptr);
      return;

    }
  }

  // No matching track id was found so we make an new TMutMCTrk
  fill_new_mc_trk(mc_hit_ptr);
}
//______________________________________________________________________
void mFvtxSlowSim::associate_mc_trk(TFvtxPisaHitMap::pointer pisa_hit_ptr, int track_id)
{
  // Get an iterator to all TFvtxMCTrk in map
  TMutMCTrkMap::iterator mc_trk_iter = _mc_trk_map->range();
  while(TMutMCTrkMap::pointer mc_trk_ptr = mc_trk_iter.next())
  {
    if(mc_trk_ptr->get()->get_track_id() == track_id) 
    {

      // Track ids match so we make association and return
      PHKey::associate(pisa_hit_ptr, mc_trk_ptr);
      return;

    }
  }

}

//______________________________________________________________________
void mFvtxSlowSim::associate_child_mc_trk(TFvtxMCHitMap::pointer mc_hit_ptr)
{
  // Get an iterator to all TFvtxMCTrk in map
  TMutMCTrkMap::iterator mc_trk_iter = _mc_trk_map->range();

  // some verbosity
  if( _mod_par->get_verbosity() >= FVTXOO::SOME ) 
    { cout << "for a mc_hit, track_id " << mc_hit_ptr->get()->get_track_id() << " n_mc_trk " << _mc_trk_map->count() << endl; }

  while(TMutMCTrkMap::pointer mc_trk_ptr = mc_trk_iter.next())
  {
    TMutMCTrkMap::pointer mc_trk_ptr2 = mc_trk_ptr;
    float ptot=0;
    float ptheta=0;
    float pphi=0;
    float r_vertex=0;
    float z_vertex=0;
    float theta_vertex=0;
    float phi_vertex=0;

    int nfile=0;
    int error=0;
    int itparent=0;
    int idparent=0;
    int idpart=0;

    int track_id = mc_trk_ptr2->get()->get_parent_track_id();
    //cout << "parent_track_id " << track_id << endl;
    while ( track_id != mc_hit_ptr->get()->get_track_id() &&
            track_id > 0 )
    {
      // Make the PISA call to get track data
      dio_ptrkstack(&track_id, &nfile, &error, &ptot, &ptheta, &pphi,
      &r_vertex, &z_vertex, &theta_vertex, &phi_vertex,
      &itparent, &idparent, &idpart);
 
      track_id = itparent;
      //cout << "In iter, parent_track_id " << track_id << endl;
    }

    //if ( track_id <= 0) continue; // This FvtxMCHit is not ancestor of this MutMCTrk;
    if ( track_id == mc_hit_ptr->get()->get_track_id() ) {
      // Parent track ids match so we make association
      PHKey::associate(mc_hit_ptr, mc_trk_ptr);
      //cout << "associate " << mc_hit_ptr->get()->get_track_id() << " and " << mc_trk_ptr->get()->get_track_id() << endl;
    }
  }
}
//______________________________________________________________________
void mFvtxSlowSim::fill_new_mc_trk(TFvtxMCHitMap::pointer mc_hit_ptr, int trackID)
{

  int track_id = trackID;
  if (trackID == 0) track_id = mc_hit_ptr->get()->get_track_id();

  // Insert an new TMutMCTrk into map
  TMutMCTrkMap::iterator mc_trk_iter = _mc_trk_map->insert_new(mc_hit_ptr->get()->get_arm());
  mc_trk_iter->get()->set_arm(mc_hit_ptr->get()->get_arm());
  mc_trk_iter->get()->from_pisa( track_id );

  // Do the association
  if (trackID == 0) PHKey::associate(mc_hit_ptr, mc_trk_iter.current());

  //Add a TMutMCTrk for the parent
  int itparent( mc_trk_iter->get()->get_parent_track_id() );
  int idparent( mc_trk_iter->get()->get_parent_id() );
  if (itparent > 0 && idparent != 0)
  {
      //Check to see if parent already has a track
      bool parent_trk_filled = false;
      TMutMCTrkMap::iterator mc_trk_iter2 = _mc_trk_map->range();
      while(TMutMCTrkMap::pointer mc_trk_ptr = mc_trk_iter2.next()){
      if(mc_trk_ptr->get()->get_track_id() == abs(itparent)) {
        parent_trk_filled = true;
        break;
      }
    }
    if (!parent_trk_filled) fill_new_mc_trk(mc_hit_ptr, abs(itparent));
  }
  
}

//____________________________________________________________
bool mFvtxSlowSim::check_consistency( const int& pisa_hit_id, TFvtxIndex volume_index, TFvtxIndex position_index )
{

 
  // check volume_index vs position_index
  bool out( true );
  if( !volume_index.check() ) 
  {
    cout << "mFvtxSlowSim::check_consistency - invalid volume_index " << volume_index << endl;
    out = false;
    _errors_id ++;
  }

  if( !volume_index.check() ) 
  {
    cout << "mFvtxSlowSim::check_consistency - invalid volume_index " << volume_index << endl;
    out = false;
    _errors_id ++;
  }

  if( ! (volume_index == position_index ) ) 
  {
    cout << "mFvtxSlowSim::check_consistency - inconsistent indexes - volume: " << volume_index << " position: " << position_index;
    
    double phi( atan2( (double)_svx->GetHit( pisa_hit_id )->GetYGlobal(), (double) _svx->GetHit( pisa_hit_id )->GetXGlobal() ) ); 
    cout << " phi: " << phi;

    {
      FvtxSector* sector(
        FvtxGeom::get_arm( volume_index.arm() )
        ->get_cage( volume_index.cage() )
        ->get_station( volume_index.station() )
        ->get_sector( volume_index.sector() ) );
      double phi_begin = sector->get_phi_begin();
      double phi_end = sector->get_phi_end();
      cout << " volume phi: (" << phi_begin << "," << phi_end << ")";
    }   
    
    {
      FvtxSector* sector(
        FvtxGeom::get_arm( position_index.arm() )
        ->get_cage( volume_index.cage() )
        ->get_station( position_index.station() )
        ->get_sector( position_index.sector() ) );
      double phi_begin = sector->get_phi_begin();
      double phi_end = sector->get_phi_end();
      cout << " position phi: (" << phi_begin << "," << phi_end << ")" << endl;
    }
        
    out = false;
    _errors_id ++;
  }

  // store volume geometry
  double z( _svx->GetHit( pisa_hit_id )->GetZGlobal() );
  double r( sqrt( FVTXOO::SQUARE( _svx->GetHit( pisa_hit_id )->GetXGlobal() ) + FVTXOO::SQUARE( _svx->GetHit( pisa_hit_id )->GetYGlobal() ) ) );
  double phi( atan2( (double)_svx->GetHit( pisa_hit_id )->GetYGlobal(), (double) _svx->GetHit( pisa_hit_id )->GetXGlobal() ) );
  GeometryMap::iterator iter = _geometry.find( volume_index );
  if( iter == _geometry.end() )
  {
    
    // insert new pair
    VolumeGeometry geom;
    geom._z = z;
    geom._phi_min = geom._phi_max = phi;
    geom._r_min = geom._r_max = r;
    _geometry.insert( make_pair( volume_index, geom ) );
    
  } else {
    
    // check z
    if( fabs(iter->second._z - z) > 0.001 ) 
      cout 
        << "mFvtxSlowSim::check_consistency - inconsistent z (geometry) -"
        << " volume: " << volume_index 
        << " stored: " << iter->second._z
        << " new: " << z << endl;
    
    // update r and phi
    if( phi > iter->second._phi_max ) iter->second._phi_max = phi;
    if( phi < iter->second._phi_min ) iter->second._phi_min = phi;
    if( r > iter->second._r_max ) iter->second._r_max = r;
    if( r < iter->second._r_min ) iter->second._r_min = r;
  }

  // check z
  if( volume_index.check() )
  {
    FvtxSector* sector(
        FvtxGeom::get_arm( volume_index.arm() )
        ->get_cage( volume_index.cage() )
        ->get_station( volume_index.station() )
        ->get_sector( volume_index.sector() ) );
    double relative_distance( fabs( ( sector->get_z() - _svx->GetHit( pisa_hit_id )->GetZGlobal() )/sector->get_z() ) );
    if( relative_distance > 0.0001 ) {
      cout << "mFvtxSlowSim::check_consistency - inconsistent z -"
          << " volume id: " << volume_index << " z volume: " << sector->get_z()
          << " pisa hit: " << _svx->GetHit( pisa_hit_id )->GetZGlobal() << endl;
      out = false;
      _errors_z ++;
    }
  }

  return out;
}

//____________________________________________________________
void mFvtxSlowSim::book_evaluation_tree( void )
{

  FVTXOO::PRINT( cout, "FvtxUnpackPisa::book_evaluation_tree" );
  _evt = 0;

  // create TFile
  PHTFileServer::get().open( _mod_par->get_evaluation_file(), "RECREATE" );
  cout << "writing to file \"" << _mod_par->get_evaluation_file() << "\"" << endl;

  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  _pisa_tree = new TTree( "pisa_eval", "pisa_eval" );
  _pisa_tree->Branch( "evt", &_evt, "evt/I", BUFFER_SIZE );
  _pisa_tree->Branch( "arm;   ", &_arm, "arm/I", BUFFER_SIZE );
  _pisa_tree->Branch( "cage", &_cage, "cage/I", BUFFER_SIZE );
  _pisa_tree->Branch( "station", &_station, "station/I", BUFFER_SIZE );
  _pisa_tree->Branch( "sector;", &_sector, "sector/I", BUFFER_SIZE );
  _pisa_tree->Branch( "column;", &_column, "column/I", BUFFER_SIZE );

  _pisa_tree->Branch( "pisa_x", &_pisa_x, "pisa_x/D", BUFFER_SIZE );
  _pisa_tree->Branch( "pisa_y", &_pisa_y, "pisa_y/D", BUFFER_SIZE );
  _pisa_tree->Branch( "pisa_z", &_pisa_z, "pisa_z/D", BUFFER_SIZE );
  _pisa_tree->Branch( "pisa_r", &_pisa_r, "pisa_r/D", BUFFER_SIZE );
  _pisa_tree->Branch( "pisa_phi", &_pisa_phi, "pisa_phi/D", BUFFER_SIZE );

  _pisa_tree->Branch( "eloss", &_eloss, "eloss/D", BUFFER_SIZE );
  _pisa_tree->Branch( "path_length", &_path_length, "path_length/D", BUFFER_SIZE );

  _pisa_tree->Branch( "pisa_x_in", &_pisa_x_in, "pisa_x_in/D", BUFFER_SIZE );
  _pisa_tree->Branch( "pisa_y_in", &_pisa_y_in, "pisa_y_in/D", BUFFER_SIZE );
  _pisa_tree->Branch( "pisa_z_in", &_pisa_z_in, "pisa_z_in/D", BUFFER_SIZE );

  _pisa_tree->Branch( "pisa_x_out", &_pisa_x_out, "pisa_x_out/D", BUFFER_SIZE );
  _pisa_tree->Branch( "pisa_y_out", &_pisa_y_out, "pisa_y_out/D", BUFFER_SIZE );
  _pisa_tree->Branch( "pisa_z_out", &_pisa_z_out, "pisa_z_out/D", BUFFER_SIZE );

  _pisa_tree->Branch( "pisa_x_local_in", &_pisa_x_local_in, "pisa_x_local_in/D", BUFFER_SIZE );
  _pisa_tree->Branch( "pisa_y_local_in", &_pisa_y_local_in, "pisa_y_local_in/D", BUFFER_SIZE );
  _pisa_tree->Branch( "pisa_z_local_in", &_pisa_z_local_in, "pisa_z_local_in/D", BUFFER_SIZE );

  _pisa_tree->Branch( "pisa_x_local_out", &_pisa_x_local_out, "pisa_x_local_out/D", BUFFER_SIZE );
  _pisa_tree->Branch( "pisa_y_local_out", &_pisa_y_local_out, "pisa_y_local_out/D", BUFFER_SIZE );
  _pisa_tree->Branch( "pisa_z_local_out", &_pisa_z_local_out, "pisa_z_local_out/D", BUFFER_SIZE );

  _pisa_tree->Branch( "volume_id", &_volume_id[0], "volume_id[9]/I", BUFFER_SIZE );

  _pisa_tree->Branch( "radius_z", &_radius_z, "radius_z/D", BUFFER_SIZE );
  _pisa_tree->Branch( "radius_inner_radius", &_radius_inner_radius, "radius_inner_radius/D", BUFFER_SIZE );
  _pisa_tree->Branch( "radius_outer_radius", &_radius_outer_radius, "radius_outer_radius/D", BUFFER_SIZE );
  _pisa_tree->Branch( "radius_phi_begin", &_radius_phi_begin, "radius_phi_begin/D", BUFFER_SIZE );
  _pisa_tree->Branch( "radius_phi_end", &_radius_phi_end, "radius_phi_end/D", BUFFER_SIZE );
  _pisa_tree->Branch( "radius_phi", &_radius_phi, "radius_phi/D", BUFFER_SIZE );

  _pisa_tree->Branch( "layer", &_layer, "layer/I", BUFFER_SIZE );
  _pisa_tree->Branch( "valid", &_valid, "valid/I", BUFFER_SIZE );
  FVTXOO::PRINT( cout, "**" );
}

//___________________________________________________________
void mFvtxSlowSim::fill_evaluation_tree( const int& pisa_hit_id, const TFvtxIndex& index, const bool& valid )
{

  if( !_pisa_tree ) book_evaluation_tree();

  // get mc_hit indexes
  _arm= index.arm();
  _cage= index.cage();
  _station= index.station();
  _sector= index.sector();
  _column= index.column();
  _valid = int(valid);

  // get pisa hit position
  _pisa_x = _svx->GetHit(pisa_hit_id)->GetXGlobal();
  _pisa_y = _svx->GetHit(pisa_hit_id)->GetYGlobal();
  _pisa_z = _svx->GetHit(pisa_hit_id)->GetZGlobal();
  _pisa_r = sqrt( FVTXOO::SQUARE( _pisa_x ) + FVTXOO::SQUARE( _pisa_y ) );
  _pisa_phi = atan2( _pisa_y, _pisa_x );

  // retrieve radius object
  FvtxSector *sector = FvtxGeom::get_arm( _arm )
    ->get_cage( _cage )
    ->get_station( _station )
    ->get_sector( _sector );

  // radius geometry
  _radius_z = sector->get_z();
  _radius_inner_radius = sector->get_inner_radius();
  _radius_outer_radius = sector->get_outer_radius();
  _radius_phi_begin = sector->get_phi_begin();
  _radius_phi_end = sector->get_phi_end();


  // radius mid phi
  if( _radius_phi_end < _radius_phi_begin ) _radius_phi_end+=2*M_PI;
  _radius_phi = ( _radius_phi_end+_radius_phi_begin )/2;

  // pisa local in position
  _pisa_x_local_in = _svx->GetHit(pisa_hit_id)->GetXLocalIn();
  _pisa_y_local_in = _svx->GetHit(pisa_hit_id)->GetYLocalIn();
  _pisa_z_local_in = _svx->GetHit(pisa_hit_id)->GetZLocalIn();

  // pisa local out position
  _pisa_x_local_out = _svx->GetHit(pisa_hit_id)->GetXLocalOut();
  _pisa_y_local_out = _svx->GetHit(pisa_hit_id)->GetYLocalOut();
  _pisa_z_local_out = _svx->GetHit(pisa_hit_id)->GetZLocalOut();

  _pisa_x_in = _svx->GetHit(pisa_hit_id)->GetXGlobalIn();
  _pisa_y_in = _svx->GetHit(pisa_hit_id)->GetYGlobalIn();
  _pisa_z_in = _svx->GetHit(pisa_hit_id)->GetZGlobalIn();
  PHPoint global_in( _pisa_x_in, _pisa_y_in, _pisa_z_in );

  _pisa_x_out = _svx->GetHit(pisa_hit_id)->GetXGlobalOut();
  _pisa_y_out = _svx->GetHit(pisa_hit_id)->GetYGlobalOut();
  _pisa_z_out = _svx->GetHit(pisa_hit_id)->GetZGlobalOut();
  PHPoint global_out( _pisa_x_out, _pisa_y_out, _pisa_z_out );

  // energy loss and path_length
  static const float econv = 882e4;
  _eloss = econv*_svx->GetHit(pisa_hit_id)->GetDele();
  _path_length = global_out.distanceToPoint( global_in );

  // volume ids
  for( int i=0; i<9; i++ )
    _volume_id[i] = _svx->GetHit(pisa_hit_id)->GetHitVolume(i);
  _layer = _svx->GetHit(pisa_hit_id)->GetLayer();

  _pisa_tree->Fill();

  // increment event
  _evt++;

}
