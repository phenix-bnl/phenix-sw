/*!
	\file mRpcSlowSim.cxx
	\brief rpc simulation module. Generate TRpcMCHit from pisa hits.
	\author H. Pereira Da Costa
	\version $Revision: 1.12 $
	\date $Date: 2011/11/30 17:37:50 $
*/

#include "mRpcSlowSim.h"
#include "RPCOO.h"
#include "RpcGeom.h"
#include "RpcGeom_proto.h"
#include "RpcGeom_v1.h"
#include "RpcGeom_v2.h"
#include "recoConsts.h"
#include "RPCPROTOGEOM.h"
#include "RPCFULLGEOM.h"
#include "RPCFINALGEOM.h"
#include "TRandom.h"

#include <rootAnc.h>
using namespace std;

//_____________________________________________
mRpcSlowSim::mRpcSlowSim() : 
	_timer( PHTimeServer::get()->insert_new("mRpcSlowSim") )
{
	_total_mc_hits.assign(0);
	_accepted_mc_hits.assign(0);
	fNumNoiseHits = 0;

	RPCOO::TRACE("initializing module mRpcSlowSim");
}

//_____________________________________________
PHBoolean mRpcSlowSim::event(PHCompositeNode* top_node)
{
	recoConsts *myrc = recoConsts::instance();

	_timer.get()->restart();	 
	try { 

		set_interface_ptrs(top_node);
		_mc_hit_map->clear();
		
		// Loop over PISA hits and do the digitization
		simulator_loop( RPCOO::Station1, _mupc1ghit);
		simulator_loop( RPCOO::Station2, _mupc2ghit);
	 	simulator_loop( RPCOO::Station3, _mupc3ghit);
		
		// Add noise into the detector
		if(myrc->get_IntFlag("AddRpcSimulationNoise")==1) { noise_loop(); }

	} catch(std::exception& e) {
		RPCOO::TRACE(e.what());
		return False;
	}	

	// If verbose dump the contents of trks and primary
	_timer.get()->stop();
	if( _mod_par->get_verbosity() >= RPCOO::ALOT) {
		_mc_hit_map->print();
		_mc_trk_map->print();
	}
		
	return True;
}

//_____________________________________________
void mRpcSlowSim::print_summary( ostream& out ) 
{ 
	RPCOO::PRINT( out, "mRpcSlowSim::print_summary" );
	for( int arm=0; arm < RPCOO::NumberOfArms; arm++ )
		for( int station=0; station < RPCOO::NumberOfStations; station++ )
			out << "arm=" << arm << " station=" << station 
					<< " total_mc_hits =    " << _total_mc_hits[ mRpcSlowSimPar::get_acceptance_index( arm, station ) ]
					<< " accepted_mc_hits = " << _accepted_mc_hits[ mRpcSlowSimPar::get_acceptance_index( arm, station ) ]
					<< endl;
	out << "Total noise_hits written = " << fNumNoiseHits << endl;

	RPCOO::PRINT( out, "**" );
}
	
//______________________________________________________________________
void mRpcSlowSim::simulator_loop( unsigned int station, mupcghitWrapper* mupcghit )
{
	recoConsts *myrc = recoConsts::instance();
	
	// get counts
	int n_hits( mupcghit->RowCount() );
       	if( _mod_par->get_verbosity() >= RPCOO::ALOT )
	  cout << "mRpcSlowSim::simulator_loop - station = " << station << " n_pisa_hits=" << n_hits << endl;
	
	// loop over pisa hits
	for( int i_hit = 0; i_hit < n_hits; i_hit++ )
	{
		
		// retrieve arm 		
		// retrieve position
		PHPoint position( 
				mupcghit->get_xyzinglo( 0, i_hit ),
				mupcghit->get_xyzinglo( 1, i_hit ),
				mupcghit->get_xyzinglo( 2, i_hit ) );
		
		/* 
			Note: _very_ unfortunately there is an inversion between PISA and offline
			naming convention of the arms. For pisa, North is 0, South is 1. For offline
			south is 0 and north is 1. We go to the offline convention here, once for all
		*/
		int arm = (mupcghit->get_arm(i_hit)) ? RPCOO::South : RPCOO::North;
			
		// increment hit counting
		_total_mc_hits[ mRpcSlowSimPar::get_acceptance_index( arm, station ) ]++;
	
		try {
			
			// compare z position (for consistancy)
			if( _mod_par->get_verbosity() >= RPCOO::SOME )
			{
				cout 
						<< "mRpcSlowSim::simulator_loop -"
						<< " pisa_hit_z = " << position.getZ();
				if(myrc->get_IntFlag("RpcGeomType")==0) {
				           cout << " RPC station z = " << RpcGeom::get_arm( arm )->get_station( station )->get_z() << endl;
					   cout << "WARNING THIS IS PROBABLY THE WRONG RPC GEOM: Set recoConsts->set_IntFlag(\"RpcGeomType\",) appropriately" << endl; }
				else if(myrc->get_IntFlag("RpcGeomType")==1) {//Prototype geometry
				           cout << " RPC station z = " << RpcGeom_proto::get_arm( arm )->get_station( station )->get_z() << endl; }
				else if(myrc->get_IntFlag("RpcGeomType")==2) {//Full geometry
				  cout << " RPC station z = " << RpcGeom_v1::get_arm( arm )->get_station( station )->get_z() << endl; }
				else if(myrc->get_IntFlag("RpcGeomType")==3) {//Final geometry
				  cout << " RPC station z = " << RpcGeom_v2::get_arm( arm )->get_station( station )->get_z() << endl; }
			}
			
			RpcStrip *temp[3] = { NULL, NULL, NULL };

			if(myrc->get_IntFlag("RpcGeomType")==0) {//default
			  // try get hit strip
			  int strip_index = RpcGeom::get_arm(arm)->get_station(station)->get_strip_index( position.getX(), position.getY() );
			  temp[0] =  RpcGeom::get_arm(arm)->get_station(station)->get_strip(strip_index); }
			else if(myrc->get_IntFlag("RpcGeomType")==1) { //Proto-type
			  //cout << " using the proto geometry ..." << endl;
			  temp[0] = new RpcStrip_proto();
			  temp[0]->SetStrip(position.getX(), position.getY(), arm, station, 0);//Search through the first radial segment first
			  temp[1] = new RpcStrip_proto();
			  temp[1]->SetStrip(position.getX(), position.getY(), arm, station, 1);//Search through the second radial segment first
			  temp[2] = new RpcStrip_proto();			
			  temp[2]->SetStrip(position.getX(), position.getY(), arm, station, 2); }
			else if(myrc->get_IntFlag("RpcGeomType")==2) { //Full Geometry
			  //cout << " using the full geometry ..." << endl;
			  temp[0] = new RpcStrip_v1();
			  temp[0]->SetStrip(position.getX(), position.getY(), arm, station, 0);//Search through the first radial segment first
			  temp[1] = new RpcStrip_v1();
			  temp[1]->SetStrip(position.getX(), position.getY(), arm, station, 1);//Search through the second radial segment first
			  temp[2] = new RpcStrip_v1();			
			  temp[2]->SetStrip(position.getX(), position.getY(), arm, station, 2); }
			else if(myrc->get_IntFlag("RpcGeomType")==3) { //Final Geometry
			  //cout << " using the full geometry ..." << endl;
			  temp[0] = new RpcStrip_v2();
			  temp[0]->SetStrip(position.getX(), position.getY(), arm, station, 0);//Search through the first radial segment first
			  temp[1] = new RpcStrip_v2();
			  temp[1]->SetStrip(position.getX(), position.getY(), arm, station, 1);//Search through the second radial segment first
			  temp[2] = new RpcStrip_v2();			
			  temp[2]->SetStrip(position.getX(), position.getY(), arm, station, 2); }

			if(_mod_par->get_verbosity() >= RPCOO::ALOT) {
			  cout << "Digitizing Hit Position at: x = " << position.getX() << "\ty = " << position.getY();
			  temp[0]->print(std::cout);
			}

			for(Int_t iRpcRadSeg=0 ; iRpcRadSeg<3 ; iRpcRadSeg++) {
			  //For the old setup, check that the strip pointer is empty
			  if(temp[iRpcRadSeg]->IsEmpty() && myrc->get_IntFlag("RpcGeomType")==0){
			    if( _mod_par->get_verbosity() >= RPCOO::ALOT ) {
			      cout << "mRpcSlowSim::simulator_loop() - No Strip was Hit. Station="
				   << station << " x=" << position.getX() 
				   << " y=" << position.getY() << " z=" << position.getZ() << endl; }
			    continue;}
			  
			  
			  //For the proto (and beyond) check that all the strip pointers are empty
			  if(temp[iRpcRadSeg]->IsEmpty() && myrc->get_IntFlag("RpcGeomType")!=0){
			    if( _mod_par->get_verbosity() >= RPCOO::ALOT ) {
			      cout << "mRpcSlowSim::simulator_loop() - No Strips were Hit in rad segment " << iRpcRadSeg << ". Station="
				   << station << " x=" << position.getX() 
				   << " y=" << position.getY() << " z=" << position.getZ() << endl; }
			    continue;}
			  else if(!temp[iRpcRadSeg]->IsEmpty() && myrc->get_IntFlag("RpcGeomType")!=0) {
			    if( _mod_par->get_verbosity() >= RPCOO::SOME ) {
			      cout << "Hit! " << position.getX() << " " << position.getY() << " " << position.getZ() << endl; } }
			  
			  //- If any strip pointers are filled, then write a new TRpcMCHit into the hit map
			  Int_t octant = temp[iRpcRadSeg]->GetOctant();
			  Int_t halfoctant = temp[iRpcRadSeg]->GetHalfOctant();
			  Int_t rseg = temp[iRpcRadSeg]->GetRSeg();
			  Int_t stripid = temp[iRpcRadSeg]->GetStripId();

			  Bool_t kIsNeighbor = kFALSE;
			  Float_t fNeighbor = fabs(gRandom->Gaus(0,1.0));//1.0 should be representative of the shower width and strip width
			  for(Int_t iNeighbors=0 ; iNeighbors<3 ; iNeighbors++) {//should be 3
			    //iNeighbors==0 -> Actual hit strip
			    //iNeighbors==1 -> Nearest neighbor hit strip
			    //iNeighbors==2 -> if(1) Next-to nearest neighbor hit strip
			    
			    if(fNeighbor<1.0 && iNeighbors==1) { break; }
			    if(fNeighbor<2.0 && iNeighbors==2) { break; }
			    Int_t thisstripid = stripid;
			    if(iNeighbors==1) { thisstripid += (fNeighbor>0) ? 1:-1; }
			    else if(iNeighbors==2) { thisstripid += (fNeighbor>0) ? -1:1; }//SHOULD BE -1:1

			    if(myrc->get_IntFlag("RpcGeomType")==0 && iNeighbors>0) { continue; } //Not in original simulations
			    else if(myrc->get_IntFlag("RpcGeomType")==1) { //proto
			      if(iNeighbors>0) {
				RPCPROTOGEOM proto(RPCPROTOGEOM::ArmNumber(arm),RPCPROTOGEOM::StationNumber(station),octant,halfoctant,RPCPROTOGEOM::RadialSegment(rseg),thisstripid);
				if(proto.checkStrip()) { kIsNeighbor = kTRUE; } } }
			    else if(myrc->get_IntFlag("RpcGeomType")==2) { //full geometry
			      if(iNeighbors>0) {
				RPCFULLGEOM fullgeom(RPCFULLGEOM::ArmNumber(arm),RPCFULLGEOM::StationNumber(station),octant,halfoctant,RPCFULLGEOM::RadialSegment(rseg),thisstripid);
				if(fullgeom.checkStrip()) { kIsNeighbor = kTRUE; } } }
			    else if(myrc->get_IntFlag("RpcGeomType")==3) { //final geometry
			      if(iNeighbors>0) {
				RPCFINALGEOM finalgeom(RPCFINALGEOM::ArmNumber(arm),RPCFINALGEOM::StationNumber(station),octant,halfoctant,RPCFINALGEOM::RadialSegment(rseg),thisstripid);
				if(finalgeom.checkStrip()) { kIsNeighbor = kTRUE; } } }
			    
			    if(iNeighbors>0 && !kIsNeighbor) { break; }
			    if( _mod_par->get_verbosity() >= RPCOO::SOME ) {
			      cout << "Inserting hit" << endl; }
			    
			  // insert new MC Hit
			  TRpcMCHitMap::iterator mc_hit_iter = _mc_hit_map->insert_new( arm, station, octant, halfoctant, rseg );
			  
			  // set position
			  mc_hit_iter->get()->set_coord( position );
			  
			  /* 
			     set momentum.
			     It is pretty misleading for now because track momentum at hit is not implemented for RPCs yet.
			     We use the track 'origin' momentum instead. To be fixed.
			     
			     Hugo - 2005/09/06
			  */
			  int trk_id = mupcghit->get_mctrack(i_hit);
			  mc_hit_iter->get()->set_track_id( trk_id );
			  mc_hit_iter->get()->set_momentum( get_pisa_trk_momentum( trk_id ) );
			  
			  // retrieve file key
			  int evt_trk( 0 );
			  int file_key( 0 );
			  dio_TrueTrackToEtrack(&trk_id, &evt_trk, &file_key);
			  mc_hit_iter->get()->set_file_key( file_key );
			  
			  // set energy loss and time of flight
			  mc_hit_iter->get()->set_eloss( mupcghit->get_dedx( i_hit ) );
			  mc_hit_iter->get()->set_tof( mupcghit->get_tof( i_hit ) ); 
			  
			  // add MC strip and associate to MC tracks
			  mc_hit_iter->get()->add_strip( thisstripid, mupcghit->get_dedx( i_hit ) );
			  associate_mc_trk(mc_hit_iter.current());
			  
			  // increment found MC hits
			  _accepted_mc_hits[ mRpcSlowSimPar::get_acceptance_index( arm, station ) ]++;
			  }//Neighbors
			  
			}//Radial segments
			for(Int_t i=0 ; i<3 ; i++) { if(temp[i]) { delete temp[i]; } }
			//delete [] temp;
			
		} catch( exception &e ) {	cout << e.what() << endl;	}
			
	}	
}

//______________________________________________________________________
void mRpcSlowSim::noise_loop()
{
	recoConsts *myrc = recoConsts::instance();
	if(myrc->get_IntFlag("RpcGeomType")==0) { return; }

	if( _mod_par->get_verbosity() >= RPCOO::SOME ) {
	  cout << "mRpcSlowSim::noise_loop" << endl; }
	
	for(Int_t fArm=0 ; fArm<2 ; fArm++) {
	  for(Int_t fStation=0 ; fStation<3 ; fStation++) {
	    for(Int_t fOctant=0 ; fOctant<8 ; fOctant++) {
	      for(Int_t fHalfOctant=0 ; fHalfOctant<2 ; fHalfOctant++) {
		for(Int_t fRadSeg=0 ; fRadSeg<3 ; fRadSeg++) {
		  for(Int_t fStrip=0 ; fStrip<64 ; fStrip++) {
		    if(myrc->get_IntFlag("RpcGeomType")==1) {
		      RPCPROTOGEOM proto(RPCPROTOGEOM::ArmNumber(fArm),RPCPROTOGEOM::StationNumber(fStation),fOctant,fHalfOctant,RPCPROTOGEOM::RadialSegment(fRadSeg),fStrip);
		      if(!proto.checkStrip()) { continue; } }
		    if(myrc->get_IntFlag("RpcGeomType")==2) {
		      RPCFULLGEOM fullgeom(RPCFULLGEOM::ArmNumber(fArm),RPCFULLGEOM::StationNumber(fStation),fOctant,fHalfOctant,RPCFULLGEOM::RadialSegment(fRadSeg),fStrip);
		      if(!fullgeom.checkStrip()) { continue; } }
		    if(myrc->get_IntFlag("RpcGeomType")==3) {
		      RPCFINALGEOM finalgeom(RPCFINALGEOM::ArmNumber(fArm),RPCFINALGEOM::StationNumber(fStation),fOctant,fHalfOctant,RPCFINALGEOM::RadialSegment(fRadSeg),fStrip);
		      if(!finalgeom.checkStrip()) { continue; } }
		    
		    //- To add in the noise from real noise tables, the only line in *THIS* code to change is
		    //- below: (i.e. to read from data base and get the real noise probability from 1% to ...%)
		    if(gRandom->Rndm()>0.01) { continue; } // Only add noise 1% of the time (per strip)
		    
		    fNumNoiseHits++;
		    //- If any strip exists then write a new TRpcMCHit into the hit map (if noise is valid)
		    TRpcMCHitMap::iterator mc_hit_iter = _mc_hit_map->insert_new(fArm,fStation,fOctant,fHalfOctant,fRadSeg);
		    
		    // set position
		    PHPoint position(0,0,0);//NULL position for noise
		    mc_hit_iter->get()->set_coord( position );
	      
		    mc_hit_iter->get()->set_track_id(0);
		    mc_hit_iter->get()->set_momentum(0);
	      
		    // retrieve file key
		    //int evt_trk( 0 );
		    //int file_key( 0 );
		    //dio_TrueTrackToEtrack(&trk_id, &evt_trk, &file_key);
		    mc_hit_iter->get()->set_file_key(0);// file_key );
		    
		    // set energy loss and time of flight
		    mc_hit_iter->get()->set_eloss(0);
		    //Using the tof is the easiest way to tag a noise hit or not, real hits have real time information
		    mc_hit_iter->get()->set_tof(0);
		    
		    // add MC strip and associate to MC tracks
		    mc_hit_iter->get()->add_strip(fStrip,0);
		    //associate_mc_trk(0);//Causes a seg vio
	    
		    // increment found MC hits
		    //_accepted_mc_hits[ mRpcSlowSimPar::get_acceptance_index( arm, station ) ]++;
		  }//fStrip
		}//fRadSeg
	      }//fHalfOctant
	    }//fOctant
	  }//fStation
	}//fArm
}

//______________________________________________________________________
void mRpcSlowSim::set_interface_ptrs(PHCompositeNode* top_node)
{	
	// module runtime parameters
	_mod_par = TMutNode<mRpcSlowSimPar>::find_node(top_node,"mRpcSlowSimPar");

	// IOC pointers
	_mc_hit_map = TMutNode<TRpcMCHitMap>::find_node(top_node,"TRpcMCHitMap");
	_mc_trk_map = TMutNode<TMutMCTrkMap>::find_node(top_node,"TMutMCTrkMap");
	
	// PISA STAFF tables
	_mupc1ghit = TMutNode<mupcghitWrapper>::find_io_node(top_node,"mupc1ghit");
	_mupc2ghit = TMutNode<mupcghitWrapper>::find_io_node(top_node,"mupc2ghit");
	_mupc3ghit = TMutNode<mupcghitWrapper>::find_io_node(top_node,"mupc3ghit");
} 

//______________________________________________________________________
PHVector mRpcSlowSim::get_pisa_trk_momentum( int trk_id )
{
	
	float p_tot( 0 );
	float p_theta( 0 );
	float p_phi( 0 );

	int n_file( 0 );
	int it_parent( 0 );
	int id_parent( 0 );
	int id_part( 0 );
	
	int error( 0 );
	float z_vertex( 0 );
	float r_vertex( 0 );
	float theta_vertex( 0 );
	float phi_vertex( 0 );
	
	dio_ptrkstack(&trk_id, &n_file, &error, &p_tot, &p_theta, &p_phi,
				&r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
				&it_parent, &id_parent, &id_part);
	
	// note: this is the momentum at track origin.
	return PHVector(
			p_tot*sin(p_theta*RPCOO::DEG_TO_RAD)*cos(p_phi*RPCOO::DEG_TO_RAD),
			p_tot*sin(p_theta*RPCOO::DEG_TO_RAD)*sin(p_phi*RPCOO::DEG_TO_RAD),
			p_tot*cos(p_theta*RPCOO::DEG_TO_RAD));
	
}

//______________________________________________________________________
void mRpcSlowSim::associate_mc_trk(TRpcMCHitMap::pointer mc_hit_ptr)
{
		
	// Get an iterator to all TRpcMCTrk in map
	TMutMCTrkMap::iterator mc_trk_iter = _mc_trk_map->range();

	// Loop over TMutMCTrk [
	//	If we find a track with matching track id 
	//		associate this hit
	//	Else 
	//		make a new TMutMCTrk
	// ]

	while(TMutMCTrkMap::pointer mc_trk_ptr = mc_trk_iter.next()){
		if(mc_trk_ptr->get()->get_track_id() == mc_hit_ptr->get()->get_track_id()) {			
			
			// Track ids match so we make association and return
			PHKey::associate(mc_hit_ptr, mc_trk_ptr);
			return;
			
		} 
	}			
	
	// No matching track id was found so we make an new TMutMCTrk
	fill_new_mc_trk(mc_hit_ptr);
}

//______________________________________________________________________
void mRpcSlowSim::fill_new_mc_trk(TRpcMCHitMap::pointer mc_hit_ptr, int trackID)
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

		  while(TMutMCTrkMap::pointer mc_trk_ptr = mc_trk_iter2.next())
		    {

		      if(mc_trk_ptr->get()->get_track_id() == abs(itparent))
			{		
			  parent_trk_filled = true;
			  break;
			}
 
		    }
		  if (!parent_trk_filled) fill_new_mc_trk(mc_hit_ptr, abs(itparent));
		}
}
