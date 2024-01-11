// $Id: MWGFvtxReco.C,v 1.29 2015/06/04 21:46:22 snowball Exp $

/*!
\file    MWGFvtxReco.C
\brief   muon-fvtx nanoDST creation, using new framework input nodes
\author  Cesar Luiz da Silva
\version $Revision: 1.29 $
\date    $Date: 2015/06/04 21:46:22 $
*/

#include <Fun4AllReturnCodes.h>
#include <PHIODataNode.h>
#include <PHGeometry.h>
#include <MUTOO.h>

// MUTOO
#include <TMutCoordMap.h>
#include <TMutGapCoordMap.h>
#include <TMutClusMap.h>
#include <TMutTrackUtil.h>
#include <TMutTrkMap.h>
#include <TMutVtxMap.h>

#include <mMutFitVtx.h>
#include <TMutExtVtx.h>

// MUIOO
#include <TMuiHitMapO.h>
#include <TMuiRoadMapO.h>
#include <TMuiClusterMapO.h>
#include <MuiCommon.hh>
#include <iostream>
#include <string>

// MWG
#include <PHInclusiveNanoCuts.h>
#include <PHMuoTracksOut.h>

#include <MWG.h>
#include <MWGVersion.h>
#include <rcp.h>

// FVTXOO
#include <mMutKalFitWithSiliRealPar.h>
#include <TFvtxTrkMap.h>
#include <TFvtxClusMap.h>
#include <TFvtxCoordMap.h>
#include <TFvtxHitMap.h>
#include <TFvtxStraightTrkParMap.h>
#include <TFvtxTrk.h>
#include <TFvtxClus.h>
#include <TFvtxCoord.h>
#include <TFvtxHit.h>
#include <PHTrackIntegratorKF.h>

#include "MWGFvtxReco.h"
#include <FVTXOO.h>

using namespace std;

//_______________________________________________________________
MWGFvtxReco::MWGFvtxReco(PHInclusiveNanoCuts *aCutter):
  _timer(PHTimeServer::get()->insert_new("MWGFVTXRECO") ),
  _cutter( aCutter ),
  _mutoo_node( 0 ),
  _muioo_node( 0 ),
  _fvtxoo_node( 0 )
{
  ThisName = "MWGFVTXRECO";
  integrator = new PHTrackIntegratorKF();
}

//_______________________________________________________________
MWGFvtxReco::~MWGFvtxReco()
{
  if(integrator) delete integrator;
}

//_______________________________________________________________
int MWGFvtxReco::Init(PHCompositeNode *top_node)
{
  MUTOO::PRINT( cout, "MWGFvtxReco::Init" );
  PHNodeIterator iter(top_node);
  PHCompositeNode *dstNode =  static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode","DST"));
  
  if( !dstNode ) {
    cout << "MWGFvtxReco::Init - cannot access DST node" << endl;
    return 1;
  }

  bool do_dimuons = _cutter->get_dodimu();

  // single muon and dimuon branches selection
  if (RCP::file_ok("MWG.rcp")) RCP::get<bool>("MWG.rcp","dodimu", do_dimuons);
  
  // create containers
 PHMuoTracksOut *particle(0);
  if( do_dimuons ) particle = MWG::newPHdiMuoTracksFvtx();
  else particle = MWG::newPHMuoTracksFvtx();


  cout << "MWGFvtxReco::Init - using class " << particle->GetName() << " (version: " << MWGVersion::get( particle->GetName() ) << ")" << endl;
  
  // old and mew muon framework selection
  dstNode->addNode( new PHIODataNode<PHObject>(particle,"PHMuoTracksOO","PHObject") );
  
  MUTOO::PRINT( cout, "**" );

  _ievt = 0;

  return 0;
}

//_______________________________________________________________
int MWGFvtxReco::InitRun(PHCompositeNode *top_node)
{
  _cutter->Initialize(top_node);
  PHNodeIterator iter(top_node);
  PHCompositeNode *runNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode","RUN"));
  if (!runNode) {
    cout << PHWHERE << "MWGFvtxReco:RUN Node missing doing nothing" << endl;
    return -1;
  } else {
    PHIODataNode<PHObject>* CutsNode =  new PHIODataNode<PHObject>(_cutter,"MWGCuts","PHObject");
    runNode->addNode(CutsNode);
  }
  return 0;
}

//_______________________________________________________________
int MWGFvtxReco::process_event(PHCompositeNode *top_node)
{
  
  // check event variables
  _timer.get()->restart();
  bool accepted = _cutter->GlobalOK(top_node);
  
  // loop over input particles; fill output; make cuts..
  try {
    
    // retrive nodes
    set_node_ptrs( top_node );
    
    // retrieve pointer to particles
    PHMuoTracksOut *particle = TMutNode<PHMuoTracksOut>::find_io_node( _ndst_node, "PHMuoTracksOO");
    particle->Reset();
    
    // retrieve new framework muon tracks, map tracks unique IDs to local tracks IDs
    map< ULong_t, int > track_keys = do_muons( particle ) ;
    
    // retrieve dimuons
    if (_cutter->get_dodimu())  do_dimuons( particle, track_keys );
    
    // check if filtering is required
    if( _cutter->get_dofilter() ) {
      cout << "MWGFvtxReco::process_event - no filtering implemented" << endl;      
    }

  } catch( exception &e ) { cout << e.what() << endl; }
  
  _timer.get()->stop();
  
  return (accepted) ? EVENT_OK:DISCARDEVENT;
}

//______________________________________________________
int MWGFvtxReco::End(PHCompositeNode* top_node)
{
  _timer.get()->print_stat();

  return 0;
}

//______________________________________________________________________________________
map< ULong_t, int > MWGFvtxReco::do_muons( PHMuoTracksOut *particle )
{
  integrator->clear();

  // list of accepted track keys
  map<ULong_t,int> keys;
  
  // retrieve tracks, check if empty
  TMutTrkMap *trk_map = TMutNode<TMutTrkMap>::find_node(_mutoo_node,"TMutTrkMap");
  if( trk_map->empty() ) return keys;
    
  // count accepted tracks
  int n_tracks(0);
  TMutTrkMap::const_iterator trk_iter = trk_map->range();
  while( TMutTrkMap::const_pointer trk_ptr = trk_iter.next() )
  { if( accept_track( trk_ptr ) ) n_tracks++; }
  
  particle->set_npart(n_tracks) ;
  particle->set_TClonesArraySize(n_tracks);
  
  // Loop over particles
  int itrk=0;
  trk_iter.reset();
  while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next()) 
  {
    
    // check if track is to be considered
    if( !accept_track( trk_ptr ) ) continue;
    
    // create new particle
    particle->AddPHParticle(itrk);
    
    // unique ID
    particle->set_uid( itrk, trk_ptr->get()->get_key().get_obj_key() );
    
    // Track parameters at primary vertex
    const TMutTrkPar* trk_par = trk_ptr->get()->get_trk_par_vtx();
    if( trk_par ) particle->set_charge(itrk, static_cast<int>(trk_ptr->get()->get_charge()) );
    
    /*
    retrieve chisquare. only the _w chisquare is retrieved
    since the _r chisquare does not make sense when using the KalmanFilter
    */
    particle->set_chisquare( itrk, trk_ptr->get()->get_w_chi_square_pdf() );
    particle->set_ndf( itrk, trk_ptr->get()->get_ndf() );
    
    particle->set_px(0, itrk, trk_par->get_px());
    particle->set_py(0, itrk, trk_par->get_py());
    particle->set_pz(0, itrk, trk_par->get_pz());
    particle->set_xpos(0, itrk, trk_par->get_x());
    particle->set_ypos(0, itrk, trk_par->get_y());
    particle->set_zpos(0, itrk, trk_par->get_z());
    
    // Covariance matrix at primary vertex
    //
    for (int iarr1=0; iarr1<5; iarr1++)
    {
      for (int iarr2=0; iarr2<5; iarr2++)
      { particle->set_cov(iarr1, iarr2, itrk, trk_par->get_covar(iarr1, iarr2)); }
    }
    
    // Track parameters at gap 0 of each station
    for (int sta=0; sta<3; ++sta)
    {
      const TMutTrkPar* sta_trk_par = trk_ptr->get()->get_trk_par_station(sta);
      if( sta_trk_par )
      {
        particle->set_px(sta+1, itrk, sta_trk_par->get_px());
        particle->set_py(sta+1, itrk, sta_trk_par->get_py());
        particle->set_pz(sta+1, itrk, sta_trk_par->get_pz());
        particle->set_xpos(sta+1, itrk, sta_trk_par->get_x());
        particle->set_ypos(sta+1, itrk, sta_trk_par->get_y());
        particle->set_zpos(sta+1, itrk, sta_trk_par->get_z());
      }
    }
    
    //Add the new kalman projection at Gap0
    if( !trk_ptr->get()->get_trk_par_list()->empty() )
    {
      const TMutTrkPar& track_par( trk_ptr->get()->get_trk_par_list()->back() );
      particle->set_px(4,itrk, track_par.get_px());
      particle->set_py(4,itrk, track_par.get_py());
      particle->set_pz(4,itrk, track_par.get_pz());
      particle->set_xpos(4,itrk, track_par.get_x());
      particle->set_ypos(4,itrk, track_par.get_y());
      particle->set_zpos(4,itrk, track_par.get_z());
    }
    
    // Hit bit pattern
    UShort_t pattern( trk_ptr->get()->get_hit_pattern() );
    particle->set_muTRhits(itrk, pattern);
    
    // get number of hits on track from hit_pattern
    int n_hit = 0;
    static const int numberOfCathodes( 16 );
    for( int i=0; i < numberOfCathodes; i++ ) { if( pattern & ( 1 << i ) ) n_hit++; }
    particle->set_nhits(itrk, n_hit );
    
    // All MUTOO ghost tracks have already been removed.
    // all remaining tracks have ghostflag set to 0
    //particle->set_ghostflag(itrk,0);
    particle->set_TMutTrk_status(itrk,trk_ptr->get()->get_status());
    
    // loop over associated roads
    TMuiRoadMapO::const_key_iterator mui_iter = trk_ptr->get()->get_associated<TMuiRoadO>();
    
    short deepest_depth = 0;
    while(TMuiRoadMapO::const_pointer mui_ptr = mui_iter.next())
    {
      
      const short iroad = (mui_ptr->get()->get_depth())-2;
      particle->set_muIDOOhits(iroad, itrk, mui_ptr->get()->get_gapbit());
      
      // road parameters
      const TMutFitPar* road_par = mui_ptr->get()->get_const_fitpar();
      if( !road_par ) continue;
      
      particle->set_muIDOOchi(iroad, itrk, road_par->get_chi_square());
      
      PHPoint gap0_point = mui_ptr->get()->get_gap0_point();
      particle->set_muIDOO_gap0(0,iroad,itrk,(gap0_point.getX()));
      particle->set_muIDOO_gap0(1,iroad,itrk,(gap0_point.getY()));
      particle->set_muIDOO_gap0(2,iroad,itrk,(gap0_point.getZ()));
      particle->set_muIDOO_gap0(3,iroad,itrk,road_par->get_dxdz());
      particle->set_muIDOO_gap0(4,iroad,itrk,road_par->get_dydz());
      
      if (mui_ptr->get()->get_depth() > deepest_depth) 
      {
        deepest_depth = mui_ptr->get()->get_depth();
        
        for(int igap=0; igap<5; igap++){
          particle->set_muid_hit_x(igap, itrk, -8888.);
          particle->set_muid_hit_y(igap, itrk, -8888.);
        }

        // get the distance between the current road and its associated clusters (index=0)
        // retrieve associated clusters
        TMuiClusterMapO::key_iterator iClust = mui_ptr->get()->get_associated<TMuiClusterO>();
        
        // loop over clusters
        int index=0;
        while(TMuiClusterMapO::pointer pClust = iClust.next()) 
        {
	  if(pClust->get()->get_orientation() == kVERT)
	    particle->set_muid_hit_x(pClust->get()->get_plane(), itrk, pClust->get()->get_centroidpos().getX());
	  else
	    particle->set_muid_hit_y(pClust->get()->get_plane(), itrk, pClust->get()->get_centroidpos().getY());

          index++;
        }
      } // depth check
    } // loop over associated roads
    
    // fill gap coordinate charge difference and errors
    unsigned short clusters_size1 = 0;
    TMutGapCoordMap::const_key_iterator gap_coord_iter( trk_ptr->get()->get_associated< TMutGapCoord >() );
    while( TMutGapCoordMap::const_pointer gap_coord_ptr = gap_coord_iter.next() )
    {
      
      // get associated coordinates
      TMutCoordMap::const_key_iterator coord_iter( gap_coord_ptr->get()->get_associated< TMutCoord >() );
      if( coord_iter.count() != 2 ) {
        cout << "MWGFvtxReco::do_muons_OO - gap coordinate does not match 2 coordinates. skipped" << endl;
        continue;
      }
      
      // save charges and error and count clusters with size==1
      float q[2] = {0,0};
      float q_error[2] = {0,0};
      while( TMutCoordMap::const_pointer coord_ptr = coord_iter.next() )
      {
        q[ coord_ptr->get()->get_cathode() ] = static_cast<float>(coord_ptr->get()->get_q_tot());
        q_error[ coord_ptr->get()->get_cathode() ] = static_cast<float>(coord_ptr->get()->get_q_error());
	TMutClusMap::key_iterator clus_iter = coord_ptr->get()->get_associated<TMutClus>();
	TMutClusMap::const_pointer clus_ptr = clus_iter.current();
	if (clus_ptr->get()->get_n_strip() == 1)
	  {
	    unsigned short index = clus_ptr->get()->get_cathode() 
	      +	MUTOO::NumberOfCathodePlanes*(clus_ptr->get()->get_gap() 
					      + MUTOO::NumberOfGaps*(clus_ptr->get()->get_station()));
	    clusters_size1 |= ( 1 << index );
	  }
      }
      
      // add 2D dependant correction to charge in cathode 1
      float correction( static_cast<float>(gap_coord_ptr->get()->get_charge_corr()));
      q[1]/=(1+correction);
      q_error[1]/=(1+correction);
      
      // calculate delta_q and error
      float delta_q = 2*( q[1]-q[0] )/( q[1] + q[0] );
      float delta_q_error = 4 *
        sqrt(
	     MUTOO::SQUARE( q[1]*q_error[0] ) +
	     MUTOO::SQUARE( q[0]*q_error[1] )) /
        MUTOO::SQUARE( q[0] + q[1] );
    
      // calculate gap coordinate location and append to track
      unsigned int index = gap_coord_ptr->get()->get_station()*3+gap_coord_ptr->get()->get_gap();
      particle->set_delta_q( index, itrk, delta_q );
      particle->set_delta_q_error( index, itrk, delta_q_error );
    
    }
    particle->set_clusters_size1(itrk, clusters_size1);
    
    //---> NOT FILLED
//     particle->set_PID(itrk,0);                         
//     particle->set_MuonConfidence(itrk,0);              
//     particle->set_PionConfidence(itrk,0);       
    
    // particle bent plane momentum at station 1
    const TMutBPPar* bp_par( trk_ptr->get()->get_bp_par() );
    if( bp_par )
    {
      particle->set_st1_bp_P(0,itrk, bp_par->get_px_st1() ); 
      particle->set_st1_bp_P(1,itrk, bp_par->get_py_st1() ); 
      particle->set_st1_bp_P(2,itrk, bp_par->get_pz_st1() ); 
    }
    
    double ref = 40.0;
    if ( _mod_par )
      ref = _mod_par->get_fvtx_mutr_proximity_zref();
    double dir = 1.0; // point to Z=0;
    if (trk_par->get_pz()<0)
      {
    	ref *= -1.0;
    	dir = -1.0;
      }
    PHPoint pref(0,0,ref);
    PHVector vnorm(0.0,0.0,dir);
    const PHPlane planeref(pref, vnorm);
    
  
    /*
    integrator.initialize( *trk_ptr->get()->get_trk_par_station(MUTOO::Station1) );
    integrator.extrapolate ( ref );
    
    TMutTrkPar extrap_trk_par;
    if (integrator.get_error())
      extrap_trk_par = *trk_par;
    else
      integrator.finish( extrap_trk_par );
    
    PHVector mutvec(extrap_trk_par.get_px(), 
    		    extrap_trk_par.get_py(),
    		    extrap_trk_par.get_pz());

    PHPoint  mutproj(extrap_trk_par.get_x(), extrap_trk_par.get_y(), ref);
    */

    TMutTrkPar trk_par_st1(*trk_ptr->get()->get_trk_par_station(MUTOO::Station1));

    bool error( false );
    PHPoint vtx( TMutExtVtx::get().get_vtx( error ) );
    if( !error )
    {
      // fit together with the vertex
      mMutFitVtx::Fitter vertex_fitter;
      vertex_fitter.add_track( trk_ptr );

      // disable usage of X and Y
      vtx.setX(0);
      vtx.setY(0);

      // set vertex errors manually
      PHPoint vtx_error( 0.1, 0.1, 0.5 );
      //PHPoint vtx_error( 0.5, 0.5, 2.5 );

      // add vertex information to fitter and do the fit
      vertex_fitter.add_vertex( vtx, vtx_error );
      vertex_fitter.fit();

      // store fit result in trk_par
      trk_par_st1 = TMutTrkPar(
        vertex_fitter.get_vtx_x(),
        vertex_fitter.get_vtx_y(),
        vertex_fitter.get_vtx_z(),
        vertex_fitter.get_px(0),
        vertex_fitter.get_py(0),
        vertex_fitter.get_pz(0),
        static_cast<int>(trk_ptr->get()->get_charge() ),
        vertex_fitter.get_chisquare() );
    }
    integrator->initialize( trk_par_st1 );
    integrator->extrapolate ( ref );

    TMutTrkPar extrap_trk_par;
    if (integrator->get_error())
      {
	cout << PHWHERE << ": MuTr track extrapolation to FVTX st1 failed.\n";
	extrap_trk_par = trk_par_st1;
      } 
    else
      {
	integrator->finish( extrap_trk_par );
      }

    PHVector mutvec(extrap_trk_par.get_px(), 
                     extrap_trk_par.get_py(),
                     extrap_trk_par.get_pz());
 
    PHPoint mutproj(extrap_trk_par.get_x(), extrap_trk_par.get_y(), ref);

    float mut_phi = atan2(extrap_trk_par.get_y(), extrap_trk_par.get_x());

    TFvtxTrkMap::const_key_iterator fvtx_trk_iter = trk_ptr->get()->get_associated<TFvtxTrk>();
    if (fvtx_trk_iter.count()>0)
      {
	while ( TFvtxTrkMap::const_pointer fvtx_trk_ptr = fvtx_trk_iter.next() )
	  {
	    TFvtxTrk* fvtx_trk = fvtx_trk_ptr->get();
	    if (fvtx_trk)
	      {
		const TMutTrkPar* fvtxmutr_trk_par = fvtx_trk->get_trk_par_mutr();	    
		if (fvtxmutr_trk_par)
		  {
		    particle->set_fvtxmutr_vtx(itrk, 0, fvtxmutr_trk_par->get_x());
		    particle->set_fvtxmutr_vtx(itrk, 1, fvtxmutr_trk_par->get_y());
		    particle->set_fvtxmutr_vtx(itrk, 2, fvtxmutr_trk_par->get_z());
		    
		    particle->set_fvtxmutr_p(itrk, 0, fvtxmutr_trk_par->get_px());
		    particle->set_fvtxmutr_p(itrk, 1, fvtxmutr_trk_par->get_py());
		    particle->set_fvtxmutr_p(itrk, 2, fvtxmutr_trk_par->get_pz());
		    for (int i=0; i<TMutTrkPar::COVAR_ROW; i++)
		      for (int j=0; j<TMutTrkPar::COVAR_ROW; j++)
			particle->set_fvtxmutr_cov(itrk, i, j, fvtxmutr_trk_par->get_covar(i,j));
		    particle->set_fvtxmutr_chi2(itrk, fvtxmutr_trk_par->get_chi_square());
		  }
		
		const TMutTrkPar* fvtx_trk_par = fvtx_trk->get_trk_par_vtx();
		if (fvtx_trk_par)
		  {
		    particle->set_fvtx_vtx(itrk, 0, fvtx_trk_par->get_x());
		    particle->set_fvtx_vtx(itrk, 1, fvtx_trk_par->get_y());
		    particle->set_fvtx_vtx(itrk, 2, fvtx_trk_par->get_z());
		    
		    particle->set_fvtx_p(itrk, 0, fvtx_trk_par->get_px());
		    particle->set_fvtx_p(itrk, 1, fvtx_trk_par->get_py());
		    particle->set_fvtx_p(itrk, 2, fvtx_trk_par->get_pz());
		    for (int i=0; i<TMutTrkPar::COVAR_ROW; i++)
		      for (int j=0; j<TMutTrkPar::COVAR_ROW; j++)
			particle->set_fvtx_cov(itrk, i, j, fvtx_trk_par->get_covar(i,j));
		    particle->set_fvtx_chi2(itrk, fvtx_trk->get_w_chi_square_pdf());
		    
		    integrator->clear();
		    integrator->initialize(fvtx_trk->get_trk_par_list()->back());
		    integrator->extrapolate ( ref );
		    if (integrator->get_error())
		      extrap_trk_par = *fvtx_trk->get_trk_par_vtx();
		    else
		      integrator->finish( extrap_trk_par );
		    
		    PHVector fvtxvec(extrap_trk_par.get_px(), 
				     extrap_trk_par.get_py(), 
				     extrap_trk_par.get_pz());
		    
		    PHPoint fvtxproj(extrap_trk_par.get_x(),
				     extrap_trk_par.get_y(), 
				     ref);

		    float fvtx_phi = atan2(fvtxproj.getY(), fvtxproj.getX());
		    float fvtx_dphi = mut_phi - fvtx_phi;
		    fvtx_dphi = 0.5*atan2(sin(2*fvtx_dphi),cos(2*fvtx_dphi));
		    float fvtx_dr = sqrt(pow(mutproj.getY() - fvtxproj.getY(),2)
					 + pow(mutproj.getX() - fvtxproj.getX(),2));
		    float fvtx_dtheta = fvtxvec.angle(mutvec);
		    particle->set_fvtx_dphi(itrk, fvtx_dphi);
		    particle->set_fvtx_dr(itrk, fvtx_dr);
		    particle->set_fvtx_dtheta(itrk, fvtx_dtheta);
		  }
		unsigned short fvtx_hit_pattern = fvtx_trk->get_hit_pattern();
		fvtx_hit_pattern |= ( fvtx_trk->get_svxhit_pattern() << 8);
		particle->set_fvtx_hits( itrk, fvtx_hit_pattern );
		TFvtxCoordMap::key_iterator coord_iter = fvtx_trk->get_associated<TFvtxCoord>();
		size_t ncoord = 0;
		while( TFvtxCoordMap::const_pointer coord_ptr = coord_iter.next() )
		  {
		    size_t arm = coord_ptr->get()->get_arm();
		    size_t cage = coord_ptr->get()->get_cage();
		    size_t station = coord_ptr->get()->get_station();
		    size_t sector = coord_ptr->get()->get_sector();
		    size_t column = coord_ptr->get()->get_column();
		    //		cout << "MWGReco_pre::arm=" << arm << " cage=" << cage << " station=" << station << " sector=" << sector
		    //		     << " column=" << column << " strip=" << coord_ptr->get()->get_peak_strip() << endl;
		    size_t plane = station*2; // plane position relative to collision point
		    if (!(sector%2) && arm==MUTOO::South) plane++;
		    if ((sector%2) && arm==MUTOO::North) plane++;
		    particle->set_fvtx_cluster_charge(itrk, station, coord_ptr->get()->get_q_tot());
		    particle->set_fvtx_strip(itrk, station, arm, cage, sector, column, coord_ptr->get()->get_peak_strip());
		    particle->set_fvtx_w(itrk, station, coord_ptr->get()->get_w());
		    ncoord ++;
		    TFvtxClusMap::key_iterator clus_iter = coord_ptr->get()->get_associated<TFvtxClus>();
		    TFvtxClusMap::const_pointer clus_ptr = clus_iter.current();
		    TFvtxHitMap::key_iterator hit_iter = clus_ptr->get()->get_associated<TFvtxHit>();
		    particle->set_fvtx_cluster_size(itrk, plane, hit_iter.count());
		  }
	      }
	  }
      }
    // track cuts
    if (_cutter->MuonOK(particle, itrk)) 
      {
	
	// Insert the track object key and itrk index into the map
	keys.insert(make_pair(trk_ptr->get()->get_key().get_obj_key(), itrk));
	
	// Increment the track if particle OK...
	itrk++;
	
      } else particle->RemovePHParticle(itrk);
    
  } // end of track loop
  
  // resize array
  particle->set_npart(itrk);
  particle->set_TClonesArraySize(itrk);
  
  return keys;
  
}

//______________________________________________________________________________________
void MWGFvtxReco::do_dimuons(PHMuoTracksOut *particle, const map<ULong_t,int>& keys )
{
  
  // iterator over track index vs key map
  typedef map<ULong_t,int>::const_iterator key_map_iterator;
  
  // retrieve vertex map and check size
  TMutVtxMap* vtx_map = TMutNode<TMutVtxMap>::find_node(_mutoo_node,"TMutVtxMap");
  if(vtx_map->empty()) return;
  
  //! resize arrays
  particle->set_ndimu(vtx_map->size());
  particle->Set_DimuArraySize(vtx_map->size());
  
  // Loop over TMutVtx objects
  int idimu=0;
  TMutVtxMap::const_iterator vtx_iter = vtx_map->range();
  while(TMutVtxMap::const_pointer vtx_ptr = vtx_iter.next())
  {
    
    // create new dimuon
    particle->AddPHDimuon(idimu);
    particle->set_dimuon_uid( idimu, vtx_ptr->get()->get_key().get_obj_key() );
    
    // Loop over associated tracks.  Find the index in the key vector
    // that matches the associated track key.  Set the particle index
    // accordingly.
    UShort_t itrk=0;
    TMutTrkMap::const_key_iterator trk_iter = vtx_ptr->get()->get_associated<TMutTrk>();
    while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next())
    {
      
      // Set the index in the dimuon object corresponding to the track
      key_map_iterator iter = keys.find(trk_ptr->get()->get_key().get_obj_key());
      if(iter != keys.end() ) particle->set_ditrkIndex(itrk++, idimu, iter->second);
      else cerr
        << "MuonNanoDSTfuncs::do_dimuons_OO - could not find associated track "
        << trk_ptr->get()->get_key().get_obj_key()
        << " in keylist. " << endl;
    }
    
    //===== computing dimuon charge
    //  0 ==> +- dimuon pair
    //  1 ==> ++ dimuon pair
    // -1 ==> -- dimuon pair
    // -2 ==> one of the muon charge = 0
    if(vtx_ptr->get()->get_sign() == TMutVtx::POSNEG) particle->set_dicharge(idimu,0);
    else if(vtx_ptr->get()->get_sign() == TMutVtx::POSPOS) particle->set_dicharge(idimu,1);
    else if(vtx_ptr->get()->get_sign() == TMutVtx::NEGNEG) particle->set_dicharge(idimu,-1);
    else particle->set_dicharge(idimu,-2);
    
    particle->set_dipx(idimu,vtx_ptr->get()->get_px());
    particle->set_dipy(idimu,vtx_ptr->get()->get_py());
    particle->set_dipz(idimu,vtx_ptr->get()->get_pz());
    
    // vertex BP information
    particle->set_vtx_bp_xpos(idimu,vtx_ptr->get()->get_x_bp());
    particle->set_vtx_bp_ypos(idimu,vtx_ptr->get()->get_y_bp());
    particle->set_vtx_bp_zpos(idimu,vtx_ptr->get()->get_z_bp());
    particle->set_vtx_bp_dca(idimu,vtx_ptr->get()->get_dca_bp());
    
    // vertex fit information
    particle->set_vtx_xpos(idimu,vtx_ptr->get()->get_x());
    particle->set_vtx_ypos(idimu,vtx_ptr->get()->get_y());
    particle->set_vtx_zpos(idimu,vtx_ptr->get()->get_z());
    
    particle->set_vtx_chrg_1(idimu,vtx_ptr->get()->get_charge1());
    particle->set_vtx_px_1(idimu,vtx_ptr->get()->get_px1());
    particle->set_vtx_py_1(idimu,vtx_ptr->get()->get_py1());
    particle->set_vtx_pz_1(idimu,vtx_ptr->get()->get_pz1());
    
    particle->set_vtx_chrg_2(idimu,vtx_ptr->get()->get_charge2());
    particle->set_vtx_px_2(idimu,vtx_ptr->get()->get_px2());
    particle->set_vtx_py_2(idimu,vtx_ptr->get()->get_py2());
    particle->set_vtx_pz_2(idimu,vtx_ptr->get()->get_pz2());
    
    particle->set_dimass(idimu,vtx_ptr->get()->get_mass());
    particle->set_vtx_chisquare(idimu,vtx_ptr->get()->get_chi_square_pdf());
    particle->set_vtx_ndf(idimu,vtx_ptr->get()->get_ndf());
    
    // Note currently only 7 elements of 9 possible are used
    for (int icov=0; icov<7; icov++)
    {
      for (int jcov=0; jcov<7; jcov++)
      { particle->set_vtx_cov(icov,jcov,idimu,vtx_ptr->get()->get_covar(icov,jcov)); }
    }
    
    //===== Dimuon quality check (so far, nothing)
    if (_cutter->diMuonOK(particle, idimu)) idimu++;
    else particle->RemovePHDimuon(idimu);
    
  }
  
  // resize array
  particle->set_ndimu(idimu);
  particle->Set_DimuArraySize(idimu);
  
}


//____________________________________________________________
bool MWGFvtxReco::set_node_ptrs(PHCompositeNode* top_node)
{
  // look for mutoo node
  PHNodeIterator node_iter(top_node);
  
  // ndst node
  _ndst_node = static_cast<PHCompositeNode*>(node_iter.findFirst("PHCompositeNode", "NDST"));
  if(!_ndst_node) _ndst_node = top_node;
  
  //! mutoo node
  _mutoo_node = static_cast<PHCompositeNode*>(node_iter.findFirst("PHCompositeNode", "MUTOO" ));
  if( !_mutoo_node )
    throw logic_error( "MWGFvtxReco::set_node_ptrs - could not find node MUTOO. You shoud run the correct Unpacker to your macro." );
  
  // muioo node
  _muioo_node = static_cast<PHCompositeNode*>(node_iter.findFirst("PHCompositeNode", "MUIOO" ));
  if( !_muioo_node )
    throw logic_error( "MWGFvtxReco::set_node_ptrs - could not find node MUIOO. You shoud run the correct Unpacker to your macro." );
  
  // fvtx node
  _fvtxoo_node = static_cast<PHCompositeNode*>(node_iter.findFirst("PHCompositeNode", "FVTXOO" ));
  if( !_fvtxoo_node )
    throw logic_error( "MWGFvtxReco::set_node_ptrs - could not find node FVTXOO. You shoud run the correct Unpacker to your macro." );

  _mod_par = TMutNode<mMutKalFitWithSiliRealPar>::find_node(top_node,"mMutKalFitWithSiliRealPar");

  return true;
}

//__________________________________________________________________
bool MWGFvtxReco::accept_track( TMutTrkMap::const_pointer trk_ptr ) const
{
  // check if track is a ghost
  if( trk_ptr->get()->get_ghost() ) return false;
  
  // check if track was fitted
  if( !trk_ptr->get()->get_reco_success() ) return false;
  
  // all checks passed
  return true;
  
}

