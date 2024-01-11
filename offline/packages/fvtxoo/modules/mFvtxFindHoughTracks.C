// $Id: mFvtxFindHoughTracks.C,v 1.52 2017/10/12 18:08:27 shlim Exp $
 
/*!
  \file		mFvtxFindHoughTracks.C
  \ingroup modules
  \brief	 Combinatorial Hough transform based pattern recognition module for the FVTX
  \author	 Aaron Key
  \version $Revision: 1.52 $
  \date		$Date: 2017/10/12 18:08:27 $
*/


#include <iterator>
#include <algorithm>
#include <boost/functional/factory.hpp>
#include <boost/function.hpp>

#include <PHGeometry.h>
#include <PHLine.h>
#include <PHVector.h>
#include <PHPoint.h>

#include <PHTFileServer.h>
#include <mFvtxFindHoughTracks.h>
#include <TFvtxGlobalParCntrl.h>
#include <TMutMCTrkMap.h>
#include <TFvtxMCHitMap.h>
#include <TFvtxHitMap.h>
#include <TFvtxClusMap.h>
#include <TFvtxSvxClusterMap.h>
#include <TFvtxCoordMap.h>
#include <mFvtxFindTrackPar.h>
#include <TMutExtVtx.h>
#include <VtxOut.h>
#include <getClass.h>

#include <FvtxGeom.h>
#include <FvtxStation.h>
#include <FvtxSector.h>
#include <FvtxColumn.h>
#include <mFvtxKalFit.h>
#include <mMutFitVtx.h>
#include <PHTrackIntegratorKF.h>

#include <TMuiRoadMapO.h>
#include <TMuiHitMapO.h>
#include <TMutTrackUtil.h>
#include <TMutTrkMap.h>

#include <Fun4AllServer.h>
#include <TriggerHelper.h>
#include <TrigLvl1.h>

#include <SvxBeamCenterPar.h>

using boost::bind;
using namespace std;
using namespace myspace;

// Self pointer for GetInstance
mFvtxFindHoughTracks *mFvtxFindHoughTracks::HFInstance = 0;

// Constructor
mFvtxFindHoughTracks::mFvtxFindHoughTracks() : 
  _vtxout(0),
	_triglvl1(0),
  _precise_vtx(0),
  _precise_vtx2(0),
  _mod_par(0),
  _trk_map(0),
  _ctrk_map(0),
  _coord_map(0),
  _svx_map(0),
  _timer("mFvxtFindHoughTracksMS"),
  _doQuickMuonReco(false),
  _hough_cluster_tree(0),
  _houghCT_pass(0),
  _found_pairs_tree(0),
  _foundPT_pass(0)
{
  _doEval = false;
  _n_svx_layers = 0;
  _use_svx_cluster = true;
  _reverseTracking = false;
  _twoHitMode = false;
  _dR = 0.5;
  _dZ = 0.5;
  _dPhi = 0.0;
  _dPhiHitsMax = 0.32;
  _rClusCutFixed = 0.03;
  _alphaClusCutFixed = 0.0015;
  _vertex_z = -999.0;
  _precise_z = -999.0;
  _precise_z2 = -999.0;
	_use_precise_vtx = false;
	_isUPC = false;
  _beam_x = 0;
  _beam_y = 0;
  _ievent = 0;
  _hitId = 0;
  _trackId = 0;

  //Master container class for pairs
  _hough_pairs = new EVHoughPairVec(FVTXOO::MAX_ARM,NSLICES);

}

// Return self-pointer 
mFvtxFindHoughTracks *mFvtxFindHoughTracks::GetInstance()
{
  if (HFInstance) return HFInstance;
  HFInstance = new mFvtxFindHoughTracks();
  return HFInstance;
}




void mFvtxFindHoughTracks::init(PHCompositeNode*)
{
  _ievent = 0;
  _use_svx_cluster = TFvtxGlobalParCntrl::get_bool_par("use_svx");


  if( _doEval ) 
  {
    //Vectors for evaluation trees
    _houghCT_clusID = new std::vector<int>();
    _houghCT_hit1_x = new std::vector<float>();
    _houghCT_hit1_y = new std::vector<float>();
    _houghCT_hit1_z = new std::vector<float>();
    _houghCT_hit1_r = new std::vector<float>();
    _houghCT_hit1_phi = new std::vector<float>();
    _houghCT_hit2_x = new std::vector<float>();
    _houghCT_hit2_y = new std::vector<float>();
    _houghCT_hit2_z = new std::vector<float>();
    _houghCT_hit2_r = new std::vector<float>();
    _houghCT_hit2_phi = new std::vector<float>();
    _houghCT_pair_rInt = new std::vector<float>();
    _houghCT_pair_sinAlpha = new std::vector<float>();
    _houghCT_hasSVXCluster = new std::vector<bool>();

    _houghCT_nHits = 0;
    _houghCT_nClusters = 0;
    _houghCT_slice = 0;
    _houghCT_nPairs = 0;
    _houghCT_pass = 0;
    _houghCT_nMCHitsFromSameTrk = -1;
    _houghCT_nMCTrksInCluster = -1;
    _houghCT_nUniqueHits = -1;
    _foundPT_slice = -1;
    _foundPT_pass = 0;
    _foundPT_arm = 0;


    _foundPT_hit1_x = 0;
    _foundPT_hit1_y = 0;
    _foundPT_hit1_z = 0;
    _foundPT_hit1_r = 0;
    _foundPT_hit1_phi = 0;
		_foundPT_hit1_station = -999;
		_foundPT_hit1_layer = -999;
    _foundPT_hit2_x = 0;
    _foundPT_hit2_y = 0;
    _foundPT_hit2_z = 0;
    _foundPT_hit2_r = 0;
    _foundPT_hit2_phi = 0;
		_foundPT_hit2_station = -999;
		_foundPT_hit2_layer = -999;
    _foundPT_pair_rInt = 0;
    _foundPT_pair_sinAlpha = 0;
		_foundPT_pair_rInt_beam = 0;
		_foundPT_pair_sinAlpha_beam = 0;
		_foundPT_pair_DcaZ = -999;


    _foundTT_track_id = 0;
    _foundTT_arm = -1;
    //Evaluation TFile
    evalFile = new TFile("HoughEval.root", "RECREATE");
    
    //Initialize TTrees for evaluation
    bookTrees();
  } 

}

void mFvtxFindHoughTracks::init_run(PHCompositeNode*)
{

  //OK to call multiple places...self-managing
  TFvtxGlobalParCntrl::init_run();

	//get beam xy from SvxDB
	SvxBeamCenterPar *par = new SvxBeamCenterPar();
	if ( par ){
		par->fetchFromDB(TFvtxGlobalParCntrl::get_pdb_run_number());
		par->print();

		_beam_x = par->getBeamCenter(0);
		_beam_y = par->getBeamCenter(1);

		delete par;
	}

  // First find and store the phi limits for our phi slices by calling FvtxGeom
  // Slices are based off of arm 0, station 1, and are taken as sector+cage*24

  // clear _phiLimits vector just in case
  _phiLimits.clear();

  int islice = 0;
  for(int icg = 0; icg < NCAGE; icg++)
    {
      for( int isect = 0; isect < NSECTOR ; isect++ )
	{
		/*
	  FvtxSector* sector_ptr = 
	    FvtxGeom::get_arm(0)->
	    get_cage(icg)->
	    get_station(1)->
	    get_sector(isect);
	  
	  _phiLimits.push_back(std::make_pair(islice, sector_ptr->get_phi_begin()));
		*/

		// phi window from FvtxSector is about 3-wedge size,
		// so boundary for each slice shifted by 1 wedge
		// updated phiLimits based on FvtxColumn

		FvtxColumn* column_ptr0 = 
			FvtxGeom::get_arm(0)->
			get_cage(icg)->
			get_station(0)->
			get_sector(isect)->
			get_column(0);

		FvtxColumn* column_ptr1 = 
			FvtxGeom::get_arm(0)->
			get_cage(icg)->
			get_station(0)->
			get_sector(isect)->
			get_column(1);

		// get the center between two columns in a slice
		float phi_center = (fabs(column_ptr0->get_phi_begin()-column_ptr1->get_phi_end())<0.001) ? column_ptr0->get_phi_begin() : column_ptr0->get_phi_end();
		// [-pi,pi]->[0,2pi]
		if ( phi_center<0 ) phi_center += 2*M_PI;

		// upper boundary is phi_center + 1 column
	  _phiLimits.push_back(std::make_pair(islice, FVTXOO::angle_normalize(phi_center+M_PI/48.)));

	  //start from 0
	  islice++;
	}      
    }
  
  // Sort the phiLimit pairs by increasing phi using the Pair2ndComp functor
  std::sort(_phiLimits.begin(), _phiLimits.end(), Pair2ndComp<int,double>());  

  if(doDebug())
    {
      //Print the limits in order
      for(std::vector<PairIntDbl>::iterator it = _phiLimits.begin(); it != _phiLimits.end(); ++it)
	{
	  std::cout << "PhiBeginLimits: " << (*it).first << "  " << (*it).second << std::endl;
	}
    }

}


void mFvtxFindHoughTracks::end(PHCompositeNode*)
{
  //Clear out the last event hit vectors
  clear(); 
  
  _timer.print_stat();

  if(_doEval)
  {
    //Write out TTrees to TFile
    if(doDebug()) std::cout << "mFvtxFindHoughTracks::end - writing eval file" << std::endl;
    evalFile->cd();
    //_hough_cluster_tree->Write();
    _found_pairs_tree->Write();
    //_found_tracks_tree->Write();
    evalFile->Close();
  }

}

void mFvtxFindHoughTracks::bookTrees()
{
  //Initialize TTrees for evaluation
  _hough_cluster_tree = new TTree("hough_clusters","hough_clusters");
  _hough_cluster_tree->Branch("event",&_ievent,"_ievent/I");
  _hough_cluster_tree->Branch("nHits",&_houghCT_nHits,"_houghCT_nHits/I");
  _hough_cluster_tree->Branch("nClusters",&_houghCT_nClusters,"_houghCT_nClusters/I");
  _hough_cluster_tree->Branch("slice",&_houghCT_slice,"_houghCT_slice/I");
  _hough_cluster_tree->Branch("nPairs",&_houghCT_nPairs,"_houghCT_nPairs/I");
  _hough_cluster_tree->Branch("pass",&_houghCT_pass,"_houghCT_pass/I");
  _hough_cluster_tree->Branch("arm",&_houghCT_arm,"_houghCT_arm/I");
  _hough_cluster_tree->Branch("clusID",&_houghCT_clusID);
  _hough_cluster_tree->Branch("hit1_x",&_houghCT_hit1_x);
  _hough_cluster_tree->Branch("hit1_y",&_houghCT_hit1_y);
  _hough_cluster_tree->Branch("hit1_z",&_houghCT_hit1_z);
  _hough_cluster_tree->Branch("hit1_r",&_houghCT_hit1_r);
  _hough_cluster_tree->Branch("hit1_phi",&_houghCT_hit1_phi);
  _hough_cluster_tree->Branch("hit2_x",&_houghCT_hit2_x);
  _hough_cluster_tree->Branch("hit2_y",&_houghCT_hit2_y);
  _hough_cluster_tree->Branch("hit2_z",&_houghCT_hit2_z);
  _hough_cluster_tree->Branch("hit2_r",&_houghCT_hit2_r);
  _hough_cluster_tree->Branch("hit2_phi",&_houghCT_hit2_phi);
  _hough_cluster_tree->Branch("pair_rInt",&_houghCT_pair_rInt);
  _hough_cluster_tree->Branch("pair_sinAlpha",&_houghCT_pair_sinAlpha);
  _hough_cluster_tree->Branch("nMCHitsFromSameTrk",&_houghCT_nMCHitsFromSameTrk);
  _hough_cluster_tree->Branch("nMCTrksInCluster",&_houghCT_nMCTrksInCluster);
  _hough_cluster_tree->Branch("nUniqueHits",&_houghCT_nUniqueHits);
  _hough_cluster_tree->Branch("pair_hasSVXCluster",&_houghCT_hasSVXCluster);
  

  _found_pairs_tree = new TTree("found_pairs","found_pairs");
  _found_pairs_tree->Branch("event",&_ievent,"_ievent/I");
  _found_pairs_tree->Branch("slice",&_foundPT_slice,"_foundPT_slice/I");
  _found_pairs_tree->Branch("pass",&_foundPT_pass,"_foundPT_pass/I");
  _found_pairs_tree->Branch("arm",&_foundPT_arm,"_foundPT_arm/I");
  _found_pairs_tree->Branch("use_precise_vtx",&_use_precise_vtx,"use_precise_vtx/O");
  _found_pairs_tree->Branch("hit1_x",&_foundPT_hit1_x);
  _found_pairs_tree->Branch("hit1_y",&_foundPT_hit1_y);
  _found_pairs_tree->Branch("hit1_z",&_foundPT_hit1_z);
  _found_pairs_tree->Branch("hit1_r",&_foundPT_hit1_r);
  _found_pairs_tree->Branch("hit1_phi",&_foundPT_hit1_phi);
  _found_pairs_tree->Branch("hit1_station",&_foundPT_hit1_station,"hit1_station/I");
  _found_pairs_tree->Branch("hit1_layer",&_foundPT_hit1_layer,"hit1_layer/I");
  _found_pairs_tree->Branch("hit2_x",&_foundPT_hit2_x);
  _found_pairs_tree->Branch("hit2_y",&_foundPT_hit2_y);
  _found_pairs_tree->Branch("hit2_z",&_foundPT_hit2_z);
  _found_pairs_tree->Branch("hit2_r",&_foundPT_hit2_r);
  _found_pairs_tree->Branch("hit2_phi",&_foundPT_hit2_phi);
  _found_pairs_tree->Branch("hit2_station",&_foundPT_hit2_station,"hit2_station/I");
  _found_pairs_tree->Branch("hit2_layer",&_foundPT_hit2_layer,"hit2_layer/I");
  _found_pairs_tree->Branch("pair_rInt",&_foundPT_pair_rInt);
  _found_pairs_tree->Branch("pair_sinAlpha",&_foundPT_pair_sinAlpha);
  _found_pairs_tree->Branch("pair_rInt_beam",&_foundPT_pair_rInt_beam);
  _found_pairs_tree->Branch("pair_sinAlpha_beam",&_foundPT_pair_sinAlpha_beam);
  _found_pairs_tree->Branch("pair_DcaZ",&_foundPT_pair_DcaZ);


  _found_tracks_tree = new TTree("found_tracks","found_tracks");
  _found_tracks_tree->Branch("track_id",&_foundTT_track_id,"_foundTT_track_id/I");
  _found_tracks_tree->Branch("arm",&_foundTT_arm,"_foundTT_arm/I");
  _found_tracks_tree->Branch("event",&_ievent,"_ievent/I");
  _found_tracks_tree->Branch("rInt",&_foundTT_pair_rInt);
  _found_tracks_tree->Branch("sinAlpha",&_foundTT_pair_sinAlpha);
  _found_tracks_tree->Branch("pair_id",&_foundTT_pair_id);

}

void mFvtxFindHoughTracks::loadExtVertex()
{
  //Reference z=0.0 is used by default
  _vertex_z = 0.0;
  _vertex = PHPoint();
  
  //Try load external vertex
  bool error( false );
  PHPoint vtx( TMutExtVtx::get().get_vtx( error ) );
  if( error && _mod_par->get_verbosity() >= FVTXOO::SOME )    
    {
      std::cerr << "mFvtxFindHoughTracks::loadExtVertex - wrong external vertex.\n";       
    }
  else
    {
      _vertex_z = vtx.getZ();
      _vertex = vtx;
    }
  if(_mod_par->get_verbosity() > FVTXOO::NONE)
    {
      std::cout << "_vertex_z in mFvtxFindHoughTracks = " << _vertex_z << std::endl;
    }

	//Use beam xy from SvxDB in init_run()
  //_beam_x = TFvtxGlobalParCntrl::get_float_par("beam_x_seed");
  //_beam_y = TFvtxGlobalParCntrl::get_float_par("beam_y_seed");

  return;
}

void mFvtxFindHoughTracks::loadPreciseVertex()
{

	_use_precise_vtx = false;

  //By default load in the BBC z vertex in case there isn't a better vertex
  _precise_z = _vertex_z;
  _precise_z2 = _vertex_z;

  _precise_vtx = _vtxout->get_Vertex("BBC");
  _precise_vtx2 = _vtxout->get_Vertex("BBC");
  
  PHPoint vtx        = PHPoint(_vtxout->get_Vertex("SVX_PRECISE"));
	//Can't have FVTX vertex at this moment 
  //PHPoint fvtx       = PHPoint(_vtxout->get_Vertex("FVTX"));
  //PHPoint fvtxSecond = PHPoint(_vtxout->get_Vertex("FVTX_SECOND"));

	if( !isnan(vtx.getZ()) && fabs(vtx.getZ()-_vertex_z)<5.0 )
  {
		_vertex_z = _precise_z = vtx.getZ();
		_vertex = _precise_vtx = vtx;
		_use_precise_vtx = true;
  }
	/*
  else if( !isnan(fvtx.getZ()) )
  {
    _precise_z = fvtx.getZ();
    _precise_vtx = fvtx;
  }
 
  if( !isnan(fvtxSecond.getZ()) )
  {
    _precise_z2 = fvtxSecond.getZ();
    _precise_vtx2 = fvtxSecond;
  }
	*/

  if( _mod_par->get_verbosity() >= FVTXOO::ALOT)
    {
      std::cout << "mFvtxFindHoughTracks::loadPreciseVertex - bbcz: " 
		<< _vertex_z << " _precise_z: " << _precise_z << " precise_z2: " 
		<< _precise_z2 << std::endl; 
    }
}

void mFvtxFindHoughTracks::checkUPCTrigger(PHCompositeNode* top_node)
{

	_isUPC = false;

	if(_triglvl1)
	{
		TriggerHelper myTH(top_node);
		if ( myTH.didLevel1TriggerFire("UltraPeriph") || 
				myTH.didLevel1TriggerFire("UltraPeriphMuon2DSouth") ||
				myTH.didLevel1TriggerFire("UltraPeriphMuon2DNorth") ||
				myTH.didLevel1TriggerFire("UltraPeriphMPC") ){
			_isUPC = true;
		}
	}

}

void mFvtxFindHoughTracks::clear()
{
  _hitId = 0;
  _trackId = 0;
  _reverseTracking = false;
  
  clearTrackVec();
  clearHitVecs();
}

void mFvtxFindHoughTracks::clearTrackVec()
{
  //Clear tracks that were made
  std::for_each(_trk_vec.begin(),_trk_vec.end(),Deleter<EVTrk*>());
  _trk_vec.clear();
}

void mFvtxFindHoughTracks::clearHitVecs()
{
  //Delete pointers to hits and clear vectors
  for( int i = 0; i < FVTXOO::MAX_ARM; i++ )  
    {
      for( int j = 0; j < NSLICES; j++ ) 
	{   
	  std::for_each(_hitsFvtx[i][j].begin(),
			_hitsFvtx[i][j].end(),
			Deleter<EVHit*>());
	  _hitsFvtx[i][j].clear();
	  _hitsFvtx_limit[i][j] = false;
	}
    }


  if( _use_svx_cluster )
    {
      for( int j = 0; j < NSLICES; j++ )
	{
	  std::for_each(_hitsSvx[j].begin(),
			_hitsSvx[j].end(),
			Deleter<EVHit*>());
	  _hitsSvx[j].clear(); 
	}
    }
  
  return;
}

PHBoolean mFvtxFindHoughTracks::event(PHCompositeNode* topNode)
{
  _timer.restart();
  _ievent++;

  //Clears the hough pairs from pairs vec
  //Important to do here because two hit tracks are
  //called after event() so we need to keep the vector 
  //of pairs until the next event
  delete _hough_pairs;
  _hough_pairs = new EVHoughPairVec(FVTXOO::MAX_ARM,NSLICES);

  //Clear hit vectors and tracks that have been made
  clear();

  //Grab nodes needed
  setInterfacePtrs(topNode);

  //Load vertex from TMutExtVtx
  loadExtVertex();
	//Load precise vertex
	loadPreciseVertex();
	//Check UPC trigger
	if( _mod_par->get_useHICuts() )
	{
		checkUPCTrigger(topNode);
	}

  //Create vectors of hits
  convertCoords();

  //Start making pairs and finding clusters then tracks
  findTracks(topNode);

  //Store tracks to FvtxTrackMap
  fillOutputNodes();

  //Two hit SVX tracks for vertex (experimental)
  if( _mod_par->get_allowTwoHitTracks() )
  {
    if( _mod_par->get_allowTwoHitSVXTracks() )
      {
	processTwoHitSVXTracks();
      }
    clearTrackVec();
  }
  
  if( _mod_par->get_verbosity() > FVTXOO::SOME || doDebug())
    {
      std::cout << "Num. tracks found: " << _trk_vec.size() << std::endl;
    }

  //Stop here to exclude time used for eval filling
  _timer.stop(); 
  
  return true;
}

void mFvtxFindHoughTracks::recalcPairDcaVals(EVPairVec v, int arm)
{
  //Recalculates DCA_r and DCA_z when there is a precise vertex
  for(EVPairVec::iterator pIt = v.begin(); pIt != v.end(); ++pIt)
    {
      (*pIt)->recalcDcaVals(_precise_vtx,_precise_vtx2,arm);
    }

}

bool mFvtxFindHoughTracks::processTwoHitSVXTracks()
{

  _twoHitMode = true;
  for( int arm = 0; arm < FVTXOO::MAX_ARM; arm++ )        
    {
      findTwoHitCandidates(arm,true);
    }
  
  fillOutputNodes();
  _twoHitMode = false;

  return true;
}

bool mFvtxFindHoughTracks::processTwoHitTracks()
{

  _timer.restart();  
  _twoHitMode = true;

  //Why different that TMutExtVtx?
  loadPreciseVertex();

  for( int arm = 0; arm < FVTXOO::MAX_ARM; arm++ )
  {
    if( _vertex_z != _precise_z )
      {
	if(doDebug()) std::cout << "mFvtxFindHoughTracks::processTwoHitTracks - SizeArmVec: " << _hough_pairs->SizeArmVec(arm) << std::endl;
	unsigned int sizeArm = _hough_pairs->SizeArmVec(arm);
	for(unsigned int i = 0; i < sizeArm; i++)
	  {
	    EVPairVec tmpVec = _hough_pairs->GetSliceVec(arm,i);
	    recalcPairDcaVals(tmpVec,arm);
	  }
      }
    
    findTwoHitCandidates(arm);
  }

  fillOutputNodes();
  clear();

  _twoHitMode = false;

  _timer.stop();

  return true;
}

//! Reset IOC and external interface pointers
void  mFvtxFindHoughTracks::setInterfacePtrs(PHCompositeNode* top_node)
{

  //Input node(s)
  _mod_par   = TMutNode<mFvtxFindTrackPar>::find_node(top_node, "mFvtxFindTrackPar" );
  _coord_map = TMutNode<TFvtxCoordMap>::find_node(top_node, "TFvtxCoordMap" );
  if( _use_svx_cluster ) 
    {
      _svx_map = TMutNode<TFvtxSvxClusterMap>::find_node(top_node, "TFvtxSvxClusterMap" );
    }
  _vtxout = TMutNode<VtxOut>::find_io_node(top_node, "VtxOut" );
  _triglvl1 = findNode::getClass<TrigLvl1>(top_node, "TrigLvl1" );;

  //Output node(s)
  _trk_map  = TMutNode<TFvtxTrkMap>::find_node(top_node, "TFvtxTrkMap" );
  _ctrk_map = TMutNode<TFvtxCompactTrkMap>::find_node(top_node, "TFvtxCompactTrkMap");

}

void mFvtxFindHoughTracks::convertCoords()
{
  //Clear the hit vectors just in case
  clearHitVecs();

  TFvtxCoordMap::iterator coord_iter = _coord_map->range();
  static bool once = true;  
  while( TFvtxCoordMap::pointer ptr = coord_iter.next() )
    {
      int arm = ptr->get()->get_arm();
      int cage = ptr->get()->get_cage();
      int station = ptr->get()->get_station();
      int sector = ptr->get()->get_sector();
      int column = ptr->get()->get_column();
      
      PHPoint cartPnt = ptr->get()->get_coord_midpoint();
      PHCylPoint pnt = cartPnt;
      double r = pnt.getR();
      double z = pnt.getZ();
      double phi = FVTXOO::angle_normalize(pnt.getPhi().getPhi());
      
      //R/Phi locaiton of the hit relative to the beam center
			const double r_beam = sqrt((cartPnt.getY() - _beam_y)*(cartPnt.getY() - _beam_y) + (cartPnt.getX() - _beam_x)*(cartPnt.getX() - _beam_x));
      const double phi_beam = FVTXOO::angle_normalize(atan2(cartPnt.getY() - _beam_y, cartPnt.getX() - _beam_x));
      if (once)
	{
	  once = false;
	  std::cout << "mFvtxFindHoughTracks::convertCoords"
		    << " - Use beam center (" << _beam_x << "," << _beam_y
		    << ") for pattern recognition on FVTX hits. First hit"
		    << " phi = " << phi << ", phi_beam = " << phi_beam
		    << std::endl;
	}
      
      
      double phiStart, phiEnd;
      boost::tie(phiStart,phiEnd) = getPhiStartEnd(arm,cage,station,sector,column);
      
      phiStart = FVTXOO::angle_normalize(phiStart + (phi_beam - phi));
      phiEnd = FVTXOO::angle_normalize(phiEnd + (phi_beam - phi));
      
      EVHit_ptr h = new EVHit();
      h->id = _hitId++;
      h->layer = -1; //Denotes that this hit did NOT come from the VTX
      h->svx_hitnum = -1; //Denotes that this hit did NOT come from the VTX
      h->arm = arm;
      h->cage = cage;
      h->station = station;
      h->sector = sector;
      h->column = column;
      h->halfwedge = FVTXGEOM::get_halfwedge_id(arm,cage,station,sector,column);
      h->x_hit = cartPnt.getX();
      h->y_hit = cartPnt.getY();
      h->x_hit_beam = cartPnt.getX() - _beam_x;
      h->y_hit_beam = cartPnt.getY() - _beam_y;
      h->z_hit = z;
      h->r_hit = r;
			h->phi_hit = phi;
			h->r_hit_beam = r_beam;
			h->phi_hit_beam = phi_beam;
      h->phi_start = phiStart;
      h->phi_end = phiEnd;
      h->ntracks = 0;
      h->setCoordPtr(ptr);
      
      //If there is an association to the MCHit, assign it here
      TFvtxMCHitMap::key_iterator it = ptr->get()->get_associated<TFvtxMCHit>();
      if( TFvtxMCHitMap::pointer p = it.current() )
	{
	  h->setMCHitPtr(p);
	}
      
      h->coordnum = ptr->get()->get_index();

      //Make sure we dont go over the max N hits allowed (memory?)
      if (_hitsFvtx[arm][h->getSlice()].size() < _mod_par->get_max_n_hit())
	{
	  //Add to hits vector
	  _hitsFvtx[arm][h->getSlice()].push_back(h);
	}
      else
	{
	  //If we went over the max N hits, set the limit bool
	  if (!_hitsFvtx_limit[arm][h->getSlice()])
	    {
	      _hitsFvtx_limit[arm][h->getSlice()] = true;
	      std::cout << "mFvtxFindHoughTracks::convertCoords - ERROR - "
			<< "Cluster count reached limit of " << _mod_par->get_max_n_hit() 
			<< " for arm " << arm << " slice " << h->getSlice()
			<< std::endl;
	    }
	  
	  delete h;
	}
      
    }// while( TFvtxCoordMap::pointer ptr = coord_iter.next() )
  
  
  if(doDebug())
    {
      std::cout << "mFvtxFindHoughTracks::convertCoords - dump hitsFvtx begin" << std::endl;
      for(unsigned int i = 0; i < 2; i++)
	{
	  for(unsigned int j = 0; j < NSLICES; j++)
	    {
	      std::cout << "Slice: " << j << std::endl;
	      for(EVHitVec::iterator it = _hitsFvtx[i][j].begin(); it != _hitsFvtx[i][j].end(); ++it)
		{
		  (*it)->print(std::cout);
		}
	    }
	}
      std::cout << "mFvtxFindHoughTracks::convertCoords - dump hitsFvtx end" << std::endl;
    }

  //Optionally process the VTX hits as well  
  if( _use_svx_cluster )
    {
      convertSvxClusters();
    }

}

void mFvtxFindHoughTracks::convertSvxClusters()
{
  // XY error for VTX clusters, should relocate this somewhere
  //static const double deltaXY = 50.0e-4/sqrt(12.0);
  
  if( _mod_par->get_verbosity() >= FVTXOO::SOME )
    {
      std::cout << "mFvtxFindHoughTracks::convertCoords - Converting VTX coords" << std::endl;
    }
  
  static bool once = true;    
  TFvtxSvxClusterMap::iterator cluster_iter = _svx_map->range();
  while ( TFvtxSvxClusterMap::pointer ptr = cluster_iter.next() )
    {
      int layer = ptr->get()->get_cluster()->get_layer();
      
      // Only use hits from the requested VTX layers
      if( !(layer < _n_svx_layers) ) continue;
      
      // Grab the cluster from the map
      const  SvxCluster *svx_cluster = ptr->get()->get_cluster();
      // Calculate all the values we want to store in the EVHit format
      double x = svx_cluster->get_xyz_global(0);
      double y = svx_cluster->get_xyz_global(1);
      double z = svx_cluster->get_xyz_global(2);
      
      // Manually exclude the more central hits in the strip pixels
      // No MuTr matches are possible inside of 10cm, so don't include those hits
      if( layer > 1 && fabs(z) < 10.0 ) continue;
      
      double r = sqrt(x*x+y*y);
      double phi = FVTXOO::angle_normalize(atan2(y,x));
      
      // r/phi locaiton of the hit relative to the beam center
			double r_beam = sqrt((x-_beam_x)*(x-_beam_x)+(y-_beam_y)*(y-_beam_y));
      double phi_beam = FVTXOO::angle_normalize(atan2(y - _beam_y,x - _beam_x));
      
      
      
      if (once)
        {
          once = false;
          std::cout << "mFvtxFindHoughTracks::convertSvxClusters"
		    << " - Use beam center (" << _beam_x << "," << _beam_y
		    << ") for pattern recognition on VTX hits. First hit"
		    <<" phi = "<<phi<<", phi_beam = "<<phi_beam<<std:: endl;
        }
      
      // Using the method found in SvxClusterContainer to get the errors in x,y
      // Then propagate these to calculate the error in phi => deltaPhi = deltaXY/r
      // 50.0e-4 (cm) is the number used by VTX group for xy resolution in pixels   
      //double deltaPhi = deltaXY/r; 

			// Due to a tiny deltaPhi window above VTX hits right next to the FVTX column boundary
			// will be excluded in phi window overlap check
			// Set deltaPhi as the half-column size
			// This phi window is only used for the phi window overlap check
			double deltaPhi = (2*M_PI)/(NSLICES*4);
      
      // Create a new EVHit and fill the VTX applicable variables
      EVHit_ptr h = new EVHit();
      
      h->id = _hitId++;
      h->layer = layer;
      h->x_hit = x;
      h->y_hit = y;
			h->x_hit_beam = x - _beam_x;
			h->y_hit_beam = y - _beam_y;
      h->z_hit = z;
      h->r_hit = r;
			h->phi_hit = phi;
			h->r_hit_beam = r_beam;
			h->phi_hit_beam = phi_beam;
      h->phi_start = phi_beam - deltaPhi;
      h->phi_end = phi_beam + deltaPhi;
      h->ntracks = 0;
      h->svx_hitnum = svx_cluster->get_hitID();
      h->setSvxPtr(ptr);
      
      // Set FVTX variables to -1 just in case someone decides to try to use them
      h->coordnum = -1;
      h->arm = -1;
      h->cage = -1;
      h->station = -1;
      h->sector = -1;
      h->column = -1;
      h->halfwedge = -1;
      
      // Use lower_bound to find the first element not less than phi using
      // our pair compare functor      
      PairVecItr sliceItr = std::lower_bound(_phiLimits.begin(),
					     _phiLimits.end(),
					     //phi_beam,
					     phi, // Use phi from global coordinate for comparison with the FVTX geometry
					     Pair2ndComp<int,double>()
					     );       
      
      int slice = ( sliceItr != _phiLimits.end() ) ? (*sliceItr).first : _phiLimits.front().first;
      
      h->slice = slice;
      
      
      //Make sure we dont go over the max N hits allowed (memory?) 
      if (_hitsFvtx[0][slice].size() + _hitsSvx[slice].size()>=_mod_par->get_max_n_hit())
	{
	  if (!_hitsFvtx_limit[0][h->getSlice()])
	    {
	      //If we went over the max N hits, set the limit bool
	      _hitsFvtx_limit[0][h->getSlice()] = true;
	      std::cout << "mFvtxFindHoughTracks::convertSvxClusters - ERROR - "
			<< "Cluster count reached limit of " << _mod_par->get_max_n_hit() 
			<< "for arm " << 0 << " slice " << h->getSlice()
			<< std::endl;
	    }
	  delete h;
	}
      else if (_hitsFvtx[1][slice].size() + _hitsSvx[slice].size()>=_mod_par->get_max_n_hit())
	{
	  if (!_hitsFvtx_limit[1][h->getSlice()])
	    {
	      //If we went over the max N hits, set the limit bool
	      _hitsFvtx_limit[1][h->getSlice()] = true;
	      std::cout << "mFvtxFindHoughTracks::convertSvxClusters - ERROR - "
			<< "Cluster count reached limit of "<< _mod_par->get_max_n_hit() 
			<< " for arm " << 1 << " slice " << h->getSlice()
			<< std::endl;
	    }
	  delete h;
	}
      else
	{
	  //Add the constructed EVHit pointer to the container for SVX hits
	  _hitsSvx[slice].push_back(h);
	}
      
    }//while ( TFvtxSvxClusterMap::pointer ptr = cluster_iter.next() )
  
  
  if(doDebug())
    {
      std::cout << "mFvtxFindHoughTracks::convertSvxCoords - dump hitsSvx begin" << std::endl;
      for(unsigned int j = 0; j < NSLICES; j++)
	{
	  std::cout << "Slice: " << j << std::endl;
	  for(EVHitVec::iterator it = _hitsSvx[j].begin(); it != _hitsSvx[j].end(); ++it)
	    {
	      (*it)->print(std::cout);
	    }
	}
    
      std::cout << "mFvtxFindHoughTracks::convertSvxCoords - dump hitsSvx end" << std::endl;
    }
  
  
  
}

void mFvtxFindHoughTracks::findTracks(PHCompositeNode* top_node)
{
  //Start to find tracks
  //Make 2 passes 
  //First pass is with tight clustering requirements
  //Second pass is rClusCut*10 and alphaClusCut*10 to pick up looser tracks
  //Steps:
  //  1) Make pairs and convert to hough space (fillHoughVec)
  //  2) Make clusters in hough space which then get checked and become tracks (graphCluster)
  
  int nTracks = 0;
  int nPasses = 2;

  for( int arm = 0 ; arm < FVTXOO::MAX_ARM; arm++ )
  {
    
    //Only reconstruct tracks near good muons
    //to speed up production
    std::vector<bool> theSlicesToReco;
    if(_doQuickMuonReco)
      {
	theSlicesToReco = getGoodMuonCandidateSlices(top_node,arm);
	if(doDebug())
	  {
	    std::cout << "mFvtxFindHoughTracks::findTracks - Quick Reco Slices with Muons for Arm " << arm << std::endl;
	    for(unsigned int i = 0; i < theSlicesToReco.size(); i++)
	      {
		std::cout << i << ":  " << theSlicesToReco[i] << std::endl;
	      }
	  }
      }

    //For evaluation
    _houghCT_arm = arm;
    _foundPT_arm = arm;
    _foundTT_arm = arm;

    //Make pairs and convert to hough space
    //Fills vector of pointers managed by EVHoughPairVec (_hough_pairs)
    int nHits = fillHoughVec(arm,theSlicesToReco);

    //In HI use a dcaZ cut when making clusters from pairs
    double dcaZCut = ( _mod_par->get_useHICuts() && nHits > 300 ) ? 4.0 : -1.0;    


    //Pass loop
    for( int i_pass = 0 ; i_pass < nPasses ; i_pass++ )         
      {
	//For evaluation
	_houghCT_pass++;
	_foundPT_pass++;
	
	//Adjacency cuts
	double theRClusCut = _mod_par->get_rClusCut();
	double theSinAlphaClusCut = _mod_par->get_alphaClusCut();

	//Form clusters and tracks
	nTracks += graphCluster(arm,dcaZCut,
				theRClusCut*pow(10,i_pass),
				theSinAlphaClusCut*pow(10,i_pass),
				theSlicesToReco);

      }
    _houghCT_pass = 0;
    _foundPT_pass = 0;
  }


}

void mFvtxFindHoughTracks::applyVertexCut(EVHitVec& v,
                                     const int arm)
{
  //Removes hits from hit vec if they are 2cm or further behind the vertex
  if( arm == 0 )
    {    
      v.erase(std::remove_if(v.begin(), v.end(),
			     bind(&EVHit::get_z,_1) > _vertex_z + 2.0), v.end());
    }
  else{
    v.erase(std::remove_if(v.begin(), v.end(),
                           bind(&EVHit::get_z,_1) < _vertex_z - 2.0), v.end());  
  }

}

int mFvtxFindHoughTracks::fillHoughVec(const int arm, std::vector<bool> quickMuonRecoSlices)
{
  //Loops over 48 slices (each slice = 3 wedges) and makes pairs
  //Pair requirements are tightened in HI

  int nHits = 0;
  bool isHI = _mod_par->get_useHICuts();

  //Slice loop
  for( int slice = 0; slice < NSLICES; slice++ ) 
  {    
    
    //Only reconstruct slices with a good muon
    if(_doQuickMuonReco)
      {
	if(quickMuonRecoSlices[slice] == false) continue;
      }

    //Pick 3 wedges
    //From slice 0 (47,0,1) --> 47 (46,47,0)
    int curOffset = slice;
    int prevOffset = slice - 1;
    if(slice == 0) prevOffset = NSLICES-1;
    int nextOffset = slice + 1;
    if(slice == NSLICES-1) nextOffset = 0;

    //Collect the FVTX and SVX hits together for making transform pairs
    //Add 3 wedges to baseHits
    EVHitVec baseHits;
    //Slice Prev
    baseHits.insert(baseHits.end(), _hitsFvtx[arm][prevOffset].begin(), _hitsFvtx[arm][prevOffset].end());
    baseHits.insert(baseHits.end(), _hitsSvx[prevOffset].begin(),       _hitsSvx[prevOffset].end());
    //Keep track of total number of hits in each wedge 
    nHits += baseHits.size();
    //Slice Curr
    baseHits.insert(baseHits.end(), _hitsFvtx[arm][curOffset].begin(), _hitsFvtx[arm][curOffset].end());
    baseHits.insert(baseHits.end(), _hitsSvx[curOffset].begin(),       _hitsSvx[curOffset].end());
    //Slice Next
    baseHits.insert(baseHits.end(), _hitsFvtx[arm][nextOffset].begin(), _hitsFvtx[arm][nextOffset].end());
    baseHits.insert(baseHits.end(), _hitsSvx[nextOffset].begin(),       _hitsSvx[nextOffset].end());

    //Remove hits behind the vertex
		if( isHI && !_isUPC ) applyVertexCut(baseHits, arm);    

    //If we have less than 2 hits, we cant make pairs
    if( baseHits.size() < 2 ) continue;	

    //sort because we're going to make pairs in Z while running in Z
    //if(arm==0){ std::sort(baseHits.begin(),baseHits.end(), EVHitGreaterZ());}
    //else{ std::sort(baseHits.begin(),baseHits.end(), EVHitLessZ());}

    //How far apart in stations are hits from the same pair allowed
    int diffStationAllowed = 2;
    if( isHI ) diffStationAllowed = 1;

    unsigned int sizeBaseHits = baseHits.size();
    for(unsigned int i = 0; i < sizeBaseHits-1; i++)
      {
	EVHit* h = baseHits[i];
	//dZ cut for hits in pairs only if its not 2 hit svx tracking
	double dZ = ( _mod_par->get_allowTwoHitSVXTracks() && h->getLayer() == 0) ? 0.0 : _dZ;	

	for(unsigned int j = i+1; j < sizeBaseHits; j++)
	  {
	    EVHit* h2 = baseHits[j];

	    //Check that hits have some minimum distance apart in Z and R
	    if(fabs(h->get_r() - h2->get_r()) < _dR){continue;}
	    if(fabs(h->get_z() - h2->get_z()) < dZ){continue;}

	    //Check that the hits are either in the same or adjacent wedges
	    if(fabs(h->getSlice() - h2->getSlice()) > 1){continue;}
	    
	    //Check that they're only 2 (1) stations apart for pp (HI)
			//Pairs of FVTX-FVTX
			if(h->getStation() >= 0 && h2->getStation() >= 0)
			{
				if(abs(h->getStation() - h2->getStation()) > diffStationAllowed){continue;}
				if(h->getStation() == h2->getStation()){continue;}
			}
			//Pairs of VTX-VTX 
	    if( (h->getLayer() == h2->getLayer()) && h->getStation() < 0 && h2->getStation() < 0){continue;}

	    //if pair passed, then keep - add to hough vec (does a duplicate check)
	    _hough_pairs->Add(arm,h->getSlice(),h,h2,_vertex_z, true /*dupe check*/, isHI, _isUPC, _use_precise_vtx);
	  }

      }


  }

  if(doDebug() || _doEval)
    {
      _foundPT_slice = 0;
      for( unsigned int islice = 0; islice < NSLICES; islice++ )
	{
	  if(doDebug())
	    {
	      std::cout << "mFvtxFindHoughTracks::fillHoughVec - dump hough vec - slice: " << islice << "  size: " << _hough_pairs->SizeSliceVec(arm,islice) << std::endl;
	      for(unsigned int k = 0; k < _hough_pairs->SizeSliceVec(arm,islice); k++) _hough_pairs->Get(arm,islice,k)->print(std::cout);
	    }
	  
	  if(_doEval)
	    {
	      EVPairVec theVec = _hough_pairs->GetSliceVec(arm,islice);
	      for(EVPairVec::iterator it = theVec.begin(); it != theVec.end(); ++it)
		{
		  _foundPT_slice = islice;
		  _foundPT_hit1_x = (*it)->h1->x_hit;
		  _foundPT_hit1_y = (*it)->h1->y_hit;
		  _foundPT_hit1_z = (*it)->h1->z_hit;
		  _foundPT_hit1_r = (*it)->h1->r_hit;
		  _foundPT_hit1_phi = (*it)->h1->phi_hit_beam;
			_foundPT_hit1_station = (*it)->h1->station;
			_foundPT_hit1_layer = (*it)->h1->layer;
		  _foundPT_hit2_x = (*it)->h2->x_hit;
		  _foundPT_hit2_y = (*it)->h2->y_hit;
		  _foundPT_hit2_z = (*it)->h2->z_hit;
		  _foundPT_hit2_r = (*it)->h2->r_hit;
			_foundPT_hit2_phi = (*it)->h2->phi_hit_beam;
			_foundPT_hit2_station = (*it)->h2->station;
			_foundPT_hit2_layer = (*it)->h2->layer;
		  _foundPT_pair_rInt = (*it)->rInt;
		  _foundPT_pair_sinAlpha = (*it)->sinAlpha;
		  _foundPT_pair_rInt_beam = (*it)->rInt_beam;
		  _foundPT_pair_sinAlpha_beam = (*it)->sinAlpha_beam;
			_foundPT_pair_DcaZ = (*it)->dcaZ;
		  _found_pairs_tree->Fill();
		}
	    }
	  
	}
  
  
      if(doDebug()) std::cout << "mFvtxFindHoughTracks::fillHoughVec - end " << std::endl;

    } 


  return nHits;
}

int mFvtxFindHoughTracks::findTwoHitCandidates(const int arm, const bool twoHitSVX)
{

  // Go through all of the pairs and find the ones that pass a
  // dcaZ, dcaR, and are unused to store these as two hit tracks

  _unusedPairs.clear();

  double the2hitMaxDcaZ = _mod_par->get_twoHitMaxDCAz();
  //double the2hitMaxDcaR = _mod_par->get_twoHitMaxDCAr();

  if( !twoHitSVX )
    {
      unsigned int sizeArm = _hough_pairs->SizeArmVec(arm);
      for(unsigned int i = 0; i < sizeArm; i++)
	{
	  unsigned int sizeSlice = _hough_pairs->SizeSliceVec(arm,i);
	  for(unsigned int j = 0; j < sizeSlice; j++)
	    {
	      EVHoughPair *tmpPair = _hough_pairs->Get(arm,i,j);
	      //If pair wasnt used in normal tracking
	      if(!tmpPair->hitsUnused()) continue;
	      //If 2 hit DCA_z is set, check it
	      if(the2hitMaxDcaZ >= 0.0)
		{
		  if(fabs(tmpPair->getDcaZ()) > the2hitMaxDcaZ) continue;
		}
	      //if(maxDcaR >= 0.0)
	      //{
	      //  if((*it)->getDcaR()> the2hitMaxDcaR) continue;
	      //}
	      _unusedPairs.push_back(tmpPair);
	    }
	}
    }
  else
    {
      //For 2 hit svx tracks
      unsigned int sizeArm = _hough_pairs->SizeArmVec(arm);
      for(unsigned int i = 0; i < sizeArm; i++)
	{
	  unsigned int sizeSlice = _hough_pairs->SizeSliceVec(arm,i);
	  for(unsigned int j = 0; j < sizeSlice; j++)
	    {
	      EVHoughPair *tmpPair = _hough_pairs->Get(arm,i,j);
	      //If pair wasnt used in normal tracking
	      if(!tmpPair->hitsUnused()) continue;
	      _unusedPairs.push_back(tmpPair);
	    }
	}
      
      //Make sure they are pixel pairs
      _unusedPairs.erase(std::remove_if(_unusedPairs.begin(), _unusedPairs.end(),
					NotPixelPair()), _unusedPairs.end());
      
    }

  if(doDebug())
    { 
      std::cout << "mFvtxFindHoughTracks::findTwoHitCandidates - pairs found: " << std::endl;
      for(EVPairVec::iterator pair = _unusedPairs.begin(); pair != _unusedPairs.end(); ++pair)
	{
	  (*pair)->print(std::cout);
	}
    } 
  
  
  // Duplicate check done when adding to houghPairs... not needed
  //std::sort(_unusedPairs.begin(), _unusedPairs.end(), EVHoughLessR());
  //EVPairVec::iterator theUniIt = std::unique(_unusedPairs.begin(), _unusedPairs.end(), EVHoughEqualRAlpha());
  //_unusedPairs.resize( std::distance(_unusedPairs.begin(),theUniIt) );
  
  // Optionally filter the hit pairs to remove tracks that are too similar --- derp?
  if( _mod_par->get_filterTwoHitTracks() )
    {
      _unusedPairs.erase(std::unique(_unusedPairs.begin(),
				     _unusedPairs.end(),
				     bind(EVHoughSimR(),_1,_2) && 
				     bind(EVHoughSimAlpha(),_1,_2)),
			 _unusedPairs.end());
    }
  
  //Make tracks from the pairs
  std::transform(_unusedPairs.begin(),_unusedPairs.end(),
                 std::back_inserter(_trk_vec),
                 bind(boost::factory<EVTrk_ptr>(),arm,_1));
  
  
  return _unusedPairs.size();
}

void mFvtxFindHoughTracks::fillUnusedPairs(const int arm,
					   const int curOffset,
					   const double maxDcaZ)
{

  //Fill _unusedPairs with pairs from _hough_pairs for making clusters from

  _unusedPairs.clear();
	_candidateHits.clear();

  //Three wedge slices 
  int prevOffset = curOffset - 1;
  if(curOffset == 0) prevOffset = NSLICES - 1;
  int nextOffset = curOffset + 1;  
  if(curOffset == NSLICES - 1) nextOffset = 0;

  //Fill unusedpairs vec
	int theSlices[3] = {curOffset,prevOffset,nextOffset};

	//First, scan the middle sector 
	for( int iSlice = 0; iSlice < 1; iSlice++)
	{
		int theOffset = theSlices[iSlice];
		unsigned int sizeSlice = _hough_pairs->SizeSliceVec(arm,theOffset);
		for(unsigned int i = 0; i < sizeSlice; i++)
		{
			EVHoughPair *tmpPair = _hough_pairs->Get(arm,theOffset,i);
			//Check whether hits from pairs were used yet
			if( !tmpPair->hitsUnused() ){ continue;}
			//Check for DCA_z in HI events
			//Checking DCA_z is moved to the step of making pairs
			//if(maxDcaZ >= 0 && tmpPair->getDcaZ() > maxDcaZ){ continue;}
			//if(maxDcaR >= 0 && (*it)->getDcaR() > maxDcaR) continue;

			_unusedPairs.push_back(tmpPair);
			_candidateHits.push_back(tmpPair->getFirstHitPtr());
			_candidateHits.push_back(tmpPair->getSecondHitPtr());
		}
	}

	if ( _candidateHits.size()==0 ) return;

	//Second, scan the neighbor sectors
  for( int iSlice = 1; iSlice < 3; iSlice++)
    {
      int theOffset = theSlices[iSlice];
      unsigned int sizeSlice = _hough_pairs->SizeSliceVec(arm,theOffset);
      for(unsigned int i = 0; i < sizeSlice; i++)
	{
	  EVHoughPair *tmpPair = _hough_pairs->Get(arm,theOffset,i);
	  //Check whether hits from pairs were used yet
	  if( !tmpPair->hitsUnused() ){ continue;}
	  //Check for DCA_z in HI events
		//Checking DCA_z is moved to the step of making pairs
	  //if(maxDcaZ >= 0 && tmpPair->getDcaZ() > maxDcaZ){ continue;}
	  //if(maxDcaR >= 0 && (*it)->getDcaR() > maxDcaR) continue;

		if (_mod_par->get_useHICuts())
		{
			//In case of HI, dphi cut is already applied
			_unusedPairs.push_back(tmpPair);
		}
		else
		{
			EVHit *tmp_h1 = tmpPair->getFirstHitPtr();
			EVHit *tmp_h2 = tmpPair->getSecondHitPtr();

			int h1_overlap =  std::count_if(_candidateHits.begin(),_candidateHits.end(),
					boost::bind<bool>(FVTXOO::AngleOverlap<double>(),
						boost::bind(&EVHit::getPhiStart,_1),
						boost::bind(&EVHit::getPhiEnd,_1),
						tmp_h1->getPhiStart(),
						tmp_h1->getPhiEnd()) &&
					boost::bind(std::not_equal_to<EVHit_ptr>(),tmp_h1,_1));

			int h2_overlap =  std::count_if(_candidateHits.begin(),_candidateHits.end(),
					boost::bind<bool>(FVTXOO::AngleOverlap<double>(),
						boost::bind(&EVHit::getPhiStart,_1),
						boost::bind(&EVHit::getPhiEnd,_1),
						tmp_h2->getPhiStart(),
						tmp_h2->getPhiEnd()) &&
					boost::bind(std::not_equal_to<EVHit_ptr>(),tmp_h2,_1));

			if ( h1_overlap>0 || h2_overlap>0 )
				_unusedPairs.push_back(tmpPair);
		}

	}
		}

  //Remove duplicates --- Duplicate check done earlier... no longer necessary
  //std::cout << "NPAIRS BEFORE: " << _unusedPairs.size() << std::endl;
  //std::sort(_unusedPairs.begin(), _unusedPairs.end(), EVHoughLessR());
  //EVPairVec::iterator theUniIt = std::unique(_unusedPairs.begin(), _unusedPairs.end(), EVHoughEqualRAlpha());
  //_unusedPairs.resize( std::distance(_unusedPairs.begin(),theUniIt) );
  //std::cout << "NPAIRS AFTER: " << _unusedPairs.size() << std::endl;
  

  if(doDebug()) 
    {
      std::cout << "mFvtxFindHoughTracks::fillUnusedPairs - dump unused pairs " << std::endl;
      for(EVPairVec::iterator it = _unusedPairs.begin(); it != _unusedPairs.end(); ++it)
	{ 
	  (*it)->print(std::cout);
	}

      std::cout << "mFvtxFindHoughTracks::fillUnusedPairs - end " << std::endl;
    }

}


void mFvtxFindHoughTracks::findCandidateHitsInRoad(EVPairVec& passedVec,
						   const int curOffset,
						   const double rClusCut,
						   const double alphaClusCut)
{
  //Fill _candidateHits vector with hits from a cluster of pairs found in hough space
  _candidateHits.clear();

  if(_doEval)
    {
      _foundTT_pair_rInt->clear();
      _foundTT_pair_sinAlpha->clear();
      _foundTT_pair_id->clear();
      for(EVPairVec::iterator theIt = passedVec.begin(); theIt != passedVec.end(); ++theIt)
	{
	  bool found = false;
	  for(std::vector<int>::iterator dupIt = _foundTT_pair_id->begin(); dupIt != _foundTT_pair_id->end(); ++dupIt)
	    {
	      if((*theIt)->id == (*dupIt)){ found = true; break;}
	    }
	  if(!found)
	    {
	      _foundTT_pair_rInt->push_back((*theIt)->rInt);
	      _foundTT_pair_sinAlpha->push_back((*theIt)->sinAlpha);
	      _foundTT_pair_id->push_back((*theIt)->id);
	    }
	}
    }

  for(EVPairVec::iterator theIt = passedVec.begin(); theIt != passedVec.end(); ++theIt)
    {
			if( (*theIt)->hitsUnused() ){
				_candidateHits.push_back((*theIt)->getFirstHitPtr());
				_candidateHits.push_back((*theIt)->getSecondHitPtr());
			}
    }      

}

EVTrk* mFvtxFindHoughTracks::makeEVTrkFromCandidates(const int arm)
{

  //Sort in R-Phi 
  std::sort(_candidateHits.begin(),_candidateHits.end(),EVHitLessR());
  
  //Erase the duplicated pointers from the hit vector
  //This happens because we are adding pairs of hits which may overlap
  _candidateHits.erase(std::unique(_candidateHits.begin(),_candidateHits.end(),EVHitEqualRPhi()),
                       _candidateHits.end());

  //Erase any hits from the track that do not have a phi overlap in a 
  // +/- _dPhi window to improve track purity
  //Get rid of?
  _candidateHits.erase(std::remove_if(_candidateHits.begin(), _candidateHits.end(), 
                                      bind(numOverlapHits(),
                                           _1,
                                           _candidateHits,
                                           _dPhi) == 0 ), 
                       _candidateHits.end());

  //If there are less than three hits left then we don't really have a track
  if( _candidateHits.size() < 3 ){  return NULL; }

  //Sort in Z to check for Z planes
  std::sort(_candidateHits.begin(),_candidateHits.end(),EVHitLessZ());
  EVHitVec _candidateCopy(_candidateHits.begin(),_candidateHits.end());
  unsigned int nZPlanes = std::distance(_candidateCopy.begin(),	
					std::unique(_candidateCopy.begin(),_candidateCopy.end(), EVHitEqualZ())
					);
  
  if(nZPlanes < 3) return NULL;

	std::set<int> set_station;
	set_station.clear();
	for(std::vector<EVHit_ptr>::iterator it1 = _candidateHits.begin(); it1 != _candidateHits.end(); ++it1)
	{
		if ((*it1)->getStation() < 0){
			if ((*it1)->getLayer() == 0)
				set_station.insert(4);
			else
				set_station.insert(5);
		}else{
			set_station.insert((*it1)->getStation());
		}
	}

	if (set_station.size() < 3) return NULL;

  //Check if we have 2 hits in the same Z, but different phi
  //If we do, then we form a median R-Phi and kill the outlier in R-Phi space
  if(nZPlanes < _candidateHits.size())
    {
      std::vector<double> thePhis, theRs;
      for(std::vector<EVHit_ptr>::iterator kk = _candidateHits.begin(); kk != _candidateHits.end(); ++kk)
	{
	  thePhis.push_back((*kk)->get_phi());
	  theRs.push_back((*kk)->get_r());
	}
      std::sort(thePhis.begin(),thePhis.end());
      std::sort(theRs.begin(),theRs.end());
      
      double medianPhi = 0;
      double medianR = 0;
      size_t theSize = _candidateHits.size();
      if (theSize % 2 == 0)
	{
	  medianPhi = (thePhis[theSize / 2 - 1] + thePhis[theSize / 2]) / 2;
	  medianR = (theRs[theSize / 2 - 1] + theRs[theSize / 2]) / 2;
	}
      else
	{
	  medianPhi = thePhis[theSize / 2];
	  medianR = theRs[theSize / 2];
	}
      

      for(std::vector<EVHit_ptr>::iterator it1 = _candidateHits.begin(); it1 != _candidateHits.end(); ++it1)
	{
	  for(std::vector<EVHit_ptr>::iterator it2 = it1+1; it2 != _candidateHits.end();)
	    {
	      if( (*it1)->get_z() == (*it2)->get_z() )
		{
		  double diffPhi1 = (*it1)->get_phi()-medianPhi;
		  double diffR1 = (*it1)->get_r()-medianR;
		  double diff1 = sqrt( diffPhi1*diffPhi1 + diffR1*diffR1);
		  
		  double diffPhi2 = (*it2)->get_phi()-medianPhi;
		  double diffR2 = (*it2)->get_r()-medianR;
		  double diff2 = sqrt( diffPhi2*diffPhi2 + diffR2*diffR2);
		  
		  if(diff1>diff2){ it1 = _candidateHits.erase(it1);}
		  else{ it2 = _candidateHits.erase(it2);}
		}
	      else{++it2;}
	    }
	}
    }


                  
  // If we have gotten this far then we have a candidate track and need to store it
  EVTrk_ptr t = new EVTrk();
  t->track = _trackId++;
  t->trk_arm = arm;
  t->vtx_z = _vertex_z;
  t->vtx = _vertex;

  //Add all of the accepted hits to the new track
  std::for_each(_candidateHits.begin(), _candidateHits.end(), bind(&EVTrk::addHit,t,_1));             

  if(_doEval)
    {
      _foundTT_track_id = t->track;
      //_found_tracks_tree->Fill();
      _foundTT_pair_rInt->clear();
      _foundTT_pair_sinAlpha->clear();
      _foundTT_pair_id->clear();
    }

  if(doDebug()) 
    {
      std::cout << "mFvtxFindHoughTracks::makeEVTrkFromCandidates - track candidate: " << std::endl;          
      for(std::vector<EVHit_ptr>::iterator it1 = _candidateHits.begin(); it1 != _candidateHits.end(); ++it1)
	{
	  (*it1)->print(std::cout);
	}
    }


  // Remove all of pointer to the pairs that were formed with hits used in this track from the _unusedPairs
  _unusedPairs.erase(std::remove_if(_unusedPairs.begin(), 
                                    _unusedPairs.end(),
                                    !bind(&EVHoughPair::hitsUnused,_1)),
                     _unusedPairs.end());

  return t;

}

void mFvtxFindHoughTracks::makeEVTrks(EVPairVec& pairVec,
				      const int arm,
				      const int curOffset,
				      const double rClusCut,
				      const double alphaClusCut)
{
  //Add hits to _candidateHits
  findCandidateHitsInRoad(pairVec, curOffset, rClusCut, alphaClusCut);
  
  //Get candidate track and add to _trk_vec
  EVTrk* trk_ptr = makeEVTrkFromCandidates(arm); 
  if( trk_ptr != NULL )
    {
      _trk_vec.push_back(trk_ptr);  
    }

}

int mFvtxFindHoughTracks::graphCluster(const int arm,
				       const double maxDcaZ, 
				       const double rClusCut,
				       const double alphaClusCut,
							 std::vector<bool> quickMuonRecoSlices)
{
  //Fill _unusedPairs from _hough_pairs in each slice
  //Cluster in hough space 
  //Make candidate tracks from these clusters

  int lastSize = _trk_vec.size();

  //For evaluation
  _houghCT_slice = 0;

  for( int curOffset = 0; curOffset < NSLICES; curOffset++ )
  {      

		if(_doQuickMuonReco)
		{
			if(quickMuonRecoSlices[curOffset] == false) continue;
		}

    //Fill _unusedPairs
    fillUnusedPairs(arm,curOffset, maxDcaZ);

    // If there are less than three pairs then we can't form a track
    if( _unusedPairs.size() < 2 ) continue;       

    //For evaluation
    _houghCT_slice = curOffset;
    
    //Form clusters and put them in _passedRoads
    _passedRoads.clear();
    findClustersHough(rClusCut,alphaClusCut);

    //Make candidate tracks from these clusters
    unsigned int roadSize = _passedRoads.size();

		std::sort(_passedRoads.begin(),_passedRoads.end(),EVPairVecSize());
    for(unsigned int i = 0; i < roadSize; i++)
      {
				if ( _passedRoads[i].size()<1 ) continue;
	makeEVTrks(_passedRoads[i], arm, curOffset, rClusCut, alphaClusCut);
      }

  }
  
  return _trk_vec.size() - lastSize;
}

std::pair<double,double>
mFvtxFindHoughTracks::getPhiStartEnd(int arm,
                                     int cage,
                                     int station,
                                     int sector,
                                     int column) const
{
  FvtxColumn* column_ptr = 
    FvtxGeom::get_arm(arm)->
    get_cage(cage)->
    get_station(station)->
    get_sector(sector)->
    get_column(column);
  
  double phi0 = 0.0;
  double phi1 = 0.0;
  boost::tie(phi0,phi1) = column_ptr->get_phi_window();

  if (arm == 0)
  {
    double _phiTmp = phi0;
    phi0 = FVTXOO::angle_normalize(phi1);
    phi1 = FVTXOO::angle_normalize(_phiTmp);
  }
  else
  {
    phi0 = FVTXOO::angle_normalize(phi0);
    phi1 = FVTXOO::angle_normalize(phi1);
  }
  
  return std::pair<double,double>(phi0,phi1);
}

void mFvtxFindHoughTracks::associateHits(EVTrk_ptr t)
{
  //Put tracks from _trk_vec into the FvtxTrkMap
  //Associate the hits used to this track

  TFvtxTrkMap::iterator trk_iter = _trk_map->insert_new(t->trk_arm); 
      
  for(EVHitVec::iterator hit_it = t->hits.begin(); hit_it != t->hits.end(); ++hit_it)
    {
      associatePtrToTrack((*hit_it),trk_iter);
    }

  // Set the trk_par for this track
  setTrkPar(trk_iter.current(),t->vtx_z,t->vtx);

}

void mFvtxFindHoughTracks::associatePtrToTrack(EVHit_ptr h, TFvtxTrkMap::iterator& trk_iter)
{   
  //Associate single MC/FVTX/SVX hits to track in FvtxTrkMap
  TFvtxMCHitMap::pointer mc_hit_ptr   = static_cast<TFvtxMCHitMap::pointer>(h->getMCHitPtr());
  TFvtxCoordMap::pointer coord_ptr    = static_cast<TFvtxCoordMap::pointer>(h->getCoordPtr());
  TFvtxSvxClusterMap::pointer svx_ptr = static_cast<TFvtxSvxClusterMap::pointer>(h->getSvxPtr());

  if( mc_hit_ptr ) PHKey::associate(trk_iter.current(), mc_hit_ptr);
  if( coord_ptr )  PHKey::associate(trk_iter.current(), coord_ptr);
  if( svx_ptr )    PHKey::associate(trk_iter.current(), svx_ptr);  

}

void mFvtxFindHoughTracks::fillOutputNodes()
{
  //Put tracks into _trk_map
  if( !_trk_map )
    { 
      throw std::runtime_error("mFvtxFindHoughTracks::fillOutputNodes: "
			       "FVTX Track map does not exist");
    }
  
  // Clear the output map for this event
  if( !_twoHitMode ){  _trk_map->clear(); }


  for(std::vector<EVTrk_ptr>::iterator it = _trk_vec.begin(); it != _trk_vec.end(); ++it)
    {
      associateHits((*it));
    }

}

void mFvtxFindHoughTracks::setTrkPar(TFvtxTrkMap::pointer trk_ptr,
				     const double _vtx_z,
				     const PHPoint& vtx)
{

  // First check to see if this trk_ptr has already been assigned parameters
  if( trk_ptr->get()->get_trk_par()->get_charge() != 0 ) return;

  // Set total momentum guess to some average accepted value
  double pz_init = (trk_ptr->get()->get_arm() == FVTXOO::North) ? 1000. : -1000.;
  double pxpz = 0.0;
  double pypz = 0.0;

  std::vector<PHPoint> pointVec;

  // Retrieve associated coordinates and add to track fit node:
  TFvtxCoordMap::key_iterator
      coord_iter(trk_ptr->get()->get_associated<TFvtxCoord>());
  
  while( TFvtxCoordMap::pointer coord_ptr = coord_iter.next() )
  {
    // For now, calculate an average px/pz, py/pz from individual 
    // coords and use this for the slope below, because poor initial track params
    // mess up the KF fit.
    PHPoint CoordMidPoint = coord_ptr->get()->get_coord_midpoint();
    pointVec.push_back(CoordMidPoint);
    pxpz += CoordMidPoint.getX()/(CoordMidPoint.getZ() - vtx.getZ());
    pypz += CoordMidPoint.getY()/(CoordMidPoint.getZ() - vtx.getZ());
  }
  // Need to add VTX hits in here, mostly because we can have tracks
  // that only have 1 hit in FVTX
  TFvtxSvxClusterMap::key_iterator
      svx_iter(trk_ptr->get()->get_associated<TFvtxSvxCluster>());
  
  while( TFvtxSvxClusterMap::pointer svx_ptr = svx_iter.next() )
  {
    double svx_x = svx_ptr->get()->get_cluster()->get_xyz_global(0);
    double svx_y = svx_ptr->get()->get_cluster()->get_xyz_global(1);
    double svx_z = svx_ptr->get()->get_cluster()->get_xyz_global(2);
          
    pxpz += svx_x/(svx_z - vtx.getZ());
    pypz += svx_y/(svx_z - vtx.getZ());
    pointVec.push_back(PHPoint(svx_x,svx_y,svx_z));
  }
    
  pxpz = pxpz/(coord_iter.count()+svx_iter.count());
  pypz = pypz/(coord_iter.count()+svx_iter.count());
      
  int charge = 1;
    
  TMutTrkPar trkPar(vtx.getX(), vtx.getY(), vtx.getZ(),   // x,y,z at trk_fit.get_z
                    pz_init*pxpz, pz_init*pypz, pz_init,  // px,py,pz at trk_fit.get_z
                    charge                                // charge (no knowledge here)
                    );
  
  // Lastly, if we are dealing with two hit tracks that will NOT pass
  // through the Kalman filter fit mark the track as successfully reconstructed
  if ( !_twoHitMode )
    {
      trk_ptr->get()->set_trk_par(trkPar); 
    }
  else 
    {
      // nominal beam line
      PHLine beamline(PHPoint(0,0,0),PHVector(0,0,1));
      
      // record the beam PCA as point on the 2-hit line
      PHLine trackline(pointVec[0],PHVector(pz_init*pxpz, pz_init*pypz, pz_init));   
      PHPoint pca = PHGeometry::closestApproachLineLine(beamline,trackline);
      
      if ((pca.getX() == -999)&&(pca.getY() == -999)&&(pca.getZ() == -999)) 
	{
	  // handle parallel cases, by giving point closest to nominal interaction point
	  pca.setX(pointVec[0].getX());
	  pca.setY(pointVec[0].getY());
	  pca.setZ(0.0);
	}
      
      // replace trkPar settings
      trkPar.set_x(pca.getX());
      trkPar.set_y(pca.getY());
      trkPar.set_z(pca.getZ());
      
      trk_ptr->get()->set_trk_par_vtx(trkPar);
      
      for(std::vector<PHPoint>::iterator pIt = pointVec.begin(); pIt != pointVec.end(); ++pIt)
	{
	  pushNodePars((*pIt), trk_ptr, pz_init, pxpz, pypz);
	}
      
      
      // No, we haven't actually run a kalman fit, but we need
      // to trick the modules looking to avoid unreconstructed tracks
      trk_ptr->get()->set_kalman_fit();
    }


}

void mFvtxFindHoughTracks::pushNodePars(const PHPoint& pt,
					TFvtxTrkMap::pointer trk_ptr,
					const double pz_init,
					const double pxpz,
					const double pypz)
{
  //Send par to track in map
  TMutTrkPar nodePar(pt.getX(),pt.getY(),pt.getZ(),
                     pz_init*pxpz, pz_init*pypz, pz_init,
                     1); // No charge knowledge
  trk_ptr->get()->push_trk_par(nodePar);  
}



void mFvtxFindHoughTracks::findClustersHough(const double rClusCut,
					     const double sinAlphaCut)
{
  //Use boost::adjacency_list to find clusters of pairs from _unusedPairs
  //Fill these clusters of pairs into _passedRoads

  EVPairVec candidateVec;  
  // Set up the top level graph
  typedef boost::adjacency_list <boost::vecS, boost::vecS, boost::undirectedS> Graph;

  std::multimap<int,EVPair_ptr> groups;

  Graph G;
      
  //Iterate through the _unusedPairs and populate the graph edges
  //based on proximity as defined by the rClusCut and alphaClusCut
  bool isHI = _mod_par->get_useHICuts();

  unsigned int pairSize = _unusedPairs.size();
  for(unsigned int i = 0; i < pairSize-1; i++)
    {
      for(unsigned int j = i+1; j < pairSize; j++)
	{
	  //Check for adjacent
	  if(_unusedPairs[i]->isAdjacent(_unusedPairs[j],rClusCut,sinAlphaCut,_mod_par->get_rCutFactor(),isHI)) add_edge(i,j,G);
	}
      add_edge(i,i,G);
    }

  std::vector<int> component(num_vertices(G));  
  connected_components(G, &component[0]);
  //int num = connected_components(G, &component[0]);

  //if(true) print_graph(G, get(boost::vertex_index, G));

  // Get the unique component ids
  std::set<int> comps;
  for (unsigned int i = 0; i < component.size(); i++)
    {
      comps.insert(component[i]);
      groups.insert(std::make_pair(component[i],_unusedPairs[i]));
    }

  //For evaluation
  int theClusID = 0;
  int theNhits = 0;
  int theNPairs = 0;
  std::vector<int> MCTrackIds, houghHits;

  for(std::set<int>::const_iterator id = comps.begin(); id != comps.end(); ++id)
    {
      std::multimap<int,EVPair_ptr>::const_iterator curr, last;
      boost::tie(curr,last) = groups.equal_range(*id);
      if(groups.count(*id) > 1)
	{
	  if(_doEval)
	    {
	      theNhits = 0;
	      theClusID++;
	    }

	  if(doDebug()) std::cout << "Group " << *id << " has " << groups.count(*id) << " Pairs:" << std::endl;
	  for ( ; curr!=last; ++curr)
	    {
	      EVPair_ptr pair = curr->second;
	      if(doDebug()) pair->print(std::cout);
	      candidateVec.push_back(pair);
	      //Eval Tree
	      if(_doEval)
		{
		  theNPairs++;
		  theNhits += 2;
		  _houghCT_clusID->push_back(theClusID);

		  _houghCT_hit1_x->push_back(pair->h1->x_hit);
		  _houghCT_hit1_y->push_back(pair->h1->y_hit);
		  _houghCT_hit1_z->push_back(pair->h1->z_hit);
		  _houghCT_hit1_r->push_back(pair->h1->r_hit);
		  _houghCT_hit1_phi->push_back(pair->h1->phi_hit);
		  houghHits.push_back(pair->h1->id);

		  _houghCT_hit2_x->push_back(pair->h2->x_hit);
		  _houghCT_hit2_y->push_back(pair->h2->y_hit);
		  _houghCT_hit2_z->push_back(pair->h2->z_hit);
		  _houghCT_hit2_r->push_back(pair->h2->r_hit);
		  _houghCT_hit2_phi->push_back(pair->h2->phi_hit);
		  houghHits.push_back(pair->h2->id);

		  _houghCT_pair_rInt->push_back(pair->rInt);
		  _houghCT_pair_sinAlpha->push_back(pair->sinAlpha);
		  _houghCT_hasSVXCluster->push_back(pair->hasSVXCluster());
		  
		  //Check different MC tracks in cluster
		  if((TFvtxMCHitMap::pointer)pair->h1->getMCHitPtr()){
		    MCTrackIds.push_back((*((TFvtxMCHitMap::pointer)pair->h1->getMCHitPtr()))->get_track_id());
		    if(doDebug()) std::cout << "MCTrkId: " << (*((TFvtxMCHitMap::pointer)pair->h1->getMCHitPtr()))->get_track_id() << "  ";
		  }
		  if((TFvtxMCHitMap::pointer)pair->h1->getMCHitPtr()){
		    MCTrackIds.push_back((*((TFvtxMCHitMap::pointer)pair->h2->getMCHitPtr()))->get_track_id());
		    if(doDebug()) std::cout << (*((TFvtxMCHitMap::pointer)pair->h2->getMCHitPtr()))->get_track_id() << std::endl;
		  }
		}	  
	    }
	  //For evaluation
	  if(_doEval)
	  {
	    
	    _houghCT_nHits = theNhits;
	    _houghCT_nClusters = theClusID; 
	    //_houghCT_slice++;
	    _houghCT_nPairs = theNPairs;
	    
	    std::sort(houghHits.begin(),houghHits.end());
	    std::vector<int>::iterator unItHough = std::unique(houghHits.begin(),houghHits.end());
	    _houghCT_nUniqueHits = std::distance(houghHits.begin(),unItHough);
	    
	    std::sort(MCTrackIds.begin(),MCTrackIds.end());
	    std::vector<int> MCTrackIds_copy = MCTrackIds;
	    std::vector<int>::iterator unIt = std::unique(MCTrackIds_copy.begin(),MCTrackIds_copy.end());
	    _houghCT_nMCTrksInCluster = std::distance(MCTrackIds_copy.begin(),unIt);
	    int maxIdCount = 0;
	    for(std::vector<int>::iterator theIt = MCTrackIds_copy.begin(); theIt != unIt; ++theIt)
	      {
		int tmpCount = std::count(MCTrackIds.begin(),MCTrackIds.end(),(*theIt));
		if(tmpCount > maxIdCount) maxIdCount = tmpCount;
	      }
	    _houghCT_nMCHitsFromSameTrk = maxIdCount;
	    
	    if(doDebug()) std::cout << "Groups(same,unique,tracks): " << _houghCT_nMCHitsFromSameTrk << "  " 
				   << _houghCT_nUniqueHits << "  " << _houghCT_nMCTrksInCluster << std::endl;
	    
	    //Fill Tree
	    //_hough_cluster_tree->Fill();
	    //Reset
	    _houghCT_hit1_x->clear();
	    _houghCT_hit1_y->clear();
	    _houghCT_hit1_z->clear();
	    _houghCT_hit1_r->clear();
	    _houghCT_hit1_phi->clear();
	    _houghCT_hit2_x->clear();
	    _houghCT_hit2_y->clear();
	    _houghCT_hit2_z->clear();
	    _houghCT_hit2_r->clear();
	    _houghCT_hit2_phi->clear();
	    _houghCT_pair_rInt->clear();
	    _houghCT_pair_sinAlpha->clear();
	    _houghCT_hasSVXCluster->clear();
	    theNPairs = 0;
	    theNhits = 0;
	    _houghCT_nMCHitsFromSameTrk = -1;
	    _houghCT_nMCTrksInCluster = -1;
	    _houghCT_nUniqueHits = -1;
	    
	  }
	  
	}
      //Add to saved vec
			if(candidateVec.size()>0)
				_passedRoads.push_back(candidateVec); 
      //Clear temporary cluster vec
      candidateVec.clear();
    }

}


/*
std::vector<bool> mFvtxFindHoughTracks::getGoodMuonCandidateSlices(PHCompositeNode* top_node, int theArm)
{

  ///////MUON REQUIREMENTS//////
  float maxChiSq = 30;
  int minMuiDepth = 2;
  //////////////////////////////

  //Set initial vector to all false
  std::vector<bool> tmpVecSlices;
  for(int i = 0; i < NSLICES; i++)
    {
      tmpVecSlices.push_back(false);
    }

  //Grab Muon Tracks
  TMutTrkMap* trk_map = 0;
  try{
    trk_map = TMutNode<TMutTrkMap>::find_node(top_node, "TMutTrkMap");
  }
  catch(std::exception& e){
    std::cout << "mFvtxFindHoughTracks::getGoodMuonCandidateSlices - cannot find TMutTrkMap!!!" << std::endl;
  }

  TMutTrkMap::const_iterator trk_iter = trk_map->range();

  //Iterate over muon tracks, require they be loose good tracks
  while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next())
    {

      //We do this for each arm separately for simplicity
      int arm      = trk_ptr->get()->get_arm();
      if(arm != theArm) continue;

      //float px     = trk_ptr->get()->get_trk_par()->get_px();
      //float py     = trk_ptr->get()->get_trk_par()->get_py();
      //float pz     = trk_ptr->get()->get_trk_par()->get_pz();
      
      int road_depth = 0;
      TMuiRoadMapO::const_key_iterator road_iter = trk_ptr->get()->get_associated<TMuiRoadO>();
      if(!road_iter.at_end()) road_depth = road_iter->get()->get_depth();
      float chisq = trk_ptr->get()->get_w_chi_square()/trk_ptr->get()->get_ndf();
      
      //Min requirements on track
      if(road_depth < minMuiDepth) continue;
      if(chisq > maxChiSq) continue;
      
      const TMutTrkPar* sta_trk_par = trk_ptr->get()->get_trk_par_station(0);
      float x = sta_trk_par->get_x();
      float y = sta_trk_par->get_y();
      float phi = atan2(y,x);//FVTXOO::angle_normalize(atan2(y,x))

      //If we make it here, check which slice this track is in
      PairVecItr sliceItr = std::lower_bound(_phiLimits.begin(), _phiLimits.end(),
					     phi, Pair2ndComp<int,double>() );
      
      int slice = ( sliceItr != _phiLimits.end() ) ? (*sliceItr).first : _phiLimits.front().first;

      std::vector<int> theSurroundingSlices;
      int curOffset = slice;
      int prevOffset = slice - 1;
      int prevOffset2 = slice - 2;
      if(slice == 0){ prevOffset = NSLICES-1; prevOffset2 = NSLICES-2;}
      else if(slice == 1){ prevOffset2 = NSLICES-1;}
      int nextOffset = slice + 1;
      int nextOffset2 = slice + 2;
      if(slice == NSLICES-1){ nextOffset = 0; nextOffset2 = 1;}
      else if(slice == NSLICES-2){ nextOffset2 = 0;}

      //Tell the code to reconstruct 5 slices around this one
      //which will be 5 wedges centered on this phi
      tmpVecSlices[prevOffset2] = true;
      tmpVecSlices[prevOffset] = true;
      tmpVecSlices[curOffset]  = true;
      tmpVecSlices[nextOffset] = true;
      tmpVecSlices[nextOffset2] = true;

    }


  return tmpVecSlices;

}
*/

std::vector<bool> mFvtxFindHoughTracks::getGoodMuonCandidateSlices(PHCompositeNode* top_node, int theArm)
{

  ///////MUON REQUIREMENTS//////
  float maxChiSq = 30;
  int minMuiDepth = 1;
  //////////////////////////////

	//Set initial vector to all false
	std::vector<bool> tmpVecSlices;
	for(int i = 0; i < NSLICES; i++)
	{
		tmpVecSlices.push_back(false);
	}

	//Check UPC trigger
	bool bUPCtrig = false;
	TriggerHelper myTH(top_node);
	if ( myTH.didLevel1TriggerFire("UltraPeriph") || 
			myTH.didLevel1TriggerFire("UltraPeriphMuon2DSouth") ||
			myTH.didLevel1TriggerFire("UltraPeriphMuon2DNorth") ||
			myTH.didLevel1TriggerFire("UltraPeriphMPC") ){
		bUPCtrig = true;
	}

	//Vertex information
	PHPoint vtx_proj;
	vtx_proj = _vtxout->get_Vertex("SVX_PRECISE"); 

	if ( isnan(vtx_proj.getZ()) ){
		vtx_proj = _vtxout->get_Vertex("SVX"); 
		if ( isnan(vtx_proj.getZ()) ){
			vtx_proj = _vtxout->get_Vertex("BBC"); 
		}
	}

	//Grab Muon Tracks
	TMutTrkMap* trk_map = 0;
	try{
		trk_map = TMutNode<TMutTrkMap>::find_node(top_node, "TMutTrkMap");
	}
	catch(std::exception& e){
		std::cout << "mFvtxFindHoughTracks::getGoodMuonCandidateSlices - cannot find TMutTrkMap!!!" << std::endl;
	}

	TMutTrkMap::const_iterator trk_iter = trk_map->range();

	//Iterate over muon tracks, require they be loose good tracks
	while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next())
	{

		if( !trk_ptr->get()->get_reco_success() ) continue;
		if( trk_ptr->get()->get_ghost() ) continue;

		//We do this for each arm separately for simplicity
		int arm      = trk_ptr->get()->get_arm();
		if(arm != theArm) continue;

		//float px     = trk_ptr->get()->get_trk_par()->get_px();
		//float py     = trk_ptr->get()->get_trk_par()->get_py();
		//float pz     = trk_ptr->get()->get_trk_par()->get_pz();

		int road_depth = 0;
		TMuiRoadMapO::const_key_iterator road_iter = trk_ptr->get()->get_associated<TMuiRoadO>();
		if(!road_iter.at_end()) road_depth = road_iter->get()->get_depth();
		float chisq = trk_ptr->get()->get_w_chi_square()/trk_ptr->get()->get_ndf();

		//Min requirements on track
		if(road_depth < minMuiDepth) continue;
		if(chisq > maxChiSq) continue;

		const TMutTrkPar* sta_trk_par = trk_ptr->get()->get_trk_par_station(MUTOO::Station1);
		float x = sta_trk_par->get_x();
		float y = sta_trk_par->get_y();
		//float phi = FVTXOO::angle_normalize(atan2(y-_beam_y,x-_beam_x));
		float phi = FVTXOO::angle_normalize(atan2(y,x));

		//If we make it here, check which slice this track is in
		PairVecItr sliceItr = std::lower_bound(_phiLimits.begin(), _phiLimits.end(),
				phi, Pair2ndComp<int,double>() );

		int slice = ( sliceItr != _phiLimits.end() ) ? (*sliceItr).first : _phiLimits.front().first;

		if (doDebug()) std::cout << "MUON SLICE: " << slice << std::endl;

		// Calculate phi using vertex fit extrapolated track instead:
		if (trk_ptr->get()->get_reco_success() && !bUPCtrig)
			//if(false)
		{
			//float zref = (trk_ptr->get()->get_arm() > 0) ? 40.0 : -40.0;
			float zref = (trk_ptr->get()->get_arm() > 0) ? 25.0 : -25.0;
			mMutFitVtx::Fitter vertex_fitter;
			vertex_fitter.add_track(trk_ptr);
			PHPoint vtx_error(0.5, 0.5, 1.0);

			//vertex_fitter.add_vertex(_vertex, vtx_error);
			vertex_fitter.add_vertex(vtx_proj, vtx_error);
			vertex_fitter.fit();

			if(doDebug()) cout << "vertex fit = " << vertex_fitter.get_vtx_z() << ", " << vertex_fitter.get_pz(0) << endl;

			TMutTrkPar mutr_par = TMutTrkPar(
					vertex_fitter.get_vtx_x(),
					vertex_fitter.get_vtx_y(),
					vertex_fitter.get_vtx_z(),
					vertex_fitter.get_px(0),
					vertex_fitter.get_py(0),
					vertex_fitter.get_pz(0),
					static_cast<int>(trk_ptr->get()->get_charge()),
					vertex_fitter.get_chisquare() );

			TMutTrkPar mutr_extrap_trk_par;
			PHTrackIntegratorKF integrator;
			integrator.initialize(mutr_par);

			integrator.extrapolate( zref );
			TMutTrkPar extrap_trk_par;

			if(doDebug()) cout << "extrap error = " << integrator.get_error() << endl;

			if (!(integrator.get_error())){
				integrator.finish(extrap_trk_par);
				//phi = FVTXOO::angle_normalize(atan2(extrap_trk_par.get_y() - _beam_y, extrap_trk_par.get_x() - _beam_x));
				phi = FVTXOO::angle_normalize(atan2(extrap_trk_par.get_y(), extrap_trk_par.get_x()));
				
				PairVecItr sliceItr2 = std::lower_bound(_phiLimits.begin(), _phiLimits.end(),
						phi, Pair2ndComp<int,double>() );

				int slice2 = ( sliceItr2 != _phiLimits.end() ) ? (*sliceItr2).first : _phiLimits.front().first;

				if(doDebug()) std::cout << "SUCCESS: " << slice2 << std::endl;

				slice = slice2;
			}else{
				std::cout << "FAILED to extrapolrate at z=" << zref << std::endl;
			}
		}


		//std::vector<int> theSurroundingSlices;
		int curOffset = slice;
		tmpVecSlices[curOffset] = true;

		const int noffset = 7;

		//prev
		for (int ioff=0; ioff<noffset; ioff++){
			int prevOffset = slice - 1 - ioff; 
			if ( prevOffset<0 ) prevOffset += NSLICES;
			tmpVecSlices[prevOffset] = true;
		}

		//next
		for (int ioff=0; ioff<noffset; ioff++){
			int nextOffset = slice + 1 + ioff; 
			if ( nextOffset>=NSLICES ) nextOffset -= NSLICES;
			tmpVecSlices[nextOffset] = true;
		}

		/*
		for (int ioff=0; ioff<noffset; ioff++){
			int prevOffset = slice - 1 - ioff; 
			int nextOffset = slice + 1 + ioff; 

			if ( prevOffset<0 ) prevOffset += NSLICES;
			if ( nextOffset>=NSLICES ) nextOffset -= NSLICES;

			tmpVecSlices[prevOffset] = true;
			tmpVecSlices[nextOffset] = true;
		}
		*/

	} //trk_ptr

	return tmpVecSlices;

}


/*-------------------------------------EVHit-----------------------------------------------*/

EVHit::EVHit() :
  id(0),
  event(0),
  layer(-1),
  arm(-1),
  cage(-1),
  station(-1),
  sector(-1),
  column(-1),
  halfwedge(-1),
  slice(-1),
  coordnum(-1),
  svx_hitnum(-1),
  x_hit(0.0),
  y_hit(0.0),
  z_hit(0.0),
  x_hit_beam(0.0),
  y_hit_beam(0.0),
  r_hit(0.0),
  phi_hit(0.0),
	r_hit_beam(0.0),
	phi_hit_beam(0.0),
  phi_start(0.0),
  phi_end(0.0),
  ntracks(0),
  trackNum(0),
  _mc_hit_ptr(NULL),
  _coord_ptr(NULL),
  _svx_ptr(NULL)
{}

std::ostream& 
EVHit::print(std::ostream& os) const
{
  int sector = this->sector;
  int arm = this->arm;
  int cage = this->cage;
  int station = this->station;
  int column = this->column;
  int layer = this->layer;
  float r = this->r_hit;
  float z = this->z_hit;
  float phi = this->phi_hit;
  float r_beam = this->r_hit_beam;
	float phi_beam = this->phi_hit_beam;
  os << "(arm,cage,sta,sec,col,layer) = (" 
     << arm << "," << cage << "," << station << "," << sector
     << "," << column << "," << layer << "): ";
  os << "r = " << r << ", z = " << z << ", phi = " << phi
     << std::endl;
  os << "r_beam = " << r_beam << ", phi_beam = " << phi_beam
     << std::endl;
  return os;
}

/*-------------------------------------EVHoughPair-----------------------------------------------*/

EVHoughPair::EVHoughPair() :
  _usedFlag(false),    
  id(-1), 
  m(0.0),
  rInt(0.0),
  sinAlpha(0.0),
	m_beam(0.0),
	rInt_beam(0.0),
	sinAlpha_beam(0.0),
  dcaZ(0.0),
  dcaR(0.0),
  _vtx_z(0.0),
  _vtx(),
	_arm(-1),
  isDupe(false),
  ntracks(0),
  h1(),
  h2()
{}

EVHoughPair::EVHoughPair(const int ID, EVHit* firstHit,EVHit* secondHit, const int arm, const double vtx_z) :
  _usedFlag(false),
  m(0.0),
  rInt(0.0),
  sinAlpha(0.0),
  dcaZ(0.0),
  dcaR(0.0),
  _vtx_z(vtx_z),
  _vtx(),
	_arm(arm),
  isDupe(false),
  ntracks(0),
  h1(firstHit),
  h2(secondHit)
{
  id = ID;
  m = (h2->r_hit - h1->r_hit)/(h2->z_hit - h1->z_hit);
  sinAlpha = m/sqrt(1.+m*m);

  m_beam = (h2->r_hit_beam - h1->r_hit_beam)/(h2->z_hit - h1->z_hit);
  sinAlpha_beam = m_beam/sqrt(1.+m_beam*m_beam);

  //rInt = h1->r_hit + m*(sgn(h1->z_hit)*z_proj - h1->z_hit);
	float tmp_z_proj = (_arm==0) ? -z_proj : z_proj;
  rInt = h1->r_hit + m*(tmp_z_proj - h1->z_hit);
	rInt_beam = h1->r_hit_beam + m_beam*(tmp_z_proj - h1->z_hit);

  dcaZ = calcDcaZ(vtx_z,arm);
  dcaR = calcDcaR(vtx_z,arm);  
}

double EVHoughPair::calcDcaZ(const double vtx_z, const int arm)
{
	float tmp_z_proj = (arm==0) ? -z_proj : z_proj;
  //return fabs(vtx_z - sgn(h1->z_hit)*z_proj + rInt/m);
  //return fabs(vtx_z - tmp_z_proj + rInt/m);
	//! use beam xy
  return (vtx_z - tmp_z_proj + rInt_beam/m_beam);
}

double EVHoughPair::calcDcaR(const double vtx_z, const int arm)
{
	float tmp_z_proj = (arm==0) ? -z_proj : z_proj;
  //return fabs(rInt - (sgn(h1->z_hit)*z_proj - vtx_z)*m);
  //return fabs(rInt - (tmp_z_proj - vtx_z)*m);
	//! use beam xy
	return (rInt_beam - (tmp_z_proj - vtx_z)*m_beam);
}

bool EVHoughPair::hitsUnused()
{
  if ( _usedFlag )
    return !_usedFlag;
  else
    return evaluateHitsUsed();
}

bool EVHoughPair::evaluateHitsUsed()
{
  _usedFlag = !(h1->getNtracks() == 0 && h2->getNtracks() == 0);
  return !_usedFlag;
}

void EVHoughPair::recalcDcaVals(PHPoint vtx,PHPoint vtx2,int arm)
{
  // Only need to recalculate if it is an unused hit pair
  if( !hitsUnused() ) return;
 
  // If there is no secondary vertex then don't waste cycles 
  if( vtx.getZ() == vtx2.getZ() )
  {
    _vtx_z = vtx.getZ();
    dcaZ = calcDcaZ(_vtx_z,arm);
    dcaR = calcDcaR(_vtx_z,arm);
    _vtx = vtx;
  }
  else // Use the minimum of the two dcas based on the two vertices
  {
    double tmpDca = calcDcaZ(vtx.getZ(),arm);
    double tmpDca2 = calcDcaZ(vtx2.getZ(),arm);
    
    if( tmpDca < tmpDca2 )
    {
      dcaZ = tmpDca;
      _vtx_z = vtx.getZ();
      _vtx = vtx;
    }
    else
    {
      dcaZ = tmpDca2;
      _vtx_z = vtx2.getZ();
      _vtx = vtx2;
    }
    dcaR = calcDcaR(_vtx_z,arm);
  }
}


/*-------------------------------------EVTrk-----------------------------------------------*/

EVTrk::EVTrk() :
  event(0),
  track(0),
  trk_arm(-1),
  nstationsHit(0),
  nlayersHit(0),
  size(0),
  vtx_z(0),
  vtx(),
  nshared(0),
  hits(0)
{ 
}

EVTrk::EVTrk(const int armIn, const EVPair_ptr& p) :
  event(0),
  track(0),
  trk_arm(armIn),
  nstationsHit(0),
  nlayersHit(0),
  size(0),
  vtx_z(p->_vtx_z),
  vtx(p->_vtx),
  nshared(0),
  hits(0)
{ 
  addHit(p->getFirstHitPtr());
  addHit(p->getSecondHitPtr());
}


EVTrk::~EVTrk()
{
  clear();
}

void EVTrk::addHit(EVHit_ptr h)
{
  h->ntracks++;
  hits.push_back(h);
}

void EVTrk::addHitPair(EVPair_ptr p)
{
  addHit(p->h1);
  addHit(p->h2);
  p->ntracks++;
}

void EVTrk::addHitNoRef(EVHit_ptr h)
{
  // Yes, it's hacky....
  addHit(h); // Add hit
  h->ntracks--; // but undo the reference count
}

void EVTrk::uniquify()
{
  std::sort(hits.begin(),hits.end(),EVHitLessR());
  // Erase the duplicated pointers from the hit vector
  hits.erase(std::unique(hits.begin(),hits.end(),EVHitEqualR()),hits.end());
}

void EVTrk::clear()
{
  size = 0;
  for( EVHitVec::iterator i = hits.begin(); i != hits.end(); ++i )
    (*i)->ntracks--;
  hits.clear();
}

void EVTrk::calculateNShared()
{
  nshared = 0;
  for( EVHitVec::iterator it = hits.begin(); it!=hits.end(); ++it )
  {
    EVHit_ptr h = *it;
    if( h->ntracks > 1 ) nshared++;
  }
  return;
}

void EVTrk::sortHitsZ()
{
  // Sort EVHits by ascending value of |z| using the member function get_z.
  std::sort(hits.begin(), hits.end(),
            bind(std::ptr_fun<double,double>(&std::fabs),
                 bind(&EVHit::get_z,_1)) < 
            bind(std::ptr_fun<double,double>(&std::fabs),
                 bind(&EVHit::get_z,_2)));
}

// Count the number of hits the track has that have #used > 1
unsigned int EVTrk::getNShared() const 
{
  return std::count_if(hits.begin(),hits.end(),
                       bind(std::greater<int>(),
                            bind(&EVHit::getNtracks,_1),
                            1));
}


/*-------------------------------------EVHoughPairVec-----------------------------------------------*/


EVHoughPairVec::EVHoughPairVec(const int NArms = FVTXOO::MAX_ARM, const int NSlices = NSLICES):
  maxArms(NArms),
  maxSlices(NSlices),
  pairId(0),
  HISinAlphaCutHigh(0.8),//Chosen so nothing unreal can be made
  HISinAlphaCutLow(0.04)//Very close to parallel with beam
{
  //Size the storage vectors
  pairs.resize(maxArms);
  for(unsigned int i = 0; i < pairs.size(); i++)
    {
      pairs[i].resize(maxSlices);
    }
}

EVHoughPairVec::~EVHoughPairVec()
{
  //Make sure we delete all the memory allocated to pairs
  for(unsigned int arm = 0; arm < maxArms; arm++)
    {
      for(unsigned int slice = 0; slice < maxSlices; slice++)
	{
	  for(std::vector<EVHoughPair*>::iterator it = pairs[arm][slice].begin(); it != pairs[arm][slice].end(); ++it)
	    {
	      delete *it;
	    }
	}
    }
}
  
void EVHoughPairVec::Add(const int arm,const int slice, EVHit* hit1, EVHit* hit2,const double vertex_z, const bool checkForDuplicate, const bool isHI, const bool isUPC, const bool use_precise_vtx)
{
	if(isHI && !isUPC)
	{
		double sinAlpha = getSinAlpha(hit1,hit2);
		//Check pair slope 
		if((arm==0 && sinAlpha>0) || (arm==1 && sinAlpha<0)) return;
		//First check to see if it passes a loose cut
		if(fabs(sinAlpha) > HISinAlphaCutHigh) return;
		if(fabs(sinAlpha) < HISinAlphaCutLow) return;
		//Check to see if it will ever make a candidate track
		//see : ...
		double rInt = getRInt(hit1,hit2,arm);
		if(fabs(sinAlpha) > 0.4)
		{
			if(failedRSinAlphaCheck(rInt,sinAlpha)) return;
		}

		double DcaZ = getDcaZ(hit1,hit2,arm,vertex_z);
		double DeltaPhi = FVTXOO::angle_normalize(hit1->get_phi_beam() - hit2->get_phi_beam());
		if (use_precise_vtx)
		{
			//SVX_PRECISE VTX
			if(hit1->getStation()<0 && hit2->getStation()<0)
			{
				//VTX-VTX
				if(fabs(DcaZ) > 1.50) return;
				if(fabs(DeltaPhi) > 0.075) return;
			}
			else if (hit1->getStation()<0 || hit2->getStation()<0)
			{
				//VTX-FVTX
				if(fabs(DcaZ) > 2.00) return;
				if(fabs(DeltaPhi) > 0.150) return;
			}
			else
			{
				//FVTX-FVTX
				if(fabs(DcaZ) > 4.00) return;
				if(fabs(DeltaPhi) > 0.130) return;
			}
		}
		else
		{
			//BBC VTX
			if(hit1->getStation()<0 && hit2->getStation()<0)
			{
				//VTX-VTX
				if(fabs(DcaZ) > 2.00) return;
				if(fabs(DeltaPhi) > 0.075) return;
			}
			else if (hit1->getStation()<0 || hit2->getStation()<0)
			{
				//VTX-FVTX
				if(fabs(DcaZ) > 2.50) return;
				if(fabs(DeltaPhi) > 0.150) return;
			}
			else
			{
				//FVTX-FVTX
				if(fabs(DcaZ) > 4.00) return;
				if(fabs(DeltaPhi) > 0.130) return;
			}
		}
	}
	else
	{
		double sinAlpha = getSinAlpha(hit1,hit2);
		//Check pair slope 
		if((arm==0 && sinAlpha>0) || (arm==1 && sinAlpha<0)) return;
	}

  //Duplicate check 
  if(checkForDuplicate) 
	{
		if(!isUnique(hit1,hit2,arm)) return;
	}

  //Create and add hough pair to pairs
  pairs[arm][slice].push_back(new EVHoughPair(pairId,hit1,hit2,arm,vertex_z));
  //Keep track of the ids for duplicate checking
  int key1 = hit1->id;
  int key2 = hit2->id;
  if(key1 > key2){ key1 = hit2->id; key2 = hit1->id;}
  pairKeys.insert(std::make_pair(key1,key2));
  pairId++;

}

inline unsigned int EVHoughPairVec::SizeSliceVec(const int arm,const int slice)
{
  return pairs[arm][slice].size();
}

inline unsigned int EVHoughPairVec::SizeArmVec(const int arm)
{
  return pairs[arm].size();
}

inline EVHoughPair* EVHoughPairVec::Get(const int arm,const int slice, int index)
{
  return pairs[arm][slice][index];
}

std::vector<EVHoughPair*> EVHoughPairVec::GetSliceVec(const int arm, const int slice)
{
  return pairs[arm][slice];
}

std::vector<std::vector<EVHoughPair*> > EVHoughPairVec::GetArmVec(const int arm)
{
  return pairs[arm];
}


inline EVHoughPair EVHoughPairVec::GetObject(const int arm,const int slice, int index)
{
  return *pairs[arm][slice][index];
}


bool EVHoughPairVec::isUnique(EVHit* hit1, EVHit* hit2, const int arm)
{
  //Check if this pair already exists
  int key1 = hit1->id; 
  int key2 = hit2->id;
  if(key1 > key2){ key1 = hit2->id; key2 = hit1->id;}

  std::set<std::pair<int,int> >::iterator it;
  it = pairKeys.find(std::make_pair(key1,key2));
  if(it != pairKeys.end()) return false;
  
  return true;

}

double EVHoughPairVec::getSinAlpha(EVHit* hit1, EVHit* hit2)
{

  //double m = (hit2->r_hit - hit1->r_hit)/(hit2->z_hit - hit1->z_hit);
	//! use beam xy
	double m = (hit2->r_hit_beam - hit1->r_hit_beam)/(hit2->z_hit - hit1->z_hit);
  return m/sqrt(1.+m*m);

}

double EVHoughPairVec::getRInt(EVHit* hit1, EVHit* hit2, int arm)
{

  const int z_proj = (arm==0) ? -20 : 20;//Project to first station

  //double m = (hit2->r_hit - hit1->r_hit)/(hit2->z_hit - hit1->z_hit);
  //return (hit1->r_hit + m*(z_proj - hit1->z_hit));

	//! use beam xy
	double m = (hit2->r_hit_beam - hit1->r_hit_beam)/(hit2->z_hit - hit1->z_hit);
  return (hit1->r_hit_beam + m*(z_proj - hit1->z_hit));

}

double EVHoughPairVec::getDcaZ(EVHit* hit1, EVHit* hit2, const int arm, const double vertex_z)
{

	const int z_proj = (arm==0) ? -20 : 20;//Project to first station

  double m = (hit2->r_hit_beam - hit1->r_hit_beam)/(hit2->z_hit - hit1->z_hit);
	double rInt = hit1->r_hit_beam + m*(z_proj - hit1->z_hit);

	return vertex_z - z_proj + rInt/m;

}

bool EVHoughPairVec::failedRSinAlphaCheck(const double rInt, const double sinAlpha)
{
  
  const double CenterOfR = 7.0;
  const double CenterOfSinAlpha = 0.0;

  const double slope = 0.026667;
  const double b1 = 0.62;
  const double b2 = 1.12;
  

  //Check four quadrants of R-SinAlpha space
  //Positve R (displaced center)
  if( rInt > CenterOfR )
    {
      //Positive sinAlpha
      if( sinAlpha > CenterOfSinAlpha )
	{
	  double y = -slope*rInt + b2;
	  if(sinAlpha > y) return true;
	}
      //Negative sinAlpha
      if( sinAlpha < CenterOfSinAlpha )
	{
	  double y = slope*rInt - b2;
	  if(sinAlpha < y) return true;
	}
    }
  //Negative R
  if( rInt < CenterOfR )
    {
      //Positive sinAlpha
      if( sinAlpha > CenterOfSinAlpha )
	{
	  double y = slope*rInt + b1;
	  if(sinAlpha > y) return true;
	}
      //Negative sinAlpha
      if( sinAlpha < CenterOfSinAlpha )
	{
	  double y = -slope*rInt - b1;
	  if(sinAlpha < y) return true;
	}
    }
  
  //Else we didnt fail
  return false;
  
  
}

