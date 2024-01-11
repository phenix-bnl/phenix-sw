
#include <math.h>
#include <vector>
#include <map>
#include <set>
#include <iostream>
#include <iterator>
#include <stdexcept>
#include <boost/bind.hpp>

#include <PHTFileServer.h>
#include <PHCylPoint.h>
#include <mFvtxFindTracks.h>
#include <TMutMCTrkMap.h>
#include <TFvtxMCHitMap.h>
#include <TFvtxTrkMap.h>
#include <TFvtxHitMap.h>
#include <TFvtxClusMap.h>
#include <TFvtxSvxClusterMap.h>
#include <TFvtxCoordMap.h>
#include <mFvtxFindTrackPar.h>
#include <TMutExtVtx.h>

#include <FvtxGeom.h>
#include <FvtxStation.h>
#include <FvtxSector.h>
#include <FvtxColumn.h>

#include <TTree.h>

using boost::bind;

// Shorthand for squaring a number
template<typename T> T SQ(const T& x) { return x*x; }

// Utility to all STL access to delete
template<typename T> struct Deleter : public std::unary_function<void,T>
{
  void operator()(const T* v) const { delete v; }
};

// Simple class to fill a TTree. It works for only those classes that have
// a single branch.
template<typename T> struct Filler : public std::unary_function<void,T>
{
  TTree* _t; // Tree to be filled
  T* _v_ptr; // buffer that tree points to
  Filler(TTree* t, T* v_ptr) : _t(t), _v_ptr(v_ptr) {}
  void operator()(T* v)
  {
    *_v_ptr = *v; // copy the data into the buffer
    _t->Fill();   // fill the tree with the contents of the buffer
  }
};

bool 
mFvtxFindTracks::RequireNMidStations::checkHits(const int nMidStations,
						const int n_svx_layers,
						const EVHitVec& start, 
						const EVHitVec& mid, 
						const EVHitVec& end) const
{
  // Get the start station # and the end station # from a hit from either vector
  // Station here is from 0 to _n_svx_layers+NFVTX_LAYERS-1 and combines layers from the SVX with true stations from the FVTX
  boost::shared_ptr<std::vector<int> > nSta (new std::vector<int>(n_svx_layers+NFVTX_LAYERS));

  int startSta = (start.front()->getStation() < 0) ? start.front()->getLayer() : start.front()->getStation()+n_svx_layers;
  int endSta = (end.front()->getStation() < 0) ? end.front()->getLayer() : end.front()->getStation()+n_svx_layers;

  // Mark which "stations" are included in the vector
  for (EVHitVec::const_iterator i=mid.begin(); i!=mid.end(); i++)
    nSta->at(((*i)->getStation() < 0) ? (*i)->getLayer() : (*i)->getStation()+n_svx_layers ) = 1; 

  // Return true if the number of ones from nSta[startSta+1] to nSta[endSta-1] is at least nMidStations
  return std::accumulate(nSta->begin()+startSta+1,nSta->begin()+endSta,0) >= nMidStations-2;
}

mFvtxFindTracks::mFvtxFindTracks() : 
  _do_evaluation(false),
  _use_svx_cluster(false),
  _n_svx_layers(0),
  _evalFilename("mFvtxFindTracks_eval.root"),
  _mode(1), 
  _dR(0.0075), 
  _dPhi(0.0),
  _vertex_z(0.0),
  _mod_par(0),
  _mc_hit_map(0),
  _trk_map(0),
  _coord_map(0),
  _svx_map(0),
  _hit_map(0),
  _timer("mFvtxFindTracks"),
  _ievent(0),
  _hitId(0),
  _trackId(0)
{
  FVTXOO::TRACE("Initializing module mFvtxFindTracks compiled " __DATE__ " " __TIME__ );
}

mFvtxFindTracks::~mFvtxFindTracks() {}

void
mFvtxFindTracks::init(PHCompositeNode*)
{
  _ievent = 0;

  if ( _do_evaluation ) 
    {
      //! open TFile
      PHTFileServer::get().open( _evalFilename, "RECREATE" );
      std::cout << "mFvtxFindTracks::init: Writing to file " << _evalFilename << std::endl;
      
      book_trees();
    }
}

void
mFvtxFindTracks::end(PHCompositeNode*)
{
  if ( _eval_trees.size() )
    {
      std::cout << "mFvtxFindTracks::end: AutoSaving the eval trees" << std::endl;
      std::for_each(_eval_trees.begin(),_eval_trees.end(),boost::bind(&TTree::AutoSave,_1,""));
    }

  _timer.print_stat();

  return;
}

void
mFvtxFindTracks::book_trees()
{
  std::for_each(_eval_trees.begin(),_eval_trees.end(),Deleter<TTree>());
  _eval_trees.clear();

  // Set the autosave value for each of the trees
  //
  std::for_each(_eval_trees.begin(),_eval_trees.end(),
		boost::bind(&TTree::SetAutoSave,_1,(Long64_t)AUTO_SAVE));
  
  return;
}

void
mFvtxFindTracks::fill_trees()
{
}

void
mFvtxFindTracks::load_ext_vertex(PHCompositeNode* top_node)
{
  // reference z is used by default
  _vertex_z = 0.0;

  // try load external vertex
  bool error( false );
  PHPoint vtx( TMutExtVtx::get().get_vtx( error ) );
  if ( error )
    {
      
      if( _mod_par->get_verbosity() >= FVTXOO::SOME )
	std::cerr << "mFvtxFindTracks::load_ext_vertex - wrong external vertex.\n";
      
    } else
      {
	_vertex_z = vtx.getZ();
      }
  
  if (_mod_par->get_verbosity() > FVTXOO::NONE)
    {
      std::cout << "_vertex_z in mFvtxFindTracks = " << _vertex_z << std::endl;
    }
  
  return;

}

void
mFvtxFindTracks::clear()
{
  _hitId = 0;
  _trackId = 0;

//   std::for_each(_trk_vec.begin(),_trk_vec.end(),Deleter<mFvtxFindTracks::EVTrk>());
//   std::for_each(_hit_vec.begin(),_hit_vec.end(),Deleter<mFvtxFindTracks::EVHit>());

  _hit_vec.clear();
  _trk_vec.clear();

  // Clear out the contents (but don't delete the pointers!) of the
  // station vectors.
  for (int i=0; i<NARMS; i++)
    {
      for(int j=0; j<NFVTX_LAYERS; j++) 
	_hitsByStation[i][j].clear();
    }

  if ( _use_svx_cluster )
    for (int i=0; i<_n_svx_layers; i++)
      _hitsByLayer[i].clear(); 

  return;
}

PHBoolean
mFvtxFindTracks::event(PHCompositeNode* topNode)
{
  _timer.restart();

  _ievent++;

  //clear();

  // Reset IOC pointers
  set_interface_ptrs(topNode);

  // Load the event vertex position:
  load_ext_vertex( topNode );

  // First convert the input data to internal data structures
  //
  if ( _mode == 0 ) convert_mchits();
  else if ( _mode == 1 ) convert_coords();
  else throw std::runtime_error("Invalid convert mode in mFvtxFindTracks::event");
  
  // Then sort them 
  sort_hits();

  // Loop over arms
  for (int i=0; i<NARMS; i++) 
    find_tracks(i);

  // Translate internal data structures to node output
  fillOutputNodes();

  // Set the track parameters 
  set_trk_par();

  clear();

  _timer.stop(); // Stop here to exclude time used for eval filling

  if ( _do_evaluation ) fill_trees();

  return true;
}

//! Reset IOC and external interface pointers
//
void 
mFvtxFindTracks::set_interface_ptrs(PHCompositeNode* top_node)
{
  // Input node(s)
  //
  _mod_par = TMutNode<mFvtxFindTrackPar>::find_node(top_node,"mFvtxFindTrackPar");
  //_mc_trk_map = TMutNode<TMutMCTrkMap>::find_node(top_node,"TMutMCTrkMap");
  //_mc_hit_map = TMutNode<TFvtxMCHitMap>::find_node(top_node,"TFvtxMCHitMap");
  _hit_map = TMutNode<TFvtxHitMap>::find_node(top_node,"TFvtxHitMap");
  _coord_map = TMutNode<TFvtxCoordMap>::find_node(top_node,"TFvtxCoordMap");
  if ( _use_svx_cluster) _svx_map = TMutNode<TFvtxSvxClusterMap>::find_node(top_node,"TFvtxSvxClusterMap");

  // Output node(s)
  //
  _trk_map = TMutNode<TFvtxTrkMap>::find_node(top_node,"TFvtxTrkMap");
}

void
mFvtxFindTracks::convert_mchits()
{
  // If the map is not present, skip conversion
  if ( ! _mc_hit_map ) return;

  // For a given arm and sector, extract the hits for all planes
  // 
  TFvtxMCHitMap::iterator hit_iter = _mc_hit_map->range();
  while ( TFvtxMCHitMap::pointer ptr = hit_iter.next() )
    {
      int arm = ptr->get()->get_arm();
      int sector = ptr->get()->get_sector();
      
      PHPoint pnt(ptr->get()->get_x(),
		  ptr->get()->get_y(),
		  ptr->get()->get_z());
      double r = sqrt(SQ(pnt.getX())+SQ(pnt.getY())); // note: we ignore any z issues for the moment
      double z = pnt.getZ();
      double phi = atan2(pnt.getY(),pnt.getX()); // returns phi on [-pi,pi]
      int cage = ptr->get()->get_cage();
      int station = ptr->get()->get_station();
      int column = ptr->get()->get_column();
      EVHit_ptr h (new EVHit());
      h->id = _hitId++;
      h->arm = arm;
      h->cage = cage;
      h->station = station;
      h->sector = sector;
      h->column = column;
      h->halfwedge = FVTXGEOM::get_halfwedge_id(arm,cage,station,sector,column);
      double phiStart, phiEnd;
      boost::tie(phiStart,phiEnd) = getPhiStartEnd(arm,cage,station,sector,column);
      h->x_hit = pnt.getX();
      h->y_hit = pnt.getY();
      h->z_hit = z;
      h->r_hit = r;
      h->phi_hit = phi;
      h->phi_start = phiStart;
      h->phi_end = phiEnd;
      h->ntracks = 0;
      h->mc_tracknum = ptr->get()->get_track_id();
      h->mc_hitnum = ptr->get()->get_index();
      _hit_vec.push_back(h);
      _hitsByStation[arm][station].push_back(h);
    }
  
  return;
}

void
mFvtxFindTracks::convert_coords()
{
  // Clear out the contents (but don't delete the pointers!) of the
  // station vectors.
  for (int i=0; i<NARMS; i++)
    {
      for(int j=0; j<NFVTX_LAYERS; j++) 
	_hitsByStation[i][j].clear();
    }

  if ( _use_svx_cluster )
    for (int i=0; i<_n_svx_layers; i++)
      _hitsByLayer[i].clear(); 

  TFvtxCoordMap::iterator coord_iter = _coord_map->range();

  while ( TFvtxCoordMap::pointer ptr = coord_iter.next() )
    {
      PHPoint cartPnt = ptr->get()->get_coord_midpoint();
      PHCylPoint pnt = cartPnt;
      double r = pnt.getR();
      double z = pnt.getZ();
      double phi = FVTXOO::angle_normalize(pnt.getPhi().getPhi());
      int arm = ptr->get()->get_arm();
      int cage = ptr->get()->get_cage();
      int station = ptr->get()->get_station();
      int sector = ptr->get()->get_sector();
      int column = ptr->get()->get_column();
      EVHit_ptr h (new EVHit());
      h->id = _hitId++;
      h->layer = -1; // Denotes that this hit did NOT come from the VTX
      h->svx_hitnum = -1; // Same
      h->arm = arm;
      h->cage = cage;
      h->station = station;
      h->sector = sector;
      h->column = column;
      h->halfwedge = FVTXGEOM::get_halfwedge_id(arm,cage,station,sector,column);
      double phiStart, phiEnd;
      boost::tie(phiStart,phiEnd) = getPhiStartEnd(arm,cage,station,sector,column);
      h->x_hit = cartPnt.getX();
      h->y_hit = cartPnt.getY();
      h->z_hit = z;
      h->r_hit = r;
      h->phi_hit = phi;
      h->phi_start = phiStart;
      h->phi_end = phiEnd;
      h->ntracks = 0;
      TFvtxMCHitMap::key_iterator it = ptr->get()->get_associated<TFvtxMCHit>();
      if ( TFvtxMCHitMap::pointer p = it.current() )
	{ 
	  h->mc_tracknum = p->get()->get_track_id();
	  h->mc_hitnum = p->get()->get_index();
	}
      h->coordnum = ptr->get()->get_index();
      _hit_vec.push_back(h);
      _hitsByStation[arm][station].push_back(h);
    }

  // XY error for VTX clusters                                                                                                                                                          
  double deltaXY = 50.0e-4/sqrt(12.0);

  // Optionally process the VTX hits as well

  if ( _use_svx_cluster )
    {
      if( _mod_par->get_verbosity() >= FVTXOO::SOME )
	std::cout << "mFvtxFindTracks::convert_coords - Converting VTX coords" << std::endl;
      
      TFvtxSvxClusterMap::iterator cluster_iter = _svx_map->range();
      while ( TFvtxSvxClusterMap::pointer ptr = cluster_iter.next() )
	{
	  int layer = ptr->get()->get_cluster()->get_layer();
	  if ( layer < _n_svx_layers ) // Only use hits from the requested VTX layers
	    {
	      // Grab the cluster from the map
	      const  SvxCluster *svx_cluster = ptr->get()->get_cluster();
	      // Calculate all the values we want to store in the EVHit format
	      double x = svx_cluster->get_xyz_global(0);
	      double y = svx_cluster->get_xyz_global(1);
	      double z = svx_cluster->get_xyz_global(2);

	      // Manually exclude the more central hits in the strip pixels
	      // No MuTr matches are possible inside of 10cm, so don't include those hits
	      if ( layer > 1 && fabs(z) < 10.0 ) continue;

	      double r = sqrt(x*x+y*y);
	      double phi = FVTXOO::angle_normalize(atan2(y,x));
	      // Need to calculate phiStart and phiEnd similar to FVTX clusters using VTX pixel sizes
	      // Using the method found in SvxClusterContainer to get the errors in x,y
	      // Then propagate these to calculate the error in phi => deltaPhi = deltaXY/r
	      // 50.0e-4 (cm) is the number used by VTX group for xy resolution in pixels	  
	      double deltaPhi = deltaXY/r; 
	  	  
	      // Create a new EVHit and fill the VTX applicable variables
	      EVHit_ptr h(new EVHit());
	      
	      h->id = _hitId++;
	      h->layer = layer;
	      h->x_hit = x;
	      h->y_hit = y;
	      h->z_hit = z;
	      h->r_hit = sqrt(x*x+y*y);
	      h->phi_hit = phi;
	      h->phi_start = phi - deltaPhi;
	      h->phi_end = phi + deltaPhi;
	      h->ntracks = 0;
	      h->svx_hitnum = svx_cluster->get_hitID();
	      
	      // Set FVTX-centric variables to -1 just in case someone decides to try to use them as such.
	      h->coordnum = -1;
	      h->arm = -1;
	      h->cage = -1;
	      h->station = -1;
	      h->sector = -1;
	      h->column = -1;
	      h->halfwedge = -1;
	      
	      // Add the constructed EVHit to total hit vector and VTX only vector sorted by layer
	      _hit_vec.push_back(h);
	      _hitsByLayer[layer].push_back(h);	  
	    }
	}
    }
}

void
mFvtxFindTracks::sort_hits()
{
  for (int i=0; i<NARMS; i++) // Loop over arms 
    { 
      for (int j=0; j<NFVTX_LAYERS; j++) // Loop over stations within this arm
	{
	  // Sorted by phi start values
	  //
	  std::sort(_hitsByStation[i][j].begin(),_hitsByStation[i][j].end(),
		    boost::bind(&EVHit::getPhiStart,_1) < boost::bind(&EVHit::getPhiStart,_2));
	}
    }
  if ( _use_svx_cluster ) 
    for (int i=0; i<_n_svx_layers; i++)
      std::sort(_hitsByLayer[i].begin(),_hitsByLayer[i].end(),
		boost::bind(&EVHit::getPhiStart,_1) < boost::bind(&EVHit::getPhiStart,_2));     

  return;
}

// Return a vector of hits for the phi window specified, sorted in r.
// Note that phi can be positive or negative, any value.  The only requirement
// is that phiStart <= phiEnd, including wrapping around the branch cut.
//
std::vector<mFvtxFindTracks::EVHit_ptr>
mFvtxFindTracks::findHits(const EVHitVec& hits, const Line1D<double>& lower, const Line1D<double>& upper)
{
  // Find the first hit to have an overlapping phi range
  //
  std::vector<mFvtxFindTracks::EVHit_ptr>::const_iterator beginHit =
    std::find_if(hits.begin(), hits.end(), bind<bool>(FVTXOO::AngleOverlap<double>(),
						      bind(&EVHit::getPhiStart,_1),
						      bind(&EVHit::getPhiEnd,_1),
						      bind(lower,bind(&EVHit::get_z,_1)),
						      bind(upper,bind(&EVHit::get_z,_1)))
		 );
  
  // Search for the last hit with overlapping phi, starting from where 
  // the previous search ended.
  //
  std::vector<mFvtxFindTracks::EVHit_ptr>::const_iterator endHit =
    std::find_if(beginHit, hits.end(), !bind<bool>(FVTXOO::AngleOverlap<double>(),
						   bind(&EVHit::getPhiStart,_1),
						   bind(&EVHit::getPhiEnd,_1),
						   bind(lower,bind(&EVHit::get_z,_1)),
						   bind(upper,bind(&EVHit::get_z,_1)))
		 );
  
  std::vector<mFvtxFindTracks::EVHit_ptr> v(beginHit,endHit);

  // Sort by radius before returning
  //
  std::sort(v.begin(),v.end(),
 	    boost::bind(&EVHit::get_r,_1) < boost::bind(&EVHit::get_r,_2));
  
  return v;
}

// Wrapper for the line1d-based version, called with constants
//
std::vector<mFvtxFindTracks::EVHit_ptr>
mFvtxFindTracks::findHits(const EVHitVec& hits, const double phiStart, const double phiEnd)
{
  Line1D<double> lower(0.0,phiStart);
  Line1D<double> upper(0.0,phiEnd);
  return findHits(hits,lower,upper);
}

// Return a vector of hits for the R window specified.
//
std::vector<mFvtxFindTracks::EVHit_ptr>
mFvtxFindTracks::findHitsR(const EVHitVec& v, double rStart, double rEnd)
{
  if ( rStart > rEnd )
    throw std::logic_error("Invalid R range in mFvtxFindTracks::findHitsR");

  EVHitVec::const_iterator beginHit = std::find_if(v.begin(),v.end(), boost::bind(&EVHit::get_r,_1) > rStart);
  EVHitVec::const_iterator endHit   = std::find_if(v.begin(),v.end(), boost::bind(&EVHit::get_r,_1) > rEnd);

  return EVHitVec(beginHit,endHit);
}

// Return a vector of hits for the R window specified.
// Assumes that the array is sorted in order of abs(dR) (ascending)
//
std::vector<mFvtxFindTracks::EVHit_ptr>
mFvtxFindTracks::findHitsDeltaR(const EVHitVec& input, double slope, double offset, double dR)
{
  // Start by sorting the input
  //
  EVHitVec v = input;
  std::sort(v.begin(),v.end(),
	    bind(std::ptr_fun<double,double>(&std::fabs),
		 bind(std::minus<double>(),bind(Line1D<double>(slope,offset),bind(&EVHit::get_z,_1)), bind(&EVHit::get_r,_1))) <
	    bind(std::ptr_fun<double,double>(&std::fabs),
		 bind(std::minus<double>(),bind(Line1D<double>(slope,offset),bind(&EVHit::get_z,_2)), bind(&EVHit::get_r,_2))));

  // The begin iterator is the start of the array
  //
  EVHitVec::const_iterator beginHit = v.begin();

  // The end iterator is determined by the search dR
  //
  EVHitVec::const_iterator endHit = 
    std::find_if(v.begin(),v.end(),
		 bind(
		      std::ptr_fun<double,double>(&std::fabs),
		      bind(std::minus<double>(),bind(Line1D<double>(slope,offset),bind(&EVHit::get_z,_1)), bind(&EVHit::get_r,_1))
		      )
		 > dR);

  return EVHitVec(beginHit,endHit);
}

void
mFvtxFindTracks::find_nStation_tracks(int arm, int nStations)
{
  double save_dPhi = _dPhi;
  double save_dR = _dR;

  int staStartMin = ( _use_svx_cluster ) ? 0 : _n_svx_layers;

  int staStartMax = _n_svx_layers+NFVTX_LAYERS-nStations;

  if ( _mod_par->get_verbosity() >= FVTXOO::SOME )
    std::cout << "Searching for " << nStations << " layer tracks" << std::endl;

  for ( int staStart = staStartMin ; staStart <= staStartMax; staStart++ )
    {
      int staEndMin = staStart+nStations-1;
      // Loop through all possible end stations 
      for ( int staEnd = staEndMin ; staEnd < _n_svx_layers+NFVTX_LAYERS ; staEnd++ )
	{
	  if ( _mod_par->get_verbosity() >= FVTXOO::SOME )
	    std::cout << "staStart: " << staStart << " staEnd: " << staEnd << std::endl;
	  for (int i = 0; i < NPASSES; i++)
	    {
	      int nfound = searchTracks<RequireNMidStationsUnusedSeeds>(arm,staStart,staEnd,nStations);
	      if ( _mod_par->get_verbosity() >= FVTXOO::SOME )
		std::cout << "Arm " << arm << ": Found " << nfound << " tracks using (dR,dPhi)="
			  << "(" << _dR << "," << _dPhi << ")" << std::endl;
      
	      // Try again now with slightly larger phi, r windows, but requiring all hits
	      // be unused.                                                                                                                         
      
	      _dPhi += 0.03;
	      if ( i < NPASSES_DR ) _dR += 0.02;
	    }  
	  _dPhi = save_dPhi;
	  _dR = save_dR;
	}
    }
}

void
mFvtxFindTracks::find_tracks(int arm)
{
  // Search thru the master list of hits, and fill a list of hits
  // unused so far.
  if( _mod_par->get_verbosity() >= FVTXOO::SOME )
    std::cout << "mFvtxFindTracks::find_tracks - Clearing usused hit vectors" << std::endl;

  for (int i=0; i<NFVTX_LAYERS; i++)
    {
      _unusedHits[i+_n_svx_layers].clear();
      std::remove_copy_if(_hitsByStation[arm][i].begin(), _hitsByStation[arm][i].end(),
			  std::back_inserter(_unusedHits[i+_n_svx_layers]), bind(&EVHit::getNtracks,_1) > 0 );
    }	      
  // Optionally do the same for the VTX
  if ( _use_svx_cluster )
    for (int i=0; i<_n_svx_layers; i++)
      {
	_unusedHits[i].clear();
	std::remove_copy_if(_hitsByLayer[i].begin(), _hitsByLayer[i].end(),
			    std::back_inserter(_unusedHits[i]), bind(&EVHit::getNtracks,_1) > 0 );
      }

  // Search through hit combinations, starting with the maximum layers per track down to 3
  for (int nStations = _n_svx_layers+NFVTX_LAYERS; nStations > 2; nStations--)
    {
      if( _mod_par->get_verbosity() >= FVTXOO::ALOT )
	std::cout << "mFvtxFindTracks::find_tracks - Searching for " << nStations << " hit tracks" << std::endl;
      find_nStation_tracks(arm,nStations);      
    }
  
  return;
}

// Checks for *exact* duplicates.  These are tracks that have all the 
// same hits.  This can happen if all combinations are allowed, and end
// overlaps are treated as separate hits.
//
void
mFvtxFindTracks::removeDuplicates(EVTrkVec& trks)
{
  std::vector<EVTrk_ptr> removed;
  for (unsigned int i=0; i<trks.size(); i++)
    {
      EVTrk_ptr t = trks[i];

      // If this track has already been removed, don't check it
      if ( find(removed.begin(),removed.end(),t) != removed.end() ) continue;

      // Loop over the rest of the list
      for( unsigned int j=i+1; j<trks.size(); j++)
	{
	  EVTrk_ptr t2 = trks[j];
	  if ( t->_hitIds == t2->_hitIds ) removed.push_back(t2);
	}
    }

  for (std::vector<EVTrk_ptr>::const_iterator it=removed.begin(); it!=removed.end(); it++)
    {
      EVTrk_ptr t = *it;
      std::vector<EVTrk_ptr>::iterator i = std::find(trks.begin(),trks.end(),t);
      if ( i != trks.end() )
	{
	  trks.erase(i);
//	  delete t;
	}
      else
	{
	  std::cout << "mFvtxFindTracks::removeDuplicates: WARNING: duplicate not found in track list!" << std::endl;
	}
    }
  return;
}

// Checks for tracks that have the "same" (similar, within cuts) parameters, 
// and applies a prescription for choosing which to keep and which to reject.
// To be developed
//
void
mFvtxFindTracks::removeSimilar(EVTrkVec& trks)
{
  return;
}

// Analytically perform a linear fit, using constant weights
//
template<typename T> std::pair<T,T>
linearFit(const std::vector<double>& x, const std::vector<double>& y)
{
  int n = x.size();
  std::vector<T> x2(x.size());
  std::vector<T> xy(x.size());
  std::transform(x.begin(),x.end(),x.begin(),x2.begin(),std::multiplies<T>());
  std::transform(x.begin(),x.end(),y.begin(),xy.begin(),std::multiplies<T>());

  T sumX  = std::accumulate(x.begin(),x.end(),0.0);
  T sumXY = std::accumulate(xy.begin(),xy.end(),0.0);
  T sumY  = std::accumulate(y.begin(),y.end(),0.0);
  T sumX2 = std::accumulate(x2.begin(),x2.end(),0.0);
  T delta = n*sumX2 - sumX*sumX;

  T slope  = 1.0/delta * (n*sumXY - sumX*sumY);
  T offset = 1.0/delta * (sumX2*sumY - sumX*sumXY);

  return std::pair<T,T>(slope,offset);
}

void
mFvtxFindTracks::removeNonOverlapCandidates(EVHitVec& v, EVHit_ptr ref)
{
  // First make sure that the reference hit is not in the list
  //
  EVHitVec::iterator new_end = std::remove(v.begin(),v.end(),ref);
  v.erase(new_end,v.end());

  // Let's attempt to handle the overlapping hits a bit smarter.  In general, overlap
  // hit have the properties:
  //
  // 1) different Z values
  // 2) same radius ids && different halfwedge ids
  // 3) different radius ids && same halfwedge ids
  //
  
  // Remove hits with the "same" Z as the reference
  new_end = 
    std::remove_if(v.begin(),v.end(),
		   boost::bind(std::ptr_fun<double,double>(&std::fabs),
			       bind(std::minus<double>(),boost::bind(&EVHit::get_z,_1),
				    ref->get_z()))
			       < 0.01
		   );


  v.erase(new_end,v.end());
  
  // Remove hits with same radius and same halfwedge Id as the reference
  //
  new_end = std::remove_if(
			   v.begin(),v.end(),
			   boost::bind(std::logical_and<bool>(),
				       boost::bind(&EVHit::getHalfWedgeId,_1) == ref->getHalfWedgeId(), 0 )
			   );
  v.erase(new_end,v.end());
  
  return;
}

template<typename T> int
mFvtxFindTracks::searchTracks(int arm, int staStart, int staEnd, int nStations)
{
  T eval;

  EVTrkVec recoTrks;

  // Currently attempts to use all VTX hits as seeds if told to do so.  This could be dangerous as
  // most VTX hits will NOT have a corresponding track in the FVTX.  Let's see what happens.
  // Most likely will have to restructure a lot of this to accomodate FVTX only seeds so let's
  // hope that isn't necessary.
  for (unsigned int i=0; i<_unusedHits[staStart].size(); i++)
    {
      EVHit_ptr h = _unusedHits[staStart][i];

      double phiStart = h->phi_start;
      double phiEnd = h->phi_end;

      //if( _mod_par->get_verbosity() >= FVTXOO::SOME )
      //	std::cout << "mFvtxFindTracks::searchTracks - phiStart: " << phiStart << " phiEnd: " << phiEnd << std::endl;
      //double sta_start_zed = h->z_hit;

      if ( eval.checkStartHit(h) == false ) continue;

      std::vector<EVHit_ptr> hits;

      // this is the station 0 track id and is used in conjunction with station 3
      // track id to determine if the track is a potential ghost track.
      //
      // TODO: reference to MC information needs to be removed.  Ghost determination should 
      // be decoupled from the pattern recognition.
      //int start_trackId = h->mc_tracknum;
	  
      // Find the possible overlap hits in the start station; BUT, we need to 
      // remove the current hit we are using for the search.
      //
      // TODO: use the seed's exact phi start, end to look for overlap candidates
      //
      //EVHitVec startSortedR = findHits(arm, staStart, phiStart, phiEnd);

      EVHitVec startSortedR = findHits(_unusedHits[staStart], phiStart-_dPhi, phiEnd+_dPhi);

      if( _mod_par->get_verbosity() >= FVTXOO::MAX )
      	std::cout << "mFvtxFindTracks::searchTracks - startSortedR size: " << startSortedR.size() << std::endl;

      // Note: this isn't actually used yet.
      hits.insert(hits.end(),startSortedR.begin(),startSortedR.end());

      removeNonOverlapCandidates(startSortedR,h);

      // Locate the end hits within the phi window to make road combinations out of.
      // Widen the search window slightly for phi
      //
      //EVHitVec endSortedR = findHits(arm, staEnd, phiStart - _dPhi, phiEnd + _dPhi);
      EVHitVec endSortedR = findHits(_unusedHits[staEnd], phiStart - _dPhi, phiEnd + _dPhi);

      if( _mod_par->get_verbosity() >= FVTXOO::MAX )
      	std::cout << "mFvtxFindTracks::searchTracks - endSortedR size: " << endSortedR.size() << std::endl;

      hits.insert(hits.end(),endSortedR.begin(),endSortedR.end());
      
      for(EVHitVec::iterator it3=endSortedR.begin(); it3!=endSortedR.end(); ++it3)
	{
	  // Start with only combinations that come roughly from the interation
	  //
	  EVHit_ptr h3 = *it3;
	  if ( h3->r_hit < h->r_hit ) continue;

	  if ( eval.checkEndHit(h3) == false ) continue;
	  
	  double sta_end_phiStart = h3->phi_start;
	  double sta_end_phiEnd = h3->phi_end;
	  //	  double sta_end_zed = h3->z_hit;

// 	  double phiStart_slope = get_phiSlope(phiStart,sta_start_zed,sta_end_phiStart,sta_end_zed);
// 	  double phiEnd_slope = get_phiSlope(phiEnd,sta_start_zed,sta_end_phiEnd,sta_end_zed); 
//           double phiStart_offset = phiStart - phiStart_slope * sta_start_zed;
// 	  double phiEnd_offset = phiEnd - phiEnd_slope * sta_start_zed;

	  // Locate the hits from the middle station that lie within the phi road
	  //
// 	  Line1D<double> lower(phiStart_slope,phiStart_offset);
// 	  Line1D<double> upper(phiEnd_slope,phiEnd_offset);
	  Line1D<double> lower(0.0,phiStart-_dPhi);
	  Line1D<double> upper(0.0,phiEnd+_dPhi);
	    
	  EVHitVec midSortedR;
	  for (int j=staStart+1; j<staEnd; j++)
	    {
	      //EVHitVec v = findHits(arm, j, phiStart, phiEnd);
	      //EVHitVec v = findHits(arm, j, lower, upper);
	      EVHitVec v = findHits(_unusedHits[j], lower, upper);
	      midSortedR.insert(midSortedR.end(),v.begin(),v.end());
	    }

	  if( _mod_par->get_verbosity() >= FVTXOO::MAX )
	    std::cout << "mFvtxFindTracks::searchTracks - midSortedR size: " << midSortedR.size() << std::endl;
	  
	  hits.insert(hits.end(),midSortedR.begin(),midSortedR.end());

	  // Widening to an array of _n_svx_layers+NFVTX_LAYERS ints to accomodate layers+stations
	  // Name is kind of unfortunate...
	  boost::shared_ptr<std::vector<int> > isStationHit (new std::vector<int>(_n_svx_layers+NFVTX_LAYERS));

	  isStationHit->at(staStart) = 1;
	  isStationHit->at(staEnd) = 1;

	  // TODO: remove reference to MC info. Ghost determination should be decoupled.
	  //int end_trackId = (*it3)->mc_tracknum;

	  // Calcluate the road in R-Z
	  //
	  double r0 = h->r_hit;
	  double z0 = h->z_hit;
	  double r3 = h3->r_hit;
	  double z3 = h3->z_hit;
	  double slope = (r3-r0)/(z3-z0);
	  double offset = r0 - slope * z0;

	  EVHitVec startHits = findHitsDeltaR(startSortedR, slope, offset, _dR);

	  EVHitVec midHits = findHitsDeltaR(midSortedR, slope, offset, _dR);
	  
	  // Process the possible overlap hits in the ending station
	  //
	  //EVHitVec endOverlaps = findHits(arm,staEnd,sta_end_phiStart,sta_end_phiEnd);
	  EVHitVec endOverlaps = findHits(_unusedHits[staEnd],sta_end_phiStart-_dPhi,sta_end_phiEnd+_dPhi);

	  removeNonOverlapCandidates(endOverlaps,h3);

	  // Also try widening the dR window a bit on the final station
	  // (not so sure this is a good idea)
	  EVHitVec endHits = findHitsDeltaR(endOverlaps, slope, offset, 0.0200);

	  // Insert the seed hits in the start and end vectors
	  startHits.push_back(h);
	  endHits.push_back(h3);

	  if( _mod_par->get_verbosity() >= FVTXOO::MAX )
	    std::cout << "mFvtxFindTracks::searchTracks - (startHits,midHits,endHits) sizes: ( " 
		      << startHits.size() << "," << midHits.size() << "," << endHits.size() << ")" << std::endl;

	  // If somehow one of these vectors is empty then there is no track
	  if ( startHits.size() == 0 ||  midHits.size() == 0 ||  endHits.size() == 0 ) continue;

	  // Require that hits pass the track evaluator before storing the candidate
	  //
	  if ( eval.checkHits(nStations,_n_svx_layers,startHits,midHits,endHits) )
	    {
	      std::vector<double> rv(startHits.size()+midHits.size()+endHits.size());
	      std::vector<double> zv(startHits.size()+midHits.size()+endHits.size());
	      std::transform(startHits.begin(),startHits.end(),zv.begin(),boost::bind(&EVHit::get_z,_1));
	      std::transform(startHits.begin(),startHits.end(),rv.begin(),boost::bind(&EVHit::get_r,_1));

	      double slopeFit, offsetFit;
	      boost::tie(slopeFit,offsetFit) = linearFit<double>(zv,rv);

	      EVTrk_ptr t (new EVTrk());
	      t->track = _trackId++;
	      t->r_slope = slope;
	      t->r_offset = offset;
	      t->trk_arm = arm;

	      // determine if track is a ghost.
	      //if ( start_trackId == end_trackId ) t->isGhost = false;
	      //else t->isGhost = true;

	      // Add the starting hit
	      // t->addHit( h );

	      // Add the possible overlap hits from the starting station
	      for (std::vector<EVHit_ptr>::iterator it=startHits.begin(); it!=startHits.end(); it++) t->addHit(*it);

	      for (std::vector<EVHit_ptr>::iterator it=midHits.begin(); it!=midHits.end(); it++)
		{		  
		  // Looks crazy but just checks to see if station is -1 (implies VTX hit)
		  // and in that case just uses the VTX layer as the isStationHit index.
		  // If station is anything other than -1 then it is a FVTX hit and the index
		  // used is the station+_n_svx_layers
		  isStationHit->at(( (*it)->station == -1 ) ? (*it)->layer : (*it)->station+_n_svx_layers ) = 1;
		  //std::auto_ptr<EVHit> hit_ptr(new EVHit(*it));
		  t->addHit(*it);//(hit_ptr);
		}

	      // Add the possible overlap hits from the ending station
	      for (std::vector<EVHit_ptr>::iterator it=endHits.begin(); it!=endHits.end(); it++) t->addHit(*it);

	      // Add the ending hit
	      //t->addHit( h3 );

	      // FVTX stations are _n_svx_layers to _n_svx_layers+NFVTX_LAYERS-1 of isStationHit
	      for (int i = 0; i < NFVTX_LAYERS; i++)
		t->nstationsHit += isStationHit->at(i+_n_svx_layers);

	      // Record the number of VTX layers hit ( 0-1 of isStationHit )
	      for (int i = 0; i < _n_svx_layers; i++)
		t->nlayersHit += isStationHit->at(i);

	      t->calculateResids();
	      recoTrks.push_back(t);
	    }
	  if( _mod_par->get_verbosity() >= FVTXOO::MAX )
	    std::cout << "mFvtxFindTracks::searchTracks - Passed evaluator block" << std::endl;
	}
    }



  // Remove any exact duplicates that might have been generated
  //
  removeDuplicates(recoTrks);

  // Remove tracks with very similar properties (consider them ghosts)
  //
  removeSimilar(recoTrks);

  // Store the results of this pass
  //
  _trk_vec.insert(_trk_vec.begin(),recoTrks.begin(),recoTrks.end());

  // Remove the hits that have been used by this pass
  // Really only need to look for hits to remove from
  // stations/layers between staStart and staEnd
  for (int i=staStart; i<=staEnd; i++)
    {
      EVHitVec::iterator new_end = 
	std::remove_if(_unusedHits[i].begin(), _unusedHits[i].end(), bind(&EVHit::getNtracks,_1) > 0 );
      _unusedHits[i].erase(new_end,_unusedHits[i].end());
    }
  
  return recoTrks.size();  
}

double
mFvtxFindTracks::get_phiSlope(
			      double sta0phi, 
			      double sta0zed,
			      double sta3phi,
			      double sta3zed
			      )
{
  if ( std::abs(sta0phi - sta3phi) > M_PI )
    {
      if ( sta0phi < M_PI )
	sta0phi += 2 * M_PI; 
      else sta0phi -= 2 * M_PI;
      }
  return (sta0phi - sta3phi)/(sta0zed - sta3zed); 
}


std::pair<double,double>
mFvtxFindTracks::getPhiStartEnd(int arm, int cage, int station, int sector, int column) const
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

  if (arm == 0){
    double _phiTmp = phi0;
    phi0 = FVTXOO::angle_normalize(phi1);
    phi1 = FVTXOO::angle_normalize(_phiTmp);
  }
  else{
    phi0 = FVTXOO::angle_normalize(phi0);
    phi1 = FVTXOO::angle_normalize(phi1);
  }

  return std::pair<double,double>(phi0,phi1);
}

void
mFvtxFindTracks::fillOutputNodes()
{
  if ( ! _trk_map ) 
    {
      // This should never happen!
      //
      throw std::runtime_error("mFvtxFindTracks::fillOutputNodes: FVTX Track map does not exist");
    }
  
  // Clear the output map
  //
  _trk_map->clear();
  
  for(std::vector<EVTrk_ptr>::iterator it=_trk_vec.begin(); it!=_trk_vec.end(); ++it)
    {
      EVTrk_ptr t = (*it);
      TFvtxTrkMap::iterator trk_iter = _trk_map->insert_new( t->trk_arm ); 
      
      // Loop over this track's hits, and associate the MCHit (if any) and the Coord
      // with the track.
      for (unsigned int i=0; i<t->getNumHits(); i++)
	{
	  int mcHitId = t->get_mc_hitnum(i);
	  int coordId = t->get_coordnum(i);
	  int svxHitId = t->get_svx_hitnum(i);
	  int arm = t->get_arm(i);
          int cage = t->get_cage(i);
	  int station = t->get_station(i);
	  int sector = t->get_sector(i);
	  int column = t->get_column(i);

	  // Ugh. Do we REALLY have to loop, loop, loop, and loop again to get to the object we want??? 
	  // There's got to be a better way.....
	  
	  if ( _mc_hit_map ) 
	    {
	      TFvtxMCHitMap::iterator mchit_iter = _mc_hit_map->get(arm,cage,station,sector,column);
	      while ( TFvtxMCHitMap::pointer ptr = mchit_iter.next() )
		{
		  if ( ptr->get()->get_index() == mcHitId ) PHKey::associate(trk_iter.current(),ptr);
		}
	    }

	  if (  _coord_map ) 
	    {
	      TFvtxCoordMap::iterator coord_iter = _coord_map->get(arm,cage,station,sector,column);
	      while ( TFvtxCoordMap::pointer ptr = coord_iter.next() )
		{
		  if ( ptr->get()->get_index() == coordId ) PHKey::associate(trk_iter.current(),ptr);
		}
	    }

	  if ( _svx_map )
	    {
	      TFvtxSvxClusterMap::iterator svx_iter = _svx_map->range();
	      while ( TFvtxSvxClusterMap::pointer ptr = svx_iter.next() )
		{
		  if ( ptr->get()->get_cluster()->get_hitID() == svxHitId ) PHKey::associate(trk_iter.current(),ptr);
		}
	    }
	}
    }
}

void 
mFvtxFindTracks::set_trk_par()
{
  PHPoint vtx(0.0,0.0,_vertex_z);

  TFvtxTrkMap::iterator trk_iter = _trk_map->range();

  while ( TFvtxTrkMap::pointer trk_ptr = trk_iter.next())
    {
      // Set total momentum guess to some average accepted value
      double pz_init = (trk_ptr->get()->get_arm()==FVTXOO::North ? 1000. : -1000.);
      double pxpz = 0.0;
      double pypz = 0.0;

      // Retrieve associated coordinates and add to track fit node:
      TFvtxCoordMap::key_iterator coord_iter( trk_ptr->get()->get_associated<TFvtxCoord>() );
      while( TFvtxCoordMap::pointer coord_ptr = coord_iter.next() )
	{
	  // For now, calculate an average px/pz, py/pz from individual 
	  // coords and use this for the slope below, because poor initial track params
	  // mess up the KF fit.
	  //
	  PHPoint CoordMidPoint = coord_ptr->get()->get_coord_midpoint();
	  pxpz += CoordMidPoint.getX()/(CoordMidPoint.getZ() - vtx.getZ());
	  pypz += CoordMidPoint.getY()/(CoordMidPoint.getZ() - vtx.getZ());
	}
      // Need to add VTX hits in here, mostly because we can have tracks that only have 1 hit in FVTX
      TFvtxSvxClusterMap::key_iterator svx_iter( trk_ptr->get()->get_associated<TFvtxSvxCluster>() );
      while( TFvtxSvxClusterMap::pointer svx_ptr = svx_iter.next() )
	{
	  pxpz += svx_ptr->get()->get_cluster()->get_xyz_global(0)/(svx_ptr->get()->get_cluster()->get_xyz_global(2) - vtx.getZ());
	  pypz += svx_ptr->get()->get_cluster()->get_xyz_global(1)/(svx_ptr->get()->get_cluster()->get_xyz_global(2) - vtx.getZ());
	}

      pxpz = pxpz/(coord_iter.count()+svx_iter.count());
      pypz = pypz/(coord_iter.count()+svx_iter.count());
      
      int charge = 1;

      TMutTrkPar trkPar(
			vtx.getX(), vtx.getY(), vtx.getZ(),   // x,y,z at trk_fit.get_z
			pz_init*pxpz, pz_init*pypz, pz_init,  // px,py,pz at trk_fit.get_z
			charge                                // charge (no knowledge here)
			);
      
      trk_ptr->get()->set_trk_par(trkPar); 
    }

  return;
}

mFvtxFindTracks::EVHit::EVHit() :
  id(0),
  event(0),
  layer(-1),
  arm(-1),
  cage(-1),
  station(-1),
  sector(-1),
  column(-1),
  halfwedge(-1),
  mc_hitnum(-1),
  mc_tracknum(-1),
  coordnum(-1),
  svx_hitnum(-1),
  r_hit(0.0),
  phi_hit(0.0),
  x_hit(0.0),
  y_hit(0.0),
  z_hit(0.0),
  phi_start(0.0),
  phi_end(0.0),
  ntracks(0),
  trackNum(0),
  px(0.0),
  py(0.0),
  pz(0.0),
  ptot(0.0),
  _mchit_ptr(0),
  _coord_ptr(0),
  _svx_ptr(0)
{ }

std::ostream& 
mFvtxFindTracks::EVHit::print(std::ostream& os) const
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
  float mc_trkid = this->mc_tracknum;
  os << "(arm,sta,sec,pla,rad,col,layer) = (" 
     << arm << "," << cage << "," << station << "," << sector << "," << column << "," << layer << "): ";
  os << "r = " << r << ", z = " << z << ", phi = " << phi << ", mctrkid = " << mc_trkid << std::endl;
  return os;
}

mFvtxFindTracks::EVTrk::EVTrk() :
  event(0),
  track(0),
  chi_2(0),
  nstationsHit(0),
  nlayersHit(0),
  size(0),
  hitnum(0),
  mc_hitnum(0),
  mc_track_id(0),
  coord_id(0),
  svx_hitnum(0),
  layer(0),
  arm(0),
  cage(0),
  station(0),
  sector(0),
  halfwedge(0),
  r(0),
  phi(0),
  zed(0),
  nshared(0),
  r_slope(0.0),
  r_offset(0.0),
  phi_slope(0.0),
  phi_offset(0.0),
  r_slopeFit(0.0),
  r_offsetFit(0.0),
  r_chi2(0.0),
  r_resid(0),
  phi_resid(0),
  r_residFit(0),
  phi_residFit(0),
  hits(0)
{ 
}

mFvtxFindTracks::EVTrk::~EVTrk()
{
  clear();
}

void
mFvtxFindTracks::EVTrk::addHit(EVHit_ptr h)
{
  hitnum.push_back( h->id );
  mc_hitnum.push_back( h->mc_hitnum );
  mc_track_id.push_back( h->mc_tracknum );
  coord_id.push_back( h->coordnum );
  svx_hitnum.push_back( h->svx_hitnum );
  layer.push_back( h->layer );
  arm.push_back( h->arm );
  cage.push_back( h->cage );
  station.push_back( h->station );
  sector.push_back( h->sector );
  halfwedge.push_back( h->halfwedge );
  r.push_back( h->r_hit );
  phi.push_back( h->phi_hit );
  zed.push_back( h->z_hit );
  _hitIds.insert(h->id);
  size++;
  h->ntracks++;

  hits.push_back(h);
}

void
mFvtxFindTracks::EVTrk::addHitNoRef(EVHit_ptr h)
{
  // Yes, it's hacky....
  addHit(h); // Add hit
  h->ntracks--; // but undo the reference count
}

void
mFvtxFindTracks::EVTrk::removeHit(EVHit_ptr h)
{
  std::vector<EVHit_ptr>::iterator it = std::find(hits.begin(),hits.end(),h);
  if ( it != hits.end() ) 
    {
      int n = std::distance(hits.begin(),it);
      hitnum.erase(hitnum.begin() + n);
      mc_hitnum.erase( mc_hitnum.begin() + n );
      mc_track_id.erase( mc_track_id.begin() + n );
      coord_id.erase( coord_id.begin() + n );
      svx_hitnum.erase( svx_hitnum.begin() + n );
      layer.erase( layer.begin() + n );
      arm.erase( arm.begin() + n );
      cage.erase( cage.begin() + n );
      station.erase( station.begin() + n );
      sector.erase( sector.begin() + n );
      halfwedge.erase( halfwedge.begin() + n );
      r.erase( r.begin() + n  );
      phi.erase( phi.begin() + n );
      zed.erase( zed.begin() + n );
      std::set<int>::iterator i = std::find(_hitIds.begin(),_hitIds.end(),h->id);
      if ( i != _hitIds.end() ) _hitIds.erase(i);
      hits.erase(it);
      h->ntracks--;
      size--;
    }
}

void
mFvtxFindTracks::EVTrk::removeHitNoRef(EVHit_ptr h)
{
  removeHit(h);
  h->ntracks++;
}

void
mFvtxFindTracks::EVTrk::clear()
{
  hitnum.clear();
  mc_hitnum.clear();
  mc_track_id.clear();
  coord_id.clear();
  svx_hitnum.clear();
  layer.clear();
  cage.clear();
  station.clear();
  sector.clear();
  halfwedge.clear();
  r.clear();
  phi.clear();
  zed.clear();
  r_resid.clear();
  phi_resid.clear();
  size = 0;
  for (std::vector<EVHit_ptr>::iterator i=hits.begin(); i!=hits.end(); i++) (*i)->ntracks--;
  hits.clear();
}

template<typename T> struct Dereference : std::unary_function<T,T*> {
  T operator()(T* p) { return *p; }
};

void
mFvtxFindTracks::EVTrk::prepare() 
{
  calculateNShared();
}

void
mFvtxFindTracks::EVTrk::calculateNShared()
{
  nshared = 0;
  for (std::vector<EVHit_ptr>::iterator it = hits.begin(); it!=hits.end(); ++it)
    {
      EVHit_ptr h = *it;
      if ( h->ntracks > 1 ) nshared++;
    }
  return;
}

void
mFvtxFindTracks::EVTrk::calculateResids()
{
  r_resid.clear();
  for ( std::vector<EVHit_ptr>::iterator it = hits.begin(); it!=hits.end(); ++it)
    {
      EVHit_ptr h = *it;
      double z = h->z_hit;
      double dr = r_slope*z + r_offset - h->r_hit;
      r_resid.push_back(dr);
    }

  linearFit();

  r_residFit.clear();
  for ( std::vector<EVHit_ptr>::iterator it = hits.begin(); it!=hits.end(); ++it)
    {
      EVHit_ptr h = *it;
      double z = h->z_hit;
      double dr = r_slopeFit*z + r_offsetFit - h->r_hit;
      r_residFit.push_back(dr);
    }
  std::vector<double> resid2(r_residFit.size());
  std::transform(r_residFit.begin(),r_residFit.end(),
		 r_residFit.begin(),resid2.begin(),std::multiplies<double>());
  r_chi2 = std::accumulate(resid2.begin(),resid2.end(),0.0);

  return;
}

void
mFvtxFindTracks::EVTrk::linearFit()
{
  std::vector<double> z2(zed.size());
  std::vector<double> zr(zed.size());
  std::transform(zed.begin(),zed.end(),zed.begin(),z2.begin(),std::multiplies<double>());
  std::transform(zed.begin(),zed.end(),r.begin(),zr.begin(),std::multiplies<double>());
  int n = getNumHits();
  double sumZ = std::accumulate(zed.begin(),zed.end(),0.0);
  double sumZR = std::accumulate(zr.begin(),zr.end(),0.0);
  double sumR = std::accumulate(r.begin(),r.end(),0.0);
  double sumZ2 = std::accumulate(z2.begin(),z2.end(),0.0);
  double delta = n*sumZ2 - sumZ*sumZ;
  r_slopeFit = 1.0/delta * (n*sumZR - sumZ*sumR);
  r_offsetFit = 1.0/delta * (sumZ2*sumR - sumZ*sumZR);
}

void
mFvtxFindTracks::EVTrk::sortHitsZ()
{
  // Sort EVHits by ascending value of |z| using the member function get_z.
  //
  std::sort(hits.begin(), hits.end(),
	    boost::bind(std::ptr_fun<double,double>(&std::fabs),boost::bind(&EVHit::get_z,_1)) < 
	    boost::bind(std::ptr_fun<double,double>(&std::fabs),boost::bind(&EVHit::get_z,_2)) );
}

// Count the number of hits the track has that have #used > 1
unsigned int 
mFvtxFindTracks::EVTrk::getNShared() const 
{
  return std::count_if(hits.begin(),hits.end(),
		       boost::bind(std::greater<int>(),boost::bind(&EVHit::getNtracks,_1),1));
}
