// $Id: mMutFindClus.cxx,v 1.35 2011/12/24 04:48:29 slash Exp $

/*!
  \file mMutFindClus.cxx
  \brief Finds groups of contiguous hit strips
  \author S.Kelly, H.Pereira
  \version $Revision: 1.35 $
  \date $Date: 2011/12/24 04:48:29 $
*/

// MUTOO headers
#include "mMutFindClus.h"
#include "mMutFindClusPar.h"
#include <TMutNode.h>
#include <TMutHitMap.h>
#include <TMutClusMap.h>
#include <PHException.h>
#include <MUTOO.h>
#include <MUTGEOM.h>
#include <MutStrip.h>

#include <boost/array.hpp>
#include <cmath>
#include <iostream>
#include <string>
#include <climits>
#include <cfloat>

using namespace std;

//_____________________________________________________________
mMutFindClus::mMutFindClus() :
  _use_section(false),
  _arm(0),
  _station(0),
  _octant(0),
  _timer( PHTimeServer::get()->insert_new("mMutFindClus") )
{
  MUTOO::TRACE("initializing module mMutFindClus",MUTOO::ALOT);
}

//_____________________________________________________________
PHBoolean mMutFindClus::event(PHCompositeNode* top_node)
{

  _timer.get()->restart();

  try {

    // Reset IOC pointers
    set_interface_ptrs(top_node);

    // Associate groups of contiguous hits with  TMutClus objects
    find_clusters();

    // update cluster status based on belonging hits
    set_cluster_status();

    // Apply any run-time specified cluster cuts
    if( _mod_par->get_do_cluster_cuts() ) { apply_cluster_cuts(); }
    // else { cout << "mMutFindClus::event - cluster cuts disabled." << endl; }

    // some dump
    if(_mod_par->get_verbosity() >= MUTOO::ALOT) _clus_map->print();

  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
    return False;
  }

  // If verbose dump the contents of the cluster map
  _timer.get()->stop();

  return True;

}


//_____________________________________________________________
PHBoolean mMutFindClus::event(
  PHCompositeNode* top_node,
  const unsigned short& arm,
  const unsigned short& station,
  const unsigned short& octant )
{

  _timer.get()->restart();

  try {

    _use_section = true;
    _arm = arm;
    _station = station;
    _octant = octant;

    // Number of gaps varies station to station
    unsigned short n_gaps = TMutGeo::get_n_gaps( _arm, _station);

    // Associate groups of contiguous hits with  TMutClus objects
    for( int half = 0; half < MUTOO::NumberOfHalfOctants; ++half)
    {
      for( int gap = 0; gap < n_gaps; ++gap )
      {
        for( int cathode = 0; cathode < MUTOO::NumberOfCathodePlanes; cathode++ )
        { find_clusters( _arm, _station, _octant, half, gap, cathode ); }
      }
    }

    // update cluster status based on belonging hits
    set_cluster_status();

    // Apply any run-time specified cluster cuts
    if( _mod_par->get_do_cluster_cuts() ) { apply_cluster_cuts(); }
    // else { cout << "mMutFindClus::event - cluster cuts disabled." << endl; }

    // reset _use_section flag
    _use_section = false;

    // dumps
    if(_mod_par->get_verbosity() >= MUTOO::ALOT) _clus_map->print();

  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
    return False;
  }

  // If verbose dump the contents of the cluster map
  _timer.get()->stop();

  return True;
}

//_____________________________________________________________
void mMutFindClus::set_interface_ptrs(PHCompositeNode* top_node)
{

  _mod_par = TMutNode<mMutFindClusPar>::find_node(top_node,"mMutFindClusPar");
  _hit_map = TMutNode<TMutHitMap>::find_node(top_node,"TMutHitMap");
  _clus_map = TMutNode<TMutClusMap>::find_node(top_node,"TMutClusMap");
  _clus_map->clear();

}

//_____________________________________________________________
void mMutFindClus::find_clusters()
{

  // Loop over all cathodes
  for( int arm = 0; arm < MUTOO::NumberOfArms; ++arm)
  {

    // keeps track of which octant of the previous station has clusters
    boost::array<bool,MUTOO::NumberOfOctants> octant_mask;
    octant_mask.assign( false );

    for( int station = MUTOO::NumberOfStations-1; station >= 0; --station )
    {
      // Number of gaps varies station to station
      unsigned short n_gaps = TMutGeo::get_n_gaps( arm, station);

      for( int octant = 0; octant < MUTOO::NumberOfOctants; ++octant)
      {

        /*
        check that there are clusters in the same octant of previous station
        since anyway we don't reconstruct tracks which cross octants, we skipp
        octants when there is no hit in the previous station
        */

        if( station < MUTOO::Station3 && !octant_mask[ octant ] )
        {

          if( _mod_par->get_verbosity() >= MUTOO::MAX )
            cout << "mMutFindClus::find_clusters - no clusters in [" << arm << "," << station+1 << "," << octant << "] "
            << " skipping ["<< arm << "," << station << "," << octant << "]" << endl;
          continue;
        }

        for( int half = 0; half < MUTOO::NumberOfHalfOctants; ++half)
        for( int gap = 0; gap < n_gaps; ++gap )
        for( int cathode = 0; cathode < MUTOO::NumberOfCathodePlanes; cathode++ )
        {
          if( find_clusters( arm, station, octant, half, gap, cathode ) ) octant_mask[ octant ] = true;
        }

      }
    }
  }

}

//_____________________________________________________________
bool mMutFindClus::find_clusters(
  const int& arm,
  const int& station,
  const int& octant,
  const int& half_octant,
  const int& gap,
  const int& cathode )
{

  // We use the fact that the TMutHitMap is sorted
  // according to arm/station/octant/half_octant/gap/cathode/strip.

  // loop over Hits
  //	test whether strip number strictly increasing at same cathode location
  //	make new cluster if strip not contiguous to strip of previous hit
  //	associate hit to cluster
  // endloop

  int previous_strip = -1; // necessary for valgrind
  TMutClusMap::pointer cluster_ptr = 0; // pointer to current cluster
  TMutHitMap::pointer prev_hit_ptr = 0; // pointer to previous hit

  // Iterator to all hits in hit map
  TMutHitMap::iterator hit_iter = _hit_map->get( arm, station, octant, half_octant, gap, cathode );
  while(TMutHitMap::pointer hit_ptr = hit_iter.next())
  {
    // Strip number of current hit
    int current_strip = hit_ptr->get()->get_strip();

    // make a new cluster
    // if the current hit is the first one (no current cluster yet)
    // or the cathode locations of the current and previous hits are different
    // (arm/station/octant/half_octant/gap/cathode)
    // or the strip number has increased by more than 2
    // (more than one missing strip between adjacent clusters).
    // Would be shorter if comparison directly possible
    // between MUTOO::cathode_locator's.
    bool new_cluster = (cluster_ptr == 0) || (current_strip > (previous_strip + 2) );
    if( new_cluster  && _mod_par->get_verbosity() >= MUTOO::MAX ) cout << "mMutFindClus::find_clusters - creating new cluster" << endl;
    if( !new_cluster ) {

      if (current_strip == (previous_strip + 2)) {

        // The strip number has increased by 2 (one strip missing).
        // One asks to make a new cluster
        // if merging adjacent clusters with one missing strip is not allowed.
        if (!(_mod_par->get_merge_adjacent_clusters())) new_cluster = true;
        else {

          // Otherwise:
          // one strip missing and merging adjacent clusters is allowed.
          if (_mod_par->get_verbosity() >= MUTOO::MAX) cout << "one strip missing between adjacent clusters" << endl;

          // One asks to make a new cluster
          // if the missing strip is not to be excused.
          if ( !excuse_strip(hit_ptr->get()->get_location(), current_strip - 1) ) new_cluster = true;
          else {
            if (_mod_par->get_verbosity() >= MUTOO::MAX) cout << "missing strip to be excused" << endl;

            // the missing strip is to be excused.
            // Insert the missing strip in the hit map
            TMutHitMap::pointer missingHit_ptr =
              _hit_map->insert_new (hit_ptr->get()->get_arm(),
                  hit_ptr->get()->get_station(),
                  hit_ptr->get()->get_octant(),
                  hit_ptr->get()->get_half_octant(),
                  hit_ptr->get()->get_gap(),
                  hit_ptr->get()->get_cathode(),
                  current_strip - 1).current();

            // with charge (q) = average of charges of adjacent strips
            float missingStripCharge =	0.5 * (hit_ptr->get()->get_q() + prev_hit_ptr->get()->get_q());
            missingHit_ptr->get()->set_q(missingStripCharge);

            // and error on this charge (error_q) very large
            missingHit_ptr->get()->set_error_q(10.0 * missingStripCharge);
            if (_mod_par->get_verbosity() >= MUTOO::MAX) {
              cout << "CURRENT CLUSTER" << endl;
              cluster_ptr->get()->print();
            }

            // Associate the missing strip to the current cluster
            PHKey::associate(missingHit_ptr, cluster_ptr);
            if (_mod_par->get_verbosity() >= MUTOO::MAX) {
              cout << "MISSING HIT" << endl;
              missingHit_ptr->get()->print();
              cout << "UPDATED CURRENT CLUSTER" << endl;
              cluster_ptr->get()->print();
              cout << "NEXT ADJACENT HIT" << endl;
              hit_ptr->get()->print();
            }
          }
        }
      }
    }

    // One makes a new cluster if required by previous code
    if( new_cluster ) cluster_ptr = _clus_map->insert_new(hit_ptr->get()->get_location()).current();

    // Associate the current hit to the current cluster
    PHKey::associate(hit_ptr, cluster_ptr);

    // Update current strip number and pointer to previous hit
    previous_strip = current_strip;
    prev_hit_ptr = hit_ptr;
  }

  // returns true if at least one cluster was created
  return (cluster_ptr);

}

//_____________________________________________________________
bool mMutFindClus::excuse_strip(const MUTOO::cathode_locator& cathodeLocator, int strip)
{

  // Missing strip to be excused if it is dead
  if ((TMutGeo::get_strip_geom(cathodeLocator, strip))->ChannelIsDead())
  {
    if (_mod_par->get_verbosity() >= MUTOO::MAX)
    { cout << "Missing strip to be excused: dead" << endl; }
    return true;
  }

  // Otherwise
  MutCalibStrip *mutCalibStrip_ptr = MutCalib();
  const PdbMutCalibStrip *pdbMutCalibStrip_ptr =
    mutCalibStrip_ptr->getPdbMutCalibStrip(cathodeLocator.get<0>(),
    cathodeLocator.get<1>(),
    cathodeLocator.get<2>(),
    cathodeLocator.get<3>(),
    cathodeLocator.get<4>(),
    cathodeLocator.get<5>(),
    strip);

  // Missing strip to be excused if no calibration for it
  if (pdbMutCalibStrip_ptr == 0)
  {
    if (_mod_par->get_verbosity() >= MUTOO::MAX)
      cout << "Missing strip to be excused: no calibration" << endl;
    return true;
  }

  // Otherwise
  // Missing strip to be excused if calibration is not valid
  if (!(pdbMutCalibStrip_ptr->isValid()))
  {
    if (_mod_par->get_verbosity() >= MUTOO::MAX)
      cout << "Missing strip to be excused: calibration not valid" << endl;
    return true;
  }

  // Otherwise
  // Missing strip to be excused if it has too low gain,
  // with gain limit to be taken from mMutFindClusPar
  if (pdbMutCalibStrip_ptr->get_gain() < _mod_par->get_min_gain())
  {
    if (_mod_par->get_verbosity() >= MUTOO::MAX)
      cout << "Missing strip to be excused: too low gain = "
      << pdbMutCalibStrip_ptr->get_gain()
      << " w.r.t. " << _mod_par->get_min_gain() << endl;
    return true;
  }

  // Otherwise
  // Missing strip not to be excused
  return false;
}

//_____________________________________________________________
void mMutFindClus::apply_cluster_cuts( void )
{

  TMutClusMap::iterator clus_iter = (_use_section) ? _clus_map->get( _arm, _station, _octant ) : _clus_map->range();
  while(TMutClusMap::pointer clus_ptr = clus_iter.next())
  apply_cluster_cuts( clus_ptr );

  return;
}

//_____________________________________________________________
void mMutFindClus::set_cluster_status( void )
{

  TMutClusMap::iterator clus_iter = (_use_section) ? _clus_map->get( _arm, _station, _octant ) : _clus_map->range();
  while(TMutClusMap::pointer clus_ptr = clus_iter.next())
  set_cluster_status( clus_ptr );

  return;
}

//_____________________________________________________________
void mMutFindClus::set_cluster_status( TMutClusMap::pointer clus_ptr )
{

  // retrieve associated hits
  TMutHitMap::const_key_iterator hit_iter( clus_ptr->get()->get_associated<TMutHit>() );
  while( TMutHitMap::const_pointer hit_ptr = hit_iter.next() )
  {

    // strip is on the edge
    if( hit_ptr->get()->get_is_edge() ) clus_ptr->get()->set_edge_strip();

    // propagate strip attenuation flag to cluster
    if( hit_ptr->get()->get_is_attenuated() ) clus_ptr->get()->set_attenuated_strip();

    // flag bad hits
    if( hit_ptr->get()->get_is_bad() ) clus_ptr->get()->set_bad_strip();

    // flag saturated hits
    if( hit_ptr->get()->get_is_saturated() ) clus_ptr->get()->set_saturated_strip();

  }

  return;

}

//_____________________________________________________________
void mMutFindClus::apply_cluster_cuts( TMutClusMap::pointer clus_ptr )
{

  // retrieve pointer to calibrations
  MutCalibStrip *CalibPointer = MutCalib();

  // If cluster contains less than the required number of strips remove from map
  if(clus_ptr->get()->get_n_strip() < _mod_par->get_min_cluster_width() )
  {
    _clus_map->erase(clus_ptr->get()->get_key());
    return;
  }

  // If cluster contains more than the required number of strips remove from map
  if(clus_ptr->get()->get_n_strip() > _mod_par->get_max_cluster_width() )
  {
    _clus_map->erase(clus_ptr->get()->get_key());
    return;
  }

  float charge_sum = 0;
  float charge_rms = 0;
  float max_charge = -FLT_MAX;

  unsigned short min_strip = USHRT_MAX;
  unsigned short max_strip = 0;
  unsigned short peak_strip = 0;

  unsigned short arm = clus_ptr->get()->get_arm();
  unsigned short station = clus_ptr->get()->get_station();
  unsigned short octant = clus_ptr->get()->get_octant();
  unsigned short half = clus_ptr->get()->get_half_octant();
  unsigned short gap = clus_ptr->get()->get_gap();
  unsigned short cath = clus_ptr->get()->get_cathode();

  // retrieve number of associated strips
  // find first, peak and last strip, total charge and calibrations
  TMutHitMap::const_key_iterator hit_iter = clus_ptr->get()->get_associated<TMutHit>();
  unsigned short n_strips = hit_iter.count();
  while(TMutHitMap::const_pointer hit_ptr = hit_iter.next())
  {

    charge_sum += hit_ptr->get()->get_q();
    charge_rms += MUTOO::SQUARE( hit_ptr->get()->get_q() );
    peak_strip = (hit_ptr->get()->get_q() > max_charge) ? hit_ptr->get()->get_strip() : peak_strip;

    // retrieve strip calibrations
    const PdbMutCalibStrip *StripCalib = CalibPointer->getPdbMutCalibStrip(
      arm,
      station,
      octant,
      half,
      gap,
      cath,
      hit_ptr->get()->get_strip());

    // From Hugo: the handling of set_peak_bound is
    // inconsistent with the cut below, applied to neighbour strips
    if (StripCalib)
    {
      if (StripCalib->isValid())
      {
        if (StripCalib->get_gain()<_mod_par->get_min_gain()) clus_ptr->get()->set_peak_bound();
      } else clus_ptr->get()->set_peak_bound();

    }

    max_charge = std::max(hit_ptr->get()->get_q(), max_charge);
    min_strip =	std::min(min_strip,hit_ptr->get()->get_strip());
    max_strip =	std::max(max_strip,hit_ptr->get()->get_strip());

  }

  // check mean charge per strip agains cuts
  // check mean charge per strip agains cuts
  float mean_charge = charge_sum/n_strips;
  charge_rms = sqrt( charge_rms/n_strips - MUTOO::SQUARE( mean_charge ) );

  if( n_strips>3 &&
      mean_charge <= _mod_par->get_min_mean_charge( arm, station ) &&
      charge_rms <= _mod_par->get_min_charge_rms( arm, station ) )
  {
    _clus_map->erase(clus_ptr->get()->get_key());
    return;
  }

  // get cathode plane index (Mutgeom conventions) and number of strips in cathode
  unsigned short CathodePlane( (cath == 0) ? MUTGEOM::Cathode1:MUTGEOM::Cathode2 );
  unsigned short NumberOfStrips( TMutGeo::get_n_strip(arm, station, octant, half, gap, CathodePlane) - 1 );

  // check cluster location with respect to edges
  if (min_strip == 0 || max_strip == NumberOfStrips) clus_ptr->get()->set_peak_bound();
  else {

    // check if we are next to a dead strip
    // Note (from Hugo): this is inconsistant with the cut above, applied to cluster strips
    // basically the same flag is used for two completely different issues
    const PdbMutCalibStrip *StripCalib = CalibPointer->getPdbMutCalibStrip(
      arm,
      station,
      octant,
      half,
      gap,
      cath,
      min_strip - 1);

    // set 'peak_bound' flag
    if(  (!(StripCalib && StripCalib->isValid() ) ) || StripCalib->get_gain()<_mod_par->get_min_gain())
    { clus_ptr->get()->set_peak_bound(); }

    // get strip
    StripCalib = CalibPointer->getPdbMutCalibStrip(
      arm,
      station,
      octant,
      half,
      gap,
      cath,
      max_strip + 1);

    if(  (!(StripCalib && StripCalib->isValid() ) ) || StripCalib->get_gain()<_mod_par->get_min_gain())
    { clus_ptr->get()->set_peak_bound(); }

  }


  // For > 3 wide check that the peak strip is not at the boundary.
  // This cut is *not* applied to station3 where we cannont affort
  // the inefficiency hit because of only 2 gaps.

  // from hugo: this code is weird: nothing is done and the other cuts are not applied
  // this is likely to be a typo. To be confirmed with Melynda
  if(
    (clus_ptr->get()->get_station() != MUTOO::Station3	&& clus_ptr->get()->get_n_strip() > 3) &&
    (peak_strip == min_strip || peak_strip == max_strip) )
    return;

  // If the total charge on the cluster is below minimum value remove from map
  if(charge_sum < _mod_par->get_min_charge_sum())
  {
    _clus_map->erase(clus_ptr->get()->get_key());
    return;
  }

  /*
  TODO: Get these from the parameter table
  also: it does not get much sense, since the charge has a somewhat arbitrary normalization
  from gap to gap, or even from octant to octant due to problem of injection wire termination
  in the (hardware) calibration system
  */
  if(charge_sum < 10) clus_ptr->get()->set_low_charge();
  if(charge_sum > 175) clus_ptr->get()->set_high_charge();

  return;

}
