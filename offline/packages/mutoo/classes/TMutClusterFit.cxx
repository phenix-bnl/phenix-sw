// $Id: TMutClusterFit.cxx,v 1.41 2018/05/21 21:23:39 pinkenbu Exp $
/*
  \file TMutClusterFit.cxx
  \author: H. Pereira
  \version $Revision: 1.41 $
  \date $Date: 2018/05/21 21:23:39 $
  \brief
  Encapsulates the cluster fit algorithm do be used by different modules
  including mMutFitCluster
*/

#include "MUTOO_FEM.h"
#include "PHException.h"
#include "TMutCathErrors.h"
#include "TMutClusterFit.h"
#include "TMutClusterFitEval.h"
#include "TMutGeo.h"
#include "TMutMathieson.h"
#include "TMutMathiesonPar.h"
#include "TMutMCHitMap.h"

// MUTGEOM
#include <MutCalib.h>
#include <MutCalibStrip.hh>
#include <MutGeom.h>
#include <MUTGEOM.h>

// STL
#include <iostream>

// GSL
#include <gsl/gsl_math.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_multifit_nlin.h>
#include <gsl/gsl_version.h>

using namespace std;

// static variables initialization
MUTOO::Verbosity TMutClusterFit::_verbosity = MUTOO::NONE;
TMutClusterFit::FitFunctionPtr TMutClusterFit::_single_trk_fit_funtion = &TMutClusterFit::mathieson_lookup;
TMutClusterFit::FitFunctionPtr TMutClusterFit::_multi_trk_fit_funtion = &TMutClusterFit::gsl_mathieson_fit;

// customizable fit parameters
double TMutClusterFit::_peak_ratio_scale = 0.48;
double TMutClusterFit::_chi_max_good_fit = 5;
double TMutClusterFit::_mc_smear_perp = 0;
double TMutClusterFit::_mc_smear_ster = 0;
int TMutClusterFit::_max_fit = 15;

// flags
bool TMutClusterFit::_multi_track_fit = true;
bool TMutClusterFit::_use_mc = false;
bool TMutClusterFit::_use_mc_ntracks = false;
bool TMutClusterFit::_use_fit_error = false;
bool TMutClusterFit::_use_zerosup_strips = true;
bool TMutClusterFit::_use_fitfunc_zerosup = false;

// // use gsl error when larger than pool error
// bool TMutClusterFit::_use_hybridmax_error = true;
//
// // force three wide cluster to be fitted with only one track
// bool TMutClusterFit::_force_three_wide_clusters = true;

//
bool TMutClusterFit::_use_hybridmax_error = true;
bool TMutClusterFit::_force_three_wide_clusters = true;

// gsl fit function parameters (non customizable)
double TMutClusterFit::_tanhs[4][MUTOO::NumberOfArms][MUTOO::NumberOfStations][TMutClusterFit::NumberOfCathodes][MUTOO::NumberOfOctants];

// numerical parameters
static const double qk3 = 0.5;
static const double sqrt_qk3 = sqrt( qk3 );
static const double qk2 = M_PI/2.0 * (1.0 - 0.5 * sqrt_qk3 ) ;
static const double qk1 =  2.0 * qk2 * sqrt_qk3 / (4.0 * atan( sqrt_qk3 ) );
static const double A1 = qk1 / qk2 / sqrt(qk3);
static const double A2init = qk1 / qk2 / sqrt(qk3);

// gsl random number initialization (for use on MC hacking of the clusters)
PHGslRng TMutClusterFit::_rng;

//________________________________________________________________
bool TMutClusterFit::fit( TMutClusMap::pointer clus_ptr )
{

  // Local sortable storage for hit pointers
  sample_list samples;
  TMutHitMap::key_iterator hit_iter = clus_ptr->get()->get_associated<TMutHit>();
  while( TMutHitMap::pointer hit_ptr = hit_iter.next() )
  {
    if( _verbosity >= MUTOO::ALOT )
    cout << "TMutClusterFit::fit - hit: " << hit_ptr->get()->get_key().get_obj_key() << " q: " << hit_ptr->get()->get_q() << endl;
    samples.push_back( hit_ptr );
  }

  if( !samples.size() )  throw std::logic_error( DESCRIPTION( "cluster has no associated hits" ) );

  // Handle single hit clusters
  if (samples.size() == 1) {
    single_hit_cluster(samples, clus_ptr);
    return true;
  }

  // Sort the hits according to decreasing charge.
  sort(samples.begin(),samples.end(),greater_q_ftor());

  // If cluster has more than 2 hit strips send to the multi_trk_fit funtion.
  if( _multi_track_fit && samples.size()>2) (*_multi_trk_fit_funtion)(samples, clus_ptr);
  else (*_single_trk_fit_funtion)(samples, clus_ptr);

  return true;

}

// //________________________________________________________________
// void TMutClusterFit::set_use_fit_error( const bool use_fit_error )
// {
//   _use_fit_error = use_fit_error;
//   return;
// }

//________________________________________________________________
void TMutClusterFit::set_single_track_fit_type( const FitType& fit_type )
{
  switch( fit_type ) {

    case PEAK_RATIO:
      _single_trk_fit_funtion = &TMutClusterFit::peak_ratio_fit;
      break;

    case MATHIESON_LOOKUP:
      _single_trk_fit_funtion = &TMutClusterFit::mathieson_lookup;
      break;

    case GSL_MATHIESON:
      _use_mc = false;
      _use_mc_ntracks = false;
      _single_trk_fit_funtion = &TMutClusterFit::gsl_mathieson_fit;
      break;

    case MC_GSL_MATHIESON:
      _use_mc = true;
      _use_mc_ntracks = false;
      _single_trk_fit_funtion = &TMutClusterFit::gsl_mathieson_fit;
      break;

    case MC_TRK_GSL_MATHIESON:
      _use_mc = false;
      _use_mc_ntracks = true;
      _single_trk_fit_funtion = &TMutClusterFit::gsl_mathieson_fit;
      break;

    case CENTER_PEAK:
      _single_trk_fit_funtion = &TMutClusterFit::center_peak_fit;
      break;

    case MC:
      _single_trk_fit_funtion = &TMutClusterFit::mc_fit;
      break;

    default:
      throw logic_error( DESCRIPTION( "wrong fit type" ) );
      break;
  }

}

//________________________________________________________________
void TMutClusterFit::set_multi_track_fit_type( const FitType& fit_type )
{
  switch( fit_type ) {

    case PEAK_RATIO:
      _multi_trk_fit_funtion = &TMutClusterFit::peak_ratio_fit;
      break;

    case MATHIESON_LOOKUP:
      _multi_trk_fit_funtion = &TMutClusterFit::mathieson_lookup;
      break;

    case GSL_MATHIESON:
      _use_mc = false;
      _use_mc_ntracks = false;
      _multi_trk_fit_funtion = &TMutClusterFit::gsl_mathieson_fit;
      break;

    case MC_GSL_MATHIESON:
      _use_mc = true;
      _use_mc_ntracks = false;
      _multi_trk_fit_funtion = &TMutClusterFit::gsl_mathieson_fit;
      break;

    case MC_TRK_GSL_MATHIESON:
      _use_mc = false;
      _use_mc_ntracks = true;
      _multi_trk_fit_funtion = &TMutClusterFit::gsl_mathieson_fit;
      break;

    case CENTER_PEAK:
      _multi_trk_fit_funtion = &TMutClusterFit::center_peak_fit;
      break;

    case MC:
      _multi_trk_fit_funtion = &TMutClusterFit::mc_fit;
      break;

    default:
      throw logic_error( DESCRIPTION( "wrong fit type" ) );
      break;
  }

}

//________________________________________________________________
void TMutClusterFit::print_tanhs( ostream& out )
{
  MUTOO::PRINT( out, "TMutClusterFit::print_tanhs" );
  for( int arm=0; arm < MUTOO::NumberOfArms; arm++ )
  for( int station = 0; station < MUTOO::NumberOfStations; station++ )
  for( int cathode = 0; cathode < NumberOfCathodes; cathode++ )
  out << "_tanhs[" << arm << "," << station << "," << cathode << "]=["
    << _tanhs[0][arm][station][cathode] << " "
    << _tanhs[1][arm][station][cathode] << " "
    << _tanhs[2][arm][station][cathode] << " "
    << _tanhs[3][arm][station][cathode] << "]"
    << endl;

  MUTOO::PRINT( out, "**" );
}


//________________________________________________________________
void TMutClusterFit::single_hit_cluster( sample_list& samples, TMutClusMap::pointer clus_ptr)
{

  // get first (and only) hit
  TMutHit * hit_ptr( samples.front()->get() );

  // For single hit clusters we assign 0 offset and unit error
  TMutClusCentroid *centroid( clus_ptr->get()->insert_new_centroid( hit_ptr->get_strip() ) );
  centroid->set_w( 0 );

  /*
    it is surprising that the error is set to half the strip width.
    In principle it should be set to the strip_width/sqrt(12), which
    corresponds to 1 standard deviation for a box distribution
    j.nagle - made the change below 07-03-2007
  */
  centroid->set_w_error( 1.0/sqrt(12.0) );
  centroid->set_q_peak( hit_ptr->get_q() );
  centroid->set_q_tot( hit_ptr->get_q() );
  centroid->set_q_tot_error( hit_ptr->get_error_q() );

}

//________________________________________________________________
void TMutClusterFit::peak_ratio_fit( sample_list& samples, TMutClusMap::pointer clus_ptr)
{

  // check that we have a cluster that is at least 2 strips wide
  if(samples.size()<2){
    MUTOO::TRACE("TMutClusterFit::peak_ratio_fit: bad cluster width");
    return;
  }

  // Calculate total charge
  double q_total=0;
  double q_tot_error = 0;
  sample_list::iterator iter = samples.begin();

  while(iter!=samples.end()){
    q_total += (*iter)->get()->get_q();
    q_tot_error += MUTOO::SQUARE((*iter)->get()->get_error_q());
    ++iter;
  }

  q_tot_error = sqrt(q_tot_error);

  // Sort the hits according to decreasing charge.
  sort(samples.begin(),samples.end(),greater_q_ftor());

  // Get the geometry of the peak strip;
  const MutStrip* strip_geo = TMutGeo::get_strip_geom(samples[0]->get()->get_location(), samples[0]->get()->get_strip());

  // Figure out the strip spacing for this strip
  double strip_space = 1;
  if(strip_geo) {
    strip_space = strip_geo->getStripSpacing();
  } else {
    MUTOO::TRACE("TMutClusterFit::peak_ratio_fit: TMutGeom returned null pointer");
  }

  // After sort peak charge is first
  double q_peak = samples[0]->get()->get_q();
  double scale = _peak_ratio_scale;
  double offset = (scale - q_peak/q_total)/(scale - 0.5)*strip_space;

  // if 2nd highest has smaller strip number offset is negative
  if(samples[1]->get()->get_strip() < samples[0]->get()->get_strip()) {
    offset *= -1.0;
  }

  TMutClusCentroid *centroid( clus_ptr->get()->insert_new_centroid( samples[0]->get()->get_strip() ) );
  centroid->set_w( offset );
  centroid->set_w_error( 1.0 );
  centroid->set_q_peak( q_peak );
  centroid->set_q_tot( q_total );
  centroid->set_q_tot_error( q_tot_error );

}

//________________________________________________________________
void TMutClusterFit::center_peak_fit( sample_list& samples, TMutClusMap::pointer clus_ptr)
{

  // Calculate total charge
  double q_total=0;
  double q_tot_error=0;
  sample_list::iterator iter = samples.begin();
  while(iter!=samples.end()){
    q_total += (*iter)->get()->get_q();
    q_tot_error += MUTOO::SQUARE((*iter)->get()->get_error_q());
    ++iter;
  }

  q_tot_error = sqrt(q_tot_error);

  // Sort the hits according to decreasing charge.
  sort(samples.begin(),samples.end(),greater_q_ftor());

  double offset = 0;
  double q_peak = samples[0]->get()->get_q();

  TMutClusCentroid *centroid( clus_ptr->get()->insert_new_centroid( samples[0]->get()->get_strip() ) );
  centroid->set_w( offset );
  centroid->set_w_error( 1.0 );
  centroid->set_q_peak( q_peak );
  centroid->set_q_tot( q_total );
  centroid->set_q_tot_error( q_tot_error );

}

//________________________________________________________________
void TMutClusterFit::mathieson_lookup( sample_list& samples, TMutClusMap::pointer clus_ptr)
{

  typedef sample_list::iterator iter;

  // get q values for peak/right/left strips
  iter peak = max_element(
         samples.begin(),
         samples.end(),
         less_q_ftor());

  // peak strip
  MutStrip* peak_strip_ptr = TMutGeo::get_strip_geom(
    (*peak)->get()->get_location(),
    (*peak)->get()->get_strip());

  double q_peak = (*peak)->get()->get_q();
  double q_left=0;
  double q_right=0;

  // note: one should really use the cluster flags here
  bool BadStrips(false);
  bool AttenStrip(false);
  unsigned short begin_strip = 999;
  Float_t q_tot_error = 0;

  /*
     Look for and set left and right strips. If we don't find them
     q values default to 0.
  */
  for( sample_list::const_iterator iter = samples.begin();iter!=samples.end(); ++iter)
  {

    begin_strip = ((*iter)->get()->get_strip()<begin_strip)? (*iter)->get()->get_strip(): begin_strip;

    if ((*iter)->get()->get_strip() - (*peak)->get()->get_strip() == -1) q_left = (*iter)->get()->get_q();
    else if((*iter)->get()->get_strip() - (*peak)->get()->get_strip() == 1) q_right = (*iter)->get()->get_q();

    if( (*iter)->get()->get_is_attenuated() ) AttenStrip = true;
    else if( (*iter)->get()->get_is_bad() ) BadStrips = true;

    q_tot_error += MUTOO::SQUARE((*iter)->get()->get_error_q());

  }

  q_tot_error = sqrt(q_tot_error);

  // set total charge to the sum of the three strips
  double q_total = q_peak + q_left + q_right;

  // Invoke Mathieson lookup -- input is a boost array of 3 charge samples
  // with peak charge in center.
  // This will have sensible output for clusters
  // of width 2 or 3.
  TMutMathieson::charge_array charges = {{q_left,q_peak,q_right}};
  pair<double,double> x_local = TMutMathieson::get_x_local(charges,peak_strip_ptr);

  // Sigma of pull distribution for given cathode plane
  double error = TMutCathErrors::get_error(
    clus_ptr->get()->get_arm(),
    clus_ptr->get()->get_station(),
    clus_ptr->get()->get_octant(),
    clus_ptr->get()->get_half_octant(),
    clus_ptr->get()->get_gap(),
    clus_ptr->get()->get_cathode());

//   // Nagle- remove this error assignment until some study is done to justify
//   // Assign a large error to any cluster that has a status bit set that
//   // might negatively impact cluster resolution.
//   error =
//     ((clus_ptr->get()->get_peak_bound() ||
//     clus_ptr->get()->get_low_charge() ||
//     clus_ptr->get()->get_high_charge())) ? 0.5 : error;

  // If a bad strip included in cluster, put position at center of cluster:
  unsigned short peak_strip = (*peak)->get()->get_strip();

  if( AttenStrip || BadStrips )
  {

    if ((int)(samples.size())%2 == 0) x_local.first = -0.5;
    else x_local.first = 0.0;

    peak_strip = begin_strip + int(samples.size()/2);
    error = samples.size()/sqrt(12.0);

  }

  // append fit information to cluster
  TMutClusCentroid *centroid( clus_ptr->get()->insert_new_centroid( peak_strip ) );
  centroid->set_w( x_local.first );
  centroid->set_w_error( error );
  centroid->set_q_peak( q_peak );
  centroid->set_q_tot( q_total );
  centroid->set_q_tot_error( q_tot_error );

  // set fit chisquare
  clus_ptr->get()->set_chi_square(x_local.second);
  clus_ptr->get()->set_use_lookup();

}

//________________________________________________________________
void TMutClusterFit::mc_fit( sample_list& samples, TMutClusMap::pointer clus_ptr )
{

  // retrieve strip geometry
  const MutStrip* strip_geo = TMutGeo::get_strip_geom(samples[0]->get()->get_location(), samples[0]->get()->get_strip());

  // Figure out the strip spacing for this strip
  double strip_space = 1;
  if(strip_geo) strip_space = strip_geo->getStripSpacing();
  else MUTOO::TRACE("TMutClusterFit::mc_fit - TMutGeom returned null pointer");

  // retrieve MC info associated to the cluster
  vector< mc_data > mc_data_vect( find_mc_data( clus_ptr ) );
  vector< double > fit_par( 2*mc_data_vect.size(), 0 );

  for( vector< mc_data >::const_iterator mc_iter = mc_data_vect.begin(); mc_iter != mc_data_vect.end(); mc_iter++ )
  {

    // get smearing for the MC (checking if the cathode is stereo or not)
    double sigma = (
      (clus_ptr->get()->get_station()==1 && clus_ptr->get()->get_cathode()==0) ||
      (!clus_ptr->get()->get_station()==1 && clus_ptr->get()->get_cathode()==1)) ?
      _mc_smear_perp : _mc_smear_ster ;

    // find matching strip
    bool found( false );
    for( sample_list::iterator iter=samples.begin(); iter!=samples.end(); ++iter )
    if(fabs((*iter)->get()->get_strip() - mc_iter->_w ) < 0.5)
    {

      found = true;

      // smear the MC hit
      double ran_num( gsl_ran_gaussian(_rng.get(),sigma) );
      double w( mc_iter->_w + ran_num );
      double w_local( (w - (*iter)->get()->get_strip())*strip_space );

      double error( TMutCathErrors::get_error(
          clus_ptr->get()->get_arm(),
          clus_ptr->get()->get_station(),
          clus_ptr->get()->get_octant(),
          clus_ptr->get()->get_half_octant(),
          clus_ptr->get()->get_gap(),
          clus_ptr->get()->get_cathode()) );

      double q_peak( (*iter)->get()->get_q() );
      double q_total( mc_iter->_q );
      double q_tot_error( 0 );

      // insert new centroid
      TMutClusCentroid *centroid( clus_ptr->get()->insert_new_centroid( (*iter)->get()->get_strip() ) );
      centroid->set_w( w_local );
      centroid->set_w_error( error );
      centroid->set_q_peak( q_peak );
      centroid->set_q_tot( q_total );
      centroid->set_q_tot_error( q_tot_error );
    }

    if( !found )
    {

      if( _verbosity >= MUTOO::ALOT )
      {

        cout
          << "TMutClusterFit::mc_fit - unable to retrieve strip matching MC hit - "
          << " mc_track_id: " << mc_iter->_mc_track_id
          << " mc_hit_q: " << mc_iter->_q
          << " mc_hit_w: " << mc_iter->_w
          << " Cluster_w [ ";

        for(  sample_list::iterator iter=samples.begin(); iter!=samples.end(); ++iter )
        { cout << (*iter)->get()->get_strip() << " "; }
        cout << "] [ ";

        for( TMutMCHit::strip_list::const_iterator mc_strip_iter = mc_iter->_mc_strips.begin(); mc_strip_iter != mc_iter->_mc_strips.end(); mc_strip_iter++ )
        { if( mc_strip_iter->get_cathode() == clus_ptr->get()->get_cathode() ) cout << mc_strip_iter->get_strip() << " "; }
        cout << "]" << endl;

      } else {

        cout << "TMutClusterFit::mc_fit - unable to retrieve strip matching MC hit - "
          << " mc_track_id: " << mc_iter->_mc_track_id
          << " mc_hit_q: " << mc_iter->_q
          << " mc_hit_w: " << mc_iter->_w
          << endl;
      }
    }
  }

}

//________________________________________________________________
void TMutClusterFit::gsl_mathieson_fit( sample_list& samples, TMutClusMap::pointer clus_ptr, unsigned int n_trk_forced )
{

  MutCalibStrip *CalibPointer = MutCalib();

  unsigned short BadStrips( 0 );
  unsigned short AttenStrip( 0 );
  unsigned short BadEdge( 0 );

  // Reorder samples by strip number
  sort(samples.begin(),samples.end(),less_strip_ftor());

  // create debugging structure to fill in tree
  TMutClusterFitEval::DebugInfo debug;

  /*
    if the number of tracks should be retrieved from the MC, take the MC information
    and update n_trk_forced
  */
  if( _use_mc_ntracks )
  n_trk_forced = find_mc_data( clus_ptr ).size();

  /*
    try guess the number of tracks passing through the cluster
    and associated centroid position
  */
  vector< peak_strip_data > peak_strips( find_peak_strips( samples ) );

  // store number of "truely" fired strips in cluster
  int unmod_cluster_width = samples.size();

  // force two wide clusters to be fitted to 1 track
  if (unmod_cluster_width == 2) n_trk_forced = 1;

  // optionally force three wide clusters to be fitted with 1 track
  if( _force_three_wide_clusters && unmod_cluster_width == 3) n_trk_forced = 1;

  /*
    if the number of tracks passing through the cluster is forced to a given value
    check how many peak strips are found and remove the last ones
  */
  if( n_trk_forced )
  {
    if(  peak_strips.size() > n_trk_forced )
    {

      // sort vector from largest to smallest peak
      sort( peak_strips.begin(), peak_strips.end(), peak_strip_data::greater_peak_charge_ftor() );

      // pop back the smallest peaks
      while( peak_strips.size() > n_trk_forced ) peak_strips.pop_back();

      // sort back to strip index
      sort( peak_strips.begin(), peak_strips.end(), peak_strip_data::less_strip_ftor() );

    } else if( peak_strips.size() < n_trk_forced ) {

      cout << "TMutClusterFit::gsl_mathieson_fit - not enough peak strips. n_trk_forced set to " << peak_strips.size() << endl;
      n_trk_forced = peak_strips.size();

    }

  }

  // allocate vectors to store local maxima (position and charge)
  vector<unsigned short> PeakStrip( samples.size(), 0 );
  vector<double> PeakCharge( samples.size(), 0 );
  vector<double> QMultiple( samples.size(), 0 );
  vector<double> XMultiple( samples.size(), 0 );
  int ntracks( static_cast<int>(peak_strips.size()) );

  for( unsigned int i=0; i<peak_strips.size(); i++ )
  {
    PeakStrip[i] = peak_strips[i]._strip;
    PeakCharge[i] = peak_strips[i]._peak_charge;
    QMultiple[i] = peak_strips[i]._charge;
    XMultiple[i] = peak_strips[i]._position;
  }

  // Get the geometry of the first strip in cluster;
  const MutStrip* strip_geo = TMutGeo::get_strip_geom(
    samples[0]->get()->get_location(),
    samples[0]->get()->get_strip());

  // Figure out the strip spacing for this strip
  double strip_space = 1;
  if(strip_geo) strip_space = strip_geo->getStripSpacing();
  else MUTOO::TRACE("TMutClusterFit::gsl_mathieson_fit - TMutGeom returned null pointer");

  // retrieve location of the first strip in cluster
  unsigned short arm( samples.front()->get()->get_arm() );
  unsigned short station( samples.front()->get()->get_station() );
  unsigned short octant( samples.front()->get()->get_octant() );
  unsigned short half( samples.front()->get()->get_half_octant() );
  unsigned short gap( samples.front()->get()->get_gap() );
  unsigned short cath( samples.front()->get()->get_cathode() );
  unsigned short CathodePlane( cath ? MUTGEOM::Cathode2:MUTGEOM::Cathode1 );

  // retrieve number of strip in the cathode plane
  unsigned short NumberOfStrips( TMutGeo::get_n_strip(arm, station, octant, half, gap, CathodePlane) - 1 );

  // Extract q values and errors from input samples vector
  vector<double> q_values;
  vector<double> q_errors;
  vector<double> q_min;
  vector<double> gains;
  vector<double> pedestals;
  vector<double> calibrms;
  vector<double> qtot_out(samples.size(),0);

  /*
    Use knowledge that charge outside of cluster bounds was apparently measured
    to be zero. Set this charge to zero and determine appropriate error.

    Set sigma of outside strips to measured rms if it is within chamber bounds
    If it is outside chamber bounds, we don't know what the real charge might
    have been so set sigma to 200.
  */

  if(_use_zerosup_strips)
  {

    q_values.push_back(0);
    if (samples.front()->get()->get_strip()-1 >= 0)
    {

      unsigned short istrip = samples.front()->get()->get_strip()-1;
      const PdbMutCalibStrip *StripCalib = CalibPointer->getPdbMutCalibStrip( arm, station, octant, half, gap, cath, istrip);
      if( StripCalib && StripCalib->isValid() )
      {

        // nagle - errors need to be consistent with that used in smearing of real hit strips
        q_errors.push_back(StripCalib->get_rms()/StripCalib->get_gain() );
        q_min.push_back( StripCalib->getCharge( int(StripCalib->get_pedestal() - (3*StripCalib->get_rms()))) );
        gains.push_back( StripCalib->get_gain() );
        pedestals.push_back( StripCalib->get_pedestal() );
        calibrms.push_back( StripCalib->get_rms() );

        // H.Pereira: there is basically no way to properly account for additional smearing to the charge (added at the
        // response stage) on the neighbor strips, because this class has no easy access to the mMutResponsePar parameter
        // module. For fired strips however, the hit_ptr->get_error_q can be used and accounts for such a smearing.

      } else {

        // Bad calibration determination; treat channel as dead
        // Nagle - these need to be corrected - cannot give zero information as a point in DOF!
        q_errors.push_back(200);
        q_min.push_back(0.);

      }

    } else {

      q_errors.push_back(200);
      q_min.push_back(0.);

    }

  }

  // process fired strip
  double xguess = 0;
  double chargesum = 0;

  unsigned short istrip = 0;

  double q_error_samp = 0;

  for( sample_list::iterator iter = samples.begin(); iter!=samples.end();++iter)
  {

    q_values.push_back((*iter)->get()->get_q());
    chargesum += (*iter)->get()->get_q();
    xguess += (*iter)->get()->get_q()*(*iter)->get()->get_strip();

    const PdbMutCalibStrip *StripCalib = CalibPointer->getPdbMutCalibStrip( arm, station, octant, half, gap, cath, (*iter)->get()->get_strip());
    if( StripCalib && StripCalib->isValid() )
    {

      q_min.push_back( StripCalib->getCharge( int(StripCalib->get_pedestal() - (3*StripCalib->get_rms()))) );
      gains.push_back( StripCalib->get_gain() );
      pedestals.push_back( StripCalib->get_pedestal() );
      calibrms.push_back( StripCalib->get_rms() );

    } else {

      q_min.push_back(0.);

    }

    // dump charges and error for non attenuated channels
    if( _verbosity >= MUTOO::MAX )
    {

      cout << "TMutClusterFit::gsl_mathieson_fit -"
        << " id="<< (*iter)->get()->get_key().get_obj_key()
        << " q=" << (*iter)->get()->get_q()
        << " error=" << (*iter)->get()->get_error_q()
        << endl;
    }

    // add proper error on the charge, calculated in the mMutCalibrate module.
    // it accounts for the rms/gain ratio picked from the calibrations, corrected by any additional smearing added
    // at the response stage (for MC), and the RMS between the 3 top samples used to calculate the charge.
    q_errors.push_back( (*iter)->get()->get_error_q() );
    q_error_samp += MUTOO::SQUARE( (*iter)->get()->get_error_q() );

    if( (*iter)->get()->get_is_attenuated() ) AttenStrip++;
    else if( (*iter)->get()->get_is_bad() )
    {

      BadStrips++;
      if (istrip==0 || istrip==samples.size() - 1) BadEdge++;

    }

    istrip++;

  }

  xguess = xguess/chargesum ;
  q_error_samp = sqrt(q_error_samp);

  // Set sigma of outside strips to measured rms if it is within chamber bounds
  // If it is outside chamber bounds, we don't know what the real charge might
  // have been so set sigma to 200.
  if(_use_zerosup_strips)
  {

    q_values.push_back(0);
    if (samples.front()->get()->get_strip() + samples.size() < NumberOfStrips)
    {

      istrip = samples.front()->get()->get_strip()+samples.size();
      const PdbMutCalibStrip *StripCalib = CalibPointer->getPdbMutCalibStrip(arm, station, octant, half, gap, cath, istrip);

      if (StripCalib && StripCalib->isValid())
      {
        q_errors.push_back(StripCalib->get_rms()/StripCalib->get_gain());
        q_min.push_back( StripCalib->getCharge( int(StripCalib->get_pedestal() - (3*StripCalib->get_rms()))) );
        gains.push_back( StripCalib->get_gain() );
        pedestals.push_back( StripCalib->get_pedestal() );
        calibrms.push_back( StripCalib->get_rms() );

        // H.Pereira: there is basically no way to properly account for additional smearing to the charge (added at the
        // response stage) on the neighbor strips, because this class has no easy access to the mMutResponsePar parameter
        // module. For fired strips however, the hit_ptr->get_error_q can be used and accounts for such a smearing.

      } else {

        // Bad calibration determination; treat channel as dead
        q_errors.push_back(200);
        q_min.push_back(0.);
      }

    } else {

      // Bad calibration determination; treat channel as dead
      q_errors.push_back(200);
      q_min.push_back(0.);

    }

  }

  // Adjust width and start for padding
  int cluster_width( samples.size() );
  int begin_strip_num( samples.front()->get()->get_strip() );
  int ihit( 0 );
  double strip_width( strip_space/2.0 );

  if(_use_zerosup_strips)
  {

    cluster_width += 2;
    begin_strip_num -= 1;

  }

  // DEBUG
  if( TMutClusterFitEval::get_do_evaluation() )
  {

    debug.is_multiple = ntracks;
    debug.width_over_int = cluster_width/2;
    debug.width_over_double = static_cast<double>(cluster_width)/2;

    for(unsigned int im=0; im<q_min.size() && im<4; im++)
    {
      debug.min_charges[im] = q_min[im];
    }

    for(unsigned int im=0; im<gains.size() && im<4; im++)
    {
      debug.gains[im] = gains[im];
      debug.pedestals[im] = pedestals[im];
      debug.rms[im] = calibrms[im];
    }

  }

  if(!_use_fitfunc_zerosup)
  {
    for(unsigned int iz=0; iz<q_min.size(); iz++)
      q_min[iz] = 0.;
  }

  // Don't try to fit more hits than NDOF:
  if (ntracks > cluster_width/2) ntracks = cluster_width/2;

  // store max number of allowed fit/parameters locally for readability
  const int max_fit = _max_fit;
  const int max_par = 2*_max_fit;

  // allocate vectors for parameters and covariance matrix
  // parameters set
  vector< double > fit_par( max_par, 0 );

  // new parameters
  vector< double > fit_par_new( max_par, 0 );

  // fitted x
  vector< double > xhitnew( max_fit, 0 );

  // initial x
  vector< double > xhitinit( max_fit, 0 );

  // fitted charges
  vector< double > qanodenew( max_fit, 0 );

  // initial charges
  vector< double > qanodeinit( max_fit, 0 );

  // parameters covariance matrix
  PHGslMatrix fit_cov( max_par, max_par );

  // error on fitted charge
  vector< double > q_tot_error( max_par, 0 );
  vector< double > w_error( max_par, 0 );

  // new parameters
  vector< double > q_tot_error_new( max_par, 0 );
  vector< double > w_error_new( max_par, 0 );

  // last fit chisquare
  double chi_square=0;

  // last fit chisquare
  double chi_square_perdof=0;

  // new fit chisquare
  double chi_square_new=0;

  // new fit chisquare
  double chi_square_perdof_new=0;
  int status, statusnew;

  int itrack;
  vector<unsigned short>::iterator PeakStrip_iter = PeakStrip.begin();
  vector<double>::iterator PeakCharge_iter = PeakCharge.begin();
  for (itrack=0; itrack<ntracks; itrack++){
    fit_par[itrack] =   XMultiple[itrack];
    xhitinit[itrack] = fit_par[itrack];
    fit_par[ntracks + itrack] =  QMultiple[itrack];
    qanodeinit[itrack] = fit_par[ntracks+itrack];
    ++PeakStrip_iter;
    ++PeakCharge_iter;
  }
  if (ntracks==1) fit_par[1] = chargesum;

  double ac = TMutMathiesonPar::get_mathieson_parameters(arm,station,octant,gap).second;
  double cc = TMutMathiesonPar::get_mathieson_parameters(arm,station,octant,gap).first;

  // DEBUG
  if( TMutClusterFitEval::get_do_evaluation() ) {
    debug.ac = ac;
    debug.cc = cc;
  }

  if( _verbosity >= MUTOO::ALOT)
  {
    cout << "ntracks = " << ntracks << endl;
    cout << "cluster_width = " << samples.size() << endl;
    for (unsigned int j =0; j<samples.size(); j++)
      cout << "q_values[i] = " << q_values[j+1] << endl;
  }

  if( _verbosity >= MUTOO::ALOT )
  {

    // dump the charges
    cout << "TMutClusterFit::gsl_mathieson_fit - q_values - ";
    for( unsigned int i=0; i<q_values.size(); i++ ) cout << q_values[i] << " ";
    cout << endl;

    // dump the errors
    cout << "TMutClusterFit::gsl_mathieson_fit - q_errors - ";
    for( unsigned int i=0; i<q_errors.size(); i++ ) cout << q_errors[i] << " ";
    cout << endl;

  }

  if (ntracks>0)
  {
    status = do_gsl_mathieson_fit(
				  q_values,
				  q_errors,
				  q_min,
				  cluster_width,
				  begin_strip_num,
				  strip_width,
				  ntracks,
				  fit_par,
				  fit_cov,
				  chi_square,
				  arm, station, 2*gap+cath, octant,
				  ac,
				  cc,
				  qtot_out);
  } else {
    status = -1;
    clus_ptr->get()->set_chi_square(100.0);
    ntracks = 1;
    if(_use_zerosup_strips)
      fit_par[0] = begin_strip_num + 1 + samples.size()/2.0 - 0.499;
    else
      fit_par[0] = begin_strip_num + samples.size()/2.0 - 0.499;

    q_tot_error[0] = q_error_samp;
    w_error[0] = strip_space * 3.0 / sqrt(12.0);
  }

  // DEBUG
  if( TMutClusterFitEval::get_do_evaluation() )
  {

    debug.n_tracks_1 = ntracks;
    for( int i=0; i< ntracks && i< TMutClusterFitEval::DebugInfo::max_tracks; i++ )
    {
      debug.x_fit_1[i] = fit_par[i];
      debug.q_fit_1[i] = fit_par[ntracks+i];
      debug.x_err_1[i] = (fit_cov(i,i)>=0) ? sqrt( fit_cov(i,i) ):0;
      debug.q_err_1[i] = (fit_cov(ntracks+i,ntracks+i)>=0) ?
        sqrt( fit_cov(ntracks+i,ntracks+i) ):
        0;
    }
    debug.chi2_1 = chi_square;

  }

  // calculate chisq/dof - for now include the extra 2 strips - IF good return status
  if( status==0 )
  {
    if (cluster_width - 2 * ntracks > 0) chi_square_perdof = chi_square / ((double) (cluster_width - 2 * ntracks));
    else chi_square_perdof = chi_square;

    // Store error on fitted charge;
    // try to protect against overly small errors
    // from gsl:
    for (int i=0; i<ntracks; i++){
      q_tot_error[i] = (fit_cov( ntracks+i, ntracks+i)<q_error_samp) ?
        q_error_samp :
        sqrt( fit_cov( ntracks+i, ntracks+i) );
      w_error[i] = sqrt( fit_cov(i,i) );
    }
  } else {
    chi_square_perdof = 100.0;
    q_tot_error[0] = q_error_samp;
    w_error[0] = strip_space * 3.0 / sqrt(12.0);
  }

  /*
    if n_trk_forced is 0, fit gives poor results
    if the cluster is large compared to ntracks,
    try fitting a different number of tracks
  */
  if(
    n_trk_forced == 0 &&
    status==0 &&
    chi_square_perdof > _chi_max_good_fit &&
    ntracks+1 <= cluster_width/2 &&
    ntracks < _max_fit )
  {

    for( int i=0; i<ntracks; ++i)
    {
      xhitnew[i] = xhitinit[i];
      qanodenew[i] = qanodeinit[i];
    }

    qanodenew[ntracks] = 0;

    // Find strip which has largest amount of charge, excluding strips already
    // tagged as "peaks"
    for (istrip = 0; istrip < cluster_width-2*_use_zerosup_strips; ++istrip) {

      bool found( false );
      for( int i = 0; i < ntracks; ++i)
        if (rint(xhitnew[i]) == (begin_strip_num+1 + istrip))
          found = true;

      if( ( !found ) && q_values[istrip+1] > qanodenew[ntracks] ) {
        qanodenew[ntracks] = q_values[istrip+1];
        xhitnew[ntracks] = begin_strip_num + 1 + istrip;
      }

    }  // Loop over strips

    //  Order hits
    ihit = 0;
    while( ihit < ntracks-1 && xhitnew[ntracks] > xhitnew[ihit] && xhitnew[ntracks] < xhitnew[ntracks-1] )
    ihit += 1;

    if (ihit == 0 && xhitnew[ntracks] < xhitnew[0]) ihit = -1;
    else ihit=ntracks-ihit;

    double xtemp = xhitnew[ntracks];
    double qtemp = qanodenew[ntracks];
    for( int i = ntracks-1; i >= (ihit+1); --i)
    {
      xhitnew[i + 1] = xhitnew[i];
      qanodenew[i + 1] = qanodenew[i];
    }

    xhitnew[ihit+1] = xtemp;
    qanodenew[ihit+1] = qtemp;

    // Do fitting
    int temp = ntracks+1;

    for( int i=0; i<temp; i++ )
    {
      fit_par_new[i] = rint(xhitnew[i]);
      fit_par_new[temp+i] = qanodenew[i];
    }

    statusnew = do_gsl_mathieson_fit(
      q_values,
      q_errors,
      q_min,
      cluster_width,
      begin_strip_num,
      strip_width,
      temp,
      fit_par_new,
      fit_cov,
      chi_square_new,
      arm, station, 2*gap+cath, octant,
      ac,
      cc,
      qtot_out);

    if (statusnew == 0)
    {
      if (cluster_width - 2 * temp > 0)
      { chi_square_perdof_new = chi_square_new / ((double) (cluster_width - 2 * temp)); }
      else { chi_square_perdof_new = chi_square_new; }

      for (int i=0; i<temp; i++)
      {
        q_tot_error_new[i] = (fit_cov( temp+i, temp+i)<q_error_samp) ? q_error_samp : sqrt( fit_cov( temp+i, temp+i) );
        w_error_new[i] = sqrt( fit_cov(i,i) );
      }

    } else {

      chi_square_perdof_new = 100.0;
      q_tot_error_new[0] = q_error_samp;
      w_error_new[0] = strip_space * 3.0 / sqrt(12.0);
    }

    // DEBUG
    if( TMutClusterFitEval::get_do_evaluation() )
    {
      debug.n_tracks_2 = temp;
      for( int i=0; i< temp && i< TMutClusterFitEval::DebugInfo::max_tracks; i++ )
      {
        debug.x_fit_2[i] = fit_par_new[i];
        debug.q_fit_2[i] = fit_par_new[temp+i];
        debug.x_err_2[i] = (fit_cov(i,i)>=0) ? sqrt( fit_cov(i,i) ):0;
        debug.q_err_2[i] = (fit_cov(temp+i,temp+i)>=0) ? sqrt( fit_cov(temp+i,temp+i) ):0;
      }
      debug.chi2_2 = chi_square_new;
    }

    if (statusnew==0 && chi_square_perdof_new < chi_square_perdof)
    {

      // DEBUG
      if( TMutClusterFitEval::get_do_evaluation() ) debug.fit_2_success = 1;

      status = statusnew;
      ntracks += 1;
      chi_square = chi_square_new;
      chi_square_perdof = chi_square_perdof_new;
      for (int i=0; i<ntracks; i++) q_tot_error[i] = q_tot_error_new[i];
      for (int i=0; i<ntracks; i++) w_error[i] = w_error_new[i];

      for (ihit = 0; ihit < ntracks; ++ihit)
      {
        fit_par[ihit] = fit_par_new[ihit];
        fit_par[ntracks+ihit] = fit_par_new[ntracks+ihit];
      }
    }
  }

  // dump the fit parameters
  if( _verbosity >= MUTOO::ALOT )
  {
    cout << "TMutClusterFit::gsl_mathieson_fit - fit_par - ";
    for( int i=0; i<ntracks*2; i++ ) cout << fit_par[i] << " ";
    cout << endl;
  }

  if (status==0) clus_ptr->get()->set_chi_square(chi_square);
  else {
    clus_ptr->get()->set_chi_square(100.0);
    ntracks = 1;
    fit_par[0] = begin_strip_num + 1 + samples.size()/2.0 - 0.5;
  }

  // if useMC flag is true, look for a MC hit contributing to this cluster.  If one
  // found, find closest fit position to it and replace it with true hit.  This allows
  // us to study performace when true positions are available to PR.
  bool mcfound = false;

  if( _use_mc )
  {

    // retrieve mc_data associated to this cluster
    vector< mc_data > mc_data_vect( find_mc_data( clus_ptr ) );
    for( vector< mc_data >::const_iterator mc_iter = mc_data_vect.begin(); mc_iter != mc_data_vect.end(); mc_iter++ )
    {
      // If a MC hit found, find closest fit position to this, and replace it with wmc:
      double delta_w = -1;
      int besttrack = 0;
      for(int itracks = 0; itracks<ntracks; ++itracks)
      if (fabs(fit_par[itracks] - mc_iter->_w) < delta_w || delta_w < 0 ){
        delta_w = fit_par[itracks] - mc_iter->_w;
        besttrack = itracks;
      }

      // get smearing for the MC (checking if the cathode is stereo or not)
      double sigma = (
        (clus_ptr->get()->get_station()==1 && clus_ptr->get()->get_cathode()==0) ||
        (!clus_ptr->get()->get_station()==1 && clus_ptr->get()->get_cathode()==1)) ?
        _mc_smear_perp : _mc_smear_ster ;

      // smear the MC hit
      double ran_num = gsl_ran_gaussian(_rng.get(),sigma);
      fit_par[besttrack] = mc_iter->_w + ran_num;

    }
  }

  // If cluster is associated with more than one track, loop over values.
  // use the strip information for the one strip that the fit coord is in...
  for(int itracks = 0; itracks<ntracks; ++itracks)
  for(  sample_list::iterator iter=samples.begin(); iter!=samples.end(); ++iter )
  if(fabs((*iter)->get()->get_strip() - fit_par[itracks]) < 0.5)
  {

    // W coordinate relative to the peak strip
    double w_local = (fit_par[itracks] - (*iter)->get()->get_strip())*strip_space;
    double q_total = qtot_out[itracks];
    double q_peak = (*iter)->get()->get_q();

    // Sigma of pull distribution for given cathode plane
    double error = TMutCathErrors::get_error(clus_ptr->get()->get_arm(),
               clus_ptr->get()->get_station(),
               clus_ptr->get()->get_octant(),
               clus_ptr->get()->get_half_octant(),
               clus_ptr->get()->get_gap(),
               clus_ptr->get()->get_cathode());

    if (!mcfound)
    {

      // Assign a large error to any cluster that has a status bit set that
      // might negatively impact cluster resolution.

      // Modify error if bad fit or something wrong with cluster:
      // nagle - need to confirm these error increases ( )
      if (ntracks == 2) error *= 1.5;
      if (ntracks  > 2) error *= 2.0;

      // Whenever one assigns as error of the size/sqrt(12), put w_local in the middle
      // nagle - July 20, 2004 - just assign an error of 3 strips wide/sqrt(12)
      // in a 5 wide cluster, giving each local peak an error of total_width/sqrt(12) is too big, and asymmetric

      // NAGLE - THIS IS A HACK SINCE WE WANT A LARGER CRITERIA FOR SAYING NO RELIABLE INFORMATION COMPARED
      // TO JUST WANTING TO PERFORM A SECOND FIT (TAKE 10 * MAX_GOOD_FIT)
      //printf("TMutClusterFit:: old_err: %f  new_err: %f\n", error, w_error[itracks]);
      //error = w_error[itracks];

      /*
      j.nagle -add some extra error checking here
      using the chi2perdof and also the original clusterwidth parameter
      */
      if (chi_square_perdof > 2.0 * _chi_max_good_fit ) {
        error = error * 3.0;
      }

      if (chi_square_perdof > 3.0 * _chi_max_good_fit ) {
        error = error * 2.0; // note for greater than 3 you get *3 and *2
      }

      if (chi_square_perdof > 10.0 * _chi_max_good_fit ) {
        w_error[itracks] = strip_space * 1.0 / sqrt(12.0);
        error = strip_space * 1.0 / sqrt(12.0);
      }

      // ???
      if( unmod_cluster_width == 4 ) error = error * 3.0;

      // Add double check that error from above is not larger than 1/sqrt(12)
      if (error > (strip_space * 1.0 / sqrt(12.0)))
      { error = strip_space * 1.0 / sqrt(12.0); }

      if (w_error[itracks] > (strip_space * 1.0 / sqrt(12.0)))
      { w_error[itracks] = strip_space * 1.0 / sqrt(12.0); }

      // deal with special problem cases below
      if( ntracks >= _max_fit )
      {
        w_error[itracks] = strip_space * 3.0 / sqrt(12.0);
        error = strip_space * 3.0 / sqrt(12.0);
      }

      // increase error if there are some attenuated strips
      if( samples.size() - AttenStrip < 2 )
      {

        w_error[itracks] = strip_space * 3.0 / sqrt(12.0);
        error = strip_space * 3.0 / sqrt(12.0);

      } else if( AttenStrip ) {

        error *= 2.0;
        w_error[itracks] *= 2.0;

      }

      // increase error if there are bad strips
      if( (samples.size() - BadStrips < 2) || BadEdge )
      {

        w_error[itracks] = strip_space * 3.0 / sqrt(12.0);
        error = strip_space * 3.0 / sqrt(12.0);

      } else if(BadStrips) {

        w_error[itracks] *= 2.0;
        error *= 2.0;

      }

      // double check that error is never set to wider than no information case
      static const double max_error_scale =  3.0 / sqrt(12.0);
      error = std::min( error,  strip_space*max_error_scale );
      w_error[itracks] = std::min( w_error[itracks], strip_space*max_error_scale );

      debug.error_from_pulldist = error;
      debug.error_from_fit = w_error[itracks];

      if( _use_hybridmax_error ) error = std::max( error, w_error[itracks] );
      else if(_use_fit_error) error = w_error[itracks];

    }	// If not using true MC hit

    // append fit information to cluster

    TMutClusCentroid *centroid( clus_ptr->get()->insert_new_centroid( (*iter)->get()->get_strip() ) );
    centroid->set_w( w_local );
    centroid->set_w_error( error );
    centroid->set_q_peak( q_peak );
    centroid->set_q_tot( q_total );
    centroid->set_q_tot_error( q_tot_error[itracks] );

    break;  // entry found for this track
  }

  // DEBUG
  if( TMutClusterFitEval::get_do_evaluation() )
  {
    debug.gsl_was_called = 1;
    TMutClusterFitEval::add_debug_info( clus_ptr->get()->get_key().get_obj_key(), debug );
  }

}

//____________________________________________________________________
vector< TMutClusterFit::peak_strip_data > TMutClusterFit::find_peak_strips( const sample_list& samples )
{

  vector< peak_strip_data > peak_strips;

  // Look at pulse shape and try to find peaks within cluster.
  double qpresent( 0 ), qlast( 0 ), qnext( 0 );
  unsigned short strippresent( 0 );
  unsigned short istrip( 0 );

  /*
    try guess the number of tracks passing through the cluster
    and associated centroid position
    (note that the iterator is _not_ incremented in the "for" statement
    but within the loop)
  */
  for( sample_list::const_iterator iter = samples.begin(); iter!=samples.end();)
  {

    qpresent = (*iter)->get()->get_q();
    strippresent = (*iter)->get()->get_strip();

    /*
      if the current strip is the last in the cluster
      and its charge is bigger than the previous strip,
      make it a peak strip
    */
    if (istrip== samples.size()-1 && qpresent >= qlast) {
      peak_strips.push_back(
        peak_strip_data(
          strippresent,
          qpresent,
          qpresent + qlast,
          (strippresent*qpresent + (strippresent-1)*qlast)/(qpresent + qlast) ) );

    }

    // get charge on next strip
    ++iter;
    if (iter==samples.end()) break;

    qnext = (*iter)->get()->get_q();

    /*
      If the current strip is the first in cluster
      and its charge is bigger than next strip,
      make it a peak strip
    */
    if (istrip==0 && qpresent > qnext) {
      peak_strips.push_back(
        peak_strip_data(
          strippresent,
          qpresent,
          qpresent + qnext,
          (strippresent*qpresent +
          (strippresent+1)*qnext)/(qpresent + qnext) ) );
    }

    /*
      if the current strip has a charge larger than its two neighbors,
      make it a peak strip
    */
    else if (qpresent >= qlast && qpresent > qnext)
    {
      peak_strips.push_back(
        peak_strip_data(
          strippresent,
          qpresent,
          qpresent + qnext + qlast,
          (strippresent*qpresent + (strippresent+1)*qnext +
          (strippresent-1)*qlast)/(qpresent + qnext + qlast) ) );

    }

    // store previous charge
    qlast = qpresent;
    ++istrip;
  }

  return peak_strips;

}

//_______________________________________________________________
TMutClusterFit::mc_data::mc_data( TMutMCHitMap::const_pointer mc_hit_ptr, const double& w ):
    _mc_hit( *mc_hit_ptr ),
    _mc_strips( *mc_hit_ptr->get()->get_strip_list() ),
    _mc_track_id( mc_hit_ptr->get()->get_track_id() ),
    _w( w ),
    _q( mc_hit_ptr->get()->get_eloss() )
{}

//_____________________________________________________________
void TMutClusterFit::mc_data::add_hit( TMutHitMap::const_pointer hit_ptr )
{
  //std::cout << "mc_data::add_hit - mc_track_id: " << _mc_track_id << " adding hit " << hit_ptr->get()->get_strip() << std::endl;
  _hits.push_back( *hit_ptr );
}

//_______________________________________________________________
vector< TMutClusterFit::mc_data > TMutClusterFit::find_mc_data( TMutClusMap::const_pointer clus_ptr )
{

  vector< mc_data > out;

  // retrieve hits associated to cluster
  TMutHitMap::const_key_iterator hit_iter = clus_ptr->get()->get_associated<TMutHit>();
  while( TMutHitMap::const_pointer hit_ptr = hit_iter.next() ){

    // retrieve associated MC hits
    TMutMCHitMap::const_key_iterator mc_hit_iter = hit_ptr->get()->get_associated<TMutMCHit>();
    while(TMutMCHitMap::const_pointer mc_hit_ptr = mc_hit_iter.next())
    {

      vector< mc_data >::iterator mc_data_iter = find_if( out.begin(), out.end(), mc_data::same_mc_track_ftor( mc_hit_ptr->get()->get_track_id() ) );
      if( mc_data_iter != out.end() )
      {
        mc_data_iter->add_hit( hit_ptr );
        continue;
      }

      // get MC hit point
      PHPoint hit_point(mc_hit_ptr->get()->get_x(), mc_hit_ptr->get()->get_y(), mc_hit_ptr->get()->get_z());

      MutArm* geometry = (clus_ptr->get()->get_arm()== MUTOO::South) ? SouthArm():NorthArm();
      MutStrip* strip_array[2] = {0};
      double strip_ip_array[2] = {0};
      MutWire* wire_array[1] = {0};
      double wire_ip=0;

      geometry->convertPisaHitToChamber(hit_point,
              wire_array,
              wire_ip,
              strip_array,
              strip_ip_array);

      int icath = clus_ptr->get()->get_cathode();
      if( !strip_array[icath] )
      {
        cout << "TMutClusterFit::find_mc_data - unable to get strip geometry" << endl;
        continue;
      }

      double ReadoutSpacing = (double)(strip_array[icath]->getStripSpacing());
      double w = (double)strip_array[icath]->getStrip() + strip_ip_array[icath]/ReadoutSpacing;

      mc_data mc_data_item( mc_hit_ptr, w );
      mc_data_item.add_hit( hit_ptr );
      out.push_back( mc_data_item );
    }
  }

  return out;

}

//________________________________________________________________
int TMutClusterFit::do_gsl_mathieson_fit(
  const vector<double>& q_values,
  const vector<double>& q_errors,
  const vector<double>& q_mins,
  const int& cluster_width,
  const int& stripbegin,
  const double& strip_width,
  const int& ntracks,
  vector<double> &par_out,
  PHGslMatrix &cov_out,
  double& chisq,
  const int& arm,
  const int& station,
  const int& cathplane,
  const int& octant,
  const double& ac_in,
  const double& cap_coupling,
  std::vector<double> &qtot_out)
{

  // convergence criterions
  static const double EPSABSPOS = 0.005;
  static const double EPSRELCHARGE = 0.05;
  static const unsigned int max_iterations = 25;

  const size_t n_parameters = ntracks*2;
  gsl_matrix* covar = gsl_matrix_alloc( n_parameters, n_parameters );
  size_t ntrk_gsl = ntracks;
  gsl_data d = {
    ntrk_gsl,
    stripbegin,
    cluster_width,
    q_values,
    q_errors,
    q_mins,
    strip_width,
    ac_in,
    arm,
    station,
    cathplane,
    octant,
    cap_coupling,
    qtot_out };

  // store parameters in gsl vector
  gsl_vector_view x = gsl_vector_view_array ( &par_out[0], n_parameters);

  // define gsl function used for the minimisation
  const size_t n_measurements = cluster_width;
  gsl_multifit_function_fdf f;
  f.f   = &_expb_f;
  f.df  = &_expb_df;
  f.fdf = &_expb_fdf;
  f.n = n_measurements;
  f.p = n_parameters;
  f.params = &d;

  // gst minimization type and solver
  const gsl_multifit_fdfsolver_type *type = gsl_multifit_fdfsolver_lmsder;
  gsl_multifit_fdfsolver *s = gsl_multifit_fdfsolver_alloc( type, n_measurements, n_parameters);
  gsl_multifit_fdfsolver_set(s, &f, &x.vector);

  // start iterations
  int status = 0;
  unsigned int iter = 0;
  do {

    iter++;
    status = gsl_multifit_fdfsolver_iterate (s);

    // force status to GSL_SUCCESS
    status = 0;

    for( int j=0; j < ntracks; j++){

      if ( fabs(gsl_vector_get(s->dx, j)) > EPSABSPOS) status = GSL_CONTINUE;
      if ( fabs(gsl_vector_get(s->dx, ntracks + j)) > EPSRELCHARGE*gsl_vector_get(s->x,ntracks+j) )
      status = GSL_CONTINUE;
    }

  } while (status == GSL_CONTINUE && iter < max_iterations);

  if (status){

    if( _verbosity >= MUTOO::ALOT ) {
      cout << "TMutClusterFit::do_gsl_mathieson_fit - failed.\n";
      cout << "  status = " << gsl_strerror (status) << endl;
      cout << "  iteration = " << iter << endl;
      cout << "  cluster_width = " << n_measurements << endl;
      cout << "  nfit = " << n_parameters/2 << endl;
      for( unsigned int j=0; j < n_measurements; j++)
      cout << "  qstrip = " << q_values[j] << endl;
    }

    gsl_matrix_free (covar);
    gsl_multifit_fdfsolver_free (s);
    return status;

  } else if (iter == max_iterations ){

    if( _verbosity >= MUTOO::ALOT )
      cout << "TMutClusterFit::do_gsl_mathieson_fit - max number of iterations reached.\n";

    gsl_matrix_free (covar);
    gsl_multifit_fdfsolver_free (s);
    return iter;
  }

#if GSL_MAJOR_VERSION == 2
  gsl_matrix * J = gsl_matrix_alloc(s->fdf->n, s->fdf->p);
  gsl_multifit_fdfsolver_jac(s, J);
  gsl_multifit_covar(J, 0.0, covar);

  // // free previousely allocated memory
  gsl_matrix_free (J);
#else
  gsl_multifit_covar (s->J, 0.0, covar);
#endif

  // Copy fitted parameters back into a and covarout:
  for( unsigned int j=0; j< n_parameters; j++ )
  {

    par_out[j] = gsl_vector_get(s->x, j);
    for( unsigned int k=0; k < n_parameters; k++)
    { cov_out( j, k ) = gsl_matrix_get(covar, j, k); }

  }

  for( int j=0; j< ntracks; j++ )
  { qtot_out[j] = d.qtot_out[j]; }

  //! warning: the method do not return the chisquare but its square_root
  // nagle - this needs to be squared to be chisq - and not yet in dof
  chisq = gsl_blas_dnrm2 (s->f);
  chisq = chisq * chisq;

  gsl_matrix_free (covar);
  gsl_multifit_fdfsolver_free (s);

  return 0;

}

//________________________________________________________________
int TMutClusterFit::_expb_f (const gsl_vector * x, void *params, gsl_vector * f)
{

  // retrieve the data in correct format
  gsl_data* p = static_cast< gsl_data* >( params );

  size_t n = p->ntracks;
  const vector< double >& q_values = p->q_values;
  const vector< double >& q_errors = p->q_errors;
  const vector< double >& q_mins = p->q_mins;
  std::vector< double > q_tot(n, 0.0);

  double cap_coupling = p->cap_coupling;
  double A2 = cap_coupling * A2init;

  //hold 4 tanh calculations for each arm, station, cathode plane
  static bool init_done  __attribute__ ((unused)) = initialize_tanhs();

  //  If _tanhs for this plane have not been calculated before, calculate them now:
  int arm = p->arm;
  int station = p->station;
  int cathplane = p->cathplane;
  int octant = p->octant;
  double AC = p->AC;
  double strip_width = p->strip_width;

  /*
    For each strip in the cluster, calculate the charge that is deposited
    on it by each track that the cluster is being fit to.  Store the total
    calculated charge in the fit parameter array y.
  */
  for (int istrip = 0; istrip < p->cluster_width; istrip++) {

    double xstrip = (double)(p->stripbegin + istrip);
    double Yi = 0.0;

    for( unsigned int itrack = 0; itrack < p->ntracks; itrack++)
    {

      double Ipeak = rint(gsl_vector_get (x, itrack));
      double Xcent = gsl_vector_get (x, itrack) - Ipeak;
      double dx = (Xcent  + (Ipeak-xstrip))*strip_width*2.0;

      double tanhdx = tanh(qk2*dx/AC);
      double tanh1 = (tanhdx + _tanhs[0][arm][station][cathplane][octant])/(1+tanhdx*_tanhs[0][arm][station][cathplane][octant]);
      double tanh2 = (tanhdx + _tanhs[1][arm][station][cathplane][octant])/(1+tanhdx*_tanhs[1][arm][station][cathplane][octant]);
      double tanh3 = (tanhdx + _tanhs[2][arm][station][cathplane][octant])/(1+tanhdx*_tanhs[2][arm][station][cathplane][octant]);
      double tanh4 = (tanhdx + _tanhs[3][arm][station][cathplane][octant])/(1+tanhdx*_tanhs[3][arm][station][cathplane][octant]);

      tanh1 = atan(sqrt_qk3 * tanh1);
      tanh2 = atan(sqrt_qk3 * tanh2);
      tanh3 = atan(sqrt_qk3 * tanh3);
      tanh4 = atan(sqrt_qk3 * tanh4);

      double temp = A1 * (tanh3 - tanh2);

      // Add charge from capacitively coupled strips:
      temp += A2 * (tanh2 - tanh1);
      temp += A2 * (tanh4 - tanh3);

			if ( isnan(temp) || temp<1e-18 ) temp = 0.0;
			//Sanghoon
			//In SL7, +/-1e-19 level of fluctuation appears wihch make 0 in SL6 as a non-zero value
			//This fluctuation causes different minimization results

      // Normalize to anode charge and add to charge already calculated for strip:
      // Need to give real penalty if anode charge is going below zero?:
      if (gsl_vector_get (x, n + itrack) > 0)
        temp *= gsl_vector_get (x, n + itrack) ;
      else
        temp *= 999;

      Yi += temp;

      q_tot[itrack] += temp;

    }

    // M.Wysocki 3/20/2007 - Check if strip should be zero-suppressed here...
    if( Yi < q_mins[istrip] ) Yi = 0.;

    // Store total charge - measured charge in f:
    gsl_vector_set (f, istrip, (Yi - q_values[istrip])/q_errors[istrip]);

  }

  // Put q_tot into fit vector
  for( unsigned int itrack = 0; itrack < p->ntracks; itrack++)
  { p->qtot_out[itrack] = q_tot[itrack]; }

  return GSL_SUCCESS;
}

//___________________________________________________________
int TMutClusterFit::_expb_df( const gsl_vector * x, void *params, gsl_matrix * J)
{

  // retrieve parameters structure
  gsl_data* p = static_cast< gsl_data* > ( params );

  // initialize fit charges
  vector< double > q_fit( p->cluster_width, 0 );

  // cast gsl_vector to non const
  gsl_vector* y = (gsl_vector*) x;

  // allocate vector for calculated function value
  gsl_vector* f = gsl_vector_alloc(p->cluster_width);

  /*
    Derivative delta for xfit should be small so that we move a small distance
    w.r.t. the strip width; delta for Q can be larger:
  */
  vector< double > deltaa( 2*p->ntracks, 0 );
  for( unsigned int i=0; i<2*p->ntracks; i++)
  { deltaa[i] = (i < p->ntracks)  ? 0.001 : 0.01; }

  // Calculate, numerically, dQ(istrip)/dparam(iparam) for each strip:
  for( unsigned int iparam=0; iparam < 2*p->ntracks; iparam++)
  {

    double aj = gsl_vector_get(x,iparam);
    double delta;
    delta = ( aj!= 0.0 ) ? deltaa[iparam]*aj : deltaa[iparam];

    // protect against using negative anode charge in determining slopes:
    if (iparam >= p->ntracks)
    {
      if (aj - delta < 0) delta = aj;
      if (aj + delta < 0) delta = -aj;
    }

    gsl_vector_set( y, iparam, aj + delta );
    int status = _expb_f( y, params, f );

    if( status == GSL_SUCCESS )
    {

      for ( int istrip=0; istrip < p->cluster_width; istrip++)
      { q_fit[istrip] = gsl_vector_get(f,istrip); }

    } else {

      if( _verbosity >= MUTOO::SOME )
      {

        cout << "TMutClusterFit::expb_df - status = " << gsl_strerror( status ) << endl;
        for( int istrip=0; istrip < p->cluster_width; istrip++)
        { cout << "qstrip = " << q_fit[istrip] << endl; }

      }

    }

    gsl_vector_set( y, iparam, aj - delta );
    if (delta != 0.0)
    {

      status = _expb_f(y, params,f);
      if(status == GSL_SUCCESS )
      {

        for( int istrip=0; istrip < p->cluster_width; istrip++)
        { gsl_matrix_set( J, istrip, iparam, (q_fit[istrip] - gsl_vector_get(f,istrip))/(2.0*delta)); }

      } else {

        if( _verbosity >= MUTOO::SOME )
        {

          cout << "TMutClusterFit::expb_df - status = " << gsl_strerror( status ) << endl;
          cout << "cluster_width = " << p->cluster_width << endl;

        }

        for( int istrip=0; istrip < p->cluster_width; istrip++)
        { gsl_matrix_set (J, istrip, iparam, 0.0 ); }

      }

    }

    gsl_vector_set( y, iparam, aj );

  }

  // free temporary vector
  gsl_vector_free(f);

  return GSL_SUCCESS;
}

//____________________________________________________________________
bool TMutClusterFit::initialize_tanhs( void )
{
  if( _verbosity >= MUTOO::ALOT ) {
    MUTOO::PRINT( cout, "TMutClusterFit::initialize_tanhs" ); }

  for( int arm=0; arm < MUTOO::NumberOfArms; arm++ )
  for( int station = 0; station < MUTOO::NumberOfStations; station++ ) {

    // get number of gaps for this station
    int n_gaps = TMutGeo::get_n_gaps( arm, station);

    // loop over octant, gap, cathodes

    for( int octant = 0; octant < 8; ++octant)
    for( int gap = 0; gap < 3; ++gap)
    for( int cathode = 0; cathode < MUTOO::NumberOfCathodePlanes; cathode++ )
    {

      // cathode index for this gap
      int cathode_index( MUTOO::NumberOfCathodePlanes*gap+cathode );

      if( gap >= n_gaps )
      for( int ith = 0; ith < 4; ith++ ) _tanhs[ith][arm][station][cathode_index][octant] = -999;
      else
      {

        // retrieve octant 0, half 0, strip 0 geometry
        const MutStrip* strip_geo = TMutGeo::get_strip_geom( arm, station, 0, 0, gap, cathode, 0 );
        if( !strip_geo )
        cout << "no geometry for strip [" << arm << "," << station << ",0,0," << gap << "," << cathode << ",0]" << endl;

        // calculate (half) strip_width
        double strip_width( ( strip_geo ? strip_geo->getStripSpacing():1.0 )/2 );

        // get capacitive couplig
        double AC = TMutMathiesonPar::get_mathieson_parameters(arm,station,octant,gap).second;

        // initialize tanhs values
        _tanhs[0][arm][station][cathode_index][octant] = tanh(-1.5*strip_width*qk2/AC);
        _tanhs[1][arm][station][cathode_index][octant] = tanh(-0.5*strip_width*qk2/AC);
        _tanhs[2][arm][station][cathode_index][octant] = tanh(0.5*strip_width*qk2/AC);
        _tanhs[3][arm][station][cathode_index][octant] = tanh(1.5*strip_width*qk2/AC);

      }
    }
  }
  if( _verbosity >= MUTOO::ALOT ) {
    MUTOO::PRINT( cout, "**" ); }
  return true;

}
