// $Id: mMutSlowSim.cxx,v 1.68 2015/11/05 02:27:22 shlim Exp $

/*!
  \file mMutSlowSim.cxx
  \brief Generate TMutMCHit and associated TMutMCTrk objects from PISA hits.
  \author S. Kelly
  \version $Revision: 1.68 $
  \date $Date: 2015/11/05 02:27:22 $
*/

#include <fkinWrapper.h>
#include <geantMass.h>
#include <TDatabasePDG.h>
#include <mumhitsWrapper.h>
#include <MUTOO.h>
#include <MUTOO_DIGIT.h>

#include <MutArm.h>
#include <MutGap.h>
#include <MutGeom.h>
#include <MutStrip.h>
#include <MutWire.h>
#include <PHException.h>
#include <PHGeometry.h>
#include <PHTable.hh>
#include <PHTimer.h>
#include <PISAEventHeader.h>
#include <PriPISAHit.h>
#include <primaryWrapper.h>
#include <root_ptrk.h>
#include <rootAnc.h>
#include <TMCPrimaryMap.h>
#include <TMutChargeCorrection.h>
#include <TMutGeo.h>
#include <TMutMathieson.h>
#include <TMutMCHitMap.h>
#include <TMutMCTrkMap.h>
#include <TMutNode.h>

#include <iostream>
#include <string>
#include <boost/array.hpp>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_math.h>
#include <cmath>

#include "mMutSlowSim.h"
#include "mMutSlowSimPar.h"

using namespace std;

//_____________________________________________
mMutSlowSim::mMutSlowSim() :
  _timer( PHTimeServer::get()->insert_new("mMutSlowSim") )
{
  _total_mc_hits.assign(0);
  _accepted_mc_hits.assign(0);

  MUTOO::TRACE("initializing module mMutSlowSim",MUTOO::ALOT);
}

//_____________________________________________
void mMutSlowSim::print_summary( ostream& out )
{
  MUTOO::PRINT( out, "mMutSlowSim::print_summary" );
  for( int arm=0; arm < MUTOO::NumberOfArms; arm++ )
  for( int station=0; station < MUTOO::NumberOfStations; station++ )
  out << "arm=" << arm << " station=" << station
    << " total_mc_hits=" << _total_mc_hits[ mMutSlowSimPar::get_acceptance_index( arm, station ) ]
    << " accepted_mc_hits=" << _accepted_mc_hits[ mMutSlowSimPar::get_acceptance_index( arm, station ) ]
    << endl;
  MUTOO::PRINT( out, "**" );
}

//_____________________________________________
// Event method.
PHBoolean mMutSlowSim::event(PHCompositeNode* top_node)
{
  _timer.get()->restart();
  try {

    // Reset IOC pointers
    set_interface_ptrs(top_node);

    // clear maps
    _mc_hit_map->clear();
    _mc_trk_map->clear();

    // Mine PISAEventHeader for primary data
    if(_mc_primary_map )
    {
      _mc_primary_map->clear();
      write_primaries();
    }

    // Loop over PISA hits and do the digitization
    simulator_loop();

    // At this point we have a list of TMutMCTrk objects
    // that have a set of associated TMutMCHits.	We loop
    // over the hits associated with a given track and
    // update the TMutMCTrk object with track parameters
    // at the most upstream MUTR gap
    finish_tracks();

    // associate primary tracks with hits and mctrk
    //
    associate_mctrk_and_primary();

  } catch(exception& e) {

    MUTOO::TRACE(e.what());
    return False;

  }

  // If verbose dump the contents of trks and primary
  //
  _timer.get()->stop();
  if(_mod_par->get_verbosity() >= MUTOO::ALOT) {
    if(_mc_primary_map) _mc_primary_map->print();
    _mc_trk_map->print();
  }
  return True;
}

//_____________________________________________
void mMutSlowSim::simulator_loop()
{
  // Loop over rows in PISA hits table [
  //	 If hit is associated with a tracking detector and is in active volume [
  //		 digitize hit
  //	 ]
  // ]

  if(_mod_par->get_verbosity() >= MUTOO::ALOT){
    MUTOO::TRACE("number of pisa hits = ", _mumhits_h.nok);
  }

  for(int ihit=0;ihit<_mumhits_h.nok;++ihit){

    // Derive standard MUTOO locators from PISA plane specification
    int arm = decode_pisa_location(_mumhits[ihit].plane,ARM);
    int station = decode_pisa_location(_mumhits[ihit].plane,STATION);
    int octant = decode_pisa_location(_mumhits[ihit].plane,OCTANT);

    if( _mod_par->get_verbosity() >= MUTOO::MAX ) cout << "mMutSlowSim::simulator_loop - read mumhit at [" << arm << "," << station << "," << octant << "]" << endl;

    // increment total number of mc hits
    _total_mc_hits[ mMutSlowSimPar::get_acceptance_index( arm, station ) ]++;

    // Do some checks to see that this is a hit in a tracking detector
    if(octant < 0) continue;
    if(station < 0 || station >= MUTOO::NumberOfStations) continue;

    // Check the hit is in the active volume
    if(in_active_volume(_mumhits[ihit]) == false) continue;

    // increment number of accepted mc hits
    _accepted_mc_hits[ mMutSlowSimPar::get_acceptance_index( arm, station ) ]++;

    // Passed all the criteria so do the digitization
    digitize(_mumhits[ihit]);
  }
}

//_____________________________________________
void mMutSlowSim::write_primaries()
{

  // Vertex data from PISAEventHeader
  //
  PISAEventHeader *pisaEventHeader = PISAEventHeader::GetEventHeader();
  if(!pisaEventHeader) return;

  int process_id = pisaEventHeader->GetEventInt(0);
  float x = pisaEventHeader->GetXvertex();
  float y = pisaEventHeader->GetYvertex();
  float z = pisaEventHeader->GetZvertex();
  int code = pisaEventHeader->GetEventCode();

  // Don't know what kind of MC were eating -- Fill in all the primary particle into primary map.
  //
  if(_mod_par->get_primary_mode() == mMutSlowSimPar::UNKNOWN)
  {

    // Get primary table header and data pointer.
    //
    TABLE_HEAD_ST pri_h = _primary->TableHeader();
    PRIMARY_ST *pri = _primary->TableData();

    for(int pri_index = 0; pri_index < pri_h.nok; pri_index++){

      // Get particle info.
      float px = pri[pri_index].px_momentum;
      float py = pri[pri_index].py_momentum;
      float pz = pri[pri_index].pz_momentum;
      double ptot = sqrt(
        MUTOO::SQUARE(px) +
        MUTOO::SQUARE(py) +
        MUTOO::SQUARE(pz) );

      int pid = pri[pri_index].idpart;
      int trkid = pri[pri_index].true_track;
      float mass = geantMass[pid-1];
      float E = sqrt(ptot*ptot + mass*mass);
      float imp = pisaEventHeader->GetImpactParameter();
      int Ncoll = pisaEventHeader->GetBinaryCollisions();

      // New TMCPrimary
      TMCPrimaryMap::iterator mc_primary_iter = _mc_primary_map->insert_new();
      mc_primary_iter->get()->set_pisa_process_id(process_id);
      mc_primary_iter->get()->set_pid((Int_t)pid);
      mc_primary_iter->get()->set_trk_id(trkid);
      mc_primary_iter->get()->set_x_orig(x);
      mc_primary_iter->get()->set_y_orig(y);
      mc_primary_iter->get()->set_z_orig(z);
      mc_primary_iter->get()->set_px_orig(px);
      mc_primary_iter->get()->set_py_orig(py);
      mc_primary_iter->get()->set_pz_orig(pz);
      if(code == 1)
	{
	  TDatabasePDG *pdgData = new TDatabasePDG();
		if ( pdgData->GetParticle(pid) ){
			mass = pdgData->GetParticle(pid)->Mass();
		}else{
			cout << "Can not find particle (" << pid << ") in the PDG database! Set mass 0 temporarily!" << endl;
			mass = 0.0;
		}
	  E = sqrt(ptot*ptot + mass*mass);
	  delete pdgData;
	}
      mc_primary_iter->get()->set_energy_orig(E);
      mc_primary_iter->get()->set_imp(imp);
      mc_primary_iter->get()->set_Ncoll(Ncoll);

      //cout << "MCPRIMARY - " << mc_primary_iter->get()->get_pidG4() << "  " << mass << "  " << E << endl;

    }
  }

  //Single mode
  if(_mod_par->get_primary_mode() == mMutSlowSimPar::SINGLE) {

    int priRows = PriPISAHit::GetPriCount();

    PriPISAHit *prihit = PriPISAHit::GetPriHitEvt();


    if(priRows != 1) throw runtime_error(DESCRIPTION("mode/input mismatch"));


    // Get particle info.
    float px = prihit->GetPx();
    float py = prihit->GetPy();
    float pz = prihit->GetPz();
    double ptot = sqrt(px*px + py*py + pz*pz);
    int pid =	prihit->GetIdpart();
    float mass = geantMass[pid-1];
    float E = sqrt(ptot*ptot + mass*mass);

    // New TMCPrimary
    TMCPrimaryMap::iterator mc_primary_iter = _mc_primary_map->insert_new();
    mc_primary_iter->get()->set_pisa_process_id(process_id);
    mc_primary_iter->get()->set_pid((Int_t)pid);
    mc_primary_iter->get()->set_x_orig(x);
    mc_primary_iter->get()->set_y_orig(y);
    mc_primary_iter->get()->set_z_orig(z);
    mc_primary_iter->get()->set_px_orig(px);
    mc_primary_iter->get()->set_py_orig(py);
    mc_primary_iter->get()->set_pz_orig(pz);
    if(code == 1)
      {
	TDatabasePDG *pdgData = new TDatabasePDG();
	mass = pdgData->GetParticle(pid)->Mass();
	E = sqrt(ptot*ptot + mass*mass);
	delete pdgData;
      }
    mc_primary_iter->get()->set_energy_orig(E);
  }

  // JPSI muons
  if(_mod_par->get_primary_mode() == mMutSlowSimPar::JPSI_MUON) {

    // JPSI data from decay Muons
    int priRows = PriPISAHit::GetPriCount();
    PriPISAHit *prihit = PriPISAHit::GetPriHitEvt();
    if(priRows != 2) throw runtime_error(DESCRIPTION("mode/input mismatch"));

    // First Muon
    float px1 = prihit->GetPx();
    float py1 = prihit->GetPy();
    float pz1 = prihit->GetPz();
    double ptot1 = sqrt(px1*px1 + py1*py1 + pz1*pz1);
    int pid1 =	prihit->GetIdpart();
    float mass1 = geantMass[pid1-1];
    float E1 = sqrt(
      MUTOO::SQUARE(ptot1) +
      MUTOO::SQUARE(mass1) );

    prihit++;

    // Second Muon
    //
    float px2 = prihit->GetPx();
    float py2 = prihit->GetPy();
    float pz2 = prihit->GetPz();
    double ptot2 = sqrt(
      MUTOO::SQUARE(px2) +
      MUTOO::SQUARE(py2) +
      MUTOO::SQUARE(pz2) );
    int pid2 =	prihit->GetIdpart();
    float mass2 = geantMass[pid2-1];
    float E2 = sqrt(
      MUTOO::SQUARE(ptot2) +
      MUTOO::SQUARE(mass2) );

    // Parent JPSI
    //
    int pid = 22;
    float px = px1+px2;
    float py = py1+py2;
    float pz = pz1+pz2;
    float E = E1 + E2;

    // New TMCPrimary
    TMCPrimaryMap::iterator mc_primary_iter = _mc_primary_map->insert_new();
    mc_primary_iter->get()->set_pisa_process_id(process_id);
    mc_primary_iter->get()->set_pid((Int_t)pid);
    mc_primary_iter->get()->set_x_orig(x);
    mc_primary_iter->get()->set_y_orig(y);
    mc_primary_iter->get()->set_z_orig(z);
    mc_primary_iter->get()->set_px_orig(px);
    mc_primary_iter->get()->set_py_orig(py);
    mc_primary_iter->get()->set_pz_orig(pz);
    if(code == 1)
      {
	TDatabasePDG *pdgData = new TDatabasePDG();
	mass1 = pdgData->GetParticle(pid1)->Mass();
	E1 = sqrt(MUTOO::SQUARE(ptot1) + MUTOO::SQUARE(mass1) );
	mass2 = pdgData->GetParticle(pid2)->Mass();
	E2 = sqrt(MUTOO::SQUARE(ptot2) + MUTOO::SQUARE(mass2) );
	E = E1 + E2;
	delete pdgData;
      }
    mc_primary_iter->get()->set_energy_orig(E);
  }
}

//_____________________________________________
void mMutSlowSim::digitize(const MUMHITS_ST& mumhit)
{
  /*
    Converts the mumhit into a distribution of charge on
    the cathode strips on both sides of the gap.	It includes lorentz error,
    anode and stereo angle diffusion error. The end product is a TMutMCHit object
    that contains a list of TMutMCStrip objects.	These TMutMCHit object are
    then associated with the appropriate TMutMCTrk object.
    Note: due to geometry inconsistancies between pisa and offline, the space point
    is extrapolated to the middle of the gap it hits
  */

  // local storage for pisa hit space point
  PHPoint hit_point( extrapolate_pisa_hit( mumhit ) );


  boost::array<MutStrip*, 2> strip_array;
  boost::array<MutWire*, 1> wire_array;
  boost::array<double, 2> strip_ip_array;
  double wire_ip(0);

  /*
    determine the pair of strips and anode wire closest to MumHit spacepoint.
    note:
  */
  int arm = decode_pisa_location(mumhit.plane,ARM);
  MutArm* geometry = MutGeom::get().get_arm( arm );
  int convert_err = geometry->convertPisaHitToChamber(
      hit_point,
      &wire_array[0], wire_ip,
      &strip_array[0], &strip_ip_array[0]);

  // Some conversion errors indicate an outdated phnx.par file.
  // The user needs to be notified.
  if(convert_err==5)
  {
    cout << "mMutSlowSim::digitize - PISA hit " << hit_point << " was not found in any station." << endl;
    cout << "mMutSlowSim::digitize - PISA hit " << PHPoint( mumhit.x[0], mumhit.x[1], mumhit.x[2] ) << " " << PHPoint(  mumhit.p[0], mumhit.p[1], mumhit.p[2] ) << endl;
  }

  if(convert_err==2)
  {
    cout << "mMutSlowSim::digitize - extrapolated PISA hit " << hit_point << " does not match to any MuTr chamber gap." << endl;
    cout << "mMutSlowSim::digitize - PISA hit " << PHPoint( mumhit.x[0], mumhit.x[1], mumhit.x[2] ) << " " << PHPoint(  mumhit.p[0], mumhit.p[1], mumhit.p[2] ) << endl;
    cout << endl;
  }

  // Didn't get a strip back from the geo call
  if(strip_array[0] == 0) return;

  // Use the wire returned to locate the
  // arm,station,octant,half_octant,gap
  MutWire* wire_ptr = wire_array[0];
  unsigned short station = wire_ptr->getStation();
  unsigned short octant = wire_ptr->getOctant();
  unsigned short half_octant = wire_ptr->getHalfOctant();
  unsigned short gap = wire_ptr->getGap();

  // Insert new TMutMCHit into map and set its data members
  TMutMCHitMap::iterator mc_hit_iter = _mc_hit_map->insert_new(arm,
     station,
     octant,
     half_octant,
     gap);


  mc_hit_iter->get()->set_track_id(mumhit.track);
  mc_hit_iter->get()->set_x(hit_point.getX());
  mc_hit_iter->get()->set_y(hit_point.getY());
  mc_hit_iter->get()->set_z(hit_point.getZ());
  mc_hit_iter->get()->set_px(mumhit.p[0]);
  mc_hit_iter->get()->set_py(mumhit.p[1]);
  mc_hit_iter->get()->set_pz(mumhit.p[2]);
  mc_hit_iter->get()->set_wire(wire_ptr->getWire());

  // Get the energy loss associated with this track crossing
  double dedx = get_energy_deposit( arm, station, octant, gap );

  mc_hit_iter->get()->set_eloss(dedx);

  /*
    add random assymetry for the charge splitting between the two cathodes
    by default, this is 0 anyway.
  */
  double asymetry = gsl_ran_gaussian(_rng.get(), _mod_par->get_correlation_width(arm,station,gap) );

  /*
    calculate charge correction from position in gap. Must be 'removed' from
    the MC charge, since it gets re-added in the FindGapCoord module
  */

  double charge_correction(
    TMutChargeCorrection::get_correction(
    mc_hit_iter->get()->get_location(),
    mc_hit_iter->get()->get_x(),
    mc_hit_iter->get()->get_y() ) );

  // Loop over 2 cathode planes
  for (int cathode=0; cathode<MUTOO::NumberOfCathodePlanes; cathode++){

    MutStrip* strip_ptr = strip_array[cathode];

    if(strip_ptr == 0) continue;

    // Distance from the hit to the center of nearest cathode strip
    double w_true = strip_ip_array[cathode];

    // Get angle between cathode strip and anode wire and offset
    // such that "straights" have a stereo_angle of zero.
    double stereo_angle = TMutGeo::get_angle_cathode_anode(arm,
       station,
       octant,
       half_octant,
       gap,
       cathode) - M_PI_2;

    // // Get error due to angle track makes with anode wire
    // double anode_error = get_anode_error(mumhit,wire_ptr);

    // // Get error due to Lorentz angle between drift direction and
    // // magnetic field vector
    // double lorentz_error = get_lorentz_error(mumhit);

    // Smear the w coordinate with appropriate modification for stereo strips
    double sin_stereo = sin(stereo_angle);
    // double cos_stereo = cos(stereo_angle);

    // nagle - july 20, 2004 - there is a small anode error in this measurement model
    // but the parameterization used in get_anode_error has an extra cos(theta)
    // determined after consultation with S.Kelly
    // double w_digit= w_true + sqrt( MUTOO::SQUARE(cos_stereo*lorentz_error) + MUTOO::SQUARE(cos_stereo*anode_error));
    double w_digit = w_true;

    // Correct stereo_strips centroid location;
    w_digit += get_stereo_correction(sin_stereo,wire_ip,station);

    // We smear w by gaussian with sigma specified by w_smear in the par object
    // nagle - july 20, 2004 - this was used before we did charge smearing for MC
    // and should be removed
    // w_digit += get_w_error();

    // Append w info to TMutMCHit
    mc_hit_iter->get()->set_w_true(cathode,w_true);
    mc_hit_iter->get()->set_w_digit(cathode,w_digit);

    // add asymetry to the charge
    double dedx_asym = dedx*(1.0 + asymetry*( (cathode==0) ? 1:-1) );

    // add charge correlation to cathode 1
    dedx_asym *= 1 - ( (cathode==0) ? 0:charge_correction );

    // Get distribution of charge from Mathieson utility class
    typedef TMutMathieson::strip_charge_array charge_array;
    charge_array charges = TMutMathieson::get_charge_array(dedx_asym, w_digit, strip_ptr);

    // Loop over charge/strip pairs and add strip to TMutMCHit
    charge_array::const_iterator chrg_iter = charges.begin();
    for(;chrg_iter!=charges.end();++chrg_iter) {

      if( isnan( chrg_iter->second ) )
      cout << "mMutSlowSim::digitize - mathieson lookup returned NaN charge - skipped." << endl;
      else mc_hit_iter->get()->add_strip(cathode,chrg_iter->first,chrg_iter->second);

    }

  }

  // Associate current TMutMCHit with appropriate TMutMCTrk
  associate_mctrk(mc_hit_iter.current());
}

//______________________________________________________________________
double mMutSlowSim::get_energy_deposit( int arm, int station, int octant, int gap ) const
{

  /*
  get pointer to relevant gap
  note: both half octants have the same landau parameters.
  We can pick either one
  */
  MutGap* gap_ptr = MutGeom::get().get_arm( arm )->
    f_pMutStations[station]->
    f_pMutOctants[octant]->
    f_pMutHalfOctants[MUTGEOM::HalfOctant1]->
    f_pMutGaps[gap];

  assert( gap_ptr );

  // Get scale and offset from module parameter table
  const double offset( gap_ptr->getLandauOffset() );
  const double scale( gap_ptr->getLandauScale() );

  // Energy deposited is offset + scale x (landau distributed random number)
  double landau( gsl_ran_landau(_rng.get()) );

  /*
    protection against gsl_ran_landau returning nan (not a number)
    for unknown reasons
  */
  while( isnan( landau ) )
  landau = gsl_ran_landau(_rng.get());

  double dedx = offset + scale * landau;

  if(dedx < 0.0) dedx = 0.0;
  return dedx;
}

//______________________________________________________________________
double mMutSlowSim::get_stereo_correction(double sin_stereo, double dx_wire, unsigned short station) const
{

  double stereo_error = dx_wire*sin_stereo;

  //This error looks different depending on which side of the gap it is.
  if(station==MUTOO::Station2) stereo_error *= -1;

  return stereo_error;
}

//______________________________________________________________________
double mMutSlowSim::get_anode_error(const MUMHITS_ST& mumhit, MutWire* wire_ptr) const
{
  // Error tracks that are not normal to the anode wire.
  PHVector wire_vector(cos(wire_ptr->getAngle()), sin(wire_ptr->getAngle()), 0);

  // Unit vector in wire direction
  wire_vector.normalize();

  // Unit vector in z direction
  PHVector z_axis(0,0,1);

  PHVector track_vector(mumhit.p[0],mumhit.p[1],mumhit.p[2]);

  // Components of track momentum in w and z directions respectively
  double x_w = track_vector.getX()*wire_vector.getX() +
    track_vector.getY()*wire_vector.getY();

  double x_z = track_vector.getZ();

  // Angle between z axis and track's projection onto wz plane
  double theta = atan2(fabs(x_w),fabs(x_z));

  // Convert angle to degrees and use parameterization taken
  // from old framework mMutXYtoFEE module to determine error.
  double theta_deg = fabs(theta*MUTOO::RAD_TO_DEG);
  double sigma = 1.0e-4*(-2.1*theta_deg + 0.457*MUTOO::SQUARE(theta_deg));

  if(sigma<0) return 0;

  // Return a Gaussian error with above sigma
  return gsl_ran_gaussian(_rng.get(),sigma);
}

//______________________________________________________________________
double mMutSlowSim::get_lorentz_error(const MUMHITS_ST& mumhit) const
{
  /* this code is obselete. always returns 0 */
  //	boost::array<float,3> x = {{mumhit.x[0],mumhit.x[1],mumhit.x[2]}};
  boost::array<float,3> b = {{0}};

  // Magnetic field lookup
  if(b[2] == 0) return 0;
  double theta = _mod_par->get_lorentz_base()*b[2];
  double sigma = 1.0E-4*(0.36*theta + 0.44*MUTOO::SQUARE(theta));
  return gsl_ran_gaussian(_rng.get(),sigma);
}

//______________________________________________________________________
double mMutSlowSim::get_w_error() const
{ return (_mod_par->get_w_smear() == 0) ? 0:gsl_ran_gaussian(_rng.get(),_mod_par->get_w_smear()); }

//______________________________________________________________________
/*! Reset IOC and external interface pointers */
void mMutSlowSim::set_interface_ptrs(PHCompositeNode* top_node)
{
  // module runtime parameters
  _mod_par = TMutNode<mMutSlowSimPar>::find_node(top_node,"mMutSlowSimPar");

  // IOC pointers
  _mc_hit_map = TMutNode<TMutMCHitMap>::find_node(top_node,"TMutMCHitMap");
  _mc_trk_map = TMutNode<TMutMCTrkMap>::find_node(top_node,"TMutMCTrkMap");

  try {
    _mc_primary_map = TMutNode<TMCPrimaryMap>::find_node(top_node,"TMCPrimaryMap");
  } catch (exception& e) { _mc_primary_map = 0; }

  // PISA input in wrapped STAF table form here.	Use of wrapped STAF tables
  // has been depreciated -- this interface will eventually go away.
  mumhitsWrapper* mumhits_ptr= TMutNode<mumhitsWrapper>::find_io_node(top_node,"mumhits");
  _mumhits_h = mumhits_ptr->TableHeader();
  _mumhits = mumhits_ptr->TableData();

  _fkinNode = TMutNode<fkinWrapper>::find_io_node(top_node,"fkin");
  _primary = TMutNode<primaryWrapper>::find_io_node(top_node,"primary");
}

//______________________________________________________________________
PHPoint mMutSlowSim::extrapolate_pisa_hit(const MUMHITS_ST& mumhit) const
{

  // Derive standard MUTOO locators from PISA plane specification
  int arm = decode_pisa_location(mumhit.plane,ARM);
  int station = decode_pisa_location(mumhit.plane,STATION);
  int octant = decode_pisa_location(mumhit.plane,OCTANT);
  int gap = decode_pisa_location(mumhit.plane,PLANE);

  /*
    retrieve gap mean z.
    half octant is choosen to 0. There should not be any z difference between two half octant
  */
  double global_z = TMutGeo::get_anode_plane_position( arm, station, octant, 0, gap).getZ();

  // need special handling of particles for which pz = 0 (!) to avoid error messages
  if( mumhit.p[2] == 0 ) return PHPoint( mumhit.x[0], mumhit.x[1], global_z );
  else return PHPoint(
    mumhit.x[0] + mumhit.p[0]/mumhit.p[2]*(global_z - mumhit.x[2]),
    mumhit.x[1] + mumhit.p[1]/mumhit.p[2]*(global_z - mumhit.x[2]),
    global_z );

}

//______________________________________________________________________
bool mMutSlowSim::in_active_volume(const MUMHITS_ST& mumhit) const
{
  return true;
  double r = sqrt(
    MUTOO::SQUARE(mumhit.x[0]) +
    MUTOO::SQUARE(mumhit.x[1]));

  // Theta = arctan(r/z)
  double theta = MUTOO::RAD_TO_DEG*atan2(r,(double) fabs(mumhit.x[2]));

  // Map from mumhit plane to MUTOO arm,station...
  int arm = decode_pisa_location(mumhit.plane,ARM);
  int station = decode_pisa_location(mumhit.plane,STATION);

  // Active volume in GEANT > that actual acceptance so we include
  // the ability to define angular acceptance in the modules runtime
  // parameters
  pair<double,double> acceptance = _mod_par->get_angular_acceptance(arm,station);
  return (theta > acceptance.first && theta < acceptance.second);
}

//______________________________________________________________________
void mMutSlowSim::associate_mctrk_and_primary()
{

  // Loop through all the mc tracks, associated them with primary tracks.
  //

  TMutMCTrkMap::iterator mc_trk_iter = _mc_trk_map->range();
  while(TMutMCTrkMap::pointer mc_trk_ptr = mc_trk_iter.next())
  {

    // only look for those who left a hit in Muon arms.
    TMutMCHitMap::key_iterator mchit_iter = mc_trk_ptr->get()->get_associated<TMutMCHit>();
    if(mchit_iter.count() == 0) continue;

    //first find the primary track id for the MC hits
    int parent_trk_id = mc_trk_ptr->get()->get_parent_track_id();

    // if parent_trk_id non-positive, it is already a primary track.
    if(parent_trk_id<=0)
    {
      // set grandparent track id to itself.
      int trk_id = mc_trk_ptr->get()->get_track_id();
      mc_trk_ptr->get()->set_grandparent_track_id(trk_id);
      continue;
    }

    int pri_trk_id = get_primary_trk_id(parent_trk_id);

    // if pri_trk_id==-1, which means no primary track is found for this hit.
    if(pri_trk_id == -1) continue;

    // Get an iterator to all TMCPrimary in the map
    TMCPrimaryMap::iterator mcpri_iter = _mc_primary_map->range();

    while(TMCPrimaryMap::pointer mcpri_ptr =	mcpri_iter.next())
    {

      if(mcpri_ptr->get()->get_trk_id()==pri_trk_id)
      {

        // pri track ids match so we make association
        PHKey::associate(mc_trk_ptr, mcpri_ptr);
        mc_trk_ptr->get()->set_grandparent_track_id(pri_trk_id);

        // We also make the mc hit and mc pri association
        while(TMutMCHitMap::pointer mchit_ptr = mchit_iter.next())
        { PHKey::associate(mchit_ptr, mcpri_ptr); }

        break;
      }
    }

  }
}

//_______________________________________________________________________________________
int mMutSlowSim::get_primary_trk_id(int parent_trk_id)
{
  // This is a recursive method to tracing the MC hit back to its most primary ancestor.
  // If a track has no parent, it's parend trackid is 0.
  // Thus, this method keeps calling itsself, until the parent id gets to zero ( parent track id less 0) or
  // no ancestor track can be found from the fkin table.

  // Get fkin table header and data pointer.
  TABLE_HEAD_ST fkin_h = _fkinNode->TableHeader();
  FKIN_ST *fkin = _fkinNode->TableData();

  if( _mod_par->get_verbosity() >= MUTOO::MAX )
  {
    cout << "mMutSlowSim::get_primary_trk_id - parent track id: " << parent_trk_id << endl;
    cout << "mMutSlowSim::get_primary_trk_id - Number of KIN tracks: " << fkin_h.nok << endl;
  }

  // find parent flag
  bool found = false;

  for(int fkin_index = 0; fkin_index < fkin_h.nok; fkin_index++)
  {

    if(fkin[fkin_index].true_track == 0) cout << "mMutSlowSim::get_primary_trk_id - true track in fkin table has 0. " <<endl;

    if( _mod_par->get_verbosity() >= MUTOO::MAX )
    { cout << "mMutSlowSim::get_primary_trk_id - current track id: " << fkin[fkin_index].true_track << endl; }

    if(fkin[fkin_index].true_track == parent_trk_id) {
      found = true;
      parent_trk_id = fkin[fkin_index].itparent;
      if(fkin[fkin_index].idparent==0||parent_trk_id<0) return fkin[fkin_index].true_track;
      break;
    }

  }

  if(found) return get_primary_trk_id(parent_trk_id);
  else {
    cerr << "mMutSlowSim::get_primary_trk_id - No primary particle is found. " << endl;
    return -1;
  }
}

//______________________________________________________________________
void mMutSlowSim::associate_mctrk(TMutMCHitMap::pointer mc_hit_ptr)
{
  // Get an iterator to all TMutMCTrk in map
  TMutMCTrkMap::iterator mctrk_iter = _mc_trk_map->range();

  // Loop over TMutMCTrk [
  //	If we find a track with matching track id
  //		associate this hit
  //	Else
  //		make a new TMutMCTrk
  // ]

  while(TMutMCTrkMap::pointer mctrk_ptr = mctrk_iter.next())
  {

    if(mctrk_ptr->get()->get_track_id() == mc_hit_ptr->get()->get_track_id())
    {

      // Track ids match so we make association and return
      PHKey::associate(mc_hit_ptr, mctrk_ptr);
      return;
    }
  }

  // No matching track id was found so we make an new TMutMCTrk
  fill_new_mctrk(mc_hit_ptr);
}

//______________________________________________________________________
void mMutSlowSim::fill_new_mctrk(TMutMCHitMap::pointer mc_hit_ptr, int trackID)
{

  int track_id = trackID;
  if (trackID == 0) track_id = mc_hit_ptr->get()->get_track_id();

  // Insert an new TMutMCTrk into map and fill information from pisa track id.
  TMutMCTrkMap::iterator mctrk_iter = _mc_trk_map->insert_new(mc_hit_ptr->get()->get_arm());
  mctrk_iter->get()->set_arm(mc_hit_ptr->get()->get_arm());
  mctrk_iter->get()->from_pisa( track_id, _mod_par->get_verbosity() );
  // mctrk_iter->get()->from_pisa( track_id, MUTOO::SOME );

  // Do the association
  if (trackID == 0) PHKey::associate(mc_hit_ptr, mctrk_iter.current());

  //Add a TMutMCTrk for the parent
  int itparent( mctrk_iter->get()->get_parent_track_id() );
  int idparent( mctrk_iter->get()->get_parent_id() );
  if (itparent > 0 && idparent != 0)
  {

    //Check to see if parent already has a track
    bool parent_track_filled = false;
    TMutMCTrkMap::iterator mctrk_iter2 = _mc_trk_map->range();
    while(TMutMCTrkMap::pointer mctrk_ptr = mctrk_iter2.next())
    {

      if(mctrk_ptr->get()->get_track_id() == abs(itparent))
      {
        parent_track_filled = true;
        break;
      }
    }

    if (!parent_track_filled) fill_new_mctrk(mc_hit_ptr, abs(itparent));
  }
}

//______________________________________________________________________
void mMutSlowSim::finish_tracks()
{
  // Loop over TMutMCTrk objects
  TMutMCTrkMap::iterator trk_iter = _mc_trk_map->range();
  while(TMutMCTrkMap::pointer trk_ptr = trk_iter.next()){

    // Get an iterator to the TMutMCHit associated with this track
    TMutMCHitMap::key_iterator hit_iter = trk_ptr->get()->get_associated<TMutMCHit>();

    // Do not update MC tracks if no MC mutr hit is associated.
    if(hit_iter.count() == 0) continue;

    // Loop over TMutMCHits and capture the upstream and downstream
    // hit pointers
    double min_z = 1e37;
    double max_z = -1e37;
    TMutMCHit* us_hit_ptr( 0 );
    TMutMCHit* ds_hit_ptr( 0 );
    while(TMutMCHitMap::pointer hit_ptr = hit_iter.next()) {
      double hit_z = fabs(hit_ptr->get()->get_z());
      if(hit_z < min_z){
        min_z = hit_z;
        us_hit_ptr = hit_ptr->get();
      }
      if(hit_z > max_z){
        max_z = hit_z;
        ds_hit_ptr = hit_ptr->get();
      }
    }

    // Update the TMutMCTrk with parameters from up
    trk_ptr->get()->set_x_us_gap(us_hit_ptr->get_x());
    trk_ptr->get()->set_y_us_gap(us_hit_ptr->get_y());
    trk_ptr->get()->set_z_us_gap(us_hit_ptr->get_z());
    trk_ptr->get()->set_px_us_gap(us_hit_ptr->get_px());
    trk_ptr->get()->set_py_us_gap(us_hit_ptr->get_py());
    trk_ptr->get()->set_pz_us_gap(us_hit_ptr->get_pz());

    // Update the TMutMCTrk with parameters from downstream hit
    trk_ptr->get()->set_x_ds_gap(ds_hit_ptr->get_x());
    trk_ptr->get()->set_y_ds_gap(ds_hit_ptr->get_y());
    trk_ptr->get()->set_z_ds_gap(ds_hit_ptr->get_z());
    trk_ptr->get()->set_px_ds_gap(ds_hit_ptr->get_px());
    trk_ptr->get()->set_py_ds_gap(ds_hit_ptr->get_py());
    trk_ptr->get()->set_pz_ds_gap(ds_hit_ptr->get_pz());
  }
}
