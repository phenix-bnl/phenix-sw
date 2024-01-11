// $Id: mMutResponse.cxx,v 1.43 2019/03/27 21:06:49 slash Exp $

/*!
  \file mMutResponse.cxx
  \brief creates Hits from MC hits
  \author S.Kelly, H.Pereira
  \version $Revision: 1.43 $
  \date $Date: 2019/03/27 21:06:49 $
*/

// MUTOO headers
#include <mMutResponse.h>
#include <mMutResponsePar.h>
#include <TMutMCHitMap.h>
#include <TMutHitMap.h>
#include <TMutNode.h>
#include <PHException.h>
#include <MUTOO.h>
#include <MUTOO_FEM.h>
#include <PHTimer.h>
#include <TMutGeo.h>

//Mut Geometry
#include <MutWire.h>
#include <MutStrip.h>
#include <TMutUnstable.h>

//Mut Calibration
#include <MutCalib.h>

// STL/BOOST/GSL
#include <iostream>
#include <string>
#include <gsl/gsl_randist.h>

//PH Geometry tools
#include <PHLine.h>
#include <PHVector.h>
#include <PHPoint.h>
#include <PHGeometry.h>

/*! \ingroup modules */

using namespace std;

//_______________________________________________
mMutResponse::mMutResponse() :
  _timer( PHTimeServer::get()->insert_new("mMutResponse"))
{
  MUTOO::TRACE("initializing module mMutResponse",MUTOO::ALOT);
}

//_______________________________________________
// Event method.
PHBoolean mMutResponse::event(PHCompositeNode* top_node)
{
  _timer.get()->restart();
  try {

    // Reset IOC pointers
    set_interface_ptrs(top_node);

    // clear map
    _hit_map->clear();

    // Loop over TMutMCHits and generate TMutHit.
    // Add noise and apply masking where appropriate.
    response_loop();

  } catch(exception& e) {
    MUTOO::TRACE(e.what());
    return False;
  }
  _timer.get()->stop();

  if(_mod_par->get_verbosity() >= MUTOO::ALOT) _hit_map->print();
  if(_mod_par->get_verbosity() >= MUTOO::SOME) _timer.get()->print();
  return True;
}

//_______________________________________________
/*! Reset IOC and external interface pointers */
void mMutResponse::set_interface_ptrs(PHCompositeNode* top_node)
{
  // Module runtime parameters
  _mod_par = TMutNode<mMutResponsePar>::find_node(top_node,"mMutResponsePar");

  // IOC
  _mc_hit_map = TMutNode<TMutMCHitMap>::find_node(top_node,"TMutMCHitMap");
  _hit_map = TMutNode<TMutHitMap>::find_node(top_node,"TMutHitMap");
}

//_______________________________________________
void mMutResponse::response_loop()
{
  // Load status for unstable HV channel and strips
  TMutUnstable::load_status( gsl_rng_uniform(_rng.get()) );

  // Get an iterator to all the TMutMCHits
  TMutMCHitMap::const_iterator mc_hit_iter = _mc_hit_map->range();


  // Loop over TMutMCHits
  while(TMutMCHitMap::const_pointer mc_hit_ptr = mc_hit_iter.next())
  {
    unsigned short arm = mc_hit_ptr->get()->get_arm();
    unsigned short station = mc_hit_ptr->get()->get_station();
    unsigned short octant = mc_hit_ptr->get()->get_octant();
    unsigned short half_octant = mc_hit_ptr->get()->get_half_octant();
    unsigned short gap = mc_hit_ptr->get()->get_gap();

    // Check if MC hit is associated with bad anode HV and skip if
    // it is.
    if(check_hv_status(mc_hit_ptr) == false)
    { continue; }

    // Put chamber efficiency into hits:
    double eff = gsl_rng_uniform(_rng.get());
    bool skip_cath[MUTOO::NumberOfPlanes];
    for(int ipla=0;ipla<MUTOO::NumberOfPlanes;ipla++){
      skip_cath[ipla]=false;
      if (eff >= _mod_par->get_chamber_efficiency( arm, station, gap, ipla , octant, half_octant))
	skip_cath[ipla]=true;
    }

    /* Write strip charges from TMutMCHit object to corresponding
     * TMutHit objects.	One TMutMCHit has deposited charge on
     * multiple strips and so modifies multiple TMutHits.	And a
     * TMutHit may have charge from more than one TMutMCHit if the
     * clusters overlap.
     */
    update_cathode(mc_hit_ptr,skip_cath);
  }

  // Add noise hits, if specified
  if(_mod_par->add_noise_hits()) get_electronics_noise();

  // Get an iterator to all the TMutMCHit
  TMutHitMap::const_iterator hit_iter = _hit_map->range();
  while(TMutHitMap::const_pointer hit_ptr = hit_iter.next())
  calculate_adc_samples(hit_ptr);

}

//_______________________________________________
void mMutResponse::update_cathode(TMutMCHitMap::const_pointer mc_hit_ptr, bool skip_cath[MUTOO::NumberOfPlanes])
{
  unsigned short arm = mc_hit_ptr->get()->get_arm();
  unsigned short station = mc_hit_ptr->get()->get_station();
  unsigned short octant = mc_hit_ptr->get()->get_octant();
  unsigned short half_octant = mc_hit_ptr->get()->get_half_octant();
  unsigned short gap = mc_hit_ptr->get()->get_gap();

  typedef TMutMCHit::strip_list strip_list;
  typedef TMutMCHit::strip_list::const_iterator strip_iterator;

  // Get an iterator for list of TMutMCStrip from TMutMCHit
  const strip_list* strips = mc_hit_ptr->get()->get_strip_list();

  for( 	strip_iterator strip_iter = strips->begin(); strip_iter!=strips->end();++strip_iter)
  {

    /*
       skip the strip if charge there is NaN
       this a temporary fix. The origin of the NaN is from mMutSlowSim
       at the pisa to DST level
    */
    if( isnan( strip_iter->get_q() ) ) {
      //if( _mod_par->get_verbosity() >= MUTOO::NONE )
      cout << "mMutResponse::update_cathode - skipping strip due to NaN charge." << endl;
      continue;
    }

    // If TMutHit exists then add contribution from this TMutMCHit,
    // if not create a new TMutHit
    unsigned short cathode = strip_iter->get_cathode();
    unsigned short strip = strip_iter->get_strip();

    if(skip_cath[cathode]) continue;

    // Check strip status and skip if fails
    if(check_strip_status(arm,
      station,
      octant,
      half_octant,
      gap,
      cathode,
      strip) == false)
    { continue; }

    TMutHitMap::iterator hit_iter = _hit_map->get(arm,
      station,
      octant,
      half_octant,
      gap,
      cathode,
      strip);

    // Look for existing hit on this strip, if none then
    // make a new one.
    if(hit_iter.at_end())
    hit_iter = _hit_map->insert_new(arm,
      station,
      octant,
      half_octant,
      gap,
      cathode,
      strip);

    // Get the charge from the TMutHit
    double charge = hit_iter->get()->get_q();

    // Add the charge from the TMutMCStrip
    charge += strip_iter->get_q();

    // Update the charge
    hit_iter->get()->set_q(charge);

    if( _mod_par->get_verbosity() >= MUTOO::ALOT )
    cout << "mMutResponse::update_cathode - hit: " << hit_iter->get()->get_key().get_obj_key() << " q: " << charge << endl;

    if( _mod_par->get_verbosity() >= MUTOO::ALOT )
      cout << "mMutResponse::update_cathode - hit: " << arm << "\t" << station << "\t" << octant << "\t" << half_octant << "\t" << gap << "\t" << cathode  << endl;

    // associate MC hit with this hit for evaluation purpose
    associate_mchit(hit_iter, mc_hit_ptr);
  }

}

//_______________________________________________
void mMutResponse::associate_mchit(TMutHitMap::iterator hit_iter,
  TMutMCHitMap::const_pointer mc_hit_ptr)
{
  TMutHitMap::pointer hit_ptr = hit_iter.current();
  PHKey::associate(TMutMCHitMap::pointer(mc_hit_ptr), hit_ptr);
}

//_______________________________________________
void mMutResponse::calculate_adc_samples(TMutHitMap::const_pointer hit_ptr)
{

  int DACSaturation, ADCSaturation;

  // Get real pedestal, gain, and rms from the database to make
  // realistic, noisy charge samples and corresponding adc samples
  unsigned short arm = hit_ptr->get()->get_arm();
  unsigned short station = hit_ptr->get()->get_station();
  unsigned short octant = hit_ptr->get()->get_octant();
  unsigned short half_octant = hit_ptr->get()->get_half_octant();
  unsigned short gap = hit_ptr->get()->get_gap();
  unsigned short cathode = hit_ptr->get()->get_cathode();
  unsigned short strip = hit_ptr->get()->get_strip();

  MutCalibStrip *calib = MutCalib();

  const PdbMutCalibStrip *dbstrip = calib->getPdbMutCalibStrip(arm,station,octant,
     half_octant,
     gap,cathode,strip);

  MutStrip *strip_ptr = TMutGeo::get_strip_geom(arm, station, octant,
    half_octant, gap, cathode, strip);

  //Get drift time. This will need to be redone to fix overlapping clusters
  double drifttime = 0.;
  if (_mod_par->add_drift_time())
  {
    double hitToWire = 0.;
    int wirecounter = 0;
    double driftvelocity = 0.0095; // cm/nanosecond

    TMutMCHitMap::key_iterator mc_hit_iter = hit_ptr->get()->get_associated<TMutMCHit>();
    while(TMutMCHitMap::pointer mc_hit_ptr = mc_hit_iter.next()){
      wirecounter++;
      PHPoint mc_point(mc_hit_ptr->get()->get_x(),
         mc_hit_ptr->get()->get_y(),
         mc_hit_ptr->get()->get_z());

      MutWire *wire_ptr = TMutGeo::get_wire_geom(arm, station, octant, half_octant, gap, mc_hit_ptr->get()->get_wire());

      PHLine WireLine( wire_ptr->getGlobalPositionBegin(), wire_ptr->getGlobalPositionEnd());

      PHVector tracMom(mc_hit_ptr->get()->get_px(),
         mc_hit_ptr->get()->get_py(),
         mc_hit_ptr->get()->get_pz());

      PHLine trackPath(mc_point, tracMom);
      hitToWire = PHGeometry::distanceLineLine(trackPath, WireLine);
    }

    drifttime = hitToWire/driftvelocity;
    if (wirecounter > 1 && _mod_par->get_verbosity() >= MUTOO::SOME )
    {
      cout
        << "mMutResponse::calculate_adc_samples - " << wirecounter
        << " MC hits contributed to this cathode. "
        <<"Drift time not correctly implemented for overlapping clusters"
        <<endl;
    }
  }

  float pedestal=0, gain=0, rms=0;

  if (dbstrip) {
    gain = dbstrip->get_gain();
    rms = dbstrip->get_rms();
    pedestal = dbstrip->get_pedestal();
  }

  /*
  Should assume there is some error in the gain and pedestal measurements:
  still default values for get_gain_error and get_pedestal_error are 0
  */
  double sigma = 1.0;
  double ran_num = gsl_ran_gaussian(_rng.get(),sigma);
  gain *= (1.0 + _mod_par->get_gain_error() * ran_num);

  /* generate random number for common noise */
  double ran_num_common = gsl_ran_gaussian(_rng.get(),sigma);
  double smearing( ran_num_common * rms / gain );

  /*
    scale up the smearing by appropriate value (read from the par modules)
    these values have been tuned so that residuals in the detectors match
    between Monte Carlo and real data
  */
  if( _mod_par->get_use_rms_scale() ) smearing *= _mod_par->get_rms_scale( arm, station, gap );

  /* add noise to pedestal. By default, _mod_par->get_pedestal_error() is 0 and nothing is done */
  pedestal += _mod_par->get_pedestal_error() * ran_num_common;

  /*
     Calculate what the peak time should be (maximum of pulse), put this
     into equation for pulse shape so that V0 can be determined for hit's
     total charge, then calculate what V would be for each sample.
  */
  double trise = _mod_par->get_rise_time();
  double tfall = _mod_par->get_fall_time();
  double offset = _mod_par->get_offset();

  // drifttime will be zero if add_drift_time not enabled
  // this I think is wrong
  // should be offset = offset + drift time.

  // offset = offset - drifttime;
  offset = offset + drifttime;

  vector<double> tsamples = _mod_par->get_time_samples();
  vector<double> charge(tsamples.size(), 0.0);
  vector<int> adc_samples(tsamples.size(), 0);

  if (strip_ptr && _mod_par->get_atten_strips() && strip_ptr->UseAttenuation())
  hit_ptr->get()->set_q(hit_ptr->get()->get_q()/10.0);

  double tpeak = trise * log(1 + tfall/(trise));
  double v0 = hit_ptr->get()->get_q() / ((1 - exp(-tpeak/trise))*exp(-tpeak/tfall));

  // Use in common noise to add to samples:
  for (unsigned int isample=0; isample < tsamples.size(); isample++)
  {
    if(gain>0)
    {

      // get the charge from build-in signal shape
      charge[isample] = -v0*(1 - exp( (offset-tsamples[isample])/trise ))*exp( (offset-tsamples[isample])/tfall );

      /*
        add error to charge samples. Use the same random number for all four samples
        assuming that the noise is common to all of them (with the same magnitude)
        this has much more impact on the hit fit done later
      */
      if(_mod_par->get_smear_q())
      { charge[isample] += smearing; }

    } else charge[isample] = 0;

    if( dbstrip && dbstrip->isValid() )
    {

      DACSaturation = dbstrip->getSaturation();
      ADCSaturation = dbstrip->getAdc(DACSaturation);
      if (int(fabs(charge[isample])) < 256)
      { adc_samples[isample] = dbstrip->getAdc( float(fabs(charge[isample])) ); }
      else { adc_samples[isample] = ADCSaturation; }

    } else {

      adc_samples[isample] = 0;
      ADCSaturation = _mod_par->get_adcmin();

    }

    if(adc_samples[isample] < ADCSaturation)
    { adc_samples[isample] = ADCSaturation;	}

    if(adc_samples[isample] > _mod_par->get_adcmax())
    { adc_samples[isample] = _mod_par->get_adcmax(); }

    // Fill the TMutHit object with its adc values
    hit_ptr->get()->set_adc(isample, adc_samples[isample]);
  }

  if( _mod_par->get_verbosity() >= MUTOO::ALOT )
  {
    cout << "mMutResponse::calculate_adc_samples - hit: "
      << hit_ptr->get()->get_key().get_obj_key()
      << " [ ";
    for( unsigned int i=0; i<tsamples.size(); i++ )
    { cout << hit_ptr->get()->get_adc(i) << ", "; }
    cout << "] pedestal: " << pedestal << " noise: " << smearing << endl;
  }

  /*
     Now add error to the total charge
     It is normally not used since we are running the mMutCalibrate on top
     of the response, to recompute the charge according to the samples
     However, you may want to skip that part and still get usable hits
  */
  ran_num = gsl_ran_gaussian(_rng.get(),sigma);
  double smear_q = hit_ptr->get()->get_q();
  if( gain > 0 ) smear_q += ran_num * rms / gain;
  else smear_q = 0.0;

  // If q smear enabled, smear the strip q by the noise
  if(_mod_par->get_smear_q()) hit_ptr->get()->set_q(smear_q);

  // we set the error on the hit even if not smeared, to get the cluster fit behave properly
  // nagle - error should be consistent with above smearing - also needs to be consistent in calibrate mode
  hit_ptr->get()->set_error_q(rms/gain);

}

//____________________________________________________________________
bool mMutResponse::check_hv_status(const TMutMCHitMap::const_pointer mc_hit_ptr)
{

  unsigned short arm = mc_hit_ptr->get()->get_arm();
  unsigned short station = mc_hit_ptr->get()->get_station();
  unsigned short octant = mc_hit_ptr->get()->get_octant();
  unsigned short half_octant = mc_hit_ptr->get()->get_half_octant();
  unsigned short gap = mc_hit_ptr->get()->get_gap();
  unsigned short wire = mc_hit_ptr->get()->get_wire();

  MutWire *wire_ptr = TMutGeo::get_wire_geom(arm, station, octant, half_octant, gap, wire);

  // HV status is true if the wire is not dead.
  bool hv_on = false;
  if(wire_ptr)
    {
      hv_on =
	!(wire_ptr->ChannelIsDead()) &&
	TMutUnstable::check_hv_status( arm, station, octant, gap, wire );
    }
  return hv_on;

}

//____________________________________________________________________
bool mMutResponse::check_strip_status(unsigned short arm,
  unsigned short station,
  unsigned short octant,
  unsigned short half_octant,
  unsigned short gap,
  unsigned short cathode,
  unsigned short strip)
{
  return bool( (TMutGeo::get_strip_geom(arm, station, octant,half_octant, gap, cathode, strip)) &&
	       TMutUnstable::check_strip_status( arm, station, octant, half_octant, gap ,cathode, strip ) );
}

//____________________________________________________________________
void mMutResponse::get_electronics_noise()
{
  MutCalibStrip *calib = MutCalib();

  // Loop over packets so that we can choose to put in noise on a packet-by-packet
  // basis if desired
  MutDCMChannelMap* channelMap = SouthArmChannelMap();

  for (int ipacket=MUTOO_FEM::PACKET_ID_BASE; ipacket<=MUTOO_FEM::NORTH_PACKET_MAX ; ipacket++)
  {

    if (ipacket==MUTOO_FEM::SOUTH_PACKET_MAX+1) channelMap = NorthArmChannelMap();
    for (int dcmchan = 0; dcmchan < MUTOO_FEM::CATH_FEM_SIZE; dcmchan++)
    {

      MutStrip* CathStrip = channelMap->getMutStrip(ipacket, dcmchan);

      if (CathStrip)
      {

        const PdbMutCalibStrip* dbstrip = calib->getPdbMutCalibStrip(
          CathStrip->getArm(),
          CathStrip->getStation(),
          CathStrip->getOctant(),
          CathStrip->getHalfOctant(),
          CathStrip->getGap(),
          CathStrip->getCathode(),
          CathStrip->getStrip());

        if (dbstrip)
        {
          float rms = dbstrip->get_rms();
          double ran_num = gsl_ran_gaussian(_rng.get(), rms);
          double adc_hit = dbstrip->get_pedestal() + ran_num*rms;
          double charge_hit = dbstrip->getCharge((int)adc_hit);
          TMutHitMap::iterator hit_iter = _hit_map->get(CathStrip->getArm(),
            CathStrip->getStation(),
            CathStrip->getOctant(),
            CathStrip->getHalfOctant(),
            CathStrip->getGap(),
            CathStrip->getCathode(),
            CathStrip->getStrip());

          // Look for existing hit on this strip, if none then
          // make a new one.
          if(hit_iter.at_end())
          {
            hit_iter = _hit_map->insert_new(CathStrip->getArm(),
                CathStrip->getStation(),
                CathStrip->getOctant(),
                CathStrip->getHalfOctant(),
                CathStrip->getGap(),
                CathStrip->getCathode(),
                CathStrip->getStrip());
            hit_iter->get()->set_q(charge_hit);
          }
        }
      }
    }
  }
}
