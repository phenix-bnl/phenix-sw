// $Id: mMutCalibrate.cxx,v 1.36 2017/10/12 01:40:32 shlim Exp $
/*
  \file mMutCalibrate.cxx
  \author  S.Kelly, H.Pereira
  \version $Revision: 1.36 $
  \date $Date: 2017/10/12 01:40:32 $
*/


#include <iostream>
#include <string>

#include <MutCalib.h>
#include <MUTOO.h>
#include <MUTOO_FEM.h>
#include <MutGeom.h>
#include <MutStrip.h>
#include <PHException.h>

#include <TMutDatabaseInit.h>
#include <TMutFitCathodeSamples.h>
#include <TMutGeo.h>
#include <TMutHitMap.h>
#include <TMutMCHitMap.h>
#include <TMutNode.h>

#include "mMutCalibrate.h"
#include "mMutCalibratePar.h"
#include "mMutResponsePar.h"

using namespace std;

//_______________________________________________
mMutCalibrate::mMutCalibrate() :
  _timer(PHTimeServer::get()->insert_new("mMutCalibrate"))
{
  MUTOO::TRACE("initializing module mMutCalibrate",MUTOO::ALOT);
}

//_________________________________________________
PHBoolean mMutCalibrate::event(PHCompositeNode* top_node)
{
  // Start the module timer
  _timer.get()->restart();

  try {

    set_interface_ptrs(top_node);
    set_fit_type(_mod_par->get_fit_type());

    // Iterator over all hits in map
    TMutHitMap::iterator hit_iter = _hit_map->range();
    while(TMutHitMap::pointer hit_ptr = hit_iter.next())
    {

      // set edge status
      if(
        hit_ptr->get()->get_strip() <= 1 ||
        hit_ptr->get()->get_strip() >= TMutGeo::get_n_strip( hit_ptr->get()->get_location() ) - 1 )
        { hit_ptr->get()->set_is_edge(); }

      // get calibrated samples
      Samples calib_samples = get_calibrated_samples(hit_ptr);
      if( calib_samples._rejected )
      {

        // see if hit is to be removed
        _hit_map->erase( hit_ptr->get()->get_key() );
        continue;

      } else {

        // fit samples
        fit_calibrated_samples( calib_samples, hit_ptr );

        // set  "bad" status
        /*
        for some reason setting both the bad and attenuated status
        results in some tracking efficiency loss. Still under investigation.
        */
        if( (!hit_ptr->get()->get_is_attenuated()) && hit_ptr->get()->get_error_q() > 20.0 )
        { hit_ptr->get()->set_is_bad(); }
      }


    }

  } catch(exception& e) {
    MUTOO::TRACE(e.what());
    return False;
  }

  // If verbose dump the contents of the hit map
  _timer.get()->stop();

  if(_mod_par->get_verbosity() >= MUTOO::ALOT) _hit_map->print();
  if(_mod_par->get_verbosity() >= MUTOO::SOME) _timer.get()->print();

  return True;
}

//_________________________________________________
void mMutCalibrate::set_interface_ptrs(PHCompositeNode* top_node)
{

  // retrieve module parameters
  _mod_par = TMutNode<mMutCalibratePar>::find_node(top_node,"mMutCalibratePar");

  // try retrieve response module parameters. Do not punt on not finding it
  // because it is not mandatory and does not exist when processing real data

  try {

    // The response parameter must be picked from the top_node parent
    // because during the embedding process the Calibrate module and the Response module
    // do not run under the same node in the tree (namely MUTOO for the former and
    // IOC_SIGNAL for the later.
    PHCompositeNode *parent( static_cast<PHCompositeNode*>( top_node->getParent() ) );

    // if the cast failed, run from this node, though it is unlikely to find a mMutResponsePar there.
    if( !parent ) parent = top_node;

    // look for the mMutResponsePar
    _res_mod_par = TMutNode<mMutResponsePar>::find_node( parent, "mMutResponsePar" );

  } catch( exception& e ) { _res_mod_par = 0; }

  // retrieve hit map and calibrations
  _hit_map = TMutNode<TMutHitMap>::find_node(top_node,"TMutHitMap");
  _calib = MutCalib();

}

//_________________________________________________
void mMutCalibrate::set_fit_type(mMutCalibratePar::FitType fit_type)
{
  switch( fit_type )
  {
    case mMutCalibratePar::AVERAGE:
    _fit_func = &mMutCalibrate::average_fit;
    break;

    case mMutCalibratePar::EXP:
    _fit_func = &mMutCalibrate::exponential_fit;
    break;

    default:
     throw runtime_error( DESCRIPTION( "invalid fit type specification in mMutCalibrate, defaulting to AVERAGE") );
    break;
  }
}

//_________________________________________________
mMutCalibrate::Samples mMutCalibrate::get_calibrated_samples(const TMutHitMap::pointer hit_ptr)
{

  // initialize output
  Samples calib_samples;

  // temporaries for clarity
  unsigned short arm = hit_ptr->get()->get_arm();
  unsigned short station = hit_ptr->get()->get_station();
  unsigned short octant = hit_ptr->get()->get_octant();
  unsigned short half_octant = hit_ptr->get()->get_half_octant();
  unsigned short gap = hit_ptr->get()->get_gap();
  unsigned short cathode = hit_ptr->get()->get_cathode();
  unsigned short strip = hit_ptr->get()->get_strip();

  // retrieve strip geometry
  MutStrip *strip_ptr = TMutGeo::get_strip_geom(arm, station, octant, half_octant, gap, cathode, strip);
  if( !strip_ptr ) {

    // didn't find valid geometry for this strip. Reject the hit
    calib_samples._rejected = true;

    MUTOO::TRACE("Missing strip geometry. hit rejected.");
    return calib_samples;

  }

  // get calibrations
  const PdbMutCalibStrip *dbstrip = _calib->getPdbMutCalibStrip(arm,station,octant, half_octant, gap,cathode,strip);
  if(!( dbstrip && dbstrip->isValid() ) )
  {

    // didn't find valid db info for this strip. Reject the hit
    calib_samples._rejected = true;
    if(_mod_par->get_verbosity()>=MUTOO::SOME)
    { MUTOO::TRACE("Missing DB info in mMutCalibrate, hit rejected."); }
    return calib_samples;

  }

  // retrieve gain and RMS
  float gain = dbstrip->get_gain();
  float rms = dbstrip->get_rms();
  int DACSaturation = dbstrip->getSaturation();
  int ADCSaturation = dbstrip->getAdc(DACSaturation);

  /*
     q_error comes from calibration Object
     it is the default value. It maybe overwritten by the sample fit
     done later, depending on the fit method you use
     nagle - just set to rms/gain and then adjust in the average_fit method
  */

  // if hit comes from MC, add error corresponding to the noise added at the response stage,
  // if any
  if(
    !hit_ptr->get()->get_associated<TMutMCHit>().empty() &&
    _res_mod_par &&
    _res_mod_par->get_smear_q()
    )
  {

    const double& rms_scale = _res_mod_par->get_rms_scale(
      hit_ptr->get()->get_arm(),
      hit_ptr->get()->get_station(),
      hit_ptr->get()->get_gap() );
    hit_ptr->get()->set_error_q( rms_scale * rms/gain );

  } else hit_ptr->get()->set_error_q(rms/gain);

  // Try to handle stuck bit in Run 4 until information gets
  // into the database:
  unsigned int packet_id = strip_ptr->getPacket_ID();
  PHTimeStamp timeStamp = TMutDatabaseInit::get_time_stamp();

  // this is needed to account for two bad packets found during run4, post-production
  static PHTimeStamp time1_11199(2004,1,9,0,0,0);
  static PHTimeStamp time2_11199(2004,2,9,0,0,0);
  static bool disable_11199 = false;
  if ((packet_id == 11199 || packet_id == 11200) && timeStamp > time1_11199 && timeStamp < time2_11199)
  {
    if( disable_11199 )
    {
      if( _mod_par->get_verbosity() >= MUTOO::ALOT )
      { cout << "mMutCalibrate::get_calibrated_samples - packet " << packet_id << " disabled" << endl; }

      calib_samples._rejected = true;
      return calib_samples;
    } else {
      if( _mod_par->get_verbosity() >= MUTOO::ALOT )
      cout
        << "mMutCalibrate::get_calibrated_samples -"
        << " packet " << packet_id << " enabled (enlarged error)" << endl;
      hit_ptr->get()->set_error_q(1024/gain);
    }
  }

  // this is needed to account for two bad packets found during run4, post-production
  static PHTimeStamp time1_11153(2004,3,6,0,0,0);
  static PHTimeStamp time2_11153(2004,5,30,0,0,0);
  if ((packet_id == 11153) && timeStamp > time1_11153 && timeStamp < time2_11153)
  { hit_ptr->get()->set_error_q(1024/gain); }

  // check attenuation
  if( strip_ptr->UseAttenuation() )
  {
    hit_ptr->get()->set_is_attenuated();
    hit_ptr->get()->set_error_q(MUTOO_FEM::ATTEN_ADC_ERROR);
  }

  bool has_negative_samples( false );
  for (unsigned short i=0; i<calib_samples.size(); ++i)
  {

    // set saturation bool if adc is out of range or gain is below
    // minimum value.  (gain conditions should be handled by bad
    // channel map)
    unsigned short adc = hit_ptr->get()->get_adc(i);

    // set saturation flag
    if( adc==MUTOO_FEM::ADC_MIN || adc<=ADCSaturation )
    { calib_samples._saturated = true; }

    // apply the calibration and store the resultant
    // q in the calib_samples array
    calib_samples.at(i).q = dbstrip->getCharge(adc);
    if (strip_ptr->ChannelIsDead())
    {
       calib_samples.at(i).q = 0.0;
       calib_samples._dead = true;
    }

    // negative charge means that
    // adc could not be converted into charge
    if (calib_samples.at(i).q < 0 )
    { has_negative_samples = true; }

    // set the time in data member array calib_samples
    // (sample times are fixed and stored in the module
    //  parameter table)
    calib_samples.at(i).t = _mod_par->get_t_sample(i);

  }

  /*
     check for negative samples.
     This is a sign that at least one conversion was ambigous
     we'll use the linear approximation instead
     For consistency, we use it on all samples
  */
  if( has_negative_samples )
  {
    for (unsigned short i=0; i<calib_samples.size(); ++i)
    {
      unsigned short adc = hit_ptr->get()->get_adc(i);
      calib_samples.at(i).q = dbstrip->getLinCharge(adc);
    }
  }

  return calib_samples;

}

//_________________________________________________
void mMutCalibrate::fit_calibrated_samples(const mMutCalibrate::Samples& calib_samples, TMutHitMap::pointer hit_ptr)
{
  // if this ADC is saturated then just return the 2'nd sample with
  // rms = SATURATED_ADC_ERROR
  if(calib_samples._saturated)
  {
    hit_ptr->get()->set_is_saturated();
    hit_ptr->get()->set_q(fabs(calib_samples.at(2).q));
    hit_ptr->get()->set_error_q(MUTOO_FEM::SATURATED_ADC_ERROR);
    return;
  }

  if (calib_samples._dead)
  {
    hit_ptr->get()->set_is_dead();
    hit_ptr->get()->set_q(fabs(calib_samples.at(2).q));
    hit_ptr->get()->set_error_q(MUTOO_FEM::DEAD_CHANNEL_ERROR);
    return;
  }

  // invoke the current sample fitting function via pointer
  // to member
  (this->*_fit_func)( calib_samples, hit_ptr);

}

//_________________________________________________
void mMutCalibrate::average_fit(const mMutCalibrate::Samples& samples, TMutHitMap::pointer hit_ptr)
{
  float qpeak = (samples.at(1).q + samples.at(2).q + samples.at(3).q)/3.0;
  float tpeak = (samples.at(1).t + samples.at(2).t + samples.at(3).t)/3.0;

  // linear interpolation to t0
  //double t0 = tpeak - ( (tpeak - samples.at(0).t)/(qpeak - samples.at(0).q) )*qpeak;
  float t0 = 0.;
	if ( fabs(qpeak - samples.at(0).q)>1e-4 ){
		t0 = tpeak - ( (tpeak - samples.at(0).t)/(qpeak - samples.at(0).q) )*qpeak;
	}
	//Sanghoon
	//There are case of qpeak-samples.at(0).q is 0 which make NAN value of t0 calculation
  hit_ptr->get()->set_q(qpeak);
  hit_ptr->get()->set_t(t0);

  // retrieve the current hit charge error error.
  float rms_over_gain = hit_ptr->get()->get_error_q();

  // calculate the RMS between the three samples as an additional error
  float rms_of_3samples = (
    MUTOO::SQUARE(samples.at(1).q - qpeak) +
    MUTOO::SQUARE(samples.at(2).q - qpeak) +
    MUTOO::SQUARE(samples.at(3).q - qpeak) ) / 3.0;

  if (rms_of_3samples > 0.0) rms_of_3samples = sqrt(rms_of_3samples);

  float new_error = MUTOO::SQUARE(rms_over_gain) + MUTOO::SQUARE(rms_of_3samples);
  if (new_error > 0.0) new_error = sqrt(new_error);

  hit_ptr->get()->set_error_q(new_error);

}

//_________________________________________________
void mMutCalibrate::exponential_fit(const mMutCalibrate::Samples& samples, TMutHitMap::pointer hit_ptr)
{
  // Map the input into the form Needed by Melynda's routine
  //
  boost::array<float,MUTOO_FEM::NSAMPLES> q_array; // sample charge
  boost::array<float,MUTOO_FEM::NSAMPLES> t_array; // sample time
  boost::array<float,MUTOO_FEM::NSAMPLES> e_array; // sample errors

  for(unsigned short i=0;i<samples.size();++i){
    q_array.at(i) = samples.at(i).q;
    t_array.at(i) = samples.at(i).t;
    // hardwired error (taken from old framework mMutApplyCalib.cc)
    //
    e_array.at(i) = 4.0;
  }

  float qpeak=0;
  float tpeak=0;
  float q0=0;
  float t0=0;
  float chi_sqr=0;
  int n_sample = _mod_par->get_n_sample();
  float t_rise = _mod_par->get_t_rise();
  float t_fall = _mod_par->get_t_fall();
  int fit_rise = _mod_par->get_fit_rise();
  int fit_fall = _mod_par->get_fit_fall();
  char fit_type[10] = "exp";

  // Call Melynda's routine with fit type hardwired to "exp"
  // The functional form in this routine is only currently
  // relevent for simulations.
  MUTOO::TMutFitCathodeSamples(
      t_array.data(),
      q_array.data(),
      e_array.data(),
      &n_sample,
      &t_rise,
      &t_fall,
      &q0,
      &t0,
      &qpeak,
      &tpeak,
      &chi_sqr,
      fit_type,
      &fit_rise,
      &fit_fall);

  hit_ptr->get()->set_q(qpeak);
  hit_ptr->get()->set_t(t0);
}
