// $Id: mMutZeroSup.cxx,v 1.25 2013/07/23 21:25:21 slash Exp $
// MUTOO headers
//
#include <mMutZeroSup.h>
#include <mMutZeroSupPar.h>
#include <TMutNode.h>
#include <TMutHitMap.h>
#include <PHException.h>
#include <MUTOO.h>
#include <TMutDatabaseInit.h>

// PHENIX headers
#include <MutGeom.h>
#include <TMutGeo.h>
#include <MutCalib.h>
#include <MutStrip.h>
#include <Event.h>

// STL/BOOST
//
#include <iostream>
#include <string>

using namespace std;

//____________________________________________________
mMutZeroSup::mMutZeroSup() : 
  _timer(PHTimeServer::get()->insert_new("mMutZeroSup"))
{
  _total_hits.assign( 0 );
  _rejected_hits.assign( 0 );
  MUTOO::TRACE("initializing module mMutZeroSup",MUTOO::ALOT);
}

//____________________________________________________
PHBoolean mMutZeroSup::event(PHCompositeNode* top_node)
{

  // Timer
  _timer.get()->restart(); 
  
  try { 
    // Reset IOC pointers
    set_interface_ptrs(top_node);
    
    // loop over hits
    TMutHitMap::iterator hit_iter = _hit_map->range();
    while( TMutHitMap::pointer hit_ptr = hit_iter.next() )
    {
      
      // adds 1 to number of submitted hits
      int index( hit_ptr->get()->get_arm() + MUTOO::NumberOfArms*hit_ptr->get()->get_station() );
      _total_hits[index]++;
      
      if( is_hit_zero_suppressed(hit_ptr) ) {
    
        _hit_map->erase(hit_ptr->get()->get_key());
        _rejected_hits[index]++;
      
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

//_____________________________________________________________
void mMutZeroSup::print_summary( ostream& out ) const
{
  MUTOO::PRINT( out, "mMutZeroSup::summary" );
  
  for( int arm = 0; arm < MUTOO::NumberOfArms; arm++ )
  for( int station = 0; station < MUTOO::NumberOfStations; station++ ) {
  
    int index( arm + MUTOO::NumberOfArms*station );
    out 
      << ( (arm==MUTOO::South) ? "South":"North" ) << " station " << station 
      << " - total: " << _total_hits[index];
    if(  _total_hits[index] )
    out  
      << " rejected: " << _rejected_hits[index]
      << " fraction: " << double( _rejected_hits[index] )/_total_hits[index]; 
    out  << endl;
    
  }
  
  MUTOO::PRINT( out, "**" );

}

//____________________________________________________
void mMutZeroSup::set_interface_ptrs(PHCompositeNode* top_node){  

  // module runtime parameters
  _mod_par = TMutNode<mMutZeroSupPar>::find_node(top_node,"mMutZeroSupPar");

  // hit map pointer
  _hit_map = TMutNode<TMutHitMap>::find_node(top_node,"TMutHitMap");  
  
  // set calib pointer
  _calib = MutCalib();

} 

//_____________________________________________________________
bool mMutZeroSup::is_hit_zero_suppressed(TMutHitMap::const_pointer hit_ptr) 
{
  
  unsigned int arm      = hit_ptr->get()->get_arm();
  unsigned int station  = hit_ptr->get()->get_station();

  // strip calibration object
  const PdbMutCalibStrip *dbstrip = _calib->getPdbMutCalibStrip( 
    hit_ptr->get()->get_arm(),
    hit_ptr->get()->get_station(),
    hit_ptr->get()->get_octant(),
    hit_ptr->get()->get_half_octant(),
    hit_ptr->get()->get_gap(),
    hit_ptr->get()->get_cathode(),
    hit_ptr->get()->get_strip());
  
  // strip geometry object
  MutStrip *strip_ptr = TMutGeo::get_strip_geom(hit_ptr->get()->get_arm(),
    hit_ptr->get()->get_station(),
    hit_ptr->get()->get_octant(),
    hit_ptr->get()->get_half_octant(),
    hit_ptr->get()->get_gap(),
    hit_ptr->get()->get_cathode(),
    hit_ptr->get()->get_strip());
  
  assert( strip_ptr );
    
  bool processed( false );
  bool suppressed( false );
  bool verbose( _mod_par->get_verbosity() >= MUTOO::SOME );
  
  if( verbose ) 
  { 
    cout 
      << "mMutZeroSup::is_hit_zero_suppressed -"
      << " [" << arm << "," << station << "]"
      << " key: " << hit_ptr->get()->get_key().get_obj_key() 
      << " packet: " << strip_ptr->getPacket_ID()
      << " dcm channel: " << strip_ptr->getDCMChannel()
      << endl;
  }
  
    //! return false (i.e. hit accepted) if dbStrip is not found.
  if( !dbstrip ) {
    
    if( verbose )
    {
      cout 
        << "mMutZeroSup::is_hit_zero_suppressed -"
        << " [" << arm << "," << station << "]"
        << " no db strip"
        << endl;
    }
    return false;
    
  }
  
  float gain = dbstrip->get_gain();
  float pedestal = dbstrip->get_pedestal();
  float rms = dbstrip->get_rms();
  
  // temporaries for readability
  unsigned short sample0 = hit_ptr->get()->get_adc(0);  
  unsigned short sample1 = hit_ptr->get()->get_adc(1);  
  unsigned short sample2 = hit_ptr->get()->get_adc(2);  
  unsigned short sample3 = hit_ptr->get()->get_adc(3);  

  // mode == 0; doing nothing
  if( _mod_par->get_mode() == mMutZeroSupPar::NONE )    
  return false;
    
  // first Monte-Carlo mode [from run2]
  if( (_mod_par->get_mode() & mMutZeroSupPar::MC_IDMUTC_DCM1) && !suppressed)    
  {
    processed = true;
    Float_t threshold_1 = _mod_par->get_mc_thresh_1(arm,station);
    suppressed = (pedestal-sample3)/gain <= rms*threshold_1; 

    if( suppressed && verbose )
    { cout << "mMutZeroSup::is_hit_zero_suppressed - [" << arm << "," << station << "] bad dcm found in zero suppression (MC)" << endl; }
    
  }
    
  // second Monte-Carlo mode
  if( (_mod_par->get_mode() & mMutZeroSupPar::MC_IDMUTC_FPGA0SUP) && !suppressed) 
  {
    processed = true;
    Float_t threshold_1 = _mod_par->get_mc_thresh_1(arm,station);
    Float_t threshold_2 = _mod_par->get_mc_thresh_2(arm,station);
    
    suppressed = (sample0-sample2) <= rms*threshold_2 || (pedestal-sample2) <= rms*threshold_1;    
    
    if( suppressed && verbose )
    { cout << "mMutZeroSup::is_hit_zero_suppressed - [" << arm << "," << station << "] bad dcm found in zero suppression (MC)" << endl; }

  }
  
  // first data mode
  if( (_mod_par->get_mode() & mMutZeroSupPar::DCM) && !suppressed )
  {
    processed = true;

    float min_rel_sample = -0.1;
    float max_rel_sample = 1.4;

    // before Run13, DCM was rejecting anything with (pedestal-sample0)/(pedestal-sample2)>1
    PHTimeStamp timeStamp = TMutDatabaseInit::get_time_stamp();
    static PHTimeStamp time_run13(2013,1,1,0,0,0);
    if (timeStamp < time_run13)
      {
	min_rel_sample = -9999.9;
	max_rel_sample = 1.0;
      }

    if (pedestal-sample2 > 0){
      suppressed = (((pedestal-sample0)/(pedestal-sample2)) < min_rel_sample) ||
	((pedestal-sample0)/(pedestal-sample2)) >= max_rel_sample || 
	((pedestal-sample2) <= rms*_mod_par->get_thresh_1());
    }
    else{
      suppressed = ((sample0-sample2) <= rms*_mod_par->get_thresh_2()) || ((pedestal-sample2) <= rms*_mod_par->get_thresh_1());
    }

    if( suppressed && verbose )
    { cout << "mMutZeroSup::is_hit_zero_suppressed - [" << arm << "," << station << "] bad dcm found in zero suppression" << endl; }
    
  }
  
  if( (_mod_par->get_mode() & mMutZeroSupPar::SHAPE) && !suppressed )
  {
    processed = true;
    suppressed =(sample0-sample2)/(pedestal-sample2) < _mod_par->get_adc13_lo() ||
      (sample0-sample2)/(pedestal-sample2) > _mod_par->get_adc13_hi() ||
      (sample1-sample2)/(pedestal-sample2) < _mod_par->get_adc23_lo() ||
      (sample1-sample2)/(pedestal-sample2) > _mod_par->get_adc23_hi() ||
      (sample3-sample2)/(pedestal-sample2) < _mod_par->get_adc43_lo() ||
      (sample3-sample2)/(pedestal-sample2) > _mod_par->get_adc43_hi();
    
    if( suppressed && verbose )
    { cout << "mMutZeroSup::is_hit_zero_suppressed - [" << arm << "," << station << "] bad shape found in zero suppression" << endl; }
    
    if( suppressed && verbose ) {
      
      // identify which cut did not makeit
      boost::array< bool, 6 > cuts = {{false}};
      cuts[0] = (sample0-sample2)/(pedestal-sample2) < _mod_par->get_adc13_lo();
      cuts[1] = (sample0-sample2)/(pedestal-sample2) > _mod_par->get_adc13_hi();
      cuts[2] = (sample1-sample2)/(pedestal-sample2) < _mod_par->get_adc23_lo();
      cuts[3] = (sample1-sample2)/(pedestal-sample2) > _mod_par->get_adc23_hi();
      cuts[4] = (sample3-sample2)/(pedestal-sample2) < _mod_par->get_adc43_lo();
      cuts[5] = (sample3-sample2)/(pedestal-sample2) > _mod_par->get_adc43_hi();
      
      cout << "mMutZeroSup::is_hit_zero_suppressed - [" << arm << "," << station << "] cut pattern:";
      for( unsigned int i=0; i<6; i++ ) cout << cuts[i];
      cout << endl;
    
    }
    
  }
  
  if ((_mod_par->get_mode() & mMutZeroSupPar::BADCHAN) && !suppressed)
  {
    processed = true;
    bool bad_status( dbstrip->getStatus()<0 );
    if( bad_status && verbose )
    { cout << "mMutZeroSup::is_hit_zero_suppressed - [" << arm << "," << station << "] bad status found in zero suppression" << endl; }

    bool dead( strip_ptr->ChannelIsDead() );
    if( dead && verbose )
    { 
      cout 
        << "mMutZeroSup::is_hit_zero_suppressed -"
        << " [" << arm << "," << station << "]"
        << " dead channel found in zero suppression:" 
        << " ["
        << hit_ptr->get()->get_arm() << ","
        << hit_ptr->get()->get_station() << ","
        << hit_ptr->get()->get_octant() << ","
        << hit_ptr->get()->get_half_octant() << ","
        << hit_ptr->get()->get_gap() << ","
        << hit_ptr->get()->get_cathode() << ","
        << hit_ptr->get()->get_strip()  << "]"
        << endl;
    }
    
    bool bad_gain( dbstrip->get_gain()<1.0 );
    if( bad_gain && verbose )
    { cout << "mMutZeroSup::is_hit_zero_suppressed - [" << arm << "," << station << "] bad gain found in zero suppression: " << dbstrip->get_gain() << endl; }
    
    suppressed = bad_status || dead || bad_gain;
    
  } 

  if( (_mod_par->get_mode() & mMutZeroSupPar::TEST) && !suppressed )
  {
    processed = true;
    if (pedestal-sample2 > 0){
      suppressed = (((pedestal-sample0)/(pedestal-sample2)) < -0.2) ||
                 ((pedestal-sample0)/(pedestal-sample2)) >2.0 || 
                 ((pedestal-sample2) <= rms*_mod_par->get_thresh_1());
    }
    else{
      suppressed = ((sample0-sample2) <= rms*_mod_par->get_thresh_2()) || ((pedestal-sample2) <= rms*_mod_par->get_thresh_1());
    }
    if( suppressed && verbose )
    { cout << "mMutZeroSup::is_hit_zero_suppressed - [" << arm << "," << station << "] bad dcm found in zero suppression" << endl; }
  } 

  if (!processed)
  {
    throw runtime_error( DESCRIPTION( "unrecognized zero suppression mode" ) );
  }
 
  
  // dump suppressed hits      
  // if( out && _mod_par->get_verbosity() >= MUTOO::MAX ) 
  if( suppressed && verbose ) 
  {
    cout << "mMutZeroSup::is_hit_zero_suppressed -"
      << " pedestal:" << pedestal
      << " gain:" << gain
      << " rms:" << rms
      << " samples(0,1,2): (" << sample0 
      << " ," << sample1 
      << " ," << sample2
      << " ) "
      << " [rejected]" << endl; 
  }

  if( !suppressed && verbose )
  { cout << "mMutZeroSup::is_hit_zero_suppressed - [" << arm << "," << station << "] hit accepted" << endl; }
    
  return suppressed;
}
