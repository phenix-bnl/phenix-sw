//////////////////////////////////////////////////////////////////
/*
  \file TMutClusterFitEval.cxx
  \author: H. Pereira
  \version $Revision: 1.7 $
  \date    $Date: 2011/12/24 04:48:21 $
  \brief runtime evaluator for the TMutClusterFit
*/
//////////////////////////////////////////////////////////////////

#include "MUTOO_FEM.h"
#include "PHTFileServer.h"
#include "TMutClusterFitEval.h"
#include "TMutHitMap.h"
#include "TMutCoordMap.h"
#include "TMutGeo.h"
#include "TMutMCHitMap.h"

#include <MutCalibStrip.hh>
#include <MutCalib.h>
#include <MutStrip.h>

using namespace std;

// static members
bool TMutClusterFitEval::_do_evaluation = false;
TNtuple* TMutClusterFitEval::_ntuple = 0;
map< unsigned int, TMutClusterFitEval::DebugInfo > TMutClusterFitEval::_debug_map;
string TMutClusterFitEval::_filename = "muon_eval.root";

//___________________________________________________________
void TMutClusterFitEval::initialize_evaluation( void )
{

  if( !_do_evaluation ) return;
  
  // check if file exist
  if( _ntuple ) return;
  
  // create new TFile
  PHTFileServer::get().open( _filename, "RECREATE" );
  
  // create variable list - we try to keep 10 variables/separate line to make counting easier
  const char *var_list = 
    "event:arm:station:octant:half:gap:cath:cluswid:chi:nfit:"
		"nhits:badmiddlestrip:badedgestrip:leftedgedet:rightedgedet:saturation:atten:badcalib:nocalib:strip0:"
    "strip1:strip2:strip3:strip4:strip5:strip6:strip7:q1:q2:q3:"
    "q4:q5:q6:q7:q8:qe1:qe2:qe3:qe4:qe5:"
    "qe6:qe7:qe8:xfit:xfit2:xfit3:xfit4:res:we2:we3:"
    "we4:xtrue:xtrue2:xtrue3:xtrue4:truepeak1:truepeak2:truepeak3:truepeak4:reconpeak1:"
    "reconpeak2:reconpeak3:reconpeak4:ntracks:cluswid_over_int:cluswid_over_float:orig_ntracks:X1:X2:X3:"
    "X4:Q1:Q2:Q3:Q4:orig_chi2:new_fit:new_ntracks:new_X1:new_X2:"
    "new_X3:new_X4:new_Q1:new_Q2:new_Q3:new_Q4:new_chi2:fit_ac:fit_cc:"
    "gsl_called:X1err:X2err:X3err:X4err:Q1err:Q2err:Q3err:Q4err:new_X1err:"
    "new_X2err:new_X3err:new_X4err:new_Q1err:new_Q2err:new_Q3err:new_Q4err:"
    "xdigit:xdigit2:xdigit3:xdigit4"
    ":fit_error:pull_error:qmin1:qmin2:qmin3:qmin4:gain1:gain2:gain3:gain4:"
    "pedestal1:pedestal2:pedestal3:pedestal4:calibrms1:calibrms2:calibrms3:calibrms4:ntotalclusters";
     // we fill until 128. next free is 129 
  /* j.nagle - added extra variable */
  
  _ntuple = new TNtuple( "clus_reco","clus_reco", var_list );
  return;

}

//______________________________________________________________________
void TMutClusterFitEval::finish_evaluation( void )
{
  
  if( !_do_evaluation ) return;

  MUTOO::TRACE("TMutClusterFitEval::finish_evaluation");			 
  PHTFileServer::get().write( _filename );
		
  return;
}

//__________________________________________________
void TMutClusterFitEval::fill_evaluation_ntuple( TMutClusMap *clus_map )
{
  
  if( !clus_map ) return;
  if( !_do_evaluation ) return;
  
  if( !_ntuple ) initialize_evaluation();
  
  //! static local event number
  static unsigned int event = 0;

  // initialize calibration singleton
  MutCalibStrip *CalibPointer = MutCalib();
  
  // loop over clusters
  TMutClusMap::iterator clus_iter = clus_map->range();

  int ntotalclusters = clus_iter.count();

  //  if (ntotalclusters>0) {
  while(TMutClusMap::pointer clus_ptr = clus_iter.next()) {

    int arm     = clus_ptr->get()->get_arm();
    int station = clus_ptr->get()->get_station();
    int octant  = clus_ptr->get()->get_octant();
    int half    = clus_ptr->get()->get_half_octant();
    int gap     = clus_ptr->get()->get_gap();
    int cathode = clus_ptr->get()->get_cathode();
    

    // double check if associated TMutHits are ordered left to right...???? ( )  check when confirmed july 3, 2004 j.nagle

    int BadMiddleStrip=0;
    int BadEdgeStrip=0;
    bool  LeftEdgeDetector = false;
    bool  RightEdgeDetector = false;
    int Saturation = 0;
    int AttenStrip = 0;
    int BadCalib = 0;
    int NoCalib = 0;

    // allocate strip errors, charge and charge_error for 8 strips
    boost::array<int, 8> strip_number = {{0}};
    boost::array<double, 8> strip_q = {{0}};
    boost::array<double, 8> strip_q_err = {{0}};
    
    // look for assiciated hits
    TMutHitMap::key_iterator hit_iter = clus_ptr->get()->get_associated<TMutHit>();
    
    //! store number of hits
    int nhits   = hit_iter.count();

    // Loop over all strips associated with the cluster
    unsigned int index = 0;
    while(TMutHitMap::pointer hit_ptr = hit_iter.next()) {

      // only store up to 8 values
      if (index == 8) break;

      strip_number[index] = hit_ptr->get()->get_strip(); 
      strip_q[index] = hit_ptr->get()->get_q(); 
      strip_q_err[index] = hit_ptr->get()->get_error_q(); 

      int strip = hit_ptr->get()->get_strip();
      
      // For each strip check a number of issues - go through list below
      // (1) is there a bad (q_error) middle strip? - if it is one strip wide then both an edge and middle!
      if( hit_iter.count()==1 || (!(index==0 || index==(hit_iter.count()-1)))) 
	    if (hit_ptr->get()->get_error_q() > 20.0) 
	    BadMiddleStrip++;

      // (2) is there a bad (q_error) edge strip?
      if( index==0 || index == (hit_iter.count()-1) )
	    if( hit_ptr->get()->get_error_q() > 20.0 )
	    BadEdgeStrip++;

      // (3) is the cluster at the edge of the detector (left edge),(right edge)?
      int NumberOfStrips =  TMutGeo::get_n_strip(arm, station, octant, half, gap, cathode) - 1;
      if (index==0 && strip == 1) LeftEdgeDetector = true;
      if (index==(hit_iter.count()-1) && strip == NumberOfStrips) RightEdgeDetector = true;
	    
      // (3 1/2) is there a bad calibration
      // set calib pointer
      const PdbMutCalibStrip *StripCalib = CalibPointer->getPdbMutCalibStrip(arm, station, octant, half, gap, cathode, strip);
      if( !StripCalib ) NoCalib++;
	    else if( !(StripCalib->isValid()) ) BadCalib++;

      // (4) is there an attenuated strip? - should we add which one? (edge or middle?????)
      MutStrip* strip_ptr;
      strip_ptr = TMutGeo::get_strip_geom(arm, station, octant, half, gap, cathode, strip);
      if (strip_ptr && strip_ptr->UseAttenuation()) AttenStrip++;
	    
      // (5) is there a saturated strip?
      int DACSaturation, ADCSaturation;  
      bool this_strip_saturation = false;
      if (StripCalib) {
	      double gain = StripCalib->get_gain();
	      
        // double rms = StripCalib->get_rms();
	      DACSaturation = StripCalib->getSaturation();
	      ADCSaturation = StripCalib->getAdc(DACSaturation);
	
        // loop over all ADC samples and see if any are saturated...
	      for (unsigned short j=0; j<4; ++j){
	        unsigned short adc = hit_ptr->get()->get_adc(j);	
	  
          // set saturation bool if adc is out of range or gain is below
	        // minimum value.  (gain conditions should be handled by bad
	        // channel map)
      	  /*
      	    if (adc==MUTOO_FEM::ADC_MIN || gain < _mod_par->get_min_gain()
      	      || (adc<=ADCSaturation && adc<=MUTOO_FEM::SATURATED_ADC_ERROR)
      	      || !StripCalib->isValid() ){
      	  */
      	  // I believe the mMutCalibratePar.h sets min_gain(0) by default and is never reset
      	  if (adc==MUTOO_FEM::ADC_MIN || gain < 0 // hard code this value until you get _mod_par working :::  _mod_par->get_min_gain()
      	      || (adc<=ADCSaturation && adc<=MUTOO_FEM::SATURATED_ADC_ERROR)
      	      || !StripCalib->isValid() ){
      	    this_strip_saturation = true;
      	  }
	    
	      }
      }
      
      if (this_strip_saturation) Saturation++;

      index++;  // increment to next strip

    } // end loop over all hits in this cluster
	
    // get cluster fit chisquare
    double clus_fit_chisq = clus_ptr->get()->get_chi_square();
    
    // loop over all coordinates associated with the cluster
    TMutCoordMap::key_iterator coord_iter = clus_ptr->get()->get_associated<TMutCoord>();
    double num_coord =  coord_iter.count();  // number of coordinates fit for this cluster

    int ncoord  = 0;
    int nmchits = 0;

    boost::array< double, 4 > w_value = {{0}};    // store up to four w fit values
    boost::array< double, 4 > w_value_err ={{0}}; // store all four even though they all get the same error
    boost::array< double, 4 > coord_peak = {{0}}; // peak strip associated with the coordinate - this is what w is relative to

    boost::array< double, 4 > w_true = {{0}};
    boost::array< double, 4 > w_digit = {{0}};
    boost::array< double, 4 > true_peak = {{0}};

    // stores vector of associated mctracks
    set< unsigned int > MCTracks;

    while(TMutCoordMap::pointer coord_ptr = coord_iter.next()) {

      if (ncoord < 4) {
      	w_value[ncoord] = coord_ptr->get()->get_w();
	      w_value_err[ncoord] = coord_ptr->get()->get_error();
	      coord_peak[ncoord] = coord_ptr->get()->get_peak_strip();
      }
      
      // coord -> cluster -> hit -> mchit -> mc_track
      TMutClusMap::key_iterator temp_clus_iter = coord_ptr->get()->get_associated<TMutClus>();
      while( TMutClusMap::pointer temp_clus_ptr = temp_clus_iter.next() ){
	TMutHitMap::key_iterator temp_hit_iter = temp_clus_ptr->get()->get_associated<TMutHit>();
	while( TMutHitMap::pointer temp_hit_ptr = temp_hit_iter.next() ){
	  TMutMCHitMap::key_iterator mc_hit_iter = temp_hit_ptr->get()->get_associated<TMutMCHit>();
	  while(TMutMCHitMap::pointer mc_hit_ptr = mc_hit_iter.next()){
            
            // check if associated track already have been found
            if( MCTracks.find( mc_hit_ptr->get()->get_track_id() ) != MCTracks.end() ) break;
            
            // insert new track_id 
            MCTracks.insert( mc_hit_ptr->get()->get_track_id() );
	    nmchits++;
	    
	      // only store up to 4 values
	      if(nmchits <= 4) {
		w_true[nmchits-1] = mc_hit_ptr->get()->get_w_true((unsigned short)cathode); 
		w_digit[nmchits-1] = mc_hit_ptr->get()->get_w_digit((unsigned short)cathode); 
		const TMutMCHit::strip_list* strips = mc_hit_ptr->get()->get_strip_list();
		double temp_peak_q = 0.0;
		for(TMutMCHit::strip_iterator strip_iter=strips->begin(); strip_iter!=strips->end(); strip_iter++ )
		  if( (strip_iter->get_q() > temp_peak_q) && (strip_iter->get_cathode() == cathode)) {
		    temp_peak_q = strip_iter->get_q();
		    true_peak[nmchits-1] = strip_iter->get_strip();
		  }
	      }
	  }
	}
      }
      
      // increment number of coordinates
      ncoord++;

    }

    // fill all entries in ntuple
    boost::array<float,200> nt_vars = {{0}};	    

    nt_vars[0] = event;
    nt_vars[1] = arm;
    nt_vars[2] = station;
    nt_vars[3] = octant;
    nt_vars[4] = half;
      
    nt_vars[5] = gap;
    nt_vars[6] = cathode;
    nt_vars[7] = nhits;             // same as the width of the cluster
    nt_vars[8] = clus_fit_chisq;    // double check that this is per dof ( )
    nt_vars[9] = num_coord;         // number of coordinates fit to the cluster

    nt_vars[10] = nmchits;          // number of mc hits contributing to the cluster
    nt_vars[11] = BadMiddleStrip;
    nt_vars[12] = BadEdgeStrip;
    nt_vars[13] = LeftEdgeDetector;
    nt_vars[14] = RightEdgeDetector;

    nt_vars[15] = Saturation;
    nt_vars[16] = AttenStrip;
    nt_vars[17] = BadCalib;
    nt_vars[18] = NoCalib;
      
    // store 8 strip values, 8 q values, 8 qerror values
    for( int k=0;k<8;k++){
      nt_vars[19+k] = strip_number[k];
      nt_vars[27+k] = strip_q[k];
      nt_vars[35+k] = strip_q_err[k];
    }
    
    // store 4 w values, 4 w_error values, 4 w_true values
    for( int k=0;k<4;k++) {
      nt_vars[43+k] = w_value[k];
      nt_vars[47+k] = w_value_err[k];
      nt_vars[51+k] = w_true[k];
      nt_vars[55+k] = true_peak[k];
      nt_vars[59+k] = coord_peak[k];
      nt_vars[106+k] = w_digit[k];
    }
    // index = 62 filled. Next empty index: 63
    
    // retrieve cluster key
    unsigned int key = clus_ptr->get()->get_key().get_obj_key();
    
    // look for key in stored DebugInfo map
    map< unsigned int, DebugInfo >::const_iterator map_iter = _debug_map.find(key);
    
    if( map_iter != _debug_map.end() ) {
      const DebugInfo &debug = map_iter->second;
      nt_vars[63] = debug.is_multiple;
      nt_vars[64] = debug.width_over_int;
      nt_vars[65] = debug.width_over_double;
      nt_vars[66] = debug.n_tracks_1;
      for( unsigned int i=0; i<4; i++ ) {
        nt_vars[67+i] = debug.x_fit_1[i];
        nt_vars[71+i] = debug.q_fit_1[i];
      }
      nt_vars[75] = debug.chi2_1;

      nt_vars[76] = debug.fit_2_success;
      nt_vars[77] = debug.n_tracks_2;
      for( unsigned int i=0; i<4; i++ ) {
        nt_vars[78+i] = debug.x_fit_2[i];
        nt_vars[82+i] = debug.q_fit_2[i];
      }
      // nagle - off by one value
      nt_vars[86] = debug.chi2_2;
      
      nt_vars[87] = debug.ac;
      nt_vars[88] = debug.cc;
      nt_vars[89] = debug.gsl_was_called;
      
      for( unsigned int i=0; i<4; i++ ) {
        nt_vars[90+i] = debug.x_err_1[i];
        nt_vars[94+i] = debug.q_err_1[i];
      }
      
      for( unsigned int i=0; i<4; i++ ) {
        nt_vars[98+i] = debug.x_err_2[i];
        nt_vars[102+i] = debug.q_err_2[i];
	nt_vars[112+i] = debug.min_charges[i];
	nt_vars[116+i] = debug.gains[i];
	nt_vars[120+i] = debug.pedestals[i];
	nt_vars[124+i] = debug.rms[i];
      }

      nt_vars[110] = debug.error_from_fit;
      nt_vars[111] = debug.error_from_pulldist;

    }

    nt_vars[128] = ntotalclusters;

    /* 
      fill the ntuple. We pass the address of the first var
      to make root believe the boost::array is a table
    */
    if( _ntuple ) _ntuple->Fill( &nt_vars[0] );
  
  } // end loop over clusters
  //  } // j.nagle - end extra check on ntotalclusters>0 until problem resolved

  // increment local event counter
  event++;

}
