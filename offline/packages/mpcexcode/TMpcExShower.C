#include "TMpcExShower.h"

#include <PHCompositeNode.h>
#include <TMpcExGeaHitContainer.h>
#include <TMpcExGeaHit.h>
#include <TMpcExHitContainer.h>
#include <TMpcExHit.h>
#include <getClass.h>
#include <fkinWrapper.h>
#include <Fun4AllServer.h>

#include <TMath.h>
#include <TGraphErrors.h>
#include <TF1.h>

unsigned int rand_uint_slow(void) {
  unsigned int r = 0;
  for (int i=0; i<32; i++) {
    r = r*2 + rand()%2;
  }
  return r;
}

//! Destructor
TMpcExShower::~TMpcExShower() {
  _hits.clear();
  for(unsigned int i=0; i<localHits.size(); i++){
    delete localHits[i]; 
  }
  localHits.clear(); 
}


//TMpcExShower::TMpcExShower(unsigned int arm) : _key(arm) {
TMpcExShower::TMpcExShower(unsigned int arm)  {

  _key = rand_uint_slow(); 
  _key1 = 0; 
  _key2 = 0; 
  _shower1 = NULL; 
  _shower2 = NULL; 

  _arm = arm;
  _hsx = -9999.;
  _hsy = -9999.;
  _seed_hsx = -9999.; 
  _seed_hsy = -9999.;
  _rms_hsx = -9999.; 
  _rms_hsy = -9999.; 
  _disp_hsx = -9999.; 
  _disp_hsy = -9999.; 
  _esum = -9999.; 
  _raw_esum = -9999.; 
  _nlayers = -1; 

  _ew_x = -9999.;
  _ew_y = -9999.;
  _ew_z = -9999.;

  for(int i=0; i<MpcExConstants::NLAYERS; i++){
    _e_layer[i] = 0.0;
    _n_layer[i] = 0;
    _rms_hsx_layer[i] = 0.0; 
    _rms_hsy_layer[i] = 0.0; 
    _disp_hsx_layer[i] = 0.0; 
    _disp_hsy_layer[i] = 0.0; 
    _hsx_layer[i] = 0.0;
    _hsy_layer[i] = 0.0;
  }
  _first_layer = -1; 
  _mpcCentTwr = -9999.; 
  _Nmpc3x3 = -1; 
  _Nmpc5x5 = -1; 
  _EmpcCorr = -9999.;
  for(int i=0; i<5; i++){
    for(int j=0; j<5; j++){
      _mpcTwrE[i][j] = 0.0;
      _mpcTwrTOF[i][j] = -9999.;
      _mpcTwrHS[i][j][0] = -9999.;
      _mpcTwrHS[i][j][1] = -9999.;
      _mpcQual[i][j] = -9999.0;
    }
  } 
  _mpc_pkix = -1; 
  _mpc_pkiy = -1; 
  _roughTotalEnergy = -9999.; 
  _CalibEInRange = 0; 
  _AlPlateEnergy = 0.0; 
  _ClosestMPCClusterDistance = -9999.; 
  _ClosestMPCClusterDistanceX = -9999.; 
  _ClosestMPCClusterDistanceY = -9999.; 
  _ClosestMPCClusterEnergy = -9999.; 
  _ClosestMPCClusterIndex = -1; 
  _ClosestMPCCluster = -1; 
  fiducial = -1; 
  _merged = -1; 
  _delete_me = -1;
  _n_iter = -1;
  _n_sat_minipads = -1;

  _fit_cached = false;
  _n_fit = 0; 
  _fit_intercept = -9999.0;
  _fit_slope = -9999.0; 
  _fit_chi2dof = -9999.0; 

  _n_fit = 0; 
  for(int i=0; i<MpcExConstants::NLAYERS+2; i++){
    _r_fit[i] = 0.0; 
    _r_fit_err[i] = 0.0; 
    _z_fit[i] = 0.0; 
    _z_fit_err[i] = 0.0; 
  }

  _vertex = 0.0; 

  _ExpansionFactor = 0.0; 

  _hits.clear(); 

  contributors.clear(); 
  ordered_index.clear(); 

  localStoreFilled = false; 
  localHits.clear(); 

}

TMpcExShower::TMpcExShower( TMpcExShower *shwr ){

  _key = shwr->key(); 
  _key1 = shwr->key1(); 
  _key2 = shwr->key2(); 
  _shower1 = shwr->getSourceShower(0); 
  _shower2 = shwr->getSourceShower(1); 

  _arm = shwr->get_arm();
  _hsx = shwr->get_hsx();
  _hsy = shwr->get_hsy();
  _vertex = shwr->get_vertex(); 
  _seed_hsx = shwr->get_seed_hsx(); 
  _seed_hsy = shwr->get_seed_hsy();
  _rms_hsx = shwr->get_rms_hsx(); 
  _rms_hsy = shwr->get_rms_hsy(); 
  _disp_hsx = shwr->get_disp_hsx(); 
  _disp_hsy = shwr->get_rms_hsy(); 
  _esum = shwr->get_esum(); 
  _raw_esum = shwr->get_raw_esum();
  _nlayers = shwr->get_nlayers(); 

  _ew_x = shwr->get_ew_x();
  _ew_y = shwr->get_ew_y();
  _ew_z = shwr->get_ew_z();

  for(int i=0; i<MpcExConstants::NLAYERS; i++){
    _e_layer[i] = shwr->get_e_layer(i);
    _n_layer[i] = shwr->get_n_layer(i);
    _rms_hsx_layer[i] = shwr->get_rms_hsx_layer(i); 
    _rms_hsy_layer[i] = shwr->get_rms_hsy_layer(i); 
    _disp_hsx_layer[i] = shwr->get_disp_hsx_layer(i); 
    _disp_hsy_layer[i] = shwr->get_disp_hsy_layer(i); 
    _hsx_layer[i] = shwr->get_hsx_layer(i);
    _hsy_layer[i] = shwr->get_hsy_layer(i);
  }
  _first_layer = shwr->get_first_layer(); 
  _mpcCentTwr = shwr->get_mpcCentTwr(); 
  _Nmpc3x3 = shwr->get_mpcN3x3(); 
  _Nmpc5x5 = shwr->get_mpcN5x5(); 
  _EmpcCorr = shwr->get_mpcECorr();
  for(int i=0; i<5; i++){
    for(int j=0; j<5; j++){
      _mpcTwrE[i][j] = shwr->get_mpcTwrE(i,j,true);
      _mpcTwrTOF[i][j] = shwr->get_mpcTwrTOF(i,j);
      _mpcTwrHS[i][j][0] = shwr->get_mpcTwrHS(i,j,0);
      _mpcTwrHS[i][j][1] = shwr->get_mpcTwrHS(i,j,1);
      _mpcQual[i][j] = shwr->get_mpc_quality(i,j);
    }
  } 
  _mpc_pkix = shwr->get_mpcPeakix(); 
  _mpc_pkiy = shwr->get_mpcPeakiy(); 
  _roughTotalEnergy = shwr->get_roughTotE(); 
  _CalibEInRange = shwr->get_CalibEInRange(); 
  _AlPlateEnergy = shwr->get_AlPlateEnergy(); 
  _ClosestMPCClusterDistance = shwr->get_ClosestMPCClusterDistance(); 
  _ClosestMPCClusterDistanceX = shwr->get_ClosestMPCClusterDistanceX(); 
  _ClosestMPCClusterDistanceY = shwr->get_ClosestMPCClusterDistanceY(); 
  _ClosestMPCClusterEnergy = shwr->get_ClosestMPCClusterEnergy(); 
  _ClosestMPCClusterIndex = shwr->get_ClosestMPCClusterIndex(); 
  _ClosestMPCCluster = shwr->get_ClosestMPCClusterClosestFlag(); 
  fiducial = shwr->get_fiducial(); 
  _merged = shwr->get_merged(); 
  _delete_me = shwr->get_delete_me();
  _n_iter = shwr->get_n_iter();
  _n_sat_minipads = shwr->get_n_sat_minipads();

  _fit_cached = shwr->_fit_cached; 
  _fit_intercept = shwr->_fit_intercept; 
  _fit_slope = shwr->_fit_slope; 
  _fit_chi2dof = shwr->_fit_chi2dof; 

  _n_fit = shwr->_n_fit; 
  for(int i=0; i<_n_fit; i++){
    _r_fit[i] = shwr->_r_fit[i]; 
    _z_fit[i] = shwr->_z_fit[i]; 
    _r_fit_err[i] = shwr->_r_fit_err[i]; 
    _z_fit_err[i] = shwr->_z_fit_err[i]; 
  }

  localStoreFilled = false; 
  unsigned int nhits = shwr->sizeHits(); 
  for(unsigned int i=0; i<nhits ; i++){
    addHit( shwr->getHit(i) );
  }
  if(shwr->localStoreFilled){
    for(unsigned int i=0; i<nhits ; i++){
      // NULL OK here - unused when localStoreFilled is not set
      TMpcExHit *hit = shwr->getHit(i, NULL); 
      TMpcExHit *newHit = hit->clone();     
      localHits.push_back(newHit); 
    }
  }
  localStoreFilled = shwr->localStoreFilled; 
    
  _ExpansionFactor = shwr->get_ExpansionFactor();

  contributors = shwr->contributors; 
  ordered_index = shwr->ordered_index; 

}

TMpcExShower::TMpcExShower( TMpcExShower *shwr1, TMpcExShower *shwr2, TMpcExHitContainer* _hit_map ){

  // This method just creates a new shower from the MPC-EX hits 
  // of the two showers. It does not update the MPC quantities. 
  // It is intended that the created shower will be used in the single 
  // track reconstruction, and then the MPC quantities updated using the 
  // shower peaks. 
  // It is assumed both showers come from the same arm, and the first 
  // shower is used to set the arm. 

  _key = rand_uint_slow(); 
  _key1 = shwr1->key(); 
  _key2 = shwr2->key(); 
  _shower1 = shwr1; 
  _shower2 = shwr2; 

  _hits.clear(); 
  localHits.clear(); 

  // To properly handle merging hits, merged showers fill the local store by default
  
  localStoreFilled = false; 
  unsigned int nhits = shwr1->sizeHits(); 
  for(unsigned int i=0; i<nhits ; i++){
    TMpcExHit *hit = shwr1->getHit(i, _hit_map); 
    addHit( hit->key() );
    TMpcExHit *newHit = hit->clone();     
    localHits.push_back(newHit); 
  }

  // When adding the second hits look for overlaps with the first shower, 
  // combine hits if necessary

  unsigned int nhits2 = shwr2->sizeHits(); 
  for(unsigned int i=0; i<nhits2 ; i++){

    TMpcExHit *hit = shwr2->getHit(i, _hit_map);
    unsigned int thisKey = hit->key(); 

    TMpcExHit *hitExists = NULL; 
    for(unsigned int j=0; j<nhits; j++){
      if(getHit(j)==thisKey){
	hitExists = getHit(j,_hit_map); 
	break; 
      }
    }

    if(hitExists){

      hitExists->set_low( hit->low() + hitExists->low() );
      hitExists->set_high( hit->high() + hitExists->high() );
      hitExists->set_status_low( hit->status_low()|hitExists->status_low() );
      hitExists->set_status_high( hit->status_high()|hitExists->status_high() );

      // Careful with minipad saturation
      // average mip at 15, average pedestal at 20 
      float satVal = (MpcExConstants::MIP_IN_keV/15.0)*(256-20); 
      if( (hit->combined() + hitExists->combined()) < satVal )
        hitExists->set_combined( hit->combined() + hitExists->combined() );
      else
        hitExists->set_combined( satVal );
	
      // Only valid combined hits are assigned to showers by the shower object
      hitExists->set_state_low( TMpcExHit::GAIN_CALIBRATED );
      hitExists->set_state_high( TMpcExHit::GAIN_CALIBRATED );
      hitExists->set_state_combined(TMpcExHit::VALID);
	
    }
    else{
      addHit( hit->key() );
      TMpcExHit *newHit = hit->clone();     
      localHits.push_back(newHit); 
    }

  }
  localStoreFilled = true; 

  _arm = shwr1->get_arm();
  _vertex = shwr1->get_vertex(); 

  // hsx, hsy are energy-weighted averages

  set_hsx( (shwr1->get_raw_esum()*shwr1->get_hsx() + shwr2->get_raw_esum()*shwr2->get_hsx())
	   /(shwr1->get_raw_esum()+shwr2->get_raw_esum()) ); 
  set_hsy( (shwr1->get_raw_esum()*shwr1->get_hsy() + shwr2->get_raw_esum()*shwr2->get_hsy())
	   /(shwr1->get_raw_esum()+shwr2->get_raw_esum()) ); 
  set_seed_hsx(get_hsx()); 
  set_seed_hsy(get_hsy()); 

  set_merged(0); 
  set_delete_me(0); 
  set_n_iter(0); 
  set_ExpansionFactor(0); 

  CalculateShowerMPCEXProperties(_hit_map); 

  _mpcCentTwr = -9999.; 
  _Nmpc3x3 = -1; 
  _Nmpc5x5 = -1; 
  _EmpcCorr = -9999.;
  for(int i=0; i<5; i++){
    for(int j=0; j<5; j++){
      _mpcTwrE[i][j] = 0.0;
      _mpcTwrTOF[i][j] = -9999.;
      _mpcTwrHS[i][j][0] = -9999.;
      _mpcTwrHS[i][j][1] = -9999.;
      _mpcQual[i][j] = -9999.0;
    }
  } 
  _mpc_pkix = -1; 
  _mpc_pkiy = -1; 
  _roughTotalEnergy = -9999.; 
  _CalibEInRange = 0; 
  _AlPlateEnergy = 0.0; 
  _ClosestMPCClusterDistance = -9999.; 
  _ClosestMPCClusterDistanceX = -9999.; 
  _ClosestMPCClusterDistanceY = -9999.; 
  _ClosestMPCClusterEnergy = -9999.; 
  _ClosestMPCClusterIndex = -1; 
  _ClosestMPCCluster = -1; 
  fiducial = -1; 

  _fit_cached = false;
  _n_fit = 0; 
  _fit_intercept = -9999.0;
  _fit_slope = -9999.0; 
  _fit_chi2dof = -9999.0; 

  // Need to call BuildContributorListMC() externally
  contributors.clear(); 
  ordered_index.clear(); 

}

void TMpcExShower::CalculateShowerMPCEXProperties(TMpcExHitContainer* _hit_map){

  double e_layer[MpcExConstants::NLAYERS] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; 
  int    n_layer[MpcExConstants::NLAYERS] = {0, 0, 0, 0, 0, 0, 0, 0}; 
  double rms_hsx_layer[MpcExConstants::NLAYERS] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; 
  double rms_hsy_layer[MpcExConstants::NLAYERS] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; 
  double sum_weight_X_layer[MpcExConstants::NLAYERS] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; 
  double sum_weight_Y_layer[MpcExConstants::NLAYERS] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; 
  double disp_hsx_layer[MpcExConstants::NLAYERS] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; 
  double disp_hsy_layer[MpcExConstants::NLAYERS] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; 
  double sum_weight_X_D_layer[MpcExConstants::NLAYERS] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; 
  double sum_weight_Y_D_layer[MpcExConstants::NLAYERS] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; 

  double x_layer[MpcExConstants::NLAYERS] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; 
  double y_layer[MpcExConstants::NLAYERS] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; 
  double z_layer[MpcExConstants::NLAYERS] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; 

  double x_layer_err[MpcExConstants::NLAYERS] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; 
  double y_layer_err[MpcExConstants::NLAYERS] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; 

  double hsx_layer[MpcExConstants::NLAYERS] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
  double hsy_layer[MpcExConstants::NLAYERS] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};

  float x_shower = 0.0; 
  float y_shower = 0.0; 
  float z_shower = 0.0; 

  // Calculate the RMS and DISPERSION of the shower
  // This also calculates the positions arrays 

  // First loop for the RMS
  unsigned int n_sat_minipads = 0; 
  unsigned int nhits = sizeHits(); 
  for(unsigned int i=0; i<nhits ; i++){

    //TMpcExHit *hit_ptr3 = _hit_map->get_hit_by_key( getHit(i) );
    TMpcExHit *hit_ptr3 = getHit(i,_hit_map);

    if(!hit_ptr3){
      std::cout << "ERROR - TMpcExShower::CalculateShowerMPCEXProperties missing hit = " << i << " total hits = " << nhits << std::endl; 
      continue; 
    }

    double q_hit = hit_ptr3->combined(); 
    short layer = hit_ptr3->layer();
	  
    // set for simulation only for now
    if( q_hit >= ((255-20.0)*(147.6/19.0)*4.5) ) n_sat_minipads++; 

    e_layer[layer] +=  q_hit;
    n_layer[layer]++; 

    x_shower += hit_ptr3->x()*q_hit; 
    y_shower += hit_ptr3->y()*q_hit; 
    z_shower += hit_ptr3->z()*q_hit; 

    // RMS

    double sigma = (hit_ptr3->minipad_x_width()/sqrt(12.0))/fabs(hit_ptr3->z()-_vertex); 
    double weight_X = q_hit*(1.0/(sigma*sigma));
    sigma = (hit_ptr3->minipad_y_width()/sqrt(12.0))/fabs(hit_ptr3->z()-_vertex); 
    double weight_Y = q_hit*(1.0/(sigma*sigma));
	  
    rms_hsx_layer[layer] +=  pow((hit_ptr3->x()/(hit_ptr3->z() - _vertex) - 
				  get_hsx()),2)*weight_X; 

    rms_hsy_layer[layer]  += pow((hit_ptr3->y()/(hit_ptr3->z() - _vertex) - 
				  get_hsy()),2)*weight_Y; 

    double hx = hit_ptr3->x()/(hit_ptr3->z()-_vertex);
    double hy = hit_ptr3->y()/(hit_ptr3->z()-_vertex);
    hsx_layer[layer] += hx*weight_X;
    hsy_layer[layer] += hy*weight_Y;

    sum_weight_X_layer[layer] += weight_X; 
    sum_weight_Y_layer[layer] += weight_Y; 

    x_layer[layer] += hit_ptr3->x()*weight_X;
    y_layer[layer] += hit_ptr3->y()*weight_Y;
    z_layer[layer] += hit_ptr3->z()*q_hit;

    x_layer_err[layer] += pow(weight_X*(hit_ptr3->minipad_x_width()/sqrt(12.0)),2); 
    y_layer_err[layer] += pow(weight_Y*(hit_ptr3->minipad_y_width()/sqrt(12.0)),2); 
  
  }

  set_n_sat_minipads(n_sat_minipads); 

  double eweight = 0.0; 
  for(int i=0; i<MpcExConstants::NLAYERS; i++) eweight += e_layer[i]; 
      
  x_shower /= eweight; 
  y_shower /= eweight; 
  z_shower /= eweight; 

  set_ew_x(x_shower); 
  set_ew_y(y_shower); 
  set_ew_z(z_shower); 

  // SECOND loop for the DISPERSION

  for(unsigned int i=0; i<nhits ; i++){

    //TMpcExHit *hit_ptr3 = _hit_map->get_hit_by_key( getHit(i) );
    TMpcExHit *hit_ptr3 = getHit(i,_hit_map);

    if(!hit_ptr3){
      std::cout << "ERROR - TMpcExShower::CalculateShowerMPCEXProperties missing hit = " << i << " total hits = " << nhits << std::endl; 
      continue; 
    }

    double q_hit = hit_ptr3->combined(); 
    short layer = hit_ptr3->layer();

    // Dispersion

    double sigma = (hit_ptr3->minipad_x_width()/sqrt(12.0))/fabs(hit_ptr3->z()-_vertex); 
    double weight_X = TMath::Max(0.0,(4.5+log(q_hit/eweight))*(1.0/(sigma*sigma)));
    sigma = (hit_ptr3->minipad_y_width()/sqrt(12.0))/fabs(hit_ptr3->z()-_vertex); 
    double weight_Y = TMath::Max(0.0,(4.5+log(q_hit/eweight))*(1.0/(sigma*sigma)));

    disp_hsx_layer[layer] +=  pow((hit_ptr3->x()/(hit_ptr3->z() - _vertex) - 
				   get_hsx()),2)*weight_X; 

    disp_hsy_layer[layer] +=  pow((hit_ptr3->y()/(hit_ptr3->z() - _vertex) - 
				   get_hsy()),2)*weight_Y; 

    sum_weight_X_D_layer[layer] += weight_X; 
    sum_weight_Y_D_layer[layer] += weight_Y; 

  }

  double rms_hsx = 0.0; 
  double rms_hsy = 0.0; 
  double disp_hsx = 0.0; 
  double disp_hsy = 0.0; 
  double sum_weight_X = 0.0; 
  double sum_weight_Y = 0.0; 
  double sum_weight_X_D = 0.0; 
  double sum_weight_Y_D = 0.0;  
  double r_layer[MpcExConstants::NLAYERS] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; 
  double r_layer_err[MpcExConstants::NLAYERS] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; 
     
  if(sizeHits()>1){

    for(int i=0; i<MpcExConstants::NLAYERS; i++){
	  
      rms_hsx += rms_hsx_layer[i]; 
      rms_hsy += rms_hsy_layer[i]; 

      sum_weight_X += sum_weight_X_layer[i]; 
      sum_weight_Y += sum_weight_Y_layer[i]; 

      disp_hsx += disp_hsx_layer[i]; 
      disp_hsy += disp_hsy_layer[i]; 

      sum_weight_X_D += sum_weight_X_D_layer[i]; 
      sum_weight_Y_D += sum_weight_Y_D_layer[i]; 

      if(e_layer[i]>0){

	rms_hsx_layer[i] = sqrt(rms_hsx_layer[i]/sum_weight_X_layer[i]); 
	rms_hsy_layer[i] = sqrt(rms_hsy_layer[i]/sum_weight_Y_layer[i]); 
	disp_hsx_layer[i] = sqrt(disp_hsx_layer[i]/sum_weight_X_D_layer[i]); 
	disp_hsy_layer[i] = sqrt(disp_hsy_layer[i]/sum_weight_Y_D_layer[i]); 
	    
	x_layer[i] = x_layer[i]/sum_weight_X_layer[i]; 
	y_layer[i] = y_layer[i]/sum_weight_Y_layer[i]; 
	z_layer[i] = z_layer[i]/e_layer[i]; 

	x_layer_err[i] = sqrt(x_layer_err[i])/sum_weight_X_layer[i]; 
	y_layer_err[i] = sqrt(y_layer_err[i])/sum_weight_Y_layer[i]; 

	r_layer[i] = sqrt(pow(x_layer[i],2) + pow(y_layer[i],2)); 
	r_layer_err[i] = sqrt(pow(x_layer[i]*x_layer_err[i],2) + pow(y_layer[i]*y_layer_err[i],2))/r_layer[i]; 

	hsx_layer[i] = hsx_layer[i]/sum_weight_X_layer[i]; 
	hsy_layer[i] = hsy_layer[i]/sum_weight_Y_layer[i]; 	    

      }
      else{
	    
	rms_hsx_layer[i] = 0.0; 
	rms_hsy_layer[i] = 0.0; 
	disp_hsx_layer[i] = 0.0; 
	disp_hsy_layer[i] = 0.0; 

	hsx_layer[i] = 0.0; 
	hsy_layer[i] = 0.0; 	    

      }

    }

    rms_hsx = sqrt(rms_hsx/sum_weight_X);
    rms_hsy = sqrt(rms_hsy/sum_weight_Y);

    disp_hsx = sqrt(disp_hsx/sum_weight_X_D);
    disp_hsy = sqrt(disp_hsy/sum_weight_Y_D);

  }
  else{

    rms_hsx = 0.0;
    rms_hsy = 0.0;

    disp_hsx = 0.0;
    disp_hsy = 0.0;

    for(int i=0; i<MpcExConstants::NLAYERS; i++){
      rms_hsx_layer[i] = 0.0; 
      rms_hsy_layer[i] = 0.0; 
      disp_hsx_layer[i] = 0.0; 
      disp_hsy_layer[i] = 0.0; 
      hsx_layer[i] = 0.0; 
      hsy_layer[i] = 0.0; 	    	  
    }

  }
      
  // Calculate number of layers, first layer
  int nlayers = 0;
  int first_layer = MpcExConstants::NLAYERS;
  for(int layer=0; layer<MpcExConstants::NLAYERS; layer++){
    if(e_layer[layer]>0.0) {
      if(first_layer == MpcExConstants::NLAYERS) first_layer = layer; 
      nlayers++;
    }
  }

  set_nlayers(nlayers);
  set_first_layer(first_layer); 

  // Fill in the shower information
  set_rms_hsx(rms_hsx); 
  set_rms_hsy(rms_hsy); 
  set_disp_hsx(disp_hsx); 
  set_disp_hsy(disp_hsy); 
  set_raw_esum(eweight/1.0e6);    // 1.0e6 convert keV->GeV
                 
  for(int layer=0; layer< MpcExConstants::NLAYERS; layer++){
    set_e_layer(layer,e_layer[layer]/1.0e6); 
    set_n_layer(layer,n_layer[layer]); 
    set_rms_hsx_layer(layer,rms_hsx_layer[layer]); 
    set_rms_hsy_layer(layer,rms_hsy_layer[layer]); 
    set_disp_hsx_layer(layer,disp_hsx_layer[layer]); 
    set_disp_hsy_layer(layer,disp_hsy_layer[layer]);
    set_hsx_layer(layer,hsx_layer[layer]); 
    set_hsy_layer(layer,hsy_layer[layer]); 
  }
      
  // Collect the r,z layer information 
  // Need to fill hit arrays as the shower information might not be contiguous
      
  float r_fit[MpcExConstants::NLAYERS]; 
  float r_fit_err[MpcExConstants::NLAYERS]; 
  float z_fit[MpcExConstants::NLAYERS]; 
  float z_fit_err[MpcExConstants::NLAYERS];
  int n_fit=0;
  for(int layer=first_layer; layer< MpcExConstants::NLAYERS; layer++){
    if(e_layer[layer]>0.0){
      r_fit[n_fit] = r_layer[layer];
      r_fit_err[n_fit] = r_layer_err[layer];
      z_fit[n_fit] = z_layer[layer];
      z_fit_err[n_fit] = 0.1; // 1.0 mm assumed z error
      n_fit++; 
    }
  }
  set_fit_data(n_fit,r_fit,z_fit,r_fit_err,z_fit_err); 
      
}


void TMpcExShower::ProjectVertex( bool include_mpc, bool include_vertex, bool use_cutoff )
{
  // if(_hits.size()>20.0){
  //   std::cout << _vertex << std::endl; 
  //   for(int i=0; i<_n_fit; i++){
  //     std::cout << _r_fit[i] << " " << _r_fit_err[i] << " " << _z_fit[i] << std::endl; 
  //   }
  // }

  // Add the MPC point into the fit if there is energy in it
  int fit_n = _n_fit; 
  if( (get_mpcE3x3(use_cutoff)>0.0) && include_mpc ){
    _r_fit[fit_n] = (MpcExConstants::MPC_REFERENCE_Z-_vertex)*sqrt( pow(get_mpc33hsx(use_cutoff),2) + pow(get_mpc33hsy(use_cutoff),2)); 
    _r_fit_err[fit_n] = (MpcExConstants::MPC_REFERENCE_Z-_vertex)*
                          sqrt( pow(get_mpc33hsx(use_cutoff)*get_mpc33hsx_err(use_cutoff),2) + pow(get_mpc33hsy(use_cutoff)*get_mpc33hsy_err(use_cutoff),2))/
                          sqrt( pow(get_mpc33hsx(use_cutoff),2) + pow(get_mpc33hsy(use_cutoff),2));
    _z_fit[fit_n] = MpcExConstants::MPC_REFERENCE_Z + (22.7/2.0); 
    if(_arm == 0) _z_fit[fit_n] = -_z_fit[fit_n];  
    _z_fit_err[fit_n] = (22.7/2.0)/4.0; // assumed error due to shower development 
    // if(_hits.size()>20.0) std::cout << _r_fit[fit_n] << " " << _r_fit_err[fit_n] << " " << _z_fit[fit_n] << std::endl; 
    fit_n++;
  }

  // Add the vertex point into the fit
  if( include_vertex ){
    _r_fit[fit_n] = 0.0; 
    _r_fit_err[fit_n] = 0.1; 
    _z_fit[fit_n] = _vertex; 
    if(_arm == 0) _z_fit[fit_n] = -_z_fit[fit_n];  
    _z_fit_err[fit_n] = 1.5;  // 1.5 cm vertex error assumed
    // if(_hits.size()>20.0) std::cout << _r_fit[fit_n] << " " << _r_fit_err[fit_n] << " " << _z_fit[fit_n] << std::endl; 
    fit_n++;
  }

  if(fit_n>1){
    TGraphErrors *gr = new TGraphErrors(fit_n,_z_fit,_r_fit,_z_fit_err,_r_fit_err); 
    TF1 *lineFit = new TF1("lineFit","pol1(0)",-MpcExConstants::MPC_REFERENCE_Z-100.0,MpcExConstants::MPC_REFERENCE_Z+100.0); 
    gr->Fit(lineFit,"Q"); 
    _fit_intercept = lineFit->GetParameter(0); // intercept
    _fit_slope = lineFit->GetParameter(1);     // slope
    if(fit_n>2)
      _fit_chi2dof = lineFit->GetChisquare()/lineFit->GetNDF();
    else
      _fit_chi2dof = lineFit->GetChisquare();
    delete gr;
    delete lineFit; 
  }
  else{
    _fit_intercept = -9999.0;
    _fit_slope = -9999.0;
    _fit_chi2dof = -9999.0; 
  }
 
  // if(_hits.size()>20.0){
  //   std::cout << _fit_intercept << " " << _fit_slope << " " << _fit_chi2dof << " " << -_fit_intercept/_fit_slope << " " 
  // 	      << _fit_intercept + _fit_slope*(MpcExConstants::MPC_REFERENCE_Z + (22.7/2.0)/1.8) << std::endl; 
  //   std::cout << std::endl; 
  // }

  _fit_cached = true; 

}


double TMpcExShower::getEta(double pt, double pz)
{
  float theta = XYtoPhi(pz,pt);
  float eta = -log(tan(theta/2.0));
  return eta; 
} 

double TMpcExShower::XYtoPhi(double x, double y)
{
  Double_t phi = atan2(y,x);
  if(phi<-TMath::PiOver2()) phi += TMath::TwoPi();
  if(phi>=3.0*TMath::PiOver2()) phi -= TMath::TwoPi();
  return phi;
}

void TMpcExShower::BuildContributorListMC(PHCompositeNode *topNode)
{

  contributors.clear(); 
  ordered_index.clear(); 

  TMpcExGeaHitContainer *geahits = findNode::getClass<TMpcExGeaHitContainer>(topNode,"TMpcExGeaHitContainer");
  // Check SIM node for embedding, it takes priority
  {
    Fun4AllServer *se = Fun4AllServer::instance();
    PHCompositeNode* _simNode = se->topNode("SIM");
    TMpcExGeaHitContainer *geahits2 = findNode::getClass<TMpcExGeaHitContainer>(_simNode,"TMpcExGeaHitContainer");
    if(geahits2) geahits = geahits2;
  }
  if(geahits == NULL) return; 

  // We will also need the calibrated hits as these will be used to calculate the energy fraction
  TMpcExHitContainer *hits = findNode::getClass<TMpcExHitContainer>(topNode,"TMpcExHitContainer");
  if(hits == NULL) return; 

  fkinWrapper *_fkin = findNode::getClass<fkinWrapper>(topNode,"fkin");
  // Check SIM node for embedding, it takes priority
  {
    Fun4AllServer *se = Fun4AllServer::instance();
    PHCompositeNode* _simNode = se->topNode("SIM");
    fkinWrapper *fkin2 = findNode::getClass<fkinWrapper> (_simNode, "fkin");
    if(fkin2) _fkin = fkin2;
  }
  if(_fkin == NULL) return; 

  // Loop over the assigned minipad keys, get the mctracks contributing to the
  // minipad, and track each back to a primary 
  // This builds a list of particles that entered the MPC-EX and contributed to 
  // the shower, and the fraction of MPC-EX energy they contributed

  float shwrEnergy = 0.0; 

  unsigned int nhits = this->sizeHits(); 
  for(unsigned int i=0; i<nhits ; i++){

    //TMpcExHit *hit = hits->get_hit_by_key( this->getHit(i) );
    TMpcExHit *hit = this->getHit(i,hits);
    if(!hit) continue; 
    float observedEnergy = hit->combined()/1.0e6;
    shwrEnergy += observedEnergy; 

    TMpcExGeaHit *hit_ptr = geahits->get_hit_by_key( this->getHit(i) );
    if(!hit_ptr) continue; 

    std::pair<std::map<int,float>::iterator,std::map<int,float>::iterator> itpair = hit_ptr->get_contributors(); 
    std::map<int,float>::iterator it = itpair.first; 

    // first get the total GEANT energy in this minipad
    float miniPadGeantEnergy = 0; 
    do{
      miniPadGeantEnergy += it->second; 
      it++; 
    }while(it!=itpair.second);

    // next, calculate the fraction of the observed, calibrated energy
    // that comes from each track. 
    it = itpair.first; 
    do{

      int mctrack = it->first;
      float energy = it->second*(observedEnergy/miniPadGeantEnergy); 

      // follow this track back until you find the primary

      int ifkin=GetIndexFromTrueTrack(_fkin,mctrack); 
      if(ifkin>=0){
		  	  
	bool primFound =  (_fkin->get_idparent(ifkin)!=0) ? false:true;  
	while(!primFound){
	  
	  mctrack = _fkin->get_itparent(ifkin); 
	  ifkin = GetIndexFromTrueTrack(_fkin,mctrack);
	  if(ifkin<0) {
	    std::cout << "TMpcExShower::BuildContributorListMC error - trail lost tracking minipad contributor!" << std::endl; 
	    break; 
	  }
	  primFound =  (_fkin->get_idparent(ifkin)!=0) ? false:true;  
	  
	}
	
	// Update the contributors list
	if(ifkin>=0){
	  std::map<int,float>::iterator it = contributors.find(mctrack); 
	  if(it!=contributors.end()){
	    // update the energy for this track in the map
	    float e = it->second; 
	    it->second = e + energy; 
	  }
	  else{
	    // add a new entry to the map
	    contributors[mctrack] = energy; 
	  }	  
	}

      }
      else{
	std::cout << "TMpcExShower::BuildContributorListMC error - minipad mctrack not found in fkin data!" << std::endl; 
      }

      it++; 
      
    }while(it!=itpair.second);

  }

  // iterate over the contributors and normalize the energy fraction

  if(contributors.size()>0){

    std::map<int,float>::iterator it = contributors.begin(); 
    do{
      it->second = it->second/shwrEnergy;     
      it++; 
    }while(it!=contributors.end()); 

    // Now fill the list of keys ordered by energy fraction 
    int idx = 1; 
    do{

      float efract = 0.0; 
      int key_found = 0; 
      std::map<int,float>::iterator it = contributors.begin(); 
      do{
	if( it->second > efract ){

	  bool found = false; 
	  if(ordered_index.size()>0){
	    std::map<int,int>::iterator ck = ordered_index.begin();
	    do{
	      if( ck->second == it->first ) {
		found = true;
		break;
	      }
	      ck++;
	    }while(ck!=ordered_index.end()); 
	  }

	  if(!found){
	    efract = it->second; 
	    key_found = it->first;
	  }

	}
	it++; 
      }while(it!=contributors.end()); 
    
      ordered_index[idx] = key_found; 
      idx++; 

    }while(ordered_index.size()<contributors.size());

  }
  
}

float TMpcExShower::GetTrueTrackFraction(PHCompositeNode *topNode, int true_track)
{

  TMpcExGeaHitContainer *geahits = findNode::getClass<TMpcExGeaHitContainer>(topNode,"TMpcExGeaHitContainer");
  // Check SIM node for embedding, it takes priority
  {
    Fun4AllServer *se = Fun4AllServer::instance();
    PHCompositeNode* _simNode = se->topNode("SIM");
    TMpcExGeaHitContainer *geahits2 = findNode::getClass<TMpcExGeaHitContainer>(_simNode,"TMpcExGeaHitContainer");
    if(geahits2) geahits = geahits2;
  }
  if(geahits == NULL) return 0.0; 

  // We will also need the calibrated hits as these will be used to calculate the energy fraction
  TMpcExHitContainer *hits = findNode::getClass<TMpcExHitContainer>(topNode,"TMpcExHitContainer");
  if(hits == NULL) return 0.0; 

  fkinWrapper *_fkin = findNode::getClass<fkinWrapper>(topNode,"fkin");
  // Check SIM node for embedding, it takes priority
  {
    Fun4AllServer *se = Fun4AllServer::instance();
    PHCompositeNode* _simNode = se->topNode("SIM");
    fkinWrapper *fkin2 = findNode::getClass<fkinWrapper> (_simNode, "fkin");
    if(fkin2) _fkin = fkin2;
  }
  if(_fkin == NULL) return 0.0; 

  float shwrEnergy = 0.0; 
  float trackEnergy = 0.0; 

  unsigned int nhits = this->sizeHits(); 
  for(unsigned int i=0; i<nhits ; i++){

    //TMpcExHit *hit = hits->get_hit_by_key( this->getHit(i) );
    TMpcExHit *hit = this->getHit(i,hits);
    if(!hit) continue; 
    float observedEnergy = hit->combined()/1.0e6;
    shwrEnergy += observedEnergy; 

    TMpcExGeaHit *hit_ptr = geahits->get_hit_by_key( this->getHit(i) );
    if(!hit_ptr) continue; 

    std::pair<std::map<int,float>::iterator,std::map<int,float>::iterator> itpair = hit_ptr->get_contributors(); 
    std::map<int,float>::iterator it = itpair.first; 

    // first get the total GEANT energy in this minipad
    float miniPadGeantEnergy = 0; 
    do{
      miniPadGeantEnergy += it->second; 
      it++; 
    }while(it!=itpair.second);

    // next, calculate the fraction of the observed, calibrated energy
    // that comes from this track. 
    it = itpair.first; 
    do{

      int mctrack = it->first;
      float energy = it->second*(observedEnergy/miniPadGeantEnergy); 

      // follow this track back until you terminate in a primary
      // or match the track you are looking found

      int ifkin=GetIndexFromTrueTrack(_fkin,mctrack); 
      if(ifkin>=0){

	bool trackFound = (mctrack==true_track) ? true:false; 
	bool primFound =  (_fkin->get_idparent(ifkin)!=0) ? false:true;  
	while(!primFound && !trackFound){
	  
	  mctrack = _fkin->get_itparent(ifkin); 
	  ifkin = GetIndexFromTrueTrack(_fkin,mctrack);
	  if(ifkin<0) {
	    std::cout << "TMpcExShower::GetTrueTrackFraction error - trail lost tracking minipad contributor!" << std::endl; 
	    break; 
	  }
	  trackFound = (mctrack==true_track) ? true:false;
	  primFound =  (_fkin->get_idparent(ifkin)!=0) ? false:true;  
	  
	}

	if(trackFound) trackEnergy += energy; 
	
      }
      else{
	std::cout << "TMpcExShower::GetTrueTrackFraction error - minipad mctrack not found in fkin data!" << std::endl; 
      }

      it++; 
      
    }while(it!=itpair.second);

  }

  // calculate and return the fraction

  if(shwrEnergy>0.0)
    return (trackEnergy/shwrEnergy); 
  else
    return 0.0; 

}


int TMpcExShower::GetIndexFromTrueTrack(fkinWrapper *_fkin, int true_track)
{

  int retIdx = -1; 

  if(_fkin && (true_track>0)){

    size_t nfkin =  _fkin->RowCount();
    for (size_t ifkin=0; ifkin<nfkin; ifkin++){
      if(_fkin->get_true_track(ifkin)==true_track){
	retIdx = ifkin; 
	break; 
      }
    }

  }

  return retIdx; 

}

void TMpcExShower::LocallyStoreHits(TMpcExHitContainer* _hit_map)
{

  if(localStoreFilled) return; 

  unsigned int nhits = sizeHits(); 
  for(unsigned int i=0; i<nhits ; i++){

    TMpcExHit *hit_ptr3 = _hit_map->get_hit_by_key( getHit(i) ); 

    if(!hit_ptr3){
      std::cout << "ERROR - TMpcExShower::LocallyStoreHits missing hit = " << i << " total hits = " << nhits << std::endl; 
      continue; 
    }

    TMpcExHit *localHit = hit_ptr3->clone(); 
    localHits.push_back(localHit); 

  }

  localStoreFilled = true; 

}

// add associated hits by key
// required for backwards compatability
void TMpcExShower::addHit(unsigned int key){ 
  _hits.push_back(key);

  if(localStoreFilled){
    std::cout << "TMpcExShower::ERROR - hit added by key when local store filled! The key list and local store will be out of sync." << std::endl; 
  }

}

// required for backwards compatability in mMpcExShower
unsigned int TMpcExShower::getHit(const size_t index)
{
   if(index<sizeHits()){
     if(localStoreFilled){
       return localHits[index]->key(); 
     }
     else{
       return _hits[index];
     }
   }
   return index;
}

TMpcExHit *TMpcExShower::getHit(unsigned int ihit, TMpcExHitContainer* _hit_map)
{

  if(localStoreFilled){
    if(ihit<localHits.size())
      return localHits[ihit];
    else
      return NULL;
  }
  else{
    TMpcExHit *hit = _hit_map->get_hit_by_key( this->getHit(ihit) );
    if(!hit) 
      return NULL;
    else
      return hit; 
  }

}

//! delete all of the hits in the container and clear it so there is nothing in the container
// just clear the list list fo keys, but delete locally held pointers
void TMpcExShower::ResetHits() 
{
  _hits.clear();
  for(unsigned int i=0; i<localHits.size(); i++){
    delete localHits[i]; 
  }
  localHits.clear(); 
}

