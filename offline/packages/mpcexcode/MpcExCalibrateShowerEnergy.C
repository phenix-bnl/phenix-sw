#include "TMpcExShower.h"
#include "MpcExCalibrateShowerEnergy.h"
#include "recoConsts.h"
#include <phool.h>
#include <iostream>
#include <cstdio>
#include <math.h> 

#include <TGraph.h>

#include "MpcExTSpline1.h"

#include "TMVA/Types.h"
#include "TMVA/Reader.h"
#include "TMVA/Tools.h"
#include "TMVA/MethodCuts.h"

using namespace std;

// The configuration data (xml files stored as char strings)

#include "ml_training_data_bdtg_arm0.h"
#include "ml_training_data_bdtg_arm0_inner.h"
#include "ml_training_data_bdtg_arm0_outer.h"
#include "ml_training_data_bdtg_arm0_E33_0.h"
#include "ml_training_data_bdtg_arm1.h"
#include "ml_training_data_bdtg_arm1_inner.h"
#include "ml_training_data_bdtg_arm1_outer.h"
#include "ml_training_data_bdtg_arm1_E33_0.h"

#include "ml_training_data_mlp_arm0.h"
#include "ml_training_data_mlp_arm0_inner.h"
#include "ml_training_data_mlp_arm0_outer.h"
#include "ml_training_data_mlp_arm0_E33_0.h"
#include "ml_training_data_mlp_arm1.h"
#include "ml_training_data_mlp_arm1_inner.h"
#include "ml_training_data_mlp_arm1_outer.h"
#include "ml_training_data_mlp_arm1_E33_0.h"


MpcExCalibrateShowerEnergy* MpcExCalibrateShowerEnergy::_instance = NULL;  

MpcExCalibrateShowerEnergy* MpcExCalibrateShowerEnergy::instance() {
  if(_instance == NULL){
    _instance = new MpcExCalibrateShowerEnergy();
  }
  return _instance;
}

MpcExCalibrateShowerEnergy::MpcExCalibrateShowerEnergy() {
  
  SetSplines(); 

  recoConsts *myrc = recoConsts::instance();
  
  if(myrc->get_IntFlag("MPCEX_ML_USE_MLP",0x0)){
     methodType = TMVA::Types::kMLP; 
    std::cout<<PHWHERE<<" Using MLP machine learning for total energy."<<std::endl;
  }
  else{
    methodType = TMVA::Types::kBDT; 
    std::cout<<PHWHERE<<" Using BDT machine learning for total energy."<<std::endl;
  }

  SetupRegression(); 

  ml_resum = 0.0; 
  ml_Cangle = 0.0; 
  ml_E33 = 0.0; 
  ml_in33 = 0.0; 
  ml_insat = 0.0; 
  ml_ifl = 0.0; 
  ml_ffx = 0.0; 
  ml_ffy = 0.0; 
  ml_vtx = 0.0; 

}

MpcExCalibrateShowerEnergy::~MpcExCalibrateShowerEnergy(){

  // delete the allocated splines
  // (splines delete the graph)

  for(int iarm=0; iarm<2; iarm++){
    for(int ilyr=0; ilyr<5; ilyr++){
      delete ex_calSpline[iarm][ilyr];
      if(ilyr<4) {
	delete mpc_calSpline[iarm][ilyr];
	delete comb_calSpline[iarm][ilyr];
      }
    }
    delete al_calSpline[iarm]; 
  }

  // delete the TMVA readers

  for(int i=0; i<2; i++){
    for(int j=0; j<3; j++){
      delete reader[i][j]; 
    }
    delete reader_E33_0[i]; 
  }

}

void MpcExCalibrateShowerEnergy::CalibrateEnergy(TMpcExShower *shower_v1, bool use_cutoff){

  DoCalibrateEnergy(shower_v1,shower_v1->get_raw_esum(),shower_v1->get_mpcE3x3(use_cutoff),NULL,NULL,NULL,true,NULL,false,0); 

}

void MpcExCalibrateShowerEnergy::CalibrateEnergyDecoupledPair(TMpcExShower *shower_v1, int nShared){

  DoCalibrateEnergy(shower_v1,shower_v1->get_raw_esum(),shower_v1->get_mpcE3x3(false),NULL,NULL,NULL,true,NULL,true,nShared); 

}

float MpcExCalibrateShowerEnergy::RecalCalibratedEnergy(TMpcExShower *shower_v1, float MpcEx_E, float MPC_E, 
							float *MPCXE, float *ALPLTE, float *MPCE, int *CalOKFlag, bool pairFlag){

  return DoCalibrateEnergy(shower_v1,MpcEx_E,MPC_E,MPCXE,ALPLTE,MPCE,false,CalOKFlag, pairFlag);

}

float MpcExCalibrateShowerEnergy::DoCalibrateEnergy(TMpcExShower *shower_v1, float MpcEx_E, float MPC_E, 
						    float *MPCXE, float *ALPLTE, float *MPCE, 
						    bool setShower, int *CalOKFlag, bool pairFlag, 
						    int nShared, bool combFlag){
  // The "pairFlag" used to activate a special 
  // low-energy MPC calibration, but is 
  // no deprecated and unused. 
  // 12/12/2017 JGL

  // Flags for linearity corrections

  bool totLinOK = false; 
  
  int iarm = shower_v1->get_arm();  
  int lfl_bin = shower_v1->get_first_layer();
  if(lfl_bin>4) lfl_bin = 4; 

  double CAngle = 1.0/sqrt(pow(shower_v1->get_hsx(),2) + pow(shower_v1->get_hsy(),2) + 1);

  //float ELOW_ENERGY_CORR =  ex_calSpline[iarm][lfl_bin]->Eval(MpcEx_E*CAngle)/CAngle; 
  
  // JGL 8/14/2021 - replaced with simple polynomial fit
  // This has a more controlled extrapolation
  float ELOW_ENERGY_CORR = 0.0;
  float x = MpcEx_E*CAngle; 
  if(iarm==0)
    ELOW_ENERGY_CORR = (0.0685426 + 6.68586*x + 7.99293*x*x)/CAngle; 
  else
    ELOW_ENERGY_CORR = (0.074324 + 6.3835*x + 9.15331*x*x)/CAngle; 

  if(setShower) shower_v1->set_esum(ELOW_ENERGY_CORR);
  if(MPCXE) *MPCXE = ELOW_ENERGY_CORR; 

  int spline_bin = 1; 
  
  if(combFlag){
    // for combined showers use full 3x3 bin
    spline_bin = 1; 
  }
  else{

    if(isInnerTower(shower_v1->get_mpcPeakix(),shower_v1->get_mpcPeakiy(),shower_v1->get_arm())) 
      spline_bin = 2;
    else if(isOuterTower(shower_v1->get_mpcPeakix(),shower_v1->get_mpcPeakiy(),shower_v1->get_arm()))
      spline_bin = 3; 
    else if(shower_v1->get_mpcN3x3() == 8)
      spline_bin = 0; 
    else if(shower_v1->get_mpcN3x3() == 9)
      spline_bin = 1; 
    else
      spline_bin = 1; 
      
  }

  float E33_CORR = 0.0; 
  if(isnan(MPC_E)){
    cout << PHWHERE << " MPC energy is NAN = " << MPC_E << endl; 
  }
  else if(MPC_E>0.0){
    if((spline_bin>=0)&&(spline_bin<4)){
      E33_CORR = mpc_calSpline[iarm][spline_bin]->Eval(MPC_E);          
    }
  }
 
  if(setShower) shower_v1->set_mpcECorr(E33_CORR);    
  if(MPCE) *MPCE = E33_CORR; 

  // Al plate energy

  // factor of 5.0 makes correlation closer to 1:1
  float AL_PLATE_ENERGY =  al_calSpline[iarm]->Eval(MpcEx_E*5.0*CAngle)/CAngle;

  if(setShower) shower_v1->set_AlPlateEnergy(AL_PLATE_ENERGY); 
  if(ALPLTE) *ALPLTE = AL_PLATE_ENERGY; 

  // OLD spline-based calibration
  float TOT_E_CORR = 0.0; 

  // NEW MLP- or BDT-based regression energy calibration
    
  int lidx = 0;
  if(!combFlag){
    if(isInnerTower(shower_v1->get_mpcPeakix(),shower_v1->get_mpcPeakiy(),shower_v1->get_arm())) lidx = 1; 
    if(isOuterTower(shower_v1->get_mpcPeakix(),shower_v1->get_mpcPeakiy(),shower_v1->get_arm())) lidx = 2; 
  }

  ml_resum = MpcEx_E; 
  ml_Cangle = CAngle; 
  ml_E33 = MPC_E;
  ml_in33 = shower_v1->get_mpcN3x3();
  if(combFlag) ml_in33 = 9; 
  ml_insat = shower_v1->get_n_sat_minipads();
  ml_ifl = shower_v1->get_first_layer(); 
  if(ml_ifl>=4) ml_ifl = 4; 

  ml_vtx = shower_v1->get_vertex(); 
  ml_ffx = shower_v1->get_hsx()*200.0; 
  ml_ffy = shower_v1->get_hsy()*200.0; 

  // Tests

  if(isnan(ml_resum)) std::cout << "ml_resum is nan!" << std::endl; 
  if(isnan(ml_Cangle)) std::cout << "ml_Cangle is nan!" << std::endl; 
  if(isnan(ml_E33)) std::cout << "ml_E33 is nan!" << std::endl; 
  if(isnan(ml_in33)) std::cout << "ml_in33 is nan!" << std::endl; 
  if(isnan(ml_insat)) std::cout << "ml_insat is nan!" << std::endl; 
  if(isnan(ml_ifl)) std::cout << "ml_ifl is nan!" << std::endl; 
  if(isnan(ml_vtx)) std::cout << "ml_vtx is nan!" << std::endl; 
  if(isnan(ml_ffx)) std::cout << "ml_ffx is nan!" << std::endl; 
  if(isnan(ml_ffy)) std::cout << "ml_ffy is nan!" << std::endl; 

  //if(MPC_E>0.0){
   TOT_E_CORR = (reader[shower_v1->get_arm()][lidx]->EvaluateRegression( method[shower_v1->get_arm()][lidx] ))[0];
   //}
   //else{
   //TOT_E_CORR = (reader_E33_0[shower_v1->get_arm()]->EvaluateRegression( method_E33_0[shower_v1->get_arm()] ))[0];
   //}

  totLinOK = true; 
  if(setShower) shower_v1->set_roughTotE(TOT_E_CORR);

  // Set calibration validity flag

  if( ((spline_bin>=0) && (spline_bin<=3)) && totLinOK &&  
      (shower_v1->get_first_layer()<=4) && (TOT_E_CORR>0.0) ){
    
    // E33=0 showers only good when in interior of MPC-EX
    
    //if( (MPC_E>0.0) || ((MPC_E==0.0)&&((spline_bin==0)||(spline_bin==1))) ){
    //  if(setShower) shower_v1->set_CalibEInRange(1);
    //  if(CalOKFlag) *CalOKFlag = 1; 
    //}
    //else{
    //  if(setShower) shower_v1->set_CalibEInRange(0);
    //  if(CalOKFlag) *CalOKFlag = 0; 
    //}

    if(setShower) shower_v1->set_CalibEInRange(1);
    if(CalOKFlag) *CalOKFlag = 1; 

  }
  else {
    if(setShower) shower_v1->set_CalibEInRange(0);
    if(CalOKFlag) *CalOKFlag = 0;  
  }

  return TOT_E_CORR; 

}

void MpcExCalibrateShowerEnergy::SetSplines()
{

  int n_ex_sppoints[2][5]; 
  double mpcex_spdata[2][5][2][400]; 

  int n_mpc_sppoints[2][4]; 
  double mpc_spdata[2][4][2][200]; 

  int n_al_sppoints[2]; 
  double al_spdata[2][2][205]; 

#include "ShowerSplineData.C"

  // Create the first order splines

  for(int iarm=0; iarm<2; iarm++){
    for(int ilyr=0; ilyr<5; ilyr++){

      double test_data_x[400]; 
      double test_data_y[400]; 

      for(int i=0; i<n_ex_sppoints[iarm][ilyr]; i++){
        test_data_x[i] = mpcex_spdata[iarm][ilyr][0][i]; 
        test_data_y[i] = mpcex_spdata[iarm][ilyr][1][i]; 
      }

      ex_calSpline[iarm][ilyr] = new MpcExTSpline1(new TGraph(n_ex_sppoints[iarm][ilyr],test_data_x,test_data_y)); 

    }
  }

  for(int iarm=0; iarm<2; iarm++){
    for(int ilyr=0; ilyr<4; ilyr++){

      double test_data_x[200]; 
      double test_data_y[200]; 

      for(int i=0; i<n_mpc_sppoints[iarm][ilyr]; i++){
        test_data_x[i] = mpc_spdata[iarm][ilyr][0][i]; 
        test_data_y[i] = mpc_spdata[iarm][ilyr][1][i]; 
      }

      mpc_calSpline[iarm][ilyr] = new MpcExTSpline1(new TGraph(n_mpc_sppoints[iarm][ilyr],test_data_x,test_data_y));

    }
  }

  for(int iarm=0; iarm<2; iarm++){

      double test_data_x[205]; 
      double test_data_y[205]; 

      for(int i=0; i<n_al_sppoints[iarm]; i++){
        test_data_x[i] = al_spdata[iarm][0][i]; 
        test_data_y[i] = al_spdata[iarm][1][i]; 
      }

      al_calSpline[iarm] = new MpcExTSpline1(new TGraph(n_al_sppoints[iarm],test_data_x,test_data_y));

  }

  // Final Linearity Correction

  int n_comb_sppoints[2][4]; 
  double comb_spdata[2][4][2][400]; 

#include "LinearityCorrectionSplineData.C"

  for(int iarm=0; iarm<2; iarm++){
    for(int ilyr=0; ilyr<4; ilyr++){

      double test_data_x[400]; 
      double test_data_y[400]; 

      for(int i=0; i<n_comb_sppoints[iarm][ilyr]; i++){
        test_data_x[i] = comb_spdata[iarm][ilyr][0][i]; 
        test_data_y[i] = comb_spdata[iarm][ilyr][1][i]; 
      }

      comb_calSpline[iarm][ilyr] = new MpcExTSpline1(new TGraph(n_comb_sppoints[iarm][ilyr],test_data_x,test_data_y));

    }
  }

}

void MpcExCalibrateShowerEnergy::SetupRegression()
{

  // Allocate the readers
  
  for(int i=0; i<2; i++){
    for(int j=0; j<3; j++){

      //cout << " create reader " << i << j << endl; 

      reader[i][j] = new TMVA::Reader( "!Color:!Silent" );   
 
      // Add variables
      
      reader[i][j]->AddVariable("resum",&ml_resum); 
      reader[i][j]->AddVariable("Cangle",&ml_Cangle); 
      reader[i][j]->AddVariable("insat",&ml_insat); 
      reader[i][j]->AddVariable("E33",&ml_E33); 
      reader[i][j]->AddVariable("in33",&ml_in33); 
      reader[i][j]->AddVariable("ifl",&ml_ifl); 
      reader[i][j]->AddVariable("vtx",&ml_vtx); 
      reader[i][j]->AddVariable("ffx",&ml_ffx); 
      reader[i][j]->AddVariable("ffy",&ml_ffy); 

      unsigned char *xmlstr = NULL; 

      switch(methodType){
      case(TMVA::Types::kBDT): 
	switch(i){
	case 0:
	  switch(j){
	  case 0:
	    xmlstr = TMVARegression_BDTG_weights_arm0_xml; 
	    break; 
	  case 1:
	    xmlstr = TMVARegression_BDTG_weights_arm0_inner_xml; 	  
	    break;
	  case 2:
	    xmlstr = TMVARegression_BDTG_weights_arm0_outer_xml; 
	    break; 
	  }
	  break; 
	case 1:
	  switch(j){
	  case 0:
	    xmlstr = TMVARegression_BDTG_weights_arm1_xml; 
	    break; 
	  case 1:
	    xmlstr = TMVARegression_BDTG_weights_arm1_inner_xml; 	  
	    break;
	  case 2:
	    xmlstr = TMVARegression_BDTG_weights_arm1_outer_xml; 
	    break; 
	  }
	  break; 
	}
	break; 
      case(TMVA::Types::kMLP):
	switch(i){
	case 0:
	  switch(j){
	  case 0:
	    xmlstr = TMVARegression_MLP_weights_arm0_xml; 
	    break; 
	  case 1:
	    xmlstr = TMVARegression_MLP_weights_arm0_inner_xml; 	  
	    break;
	  case 2:
	    xmlstr = TMVARegression_MLP_weights_arm0_outer_xml; 
	    break; 
	  }
	  break; 
	case 1:
	  switch(j){
	  case 0:
	    xmlstr = TMVARegression_MLP_weights_arm1_xml; 
	    break; 
	  case 1:
	    xmlstr = TMVARegression_MLP_weights_arm1_inner_xml; 	  
	    break;
	  case 2:
	    xmlstr = TMVARegression_MLP_weights_arm1_outer_xml; 
	    break; 
	  }
	  break; 
	}
	break; 

      default:
	// should never get here
	break; 
      }

      // Read training data

      method[i][j] = dynamic_cast<TMVA::MethodBase*>(reader[i][j]->BookMVA( methodType, (char *)xmlstr )); 

    }
  }

  for(int i=0; i<2; i++){

    reader_E33_0[i] = new TMVA::Reader( "!Color:!Silent" );   
 
    // Add variables
      
    reader_E33_0[i]->AddVariable("resum",&ml_resum); 
    reader_E33_0[i]->AddVariable("Cangle",&ml_Cangle); 
    reader_E33_0[i]->AddVariable("insat",&ml_insat); 
    reader_E33_0[i]->AddVariable("ifl",&ml_ifl); 
    reader_E33_0[i]->AddVariable("vtx",&ml_vtx); 
    reader_E33_0[i]->AddVariable("ffx",&ml_ffx); 
    reader_E33_0[i]->AddVariable("ffy",&ml_ffy); 

    // Read training data

    unsigned char *xmlstr = NULL; 

    switch(methodType){
    case(TMVA::Types::kBDT): 
      switch(i){
      case 0:
	xmlstr = TMVARegression_BDTG_weights_arm0_E33_0_xml; 
	break; 
      case 1:
	xmlstr = TMVARegression_BDTG_weights_arm1_E33_0_xml; 
	break; 
      }
      break; 
    case(TMVA::Types::kMLP):
      switch(i){
      case 0:
	xmlstr = TMVARegression_MLP_weights_arm0_E33_0_xml; 
	break; 
      case 1:
	xmlstr = TMVARegression_MLP_weights_arm1_E33_0_xml; 
	break; 
      }
      break;
    default:
      // should never get here
      break; 
    }

    method_E33_0[i] = dynamic_cast<TMVA::MethodBase*>(reader_E33_0[i]->BookMVA( methodType, (char *)xmlstr )); 

  }

}


int MpcExCalibrateShowerEnergy::isInnerTower(int x, int y, int arm){

  int retVal = 0;

  if(arm == 0){
    
    if( x==4 && ( (y==6) || (y==7) || (y==8) || (y==9) || (y==10) || (y==11)) ){//left edge
      retVal = 1;
    }
    else if( (x==5) && ( (y==5) || (y==12) ) ){//left corners(top and bottom)
      retVal = 1;
    }
    else if( y==13  && ( (x==6) || (x==7) || (x==8) || (x==9) || (x==10) || (x==11) ) ){//top row
      retVal = 1;
    }
    else if( y==4  && ( (x==6) || (x==7) || (x==8) || (x==9) || (x==10) || (x==11) ) ){//bottom row
      retVal = 1;
    }
    else if( x==12 && ( (y==5) || (y==12) ) ){//right corners (top and bottom)
      retVal = 1;
    }
    else if( x==13  && ( (y==6) || (y==7) || (y==8) || (y==9) || (y==10) || (y==11) ) ){//right edge
      retVal = 1;
    }
    else{//not inner tower
      retVal = 0;
    }

  }//end arm 0

  if(arm == 1){

    if( x==5 && ( (y==7) || (y==8) || (y==9) || (y==10) ) ){//left edge
      retVal = 1;
    }
    else if( x==6 && ( (y==6) || (y==11) ) ){//left corners(top and bottom)
      retVal = 1;
    }
    else if( y==12  && ( (x==7) || (x==8) || (x==9) || (x==10) ) ){//top row
      retVal = 1;
    }
    else if( y==5  && ( (x==7) || (x==8) || (x==9) || (x==10) ) ){//bottom row
      retVal = 1;
    }
    else if( x==11 && ( (y==6) || (y==11) ) ){//right corners (top and bottom)
      retVal = 1;
    }
    else if( x==12 && ( (y==7) || (y==8) || (y==9) || (y==10) ) ){//right edge
      retVal = 1;
    }
    else{
      retVal = 0;
    }
  }//end arm 1

  return retVal;

}//end isInnerTower()


int MpcExCalibrateShowerEnergy::isOuterTower(int x, int y, int arm){

  int retVal = 0;

  if(arm == 0){

    if( ( (x==0)||(x==17)) && ( (y==6) || (y==7) || (y==8) || (y==9) || (y==10) || (y==11) ) ){
      retVal = 1;
    }
    else if( ((x==1)||(x==16)) && ( (y==4) || (y==5) || (y==12) || (y==13) ) ){
      retVal = 1;
    }
    else if( (((x==2)||(x==15)) && ((y==3)||(y==14))) ){
      retVal = 1;
    }
    else if( (((x==3)||(x==14)) && ((y==2)||(y==15))) ){
      retVal = 1;
    }
    else if( ((y==1) || (y==16)) && ((x==4) || (x==5) || (x==12) || (x==13)) ){
      retVal = 1;
    }
    else if( ((y==0) || (y==17)) && ((x==6) || (x==7) || (x==8) || (x==9) || (x==10) || (x==11)) ){
      retVal = 1;
    }
    else{
      retVal = 0;
    }
  }//end arm 0

  if(arm == 1){

    if( ( (x==0)||(x==17)) && ( (y==7) || (y==8) || (y==9) || (y==10) ) ){
      retVal = 1;
    }
    else if( ((x==1)||(x==16)) && ( (y==4) || (y==5) || (y==6) || (y==11) || (y==12) || (y==13) ) ){
      retVal = 1;
    }
    else if( (((x==2)||(x==15)) && ((y==3)||(y==14))) ){
      retVal = 1;
    }
    else if( (((x==3)||(x==14)) && ((y==2)||(y==15))) ){
      retVal = 1;
    }
    else if( ((y==1) || (y==16)) && ((x==4) || (x==5) || (x==12) || (x==13)) ){
      retVal = 1;
    }
    else if( ((y==0) || (y==17)) && ((x==6) || (x==7) || (x==8) || (x==9) || (x==10) || (x==11)) ){
      retVal = 1;
    }
    else{
      retVal = 0;
    }


  }//end arm 1

  return retVal;

}//end isOuterTower
