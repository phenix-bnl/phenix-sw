
#include <fstream>
#include <cmath>
#include <gsl/gsl_randist.h>

#include "Acc.h"
#include "AccSim.h"

using std::cout;
using std::endl;
using std::ifstream;

//--------------------------------------------------
AccSim::AccSim()
{
 
  // initialize gsl random generator
  cout << "AccSim::AccSim - initializing random number generator" << endl;
  gsl_rng_env_setup();
  const gsl_rng_type *T = gsl_rng_default;
  _rng = gsl_rng_alloc(T);
  
  // initialize parameters
  // we call it once, to make sure all variables 
  // are initialized prior to the first call
  InitParameters();
  
}

//--------------------------------------------------
AccSim::~AccSim()
{
  // free gsl random generator
  gsl_rng_free( _rng ); 
}

//--------------------------------------------------
void AccSim::CalcCherenkov(int icounter, float gen[15])
{
  // Get Parameters for calcilation
  GetParameters(icounter,gen);

  // Calculate the number of cherenkov photon
  Nphoton = NumberOfPhoton(mom_sim,energy_sim,step_sim,charge_sim);

  // Convert GEANT frame to Optical Simulation frame
  InitPosition(icounter,pos_sim,pos_pre);

  // Nphoton Loop
  for(int nLoop=0; nLoop<Nphoton; nLoop++)
  {
    
    timing = 0.0;
    // Determine the property of photon
    PropertyOfPhoton();
    
    // Calculate the initial property of cherenkvo photon
    EmitePhoton(gen,beam_dx,beam_dy,beam_dz);
    
    // whether photon is in aerogel or not
    bool flag_aer = true;  
    
    while(true)
    { 
      // start propagation loop
      //--- judge of absorption & scattering ---
      if(flag_aer)
      {
        // absorption or not
        bool flag_abs = JudgeAbs(pro_abs);
        
        // photon is absorbed, go to next photon
        if(!flag_abs) break; 
        
        // scattering or not
        bool flag_sct = JudgeSct(pro_sct);

        //Photon is scattered
        if(!flag_sct) Scattering(); 
      }
      
      //--- transition of cherenkov photon & calculate path length---
      Transition(flag_aer);

      //-------- Reflection at wall (x = -5.7850 cm)---------
      if(pos_x <= -5.7850)
      {
        bool flag_ref = JudgeRef(pro_ref);
        if(flag_ref)
        {
          //--- Reflect at inner surface ---
          ReflectX(pos_x,pos_y,pos_z,dx,dy,dz,-1);
        } else {
          // photon is absorbed at wall
          break; 
        }
        
        //-------- Reflection at wall (x = 5.7850 cm)---------
      } else if(pos_x >= 5.7850) {
        
        bool flag_ref = JudgeRef(pro_ref);
        if(flag_ref)
        {
          ReflectX(pos_x,pos_y,pos_z,dx,dy,dz,1); //Reflect at inner surface
        }else{
          break; // photon is absorbed at wall
        }
 
        //-------- Reflection at wall (y = -11.410 cm)---------
      } else if(pos_y <= -11.410) {
        bool flag_pmt = JudgePmt(pos_x,pos_y,pos_z,dx,dy,dz,-1);
        if(!flag_pmt){
          bool flag_ref = JudgeRef(pro_ref);
          if(flag_ref){
            ReflectY(pos_x,pos_y,pos_z,dx,dy,dz,-1); // Reflect at inner surface
          }else{
            break; // photon is absorbed at wall
          }
        }else if(flag_pmt){
          bool flag_trans = JudgeTrans(dx,dz,AER_INDEX,GLS_INDEX); 
          if(flag_trans){ 
            bool flag_pe = JudgePe(waveleng);
            if(flag_pe){
              // photo-electoron is obseved
              Npe[0]++; 
              float arrival_time = TimeOfArrival(timing); // Calc. propagation time
              float flight_time0 = arrival_time + emite_time; // Calc. Time-of-Flight
              TimeOfSignal(flight_time0,cntid,-1); //Calc. plus shape
              break; // go to next phtoon
            }else if(!flag_pe){
              break; // photo-electron is not observed
            }
          
          }else if(!flag_trans){
            ReflectGlass(pos_x,pos_y,pos_z,dx,dy,dz,-1);
          }
        }
        //-------- Reflection at wall (y = 11.410 cm)---------
      } else if(pos_y >= 11.410){
        bool flag_pmt = JudgePmt(pos_x,pos_y,pos_z,dx,dy,dz,1);
        if(!flag_pmt){
          bool flag_ref = JudgeRef(pro_ref);
          if(flag_ref){
            ReflectY(pos_x,pos_y,pos_z,dx,dy,dz,1); // Reflect at inner surface
          }else{
            break; // photon is absorbed at wall
          }
        }else if(flag_pmt){
          bool flag_trans = JudgeTrans(dx,dz,AER_INDEX,GLS_INDEX); 
          if(flag_trans){ 
            bool flag_pe = JudgePe(waveleng);
            if(flag_pe){
              Npe[1]++; // photo-electoron is obseved
              float arrival_time = TimeOfArrival(timing); //Calc. propagation time
              float flight_time1 = arrival_time + emite_time; // Calc. Time-of-Flight
              TimeOfSignal(flight_time1,cntid,1); // Calc. Plus shape
              break; // go to next photon
            }else if(!flag_pe){
              break; // photo-electron is not observed
            }
          }else if(!flag_trans){
            ReflectGlass(pos_x,pos_y,pos_z,dx,dy,dz,1);
          }
        }
        
        //-------- Reflection at wall (z = 0.0 cm)---------
      }else if(pos_z <= 0.0){
        bool flag_ref = JudgeRef(pro_ref);
        if(flag_ref){
          ReflectZ(pos_x,pos_y,pos_z,dx,dy,dz,-1); // Reflect at inner surface
        }else{
          break; // photon is absorbed at wall
        }
      }else if(pos_z >= 12.160){
        flag_aer = false;
        //-------- Reflection at wall (z = 20.720 cm)---------
        if(pos_z >= 20.720){
          bool flag_ref = JudgeRef(pro_ref);
          if(flag_ref){
            ReflectZ(pos_x,pos_y,pos_z,dx,dy,dz,1); // Reflect at inner surface
          }else{
            break;
          }
        }
      }else if(pos_z < 12.160){
        flag_aer = true;
      } // end of reflection
    } // end of propagation loop (go to next photon)
    
    
    
  } //end of Nphoton Loop
  
  //cout<<"Counter ="<<icounter<<endl;
  //cout<<"Number Of PhotoElectron= "<<Npe[0]<<":"<<Npe[1]<<endl;
  
  
  int pmtid = ConvertPmtId(cntid);
  if(pmtid == 1){
    ADC_sim[cntid][0] += (int)(ADC_gain[cntid][0]*Npe[0]);
    ADC_sim[cntid][1] += (int)(ADC_gain[cntid][1]*Npe[1]);
  }else if(pmtid == -1){
    ADC_sim[cntid][0] += (int)(ADC_gain[cntid][0]*Npe[1]);
    ADC_sim[cntid][1] += (int)(ADC_gain[cntid][1]*Npe[0]);
  }
  
  // cout << "AccSim::CalcCherenkov - (" << ADC_sim[cntid][0] << "," << ADC_sim[cntid][1] << ")" << endl;
  
  /*
  for(int ipmt=0; ipmt<2; ipmt++){
    ADC_sim[cntid][ipmt] += (int)ADC_gain[cntid][ipmt]*Npe[ipmt];
  }
  */

  // Save parameters of this step (position & momentum)
  SaveParameters(gen);
  Npe[0] = 0;
  Npe[1] = 0;
}

//--------------------------------------------------
float AccSim::CalibADC(int counter, int pmt, int adc)
{

  return 0;
}

//--------------------------------------------------
void AccSim::SetCalibPara()
{
  for (int i=0; i<TOT_NCNT; i++) {
    for (int j=0; j<2; j++) {

      ADC_pre_sim[i][j] = 100;
      ADC_gain[i][j]    = 30.0;
      TDC_ped_sim[i][j] = 100;
      TDC_gain[i][j]    = 10.0;

    }
  }

}


//--------------------------------------------------
void AccSim::SetCalibParaFile(const char* calibFile)
{
  cout<<"Call AccSim::SetCalibParaFile()"<<endl<<endl;
  
  int   f1,f2,f3,f5;
  float f4,f6;
  ifstream calFile;
  cout << "info. AccSim::SetCalibParaFile, opening calibFile : "<<calibFile<<endl;
  calFile.open(calibFile);
  for (int i=0; i<TOT_NCNT; i++) {
    for (int j=0; j<2; j++) {
      calFile >> f1 >> f2 >> f3 >> f4 >> f5 >> f6;
      cout << "Box # = " << i+1 << " : " << "PMT # = " << j << endl;
      cout << "ADC Pre = " << f3 << " : " 
	   << "ADC Gain = " << f4 << " : " 
	   << "TDC Pedestal = " << f5 << " : " 
	   << "TDC Gain = " << f6 << endl;
      ADC_pre_sim[i][j] = f3;
      ADC_gain[i][j]    = f4;
      TDC_ped_sim[i][j] = f5;
      TDC_gain[i][j]    = f6;
    }
  }
  calFile.close();
}

//--------------------------------------------------
int AccSim::ConvertPmtId(int counter)
{
  int para1 = (int)counter/10;
  int flag  = para1%2;
  int pmtid = 0;

  if(flag == 0){
    pmtid = 1;
  }else if(flag == 1){
    pmtid = -1;
  }
  return pmtid;
}

//--------------------------------------------------
int AccSim::ConvertBoxId(int counter)
{
  int para1 = (int)counter/10;
  int flag  = para1%2;
  int conv  = 0;

  if(flag == 0){
    conv = counter + 10;
  }else if(flag == 1){
    conv = counter - 10;
  }
  return conv;
}

//--------------------------------------------------
float AccSim::GetSimSignal(int counter, int time_para, int flag)
{
  // flag= 0 -> at north side PMT 
  // flag= 1 -> at south side PMT

  float signal = 0.0;
  int flagpmt = ConvertPmtId(counter);
  if(flag == 0){
    if(flagpmt == 1){
      signal = signal_neg[counter][time_para];
    }else if(flagpmt == -1){
      signal = signal_pos[counter][time_para];
    }
  }else if(flag == 1){
    if(flagpmt == 1){
      signal = signal_pos[counter][time_para];
    }else if(flagpmt == -1){
      signal = signal_neg[counter][time_para];
    }
  }
  return signal;
}

//--------------------------------------------------
int AccSim::GetSimADC(int counter, int flag)
{
  // flag= 0 -> at north side PMT 
  // flag= 1 -> at south side PMT

  int signal = 0;
  if(flag == 0){
    if(GetSimADCPost(counter,0) < 1023){
      signal = ADC_sim[counter][0];
    }else{
      signal = 1023 - ADC_pre_sim[counter][0];
    }
  }else if(flag ==  1){
    if(GetSimADCPost(counter,1) < 1023){
      signal = ADC_sim[counter][1];
    }else{
      signal = 1023 - ADC_pre_sim[counter][0];    
    }
  }
  return signal;
}

//--------------------------------------------------
int AccSim::GetSimADCPost(int counter, int flag)
{
  // flag= 0 -> at north side PMT 
  // flag= 1 -> at south side PMT

  int signal = 0;
  if(flag == 0){
    signal = ADC_pre_sim[counter][0] + ADC_sim[counter][0];
    // OverFlow
    if(signal > 1023){
      signal = 1023;
    }
  }else if(flag == 1){
    signal = ADC_pre_sim[counter][1] + ADC_sim[counter][1];
    // OverFlow
    if(signal > 1023){
      signal = 1023;
    }
  }
  return signal;
}

//--------------------------------------------------
int AccSim::GetSimADCPre(int counter, int flag)
{
  // flag= 0 -> at north side PMT 
  // flag= 1 -> at south side PMT

  int signal = 0;
  if(flag == 0){
    signal = ADC_pre_sim[counter][0];
  }else if(flag == 1){
    signal = ADC_pre_sim[counter][1];
  }
  return signal;
}

//--------------------------------------------------
float AccSim::GetSimTDCGain(int counter, int flag)
{
  // flag= 0 -> at north side PMT 
  // flag= 1 -> at south side PMT

  float signal = 0.0;
  if(flag == 0){
    signal = TDC_gain[counter][0];
  }else if(flag == 1){
    signal = TDC_gain[counter][1];
  }
  return signal;
}

//--------------------------------------------------
float AccSim::GetSimADCGain(int counter, int flag)
{
  // flag= 0 -> at north side PMT 
  // flag= 1 -> at south side PMT

  float signal = 0.0;
  if(flag == 0){
    signal = ADC_gain[counter][0];
  }else if(flag == 1){
    signal = ADC_gain[counter][1];
  }
  return signal;
}

//--------------------------------------------------
int AccSim::GetSimTDCPed(int counter, int flag)
{
  // flag= 0 -> at north side PMT 
  // flag= 1 -> at south side PMT

  int signal = 0;
  if(flag == 0){
    signal = TDC_ped_sim[counter][0];
  }else if(flag == 1){
    signal = TDC_ped_sim[counter][1];
  }
  return signal;
}

//--------------------------------------------------
void AccSim::TimeOfSignal(float time_para, int counter, int flag)
{
  // flag= 1 -> reflect at positive side 
  // flag=-1 -> reflect at negative side
  
  float gain_tmp   = 1.6e-3;  float resistor   = 50.0;
  float time_const = 6.0;     float V_out = 0.0;
  //
  float term1 = (gain_tmp*resistor)/(time_const*time_const);
  for(int time_bin=0; time_bin<1000; time_bin++){
    float term2 = (time_bin/10.0-time_para);
    float term3 = exp(-1.0*(time_bin/10.0-time_para)/time_const);
    V_out = term1*term2*term3;
    if(V_out >= 0.0001){
      if(flag == 1){
	signal_pos[counter][time_bin] += V_out; // 1ns = 10bin
      }else if(flag == -1){
	signal_neg[counter][time_bin] += V_out; // 1ns = 10bin
      }
    }
  }
}

//--------------------------------------------------
float AccSim::TimeOfArrival(float time_para)
{
  return time_para/LIGHT;
}

//--------------------------------------------------
void AccSim::ReflectGlass(float x_pos, float y_pos, float z_pos, 
		          float dir_x, float dir_y, float dir_z, 
		          int   flag)
{
  // flag= 1 -> reflect at positive side 
  // flag=-1 -> reflect at negative side
  float inter_posx = 0.0;  float inter_posy = 0.0;
  float inter_posz = 0.0;  float passlength = 0.0;

  if(flag == 1){
    inter_posx = (dir_x/dir_y)*(11.41-y_pos)+x_pos;
    inter_posy = 11.41; 
    inter_posz = (dir_z/dir_y)*(11.41-y_pos)+z_pos;
    passlength = sqrt(pow((inter_posx-x_pos),2.0)+pow((inter_posy-y_pos),2.0)
		     +pow((inter_posz-z_pos),2.0));
  }else if(flag == -1){
    inter_posx = (dir_x/dir_y)*(-11.41-y_pos)+x_pos;
    inter_posy = -11.41; 
    inter_posz = (dir_z/dir_y)*(-11.41-y_pos)+z_pos;
    passlength = sqrt(pow((inter_posx-x_pos),2.0)+pow((inter_posy-y_pos),2.0)
		     +pow((inter_posz-z_pos),2.0));
    dx = dir_x;
    dy = -1.0*dir_y;
    dz = dir_z;
  }
  
  pos_x = inter_posx + passlength*dx; 
  pos_y = inter_posy + passlength*dy; 
  pos_z = inter_posz + passlength*dz; 
}

//--------------------------------------------------
void AccSim::ReflectZ(float x_pos, float y_pos, float z_pos, 
		      float dir_x, float dir_y, float dir_z, 
		      int   flag)
{
  // flag= 1 -> reflect at y=+20.720cm 
  // flag=-1 -> reflect at y= 0.0cm
  float inter_posx = 0.0;  float inter_posy = 0.0;
  float inter_posz = 0.0;  float theta_ref  = 0.0;
  float phi_ref    = 0.0;  float passlength = 0.0;
  
  if(flag == 1){
    inter_posx = (dir_x/dir_z)*(20.72-z_pos)+x_pos;
    inter_posy = (dir_y/dir_z)*(20.72-z_pos)+y_pos; 
    inter_posz = 20.720;
    theta_ref  = 0.5*PI+acos((float)gsl_rng_uniform(_rng));
    phi_ref    = 2.0*PI*((float)gsl_rng_uniform(_rng));
    passlength = sqrt(pow((inter_posx-x_pos),2.0)+pow((inter_posy-y_pos),2.0)
		     +pow((inter_posz-z_pos),2.0));
  }else if(flag == -1){
    inter_posx = (dir_x/dir_z)*(-z_pos)+x_pos;
    inter_posy = (dir_y/dir_z)*(-z_pos)+y_pos; 
    inter_posz = 0.0;
    theta_ref  = acos((float)gsl_rng_uniform(_rng));
    phi_ref    = 2.0*PI*((float)gsl_rng_uniform(_rng));
    passlength = sqrt(pow((inter_posx-x_pos),2.0)+pow((inter_posy-y_pos),2.0)
	             +pow((inter_posz-z_pos),2.0));
  }

  dx = sin(theta_ref)*cos(phi_ref);
  dy = sin(theta_ref)*sin(phi_ref);
  dz = cos(theta_ref);

  pos_x = inter_posx + passlength*dx; 
  pos_y = inter_posy + passlength*dy; 
  pos_z = inter_posz + passlength*dz; 
}

//--------------------------------------------------
void AccSim::ReflectY(float x_pos, float y_pos, float z_pos, 
		      float dir_x, float dir_y, float dir_z, 
		      int   flag)
{
  // flag= 1 -> reflect at y=+11.410cm 
  // flag=-1 -> reflect at y=-11.410cm
  float inter_posx = 0.0;  float inter_posy = 0.0;
  float inter_posz = 0.0;  float theta_ref  = 0.0;
  float phi_ref    = 0.0;  float passlength = 0.0;

  if(flag == 1){
    inter_posx = (dir_x/dir_y)*(11.41-y_pos)+x_pos;
    inter_posy = 11.41; 
    inter_posz = (dir_z/dir_y)*(11.41-y_pos)+z_pos;
    theta_ref  = acos(2.0*((float)gsl_rng_uniform(_rng))-1.0);
    phi_ref    = PI+PI*((float)gsl_rng_uniform(_rng));
    passlength = sqrt(pow((inter_posx-x_pos),2.0)+pow((inter_posy-y_pos),2.0)
	             +pow((inter_posz-z_pos),2.0));
  }else if(flag == -1){
    inter_posx = (dir_x/dir_y)*(-11.41-y_pos)+x_pos;
    inter_posy = -11.41; 
    inter_posz = (dir_z/dir_y)*(-11.41-y_pos)+z_pos;
    theta_ref  = acos(2.0*((float)gsl_rng_uniform(_rng))-1.0);
    phi_ref    = PI*((float)gsl_rng_uniform(_rng));
    passlength = sqrt(pow((inter_posx-x_pos),2.0)+pow((inter_posy-y_pos),2.0)
	             +pow((inter_posz-z_pos),2.0));
  }

  dx = sin(theta_ref)*cos(phi_ref);
  dy = sin(theta_ref)*sin(phi_ref);
  dz = cos(theta_ref);

  pos_x = inter_posx + passlength*dx; 
  pos_y = inter_posy + passlength*dy; 
  pos_z = inter_posz + passlength*dz; 
}

//--------------------------------------------------
void AccSim::ReflectX(float x_pos, float y_pos, float z_pos, 
		      float dir_x, float dir_y, float dir_z, 
		      int   flag)
{
  // flag= 1 -> reflect at x=+5.7850cm 
  // flag=-1 -> reflect at x=-5.7850cm
  float inter_posx = 0.0;  float inter_posy = 0.0;
  float inter_posz = 0.0;  float theta_ref  = 0.0;
  float phi_ref    = 0.0;  float passlength = 0.0;

  if(flag == 1){
    inter_posx = 5.785;
    inter_posy = (dir_y/dir_x)*(5.785-x_pos)+y_pos;
    inter_posz = (dir_z/dir_x)*(5.785-x_pos)+z_pos;
    theta_ref  = acos(2.0*((float)gsl_rng_uniform(_rng))-1.0);
    phi_ref    = 0.5*PI+PI*((float)gsl_rng_uniform(_rng));
    passlength = sqrt(pow((inter_posx-x_pos),2.0)+pow((inter_posy-y_pos),2.0)
		     +pow((inter_posz-z_pos),2.0));
  }else if(flag == -1){
    inter_posx = -5.785;
    inter_posy = (dir_y/dir_x)*(-5.785-x_pos)+y_pos;
    inter_posz = (dir_z/dir_x)*(-5.785-x_pos)+z_pos;
    theta_ref  = acos(2.0*((float)gsl_rng_uniform(_rng))-1.0);
    phi_ref    = 1.5*PI+PI*((float)gsl_rng_uniform(_rng));
    passlength = sqrt(pow((inter_posx-x_pos),2.0)+pow((inter_posy-y_pos),2.0)
                     +pow((inter_posz-z_pos),2.0));
  }
  dx = sin(theta_ref)*cos(phi_ref);
  dy = sin(theta_ref)*sin(phi_ref);
  dz = cos(theta_ref);

  pos_x = inter_posx + passlength*dx; 
  pos_y = inter_posy + passlength*dy; 
  pos_z = inter_posz + passlength*dz; 
}

//--------------------------------------------------
void AccSim::Transition(bool flag)
{
  pos_x = pos_x + STEP_PHOTON*dx;
  pos_y = pos_y + STEP_PHOTON*dy;
  pos_z = pos_z + STEP_PHOTON*dz;

  if(flag){
    timing += AER_INDEX * STEP_PHOTON; 
  }else{
    timing += STEP_PHOTON;
  }
}

//--------------------------------------------------
void AccSim::Scattering()
{
  float theta_sct = acos(2.0*((float)gsl_rng_uniform(_rng))-1.0);
  float phi_sct = 2.0*PI*((float)gsl_rng_uniform(_rng));
  dx = sin(theta_sct)*cos(phi_sct);
  dy = sin(theta_sct)*sin(phi_sct);
  dz = cos(theta_sct);
  //cout<<"Scattering="<<dx<<":"<<dy<<":"<<dz<<endl;  
}

//--------------------------------------------------
bool AccSim::JudgePe(float wave)
{
  double quantum_eff  = 0.0;
  double index_number = 0.0;
  if(wave <= 330.0){
    index_number = (wave/120.0)-1.27;
  }else if(wave > 330.0 && wave <= 400.0){
    index_number = 1.48;
  }else if(wave > 400.0 && wave <= 500.0){
    index_number = (-3.0/1000.0)*wave+2.68;
  }else if(wave > 500.0 && wave <= 600.0){
    index_number = (-11.0/1250.0)*wave+5.58;
  }else if(waveleng > 600.0){
    index_number = (-3.0/250.0)*wave+7.50;
  }
  //
  quantum_eff = pow(10.0,index_number);
  //cout<<"QE="<<quantum_eff<<endl;
  float pro = 100.0*((float)gsl_rng_uniform(_rng));
  if(pro <= quantum_eff){
    return true;
  }else{
    return false;
  }
}

//--------------------------------------------------
bool AccSim::JudgeTrans(float x_dir, float z_dir, float AER, float GLS)
{
  float sth1 = sqrt(x_dir*x_dir+z_dir*z_dir);
  float sth2 = (AER*sth1)/GLS;
  float cth1 = sqrt(1.0-sth1*sth1);
  float cth2 = sqrt(1.0-sth2*sth2);
  //
  float ts = ((2.0*AER*cth1)/(AER*cth1+GLS*cth2))*cth1;
  float tp = ((2.0*AER*cth1)/(GLS*cth1+AER*cth2))*sth1;
  float rs = ((AER*cth1-GLS*cth2)/(GLS*cth1+AER*cth2))*cth1;
  float rp = ((GLS*cth1-AER*cth2)/(GLS*cth1+AER*cth2))*sth1;
  //
  float tt = sqrt(ts*ts+tp*tp);
  float rt = sqrt(rs*rs+rp*rp);
  float pro_trans = 100.0*tt/(tt+rt);
  //
  float trans = 100.0*((float)gsl_rng_uniform(_rng));
  if(trans <= pro_trans){
    return true;
  }else{
    return false;
  }
}

//--------------------------------------------------
bool AccSim::JudgePmt(float x_pos, float y_pos, float z_pos, float x_dir, float y_dir, float z_dir, int flag)
{
  // flag=-1 -> negative position
  // flag= 1 -> positive position 
  float inter_x = 0.0;  float inter_z = 0.0;
  if(flag == -1){
    inter_x = (x_dir/y_dir)*(-11.41-y_pos)+x_pos;
    inter_z = (z_dir/y_dir)*(-11.41-y_pos)+z_pos;
  }else if(flag == 1){
    inter_x = (x_dir/y_dir)*(11.41-y_pos)+x_pos;
    inter_z = (z_dir/y_dir)*(11.41-y_pos)+z_pos;
  }
  // judge whether photon is in the region of PMT or not
  if(pow(inter_x,2.0)+pow((inter_z-16.44),2.0) <= pow(PMT_SIZE,2.0)){
    return true;
  }else{
    return false;
  }
}

//--------------------------------------------------
bool AccSim::JudgeRef(float probability)
{
  float ref = 0.0;
  ref = 100.0*((float)gsl_rng_uniform(_rng));
  // cout << "AccSim::JudgeRef - ref:" << ref << " " << (ref <= probability ? "true":"false") << endl;
  if(ref <= probability){
    return true;
  }else{
    return false;
  }
}

//--------------------------------------------------
bool AccSim::JudgeSct(float probability)
{
  float sct = 0.0;
  sct = 100.0*((float)gsl_rng_uniform(_rng));
  // cout << "AccSim::JudgeSct - sct:" << sct << " " << (sct <= probability ? "true":"false") << endl;
  if(sct <= probability){
    return true;
  }else{
    return false;
  }
}

//--------------------------------------------------
bool AccSim::JudgeAbs(float probability)
{
  float abs = 0.0;
  abs = 100.0*((float)gsl_rng_uniform(_rng));
  // cout << "AccSim::JudgeAbs - abs:" << abs << " " << (abs <= probability ? "true":"false") << endl;
  if(abs <= probability){
    return true;
  }else{
    return false;
  }
}

//--------------------------------------------------
void AccSim::InitParameters()
{

  for(int i=0; i<3; i++)
  {
    pos_sim[i] = 0.0;
    pos_pre[i] = 0.0;
  }

  for(int i=0; i<4; i++)
  {
    mom_sim[i] = 0.0; 
    mom_pre[i] = 0.0;
  }

  for(int i=0; i<TOT_NCNT; i++){
    ADC_sim[i][0] = 0;  
    ADC_sim[i][1] = 0;
    ADC_pos_sim[i][0] = 0;  
    ADC_pos_sim[i][1] = 0;
    for(int j=0; j<1000; j++){
      signal_pos[i][j] = 0;
      signal_neg[i][j] = 0;
    }
  }
  
  //
  tof_sim    = 0.0; 
  step_sim   = 0.0; 
  energy_sim = 0.0;
  charge_sim = 0.0; 
  beta       = 0.0; 
  init_posx  = 0.0; 
  init_posy  = 0.0; 
  init_posz  = 0.0; 
  beam_dx    = 0.0;
  beam_dy    = 0.0;
  beam_dz    = 0.0;
  waveleng   = 0.0; 
  pro_abs    = 0.0; 
  pro_sct    = 0.0;
  pro_ref    = 0.0; 
  emite_time = 0.0;
  pos_x      = 0.0; 
  pos_y      = 0.0;
  pos_z      = 0.0;
}


//--------------------------------------------------
void AccSim::GetParameters(int icounter, float para[15])
{
  pos_sim[0] = para[0];    pos_sim[1] = para[1];
  pos_sim[2] = para[2];    mom_sim[0] = para[3];
  mom_sim[1] = para[4];    mom_sim[2] = para[5];
  mom_sim[3] = para[13];   tof_sim    = para[7];
  step_sim   = para[10];   energy_sim = para[11];
  charge_sim = para[12];
  
  if(icounter<1 || icounter>ACC::ACC_NBOX){
    cout<<"Error AerSim::CalcCherenkov, out of range (box number)"<<endl;
  }else{
    cntid = icounter - 1;
  }
}

//--------------------------------------------------
int AccSim::NumberOfPhoton(float mom_s[4], float energy_s, float step_s, float charge_s)
{
  int   Np   = 0;
  beta = mom_s[3]/energy_s;
  
  // Calcilate the mean number of cherenkov photon
  if(beta*AER_INDEX >= 1.0){
    float term1 = ((1.0/300.0)-(1.0/650.0))*(pow(10.0,7.0));
    float term2 = 1.0 - 1.0/((AER_INDEX*beta)*(AER_INDEX*beta));
    Np = (int)(2.0*PI*ALPHA*(charge_s*charge_s)*step_s*term1*term2); 
  }

  // apply the poisson distribution to number of photon
  float flag_psn = 0.0;
  float ran = 0.0;
  int   psn = 0;
  if(Np > 0){
    while(true){
      ran = (float)gsl_rng_uniform(_rng);
      flag_psn += log(ran);
      if(flag_psn >= (float)-1.0*Np){
	psn++;
      }else {
	break;
      }
    }
  }
  return psn;
}

//--------------------------------------------------
void AccSim::InitPosition(int icounter, float para_n[3], float para_o[3])
{
  hori_id = (int)(((icounter-1)/10)+1);
  vert_id = ((icounter-1)%10)+1;
  float tmp1 = para_n[0] - para_o[0];
  float tmp2 = para_n[1] - para_o[1];
  float tmp3 = para_n[2] - para_o[2];
  float tmp4 = sqrt(tmp1*tmp1 + tmp2*tmp2 + tmp3*tmp3);
  if((hori_id%2) == 0){
    init_posx = para_o[1]-9.91740-(11.0-2.0*vert_id)*5.925;
    init_posy = para_o[2]-0.8290-(17.0-2.0*hori_id)*11.55;
    init_posz = para_o[0]-443.360; 
    beam_dx   = tmp2/tmp4;
    beam_dy   = tmp3/tmp4;
    beam_dz   = tmp1/tmp4;
  }else{
    init_posx = -1.0*(para_o[1]-9.91740-(11.0-2.0*vert_id)*5.925);
    init_posy = -1.0*(para_o[2]-0.8290-(17.0-2.0*hori_id)*11.55);
    init_posz = -1.0*(para_o[0]-455.470);
    beam_dx   = -1.0*tmp2/tmp4;
    beam_dy   = -1.0*tmp3/tmp4;
    beam_dz   = -1.0*tmp1/tmp4;
  }
}

//--------------------------------------------------
void AccSim::SaveParameters(float para[15])
{
  pos_pre[0] = para[0];    pos_pre[1] = para[1];
  pos_pre[2] = para[2];    mom_pre[0] = para[3];
  mom_pre[1] = para[4];    mom_pre[2] = para[5];
  mom_pre[3] = para[13];
}

//--------------------------------------------------
void AccSim::PropertyOfPhoton()
{
  waveleng = 1.0/(((float)gsl_rng_uniform(_rng))*(7.0/3900.0)+(1.0/650.0));
  float absleng  = (AER_ABS/pow(415.0,2.0))*(pow(waveleng,2.0));
  float sctleng  = (AER_SCT/pow(415.0,4.0))*(pow(waveleng,4.0));
  pro_abs  = 100.0*exp(-1.0*STEP_PHOTON/absleng); // [%]
  pro_sct  = 100.0*exp(-1.0*STEP_PHOTON/sctleng); // [%]
  //pro_ref  = 100.0*((-0.035/300.0)*waveleng+1.02);
  pro_ref = 99.0;
}

//--------------------------------------------------
void AccSim::EmitePhoton(float para[15],float dir_x,float dir_y,float dir_z)
{
  float position = ((float)gsl_rng_uniform(_rng))*step_sim;
  // initial position emitted photon
  pos_x          = init_posx+position*beam_dx;
  pos_y          = init_posy+position*beam_dy;
  pos_z          = init_posz+position*beam_dz;

  // initial time emitted photon
  emite_time     = (position/(beta*29.979))+para[7];

  // initial direction emitted photon
  // cherenkov emite
  float theta = acos(1.0/(AER_INDEX*beta));
  float phi   = 2.0*PI*((float)gsl_rng_uniform(_rng));
  float cherenkov_dx = sin(theta)*cos(phi);
  float cherenkov_dy = sin(theta)*sin(phi);
  float cherenkov_dz = cos(theta);
  // new frame
  float theta_new = acos(beam_dz);
  float phi_new  = atan2(beam_dy,beam_dx); 
  // rotation axis
  float rot_theta = 0.5*PI;
  float rot_phi   = phi_new + 1.50*PI;
  float rot_dx = sin(rot_theta)*cos(rot_phi);
  float rot_dy = sin(rot_theta)*sin(rot_phi);
  float rot_dz = cos(rot_theta);
  //  rotate
  dx=(rot_dx*rot_dx+(1.0-rot_dx*rot_dx)*cos(theta_new))*cherenkov_dx
    +(rot_dx*rot_dy-rot_dx*rot_dy*cos(theta_new)-rot_dz*sin(theta_new))*cherenkov_dy
    +(rot_dx*rot_dz-rot_dx*rot_dz*cos(theta_new)-rot_dy*sin(theta_new))*cherenkov_dz;

  dy=(rot_dx*rot_dy-rot_dx*rot_dy*cos(theta_new)+rot_dz*sin(theta_new))*cherenkov_dx
    +(rot_dy*rot_dy+(1.0-rot_dy*rot_dy)*cos(theta_new))*cherenkov_dy
    +(rot_dy*rot_dz-rot_dy*rot_dz*cos(theta_new)-rot_dx*sin(theta_new))*cherenkov_dz;

  dz=(rot_dx*rot_dz-rot_dx*rot_dz*cos(theta_new)-rot_dy*sin(theta_new))*cherenkov_dx
    +(rot_dy*rot_dz-rot_dy*rot_dz*cos(theta_new)+rot_dx*sin(theta_new))*cherenkov_dy
    +(rot_dz*rot_dz+(1.0-rot_dz*rot_dz)*cos(theta_new))*cherenkov_dz;
}
