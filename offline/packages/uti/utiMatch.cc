// Jiangyong Jia
//
// this class is a first attempt to parametrize the matching for various subsystems
// the matching should be done for two variables, Phi and Z, for each detector
// they are : PC1e, PC1w, PC2,PC3e,PC3w,TOF,PBGL, PBSCe, PBSCw
// for each varible I need to parametrize the matching as function of momentum(maybe
// also other variables like theta, and bbcvertex). right now there are three kind of
// functions being used: 
// 1) f1  = p[0]
// 2) f2  = p[0]/mom + p[1]; 
// 3) f3  = sqrt(p[0]*p[0]/(mom*mom) + p[1]*p[1])
// here one should inteprete mom as   charge*|mom|
// usually I use f1 as a fitting function for mean of the match.
// except for emc detector where Vald Pantuev suggest I should use
// p[0]*sin(alpha) + p[1], but since alpha = K/mom, and alpha is usually small, it becomes f2
// I always use f3 as function for fit to sig of the match, it has two parts which corresponds
// to multiple scattering and detector resolution
//
//
// 1). use d_xxx_yyy_match(float p, float dv) function to calulated the match in number of sigmas
// xxx can be PC2, PC3E etc, yyy can be either Phi or Z. p should be the signed momentum of the
// track, and dv = hit location  - projection. Z is in cm, phi is in radian
// if you want also get the shift that needed for the correction, you can call function
// d_xxx_yyy_match(float p, float dv,float &shift).
// 2). "Z" match of emc depends on beta angle of the track and the shower type -- ELECTRON like(for electron),
// MIP like(charged Hadron), NUCLEAR interaction like(Charged Hadron). From what I learned from sasha, all clusters
// in emc have been corrected assuming they are photons. the correction I applied here account for the difference
// between charged particle shower and photon. In principle, this difference depend on the shower type. 
// in practice I determine matching parameters using all charged tracks, no distinction between MIP and NUCLEAR(so 
// the parameters are identical. For electron, since the shower shape is very similar to photon, it the correction due
// to shower shape difference should be small, although one should take into account the bending effect.
// I haven't studied this because it requires a lot of statistics.
// 
// The Z match function is  d_PBXX_z_match(float p,float beta, float dv,int type) (XX is GL or SC), where beta is 
// the beta angle of DCH track, type is 1(electron), 2 MIP, 3 NUCLEAR.
// 3). "PHI" match of emc I calculate everything in the same way. since the incident angle is small,
// the differences are small better different showers.
//
// Xinhua Li for TEC in run2
// TEC matching is slightly different from the aboves.
// It is done with the following two quantities:
//       a) phi(TEC hit in r=4.4m reference plane) - projection to there     
//       b) alpha(TEC) - 0.534 * alpha(DCH);
//          where the factor (0.534) is probably due to an effect of 
//          the remaining magnetic field outside of DCH   
//                      
// Xinhua Li updates TEC part for run3 on 06/23/2003
// no change in the ratio of alpha(TEC) to alpha(DCH) 
// but updated matching in alpha and phi 
//
// Xinhua Li updates TEC part for run4 on 03/05/2004
//

#include <cmath>
#include "utiMatch.h"
#include <iostream>
using namespace std;
char utiMatch::subsystems[8][100]={
  "PC2","PC3e","PC3w","TOF","TEC","PBGL","PBSCe","PBSCw"
};
void utiMatch::Load_default(){

  //run3 values
  pc2z_rms_run3[0]     = 0.6174;
  pc2z_rms_run3[1]     = 0.9414;
  pc3ez_rms_run3[0]    = 1;
  pc3ez_rms_run3[1]    = 1.33;
  pc3wz_rms_run3[0]    = 0.999;
  pc3wz_rms_run3[1]    = 1.303;
  tofz_rms_run3[0]     = 0.719;
  tofz_rms_run3[1]     = 1.428;

  pc2phi_rms_run3[0]   = 1.23*0.001;
  pc2phi_rms_run3[1]   = 1.16*0.001;
  pc3ephi_rms_run3[0]  = 1.61*0.001;
  pc3ephi_rms_run3[1]  = 1.35*0.001;
  pc3wphi_rms_run3[0]  = 1.62*0.001;
  pc3wphi_rms_run3[1]  = 1.3*0.001;
  tofphi_rms_run3[0]   = 1.47*0.001;
  tofphi_rms_run3[1]   = 3.58*0.001;


  pc2z_mean_run3[0][0]    = -0.48;  pc2z_mean_run3[0][1]    = 0.31; //north
  pc2z_mean_run3[1][0]    = 0.34;   pc2z_mean_run3[1][1]    = 0.042;//south
  pc3ez_mean_run3[0][0]   = -0.755; pc3ez_mean_run3[0][1]   = 0.5;  //north
  pc3ez_mean_run3[1][0]   = 0.496;  pc3ez_mean_run3[1][1]   = -0.241;//south
  pc3wz_mean_run3[0][0]   = -0.593; pc3wz_mean_run3[0][1]   = 0.391; //north
  pc3wz_mean_run3[1][0]   = 0.5;    pc3wz_mean_run3[1][1]   = -0.018;//south
  tofz_mean_run3[0][0]    = -0.523; tofz_mean_run3[0][1]    = -0.267; //north
  tofz_mean_run3[1][0]    = 0.587;  tofz_mean_run3[1][1]    = -0.126;//south

  pc2phi_mean_run3[0][0]  = 0.069*0.001;      pc2phi_mean_run3[0][1]  = -0.22*0.001;  //north plus
  pc2phi_mean_run3[1][0]  = -0.014*0.001;     pc2phi_mean_run3[1][1]  = -0.579*0.001; //north minus
  pc2phi_mean_run3[2][0]  = -0.059*0.001;     pc2phi_mean_run3[2][1]  = 0.044*0.001;  //south plus
  pc2phi_mean_run3[3][0]  = -0.127*0.001;     pc2phi_mean_run3[3][1]  = -0.279*0.001; //south minus
  pc3ephi_mean_run3[0][0]  = 0.335*0.001;     pc3ephi_mean_run3[0][1]  = 0.165*0.001;  //north plus
  pc3ephi_mean_run3[1][0]  = -0.404*0.001;    pc3ephi_mean_run3[1][1]  = -0.091*0.001; //north minus
  pc3ephi_mean_run3[2][0]  = 0.713*0.001;     pc3ephi_mean_run3[2][1]  = 0.836*0.001;  //south plus
  pc3ephi_mean_run3[3][0]  = -0.879*0.001;    pc3ephi_mean_run3[3][1]  = 0.641*0.001;  //south minus
  pc3wphi_mean_run3[0][0]  = 0.353*0.001;     pc3wphi_mean_run3[0][1]  = -0.174*0.001; //north plus
  pc3wphi_mean_run3[1][0]  = -0.153*0.001;    pc3wphi_mean_run3[1][1]  = -0.503*0.001; //north minus
  pc3wphi_mean_run3[2][0]  = 0.178*0.001;     pc3wphi_mean_run3[2][1]  = -0.16*0.001;  //south plus
  pc3wphi_mean_run3[3][0]  = -0.133*0.001;    pc3wphi_mean_run3[3][1]  = -0.475*0.001; //south minus
  tofphi_mean_run3[0][0]  = 2.53*0.001;       tofphi_mean_run3[0][1]  = -0.4*0.001;  //plus
  tofphi_mean_run3[1][0]  = -2.08*0.001;      tofphi_mean_run3[1][1]  = -0.4*0.001; //minus


  //run2 values
  pc2phi_mean_plus[0]  = 0;//not used
  pc2phi_mean_plus[1]  = 0.1*0.001;
  pc2phi_mean_minus[0]  = 0;//not used
  pc2phi_mean_minus[1]  = (-0.5)*0.001;
  pc2phi_rms[0]   = 1.52*0.001;
  pc2phi_rms[1]   = 1.51*0.001;

  pc2z_mean[0]    = 0;//not used
  pc2z_mean[1]    = 0.028;
  pc2z_rms[0]     = 0.51;
  pc2z_rms[1]     = 0.60;

  pc3ephi_mean_plus[0] = 0;//not used
  pc3ephi_mean_plus[1] = (0.94-0.3)*0.001;
  pc3ephi_mean_minus[0] = 0;//not used
  pc3ephi_mean_minus[1] = (-0.13+0.5)*0.001;//0.5
  pc3ephi_rms[0]  = 2*0.001;
  pc3ephi_rms[1]  = 1.80*0.001;

  pc3ez_mean[0]   = 0;//not used
  pc3ez_mean[1]   = -0.16;
  pc3ez_rms[0]    = 0.81;
  pc3ez_rms[1]    = 0.83;

  pc3wphi_mean_plus[0] = 0*0.001;//not used
  pc3wphi_mean_plus[1] = (0.18)*0.001;//-0.25
  pc3wphi_mean_minus[0] = 0*0.001;//not used
  pc3wphi_mean_minus[1] = (-0.66)*0.001;//0.1
  pc3wphi_rms[0]  = 2.1*0.001;
  pc3wphi_rms[1]  = 1.8*0.001;

  pc3wz_mean[0]   = 0;//not used
  pc3wz_mean[1]   = -0.016;
  pc3wz_rms[0]    = 0.872;
  pc3wz_rms[1]    = 0.81;

  tofphi_mean_plus[0]  = 0.66*0.001;
  tofphi_mean_plus[1]  = 1.57*0.001;
  tofphi_mean_minus[0]  = -0.73*0.001;
  tofphi_mean_minus[1]  = 0.77*0.001;
  tofphi_rms[0]   = 2.19*0.001;
  tofphi_rms[1]   = 3.69*0.001;
  tofz_mean[0]    = 0;//not used
  tofz_mean[1]    = -0.445;
  tofz_rms[0]     = 0.91;
  tofz_rms[1]     = 0.95;
  //run2 and run3 values
  tecphi_mean_plus[0]  = 0.001*0.356067;
  tecphi_mean_plus[1]  = 0.001*(-0.00546649);
  tecphi_mean_minus[0] = 0.001*(-0.458005);
  tecphi_mean_minus[1] = 0.001*0.00371022;
  tecphi_rms[0]   = 0.001*1.14487;
  tecphi_rms[1]   = 0.001*1.00739;

  tecalpha_mean_plus[0]  = 0.001*0.352124;
  tecalpha_mean_plus[1]  = 0.001*(-1.01778);
  tecalpha_mean_minus[0] = 0.001*(-0.374325);
  tecalpha_mean_minus[1] = 0.001*(-0.975867);
  tecalpha_rms[0]   = 0.001*4.6676;
  tecalpha_rms[1]   = 0.001*4.76582;

  pbglphi_mean_plus[0]  = 4.17*0.001;
  pbglphi_mean_plus[1]  = (0.605-0.2)*0.001;
  pbglphi_mean_minus[0] = -3.88*0.001;
  pbglphi_mean_minus[1] = (-0.622+0.2)*0.001;
  pbglphi_rms[0]        = 2.4*0.001;
  pbglphi_rms[1]        = 3.6*0.001;

  pbglz_mean[0]         = 0;//not used
  pbglz_mean[1]         = -0.55;
  pbglz_rms[0]          = 1;
  pbglz_rms[1]          = 2.5;
  //beta<1.57  3.22 1.58, beta>1.57 2.85,1.52
  // here just use mean : 3.03,1.55
  pbglz_shift_electron[0]       = 3.03;
  pbglz_shift_electron[1]       = 1.55;
  pbglz_shift_mip[0]         = 3.03;
  pbglz_shift_mip[1]         = 1.55;
  pbglz_shift_nucl[0]        = 3.03;
  pbglz_shift_nucl[1]        = 1.55;

  pbscephi_mean_plus[0] = (1.81)*0.001;//positive mean
  pbscephi_mean_plus[1] = (2.48-0.3)*0.001;
  pbscephi_mean_minus[0]= (-2.12)*0.001;//negtive mean
  pbscephi_mean_minus[1]= (1.02+0.3)*0.001;
  pbscephi_rms[0]       = 2.6*0.001;
  pbscephi_rms[1]       = 4.6*0.001;
  pbscez_mean[0]        = 0;//not used
  pbscez_mean[1]        = -0.35;
  pbscez_rms[0]         = 1.4;//0.821;
  pbscez_rms[1]         = 2.5;//3.25;
  
  pbscez_shift_electron[0]      = 10.9;
  pbscez_shift_electron[1]      = 2.03;
  pbscez_shift_mip[0]        = 10.9;
  pbscez_shift_mip[1]        = 2.03;
  pbscez_shift_nucl[0]       = 10.9;
  pbscez_shift_nucl[1]       = 2.03;

  pbscwphi_mean_plus[0] = (2.27)*0.001;//positive mean
  pbscwphi_mean_plus[1] = (-0.44)*0.001;
  pbscwphi_mean_minus[0]= (-1.99)*0.001;//negtive mean
  pbscwphi_mean_minus[1]= (-1.57)*0.001;
  pbscwphi_rms[0]       = 2.7*0.001;
  pbscwphi_rms[1]       = 4.7*0.001;

  pbscwz_mean[0]        = 0;//not used
  pbscwz_mean[1]        = 0.081;
  pbscwz_rms[0]         = 1.8;
  pbscwz_rms[1]         = 2.4;
  //beta<1.57  10.6 1.98, beta>1.57 10.2,1.92
  // here just use mean : 10.4,1.95
  pbscwz_shift_electron[0]      = 10.4;
  pbscwz_shift_electron[1]      = 1.95;
  pbscwz_shift_mip[0]        = 10.4;
  pbscwz_shift_mip[1]        = 1.95;
  pbscwz_shift_nucl[0]       = 10.4;
  pbscwz_shift_nucl[1]       = 1.95;
}
utiMatch::utiMatch(){
  Load_default();
}
void utiMatch::set_xxx_z_match(int detector,float m1,float m2,float sig1,float sig2){
  if(detector<0||detector>7||detector==4){
    cout<<" invalid detector number"<<endl;
    return ;
  }    
  cout<<"Modifying "<<subsystems[detector]<<" Z matching parameters"<<endl;
  if(detector==0){
    pc2z_mean[0] = m1;    pc2z_mean[1] = m2;
    pc2z_rms[0] = sig1;   pc2z_rms[1] = sig2;
  }else if(detector==1){
    pc3ez_mean[0] = m1;   pc3ez_mean[1] = m2;
    pc3ez_rms[0] = sig1;  pc3ez_rms[1] = sig2;
  }else if(detector==2){
    pc3wz_mean[0] = m1;   pc3wz_mean[1] = m2;
    pc3wz_rms[0] = sig1;  pc3wz_rms[1] = sig2;
  }else if(detector==3){
    tofz_mean[0] = m1;    tofz_mean[1] = m2;
    tofz_rms[0] = sig1;   tofz_rms[1] = sig2;
  }else if(detector==5){
    pbglz_mean[0] = m1;   pbglz_mean[1] = m2;
    pbglz_rms[0] = sig1;  pbglz_rms[1] = sig2;
  }else if(detector==6){
    pbscez_mean[0] = m1;  pbscez_mean[1] = m2;
    pbscez_rms[0] = sig1; pbscez_rms[1] = sig2;
  }else if(detector==7){
    pbscwz_mean[0] = m1;  pbscwz_mean[1] = m2;
    pbscwz_rms[0] = sig1; pbscwz_rms[1] = sig2;
  }
}
void utiMatch::set_xxx_phi_match(int detector,
				 float mplus1,float mplus2,float mminus1,float mminus2,
				 float sig1,float sig2){
  if(detector<0||detector>7){
    cout<<" invalid detector number"<<endl;
    return;
  }    
  cout<<"Modifying "<<subsystems[detector]<<" Phi matching parameters"<<endl;
  if(detector==0){
    pc2phi_mean_plus[0]  = mplus1;         pc2phi_mean_plus[1] = mplus2;
    pc2phi_mean_minus[0] = mminus1;        pc2phi_mean_minus[1]= mminus2;
    pc2phi_rms[0] = sig1;                  pc2phi_rms[1] =sig2;
  }else if(detector==1){
    pc3ephi_mean_plus[0]  = mplus1;        pc3ephi_mean_plus[1] = mplus2;
    pc3ephi_mean_minus[0] = mminus1;       pc3ephi_mean_minus[1]= mminus2;
    pc3ephi_rms[0] = sig1;                 pc3ephi_rms[1] =sig2;
  }else if(detector==2){
    pc3wphi_mean_plus[0]  = mplus1;        pc3wphi_mean_plus[1] = mplus2;
    pc3wphi_mean_minus[0] = mminus1;       pc3wphi_mean_minus[1]= mminus2;
    pc3wphi_rms[0] = sig1;                 pc3wphi_rms[1] =sig2;
  }else if(detector==3){
    tofphi_mean_plus[0]  = mplus1;         tofphi_mean_plus[1] = mplus2;
    tofphi_mean_minus[0] = mminus1;        tofphi_mean_minus[1]= mminus2;
    tofphi_rms[0] = sig1;                  tofphi_rms[1] =sig2;
  }else if(detector==4){
    tecphi_mean_plus[0]  = mplus1;         tecphi_mean_plus[1] = mplus2;
    tecphi_mean_minus[0] = mminus1;        tecphi_mean_minus[1]= mminus2;
    tecphi_rms[0] = sig1;                  tecphi_rms[1] =sig2;
  }else if(detector==5){
    pbglphi_mean_plus[0]  = mplus1;        pbglphi_mean_plus[1] = mplus2;
    pbglphi_mean_minus[0] = mminus1;       pbglphi_mean_minus[1]= mminus2;
    pbglphi_rms[0] = sig1;                 pbglphi_rms[1] =sig2;
  }else if(detector==6){
    pbscephi_mean_plus[0]  = mplus1;       pbscephi_mean_plus[1] = mplus2;
    pbscephi_mean_minus[0] = mminus1;      pbscephi_mean_minus[1]= mminus2;
    pbscephi_rms[0] = sig1;                pbscephi_rms[1] =sig2;
  }else if(detector==7){
    pbscwphi_mean_plus[0]  = mplus1;       pbscwphi_mean_plus[1] = mplus2;
    pbscwphi_mean_minus[0] = mminus1;      pbscwphi_mean_minus[1]= mminus2;
    pbscwphi_rms[0] = sig1;                pbscwphi_rms[1] =sig2;
  }
}
// TEC alpha matching is done through alpha(TEC) - alpha(DCH)*0.534
void utiMatch::set_tec_alpha_match(float mplus1,float mplus2,
                    float mminus1,float mminus2,float sig1,float sig2){
    tecalpha_mean_plus[0]  = mplus1;         tecalpha_mean_plus[1] = mplus2;
    tecalpha_mean_minus[0] = mminus1;        tecalpha_mean_minus[1]= mminus2;
    tecalpha_rms[0] = sig1;                  tecalpha_rms[1] =sig2;
}

void utiMatch::set_xxx_pbglz_shift (float electron1,float electron2,
				    float mip1,float mip2,float nucl1,float nucl2){
  pbglz_shift_electron[0] = electron1;
  pbglz_shift_electron[1] = electron2;
  pbglz_shift_mip[0]   = mip1;
  pbglz_shift_mip[1]   = mip2;
  pbglz_shift_nucl[0]  = nucl1;
  pbglz_shift_nucl[1]  = nucl2;
}
void utiMatch::set_xxx_pbscez_shift(float electron1,float electron2,
				    float mip1,float mip2,float nucl1,float nucl2){
  pbscez_shift_electron[0] = electron1;
  pbscez_shift_electron[1] = electron2;
  pbscez_shift_mip[0]   = mip1;
  pbscez_shift_mip[1]   = mip2;
  pbscez_shift_nucl[0]  = nucl1;
  pbscez_shift_nucl[1]  = nucl2;
}
void utiMatch::set_xxx_pbscwz_shift(float electron1,float electron2,
				    float mip1,float mip2,float nucl1,float nucl2){
  pbscwz_shift_electron[0] = electron1;
  pbscwz_shift_electron[1] = electron2;
  pbscwz_shift_mip[0]   = mip1;
  pbscwz_shift_mip[1]   = mip2;
  pbscwz_shift_nucl[0]  = nucl1;
  pbscwz_shift_nucl[1]  = nucl2;
}
//run3 functions
float utiMatch::d_PC2_phi_match(float p, float zed, float dv){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pc2phi_rms_run3[0]*pc2phi_rms_run3[0]/(p*p) +  pc2phi_rms_run3[1]*pc2phi_rms_run3[1]);
  float offset;
  int i;
  if(zed>0){//north
    if(p>0){
      i=0;
    }else{
      i=1;
    }
  }else{//south
    if(p>0){
      i=2;
    }else{
      i=3;
    }
  }
  offset = pc2phi_mean_run3[i][0]*exp(-0.7*fabs(p))+pc2phi_mean_run3[i][1];

  tmpshift = offset;
  return (dv-offset)/sigma_width;
}
float utiMatch::dv_PC2_phi_match(float p, float zed,float sig){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pc2phi_rms_run3[0]*pc2phi_rms_run3[0]/(p*p) +  pc2phi_rms_run3[1]*pc2phi_rms_run3[1]);
  float offset;
  int i=0;
  if(zed>0){//north
    if(p>0){
      i=0;
    }else{
      i=1;
    }
  }else{//south
    if(p>0){
      i=2;
    }else{
      i=3;
    }
  }
  offset = pc2phi_mean_run3[i][0]*exp(-0.7*fabs(p))+pc2phi_mean_run3[i][1];
  tmpshift = offset;
  return offset+sig*sigma_width;
}
float utiMatch::d_PC2_z_match(float p, float zed, float dv){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pc2z_rms_run3[0]*pc2z_rms_run3[0]/(p*p) +  pc2z_rms_run3[1]*pc2z_rms_run3[1]);
  float offset;
  int i;
  if(zed>0){ i=0;}
  else{i=1;}
  offset = pc2z_mean_run3[i][0]*exp(-0.7*fabs(p))+pc2z_mean_run3[i][1];

  tmpshift = offset;
  return (dv-offset)/sigma_width;
}
float utiMatch::dv_PC2_z_match(float p, float zed, float sig){
  if(p==0.0) return 1000;

  float sigma_width = sqrt(pc2z_rms_run3[0]*pc2z_rms_run3[0]/(p*p) +  pc2z_rms_run3[1]*pc2z_rms_run3[1]);
  float offset;
  int i=0;
  if(zed>0){ i=0;}
  else{i=1;}
  offset = pc2z_mean_run3[i][0]*exp(-0.7*fabs(p))+pc2z_mean_run3[i][1];

  tmpshift = offset;
  return offset+sig*sigma_width;
}

float utiMatch::d_PC3e_phi_match(float p, float zed, float dv){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pc3ephi_rms_run3[0]*pc3ephi_rms_run3[0]/(p*p) +  pc3ephi_rms_run3[1]*pc3ephi_rms_run3[1]);
  float offset;
  int i=0;
  if(zed>0){//north
    if(p>0){
      i=0;
    }else{
      i=1;
    }
  }else{//south
    if(p>0){
      i=2;
    }else{
      i=3;
    }
  }
  offset = pc3ephi_mean_run3[i][0]*exp(-0.7*fabs(p))+pc3ephi_mean_run3[i][1];

  tmpshift = offset;
  return (dv-offset)/sigma_width;
}
float utiMatch::dv_PC3e_phi_match(float p, float zed,float sig){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pc3ephi_rms_run3[0]*pc3ephi_rms_run3[0]/(p*p) +  pc3ephi_rms_run3[1]*pc3ephi_rms_run3[1]);
  float offset;
  int i=0;
  if(zed>0){//north
    if(p>0){
      i=0;
    }else{
      i=1;
    }
  }else{//south
    if(p>0){
      i=2;
    }else{
      i=3;
    }
  }
  offset = pc2phi_mean_run3[i][0]*exp(-0.7*fabs(p))+pc2phi_mean_run3[i][1];

  tmpshift = offset;
  return offset+sig*sigma_width;
}
float utiMatch::d_PC3e_z_match(float p, float zed,float dv){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pc3ez_rms_run3[0]*pc3ez_rms_run3[0]/(p*p) +  pc3ez_rms_run3[1]*pc3ez_rms_run3[1]);
  float offset;
  int i=0;
  if(zed>0){ i=0;}
  else{i=1;}
  offset = pc3ez_mean_run3[i][0]*exp(-0.7*fabs(p))+pc3ez_mean_run3[i][1];

  tmpshift = offset;
  return (dv-offset)/sigma_width;
}
float utiMatch::dv_PC3e_z_match(float p, float zed,float sig){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pc3ez_rms_run3[0]*pc3ez_rms_run3[0]/(p*p) +  pc3ez_rms_run3[1]*pc3ez_rms_run3[1]);
  float offset;
  int i=0;
  if(zed>0){ i=0;}
  else{i=1;}
  offset = pc2z_mean_run3[i][0]*exp(-0.7*fabs(p))+pc2z_mean_run3[i][1];
  tmpshift = offset;
  return offset+sig*sigma_width;
}
float utiMatch::d_PC3w_phi_match(float p, float zed,float dv){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pc3wphi_rms_run3[0]*pc3wphi_rms_run3[0]/(p*p) +  pc3wphi_rms_run3[1]*pc3wphi_rms_run3[1]);
  float offset ;
  int i=0;
  if(zed>0){//north
    if(p>0){
      i=0;
    }else{
      i=1;
    }
  }else{//south
    if(p>0){
      i=2;
    }else{
      i=3;
    }
  }
  offset = pc3wphi_mean_run3[i][0]*exp(-0.7*fabs(p))+pc3wphi_mean_run3[i][1];

  tmpshift = offset;
  return (dv-offset)/sigma_width;
}
float utiMatch::dv_PC3w_phi_match(float p, float zed,float sig){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pc3wphi_rms_run3[0]*pc3wphi_rms_run3[0]/(p*p) +  pc3wphi_rms_run3[1]*pc3wphi_rms_run3[1]);
  float offset ;
  int i=0;
  if(zed>0){//north
    if(p>0){
      i=0;
    }else{
      i=1;
    }
  }else{//south
    if(p>0){
      i=2;
    }else{
      i=3;
    }
  }
  offset = pc3wphi_mean_run3[i][0]*exp(-0.7*fabs(p))+pc3wphi_mean_run3[i][1];
 
  tmpshift = offset;
  return offset+sig*sigma_width;
}
float utiMatch::d_PC3w_z_match(float p, float zed,float dv){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pc3wz_rms_run3[0]*pc3wz_rms_run3[0]/(p*p) +  pc3wz_rms_run3[1]*pc3wz_rms_run3[1]);
  float offset;
  int i=0;
  if(zed>0){ i=0;}
  else{i=1;}
  offset = pc3wz_mean_run3[i][0]*exp(-0.7*fabs(p))+pc3wz_mean_run3[i][1];

  tmpshift = offset;
  return (dv-offset)/sigma_width;
}
float utiMatch::dv_PC3w_z_match(float p, float zed, float sig){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pc3wz_rms_run3[0]*pc3wz_rms_run3[0]/(p*p) +  pc3wz_rms_run3[1]*pc3wz_rms_run3[1]);
  float offset;
  int i=0;
  if(zed>0){ i=0;}
  else{i=1;}
  offset = pc3wz_mean_run3[i][0]*exp(-0.7*fabs(p))+pc3wz_mean_run3[i][1];
  
  tmpshift = offset;
  return offset+sig*sigma_width;
}
float utiMatch::d_TOF_phi_match(float p, float zed, float dv){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(tofphi_rms_run3[0]*tofphi_rms_run3[0]/(p*p) +  tofphi_rms_run3[1]*tofphi_rms_run3[1]);
  float offset;
  int i=0;
  if(p>0){
    i=0;
  }else{
    i=1;
  }
  offset = tofphi_mean_run3[i][0]*exp(-0.7*fabs(p))+tofphi_mean_run3[i][1];

  tmpshift = offset;
  return (dv-offset)/sigma_width;
}
float utiMatch::dv_TOF_phi_match(float p, float zed, float sig){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(tofphi_rms_run3[0]*tofphi_rms_run3[0]/(p*p) +  tofphi_rms_run3[1]*tofphi_rms_run3[1]);
  float offset;
  int i=0;
  if(p>0){
    i=0;
  }else{
    i=1;
  }
  offset = tofphi_mean_run3[i][0]*exp(-0.7*fabs(p))+tofphi_mean_run3[i][1];
  tmpshift = offset;
  return offset+sig*sigma_width;
}
float utiMatch::d_TOF_z_match(float p, float zed, float dv){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(tofz_rms_run3[0]*tofz_rms_run3[0]/(p*p) +  tofz_rms_run3[1]*tofz_rms_run3[1]);
  float offset;
  int i;
  if(zed>0){ i=0;}
  else{i=1;}
  offset = tofz_mean_run3[i][0]*exp(-0.7*fabs(p))+tofz_mean_run3[i][1];

  tmpshift = offset;
  return (dv-offset)/sigma_width;
}
float utiMatch::dv_TOF_z_match(float p, float zed, float sig){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(tofz_rms_run3[0]*tofz_rms_run3[0]/(p*p) +  tofz_rms_run3[1]*tofz_rms_run3[1]);
  float offset;
  int i;
  if(zed>0){ i=0;}
  else{i=1;}
  offset = tofz_mean_run3[i][0]*exp(-0.7*fabs(p))+tofz_mean_run3[i][1];

  tmpshift = offset;
  return offset+sig*sigma_width;
}
//run2 functions
float utiMatch::d_PC2_phi_match(float p, float dv){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pc2phi_rms[0]*pc2phi_rms[0]/(p*p) +  pc2phi_rms[1]*pc2phi_rms[1]);
  float offset;
  if(p>0){
    offset = pc2phi_mean_plus[1];
  }else{
    offset = pc2phi_mean_minus[1];
  }
  tmpshift = offset;
  return (dv-offset)/sigma_width;
}
float utiMatch::dv_PC2_phi_match(float p, float sig){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pc2phi_rms[0]*pc2phi_rms[0]/(p*p) +  pc2phi_rms[1]*pc2phi_rms[1]);
  float offset;
  if(p>0){
    offset = pc2phi_mean_plus[1];
  }else{
    offset = pc2phi_mean_minus[1];
  }
  tmpshift = offset;
  return offset+sig*sigma_width;
}
float utiMatch::d_PC2_z_match(float p, float dv){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pc2z_rms[0]*pc2z_rms[0]/(p*p) +  pc2z_rms[1]*pc2z_rms[1]);
  float offset      = pc2z_mean[1];

  tmpshift = offset;
  return (dv-offset)/sigma_width;
}
float utiMatch::dv_PC2_z_match(float p, float sig){
  if(p==0.0) return 1000;

  float sigma_width = sqrt(pc2z_rms[0]*pc2z_rms[0]/(p*p) +  pc2z_rms[1]*pc2z_rms[1]);
  float offset      = pc2z_mean[1];

  tmpshift = offset;
  return offset+sig*sigma_width;
}

float utiMatch::d_PC3e_phi_match(float p, float dv){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pc3ephi_rms[0]*pc3ephi_rms[0]/(p*p) +  pc3ephi_rms[1]*pc3ephi_rms[1]);
  float offset;
  if(p>0){
    offset = pc3ephi_mean_plus[1];
  }else{
    offset = pc3ephi_mean_minus[1];
  }   

  tmpshift = offset;
  return (dv-offset)/sigma_width;
}
float utiMatch::dv_PC3e_phi_match(float p, float sig){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pc3ephi_rms[0]*pc3ephi_rms[0]/(p*p) +  pc3ephi_rms[1]*pc3ephi_rms[1]);
  float offset;
  if(p>0){
    offset = pc3ephi_mean_plus[1];
  }else{
    offset = pc3ephi_mean_minus[1];
  }   

  tmpshift = offset;
  return offset+sig*sigma_width;
}
float utiMatch::d_PC3e_z_match(float p, float dv){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pc3ez_rms[0]*pc3ez_rms[0]/(p*p) +  pc3ez_rms[1]*pc3ez_rms[1]);
  float offset      = pc3ez_mean[1];

  tmpshift = offset;
  return (dv-offset)/sigma_width;
}
float utiMatch::dv_PC3e_z_match(float p, float sig){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pc3ez_rms[0]*pc3ez_rms[0]/(p*p) +  pc3ez_rms[1]*pc3ez_rms[1]);
  float offset      = pc3ez_mean[1];

  tmpshift = offset;
  return offset+sig*sigma_width;
}
float utiMatch::d_PC3w_phi_match(float p, float dv){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pc3wphi_rms[0]*pc3wphi_rms[0]/(p*p) +  pc3wphi_rms[1]*pc3wphi_rms[1]);
  float offset ;
  if(p>0){
    offset = pc3wphi_mean_plus[1];
  }else{
    offset = pc3wphi_mean_minus[1];
  }   
  tmpshift = offset;
  return (dv-offset)/sigma_width;
}
float utiMatch::dv_PC3w_phi_match(float p, float sig){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pc3wphi_rms[0]*pc3wphi_rms[0]/(p*p) +  pc3wphi_rms[1]*pc3wphi_rms[1]);
  float offset ;
  if(p>0){
    offset = pc3wphi_mean_plus[1];
  }else{
    offset = pc3wphi_mean_minus[1];
  }   
  tmpshift = offset;
  return offset+sig*sigma_width;
}
float utiMatch::d_PC3w_z_match(float p, float dv){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pc3wz_rms[0]*pc3wz_rms[0]/(p*p) +  pc3wz_rms[1]*pc3wz_rms[1]);
  float offset      = pc3wz_mean[1];

  tmpshift = offset;
  return (dv-offset)/sigma_width;
}
float utiMatch::dv_PC3w_z_match(float p, float sig){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pc3wz_rms[0]*pc3wz_rms[0]/(p*p) +  pc3wz_rms[1]*pc3wz_rms[1]);
  float offset      = pc3wz_mean[1];

  tmpshift = offset;
  return offset+sig*sigma_width;
}
float utiMatch::d_TOF_phi_match(float p, float dv){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(tofphi_rms[0]*tofphi_rms[0]/(p*p) +  tofphi_rms[1]*tofphi_rms[1]);
  float offset;
  if(p>0){
    offset = tofphi_mean_plus[0]/fabs(p) + tofphi_mean_plus[1];
  }else{
    offset = tofphi_mean_minus[0]/fabs(p) + tofphi_mean_minus[1];
  }

  tmpshift = offset;
  return (dv-offset)/sigma_width;
}
float utiMatch::dv_TOF_phi_match(float p, float sig){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(tofphi_rms[0]*tofphi_rms[0]/(p*p) +  tofphi_rms[1]*tofphi_rms[1]);
  float offset;
  if(p>0){
    offset = tofphi_mean_plus[0]/fabs(p) + tofphi_mean_plus[1];
  }else{
    offset = tofphi_mean_minus[0]/fabs(p) + tofphi_mean_minus[1];
  }

  tmpshift = offset;
  return offset+sig*sigma_width;
}
float utiMatch::d_TOF_z_match(float p, float dv){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(tofz_rms[0]*tofz_rms[0]/(p*p) +  tofz_rms[1]*tofz_rms[1]);
  float offset      = tofz_mean[1];

  tmpshift = offset;
  return (dv-offset)/sigma_width;
}
float utiMatch::dv_TOF_z_match(float p, float sig){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(tofz_rms[0]*tofz_rms[0]/(p*p) +  tofz_rms[1]*tofz_rms[1]);
  float offset      = tofz_mean[1];

  tmpshift = offset;
  return offset+sig*sigma_width;
}
//run2 and run3 functions
float utiMatch::d_TEC_phi_match(float p, float dv){
  if(p==0.0) return 10000;

  float pvalue=p; if(p<0)pvalue=0-p;

  float sigma_width = sqrt(tecphi_rms[0]*tecphi_rms[0]/p/p/sqrt(pvalue) +  
                           tecphi_rms[1]*tecphi_rms[1]);
  float offset;
  if(p>0){
    offset = tecphi_mean_plus[0]+tecphi_mean_plus[1]/p/p/p/p;
  }else{
    offset = tecphi_mean_minus[0]+tecphi_mean_minus[1]/p/p/p/p;
  }   

  tmpshift = offset;
  return (dv-offset)/sigma_width;
}
float utiMatch::dv_TEC_phi_match(float p, float sig){
  if(p==0.0) return 10000;

  float pvalue=p; if(p<0)pvalue=0-p;

  float sigma_width = sqrt(tecphi_rms[0]*tecphi_rms[0]/p/p/sqrt(pvalue) +  
                           tecphi_rms[1]*tecphi_rms[1]);
  float offset;
  if(p>0){
    offset = tecphi_mean_plus[0]+tecphi_mean_plus[1]/p/p/p/p;
  }else{
    offset = tecphi_mean_minus[0]+tecphi_mean_minus[1]/p/p/p/p;
  }   

  tmpshift = offset;
  return offset+sig*sigma_width;
}
float utiMatch::d_TEC_alpha_match(float p, float dv){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(tecalpha_rms[0]*tecalpha_rms[0]/(p*p) +  
                           tecalpha_rms[1]*tecalpha_rms[1]);

  float offset;
  if(p>0){
    offset = tecalpha_mean_plus[0]/p/p+tecalpha_mean_plus[1];
  }else{
    offset = tecalpha_mean_minus[0]/p/p+tecalpha_mean_minus[1];
  }   

  tmpshift = offset;
  return (dv-offset)/sigma_width;
}
float utiMatch::dv_TEC_alpha_match(float p, float sig){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(tecalpha_rms[0]*tecalpha_rms[0]/(p*p) +  
                           tecalpha_rms[1]*tecalpha_rms[1]);

  float offset;
  if(p>0){
    offset = tecalpha_mean_plus[0]/p/p+tecalpha_mean_plus[1];
  }else{
    offset = tecalpha_mean_minus[0]/p/p+tecalpha_mean_minus[1];
  }   

  tmpshift = offset;
  return offset+sig*sigma_width;
}
float utiMatch::d_PBGL_phi_match(float p, float dv, int type){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pbglphi_rms[0]*pbglphi_rms[0]/(p*p) +  pbglphi_rms[1]*pbglphi_rms[1]);
  float offset=0;
  if(p>0){
    offset = pbglphi_mean_plus[0]/p + pbglphi_mean_plus[1];
  }else{
    offset = pbglphi_mean_minus[0]/fabs(p) + pbglphi_mean_minus[1];    
  }
  tmpshift = offset;
  float sigma0 =  (dv-offset)/sigma_width;
  if(type==1){
    return PBGL_sdphi_electron(sigma0,p);
  }
  return sigma0;
}
float utiMatch::dv_PBGL_phi_match(float p, float sig,int type){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pbglphi_rms[0]*pbglphi_rms[0]/(p*p) +  pbglphi_rms[1]*pbglphi_rms[1]);
  float offset=0;
  if(p>0){
    offset = pbglphi_mean_plus[0]/p + pbglphi_mean_plus[1];
  }else{
    offset = pbglphi_mean_minus[0]/fabs(p) + pbglphi_mean_minus[1];    
  }
  tmpshift = offset;
  return offset+sig*sigma_width;
}
float utiMatch::d_PBGL_z_match(float p,float beta, float dv,int type){
  if(fabs(p)<=0.21) return 10000;

  float sigma_width = sqrt(pbglz_rms[0]*pbglz_rms[0]/(p*p) +  pbglz_rms[1]*pbglz_rms[1]);
  float offset,tmp;

  offset = pbglz_mean[0]/fabs(p) + pbglz_mean[1];
  if(type==1){
    tmp    = (pbglz_shift_electron[0] + pbglz_shift_electron[1]*log(fabs(p) - 0.2))*cos(beta);
  }else if(type==2){
    tmp    = (pbglz_shift_mip[0] + pbglz_shift_mip[1]*log(fabs(p) - 0.2))*cos(beta);
  }else{
    tmp    = (pbglz_shift_nucl[0] + pbglz_shift_nucl[1]*log(fabs(p) - 0.2))*cos(beta);
  }
  offset +=tmp;
  tmpshift = offset;
  float sigma0 =  (dv-offset)/sigma_width;
  if(type==1){
    return PBGL_sdz_electron(sigma0,p,beta);
  }
  return sigma0;
}
float utiMatch::dv_PBGL_z_match(float p,float beta, float sig,int type){
  if(fabs(p)<=0.21) return 10000;

  float sigma_width = sqrt(pbglz_rms[0]*pbglz_rms[0]/(p*p) +  pbglz_rms[1]*pbglz_rms[1]);
  float offset,tmp;

  offset = pbglz_mean[0]/fabs(p) + pbglz_mean[1];
  if(type==1){
    tmp    = (pbglz_shift_electron[0] + pbglz_shift_electron[1]*log(fabs(p) - 0.2))*cos(beta);
  }else if(type==2){
    tmp    = (pbglz_shift_mip[0] + pbglz_shift_mip[1]*log(fabs(p) - 0.2))*cos(beta);
  }else{
    tmp    = (pbglz_shift_nucl[0] + pbglz_shift_nucl[1]*log(fabs(p) - 0.2))*cos(beta);
  }
  offset +=tmp;
  tmpshift = offset;
  return offset+sig*sigma_width;
}
float utiMatch::d_PBSCe_phi_match(float p, float dv,int type){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pbscephi_rms[0]*pbscephi_rms[0]/(p*p) +  pbscephi_rms[1]*pbscephi_rms[1]);
  float offset=0;
  if(p>0){
    offset = pbscephi_mean_plus[0]/p + pbscephi_mean_plus[1];
  }else{
    offset = pbscephi_mean_minus[0]/fabs(p) + pbscephi_mean_minus[1];    
  }
  tmpshift = offset;
  float sigma0 =  (dv-offset)/sigma_width;
  if(type==1){
    return PBSC_sdphi_electron(sigma0,p);
  }
  return sigma0;
}
float utiMatch::dv_PBSCe_phi_match(float p, float sig,int type){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pbscephi_rms[0]*pbscephi_rms[0]/(p*p) +  pbscephi_rms[1]*pbscephi_rms[1]);
  float offset=0;
  if(p>0){
    offset = pbscephi_mean_plus[0]/p + pbscephi_mean_plus[1];
  }else{
    offset = pbscephi_mean_minus[0]/fabs(p) + pbscephi_mean_minus[1];    
  }
  tmpshift = offset;
  return offset+sig*sigma_width;
}
float utiMatch::d_PBSCe_z_match(float p,float beta, float dv,int type){
  if(fabs(p)<0.21) return 10000;

  float sigma_width = sqrt(pbscez_rms[0]*pbscez_rms[0]/(p*p) +  pbscez_rms[1]*pbscez_rms[1]);
  float offset,tmp;
  offset = pbscez_mean[0]/fabs(p) + pbscez_mean[1];

  if(type==1){
    tmp    = (pbscez_shift_electron[0] + pbscez_shift_electron[1]*log(fabs(p) - 0.2))*cos(beta);
  }else if(type==2){
    tmp    = (pbscez_shift_mip[0] + pbscez_shift_mip[1]*log(fabs(p) - 0.2))*cos(beta);
  }else{
    tmp    = (pbscez_shift_nucl[0] + pbscez_shift_nucl[1]*log(fabs(p) - 0.2))*cos(beta);
  }
  offset +=tmp;
  tmpshift = offset;
  float sigma0 =  (dv-offset)/sigma_width;
  if(type==1){
    return PBSC_sdz_electron(sigma0,p, beta);
  }
  return sigma0;
}

float utiMatch::dv_PBSCe_z_match(float p,float beta, float sig,int type){
  if(fabs(p)<0.21) return 10000;

  float sigma_width = sqrt(pbscez_rms[0]*pbscez_rms[0]/(p*p) +  pbscez_rms[1]*pbscez_rms[1]);
  float offset,tmp;
  offset = pbscez_mean[0]/fabs(p) + pbscez_mean[1];

  if(type==1){
    tmp    = (pbscez_shift_electron[0] + pbscez_shift_electron[1]*log(fabs(p) - 0.2))*cos(beta);
  }else if(type==2){
    tmp    = (pbscez_shift_mip[0] + pbscez_shift_mip[1]*log(fabs(p) - 0.2))*cos(beta);
  }else{
    tmp    = (pbscez_shift_nucl[0] + pbscez_shift_nucl[1]*log(fabs(p) - 0.2))*cos(beta);
  }
  offset +=tmp;
  tmpshift = offset;
  return offset+sig*sigma_width;
}
float utiMatch::d_PBSCw_phi_match(float p, float dv, int type){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pbscwphi_rms[0]*pbscwphi_rms[0]/(p*p) +  pbscwphi_rms[1]*pbscwphi_rms[1]);
  float offset=0;
  if(p>0){
    offset = pbscwphi_mean_plus[0]/p + pbscwphi_mean_plus[1];
  }else{
    offset = pbscwphi_mean_minus[0]/fabs(p) + pbscwphi_mean_minus[1];    
  }
  tmpshift = offset;
  float sigma0 =  (dv-offset)/sigma_width;
  if(type==1){//correct for electron
    return PBSC_sdphi_electron(sigma0,p);
  }
  return sigma0;
}
float utiMatch::dv_PBSCw_phi_match(float p, float sig,int type){
  if(p==0.0) return 10000;

  float sigma_width = sqrt(pbscwphi_rms[0]*pbscwphi_rms[0]/(p*p) +  pbscwphi_rms[1]*pbscwphi_rms[1]);
  float offset=0;
  if(p>0){
    offset = pbscwphi_mean_plus[0]/p + pbscwphi_mean_plus[1];
  }else{
    offset = pbscwphi_mean_minus[0]/fabs(p) + pbscwphi_mean_minus[1];    
  }
  tmpshift = offset;
  return offset+sig*sigma_width;
}
float utiMatch::d_PBSCw_z_match(float p, float beta,float dv,int type){
  if(fabs(p)<=0.21) return 10000;

  float sigma_width = sqrt(pbscwz_rms[0]*pbscwz_rms[0]/(p*p) +  pbscwz_rms[1]*pbscwz_rms[1]);
  float offset,tmp;
  offset = pbscwz_mean[0]/fabs(p) + pbscwz_mean[1];
  if(type==1){
    tmp    = (pbscwz_shift_electron[0] + pbscwz_shift_electron[1]*log(fabs(p) - 0.2))*cos(beta);
  }else if(type==2){
    tmp    = (pbscwz_shift_mip[0] + pbscwz_shift_mip[1]*log(fabs(p) - 0.2))*cos(beta);
  }else{
    tmp    = (pbscwz_shift_nucl[0] + pbscwz_shift_nucl[1]*log(fabs(p) - 0.2))*cos(beta);
  }
  offset +=tmp;
  tmpshift = offset;
  float sigma0 =  (dv-offset)/sigma_width;
  if(type==1){
    return PBSC_sdz_electron(sigma0,p, beta);
  }
  return sigma0;
  //  return (dv-offset)/sigma_width;
}
float utiMatch::dv_PBSCw_z_match(float p, float beta,float sig,int type){
  if(fabs(p)<=0.21) return 10000;

  float sigma_width = sqrt(pbscwz_rms[0]*pbscwz_rms[0]/(p*p) +  pbscwz_rms[1]*pbscwz_rms[1]);
  float offset,tmp;
  offset = pbscwz_mean[0]/fabs(p) + pbscwz_mean[1];
  if(type==1){
    tmp    = (pbscwz_shift_electron[0] + pbscwz_shift_electron[1]*log(fabs(p) - 0.2))*cos(beta);
  }else if(type==2){
    tmp    = (pbscwz_shift_mip[0] + pbscwz_shift_mip[1]*log(fabs(p) - 0.2))*cos(beta);
  }else{
    tmp    = (pbscwz_shift_nucl[0] + pbscwz_shift_nucl[1]*log(fabs(p) - 0.2))*cos(beta);
  }
  offset +=tmp;
  tmpshift = offset;
  return offset+sig*sigma_width;
}


float utiMatch::PBSC_sdphi_electron(float sdphi0,float p){
  float sdphi = sdphi0+0.07/p+0.003/(p*p*p);
  sdphi = sdphi/(0.246+0.268/sqrt(fabs(p)));//normalize
  return sdphi;
}

float utiMatch::PBSC_sdz_electron(float sdz0,float p, float beta){
  p = fabs(p);
  float cz     = 2.055+1.046*log(p);
  float sdz  = sdz0 - cz*(beta-1.57) - 0.04;
  if(p<0.25) { 
    sdz = sdz/0.51;
  } else if (p < 0.3) {
    sdz = (sdz +0.007)/0.53;
  } else if( p< 0.4) {
    sdz = sdz/0.54;
  } else if( p< 0.8) {
    sdz = (sdz + 0.03)/0.54;
  } else if( p < 1.0) {
    sdz = (sdz + 0.045)/0.52;
  } else if( p < 1.5) {
    sdz = (sdz + 0.06)/0.5;
 } else {
   sdz = (sdz + 0.08)/0.48;
 }
  return sdz;
}
float utiMatch::PBGL_sdphi_electron(float sdphi0,float p){
  float sdphi =  sdphi0 + 0.01/(p*p*p);
  p = fabs(p);
  if(p < 0.3) {
    sdphi /= 1.05;
  } else if(p<0.4) {
    sdphi /= 0.76;
  } else if(p<0.5) {
    sdphi /= 0.70;
  } else if(p<0.6) {
    sdphi /= 0.66;
  } else if(p<0.8) {
    sdphi /= 0.60;
  } else {
    sdphi /= 0.55;
  }
  return sdphi;
}
float utiMatch::PBGL_sdz_electron(float sdz0,float p, float beta){
  p = fabs(p);
  float sdz   = sdz0   - 2.2*(beta-1.57);
  if(p < 0.3) {
    sdz /= 0.9;
  } else if(p<0.4) {
    sdz /= 0.8;
  } else if(p<0.5) {
    sdz /= 0.75;
  } else if(p<0.6) {
    sdz /= 0.7;
  } else if(p<0.8) {
    sdz /= 0.64;
  } else {
    sdz /= 0.6;
  }
  return sdz;
}

void  utiMatch::pc2pc3_corr_phi(float pc2sdphi,float pc3sdphi,float&naphi,float&brphi){
  naphi = 0.6604*(pc2sdphi-1.137*pc3sdphi);
  brphi = 0.6604*(1.137*pc2sdphi+pc3sdphi);
}
void  utiMatch::pc2pc3_corr_z(float pc2sdz,float pc3sdz,float&naz,float&brz,float centc){
  if(centc<0||centc>100) return;

  naz = 1.482*(pc2sdz-1.03*pc3sdz);
  brz = 0.524*(1.03*pc2sdz+pc3sdz);
  float mean = 0.139-0.00873*centc;
  float sig  = 0.0002354*(centc-25)*(centc-25)+0.881;
  brz  = (brz-mean)/sig;
}

