#include "YWCutter.h"
#include "YWParticle.h"
#include "HbdCutterThresholdParameters.h"

#include <cmath>
#include <cstdlib>
#include <iostream>
#include <string>

using namespace std;

YWCutter::YWCutter(){
	verbose = 0;
	rejection = 10;
	//Projection Cut
	hbdpphi_cut = 0./60;
	hbdpz_cut = 0; 
	//CA Cut

	//Constants
	r2d = 180./3.14159265;
	d2r = 3.14159265/180.;

	runnumber = -9999;

}
YWCutter::~YWCutter(){

}

void YWCutter::Init(){
	if(verbose)cout<<"YWCutter::Init() begin.."<<endl;
	
	if (mc_flag == 0 || mc_flag == 2){
	  //Initialize thresholds for Run-10 200 GeV
	  if (runnumber > 300000 && runnumber <  310698) Initialize_200GeV();
	  //Initialize thresholds for Run-10 62 GeV
	  else if (runnumber >=  310698) Initialize_62GeV();
	  else{ cout << "YWCutter: no valid thresholds for the run " << runnumber << endl; }
	}
	else if (mc_flag == 1 || mc_flag == 3){
	  Initialize_HIJING();
	}
}

void YWCutter::Initialize_200GeV(){
	if(verbose)cout<<"YWCutter::Init() begin.."<<endl;
	//initialize with 1pad cluster
	for(int i=0;i<23;i++){
		for(int j=0;j<6;j++){
			for(int k=0;k<25;k++){
				for(int l=0;l<24;l++){
					if(i==type0||i==type1||i==type2||i==type3||i==type4){
						container[i][j][k].th_par[l][0]=par_200GeV_0_0[l][0];
						container[i][j][k].th_par[l][1]=par_200GeV_0_0[l][1];
					}
					if(i==type11){
						container[i][j][k].th_par[l][0]=par_200GeV_11_0[l][0];
						container[i][j][k].th_par[l][1]=par_200GeV_11_0[l][1];
					}
					if(i==type12){
						container[i][j][k].th_par[l][0]=par_200GeV_12_0[l][0];
						container[i][j][k].th_par[l][1]=par_200GeV_12_0[l][1];
					}
					if(i==type31||i==type33||i==type35||i==type36||i==type37||i==type38){
						container[i][j][k].th_par[l][0]=par_200GeV_33_0[l][0];
						container[i][j][k].th_par[l][1]=par_200GeV_33_0[l][1];
					}
					if(i==type32||i==type34 || i==type39 || i==type40 || i==type41||i==type42){
						container[i][j][k].th_par[l][0]=par_200GeV_32_0[l][0];
						container[i][j][k].th_par[l][1]=par_200GeV_32_0[l][1];
					}
					if(i == type64 || i == type62){
						container[i][j][k].th_par[l][0]=par_200GeV_64_0[l][0];
						container[i][j][k].th_par[l][1]=par_200GeV_64_0[l][1];
					}
					if(i==type63||i==type61){
						container[i][j][k].th_par[l][0]=par_200GeV_63_0[l][0];
						container[i][j][k].th_par[l][1]=par_200GeV_63_0[l][1];
					}
				}
				for(int l=0;l<4;l++){
					if(l==0){
						container[i][j][k].cluster_pads[l]=0;
					}else{
						container[i][j][k].cluster_pads[l]=-999;
					}
				}
				container[i][j][k].weight_pad0=1;
			}
		}
	}

	//Fill 1 pad threshold for non-1pad cluster
		//padtype 0
	for(int i=0;i<6;i++){
		for(int j=9;j<25;j++){
			if(j==9){
				for(int k=0;k<24;k++){
					container[type0][i][j].th_par[k][0]=par_200GeV_0_2[k][0];
					container[type0][i][j].th_par[k][1]=par_200GeV_0_2[k][1];
				}
				container[type0][i][j].cluster_pads[0]= 0;
				container[type0][i][j].cluster_pads[1]= 1;
				container[type0][i][j].cluster_pads[2]= 2;
			}else if(j>=10&&j<=14){
				for(int k=0;k<24;k++){
					container[type0][i][j].th_par[k][0]=par_200GeV_0_1[k][0];
					container[type0][i][j].th_par[k][1]=par_200GeV_0_1[k][1];
				}
				container[type0][i][j].cluster_pads[0]= 0;
				container[type0][i][j].cluster_pads[1]= 1;
			}else if(j==15){
				for(int k=0;k<24;k++){
					container[type0][i][j].th_par[k][0]=par_200GeV_0_2[k][0];
					container[type0][i][j].th_par[k][1]=par_200GeV_0_2[k][1];
				}
				container[type0][i][j].cluster_pads[0]= 0;
				container[type0][i][j].cluster_pads[1]= 1;
				container[type0][i][j].cluster_pads[2]= 3;
			}
		}
	}
	//padtype 1
	//seg0
	for(int i=0;i<25;i++){
		if(i==8&&(i>=13&&i<=15)){
			for(int k=0;k<24;k++){
				container[type1][0][i].th_par[k][0] = par_200GeV_1_089[k][0];	
				container[type1][0][i].th_par[k][1] = par_200GeV_1_089[k][1];	
			}
			container[type1][0][i].cluster_pads[0]= 0;
			container[type1][0][i].cluster_pads[1]= 1;
			container[type1][0][i].cluster_pads[2]= 3;
		}
		if(i>=10&&i<=12){
			for(int k=0;k<24;k++){
				container[type1][0][i].th_par[k][0] = par_200GeV_1_01[k][0];	
				container[type1][0][i].th_par[k][1] = par_200GeV_1_01[k][1];	
			}
			container[type1][0][i].cluster_pads[0]= 0;
			container[type1][0][i].cluster_pads[1]= 1;
		}
		if(i==9){
			for(int k=0;k<24;k++){
				container[type1][0][i].th_par[k][0] = par_200GeV_1_019[k][0];	
				container[type1][0][i].th_par[k][1] = par_200GeV_1_019[k][1];	
			}
			container[type1][0][i].cluster_pads[0]= 0;
			container[type1][0][i].cluster_pads[1]= 1;
			container[type1][0][i].cluster_pads[2]= 2;
		}
	}
	//seg1
	for(int i=0;i<25;i++){
		if(i==9){
			for(int k=0;k<24;k++){
				container[type1][1][i].th_par[k][0] = par_200GeV_1_019[k][0];	
				container[type1][1][i].th_par[k][1] = par_200GeV_1_019[k][1];	
			}
			container[type1][1][i].cluster_pads[0]= 0;
			container[type1][1][i].cluster_pads[1]= 1;
			container[type1][1][i].cluster_pads[2]= 2;
		}
		if(i==15){
			for(int k=0;k<24;k++){
				container[type1][1][i].th_par[k][0] = par_200GeV_1_019[k][0];	
				container[type1][1][i].th_par[k][1] = par_200GeV_1_019[k][1];	
			}
			container[type1][1][i].cluster_pads[0]= 0;
			container[type1][1][i].cluster_pads[1]= 1;
			container[type1][1][i].cluster_pads[2]= 3;
		}
		if(i>=10&&i<=14){
			for(int k=0;k<24;k++){
				container[type1][1][i].th_par[k][0] = par_200GeV_1_01[k][0];	
				container[type1][1][i].th_par[k][1] = par_200GeV_1_01[k][1];	
			}
			container[type1][1][i].cluster_pads[0]= 0;
			container[type1][1][i].cluster_pads[1]= 1;
		}
	}
	//seg2
	for(int i=0;i<25;i++){
		if(i==4&&(i>=9&&i<=11)){
			for(int k=0;k<24;k++){
				container[type1][2][i].th_par[k][0] = par_200GeV_1_089[k][0];	
				container[type1][2][i].th_par[k][1] = par_200GeV_1_089[k][1];	
			}
			container[type1][2][i].cluster_pads[0]= 0;
			container[type1][2][i].cluster_pads[1]= 1;
			container[type1][2][i].cluster_pads[2]= 2;
		}
		if(i>=12&&i<=14){
			for(int k=0;k<24;k++){
				container[type1][2][i].th_par[k][0] = par_200GeV_1_01[k][0];	
				container[type1][2][i].th_par[k][1] = par_200GeV_1_01[k][1];	
			}
			container[type1][2][i].cluster_pads[0]= 0;
			container[type1][2][i].cluster_pads[1]= 1;
		}
		if(i==15){
			for(int k=0;k<24;k++){
				container[type1][2][i].th_par[k][0] = par_200GeV_1_019[k][0];	
				container[type1][2][i].th_par[k][1] = par_200GeV_1_019[k][1];	
			}
			container[type1][2][i].cluster_pads[0]= 0;
			container[type1][2][i].cluster_pads[1]= 1;
			container[type1][2][i].cluster_pads[2]= 3;
		}
	}
	//seg3
	for(int i=0;i<25;i++){
		if(!(i==0||i==2)){
			for(int k=0;k<24;k++){
				container[type1][3][i].th_par[k][0] = par_200GeV_1_08[k][0];	
				container[type1][3][i].th_par[k][1] = par_200GeV_1_08[k][1];	
			}
			container[type1][3][i].cluster_pads[0]= 0;
			container[type1][3][i].cluster_pads[1]= 1;
		}
	}
	//seg4
	for(int i=0;i<25;i++){
		if(i==4||i==9||i==10||i==16||i==17||i==18){
			for(int k=0;k<24;k++){
				container[type1][4][i].th_par[k][0] = par_200GeV_1_08[k][0];	
				container[type1][4][i].th_par[k][1] = par_200GeV_1_08[k][1];	
			}
			container[type1][4][i].cluster_pads[0]= 0;
			container[type1][4][i].cluster_pads[1]= 1;
		}
		if(i==20){
			for(int k=0;k<24;k++){
				container[type1][4][i].th_par[k][0] = par_200GeV_1_0m185[k][0];	
				container[type1][4][i].th_par[k][1] = par_200GeV_1_0m185[k][1];	
			}
			container[type1][4][i].cluster_pads[0]= 0;
			container[type1][4][i].cluster_pads[1]= 6;
		}
		if(i==8||i==14||i==15||i==22||i==23||i==24){
			for(int k=0;k<24;k++){
				container[type1][4][i].th_par[k][0] = par_200GeV_1_08[k][0];	
				container[type1][4][i].th_par[k][1] = par_200GeV_1_08[k][1];	
			}
			container[type1][4][i].cluster_pads[0]= 0;
			container[type1][4][i].cluster_pads[1]= 2;
		}
	}
	//seg5
	for(int i=0;i<25;i++){
		if(!(i==0||i==2)){
			for(int k=0;k<24;k++){
				container[type1][5][i].th_par[k][0] = par_200GeV_1_08[k][0];	
				container[type1][5][i].th_par[k][1] = par_200GeV_1_08[k][1];	
			}
			container[type1][5][i].cluster_pads[0]= 0;
			container[type1][5][i].cluster_pads[1]= 1;
		}
	}
	//padtype 2
	//seg0
	for(int i=0;i<25;i++){
		if(!(i==0||i==2)){
			for(int k=0;k<24;k++){
				container[type2][0][i].th_par[k][0] = par_200GeV_2_09[k][0];	
				container[type2][0][i].th_par[k][1] = par_200GeV_2_09[k][1];	
			}
			container[type2][0][i].cluster_pads[0]= 0;
			container[type2][0][i].cluster_pads[1]= 1;
		}
	}
	//seg1
	for(int i=0;i<25;i++){
		if(i==4||i==9||i==10||i==16||i==17||i==18){
			for(int k=0;k<24;k++){
				container[type2][1][i].th_par[k][0] = par_200GeV_2_09[k][0];	
				container[type2][1][i].th_par[k][1] = par_200GeV_2_09[k][1];	
			}
			container[type2][1][i].cluster_pads[0]= 0;
			container[type2][1][i].cluster_pads[1]= 1;
		}
		if(i==20){
			for(int k=0;k<24;k++){
				container[type2][1][i].th_par[k][0] = par_200GeV_2_0185[k][0];	
				container[type2][1][i].th_par[k][1] = par_200GeV_2_0185[k][1];	
			}
			container[type2][1][i].cluster_pads[0]= 0;
			container[type2][1][i].cluster_pads[1]= 6;
		}
		if(i==8||i==14||i==15||i==22||i==23||i==24){
			for(int k=0;k<24;k++){
				container[type2][1][i].th_par[k][0] = par_200GeV_2_09[k][0];	
				container[type2][1][i].th_par[k][1] = par_200GeV_2_09[k][1];	
			}
			container[type2][1][i].cluster_pads[0]= 0;
			container[type2][1][i].cluster_pads[1]= 2;
		}
	}
	//seg2
	for(int i=0;i<25;i++){
		if(!(i==0||i==2)){
			for(int k=0;k<24;k++){
				container[type2][2][i].th_par[k][0] = par_200GeV_2_09[k][0];	
				container[type2][2][i].th_par[k][1] = par_200GeV_2_09[k][1];	
			}
			container[type2][2][i].cluster_pads[0]= 0;
			container[type2][2][i].cluster_pads[1]= 1;
		}
	}
	//seg3
	for(int i=0;i<25;i++){
		if(i==8&&(i>=13&&i<=15)){
			for(int k=0;k<24;k++){
				container[type2][3][i].th_par[k][0] = par_200GeV_2_089[k][0];	
				container[type2][3][i].th_par[k][1] = par_200GeV_2_089[k][1];	
			}
			container[type2][3][i].cluster_pads[0]= 0;
			container[type2][3][i].cluster_pads[1]= 1;
			container[type2][3][i].cluster_pads[1]= 3;
		}
		if(i>=10&&i<=12){
			for(int k=0;k<24;k++){
				container[type2][3][i].th_par[k][0] = par_200GeV_2_0m1[k][0];	
				container[type2][3][i].th_par[k][1] = par_200GeV_2_0m1[k][1];	
			}
			container[type2][3][i].cluster_pads[0]= 0;
			container[type2][3][i].cluster_pads[1]= 1;
		}
		if(i==9){
			for(int k=0;k<24;k++){
				container[type2][3][i].th_par[k][0] = par_200GeV_2_0m18[k][0];	
				container[type2][3][i].th_par[k][1] = par_200GeV_2_0m18[k][1];	
			}
			container[type2][3][i].cluster_pads[0]= 0;
			container[type2][3][i].cluster_pads[1]= 1;
			container[type2][3][i].cluster_pads[2]= 2;
		}
	}
	//seg4
	for(int i=0;i<25;i++){
		if(i==9){
			for(int k=0;k<24;k++){
				container[type2][4][i].th_par[k][0] = par_200GeV_2_0m18[k][0];	
				container[type2][4][i].th_par[k][1] = par_200GeV_2_0m18[k][1];	
			}
			container[type2][4][i].cluster_pads[0]= 0;
			container[type2][4][i].cluster_pads[1]= 1;
			container[type2][4][i].cluster_pads[2]= 2;
		}
		if(i==15){
			for(int k=0;k<24;k++){
				container[type2][4][i].th_par[k][0] = par_200GeV_2_0m18[k][0];	
				container[type2][4][i].th_par[k][1] = par_200GeV_2_0m18[k][1];	
			}
			container[type2][4][i].cluster_pads[0]= 0;
			container[type2][4][i].cluster_pads[1]= 1;
			container[type2][4][i].cluster_pads[2]= 3;
		}
		if(i>=10&&i<=14){
			for(int k=0;k<24;k++){
				container[type2][4][i].th_par[k][0] = par_200GeV_2_0m1[k][0];	
				container[type2][4][i].th_par[k][1] = par_200GeV_2_0m1[k][1];	
			}
			container[type2][4][i].cluster_pads[0]= 0;
			container[type2][4][i].cluster_pads[1]= 1;
		}
	}
	//seg5
	for(int i=0;i<25;i++){
		if(i==4&&(i>=9&&i<=11)){
			for(int k=0;k<24;k++){
				container[type2][5][i].th_par[k][0] = par_200GeV_2_089[k][0];	
				container[type2][5][i].th_par[k][1] = par_200GeV_2_089[k][1];	
			}
			container[type2][5][i].cluster_pads[0]= 0;
			container[type2][5][i].cluster_pads[1]= 1;
			container[type2][5][i].cluster_pads[2]= 2;
		}
		if(i>=12&&i<=14){
			for(int k=0;k<24;k++){
				container[type2][5][i].th_par[k][0] = par_200GeV_2_0m1[k][0];	
				container[type2][5][i].th_par[k][1] = par_200GeV_2_0m1[k][1];	
			}
			container[type2][5][i].cluster_pads[0]= 0;
			container[type2][5][i].cluster_pads[1]= 1;
		}
		if(i==15){
			for(int k=0;k<24;k++){
				container[type2][5][i].th_par[k][0] = par_200GeV_2_0m18[k][0];	
				container[type2][5][i].th_par[k][1] = par_200GeV_2_0m18[k][1];	
			}
			container[type2][5][i].cluster_pads[0]= 0;
			container[type2][5][i].cluster_pads[1]= 1;
			container[type2][5][i].cluster_pads[2]= 3;
		}
	}
	//padtype 3
	//seg 0-2
	for(int i=0;i<3;i++){
		for(int j=9;j<25;j++){
			if(j==9){
				for(int k=0;k<24;k++){
					container[type3][i][j].th_par[k][0]=par_200GeV_0_2[k][0];
					container[type3][i][j].th_par[k][1]=par_200GeV_0_2[k][1];
				}
				container[type3][i][j].cluster_pads[0]= 0;
				container[type3][i][j].cluster_pads[1]= 1;
				container[type3][i][j].cluster_pads[2]= 2;
			}else if(j>=10&&j<=14){
				for(int k=0;k<24;k++){
					container[type3][i][j].th_par[k][0]=par_200GeV_0_1[k][0];
					container[type3][i][j].th_par[k][1]=par_200GeV_0_1[k][1];
				}
				container[type3][i][j].cluster_pads[0]= 0;
				container[type3][i][j].cluster_pads[1]= 1;
			}else if(j==15){
				for(int k=0;k<24;k++){
					container[type3][i][j].th_par[k][0]=par_200GeV_0_2[k][0];
					container[type3][i][j].th_par[k][1]=par_200GeV_0_2[k][1];
				}
				container[type3][i][j].cluster_pads[0]= 0;
				container[type3][i][j].cluster_pads[1]= 1;
				container[type3][i][j].cluster_pads[2]= 3;
			}
		}
	}
	//seg3
	for(int i=0;i<25;i++){
		if(i==4||i==9){
			for(int k=0;k<24;k++){
				container[type3][3][i].th_par[k][0] = par_200GeV_12_09[k][0];	
				container[type3][3][i].th_par[k][1] = par_200GeV_12_09[k][1];	
			}
			container[type3][3][i].cluster_pads[0]= 0;
			container[type3][3][i].cluster_pads[1]= 2;
		}else if(i>=10&&i<=12){
			for(int k=0;k<24;k++){
				container[type3][3][i].th_par[k][0] = par_200GeV_12_019[k][0];	
				container[type3][3][i].th_par[k][1] = par_200GeV_12_019[k][1];	
			}
			container[type3][3][i].cluster_pads[0]= 0;
			container[type3][3][i].cluster_pads[1]= 1;
			container[type3][3][i].cluster_pads[2]= 2;
		}else if(i>=13&&i<=14){
			for(int k=0;k<24;k++){
				container[type3][3][i].th_par[k][0] = par_200GeV_0_1[k][0];	
				container[type3][3][i].th_par[k][1] = par_200GeV_0_1[k][1];	
			}
			container[type3][3][i].cluster_pads[0]= 0;
			container[type3][3][i].cluster_pads[1]= 1;
		}else if(i==15){
			for(int k=0;k<24;k++){
				container[type3][3][i].th_par[k][0] = par_200GeV_0_2[k][0];	
				container[type3][3][i].th_par[k][1] = par_200GeV_0_2[k][1];	
			}
			container[type3][3][i].cluster_pads[0]= 0;
			container[type3][3][i].cluster_pads[1]= 1;
			container[type3][3][i].cluster_pads[2]= 3;
		}
	}
	//seg4
	for(int i=0;i<25;i++){
		if(i>=1){
			for(int k=0;k<24;k++){
				container[type3][4][i].th_par[k][0] = par_200GeV_12_09[k][0];	
				container[type3][4][i].th_par[k][1] = par_200GeV_12_09[k][1];	
			}
			container[type3][4][i].cluster_pads[0]= 0;
			container[type3][4][i].cluster_pads[1]= 1;
		}
	}
	//seg5
	for(int i=0;i<25;i++){
		if(i==8||i==15){
			for(int k=0;k<24;k++){
				container[type3][5][i].th_par[k][0] = par_200GeV_12_09[k][0];	
				container[type3][5][i].th_par[k][1] = par_200GeV_12_09[k][1];	
			}
			container[type3][5][i].cluster_pads[0]= 0;
			container[type3][5][i].cluster_pads[1]= 3;
		}else if(i>=12&&i<=14){
			for(int k=0;k<24;k++){
				container[type3][5][i].th_par[k][0] = par_200GeV_12_019[k][0];	
				container[type3][5][i].th_par[k][1] = par_200GeV_12_019[k][1];	
			}
			container[type3][5][i].cluster_pads[0]= 0;
			container[type3][5][i].cluster_pads[1]= 1;
			container[type3][5][i].cluster_pads[2]= 3;
		}else if(i>=10&&i<=11){
			for(int k=0;k<24;k++){
				container[type3][5][i].th_par[k][0] = par_200GeV_0_1[k][0];	
				container[type3][5][i].th_par[k][1] = par_200GeV_0_1[k][1];	
			}
			container[type3][5][i].cluster_pads[0]= 0;
			container[type3][5][i].cluster_pads[1]= 1;
		}else if(i==9){
			for(int k=0;k<24;k++){
				container[type3][5][i].th_par[k][0] = par_200GeV_0_2[k][0];	
				container[type3][5][i].th_par[k][1] = par_200GeV_0_2[k][1];	
			}
			container[type3][5][i].cluster_pads[0]= 0;
			container[type3][5][i].cluster_pads[1]= 1;
			container[type3][5][i].cluster_pads[2]= 2;
		}
	}
	//padtype 4
	//seg 3-5
	for(int i=3;i<6;i++){
		for(int j=9;j<25;j++){
			if(j==9){
				for(int k=0;k<24;k++){
					container[type4][i][j].th_par[k][0]=par_200GeV_0_2[k][0];
					container[type4][i][j].th_par[k][1]=par_200GeV_0_2[k][1];
				}
				container[type4][i][j].cluster_pads[0]= 0;
				container[type4][i][j].cluster_pads[1]= 1;
				container[type4][i][j].cluster_pads[2]= 2;
			}else if(j>=10&&j<=14){
				for(int k=0;k<24;k++){
					container[type4][i][j].th_par[k][0]=par_200GeV_0_1[k][0];
					container[type4][i][j].th_par[k][1]=par_200GeV_0_1[k][1];
				}
				container[type4][i][j].cluster_pads[0]= 0;
				container[type4][i][j].cluster_pads[1]= 1;
			}else if(j==15){
				for(int k=0;k<24;k++){
					container[type4][i][j].th_par[k][0]=par_200GeV_0_2[k][0];
					container[type4][i][j].th_par[k][1]=par_200GeV_0_2[k][1];
				}
				container[type4][i][j].cluster_pads[0]= 0;
				container[type4][i][j].cluster_pads[1]= 1;
				container[type4][i][j].cluster_pads[2]= 3;
			}
		}
	}
	//seg0
	for(int i=0;i<25;i++){
		if(i==4||i==9){
			for(int k=0;k<24;k++){
				container[type4][0][i].th_par[k][0] = par_200GeV_11_08[k][0];	
				container[type4][0][i].th_par[k][1] = par_200GeV_11_08[k][1];	
			}
			container[type4][0][i].cluster_pads[0]= 0;
			container[type4][0][i].cluster_pads[1]= 2;
		}else if(i>=10&&i<=12){
			for(int k=0;k<24;k++){
				container[type4][0][i].th_par[k][0] = par_200GeV_11_0m18[k][0];	
				container[type4][0][i].th_par[k][1] = par_200GeV_11_0m18[k][1];	
			}
			container[type4][0][i].cluster_pads[0]= 0;
			container[type4][0][i].cluster_pads[1]= 1;
			container[type4][0][i].cluster_pads[2]= 2;
		}else if(i>=13&&i<=14){
			for(int k=0;k<24;k++){
				container[type4][0][i].th_par[k][0] = par_200GeV_0_1[k][0];	
				container[type4][0][i].th_par[k][1] = par_200GeV_0_1[k][1];	
			}
			container[type4][0][i].cluster_pads[0]= 0;
			container[type4][0][i].cluster_pads[1]= 1;
		}else if(i==15){
			for(int k=0;k<24;k++){
				container[type4][0][i].th_par[k][0] = par_200GeV_0_2[k][0];	
				container[type4][0][i].th_par[k][1] = par_200GeV_0_2[k][1];	
			}
			container[type4][0][i].cluster_pads[0]= 0;
			container[type4][0][i].cluster_pads[1]= 1;
			container[type4][0][i].cluster_pads[2]= 3;
		}
	}
	//seg1
	for(int i=0;i<25;i++){
		if(i>=1){
			for(int k=0;k<24;k++){
				container[type4][4][i].th_par[k][0] = par_200GeV_11_08[k][0];	
				container[type4][4][i].th_par[k][1] = par_200GeV_11_08[k][1];	
			}
			container[type4][4][i].cluster_pads[0]= 0;
			container[type4][4][i].cluster_pads[1]= 1;
		}
	}
	//seg2
	for(int i=0;i<25;i++){
		if(i==8||i==15){
			for(int k=0;k<24;k++){
				container[type4][0][i].th_par[k][0] = par_200GeV_11_08[k][0];	
				container[type4][0][i].th_par[k][1] = par_200GeV_11_08[k][1];	
			}
			container[type4][0][i].cluster_pads[0]= 0;
			container[type4][0][i].cluster_pads[1]= 3;
		}else if(i>=12&&i<=14){
			for(int k=0;k<24;k++){
				container[type4][0][i].th_par[k][0] = par_200GeV_11_0m18[k][0];	
				container[type4][0][i].th_par[k][1] = par_200GeV_11_0m18[k][1];	
			}
			container[type4][0][i].cluster_pads[0]= 0;
			container[type4][0][i].cluster_pads[1]= 1;
			container[type4][0][i].cluster_pads[2]= 3;
		}else if(i>=10&&i<=11){
			for(int k=0;k<24;k++){
				container[type4][0][i].th_par[k][0] = par_200GeV_0_1[k][0];	
				container[type4][0][i].th_par[k][1] = par_200GeV_0_1[k][1];	
			}
			container[type4][0][i].cluster_pads[0]= 0;
			container[type4][0][i].cluster_pads[1]= 1;
		}else if(i==9){
			for(int k=0;k<24;k++){
				container[type4][0][i].th_par[k][0] = par_200GeV_0_2[k][0];	
				container[type4][0][i].th_par[k][1] = par_200GeV_0_2[k][1];	
			}
			container[type4][0][i].cluster_pads[0]= 0;
			container[type4][0][i].cluster_pads[1]= 1;
			container[type4][0][i].cluster_pads[2]= 2;
		}
	}
	//padtype 11
	//seg0
	for(int i=0;i<25;i++){
		if(i==1||i==5||i==6||i==12||i==13||i==21||i==22||i==4||i==10||i==11||i==19||i==20){
			for(int k=0;k<24;k++){
				container[type11][0][i].th_par[k][0] = par_200GeV_11_0184[k][0];	
				container[type11][0][i].th_par[k][1] = par_200GeV_11_0184[k][1];	
			}
			container[type11][0][i].cluster_pads[0]= 0;
			container[type11][0][i].cluster_pads[1]= 4;
		}
	}
	//seg1
	for(int i=0;i<25;i++){
		if(i>=1){
			for(int k=0;k<24;k++){
				container[type11][1][i].th_par[k][0] = par_200GeV_11_0184[k][0];	
				container[type11][1][i].th_par[k][1] = par_200GeV_11_0184[k][1];	
			}
			container[type11][1][i].cluster_pads[0]= 0;
			container[type11][1][i].cluster_pads[1]= 4;
		}
	}
	//seg2
	for(int i=0;i<25;i++){
		if(i==3||i==6||i==7||i==11||i==12||i==18||i==19||i==8||i==13||i==14||i==20||i==21){
			for(int k=0;k<24;k++){
				container[type11][2][i].th_par[k][0] = par_200GeV_11_0184[k][0];	
				container[type11][2][i].th_par[k][1] = par_200GeV_11_0184[k][1];	
			}
			container[type11][2][i].cluster_pads[0]= 0;
			container[type11][2][i].cluster_pads[1]= 4;
		}
	}
	//seg3
	for(int i=0;i<25;i++){
		if(i==8||i==6||(i>=9&&i<=15)){
			for(int k=0;k<24;k++){
				container[type11][3][i].th_par[k][0] = par_200GeV_11_08[k][0];	
				container[type11][3][i].th_par[k][1] = par_200GeV_11_08[k][1];	
			}
			container[type11][3][i].cluster_pads[0]= 0;
			container[type11][3][i].cluster_pads[1]= 1;
		}
	}
	//seg4
	for(int i=0;i<25;i++){
		if(i==9){
			for(int k=0;k<24;k++){
				container[type11][4][i].th_par[k][0] = par_200GeV_11_0m18[k][0];	
				container[type11][4][i].th_par[k][1] = par_200GeV_11_0m18[k][1];	
			}
			container[type11][4][i].cluster_pads[0]= 0;
			container[type11][4][i].cluster_pads[1]= 1;
			container[type11][4][i].cluster_pads[2]= 2;
		}
		if(i==15){
			for(int k=0;k<24;k++){
				container[type11][4][i].th_par[k][0] = par_200GeV_11_0m18[k][0];	
				container[type11][4][i].th_par[k][1] = par_200GeV_11_0m18[k][1];	
			}
			container[type11][4][i].cluster_pads[0]= 0;
			container[type11][4][i].cluster_pads[1]= 1;
			container[type11][4][i].cluster_pads[2]= 3;
		}
		if(i==11||i==13){
			for(int k=0;k<24;k++){
				container[type11][4][i].th_par[k][0] = par_200GeV_11_08[k][0];	
				container[type11][4][i].th_par[k][1] = par_200GeV_11_08[k][1];	
			}
			container[type11][4][i].cluster_pads[0]= 0;
			container[type11][4][i].cluster_pads[1]= 1;
		}
	}
	//seg5
	for(int i=0;i<25;i++){
		if(i==4||i==6||(i>=9&&i<=15)){
			for(int k=0;k<24;k++){
				container[type11][5][i].th_par[k][0] = par_200GeV_11_08[k][0];	
				container[type11][5][i].th_par[k][1] = par_200GeV_11_08[k][1];	
			}
			container[type11][5][i].cluster_pads[0]= 0;
			container[type11][5][i].cluster_pads[1]= 1;
		}
	}
	//padtype 12
	//seg0
	for(int i=0;i<25;i++){
		if(i==8||i==6||(i>=9&&i<=15)){
			for(int k=0;k<24;k++){
				container[type12][0][i].th_par[k][0] = par_200GeV_12_09[k][0];	
				container[type12][0][i].th_par[k][1] = par_200GeV_12_09[k][1];	
			}
			container[type12][0][i].cluster_pads[0]= 0;
			container[type12][0][i].cluster_pads[1]= 1;
		}
	}
	//seg1
	for(int i=0;i<25;i++){
		if(i==9){
			for(int k=0;k<24;k++){
				container[type12][1][i].th_par[k][0] = par_200GeV_12_019[k][0];	
				container[type12][1][i].th_par[k][1] = par_200GeV_12_019[k][1];	
			}
			container[type12][1][i].cluster_pads[0]= 0;
			container[type12][1][i].cluster_pads[1]= 1;
			container[type12][1][i].cluster_pads[0]= 2;
		}
		if(i==15){
			for(int k=0;k<24;k++){
				container[type12][1][i].th_par[k][0] = par_200GeV_12_019[k][0];	
				container[type12][1][i].th_par[k][1] = par_200GeV_12_019[k][1];	
			}
			container[type12][1][i].cluster_pads[0]= 0;
			container[type12][1][i].cluster_pads[1]= 1;
			container[type12][1][i].cluster_pads[2]= 3;
		}
		if(i==11||i==13){
			for(int k=0;k<24;k++){
				container[type12][1][i].th_par[k][0] = par_200GeV_12_09[k][0];	
				container[type12][1][i].th_par[k][1] = par_200GeV_12_09[k][1];	
			}
			container[type12][1][i].cluster_pads[0]= 0;
			container[type12][1][i].cluster_pads[1]= 1;
		}
	}
	//seg2
	for(int i=0;i<25;i++){
		if(i==4||i==6||(i>=9&&i<=15)){
			for(int k=0;k<24;k++){
				container[type12][2][i].th_par[k][0] = par_200GeV_12_09[k][0];	
				container[type12][2][i].th_par[k][1] = par_200GeV_12_09[k][1];	
			}
			container[type12][2][i].cluster_pads[0]= 0;
			container[type12][2][i].cluster_pads[1]= 1;
		}
	}
	//seg3
	for(int i=0;i<25;i++){
		if(i==1||i==5||i==6||i==12||i==13||i==21||i==22||i==4||i==10||i==11||i==19||i==20){
			for(int k=0;k<24;k++){
				container[type12][3][i].th_par[k][0] = par_200GeV_12_0m184[k][0];	
				container[type12][3][i].th_par[k][1] = par_200GeV_12_0m184[k][1];	
			}
			container[type12][3][i].cluster_pads[0]= 0;
			container[type12][3][i].cluster_pads[1]= 4;
		}
	}
	//seg4
	for(int i=0;i<25;i++){
		if(i>=1){
			for(int k=0;k<24;k++){
				container[type12][4][i].th_par[k][0] = par_200GeV_12_0m184[k][0];	
				container[type12][4][i].th_par[k][1] = par_200GeV_12_0m184[k][1];	
			}
			container[type12][4][i].cluster_pads[0]= 0;
			container[type12][4][i].cluster_pads[1]= 4;
		}
	}
	//seg5
	for(int i=0;i<25;i++){
		if(i==3||i==6||i==7||i==11||i==12||i==18||i==19||i==8||i==14||i==13||i==20||i==21){
			for(int k=0;k<24;k++){
				container[type12][3][i].th_par[k][0] = par_200GeV_12_0m184[k][0];	
				container[type12][3][i].th_par[k][1] = par_200GeV_12_0m184[k][1];	
			}
			container[type12][3][i].cluster_pads[0]= 0;
			container[type12][3][i].cluster_pads[1]= 4;
		}
	}
	//padtype 32
	//seg0
	for(int i=0;i<25;i++){
		if(i>=16&&i<=18){
			for(int k=0;k<24;k++){
				container[type32][0][i].th_par[k][0] = par_200GeV_32_019[k][0];	
				container[type32][0][i].th_par[k][1] = par_200GeV_32_019[k][1];	
			}
			container[type32][0][i].cluster_pads[0]= 0;
			container[type32][0][i].cluster_pads[1]= 1;
			container[type32][0][i].cluster_pads[2]= 2;
		}else if(i>=20&&i<=23){
			for(int k=0;k<24;k++){
				container[type32][0][i].th_par[k][0] = par_200GeV_32_09[k][0];	
				container[type32][0][i].th_par[k][1] = par_200GeV_32_09[k][1];	
			}
			container[type32][0][i].cluster_pads[0]= 0;
			container[type32][0][i].cluster_pads[1]= 1;
		}else if(i==24){
			for(int k=0;k<24;k++){
				container[type32][0][i].th_par[k][0] = par_200GeV_32_089[k][0];	
				container[type32][0][i].th_par[k][1] = par_200GeV_32_089[k][1];	
			}
			container[type32][0][i].cluster_pads[0]= 0;
			container[type32][0][i].cluster_pads[1]= 1;
			container[type32][0][i].cluster_pads[2]= 3;
		}
	}
	//seg1
	for(int i=0;i<25;i++){
		if(i==9){
			for(int k=0;k<24;k++){
				container[type32][1][i].th_par[k][0] = par_200GeV_32_01m8[k][0];	
				container[type32][1][i].th_par[k][1] = par_200GeV_32_01m8[k][1];	
			}
			container[type32][1][i].cluster_pads[0]= 0;
			container[type32][1][i].cluster_pads[1]= 1;
			container[type32][1][i].cluster_pads[2]= 2;
		}else if(i>=10&&i<=15){
			for(int k=0;k<24;k++){
				container[type32][1][i].th_par[k][0] = par_200GeV_32_01[k][0];	
				container[type32][1][i].th_par[k][1] = par_200GeV_32_01[k][1];	
			}
			container[type32][1][i].cluster_pads[0]= 0;
			container[type32][1][i].cluster_pads[1]= 1;
		}
	}
	//seg2
	for(int i=0;i<25;i++){
		if(i>=9&&i<=15){
			for(int k=0;k<24;k++){
				container[type32][2][i].th_par[k][0] = par_200GeV_32_0m8[k][0];	
				container[type32][2][i].th_par[k][1] = par_200GeV_32_0m8[k][1];	
			}
			container[type32][2][i].cluster_pads[0]= 0;
			container[type32][2][i].cluster_pads[1]= 1;
		}
	}
	//seg3
	for(int i=0;i<25;i++){
		if(i>=9&&i<=15){
			for(int k=0;k<24;k++){
				container[type32][3][i].th_par[k][0] = par_200GeV_32_0m8[k][0];	
				container[type32][3][i].th_par[k][1] = par_200GeV_32_0m8[k][1];	
			}
			container[type32][3][i].cluster_pads[0]= 0;
			container[type32][3][i].cluster_pads[1]= 1;
		}
	}
	//seg4
	for(int i=0;i<25;i++){
		if(i==15){
			for(int k=0;k<24;k++){
				container[type32][4][i].th_par[k][0] = par_200GeV_32_01m8[k][0];	
				container[type32][4][i].th_par[k][1] = par_200GeV_32_01m8[k][1];	
			}
			container[type32][4][i].cluster_pads[0]= 0;
			container[type32][4][i].cluster_pads[1]= 1;
			container[type32][4][i].cluster_pads[2]= 3;
		}else if(i>=9&&i<=14){
			for(int k=0;k<24;k++){
				container[type32][4][i].th_par[k][0] = par_200GeV_32_01[k][0];	
				container[type32][4][i].th_par[k][1] = par_200GeV_32_01[k][1];	
			}
			container[type32][4][i].cluster_pads[0]= 0;
			container[type32][4][i].cluster_pads[1]= 1;
		}
	}
	//seg 5
	for(int i=0;i<25;i++){
		if(i>=22&&i<=24){
			for(int k=0;k<24;k++){
				container[type32][5][i].th_par[k][0] = par_200GeV_32_019[k][0];	
				container[type32][5][i].th_par[k][1] = par_200GeV_32_019[k][1];	
			}
			container[type32][5][i].cluster_pads[0]= 0;
			container[type32][5][i].cluster_pads[1]= 1;
			container[type32][5][i].cluster_pads[2]= 3;
		}else if(i>=17&&i<=20){
			for(int k=0;k<24;k++){
				container[type32][5][i].th_par[k][0] = par_200GeV_32_09[k][0];	
				container[type32][5][i].th_par[k][1] = par_200GeV_32_09[k][1];	
			}
			container[type32][5][i].cluster_pads[0]= 0;
			container[type32][5][i].cluster_pads[1]= 1;
		}else if(i==16){
			for(int k=0;k<24;k++){
				container[type32][5][i].th_par[k][0] = par_200GeV_32_089[k][0];	
				container[type32][5][i].th_par[k][1] = par_200GeV_32_089[k][1];	
			}
			container[type32][5][i].cluster_pads[0]= 0;
			container[type32][5][i].cluster_pads[1]= 1;
			container[type32][5][i].cluster_pads[2]= 2;
		}
	}
	//padtype 33
	//seg0
	for(int i=0;i<25;i++){
		if(i>=9&&i<=15){
			for(int k=0;k<24;k++){
				container[type33][0][i].th_par[k][0] = par_200GeV_33_09[k][0];	
				container[type33][0][i].th_par[k][1] = par_200GeV_33_09[k][1];	
			}
			container[type33][0][i].cluster_pads[0]= 0;
			container[type33][0][i].cluster_pads[1]= 1;
		}
	}
	//seg1
	for(int i=0;i<25;i++){
		if(i==15){
			for(int k=0;k<24;k++){
				container[type33][1][i].th_par[k][0] = par_200GeV_33_019[k][0];	
				container[type33][1][i].th_par[k][1] = par_200GeV_33_019[k][1];	
			}
			container[type33][1][i].cluster_pads[0]= 0;
			container[type33][1][i].cluster_pads[1]= 1;
			container[type33][1][i].cluster_pads[2]= 3;
		}else if(i>=9&&i<=14){
			for(int k=0;k<24;k++){
				container[type33][1][i].th_par[k][0] = par_200GeV_33_01[k][0];	
				container[type33][1][i].th_par[k][1] = par_200GeV_33_01[k][1];	
			}
			container[type33][1][i].cluster_pads[0]= 0;
			container[type33][1][i].cluster_pads[1]= 1;
		}
	}
	//seg2
	for(int i=0;i<25;i++){
		if(i==16){
			for(int k=0;k<24;k++){
				container[type33][2][i].th_par[k][0] = par_200GeV_33_0m9m8[k][0];	
				container[type33][2][i].th_par[k][1] = par_200GeV_33_0m9m8[k][1];	
			}
			container[type33][2][i].cluster_pads[0]= 0;
			container[type33][2][i].cluster_pads[1]= 1;
			container[type33][2][i].cluster_pads[2]= 2;
		}else if(i>=17&&i<=20){
			for(int k=0;k<24;k++){
				container[type33][2][i].th_par[k][0] = par_200GeV_33_0m8[k][0];	
				container[type33][2][i].th_par[k][1] = par_200GeV_33_0m8[k][1];	
			}
			container[type33][2][i].cluster_pads[0]= 0;
			container[type33][2][i].cluster_pads[1]= 1;
		}else if(i>=22&&i<=24){
			for(int k=0;k<24;k++){
				container[type33][2][i].th_par[k][0] = par_200GeV_33_01m8[k][0];	
				container[type33][2][i].th_par[k][1] = par_200GeV_33_01m8[k][1];	
			}
			container[type33][2][i].cluster_pads[0]= 0;
			container[type33][2][i].cluster_pads[1]= 1;
			container[type33][2][i].cluster_pads[2]= 3;
		}
	}
	//seg3
	for(int i=0;i<25;i++){
		if(i>=16&&i<=18){
			for(int k=0;k<24;k++){
				container[type33][3][i].th_par[k][0] = par_200GeV_33_01m8[k][0];	
				container[type33][3][i].th_par[k][1] = par_200GeV_33_01m8[k][1];	
			}
			container[type33][3][i].cluster_pads[0]= 0;
			container[type33][3][i].cluster_pads[1]= 1;
			container[type33][3][i].cluster_pads[2]= 2;
		}else if(i>=20&&i<=23){
			for(int k=0;k<24;k++){
				container[type33][3][i].th_par[k][0] = par_200GeV_33_0m8[k][0];	
				container[type33][3][i].th_par[k][1] = par_200GeV_33_0m8[k][1];	
			}
			container[type33][3][i].cluster_pads[0]= 0;
			container[type33][3][i].cluster_pads[1]= 1;
		}else if(i==24){
			for(int k=0;k<24;k++){
				container[type33][3][i].th_par[k][0] = par_200GeV_33_0m9m8[k][0];	
				container[type33][3][i].th_par[k][1] = par_200GeV_33_0m9m8[k][1];	
			}
			container[type33][3][i].cluster_pads[0]= 0;
			container[type33][3][i].cluster_pads[1]= 1;
			container[type33][3][i].cluster_pads[2]= 3;
		}
	}
	//seg4
	for(int i=0;i<25;i++){
		if(i==9){
			for(int k=0;k<24;k++){
				container[type33][4][i].th_par[k][0] = par_200GeV_33_019[k][0];	
				container[type33][4][i].th_par[k][1] = par_200GeV_33_019[k][1];	
			}
			container[type33][4][i].cluster_pads[0]= 0;
			container[type33][4][i].cluster_pads[1]= 1;
			container[type33][4][i].cluster_pads[2]= 2;
		}else if(i>=10&&i<=15){
			for(int k=0;k<24;k++){
				container[type33][4][i].th_par[k][0] = par_200GeV_33_01[k][0];	
				container[type33][4][i].th_par[k][1] = par_200GeV_33_01[k][1];	
			}
			container[type33][4][i].cluster_pads[0]= 0;
			container[type33][4][i].cluster_pads[1]= 1;
		}
	}
	//seg5
	for(int i=0;i<25;i++){
		if(i>=9&&i<=15){
			for(int k=0;k<24;k++){
				container[type33][5][i].th_par[k][0] = par_200GeV_33_09[k][0];	
				container[type33][5][i].th_par[k][1] = par_200GeV_33_09[k][1];	
			}
			container[type33][5][i].cluster_pads[0]= 0;
			container[type33][5][i].cluster_pads[1]= 1;
		}
	}

	//padtype 31
	//seg0
	for(int i=0;i<25;i++){
		if(i>=16&&i<=18){
			for(int k=0;k<24;k++){
				container[type31][0][i].th_par[k][0] = par_200GeV_33_01[k][0];	
				container[type31][0][i].th_par[k][1] = par_200GeV_33_01[k][1];	
			}
			container[type31][0][i].cluster_pads[0]= 0;
			container[type31][0][i].cluster_pads[1]= 1;
		}
	}
	//seg1
	for(int i=0;i<25;i++){
		if(i==9){
			for(int k=0;k<24;k++){
				container[type31][1][i].th_par[k][0] = par_200GeV_33_019[k][0];	
				container[type31][1][i].th_par[k][1] = par_200GeV_33_019[k][1];	
			}
			container[type31][1][i].cluster_pads[0]= 0;
			container[type31][1][i].cluster_pads[1]= 1;
			container[type31][1][i].cluster_pads[2]= 2;
		}else if(i>=10&&i<=15){
			for(int k=0;k<24;k++){
				container[type31][1][i].th_par[k][0] = par_200GeV_33_01[k][0];	
				container[type31][1][i].th_par[k][1] = par_200GeV_33_01[k][1];	
			}
			container[type31][1][i].cluster_pads[0]= 0;
			container[type31][1][i].cluster_pads[1]= 1;
		}
	}
	//seg2
	for(int i=0;i<25;i++){
		if(i>=9&&i<=15){
			for(int k=0;k<24;k++){
				container[type31][2][i].th_par[k][0] = par_200GeV_33_09[k][0];	
				container[type31][2][i].th_par[k][1] = par_200GeV_33_09[k][1];	
			}
			container[type31][2][i].cluster_pads[0]= 0;
			container[type31][2][i].cluster_pads[1]= 1;
		}
	}
	//seg3
	for(int i=0;i<25;i++){
		if(i>=9&&i<=15){
			for(int k=0;k<24;k++){
				container[type31][3][i].th_par[k][0] = par_200GeV_33_09[k][0];	
				container[type31][3][i].th_par[k][1] = par_200GeV_33_09[k][1];	
			}
			container[type31][3][i].cluster_pads[0]= 0;
			container[type31][3][i].cluster_pads[1]= 1;
		}
	}
	//seg4
	for(int i=0;i<25;i++){
		if(i==15){
			for(int k=0;k<24;k++){
				container[type31][4][i].th_par[k][0] = par_200GeV_33_019[k][0];	
				container[type31][4][i].th_par[k][1] = par_200GeV_33_019[k][1];	
			}
			container[type31][4][i].cluster_pads[0]= 0;
			container[type31][4][i].cluster_pads[1]= 1;
			container[type31][4][i].cluster_pads[2]= 3;
		}else if(i>=9&&i<=14){
			for(int k=0;k<24;k++){
				container[type31][4][i].th_par[k][0] = par_200GeV_33_01[k][0];	
				container[type31][4][i].th_par[k][1] = par_200GeV_33_01[k][1];	
			}
			container[type31][4][i].cluster_pads[0]= 0;
			container[type31][4][i].cluster_pads[1]= 1;
		}
	}
	//seg 5
	for(int i=0;i<25;i++){
		if(i>=22&&i<=24){
			for(int k=0;k<24;k++){
				container[type31][5][i].th_par[k][0] = par_200GeV_33_01[k][0];	
				container[type31][5][i].th_par[k][1] = par_200GeV_33_01[k][1];	
			}
			container[type31][5][i].cluster_pads[0]= 0;
			container[type31][5][i].cluster_pads[1]= 1;
		}
	}
	//padtype 34
	//seg0
	for(int i=0;i<25;i++){
		if(i>=9&&i<=15){
			for(int k=0;k<24;k++){
				container[type34][0][i].th_par[k][0] = par_200GeV_32_0m8[k][0];	
				container[type34][0][i].th_par[k][1] = par_200GeV_32_0m8[k][1];	
			}
			container[type34][0][i].cluster_pads[0]= 0;
			container[type34][0][i].cluster_pads[1]= 1;
		}
	}
	//seg1
	for(int i=0;i<25;i++){
		if(i==15){
			for(int k=0;k<24;k++){
				container[type34][1][i].th_par[k][0] = par_200GeV_32_01m8[k][0];	
				container[type34][1][i].th_par[k][1] = par_200GeV_32_01m8[k][1];	
			}
			container[type34][1][i].cluster_pads[0]= 0;
			container[type34][1][i].cluster_pads[1]= 1;
			container[type34][1][i].cluster_pads[2]= 3;
		}else if(i>=9&&i<=14){
			for(int k=0;k<24;k++){
				container[type34][1][i].th_par[k][0] = par_200GeV_32_01[k][0];	
				container[type34][1][i].th_par[k][1] = par_200GeV_32_01[k][1];	
			}
			container[type34][1][i].cluster_pads[0]= 0;
			container[type34][1][i].cluster_pads[1]= 1;
		}
	}
	//seg2
	for(int i=0;i<25;i++){
		if(i>=22&&i<=24){
			for(int k=0;k<24;k++){
				container[type34][2][i].th_par[k][0] = par_200GeV_32_01[k][0];	
				container[type34][2][i].th_par[k][1] = par_200GeV_32_01[k][1];	
			}
			container[type34][2][i].cluster_pads[0]= 0;
			container[type34][2][i].cluster_pads[1]= 1;
		}
	}
	//seg3
	for(int i=0;i<25;i++){
		if(i>=16&&i<=18){
			for(int k=0;k<24;k++){
				container[type34][3][i].th_par[k][0] = par_200GeV_32_01[k][0];	
				container[type34][3][i].th_par[k][1] = par_200GeV_32_01[k][1];	
			}
			container[type34][3][i].cluster_pads[0]= 0;
			container[type34][3][i].cluster_pads[1]= 1;
		}
	}
	//seg4
	for(int i=0;i<25;i++){
		if(i==9){
			for(int k=0;k<24;k++){
				container[type34][4][i].th_par[k][0] = par_200GeV_32_01m8[k][0];	
				container[type34][4][i].th_par[k][1] = par_200GeV_32_01m8[k][1];	
			}
			container[type34][4][i].cluster_pads[0]= 0;
			container[type34][4][i].cluster_pads[1]= 1;
			container[type34][4][i].cluster_pads[2]= 2;
		}else if(i>=10&&i<=15){
			for(int k=0;k<24;k++){
				container[type32][4][i].th_par[k][0] = par_200GeV_32_01[k][0];	
				container[type32][4][i].th_par[k][1] = par_200GeV_32_01[k][1];	
			}
			container[type32][4][i].cluster_pads[0]= 0;
			container[type32][4][i].cluster_pads[1]= 1;
		}
	}
	//seg5
	for(int i=0;i<25;i++){
		if(i>=9&&i<=15){
			for(int k=0;k<24;k++){
				container[type32][5][i].th_par[k][0] = par_200GeV_32_0m8[k][0];	
				container[type32][5][i].th_par[k][1] = par_200GeV_32_0m8[k][1];	
			}
			container[type32][5][i].cluster_pads[0]= 0;
			container[type32][5][i].cluster_pads[1]= 1;
		}
	}
	//padtype 39
	for(int i=0;i<6;i++){
		for(int j=0;j<25;j++){
				for(int k=0;k<24;k++){
					container[type39][i][j].th_par[k][0] = container[type32][i][j].th_par[k][0];
					container[type39][i][j].th_par[k][1] = container[type32][i][j].th_par[k][1];
					if(i==5&&j>=22&&j<=24){
						container[type39][i][j].th_par[k][0] = par_200GeV_64_019[k][0];
						container[type39][i][j].th_par[k][1] = par_200GeV_64_019[k][1];
					}
					if(i==4&&j>=9&&j<=14){
						container[type39][i][j].th_par[k][0] = par_200GeV_64_01[k][0];
						container[type39][i][j].th_par[k][1] = par_200GeV_64_01[k][1];
					}
					if(i==3&&j==15){
						container[type39][i][j].th_par[k][0] = par_200GeV_64_01m8[k][0];
						container[type39][i][j].th_par[k][1] = par_200GeV_64_01m8[k][1];
					}
				}
				container[type39][i][j].cluster_pads[0]= container[type32][i][j].cluster_pads[0];
				container[type39][i][j].cluster_pads[1]= container[type32][i][j].cluster_pads[1];
				container[type39][i][j].cluster_pads[2]= container[type32][i][j].cluster_pads[2];
				container[type39][i][j].cluster_pads[3]= container[type32][i][j].cluster_pads[3];
		}
	}
	//padtype 40
	for(int i=0;i<6;i++){
		for(int j=0;j<25;j++){
				for(int k=0;k<24;k++){
					container[type40][i][j].th_par[k][0] = container[type32][i][j].th_par[k][0];
					container[type40][i][j].th_par[k][1] = container[type32][i][j].th_par[k][1];
					if(i==0&&j>=16&&j<=18){
						container[type40][i][j].th_par[k][0] = par_200GeV_63_08m1[k][0];
						container[type40][i][j].th_par[k][1] = par_200GeV_63_08m1[k][1];
					}
					if(i==1&&j>=10&&j<=15){
						container[type40][i][j].th_par[k][0] = par_200GeV_63_0m1[k][0];
						container[type40][i][j].th_par[k][1] = par_200GeV_63_0m1[k][1];
					}
					if(i==2&&j==9){
						container[type40][i][j].th_par[k][0] = par_200GeV_63_0m1m9[k][0];
						container[type40][i][j].th_par[k][1] = par_200GeV_63_0m1m9[k][1];
					}
				}
				container[type40][i][j].cluster_pads[0]= container[type32][i][j].cluster_pads[0];
				container[type40][i][j].cluster_pads[1]= container[type32][i][j].cluster_pads[1];
				container[type40][i][j].cluster_pads[2]= container[type32][i][j].cluster_pads[2];
				container[type40][i][j].cluster_pads[3]= container[type32][i][j].cluster_pads[3];
		}
	}
	//padtype 41
	for(int i=0;i<6;i++){
		for(int j=0;j<25;j++){
				for(int k=0;k<24;k++){
					container[type41][i][j].th_par[k][0] = container[type34][i][j].th_par[k][0];
					container[type41][i][j].th_par[k][1] = container[type34][i][j].th_par[k][1];
					if(i==5&&j>=22&&j<=24){
						container[type41][i][j].th_par[k][0] = par_200GeV_64_019[k][0];
						container[type41][i][j].th_par[k][1] = par_200GeV_64_019[k][1];
					}
					if(i==4&&j>=9&&j<=14){
						container[type41][i][j].th_par[k][0] = par_200GeV_64_01[k][0];
						container[type41][i][j].th_par[k][1] = par_200GeV_64_01[k][1];
					}
				}
				container[type41][i][j].cluster_pads[0]= container[type34][i][j].cluster_pads[0];
				container[type41][i][j].cluster_pads[1]= container[type34][i][j].cluster_pads[1];
				container[type41][i][j].cluster_pads[2]= container[type34][i][j].cluster_pads[2];
				container[type41][i][j].cluster_pads[3]= container[type34][i][j].cluster_pads[3];
		}
	}
	//padtype 42
	for(int i=0;i<6;i++){
		for(int j=0;j<25;j++){
				for(int k=0;k<24;k++){
					container[type42][i][j].th_par[k][0] = container[type34][i][j].th_par[k][0];
					container[type42][i][j].th_par[k][1] = container[type34][i][j].th_par[k][1];
					if(i==0&&j>=16&&j<=18){
						container[type42][i][j].th_par[k][0] = par_200GeV_63_08m1[k][0];
						container[type42][i][j].th_par[k][1] = par_200GeV_63_08m1[k][1];
					}
					if(i==1&&j>=10&&j<=15){
						container[type42][i][j].th_par[k][0] = par_200GeV_63_0m1[k][0];
						container[type42][i][j].th_par[k][1] = par_200GeV_63_0m1[k][1];
					}
				}
				container[type42][i][j].cluster_pads[0]= container[type34][i][j].cluster_pads[0];
				container[type42][i][j].cluster_pads[1]= container[type34][i][j].cluster_pads[1];
				container[type42][i][j].cluster_pads[2]= container[type34][i][j].cluster_pads[2];
				container[type42][i][j].cluster_pads[3]= container[type34][i][j].cluster_pads[3];
		}
	}

	if(verbose)cout<<"YWCutter::Init() end!"<<endl;
}


void YWCutter::Initialize_62GeV(){
	if(verbose)cout<<"YWCutter::Init() begin.."<<endl;
	//initialize with 1pad cluster
	for(int i=0;i<23;i++){
		for(int j=0;j<6;j++){
			for(int k=0;k<25;k++){
				for(int l=0;l<24;l++){
					if(i==type0||i==type1||i==type2||i==type3||i==type4){
						container[i][j][k].th_par[l][0]=par_62GeV_0_0[l][0];
						container[i][j][k].th_par[l][1]=par_62GeV_0_0[l][1];
					}
					if(i==type11){
						container[i][j][k].th_par[l][0]=par_62GeV_11_0[l][0];
						container[i][j][k].th_par[l][1]=par_62GeV_11_0[l][1];
					}
					if(i==type12){
						container[i][j][k].th_par[l][0]=par_62GeV_12_0[l][0];
						container[i][j][k].th_par[l][1]=par_62GeV_12_0[l][1];
					}
					if(i==type31||i==type33||i==type35||i==type36||i==type37||i==type38){
						container[i][j][k].th_par[l][0]=par_62GeV_33_0[l][0];
						container[i][j][k].th_par[l][1]=par_62GeV_33_0[l][1];
					}
					if(i==type32||i==type34 || i==type39 || i==type40 || i==type41||i==type42){
						container[i][j][k].th_par[l][0]=par_62GeV_32_0[l][0];
						container[i][j][k].th_par[l][1]=par_62GeV_32_0[l][1];
					}
					if(i == type64 || i == type62){
						container[i][j][k].th_par[l][0]=par_62GeV_64_0[l][0];
						container[i][j][k].th_par[l][1]=par_62GeV_64_0[l][1];
					}
					if(i==type63||i==type61){
						container[i][j][k].th_par[l][0]=par_62GeV_63_0[l][0];
						container[i][j][k].th_par[l][1]=par_62GeV_63_0[l][1];
					}
				}
				for(int l=0;l<4;l++){
					if(l==0){
						container[i][j][k].cluster_pads[l]=0;
					}else{
						container[i][j][k].cluster_pads[l]=-999;
					}
				}
				container[i][j][k].weight_pad0=1;
			}
		}
	}

	//Fill 1 pad threshold for non-1pad cluster
		//padtype 0
	for(int i=0;i<6;i++){
		for(int j=9;j<25;j++){
			if(j==9){
				for(int k=0;k<24;k++){
					container[type0][i][j].th_par[k][0]=par_62GeV_0_2[k][0];
					container[type0][i][j].th_par[k][1]=par_62GeV_0_2[k][1];
				}
				container[type0][i][j].cluster_pads[0]= 0;
				container[type0][i][j].cluster_pads[1]= 1;
				container[type0][i][j].cluster_pads[2]= 2;
			}else if(j>=10&&j<=14){
				for(int k=0;k<24;k++){
					container[type0][i][j].th_par[k][0]=par_62GeV_0_1[k][0];
					container[type0][i][j].th_par[k][1]=par_62GeV_0_1[k][1];
				}
				container[type0][i][j].cluster_pads[0]= 0;
				container[type0][i][j].cluster_pads[1]= 1;
			}else if(j==15){
				for(int k=0;k<24;k++){
					container[type0][i][j].th_par[k][0]=par_62GeV_0_2[k][0];
					container[type0][i][j].th_par[k][1]=par_62GeV_0_2[k][1];
				}
				container[type0][i][j].cluster_pads[0]= 0;
				container[type0][i][j].cluster_pads[1]= 1;
				container[type0][i][j].cluster_pads[2]= 3;
			}
		}
	}
	//padtype 1
	//seg0
	for(int i=0;i<25;i++){
		if(i==8&&(i>=13&&i<=15)){
			for(int k=0;k<24;k++){
				container[type1][0][i].th_par[k][0] = par_62GeV_1_089[k][0];	
				container[type1][0][i].th_par[k][1] = par_62GeV_1_089[k][1];	
			}
			container[type1][0][i].cluster_pads[0]= 0;
			container[type1][0][i].cluster_pads[1]= 1;
			container[type1][0][i].cluster_pads[2]= 3;
		}
		if(i>=10&&i<=12){
			for(int k=0;k<24;k++){
				container[type1][0][i].th_par[k][0] = par_62GeV_1_01[k][0];	
				container[type1][0][i].th_par[k][1] = par_62GeV_1_01[k][1];	
			}
			container[type1][0][i].cluster_pads[0]= 0;
			container[type1][0][i].cluster_pads[1]= 1;
		}
		if(i==9){
			for(int k=0;k<24;k++){
				container[type1][0][i].th_par[k][0] = par_62GeV_1_019[k][0];	
				container[type1][0][i].th_par[k][1] = par_62GeV_1_019[k][1];	
			}
			container[type1][0][i].cluster_pads[0]= 0;
			container[type1][0][i].cluster_pads[1]= 1;
			container[type1][0][i].cluster_pads[2]= 2;
		}
	}
	//seg1
	for(int i=0;i<25;i++){
		if(i==9){
			for(int k=0;k<24;k++){
				container[type1][1][i].th_par[k][0] = par_62GeV_1_019[k][0];	
				container[type1][1][i].th_par[k][1] = par_62GeV_1_019[k][1];	
			}
			container[type1][1][i].cluster_pads[0]= 0;
			container[type1][1][i].cluster_pads[1]= 1;
			container[type1][1][i].cluster_pads[2]= 2;
		}
		if(i==15){
			for(int k=0;k<24;k++){
				container[type1][1][i].th_par[k][0] = par_62GeV_1_019[k][0];	
				container[type1][1][i].th_par[k][1] = par_62GeV_1_019[k][1];	
			}
			container[type1][1][i].cluster_pads[0]= 0;
			container[type1][1][i].cluster_pads[1]= 1;
			container[type1][1][i].cluster_pads[2]= 3;
		}
		if(i>=10&&i<=14){
			for(int k=0;k<24;k++){
				container[type1][1][i].th_par[k][0] = par_62GeV_1_01[k][0];	
				container[type1][1][i].th_par[k][1] = par_62GeV_1_01[k][1];	
			}
			container[type1][1][i].cluster_pads[0]= 0;
			container[type1][1][i].cluster_pads[1]= 1;
		}
	}
	//seg2
	for(int i=0;i<25;i++){
		if(i==4&&(i>=9&&i<=11)){
			for(int k=0;k<24;k++){
				container[type1][2][i].th_par[k][0] = par_62GeV_1_089[k][0];	
				container[type1][2][i].th_par[k][1] = par_62GeV_1_089[k][1];	
			}
			container[type1][2][i].cluster_pads[0]= 0;
			container[type1][2][i].cluster_pads[1]= 1;
			container[type1][2][i].cluster_pads[2]= 2;
		}
		if(i>=12&&i<=14){
			for(int k=0;k<24;k++){
				container[type1][2][i].th_par[k][0] = par_62GeV_1_01[k][0];	
				container[type1][2][i].th_par[k][1] = par_62GeV_1_01[k][1];	
			}
			container[type1][2][i].cluster_pads[0]= 0;
			container[type1][2][i].cluster_pads[1]= 1;
		}
		if(i==15){
			for(int k=0;k<24;k++){
				container[type1][2][i].th_par[k][0] = par_62GeV_1_019[k][0];	
				container[type1][2][i].th_par[k][1] = par_62GeV_1_019[k][1];	
			}
			container[type1][2][i].cluster_pads[0]= 0;
			container[type1][2][i].cluster_pads[1]= 1;
			container[type1][2][i].cluster_pads[2]= 3;
		}
	}
	//seg3
	for(int i=0;i<25;i++){
		if(!(i==0||i==2)){
			for(int k=0;k<24;k++){
				container[type1][3][i].th_par[k][0] = par_62GeV_1_08[k][0];	
				container[type1][3][i].th_par[k][1] = par_62GeV_1_08[k][1];	
			}
			container[type1][3][i].cluster_pads[0]= 0;
			container[type1][3][i].cluster_pads[1]= 1;
		}
	}
	//seg4
	for(int i=0;i<25;i++){
		if(i==4||i==9||i==10||i==16||i==17||i==18){
			for(int k=0;k<24;k++){
				container[type1][4][i].th_par[k][0] = par_62GeV_1_08[k][0];	
				container[type1][4][i].th_par[k][1] = par_62GeV_1_08[k][1];	
			}
			container[type1][4][i].cluster_pads[0]= 0;
			container[type1][4][i].cluster_pads[1]= 1;
		}
		if(i==20){
			for(int k=0;k<24;k++){
				container[type1][4][i].th_par[k][0] = par_62GeV_1_0m185[k][0];	
				container[type1][4][i].th_par[k][1] = par_62GeV_1_0m185[k][1];	
			}
			container[type1][4][i].cluster_pads[0]= 0;
			container[type1][4][i].cluster_pads[1]= 6;
		}
		if(i==8||i==14||i==15||i==22||i==23||i==24){
			for(int k=0;k<24;k++){
				container[type1][4][i].th_par[k][0] = par_62GeV_1_08[k][0];	
				container[type1][4][i].th_par[k][1] = par_62GeV_1_08[k][1];	
			}
			container[type1][4][i].cluster_pads[0]= 0;
			container[type1][4][i].cluster_pads[1]= 2;
		}
	}
	//seg5
	for(int i=0;i<25;i++){
		if(!(i==0||i==2)){
			for(int k=0;k<24;k++){
				container[type1][5][i].th_par[k][0] = par_62GeV_1_08[k][0];	
				container[type1][5][i].th_par[k][1] = par_62GeV_1_08[k][1];	
			}
			container[type1][5][i].cluster_pads[0]= 0;
			container[type1][5][i].cluster_pads[1]= 1;
		}
	}
	//padtype 2
	//seg0
	for(int i=0;i<25;i++){
		if(!(i==0||i==2)){
			for(int k=0;k<24;k++){
				container[type2][0][i].th_par[k][0] = par_62GeV_2_09[k][0];	
				container[type2][0][i].th_par[k][1] = par_62GeV_2_09[k][1];	
			}
			container[type2][0][i].cluster_pads[0]= 0;
			container[type2][0][i].cluster_pads[1]= 1;
		}
	}
	//seg1
	for(int i=0;i<25;i++){
		if(i==4||i==9||i==10||i==16||i==17||i==18){
			for(int k=0;k<24;k++){
				container[type2][1][i].th_par[k][0] = par_62GeV_2_09[k][0];	
				container[type2][1][i].th_par[k][1] = par_62GeV_2_09[k][1];	
			}
			container[type2][1][i].cluster_pads[0]= 0;
			container[type2][1][i].cluster_pads[1]= 1;
		}
		if(i==20){
			for(int k=0;k<24;k++){
				container[type2][1][i].th_par[k][0] = par_62GeV_2_0185[k][0];	
				container[type2][1][i].th_par[k][1] = par_62GeV_2_0185[k][1];	
			}
			container[type2][1][i].cluster_pads[0]= 0;
			container[type2][1][i].cluster_pads[1]= 6;
		}
		if(i==8||i==14||i==15||i==22||i==23||i==24){
			for(int k=0;k<24;k++){
				container[type2][1][i].th_par[k][0] = par_62GeV_2_09[k][0];	
				container[type2][1][i].th_par[k][1] = par_62GeV_2_09[k][1];	
			}
			container[type2][1][i].cluster_pads[0]= 0;
			container[type2][1][i].cluster_pads[1]= 2;
		}
	}
	//seg2
	for(int i=0;i<25;i++){
		if(!(i==0||i==2)){
			for(int k=0;k<24;k++){
				container[type2][2][i].th_par[k][0] = par_62GeV_2_09[k][0];	
				container[type2][2][i].th_par[k][1] = par_62GeV_2_09[k][1];	
			}
			container[type2][2][i].cluster_pads[0]= 0;
			container[type2][2][i].cluster_pads[1]= 1;
		}
	}
	//seg3
	for(int i=0;i<25;i++){
		if(i==8&&(i>=13&&i<=15)){
			for(int k=0;k<24;k++){
				container[type2][3][i].th_par[k][0] = par_62GeV_2_089[k][0];	
				container[type2][3][i].th_par[k][1] = par_62GeV_2_089[k][1];	
			}
			container[type2][3][i].cluster_pads[0]= 0;
			container[type2][3][i].cluster_pads[1]= 1;
			container[type2][3][i].cluster_pads[1]= 3;
		}
		if(i>=10&&i<=12){
			for(int k=0;k<24;k++){
				container[type2][3][i].th_par[k][0] = par_62GeV_2_0m1[k][0];	
				container[type2][3][i].th_par[k][1] = par_62GeV_2_0m1[k][1];	
			}
			container[type2][3][i].cluster_pads[0]= 0;
			container[type2][3][i].cluster_pads[1]= 1;
		}
		if(i==9){
			for(int k=0;k<24;k++){
				container[type2][3][i].th_par[k][0] = par_62GeV_2_0m18[k][0];	
				container[type2][3][i].th_par[k][1] = par_62GeV_2_0m18[k][1];	
			}
			container[type2][3][i].cluster_pads[0]= 0;
			container[type2][3][i].cluster_pads[1]= 1;
			container[type2][3][i].cluster_pads[2]= 2;
		}
	}
	//seg4
	for(int i=0;i<25;i++){
		if(i==9){
			for(int k=0;k<24;k++){
				container[type2][4][i].th_par[k][0] = par_62GeV_2_0m18[k][0];	
				container[type2][4][i].th_par[k][1] = par_62GeV_2_0m18[k][1];	
			}
			container[type2][4][i].cluster_pads[0]= 0;
			container[type2][4][i].cluster_pads[1]= 1;
			container[type2][4][i].cluster_pads[2]= 2;
		}
		if(i==15){
			for(int k=0;k<24;k++){
				container[type2][4][i].th_par[k][0] = par_62GeV_2_0m18[k][0];	
				container[type2][4][i].th_par[k][1] = par_62GeV_2_0m18[k][1];	
			}
			container[type2][4][i].cluster_pads[0]= 0;
			container[type2][4][i].cluster_pads[1]= 1;
			container[type2][4][i].cluster_pads[2]= 3;
		}
		if(i>=10&&i<=14){
			for(int k=0;k<24;k++){
				container[type2][4][i].th_par[k][0] = par_62GeV_2_0m1[k][0];	
				container[type2][4][i].th_par[k][1] = par_62GeV_2_0m1[k][1];	
			}
			container[type2][4][i].cluster_pads[0]= 0;
			container[type2][4][i].cluster_pads[1]= 1;
		}
	}
	//seg5
	for(int i=0;i<25;i++){
		if(i==4&&(i>=9&&i<=11)){
			for(int k=0;k<24;k++){
				container[type2][5][i].th_par[k][0] = par_62GeV_2_089[k][0];	
				container[type2][5][i].th_par[k][1] = par_62GeV_2_089[k][1];	
			}
			container[type2][5][i].cluster_pads[0]= 0;
			container[type2][5][i].cluster_pads[1]= 1;
			container[type2][5][i].cluster_pads[2]= 2;
		}
		if(i>=12&&i<=14){
			for(int k=0;k<24;k++){
				container[type2][5][i].th_par[k][0] = par_62GeV_2_0m1[k][0];	
				container[type2][5][i].th_par[k][1] = par_62GeV_2_0m1[k][1];	
			}
			container[type2][5][i].cluster_pads[0]= 0;
			container[type2][5][i].cluster_pads[1]= 1;
		}
		if(i==15){
			for(int k=0;k<24;k++){
				container[type2][5][i].th_par[k][0] = par_62GeV_2_0m18[k][0];	
				container[type2][5][i].th_par[k][1] = par_62GeV_2_0m18[k][1];	
			}
			container[type2][5][i].cluster_pads[0]= 0;
			container[type2][5][i].cluster_pads[1]= 1;
			container[type2][5][i].cluster_pads[2]= 3;
		}
	}
	//padtype 3
	//seg 0-2
	for(int i=0;i<3;i++){
		for(int j=9;j<25;j++){
			if(j==9){
				for(int k=0;k<24;k++){
					container[type3][i][j].th_par[k][0]=par_62GeV_0_2[k][0];
					container[type3][i][j].th_par[k][1]=par_62GeV_0_2[k][1];
				}
				container[type3][i][j].cluster_pads[0]= 0;
				container[type3][i][j].cluster_pads[1]= 1;
				container[type3][i][j].cluster_pads[2]= 2;
			}else if(j>=10&&j<=14){
				for(int k=0;k<24;k++){
					container[type3][i][j].th_par[k][0]=par_62GeV_0_1[k][0];
					container[type3][i][j].th_par[k][1]=par_62GeV_0_1[k][1];
				}
				container[type3][i][j].cluster_pads[0]= 0;
				container[type3][i][j].cluster_pads[1]= 1;
			}else if(j==15){
				for(int k=0;k<24;k++){
					container[type3][i][j].th_par[k][0]=par_62GeV_0_2[k][0];
					container[type3][i][j].th_par[k][1]=par_62GeV_0_2[k][1];
				}
				container[type3][i][j].cluster_pads[0]= 0;
				container[type3][i][j].cluster_pads[1]= 1;
				container[type3][i][j].cluster_pads[2]= 3;
			}
		}
	}
	//seg3
	for(int i=0;i<25;i++){
		if(i==4||i==9){
			for(int k=0;k<24;k++){
				container[type3][3][i].th_par[k][0] = par_62GeV_12_09[k][0];	
				container[type3][3][i].th_par[k][1] = par_62GeV_12_09[k][1];	
			}
			container[type3][3][i].cluster_pads[0]= 0;
			container[type3][3][i].cluster_pads[1]= 2;
		}else if(i>=10&&i<=12){
			for(int k=0;k<24;k++){
				container[type3][3][i].th_par[k][0] = par_62GeV_12_019[k][0];	
				container[type3][3][i].th_par[k][1] = par_62GeV_12_019[k][1];	
			}
			container[type3][3][i].cluster_pads[0]= 0;
			container[type3][3][i].cluster_pads[1]= 1;
			container[type3][3][i].cluster_pads[2]= 2;
		}else if(i>=13&&i<=14){
			for(int k=0;k<24;k++){
				container[type3][3][i].th_par[k][0] = par_62GeV_0_1[k][0];	
				container[type3][3][i].th_par[k][1] = par_62GeV_0_1[k][1];	
			}
			container[type3][3][i].cluster_pads[0]= 0;
			container[type3][3][i].cluster_pads[1]= 1;
		}else if(i==15){
			for(int k=0;k<24;k++){
				container[type3][3][i].th_par[k][0] = par_62GeV_0_2[k][0];	
				container[type3][3][i].th_par[k][1] = par_62GeV_0_2[k][1];	
			}
			container[type3][3][i].cluster_pads[0]= 0;
			container[type3][3][i].cluster_pads[1]= 1;
			container[type3][3][i].cluster_pads[2]= 3;
		}
	}
	//seg4
	for(int i=0;i<25;i++){
		if(i>=1){
			for(int k=0;k<24;k++){
				container[type3][4][i].th_par[k][0] = par_62GeV_12_09[k][0];	
				container[type3][4][i].th_par[k][1] = par_62GeV_12_09[k][1];	
			}
			container[type3][4][i].cluster_pads[0]= 0;
			container[type3][4][i].cluster_pads[1]= 1;
		}
	}
	//seg5
	for(int i=0;i<25;i++){
		if(i==8||i==15){
			for(int k=0;k<24;k++){
				container[type3][5][i].th_par[k][0] = par_62GeV_12_09[k][0];	
				container[type3][5][i].th_par[k][1] = par_62GeV_12_09[k][1];	
			}
			container[type3][5][i].cluster_pads[0]= 0;
			container[type3][5][i].cluster_pads[1]= 3;
		}else if(i>=12&&i<=14){
			for(int k=0;k<24;k++){
				container[type3][5][i].th_par[k][0] = par_62GeV_12_019[k][0];	
				container[type3][5][i].th_par[k][1] = par_62GeV_12_019[k][1];	
			}
			container[type3][5][i].cluster_pads[0]= 0;
			container[type3][5][i].cluster_pads[1]= 1;
			container[type3][5][i].cluster_pads[2]= 3;
		}else if(i>=10&&i<=11){
			for(int k=0;k<24;k++){
				container[type3][5][i].th_par[k][0] = par_62GeV_0_1[k][0];	
				container[type3][5][i].th_par[k][1] = par_62GeV_0_1[k][1];	
			}
			container[type3][5][i].cluster_pads[0]= 0;
			container[type3][5][i].cluster_pads[1]= 1;
		}else if(i==9){
			for(int k=0;k<24;k++){
				container[type3][5][i].th_par[k][0] = par_62GeV_0_2[k][0];	
				container[type3][5][i].th_par[k][1] = par_62GeV_0_2[k][1];	
			}
			container[type3][5][i].cluster_pads[0]= 0;
			container[type3][5][i].cluster_pads[1]= 1;
			container[type3][5][i].cluster_pads[2]= 2;
		}
	}
	//padtype 4
	//seg 3-5
	for(int i=3;i<6;i++){
		for(int j=9;j<25;j++){
			if(j==9){
				for(int k=0;k<24;k++){
					container[type4][i][j].th_par[k][0]=par_62GeV_0_2[k][0];
					container[type4][i][j].th_par[k][1]=par_62GeV_0_2[k][1];
				}
				container[type4][i][j].cluster_pads[0]= 0;
				container[type4][i][j].cluster_pads[1]= 1;
				container[type4][i][j].cluster_pads[2]= 2;
			}else if(j>=10&&j<=14){
				for(int k=0;k<24;k++){
					container[type4][i][j].th_par[k][0]=par_62GeV_0_1[k][0];
					container[type4][i][j].th_par[k][1]=par_62GeV_0_1[k][1];
				}
				container[type4][i][j].cluster_pads[0]= 0;
				container[type4][i][j].cluster_pads[1]= 1;
			}else if(j==15){
				for(int k=0;k<24;k++){
					container[type4][i][j].th_par[k][0]=par_62GeV_0_2[k][0];
					container[type4][i][j].th_par[k][1]=par_62GeV_0_2[k][1];
				}
				container[type4][i][j].cluster_pads[0]= 0;
				container[type4][i][j].cluster_pads[1]= 1;
				container[type4][i][j].cluster_pads[2]= 3;
			}
		}
	}
	//seg0
	for(int i=0;i<25;i++){
		if(i==4||i==9){
			for(int k=0;k<24;k++){
				container[type4][0][i].th_par[k][0] = par_62GeV_11_08[k][0];	
				container[type4][0][i].th_par[k][1] = par_62GeV_11_08[k][1];	
			}
			container[type4][0][i].cluster_pads[0]= 0;
			container[type4][0][i].cluster_pads[1]= 2;
		}else if(i>=10&&i<=12){
			for(int k=0;k<24;k++){
				container[type4][0][i].th_par[k][0] = par_62GeV_11_0m18[k][0];	
				container[type4][0][i].th_par[k][1] = par_62GeV_11_0m18[k][1];	
			}
			container[type4][0][i].cluster_pads[0]= 0;
			container[type4][0][i].cluster_pads[1]= 1;
			container[type4][0][i].cluster_pads[2]= 2;
		}else if(i>=13&&i<=14){
			for(int k=0;k<24;k++){
				container[type4][0][i].th_par[k][0] = par_62GeV_0_1[k][0];	
				container[type4][0][i].th_par[k][1] = par_62GeV_0_1[k][1];	
			}
			container[type4][0][i].cluster_pads[0]= 0;
			container[type4][0][i].cluster_pads[1]= 1;
		}else if(i==15){
			for(int k=0;k<24;k++){
				container[type4][0][i].th_par[k][0] = par_62GeV_0_2[k][0];	
				container[type4][0][i].th_par[k][1] = par_62GeV_0_2[k][1];	
			}
			container[type4][0][i].cluster_pads[0]= 0;
			container[type4][0][i].cluster_pads[1]= 1;
			container[type4][0][i].cluster_pads[2]= 3;
		}
	}
	//seg1
	for(int i=0;i<25;i++){
		if(i>=1){
			for(int k=0;k<24;k++){
				container[type4][4][i].th_par[k][0] = par_62GeV_11_08[k][0];	
				container[type4][4][i].th_par[k][1] = par_62GeV_11_08[k][1];	
			}
			container[type4][4][i].cluster_pads[0]= 0;
			container[type4][4][i].cluster_pads[1]= 1;
		}
	}
	//seg2
	for(int i=0;i<25;i++){
		if(i==8||i==15){
			for(int k=0;k<24;k++){
				container[type4][0][i].th_par[k][0] = par_62GeV_11_08[k][0];	
				container[type4][0][i].th_par[k][1] = par_62GeV_11_08[k][1];	
			}
			container[type4][0][i].cluster_pads[0]= 0;
			container[type4][0][i].cluster_pads[1]= 3;
		}else if(i>=12&&i<=14){
			for(int k=0;k<24;k++){
				container[type4][0][i].th_par[k][0] = par_62GeV_11_0m18[k][0];	
				container[type4][0][i].th_par[k][1] = par_62GeV_11_0m18[k][1];	
			}
			container[type4][0][i].cluster_pads[0]= 0;
			container[type4][0][i].cluster_pads[1]= 1;
			container[type4][0][i].cluster_pads[2]= 3;
		}else if(i>=10&&i<=11){
			for(int k=0;k<24;k++){
				container[type4][0][i].th_par[k][0] = par_62GeV_0_1[k][0];	
				container[type4][0][i].th_par[k][1] = par_62GeV_0_1[k][1];	
			}
			container[type4][0][i].cluster_pads[0]= 0;
			container[type4][0][i].cluster_pads[1]= 1;
		}else if(i==9){
			for(int k=0;k<24;k++){
				container[type4][0][i].th_par[k][0] = par_62GeV_0_2[k][0];	
				container[type4][0][i].th_par[k][1] = par_62GeV_0_2[k][1];	
			}
			container[type4][0][i].cluster_pads[0]= 0;
			container[type4][0][i].cluster_pads[1]= 1;
			container[type4][0][i].cluster_pads[2]= 2;
		}
	}
	//padtype 11
	//seg0
	for(int i=0;i<25;i++){
		if(i==1||i==5||i==6||i==12||i==13||i==21||i==22||i==4||i==10||i==11||i==19||i==20){
			for(int k=0;k<24;k++){
				container[type11][0][i].th_par[k][0] = par_62GeV_11_0184[k][0];	
				container[type11][0][i].th_par[k][1] = par_62GeV_11_0184[k][1];	
			}
			container[type11][0][i].cluster_pads[0]= 0;
			container[type11][0][i].cluster_pads[1]= 4;
		}
	}
	//seg1
	for(int i=0;i<25;i++){
		if(i>=1){
			for(int k=0;k<24;k++){
				container[type11][1][i].th_par[k][0] = par_62GeV_11_0184[k][0];	
				container[type11][1][i].th_par[k][1] = par_62GeV_11_0184[k][1];	
			}
			container[type11][1][i].cluster_pads[0]= 0;
			container[type11][1][i].cluster_pads[1]= 4;
		}
	}
	//seg2
	for(int i=0;i<25;i++){
		if(i==3||i==6||i==7||i==11||i==12||i==18||i==19||i==8||i==13||i==14||i==20||i==21){
			for(int k=0;k<24;k++){
				container[type11][2][i].th_par[k][0] = par_62GeV_11_0184[k][0];	
				container[type11][2][i].th_par[k][1] = par_62GeV_11_0184[k][1];	
			}
			container[type11][2][i].cluster_pads[0]= 0;
			container[type11][2][i].cluster_pads[1]= 4;
		}
	}
	//seg3
	for(int i=0;i<25;i++){
		if(i==8||i==6||(i>=9&&i<=15)){
			for(int k=0;k<24;k++){
				container[type11][3][i].th_par[k][0] = par_62GeV_11_08[k][0];	
				container[type11][3][i].th_par[k][1] = par_62GeV_11_08[k][1];	
			}
			container[type11][3][i].cluster_pads[0]= 0;
			container[type11][3][i].cluster_pads[1]= 1;
		}
	}
	//seg4
	for(int i=0;i<25;i++){
		if(i==9){
			for(int k=0;k<24;k++){
				container[type11][4][i].th_par[k][0] = par_62GeV_11_0m18[k][0];	
				container[type11][4][i].th_par[k][1] = par_62GeV_11_0m18[k][1];	
			}
			container[type11][4][i].cluster_pads[0]= 0;
			container[type11][4][i].cluster_pads[1]= 1;
			container[type11][4][i].cluster_pads[2]= 2;
		}
		if(i==15){
			for(int k=0;k<24;k++){
				container[type11][4][i].th_par[k][0] = par_62GeV_11_0m18[k][0];	
				container[type11][4][i].th_par[k][1] = par_62GeV_11_0m18[k][1];	
			}
			container[type11][4][i].cluster_pads[0]= 0;
			container[type11][4][i].cluster_pads[1]= 1;
			container[type11][4][i].cluster_pads[2]= 3;
		}
		if(i==11||i==13){
			for(int k=0;k<24;k++){
				container[type11][4][i].th_par[k][0] = par_62GeV_11_08[k][0];	
				container[type11][4][i].th_par[k][1] = par_62GeV_11_08[k][1];	
			}
			container[type11][4][i].cluster_pads[0]= 0;
			container[type11][4][i].cluster_pads[1]= 1;
		}
	}
	//seg5
	for(int i=0;i<25;i++){
		if(i==4||i==6||(i>=9&&i<=15)){
			for(int k=0;k<24;k++){
				container[type11][5][i].th_par[k][0] = par_62GeV_11_08[k][0];	
				container[type11][5][i].th_par[k][1] = par_62GeV_11_08[k][1];	
			}
			container[type11][5][i].cluster_pads[0]= 0;
			container[type11][5][i].cluster_pads[1]= 1;
		}
	}
	//padtype 12
	//seg0
	for(int i=0;i<25;i++){
		if(i==8||i==6||(i>=9&&i<=15)){
			for(int k=0;k<24;k++){
				container[type12][0][i].th_par[k][0] = par_62GeV_12_09[k][0];	
				container[type12][0][i].th_par[k][1] = par_62GeV_12_09[k][1];	
			}
			container[type12][0][i].cluster_pads[0]= 0;
			container[type12][0][i].cluster_pads[1]= 1;
		}
	}
	//seg1
	for(int i=0;i<25;i++){
		if(i==9){
			for(int k=0;k<24;k++){
				container[type12][1][i].th_par[k][0] = par_62GeV_12_019[k][0];	
				container[type12][1][i].th_par[k][1] = par_62GeV_12_019[k][1];	
			}
			container[type12][1][i].cluster_pads[0]= 0;
			container[type12][1][i].cluster_pads[1]= 1;
			container[type12][1][i].cluster_pads[0]= 2;
		}
		if(i==15){
			for(int k=0;k<24;k++){
				container[type12][1][i].th_par[k][0] = par_62GeV_12_019[k][0];	
				container[type12][1][i].th_par[k][1] = par_62GeV_12_019[k][1];	
			}
			container[type12][1][i].cluster_pads[0]= 0;
			container[type12][1][i].cluster_pads[1]= 1;
			container[type12][1][i].cluster_pads[2]= 3;
		}
		if(i==11||i==13){
			for(int k=0;k<24;k++){
				container[type12][1][i].th_par[k][0] = par_62GeV_12_09[k][0];	
				container[type12][1][i].th_par[k][1] = par_62GeV_12_09[k][1];	
			}
			container[type12][1][i].cluster_pads[0]= 0;
			container[type12][1][i].cluster_pads[1]= 1;
		}
	}
	//seg2
	for(int i=0;i<25;i++){
		if(i==4||i==6||(i>=9&&i<=15)){
			for(int k=0;k<24;k++){
				container[type12][2][i].th_par[k][0] = par_62GeV_12_09[k][0];	
				container[type12][2][i].th_par[k][1] = par_62GeV_12_09[k][1];	
			}
			container[type12][2][i].cluster_pads[0]= 0;
			container[type12][2][i].cluster_pads[1]= 1;
		}
	}
	//seg3
	for(int i=0;i<25;i++){
		if(i==1||i==5||i==6||i==12||i==13||i==21||i==22||i==4||i==10||i==11||i==19||i==20){
			for(int k=0;k<24;k++){
				container[type12][3][i].th_par[k][0] = par_62GeV_12_0m184[k][0];	
				container[type12][3][i].th_par[k][1] = par_62GeV_12_0m184[k][1];	
			}
			container[type12][3][i].cluster_pads[0]= 0;
			container[type12][3][i].cluster_pads[1]= 4;
		}
	}
	//seg4
	for(int i=0;i<25;i++){
		if(i>=1){
			for(int k=0;k<24;k++){
				container[type12][4][i].th_par[k][0] = par_62GeV_12_0m184[k][0];	
				container[type12][4][i].th_par[k][1] = par_62GeV_12_0m184[k][1];	
			}
			container[type12][4][i].cluster_pads[0]= 0;
			container[type12][4][i].cluster_pads[1]= 4;
		}
	}
	//seg5
	for(int i=0;i<25;i++){
		if(i==3||i==6||i==7||i==11||i==12||i==18||i==19||i==8||i==14||i==13||i==20||i==21){
			for(int k=0;k<24;k++){
				container[type12][3][i].th_par[k][0] = par_62GeV_12_0m184[k][0];	
				container[type12][3][i].th_par[k][1] = par_62GeV_12_0m184[k][1];	
			}
			container[type12][3][i].cluster_pads[0]= 0;
			container[type12][3][i].cluster_pads[1]= 4;
		}
	}
	//padtype 32
	//seg0
	for(int i=0;i<25;i++){
		if(i>=16&&i<=18){
			for(int k=0;k<24;k++){
				container[type32][0][i].th_par[k][0] = par_62GeV_32_019[k][0];	
				container[type32][0][i].th_par[k][1] = par_62GeV_32_019[k][1];	
			}
			container[type32][0][i].cluster_pads[0]= 0;
			container[type32][0][i].cluster_pads[1]= 1;
			container[type32][0][i].cluster_pads[2]= 2;
		}else if(i>=20&&i<=23){
			for(int k=0;k<24;k++){
				container[type32][0][i].th_par[k][0] = par_62GeV_32_09[k][0];	
				container[type32][0][i].th_par[k][1] = par_62GeV_32_09[k][1];	
			}
			container[type32][0][i].cluster_pads[0]= 0;
			container[type32][0][i].cluster_pads[1]= 1;
		}else if(i==24){
			for(int k=0;k<24;k++){
				container[type32][0][i].th_par[k][0] = par_62GeV_32_089[k][0];	
				container[type32][0][i].th_par[k][1] = par_62GeV_32_089[k][1];	
			}
			container[type32][0][i].cluster_pads[0]= 0;
			container[type32][0][i].cluster_pads[1]= 1;
			container[type32][0][i].cluster_pads[2]= 3;
		}
	}
	//seg1
	for(int i=0;i<25;i++){
		if(i==9){
			for(int k=0;k<24;k++){
				container[type32][1][i].th_par[k][0] = par_62GeV_32_01m8[k][0];	
				container[type32][1][i].th_par[k][1] = par_62GeV_32_01m8[k][1];	
			}
			container[type32][1][i].cluster_pads[0]= 0;
			container[type32][1][i].cluster_pads[1]= 1;
			container[type32][1][i].cluster_pads[2]= 2;
		}else if(i>=10&&i<=15){
			for(int k=0;k<24;k++){
				container[type32][1][i].th_par[k][0] = par_62GeV_32_01[k][0];	
				container[type32][1][i].th_par[k][1] = par_62GeV_32_01[k][1];	
			}
			container[type32][1][i].cluster_pads[0]= 0;
			container[type32][1][i].cluster_pads[1]= 1;
		}
	}
	//seg2
	for(int i=0;i<25;i++){
		if(i>=9&&i<=15){
			for(int k=0;k<24;k++){
				container[type32][2][i].th_par[k][0] = par_62GeV_32_0m8[k][0];	
				container[type32][2][i].th_par[k][1] = par_62GeV_32_0m8[k][1];	
			}
			container[type32][2][i].cluster_pads[0]= 0;
			container[type32][2][i].cluster_pads[1]= 1;
		}
	}
	//seg3
	for(int i=0;i<25;i++){
		if(i>=9&&i<=15){
			for(int k=0;k<24;k++){
				container[type32][3][i].th_par[k][0] = par_62GeV_32_0m8[k][0];	
				container[type32][3][i].th_par[k][1] = par_62GeV_32_0m8[k][1];	
			}
			container[type32][3][i].cluster_pads[0]= 0;
			container[type32][3][i].cluster_pads[1]= 1;
		}
	}
	//seg4
	for(int i=0;i<25;i++){
		if(i==15){
			for(int k=0;k<24;k++){
				container[type32][4][i].th_par[k][0] = par_62GeV_32_01m8[k][0];	
				container[type32][4][i].th_par[k][1] = par_62GeV_32_01m8[k][1];	
			}
			container[type32][4][i].cluster_pads[0]= 0;
			container[type32][4][i].cluster_pads[1]= 1;
			container[type32][4][i].cluster_pads[2]= 3;
		}else if(i>=9&&i<=14){
			for(int k=0;k<24;k++){
				container[type32][4][i].th_par[k][0] = par_62GeV_32_01[k][0];	
				container[type32][4][i].th_par[k][1] = par_62GeV_32_01[k][1];	
			}
			container[type32][4][i].cluster_pads[0]= 0;
			container[type32][4][i].cluster_pads[1]= 1;
		}
	}
	//seg 5
	for(int i=0;i<25;i++){
		if(i>=22&&i<=24){
			for(int k=0;k<24;k++){
				container[type32][5][i].th_par[k][0] = par_62GeV_32_019[k][0];	
				container[type32][5][i].th_par[k][1] = par_62GeV_32_019[k][1];	
			}
			container[type32][5][i].cluster_pads[0]= 0;
			container[type32][5][i].cluster_pads[1]= 1;
			container[type32][5][i].cluster_pads[2]= 3;
		}else if(i>=17&&i<=20){
			for(int k=0;k<24;k++){
				container[type32][5][i].th_par[k][0] = par_62GeV_32_09[k][0];	
				container[type32][5][i].th_par[k][1] = par_62GeV_32_09[k][1];	
			}
			container[type32][5][i].cluster_pads[0]= 0;
			container[type32][5][i].cluster_pads[1]= 1;
		}else if(i==16){
			for(int k=0;k<24;k++){
				container[type32][5][i].th_par[k][0] = par_62GeV_32_089[k][0];	
				container[type32][5][i].th_par[k][1] = par_62GeV_32_089[k][1];	
			}
			container[type32][5][i].cluster_pads[0]= 0;
			container[type32][5][i].cluster_pads[1]= 1;
			container[type32][5][i].cluster_pads[2]= 2;
		}
	}
	//padtype 33
	//seg0
	for(int i=0;i<25;i++){
		if(i>=9&&i<=15){
			for(int k=0;k<24;k++){
				container[type33][0][i].th_par[k][0] = par_62GeV_33_09[k][0];	
				container[type33][0][i].th_par[k][1] = par_62GeV_33_09[k][1];	
			}
			container[type33][0][i].cluster_pads[0]= 0;
			container[type33][0][i].cluster_pads[1]= 1;
		}
	}
	//seg1
	for(int i=0;i<25;i++){
		if(i==15){
			for(int k=0;k<24;k++){
				container[type33][1][i].th_par[k][0] = par_62GeV_33_019[k][0];	
				container[type33][1][i].th_par[k][1] = par_62GeV_33_019[k][1];	
			}
			container[type33][1][i].cluster_pads[0]= 0;
			container[type33][1][i].cluster_pads[1]= 1;
			container[type33][1][i].cluster_pads[2]= 3;
		}else if(i>=9&&i<=14){
			for(int k=0;k<24;k++){
				container[type33][1][i].th_par[k][0] = par_62GeV_33_01[k][0];	
				container[type33][1][i].th_par[k][1] = par_62GeV_33_01[k][1];	
			}
			container[type33][1][i].cluster_pads[0]= 0;
			container[type33][1][i].cluster_pads[1]= 1;
		}
	}
	//seg2
	for(int i=0;i<25;i++){
		if(i==16){
			for(int k=0;k<24;k++){
				container[type33][2][i].th_par[k][0] = par_62GeV_33_0m9m8[k][0];	
				container[type33][2][i].th_par[k][1] = par_62GeV_33_0m9m8[k][1];	
			}
			container[type33][2][i].cluster_pads[0]= 0;
			container[type33][2][i].cluster_pads[1]= 1;
			container[type33][2][i].cluster_pads[2]= 2;
		}else if(i>=17&&i<=20){
			for(int k=0;k<24;k++){
				container[type33][2][i].th_par[k][0] = par_62GeV_33_0m8[k][0];	
				container[type33][2][i].th_par[k][1] = par_62GeV_33_0m8[k][1];	
			}
			container[type33][2][i].cluster_pads[0]= 0;
			container[type33][2][i].cluster_pads[1]= 1;
		}else if(i>=22&&i<=24){
			for(int k=0;k<24;k++){
				container[type33][2][i].th_par[k][0] = par_62GeV_33_01m8[k][0];	
				container[type33][2][i].th_par[k][1] = par_62GeV_33_01m8[k][1];	
			}
			container[type33][2][i].cluster_pads[0]= 0;
			container[type33][2][i].cluster_pads[1]= 1;
			container[type33][2][i].cluster_pads[2]= 3;
		}
	}
	//seg3
	for(int i=0;i<25;i++){
		if(i>=16&&i<=18){
			for(int k=0;k<24;k++){
				container[type33][3][i].th_par[k][0] = par_62GeV_33_01m8[k][0];	
				container[type33][3][i].th_par[k][1] = par_62GeV_33_01m8[k][1];	
			}
			container[type33][3][i].cluster_pads[0]= 0;
			container[type33][3][i].cluster_pads[1]= 1;
			container[type33][3][i].cluster_pads[2]= 2;
		}else if(i>=20&&i<=23){
			for(int k=0;k<24;k++){
				container[type33][3][i].th_par[k][0] = par_62GeV_33_0m8[k][0];	
				container[type33][3][i].th_par[k][1] = par_62GeV_33_0m8[k][1];	
			}
			container[type33][3][i].cluster_pads[0]= 0;
			container[type33][3][i].cluster_pads[1]= 1;
		}else if(i==24){
			for(int k=0;k<24;k++){
				container[type33][3][i].th_par[k][0] = par_62GeV_33_0m9m8[k][0];	
				container[type33][3][i].th_par[k][1] = par_62GeV_33_0m9m8[k][1];	
			}
			container[type33][3][i].cluster_pads[0]= 0;
			container[type33][3][i].cluster_pads[1]= 1;
			container[type33][3][i].cluster_pads[2]= 3;
		}
	}
	//seg4
	for(int i=0;i<25;i++){
		if(i==9){
			for(int k=0;k<24;k++){
				container[type33][4][i].th_par[k][0] = par_62GeV_33_019[k][0];	
				container[type33][4][i].th_par[k][1] = par_62GeV_33_019[k][1];	
			}
			container[type33][4][i].cluster_pads[0]= 0;
			container[type33][4][i].cluster_pads[1]= 1;
			container[type33][4][i].cluster_pads[2]= 2;
		}else if(i>=10&&i<=15){
			for(int k=0;k<24;k++){
				container[type33][4][i].th_par[k][0] = par_62GeV_33_01[k][0];	
				container[type33][4][i].th_par[k][1] = par_62GeV_33_01[k][1];	
			}
			container[type33][4][i].cluster_pads[0]= 0;
			container[type33][4][i].cluster_pads[1]= 1;
		}
	}
	//seg5
	for(int i=0;i<25;i++){
		if(i>=9&&i<=15){
			for(int k=0;k<24;k++){
				container[type33][5][i].th_par[k][0] = par_62GeV_33_09[k][0];	
				container[type33][5][i].th_par[k][1] = par_62GeV_33_09[k][1];	
			}
			container[type33][5][i].cluster_pads[0]= 0;
			container[type33][5][i].cluster_pads[1]= 1;
		}
	}

	//padtype 31
	//seg0
	for(int i=0;i<25;i++){
		if(i>=16&&i<=18){
			for(int k=0;k<24;k++){
				container[type31][0][i].th_par[k][0] = par_62GeV_33_01[k][0];	
				container[type31][0][i].th_par[k][1] = par_62GeV_33_01[k][1];	
			}
			container[type31][0][i].cluster_pads[0]= 0;
			container[type31][0][i].cluster_pads[1]= 1;
		}
	}
	//seg1
	for(int i=0;i<25;i++){
		if(i==9){
			for(int k=0;k<24;k++){
				container[type31][1][i].th_par[k][0] = par_62GeV_33_019[k][0];	
				container[type31][1][i].th_par[k][1] = par_62GeV_33_019[k][1];	
			}
			container[type31][1][i].cluster_pads[0]= 0;
			container[type31][1][i].cluster_pads[1]= 1;
			container[type31][1][i].cluster_pads[2]= 2;
		}else if(i>=10&&i<=15){
			for(int k=0;k<24;k++){
				container[type31][1][i].th_par[k][0] = par_62GeV_33_01[k][0];	
				container[type31][1][i].th_par[k][1] = par_62GeV_33_01[k][1];	
			}
			container[type31][1][i].cluster_pads[0]= 0;
			container[type31][1][i].cluster_pads[1]= 1;
		}
	}
	//seg2
	for(int i=0;i<25;i++){
		if(i>=9&&i<=15){
			for(int k=0;k<24;k++){
				container[type31][2][i].th_par[k][0] = par_62GeV_33_09[k][0];	
				container[type31][2][i].th_par[k][1] = par_62GeV_33_09[k][1];	
			}
			container[type31][2][i].cluster_pads[0]= 0;
			container[type31][2][i].cluster_pads[1]= 1;
		}
	}
	//seg3
	for(int i=0;i<25;i++){
		if(i>=9&&i<=15){
			for(int k=0;k<24;k++){
				container[type31][3][i].th_par[k][0] = par_62GeV_33_09[k][0];	
				container[type31][3][i].th_par[k][1] = par_62GeV_33_09[k][1];	
			}
			container[type31][3][i].cluster_pads[0]= 0;
			container[type31][3][i].cluster_pads[1]= 1;
		}
	}
	//seg4
	for(int i=0;i<25;i++){
		if(i==15){
			for(int k=0;k<24;k++){
				container[type31][4][i].th_par[k][0] = par_62GeV_33_019[k][0];	
				container[type31][4][i].th_par[k][1] = par_62GeV_33_019[k][1];	
			}
			container[type31][4][i].cluster_pads[0]= 0;
			container[type31][4][i].cluster_pads[1]= 1;
			container[type31][4][i].cluster_pads[2]= 3;
		}else if(i>=9&&i<=14){
			for(int k=0;k<24;k++){
				container[type31][4][i].th_par[k][0] = par_62GeV_33_01[k][0];	
				container[type31][4][i].th_par[k][1] = par_62GeV_33_01[k][1];	
			}
			container[type31][4][i].cluster_pads[0]= 0;
			container[type31][4][i].cluster_pads[1]= 1;
		}
	}
	//seg 5
	for(int i=0;i<25;i++){
		if(i>=22&&i<=24){
			for(int k=0;k<24;k++){
				container[type31][5][i].th_par[k][0] = par_62GeV_33_01[k][0];	
				container[type31][5][i].th_par[k][1] = par_62GeV_33_01[k][1];	
			}
			container[type31][5][i].cluster_pads[0]= 0;
			container[type31][5][i].cluster_pads[1]= 1;
		}
	}
	//padtype 34
	//seg0
	for(int i=0;i<25;i++){
		if(i>=9&&i<=15){
			for(int k=0;k<24;k++){
				container[type34][0][i].th_par[k][0] = par_62GeV_32_0m8[k][0];	
				container[type34][0][i].th_par[k][1] = par_62GeV_32_0m8[k][1];	
			}
			container[type34][0][i].cluster_pads[0]= 0;
			container[type34][0][i].cluster_pads[1]= 1;
		}
	}
	//seg1
	for(int i=0;i<25;i++){
		if(i==15){
			for(int k=0;k<24;k++){
				container[type34][1][i].th_par[k][0] = par_62GeV_32_01m8[k][0];	
				container[type34][1][i].th_par[k][1] = par_62GeV_32_01m8[k][1];	
			}
			container[type34][1][i].cluster_pads[0]= 0;
			container[type34][1][i].cluster_pads[1]= 1;
			container[type34][1][i].cluster_pads[2]= 3;
		}else if(i>=9&&i<=14){
			for(int k=0;k<24;k++){
				container[type34][1][i].th_par[k][0] = par_62GeV_32_01[k][0];	
				container[type34][1][i].th_par[k][1] = par_62GeV_32_01[k][1];	
			}
			container[type34][1][i].cluster_pads[0]= 0;
			container[type34][1][i].cluster_pads[1]= 1;
		}
	}
	//seg2
	for(int i=0;i<25;i++){
		if(i>=22&&i<=24){
			for(int k=0;k<24;k++){
				container[type34][2][i].th_par[k][0] = par_62GeV_32_01[k][0];	
				container[type34][2][i].th_par[k][1] = par_62GeV_32_01[k][1];	
			}
			container[type34][2][i].cluster_pads[0]= 0;
			container[type34][2][i].cluster_pads[1]= 1;
		}
	}
	//seg3
	for(int i=0;i<25;i++){
		if(i>=16&&i<=18){
			for(int k=0;k<24;k++){
				container[type34][3][i].th_par[k][0] = par_62GeV_32_01[k][0];	
				container[type34][3][i].th_par[k][1] = par_62GeV_32_01[k][1];	
			}
			container[type34][3][i].cluster_pads[0]= 0;
			container[type34][3][i].cluster_pads[1]= 1;
		}
	}
	//seg4
	for(int i=0;i<25;i++){
		if(i==9){
			for(int k=0;k<24;k++){
				container[type34][4][i].th_par[k][0] = par_62GeV_32_01m8[k][0];	
				container[type34][4][i].th_par[k][1] = par_62GeV_32_01m8[k][1];	
			}
			container[type34][4][i].cluster_pads[0]= 0;
			container[type34][4][i].cluster_pads[1]= 1;
			container[type34][4][i].cluster_pads[2]= 2;
		}else if(i>=10&&i<=15){
			for(int k=0;k<24;k++){
				container[type32][4][i].th_par[k][0] = par_62GeV_32_01[k][0];	
				container[type32][4][i].th_par[k][1] = par_62GeV_32_01[k][1];	
			}
			container[type32][4][i].cluster_pads[0]= 0;
			container[type32][4][i].cluster_pads[1]= 1;
		}
	}
	//seg5
	for(int i=0;i<25;i++){
		if(i>=9&&i<=15){
			for(int k=0;k<24;k++){
				container[type32][5][i].th_par[k][0] = par_62GeV_32_0m8[k][0];	
				container[type32][5][i].th_par[k][1] = par_62GeV_32_0m8[k][1];	
			}
			container[type32][5][i].cluster_pads[0]= 0;
			container[type32][5][i].cluster_pads[1]= 1;
		}
	}
	//padtype 39
	for(int i=0;i<6;i++){
		for(int j=0;j<25;j++){
				for(int k=0;k<24;k++){
					container[type39][i][j].th_par[k][0] = container[type32][i][j].th_par[k][0];
					container[type39][i][j].th_par[k][1] = container[type32][i][j].th_par[k][1];
					if(i==5&&j>=22&&j<=24){
						container[type39][i][j].th_par[k][0] = par_62GeV_64_019[k][0];
						container[type39][i][j].th_par[k][1] = par_62GeV_64_019[k][1];
					}
					if(i==4&&j>=9&&j<=14){
						container[type39][i][j].th_par[k][0] = par_62GeV_64_01[k][0];
						container[type39][i][j].th_par[k][1] = par_62GeV_64_01[k][1];
					}
					if(i==3&&j==15){
						container[type39][i][j].th_par[k][0] = par_62GeV_64_01m8[k][0];
						container[type39][i][j].th_par[k][1] = par_62GeV_64_01m8[k][1];
					}
				}
				container[type39][i][j].cluster_pads[0]= container[type32][i][j].cluster_pads[0];
				container[type39][i][j].cluster_pads[1]= container[type32][i][j].cluster_pads[1];
				container[type39][i][j].cluster_pads[2]= container[type32][i][j].cluster_pads[2];
				container[type39][i][j].cluster_pads[3]= container[type32][i][j].cluster_pads[3];
		}
	}
	//padtype 40
	for(int i=0;i<6;i++){
		for(int j=0;j<25;j++){
				for(int k=0;k<24;k++){
					container[type40][i][j].th_par[k][0] = container[type32][i][j].th_par[k][0];
					container[type40][i][j].th_par[k][1] = container[type32][i][j].th_par[k][1];
					if(i==0&&j>=16&&j<=18){
						container[type40][i][j].th_par[k][0] = par_62GeV_63_08m1[k][0];
						container[type40][i][j].th_par[k][1] = par_62GeV_63_08m1[k][1];
					}
					if(i==1&&j>=10&&j<=15){
						container[type40][i][j].th_par[k][0] = par_62GeV_63_0m1[k][0];
						container[type40][i][j].th_par[k][1] = par_62GeV_63_0m1[k][1];
					}
					if(i==2&&j==9){
						container[type40][i][j].th_par[k][0] = par_62GeV_63_0m1m9[k][0];
						container[type40][i][j].th_par[k][1] = par_62GeV_63_0m1m9[k][1];
					}
				}
				container[type40][i][j].cluster_pads[0]= container[type32][i][j].cluster_pads[0];
				container[type40][i][j].cluster_pads[1]= container[type32][i][j].cluster_pads[1];
				container[type40][i][j].cluster_pads[2]= container[type32][i][j].cluster_pads[2];
				container[type40][i][j].cluster_pads[3]= container[type32][i][j].cluster_pads[3];
		}
	}
	//padtype 41
	for(int i=0;i<6;i++){
		for(int j=0;j<25;j++){
				for(int k=0;k<24;k++){
					container[type41][i][j].th_par[k][0] = container[type34][i][j].th_par[k][0];
					container[type41][i][j].th_par[k][1] = container[type34][i][j].th_par[k][1];
					if(i==5&&j>=22&&j<=24){
						container[type41][i][j].th_par[k][0] = par_62GeV_64_019[k][0];
						container[type41][i][j].th_par[k][1] = par_62GeV_64_019[k][1];
					}
					if(i==4&&j>=9&&j<=14){
						container[type41][i][j].th_par[k][0] = par_62GeV_64_01[k][0];
						container[type41][i][j].th_par[k][1] = par_62GeV_64_01[k][1];
					}
				}
				container[type41][i][j].cluster_pads[0]= container[type34][i][j].cluster_pads[0];
				container[type41][i][j].cluster_pads[1]= container[type34][i][j].cluster_pads[1];
				container[type41][i][j].cluster_pads[2]= container[type34][i][j].cluster_pads[2];
				container[type41][i][j].cluster_pads[3]= container[type34][i][j].cluster_pads[3];
		}
	}
	//padtype 42
	for(int i=0;i<6;i++){
		for(int j=0;j<25;j++){
				for(int k=0;k<24;k++){
					container[type42][i][j].th_par[k][0] = container[type34][i][j].th_par[k][0];
					container[type42][i][j].th_par[k][1] = container[type34][i][j].th_par[k][1];
					if(i==0&&j>=16&&j<=18){
						container[type42][i][j].th_par[k][0] = par_62GeV_63_08m1[k][0];
						container[type42][i][j].th_par[k][1] = par_62GeV_63_08m1[k][1];
					}
					if(i==1&&j>=10&&j<=15){
						container[type42][i][j].th_par[k][0] = par_62GeV_63_0m1[k][0];
						container[type42][i][j].th_par[k][1] = par_62GeV_63_0m1[k][1];
					}
				}
				container[type42][i][j].cluster_pads[0]= container[type34][i][j].cluster_pads[0];
				container[type42][i][j].cluster_pads[1]= container[type34][i][j].cluster_pads[1];
				container[type42][i][j].cluster_pads[2]= container[type34][i][j].cluster_pads[2];
				container[type42][i][j].cluster_pads[3]= container[type34][i][j].cluster_pads[3];
		}
	}

	if(verbose)cout<<"YWCutter::Init() end!"<<endl;
}

void YWCutter::Initialize_HIJING(){
	if(verbose)cout<<"YWCutter::Init() begin.."<<endl;
	//initialize with 1pad cluster
	for(int i=0;i<23;i++){
		for(int j=0;j<6;j++){
			for(int k=0;k<25;k++){
				for(int l=0;l<24;l++){
					if(i==type0||i==type1||i==type2||i==type3||i==type4){
						container[i][j][k].th_par[l][0]=par_hijing_0_0[l][0];
						container[i][j][k].th_par[l][1]=par_hijing_0_0[l][1];
					}
					if(i==type11){
						container[i][j][k].th_par[l][0]=par_hijing_11_0[l][0];
						container[i][j][k].th_par[l][1]=par_hijing_11_0[l][1];
					}
					if(i==type12){
						container[i][j][k].th_par[l][0]=par_hijing_12_0[l][0];
						container[i][j][k].th_par[l][1]=par_hijing_12_0[l][1];
					}
					if(i==type31||i==type33||i==type35||i==type36||i==type37||i==type38){
						container[i][j][k].th_par[l][0]=par_hijing_33_0[l][0];
						container[i][j][k].th_par[l][1]=par_hijing_33_0[l][1];
					}
					if(i==type32||i==type34 || i==type39 || i==type40 || i==type41||i==type42){
						container[i][j][k].th_par[l][0]=par_hijing_32_0[l][0];
						container[i][j][k].th_par[l][1]=par_hijing_32_0[l][1];
					}
					if(i == type64 || i == type62){
						container[i][j][k].th_par[l][0]=par_hijing_64_0[l][0];
						container[i][j][k].th_par[l][1]=par_hijing_64_0[l][1];
					}
					if(i==type63||i==type61){
						container[i][j][k].th_par[l][0]=par_hijing_63_0[l][0];
						container[i][j][k].th_par[l][1]=par_hijing_63_0[l][1];
					}
				}
				for(int l=0;l<4;l++){
					if(l==0){
						container[i][j][k].cluster_pads[l]=0;
					}else{
						container[i][j][k].cluster_pads[l]=-999;
					}
				}
				container[i][j][k].weight_pad0=1;
			}
		}
	}

	//Fill 1 pad threshold for non-1pad cluster
		//padtype 0
	for(int i=0;i<6;i++){
		for(int j=9;j<25;j++){
			if(j==9){
				for(int k=0;k<24;k++){
					container[type0][i][j].th_par[k][0]=par_hijing_0_2[k][0];
					container[type0][i][j].th_par[k][1]=par_hijing_0_2[k][1];
				}
				container[type0][i][j].cluster_pads[0]= 0;
				container[type0][i][j].cluster_pads[1]= 1;
				container[type0][i][j].cluster_pads[2]= 2;
			}else if(j>=10&&j<=14){
				for(int k=0;k<24;k++){
					container[type0][i][j].th_par[k][0]=par_hijing_0_1[k][0];
					container[type0][i][j].th_par[k][1]=par_hijing_0_1[k][1];
				}
				container[type0][i][j].cluster_pads[0]= 0;
				container[type0][i][j].cluster_pads[1]= 1;
			}else if(j==15){
				for(int k=0;k<24;k++){
					container[type0][i][j].th_par[k][0]=par_hijing_0_2[k][0];
					container[type0][i][j].th_par[k][1]=par_hijing_0_2[k][1];
				}
				container[type0][i][j].cluster_pads[0]= 0;
				container[type0][i][j].cluster_pads[1]= 1;
				container[type0][i][j].cluster_pads[2]= 3;
			}
		}
	}
	//padtype 1
	//seg0
	for(int i=0;i<25;i++){
		if(i==8&&(i>=13&&i<=15)){
			for(int k=0;k<24;k++){
				container[type1][0][i].th_par[k][0] = par_hijing_1_089[k][0];	
				container[type1][0][i].th_par[k][1] = par_hijing_1_089[k][1];	
			}
			container[type1][0][i].cluster_pads[0]= 0;
			container[type1][0][i].cluster_pads[1]= 1;
			container[type1][0][i].cluster_pads[2]= 3;
		}
		if(i>=10&&i<=12){
			for(int k=0;k<24;k++){
				container[type1][0][i].th_par[k][0] = par_hijing_1_01[k][0];	
				container[type1][0][i].th_par[k][1] = par_hijing_1_01[k][1];	
			}
			container[type1][0][i].cluster_pads[0]= 0;
			container[type1][0][i].cluster_pads[1]= 1;
		}
		if(i==9){
			for(int k=0;k<24;k++){
				container[type1][0][i].th_par[k][0] = par_hijing_1_019[k][0];	
				container[type1][0][i].th_par[k][1] = par_hijing_1_019[k][1];	
			}
			container[type1][0][i].cluster_pads[0]= 0;
			container[type1][0][i].cluster_pads[1]= 1;
			container[type1][0][i].cluster_pads[2]= 2;
		}
	}
	//seg1
	for(int i=0;i<25;i++){
		if(i==9){
			for(int k=0;k<24;k++){
				container[type1][1][i].th_par[k][0] = par_hijing_1_019[k][0];	
				container[type1][1][i].th_par[k][1] = par_hijing_1_019[k][1];	
			}
			container[type1][1][i].cluster_pads[0]= 0;
			container[type1][1][i].cluster_pads[1]= 1;
			container[type1][1][i].cluster_pads[2]= 2;
		}
		if(i==15){
			for(int k=0;k<24;k++){
				container[type1][1][i].th_par[k][0] = par_hijing_1_019[k][0];	
				container[type1][1][i].th_par[k][1] = par_hijing_1_019[k][1];	
			}
			container[type1][1][i].cluster_pads[0]= 0;
			container[type1][1][i].cluster_pads[1]= 1;
			container[type1][1][i].cluster_pads[2]= 3;
		}
		if(i>=10&&i<=14){
			for(int k=0;k<24;k++){
				container[type1][1][i].th_par[k][0] = par_hijing_1_01[k][0];	
				container[type1][1][i].th_par[k][1] = par_hijing_1_01[k][1];	
			}
			container[type1][1][i].cluster_pads[0]= 0;
			container[type1][1][i].cluster_pads[1]= 1;
		}
	}
	//seg2
	for(int i=0;i<25;i++){
		if(i==4&&(i>=9&&i<=11)){
			for(int k=0;k<24;k++){
				container[type1][2][i].th_par[k][0] = par_hijing_1_089[k][0];	
				container[type1][2][i].th_par[k][1] = par_hijing_1_089[k][1];	
			}
			container[type1][2][i].cluster_pads[0]= 0;
			container[type1][2][i].cluster_pads[1]= 1;
			container[type1][2][i].cluster_pads[2]= 2;
		}
		if(i>=12&&i<=14){
			for(int k=0;k<24;k++){
				container[type1][2][i].th_par[k][0] = par_hijing_1_01[k][0];	
				container[type1][2][i].th_par[k][1] = par_hijing_1_01[k][1];	
			}
			container[type1][2][i].cluster_pads[0]= 0;
			container[type1][2][i].cluster_pads[1]= 1;
		}
		if(i==15){
			for(int k=0;k<24;k++){
				container[type1][2][i].th_par[k][0] = par_hijing_1_019[k][0];	
				container[type1][2][i].th_par[k][1] = par_hijing_1_019[k][1];	
			}
			container[type1][2][i].cluster_pads[0]= 0;
			container[type1][2][i].cluster_pads[1]= 1;
			container[type1][2][i].cluster_pads[2]= 3;
		}
	}
	//seg3
	for(int i=0;i<25;i++){
		if(!(i==0||i==2)){
			for(int k=0;k<24;k++){
				container[type1][3][i].th_par[k][0] = par_hijing_1_08[k][0];	
				container[type1][3][i].th_par[k][1] = par_hijing_1_08[k][1];	
			}
			container[type1][3][i].cluster_pads[0]= 0;
			container[type1][3][i].cluster_pads[1]= 1;
		}
	}
	//seg4
	for(int i=0;i<25;i++){
		if(i==4||i==9||i==10||i==16||i==17||i==18){
			for(int k=0;k<24;k++){
				container[type1][4][i].th_par[k][0] = par_hijing_1_08[k][0];	
				container[type1][4][i].th_par[k][1] = par_hijing_1_08[k][1];	
			}
			container[type1][4][i].cluster_pads[0]= 0;
			container[type1][4][i].cluster_pads[1]= 1;
		}
		if(i==20){
			for(int k=0;k<24;k++){
				container[type1][4][i].th_par[k][0] = par_hijing_1_0m185[k][0];	
				container[type1][4][i].th_par[k][1] = par_hijing_1_0m185[k][1];	
			}
			container[type1][4][i].cluster_pads[0]= 0;
			container[type1][4][i].cluster_pads[1]= 6;
		}
		if(i==8||i==14||i==15||i==22||i==23||i==24){
			for(int k=0;k<24;k++){
				container[type1][4][i].th_par[k][0] = par_hijing_1_08[k][0];	
				container[type1][4][i].th_par[k][1] = par_hijing_1_08[k][1];	
			}
			container[type1][4][i].cluster_pads[0]= 0;
			container[type1][4][i].cluster_pads[1]= 2;
		}
	}
	//seg5
	for(int i=0;i<25;i++){
		if(!(i==0||i==2)){
			for(int k=0;k<24;k++){
				container[type1][5][i].th_par[k][0] = par_hijing_1_08[k][0];	
				container[type1][5][i].th_par[k][1] = par_hijing_1_08[k][1];	
			}
			container[type1][5][i].cluster_pads[0]= 0;
			container[type1][5][i].cluster_pads[1]= 1;
		}
	}
	//padtype 2
	//seg0
	for(int i=0;i<25;i++){
		if(!(i==0||i==2)){
			for(int k=0;k<24;k++){
				container[type2][0][i].th_par[k][0] = par_hijing_2_09[k][0];	
				container[type2][0][i].th_par[k][1] = par_hijing_2_09[k][1];	
			}
			container[type2][0][i].cluster_pads[0]= 0;
			container[type2][0][i].cluster_pads[1]= 1;
		}
	}
	//seg1
	for(int i=0;i<25;i++){
		if(i==4||i==9||i==10||i==16||i==17||i==18){
			for(int k=0;k<24;k++){
				container[type2][1][i].th_par[k][0] = par_hijing_2_09[k][0];	
				container[type2][1][i].th_par[k][1] = par_hijing_2_09[k][1];	
			}
			container[type2][1][i].cluster_pads[0]= 0;
			container[type2][1][i].cluster_pads[1]= 1;
		}
		if(i==20){
			for(int k=0;k<24;k++){
				container[type2][1][i].th_par[k][0] = par_hijing_2_0185[k][0];	
				container[type2][1][i].th_par[k][1] = par_hijing_2_0185[k][1];	
			}
			container[type2][1][i].cluster_pads[0]= 0;
			container[type2][1][i].cluster_pads[1]= 6;
		}
		if(i==8||i==14||i==15||i==22||i==23||i==24){
			for(int k=0;k<24;k++){
				container[type2][1][i].th_par[k][0] = par_hijing_2_09[k][0];	
				container[type2][1][i].th_par[k][1] = par_hijing_2_09[k][1];	
			}
			container[type2][1][i].cluster_pads[0]= 0;
			container[type2][1][i].cluster_pads[1]= 2;
		}
	}
	//seg2
	for(int i=0;i<25;i++){
		if(!(i==0||i==2)){
			for(int k=0;k<24;k++){
				container[type2][2][i].th_par[k][0] = par_hijing_2_09[k][0];	
				container[type2][2][i].th_par[k][1] = par_hijing_2_09[k][1];	
			}
			container[type2][2][i].cluster_pads[0]= 0;
			container[type2][2][i].cluster_pads[1]= 1;
		}
	}
	//seg3
	for(int i=0;i<25;i++){
		if(i==8&&(i>=13&&i<=15)){
			for(int k=0;k<24;k++){
				container[type2][3][i].th_par[k][0] = par_hijing_2_089[k][0];	
				container[type2][3][i].th_par[k][1] = par_hijing_2_089[k][1];	
			}
			container[type2][3][i].cluster_pads[0]= 0;
			container[type2][3][i].cluster_pads[1]= 1;
			container[type2][3][i].cluster_pads[1]= 3;
		}
		if(i>=10&&i<=12){
			for(int k=0;k<24;k++){
				container[type2][3][i].th_par[k][0] = par_hijing_2_0m1[k][0];	
				container[type2][3][i].th_par[k][1] = par_hijing_2_0m1[k][1];	
			}
			container[type2][3][i].cluster_pads[0]= 0;
			container[type2][3][i].cluster_pads[1]= 1;
		}
		if(i==9){
			for(int k=0;k<24;k++){
				container[type2][3][i].th_par[k][0] = par_hijing_2_0m18[k][0];	
				container[type2][3][i].th_par[k][1] = par_hijing_2_0m18[k][1];	
			}
			container[type2][3][i].cluster_pads[0]= 0;
			container[type2][3][i].cluster_pads[1]= 1;
			container[type2][3][i].cluster_pads[2]= 2;
		}
	}
	//seg4
	for(int i=0;i<25;i++){
		if(i==9){
			for(int k=0;k<24;k++){
				container[type2][4][i].th_par[k][0] = par_hijing_2_0m18[k][0];	
				container[type2][4][i].th_par[k][1] = par_hijing_2_0m18[k][1];	
			}
			container[type2][4][i].cluster_pads[0]= 0;
			container[type2][4][i].cluster_pads[1]= 1;
			container[type2][4][i].cluster_pads[2]= 2;
		}
		if(i==15){
			for(int k=0;k<24;k++){
				container[type2][4][i].th_par[k][0] = par_hijing_2_0m18[k][0];	
				container[type2][4][i].th_par[k][1] = par_hijing_2_0m18[k][1];	
			}
			container[type2][4][i].cluster_pads[0]= 0;
			container[type2][4][i].cluster_pads[1]= 1;
			container[type2][4][i].cluster_pads[2]= 3;
		}
		if(i>=10&&i<=14){
			for(int k=0;k<24;k++){
				container[type2][4][i].th_par[k][0] = par_hijing_2_0m1[k][0];	
				container[type2][4][i].th_par[k][1] = par_hijing_2_0m1[k][1];	
			}
			container[type2][4][i].cluster_pads[0]= 0;
			container[type2][4][i].cluster_pads[1]= 1;
		}
	}
	//seg5
	for(int i=0;i<25;i++){
		if(i==4&&(i>=9&&i<=11)){
			for(int k=0;k<24;k++){
				container[type2][5][i].th_par[k][0] = par_hijing_2_089[k][0];	
				container[type2][5][i].th_par[k][1] = par_hijing_2_089[k][1];	
			}
			container[type2][5][i].cluster_pads[0]= 0;
			container[type2][5][i].cluster_pads[1]= 1;
			container[type2][5][i].cluster_pads[2]= 2;
		}
		if(i>=12&&i<=14){
			for(int k=0;k<24;k++){
				container[type2][5][i].th_par[k][0] = par_hijing_2_0m1[k][0];	
				container[type2][5][i].th_par[k][1] = par_hijing_2_0m1[k][1];	
			}
			container[type2][5][i].cluster_pads[0]= 0;
			container[type2][5][i].cluster_pads[1]= 1;
		}
		if(i==15){
			for(int k=0;k<24;k++){
				container[type2][5][i].th_par[k][0] = par_hijing_2_0m18[k][0];	
				container[type2][5][i].th_par[k][1] = par_hijing_2_0m18[k][1];	
			}
			container[type2][5][i].cluster_pads[0]= 0;
			container[type2][5][i].cluster_pads[1]= 1;
			container[type2][5][i].cluster_pads[2]= 3;
		}
	}
	//padtype 3
	//seg 0-2
	for(int i=0;i<3;i++){
		for(int j=9;j<25;j++){
			if(j==9){
				for(int k=0;k<24;k++){
					container[type3][i][j].th_par[k][0]=par_hijing_0_2[k][0];
					container[type3][i][j].th_par[k][1]=par_hijing_0_2[k][1];
				}
				container[type3][i][j].cluster_pads[0]= 0;
				container[type3][i][j].cluster_pads[1]= 1;
				container[type3][i][j].cluster_pads[2]= 2;
			}else if(j>=10&&j<=14){
				for(int k=0;k<24;k++){
					container[type3][i][j].th_par[k][0]=par_hijing_0_1[k][0];
					container[type3][i][j].th_par[k][1]=par_hijing_0_1[k][1];
				}
				container[type3][i][j].cluster_pads[0]= 0;
				container[type3][i][j].cluster_pads[1]= 1;
			}else if(j==15){
				for(int k=0;k<24;k++){
					container[type3][i][j].th_par[k][0]=par_hijing_0_2[k][0];
					container[type3][i][j].th_par[k][1]=par_hijing_0_2[k][1];
				}
				container[type3][i][j].cluster_pads[0]= 0;
				container[type3][i][j].cluster_pads[1]= 1;
				container[type3][i][j].cluster_pads[2]= 3;
			}
		}
	}
	//seg3
	for(int i=0;i<25;i++){
		if(i==4||i==9){
			for(int k=0;k<24;k++){
				container[type3][3][i].th_par[k][0] = par_hijing_12_09[k][0];	
				container[type3][3][i].th_par[k][1] = par_hijing_12_09[k][1];	
			}
			container[type3][3][i].cluster_pads[0]= 0;
			container[type3][3][i].cluster_pads[1]= 2;
		}else if(i>=10&&i<=12){
			for(int k=0;k<24;k++){
				container[type3][3][i].th_par[k][0] = par_hijing_12_019[k][0];	
				container[type3][3][i].th_par[k][1] = par_hijing_12_019[k][1];	
			}
			container[type3][3][i].cluster_pads[0]= 0;
			container[type3][3][i].cluster_pads[1]= 1;
			container[type3][3][i].cluster_pads[2]= 2;
		}else if(i>=13&&i<=14){
			for(int k=0;k<24;k++){
				container[type3][3][i].th_par[k][0] = par_hijing_0_1[k][0];	
				container[type3][3][i].th_par[k][1] = par_hijing_0_1[k][1];	
			}
			container[type3][3][i].cluster_pads[0]= 0;
			container[type3][3][i].cluster_pads[1]= 1;
		}else if(i==15){
			for(int k=0;k<24;k++){
				container[type3][3][i].th_par[k][0] = par_hijing_0_2[k][0];	
				container[type3][3][i].th_par[k][1] = par_hijing_0_2[k][1];	
			}
			container[type3][3][i].cluster_pads[0]= 0;
			container[type3][3][i].cluster_pads[1]= 1;
			container[type3][3][i].cluster_pads[2]= 3;
		}
	}
	//seg4
	for(int i=0;i<25;i++){
		if(i>=1){
			for(int k=0;k<24;k++){
				container[type3][4][i].th_par[k][0] = par_hijing_12_09[k][0];	
				container[type3][4][i].th_par[k][1] = par_hijing_12_09[k][1];	
			}
			container[type3][4][i].cluster_pads[0]= 0;
			container[type3][4][i].cluster_pads[1]= 1;
		}
	}
	//seg5
	for(int i=0;i<25;i++){
		if(i==8||i==15){
			for(int k=0;k<24;k++){
				container[type3][5][i].th_par[k][0] = par_hijing_12_09[k][0];	
				container[type3][5][i].th_par[k][1] = par_hijing_12_09[k][1];	
			}
			container[type3][5][i].cluster_pads[0]= 0;
			container[type3][5][i].cluster_pads[1]= 3;
		}else if(i>=12&&i<=14){
			for(int k=0;k<24;k++){
				container[type3][5][i].th_par[k][0] = par_hijing_12_019[k][0];	
				container[type3][5][i].th_par[k][1] = par_hijing_12_019[k][1];	
			}
			container[type3][5][i].cluster_pads[0]= 0;
			container[type3][5][i].cluster_pads[1]= 1;
			container[type3][5][i].cluster_pads[2]= 3;
		}else if(i>=10&&i<=11){
			for(int k=0;k<24;k++){
				container[type3][5][i].th_par[k][0] = par_hijing_0_1[k][0];	
				container[type3][5][i].th_par[k][1] = par_hijing_0_1[k][1];	
			}
			container[type3][5][i].cluster_pads[0]= 0;
			container[type3][5][i].cluster_pads[1]= 1;
		}else if(i==9){
			for(int k=0;k<24;k++){
				container[type3][5][i].th_par[k][0] = par_hijing_0_2[k][0];	
				container[type3][5][i].th_par[k][1] = par_hijing_0_2[k][1];	
			}
			container[type3][5][i].cluster_pads[0]= 0;
			container[type3][5][i].cluster_pads[1]= 1;
			container[type3][5][i].cluster_pads[2]= 2;
		}
	}
	//padtype 4
	//seg 3-5
	for(int i=3;i<6;i++){
		for(int j=9;j<25;j++){
			if(j==9){
				for(int k=0;k<24;k++){
					container[type4][i][j].th_par[k][0]=par_hijing_0_2[k][0];
					container[type4][i][j].th_par[k][1]=par_hijing_0_2[k][1];
				}
				container[type4][i][j].cluster_pads[0]= 0;
				container[type4][i][j].cluster_pads[1]= 1;
				container[type4][i][j].cluster_pads[2]= 2;
			}else if(j>=10&&j<=14){
				for(int k=0;k<24;k++){
					container[type4][i][j].th_par[k][0]=par_hijing_0_1[k][0];
					container[type4][i][j].th_par[k][1]=par_hijing_0_1[k][1];
				}
				container[type4][i][j].cluster_pads[0]= 0;
				container[type4][i][j].cluster_pads[1]= 1;
			}else if(j==15){
				for(int k=0;k<24;k++){
					container[type4][i][j].th_par[k][0]=par_hijing_0_2[k][0];
					container[type4][i][j].th_par[k][1]=par_hijing_0_2[k][1];
				}
				container[type4][i][j].cluster_pads[0]= 0;
				container[type4][i][j].cluster_pads[1]= 1;
				container[type4][i][j].cluster_pads[2]= 3;
			}
		}
	}
	//seg0
	for(int i=0;i<25;i++){
		if(i==4||i==9){
			for(int k=0;k<24;k++){
				container[type4][0][i].th_par[k][0] = par_hijing_11_08[k][0];	
				container[type4][0][i].th_par[k][1] = par_hijing_11_08[k][1];	
			}
			container[type4][0][i].cluster_pads[0]= 0;
			container[type4][0][i].cluster_pads[1]= 2;
		}else if(i>=10&&i<=12){
			for(int k=0;k<24;k++){
				container[type4][0][i].th_par[k][0] = par_hijing_11_0m18[k][0];	
				container[type4][0][i].th_par[k][1] = par_hijing_11_0m18[k][1];	
			}
			container[type4][0][i].cluster_pads[0]= 0;
			container[type4][0][i].cluster_pads[1]= 1;
			container[type4][0][i].cluster_pads[2]= 2;
		}else if(i>=13&&i<=14){
			for(int k=0;k<24;k++){
				container[type4][0][i].th_par[k][0] = par_hijing_0_1[k][0];	
				container[type4][0][i].th_par[k][1] = par_hijing_0_1[k][1];	
			}
			container[type4][0][i].cluster_pads[0]= 0;
			container[type4][0][i].cluster_pads[1]= 1;
		}else if(i==15){
			for(int k=0;k<24;k++){
				container[type4][0][i].th_par[k][0] = par_hijing_0_2[k][0];	
				container[type4][0][i].th_par[k][1] = par_hijing_0_2[k][1];	
			}
			container[type4][0][i].cluster_pads[0]= 0;
			container[type4][0][i].cluster_pads[1]= 1;
			container[type4][0][i].cluster_pads[2]= 3;
		}
	}
	//seg1
	for(int i=0;i<25;i++){
		if(i>=1){
			for(int k=0;k<24;k++){
				container[type4][4][i].th_par[k][0] = par_hijing_11_08[k][0];	
				container[type4][4][i].th_par[k][1] = par_hijing_11_08[k][1];	
			}
			container[type4][4][i].cluster_pads[0]= 0;
			container[type4][4][i].cluster_pads[1]= 1;
		}
	}
	//seg2
	for(int i=0;i<25;i++){
		if(i==8||i==15){
			for(int k=0;k<24;k++){
				container[type4][0][i].th_par[k][0] = par_hijing_11_08[k][0];	
				container[type4][0][i].th_par[k][1] = par_hijing_11_08[k][1];	
			}
			container[type4][0][i].cluster_pads[0]= 0;
			container[type4][0][i].cluster_pads[1]= 3;
		}else if(i>=12&&i<=14){
			for(int k=0;k<24;k++){
				container[type4][0][i].th_par[k][0] = par_hijing_11_0m18[k][0];	
				container[type4][0][i].th_par[k][1] = par_hijing_11_0m18[k][1];	
			}
			container[type4][0][i].cluster_pads[0]= 0;
			container[type4][0][i].cluster_pads[1]= 1;
			container[type4][0][i].cluster_pads[2]= 3;
		}else if(i>=10&&i<=11){
			for(int k=0;k<24;k++){
				container[type4][0][i].th_par[k][0] = par_hijing_0_1[k][0];	
				container[type4][0][i].th_par[k][1] = par_hijing_0_1[k][1];	
			}
			container[type4][0][i].cluster_pads[0]= 0;
			container[type4][0][i].cluster_pads[1]= 1;
		}else if(i==9){
			for(int k=0;k<24;k++){
				container[type4][0][i].th_par[k][0] = par_hijing_0_2[k][0];	
				container[type4][0][i].th_par[k][1] = par_hijing_0_2[k][1];	
			}
			container[type4][0][i].cluster_pads[0]= 0;
			container[type4][0][i].cluster_pads[1]= 1;
			container[type4][0][i].cluster_pads[2]= 2;
		}
	}
	//padtype 11
	//seg0
	for(int i=0;i<25;i++){
		if(i==1||i==5||i==6||i==12||i==13||i==21||i==22||i==4||i==10||i==11||i==19||i==20){
			for(int k=0;k<24;k++){
				container[type11][0][i].th_par[k][0] = par_hijing_11_0184[k][0];	
				container[type11][0][i].th_par[k][1] = par_hijing_11_0184[k][1];	
			}
			container[type11][0][i].cluster_pads[0]= 0;
			container[type11][0][i].cluster_pads[1]= 4;
		}
	}
	//seg1
	for(int i=0;i<25;i++){
		if(i>=1){
			for(int k=0;k<24;k++){
				container[type11][1][i].th_par[k][0] = par_hijing_11_0184[k][0];	
				container[type11][1][i].th_par[k][1] = par_hijing_11_0184[k][1];	
			}
			container[type11][1][i].cluster_pads[0]= 0;
			container[type11][1][i].cluster_pads[1]= 4;
		}
	}
	//seg2
	for(int i=0;i<25;i++){
		if(i==3||i==6||i==7||i==11||i==12||i==18||i==19||i==8||i==13||i==14||i==20||i==21){
			for(int k=0;k<24;k++){
				container[type11][2][i].th_par[k][0] = par_hijing_11_0184[k][0];	
				container[type11][2][i].th_par[k][1] = par_hijing_11_0184[k][1];	
			}
			container[type11][2][i].cluster_pads[0]= 0;
			container[type11][2][i].cluster_pads[1]= 4;
		}
	}
	//seg3
	for(int i=0;i<25;i++){
		if(i==8||i==6||(i>=9&&i<=15)){
			for(int k=0;k<24;k++){
				container[type11][3][i].th_par[k][0] = par_hijing_11_08[k][0];	
				container[type11][3][i].th_par[k][1] = par_hijing_11_08[k][1];	
			}
			container[type11][3][i].cluster_pads[0]= 0;
			container[type11][3][i].cluster_pads[1]= 1;
		}
	}
	//seg4
	for(int i=0;i<25;i++){
		if(i==9){
			for(int k=0;k<24;k++){
				container[type11][4][i].th_par[k][0] = par_hijing_11_0m18[k][0];	
				container[type11][4][i].th_par[k][1] = par_hijing_11_0m18[k][1];	
			}
			container[type11][4][i].cluster_pads[0]= 0;
			container[type11][4][i].cluster_pads[1]= 1;
			container[type11][4][i].cluster_pads[2]= 2;
		}
		if(i==15){
			for(int k=0;k<24;k++){
				container[type11][4][i].th_par[k][0] = par_hijing_11_0m18[k][0];	
				container[type11][4][i].th_par[k][1] = par_hijing_11_0m18[k][1];	
			}
			container[type11][4][i].cluster_pads[0]= 0;
			container[type11][4][i].cluster_pads[1]= 1;
			container[type11][4][i].cluster_pads[2]= 3;
		}
		if(i==11||i==13){
			for(int k=0;k<24;k++){
				container[type11][4][i].th_par[k][0] = par_hijing_11_08[k][0];	
				container[type11][4][i].th_par[k][1] = par_hijing_11_08[k][1];	
			}
			container[type11][4][i].cluster_pads[0]= 0;
			container[type11][4][i].cluster_pads[1]= 1;
		}
	}
	//seg5
	for(int i=0;i<25;i++){
		if(i==4||i==6||(i>=9&&i<=15)){
			for(int k=0;k<24;k++){
				container[type11][5][i].th_par[k][0] = par_hijing_11_08[k][0];	
				container[type11][5][i].th_par[k][1] = par_hijing_11_08[k][1];	
			}
			container[type11][5][i].cluster_pads[0]= 0;
			container[type11][5][i].cluster_pads[1]= 1;
		}
	}
	//padtype 12
	//seg0
	for(int i=0;i<25;i++){
		if(i==8||i==6||(i>=9&&i<=15)){
			for(int k=0;k<24;k++){
				container[type12][0][i].th_par[k][0] = par_hijing_12_09[k][0];	
				container[type12][0][i].th_par[k][1] = par_hijing_12_09[k][1];	
			}
			container[type12][0][i].cluster_pads[0]= 0;
			container[type12][0][i].cluster_pads[1]= 1;
		}
	}
	//seg1
	for(int i=0;i<25;i++){
		if(i==9){
			for(int k=0;k<24;k++){
				container[type12][1][i].th_par[k][0] = par_hijing_12_019[k][0];	
				container[type12][1][i].th_par[k][1] = par_hijing_12_019[k][1];	
			}
			container[type12][1][i].cluster_pads[0]= 0;
			container[type12][1][i].cluster_pads[1]= 1;
			container[type12][1][i].cluster_pads[0]= 2;
		}
		if(i==15){
			for(int k=0;k<24;k++){
				container[type12][1][i].th_par[k][0] = par_hijing_12_019[k][0];	
				container[type12][1][i].th_par[k][1] = par_hijing_12_019[k][1];	
			}
			container[type12][1][i].cluster_pads[0]= 0;
			container[type12][1][i].cluster_pads[1]= 1;
			container[type12][1][i].cluster_pads[2]= 3;
		}
		if(i==11||i==13){
			for(int k=0;k<24;k++){
				container[type12][1][i].th_par[k][0] = par_hijing_12_09[k][0];	
				container[type12][1][i].th_par[k][1] = par_hijing_12_09[k][1];	
			}
			container[type12][1][i].cluster_pads[0]= 0;
			container[type12][1][i].cluster_pads[1]= 1;
		}
	}
	//seg2
	for(int i=0;i<25;i++){
		if(i==4||i==6||(i>=9&&i<=15)){
			for(int k=0;k<24;k++){
				container[type12][2][i].th_par[k][0] = par_hijing_12_09[k][0];	
				container[type12][2][i].th_par[k][1] = par_hijing_12_09[k][1];	
			}
			container[type12][2][i].cluster_pads[0]= 0;
			container[type12][2][i].cluster_pads[1]= 1;
		}
	}
	//seg3
	for(int i=0;i<25;i++){
		if(i==1||i==5||i==6||i==12||i==13||i==21||i==22||i==4||i==10||i==11||i==19||i==20){
			for(int k=0;k<24;k++){
				container[type12][3][i].th_par[k][0] = par_hijing_12_0m184[k][0];	
				container[type12][3][i].th_par[k][1] = par_hijing_12_0m184[k][1];	
			}
			container[type12][3][i].cluster_pads[0]= 0;
			container[type12][3][i].cluster_pads[1]= 4;
		}
	}
	//seg4
	for(int i=0;i<25;i++){
		if(i>=1){
			for(int k=0;k<24;k++){
				container[type12][4][i].th_par[k][0] = par_hijing_12_0m184[k][0];	
				container[type12][4][i].th_par[k][1] = par_hijing_12_0m184[k][1];	
			}
			container[type12][4][i].cluster_pads[0]= 0;
			container[type12][4][i].cluster_pads[1]= 4;
		}
	}
	//seg5
	for(int i=0;i<25;i++){
		if(i==3||i==6||i==7||i==11||i==12||i==18||i==19||i==8||i==14||i==13||i==20||i==21){
			for(int k=0;k<24;k++){
				container[type12][3][i].th_par[k][0] = par_hijing_12_0m184[k][0];	
				container[type12][3][i].th_par[k][1] = par_hijing_12_0m184[k][1];	
			}
			container[type12][3][i].cluster_pads[0]= 0;
			container[type12][3][i].cluster_pads[1]= 4;
		}
	}
	//padtype 32
	//seg0
	for(int i=0;i<25;i++){
		if(i>=16&&i<=18){
			for(int k=0;k<24;k++){
				container[type32][0][i].th_par[k][0] = par_hijing_32_019[k][0];	
				container[type32][0][i].th_par[k][1] = par_hijing_32_019[k][1];	
			}
			container[type32][0][i].cluster_pads[0]= 0;
			container[type32][0][i].cluster_pads[1]= 1;
			container[type32][0][i].cluster_pads[2]= 2;
		}else if(i>=20&&i<=23){
			for(int k=0;k<24;k++){
				container[type32][0][i].th_par[k][0] = par_hijing_32_09[k][0];	
				container[type32][0][i].th_par[k][1] = par_hijing_32_09[k][1];	
			}
			container[type32][0][i].cluster_pads[0]= 0;
			container[type32][0][i].cluster_pads[1]= 1;
		}else if(i==24){
			for(int k=0;k<24;k++){
				container[type32][0][i].th_par[k][0] = par_hijing_32_089[k][0];	
				container[type32][0][i].th_par[k][1] = par_hijing_32_089[k][1];	
			}
			container[type32][0][i].cluster_pads[0]= 0;
			container[type32][0][i].cluster_pads[1]= 1;
			container[type32][0][i].cluster_pads[2]= 3;
		}
	}
	//seg1
	for(int i=0;i<25;i++){
		if(i==9){
			for(int k=0;k<24;k++){
				container[type32][1][i].th_par[k][0] = par_hijing_32_01m8[k][0];	
				container[type32][1][i].th_par[k][1] = par_hijing_32_01m8[k][1];	
			}
			container[type32][1][i].cluster_pads[0]= 0;
			container[type32][1][i].cluster_pads[1]= 1;
			container[type32][1][i].cluster_pads[2]= 2;
		}else if(i>=10&&i<=15){
			for(int k=0;k<24;k++){
				container[type32][1][i].th_par[k][0] = par_hijing_32_01[k][0];	
				container[type32][1][i].th_par[k][1] = par_hijing_32_01[k][1];	
			}
			container[type32][1][i].cluster_pads[0]= 0;
			container[type32][1][i].cluster_pads[1]= 1;
		}
	}
	//seg2
	for(int i=0;i<25;i++){
		if(i>=9&&i<=15){
			for(int k=0;k<24;k++){
				container[type32][2][i].th_par[k][0] = par_hijing_32_0m8[k][0];	
				container[type32][2][i].th_par[k][1] = par_hijing_32_0m8[k][1];	
			}
			container[type32][2][i].cluster_pads[0]= 0;
			container[type32][2][i].cluster_pads[1]= 1;
		}
	}
	//seg3
	for(int i=0;i<25;i++){
		if(i>=9&&i<=15){
			for(int k=0;k<24;k++){
				container[type32][3][i].th_par[k][0] = par_hijing_32_0m8[k][0];	
				container[type32][3][i].th_par[k][1] = par_hijing_32_0m8[k][1];	
			}
			container[type32][3][i].cluster_pads[0]= 0;
			container[type32][3][i].cluster_pads[1]= 1;
		}
	}
	//seg4
	for(int i=0;i<25;i++){
		if(i==15){
			for(int k=0;k<24;k++){
				container[type32][4][i].th_par[k][0] = par_hijing_32_01m8[k][0];	
				container[type32][4][i].th_par[k][1] = par_hijing_32_01m8[k][1];	
			}
			container[type32][4][i].cluster_pads[0]= 0;
			container[type32][4][i].cluster_pads[1]= 1;
			container[type32][4][i].cluster_pads[2]= 3;
		}else if(i>=9&&i<=14){
			for(int k=0;k<24;k++){
				container[type32][4][i].th_par[k][0] = par_hijing_32_01[k][0];	
				container[type32][4][i].th_par[k][1] = par_hijing_32_01[k][1];	
			}
			container[type32][4][i].cluster_pads[0]= 0;
			container[type32][4][i].cluster_pads[1]= 1;
		}
	}
	//seg 5
	for(int i=0;i<25;i++){
		if(i>=22&&i<=24){
			for(int k=0;k<24;k++){
				container[type32][5][i].th_par[k][0] = par_hijing_32_019[k][0];	
				container[type32][5][i].th_par[k][1] = par_hijing_32_019[k][1];	
			}
			container[type32][5][i].cluster_pads[0]= 0;
			container[type32][5][i].cluster_pads[1]= 1;
			container[type32][5][i].cluster_pads[2]= 3;
		}else if(i>=17&&i<=20){
			for(int k=0;k<24;k++){
				container[type32][5][i].th_par[k][0] = par_hijing_32_09[k][0];	
				container[type32][5][i].th_par[k][1] = par_hijing_32_09[k][1];	
			}
			container[type32][5][i].cluster_pads[0]= 0;
			container[type32][5][i].cluster_pads[1]= 1;
		}else if(i==16){
			for(int k=0;k<24;k++){
				container[type32][5][i].th_par[k][0] = par_hijing_32_089[k][0];	
				container[type32][5][i].th_par[k][1] = par_hijing_32_089[k][1];	
			}
			container[type32][5][i].cluster_pads[0]= 0;
			container[type32][5][i].cluster_pads[1]= 1;
			container[type32][5][i].cluster_pads[2]= 2;
		}
	}
	//padtype 33
	//seg0
	for(int i=0;i<25;i++){
		if(i>=9&&i<=15){
			for(int k=0;k<24;k++){
				container[type33][0][i].th_par[k][0] = par_hijing_33_09[k][0];	
				container[type33][0][i].th_par[k][1] = par_hijing_33_09[k][1];	
			}
			container[type33][0][i].cluster_pads[0]= 0;
			container[type33][0][i].cluster_pads[1]= 1;
		}
	}
	//seg1
	for(int i=0;i<25;i++){
		if(i==15){
			for(int k=0;k<24;k++){
				container[type33][1][i].th_par[k][0] = par_hijing_33_019[k][0];	
				container[type33][1][i].th_par[k][1] = par_hijing_33_019[k][1];	
			}
			container[type33][1][i].cluster_pads[0]= 0;
			container[type33][1][i].cluster_pads[1]= 1;
			container[type33][1][i].cluster_pads[2]= 3;
		}else if(i>=9&&i<=14){
			for(int k=0;k<24;k++){
				container[type33][1][i].th_par[k][0] = par_hijing_33_01[k][0];	
				container[type33][1][i].th_par[k][1] = par_hijing_33_01[k][1];	
			}
			container[type33][1][i].cluster_pads[0]= 0;
			container[type33][1][i].cluster_pads[1]= 1;
		}
	}
	//seg2
	for(int i=0;i<25;i++){
		if(i==16){
			for(int k=0;k<24;k++){
				container[type33][2][i].th_par[k][0] = par_hijing_33_0m9m8[k][0];	
				container[type33][2][i].th_par[k][1] = par_hijing_33_0m9m8[k][1];	
			}
			container[type33][2][i].cluster_pads[0]= 0;
			container[type33][2][i].cluster_pads[1]= 1;
			container[type33][2][i].cluster_pads[2]= 2;
		}else if(i>=17&&i<=20){
			for(int k=0;k<24;k++){
				container[type33][2][i].th_par[k][0] = par_hijing_33_0m8[k][0];	
				container[type33][2][i].th_par[k][1] = par_hijing_33_0m8[k][1];	
			}
			container[type33][2][i].cluster_pads[0]= 0;
			container[type33][2][i].cluster_pads[1]= 1;
		}else if(i>=22&&i<=24){
			for(int k=0;k<24;k++){
				container[type33][2][i].th_par[k][0] = par_hijing_33_01m8[k][0];	
				container[type33][2][i].th_par[k][1] = par_hijing_33_01m8[k][1];	
			}
			container[type33][2][i].cluster_pads[0]= 0;
			container[type33][2][i].cluster_pads[1]= 1;
			container[type33][2][i].cluster_pads[2]= 3;
		}
	}
	//seg3
	for(int i=0;i<25;i++){
		if(i>=16&&i<=18){
			for(int k=0;k<24;k++){
				container[type33][3][i].th_par[k][0] = par_hijing_33_01m8[k][0];	
				container[type33][3][i].th_par[k][1] = par_hijing_33_01m8[k][1];	
			}
			container[type33][3][i].cluster_pads[0]= 0;
			container[type33][3][i].cluster_pads[1]= 1;
			container[type33][3][i].cluster_pads[2]= 2;
		}else if(i>=20&&i<=23){
			for(int k=0;k<24;k++){
				container[type33][3][i].th_par[k][0] = par_hijing_33_0m8[k][0];	
				container[type33][3][i].th_par[k][1] = par_hijing_33_0m8[k][1];	
			}
			container[type33][3][i].cluster_pads[0]= 0;
			container[type33][3][i].cluster_pads[1]= 1;
		}else if(i==24){
			for(int k=0;k<24;k++){
				container[type33][3][i].th_par[k][0] = par_hijing_33_0m9m8[k][0];	
				container[type33][3][i].th_par[k][1] = par_hijing_33_0m9m8[k][1];	
			}
			container[type33][3][i].cluster_pads[0]= 0;
			container[type33][3][i].cluster_pads[1]= 1;
			container[type33][3][i].cluster_pads[2]= 3;
		}
	}
	//seg4
	for(int i=0;i<25;i++){
		if(i==9){
			for(int k=0;k<24;k++){
				container[type33][4][i].th_par[k][0] = par_hijing_33_019[k][0];	
				container[type33][4][i].th_par[k][1] = par_hijing_33_019[k][1];	
			}
			container[type33][4][i].cluster_pads[0]= 0;
			container[type33][4][i].cluster_pads[1]= 1;
			container[type33][4][i].cluster_pads[2]= 2;
		}else if(i>=10&&i<=15){
			for(int k=0;k<24;k++){
				container[type33][4][i].th_par[k][0] = par_hijing_33_01[k][0];	
				container[type33][4][i].th_par[k][1] = par_hijing_33_01[k][1];	
			}
			container[type33][4][i].cluster_pads[0]= 0;
			container[type33][4][i].cluster_pads[1]= 1;
		}
	}
	//seg5
	for(int i=0;i<25;i++){
		if(i>=9&&i<=15){
			for(int k=0;k<24;k++){
				container[type33][5][i].th_par[k][0] = par_hijing_33_09[k][0];	
				container[type33][5][i].th_par[k][1] = par_hijing_33_09[k][1];	
			}
			container[type33][5][i].cluster_pads[0]= 0;
			container[type33][5][i].cluster_pads[1]= 1;
		}
	}

	//padtype 31
	//seg0
	for(int i=0;i<25;i++){
		if(i>=16&&i<=18){
			for(int k=0;k<24;k++){
				container[type31][0][i].th_par[k][0] = par_hijing_33_01[k][0];	
				container[type31][0][i].th_par[k][1] = par_hijing_33_01[k][1];	
			}
			container[type31][0][i].cluster_pads[0]= 0;
			container[type31][0][i].cluster_pads[1]= 1;
		}
	}
	//seg1
	for(int i=0;i<25;i++){
		if(i==9){
			for(int k=0;k<24;k++){
				container[type31][1][i].th_par[k][0] = par_hijing_33_019[k][0];	
				container[type31][1][i].th_par[k][1] = par_hijing_33_019[k][1];	
			}
			container[type31][1][i].cluster_pads[0]= 0;
			container[type31][1][i].cluster_pads[1]= 1;
			container[type31][1][i].cluster_pads[2]= 2;
		}else if(i>=10&&i<=15){
			for(int k=0;k<24;k++){
				container[type31][1][i].th_par[k][0] = par_hijing_33_01[k][0];	
				container[type31][1][i].th_par[k][1] = par_hijing_33_01[k][1];	
			}
			container[type31][1][i].cluster_pads[0]= 0;
			container[type31][1][i].cluster_pads[1]= 1;
		}
	}
	//seg2
	for(int i=0;i<25;i++){
		if(i>=9&&i<=15){
			for(int k=0;k<24;k++){
				container[type31][2][i].th_par[k][0] = par_hijing_33_09[k][0];	
				container[type31][2][i].th_par[k][1] = par_hijing_33_09[k][1];	
			}
			container[type31][2][i].cluster_pads[0]= 0;
			container[type31][2][i].cluster_pads[1]= 1;
		}
	}
	//seg3
	for(int i=0;i<25;i++){
		if(i>=9&&i<=15){
			for(int k=0;k<24;k++){
				container[type31][3][i].th_par[k][0] = par_hijing_33_09[k][0];	
				container[type31][3][i].th_par[k][1] = par_hijing_33_09[k][1];	
			}
			container[type31][3][i].cluster_pads[0]= 0;
			container[type31][3][i].cluster_pads[1]= 1;
		}
	}
	//seg4
	for(int i=0;i<25;i++){
		if(i==15){
			for(int k=0;k<24;k++){
				container[type31][4][i].th_par[k][0] = par_hijing_33_019[k][0];	
				container[type31][4][i].th_par[k][1] = par_hijing_33_019[k][1];	
			}
			container[type31][4][i].cluster_pads[0]= 0;
			container[type31][4][i].cluster_pads[1]= 1;
			container[type31][4][i].cluster_pads[2]= 3;
		}else if(i>=9&&i<=14){
			for(int k=0;k<24;k++){
				container[type31][4][i].th_par[k][0] = par_hijing_33_01[k][0];	
				container[type31][4][i].th_par[k][1] = par_hijing_33_01[k][1];	
			}
			container[type31][4][i].cluster_pads[0]= 0;
			container[type31][4][i].cluster_pads[1]= 1;
		}
	}
	//seg 5
	for(int i=0;i<25;i++){
		if(i>=22&&i<=24){
			for(int k=0;k<24;k++){
				container[type31][5][i].th_par[k][0] = par_hijing_33_01[k][0];	
				container[type31][5][i].th_par[k][1] = par_hijing_33_01[k][1];	
			}
			container[type31][5][i].cluster_pads[0]= 0;
			container[type31][5][i].cluster_pads[1]= 1;
		}
	}
	//padtype 34
	//seg0
	for(int i=0;i<25;i++){
		if(i>=9&&i<=15){
			for(int k=0;k<24;k++){
				container[type34][0][i].th_par[k][0] = par_hijing_32_0m8[k][0];	
				container[type34][0][i].th_par[k][1] = par_hijing_32_0m8[k][1];	
			}
			container[type34][0][i].cluster_pads[0]= 0;
			container[type34][0][i].cluster_pads[1]= 1;
		}
	}
	//seg1
	for(int i=0;i<25;i++){
		if(i==15){
			for(int k=0;k<24;k++){
				container[type34][1][i].th_par[k][0] = par_hijing_32_01m8[k][0];	
				container[type34][1][i].th_par[k][1] = par_hijing_32_01m8[k][1];	
			}
			container[type34][1][i].cluster_pads[0]= 0;
			container[type34][1][i].cluster_pads[1]= 1;
			container[type34][1][i].cluster_pads[2]= 3;
		}else if(i>=9&&i<=14){
			for(int k=0;k<24;k++){
				container[type34][1][i].th_par[k][0] = par_hijing_32_01[k][0];	
				container[type34][1][i].th_par[k][1] = par_hijing_32_01[k][1];	
			}
			container[type34][1][i].cluster_pads[0]= 0;
			container[type34][1][i].cluster_pads[1]= 1;
		}
	}
	//seg2
	for(int i=0;i<25;i++){
		if(i>=22&&i<=24){
			for(int k=0;k<24;k++){
				container[type34][2][i].th_par[k][0] = par_hijing_32_01[k][0];	
				container[type34][2][i].th_par[k][1] = par_hijing_32_01[k][1];	
			}
			container[type34][2][i].cluster_pads[0]= 0;
			container[type34][2][i].cluster_pads[1]= 1;
		}
	}
	//seg3
	for(int i=0;i<25;i++){
		if(i>=16&&i<=18){
			for(int k=0;k<24;k++){
				container[type34][3][i].th_par[k][0] = par_hijing_32_01[k][0];	
				container[type34][3][i].th_par[k][1] = par_hijing_32_01[k][1];	
			}
			container[type34][3][i].cluster_pads[0]= 0;
			container[type34][3][i].cluster_pads[1]= 1;
		}
	}
	//seg4
	for(int i=0;i<25;i++){
		if(i==9){
			for(int k=0;k<24;k++){
				container[type34][4][i].th_par[k][0] = par_hijing_32_01m8[k][0];	
				container[type34][4][i].th_par[k][1] = par_hijing_32_01m8[k][1];	
			}
			container[type34][4][i].cluster_pads[0]= 0;
			container[type34][4][i].cluster_pads[1]= 1;
			container[type34][4][i].cluster_pads[2]= 2;
		}else if(i>=10&&i<=15){
			for(int k=0;k<24;k++){
				container[type32][4][i].th_par[k][0] = par_hijing_32_01[k][0];	
				container[type32][4][i].th_par[k][1] = par_hijing_32_01[k][1];	
			}
			container[type32][4][i].cluster_pads[0]= 0;
			container[type32][4][i].cluster_pads[1]= 1;
		}
	}
	//seg5
	for(int i=0;i<25;i++){
		if(i>=9&&i<=15){
			for(int k=0;k<24;k++){
				container[type32][5][i].th_par[k][0] = par_hijing_32_0m8[k][0];	
				container[type32][5][i].th_par[k][1] = par_hijing_32_0m8[k][1];	
			}
			container[type32][5][i].cluster_pads[0]= 0;
			container[type32][5][i].cluster_pads[1]= 1;
		}
	}
	//padtype 39
	for(int i=0;i<6;i++){
		for(int j=0;j<25;j++){
				for(int k=0;k<24;k++){
					container[type39][i][j].th_par[k][0] = container[type32][i][j].th_par[k][0];
					container[type39][i][j].th_par[k][1] = container[type32][i][j].th_par[k][1];
					if(i==5&&j>=22&&j<=24){
						container[type39][i][j].th_par[k][0] = par_hijing_64_019[k][0];
						container[type39][i][j].th_par[k][1] = par_hijing_64_019[k][1];
					}
					if(i==4&&j>=9&&j<=14){
						container[type39][i][j].th_par[k][0] = par_hijing_64_01[k][0];
						container[type39][i][j].th_par[k][1] = par_hijing_64_01[k][1];
					}
					if(i==3&&j==15){
						container[type39][i][j].th_par[k][0] = par_hijing_64_01m8[k][0];
						container[type39][i][j].th_par[k][1] = par_hijing_64_01m8[k][1];
					}
				}
				container[type39][i][j].cluster_pads[0]= container[type32][i][j].cluster_pads[0];
				container[type39][i][j].cluster_pads[1]= container[type32][i][j].cluster_pads[1];
				container[type39][i][j].cluster_pads[2]= container[type32][i][j].cluster_pads[2];
				container[type39][i][j].cluster_pads[3]= container[type32][i][j].cluster_pads[3];
		}
	}
	//padtype 40
	for(int i=0;i<6;i++){
		for(int j=0;j<25;j++){
				for(int k=0;k<24;k++){
					container[type40][i][j].th_par[k][0] = container[type32][i][j].th_par[k][0];
					container[type40][i][j].th_par[k][1] = container[type32][i][j].th_par[k][1];
					if(i==0&&j>=16&&j<=18){
						container[type40][i][j].th_par[k][0] = par_hijing_63_08m1[k][0];
						container[type40][i][j].th_par[k][1] = par_hijing_63_08m1[k][1];
					}
					if(i==1&&j>=10&&j<=15){
						container[type40][i][j].th_par[k][0] = par_hijing_63_0m1[k][0];
						container[type40][i][j].th_par[k][1] = par_hijing_63_0m1[k][1];
					}
					if(i==2&&j==9){
						container[type40][i][j].th_par[k][0] = par_hijing_63_0m1m9[k][0];
						container[type40][i][j].th_par[k][1] = par_hijing_63_0m1m9[k][1];
					}
				}
				container[type40][i][j].cluster_pads[0]= container[type32][i][j].cluster_pads[0];
				container[type40][i][j].cluster_pads[1]= container[type32][i][j].cluster_pads[1];
				container[type40][i][j].cluster_pads[2]= container[type32][i][j].cluster_pads[2];
				container[type40][i][j].cluster_pads[3]= container[type32][i][j].cluster_pads[3];
		}
	}
	//padtype 41
	for(int i=0;i<6;i++){
		for(int j=0;j<25;j++){
				for(int k=0;k<24;k++){
					container[type41][i][j].th_par[k][0] = container[type34][i][j].th_par[k][0];
					container[type41][i][j].th_par[k][1] = container[type34][i][j].th_par[k][1];
					if(i==5&&j>=22&&j<=24){
						container[type41][i][j].th_par[k][0] = par_hijing_64_019[k][0];
						container[type41][i][j].th_par[k][1] = par_hijing_64_019[k][1];
					}
					if(i==4&&j>=9&&j<=14){
						container[type41][i][j].th_par[k][0] = par_hijing_64_01[k][0];
						container[type41][i][j].th_par[k][1] = par_hijing_64_01[k][1];
					}
				}
				container[type41][i][j].cluster_pads[0]= container[type34][i][j].cluster_pads[0];
				container[type41][i][j].cluster_pads[1]= container[type34][i][j].cluster_pads[1];
				container[type41][i][j].cluster_pads[2]= container[type34][i][j].cluster_pads[2];
				container[type41][i][j].cluster_pads[3]= container[type34][i][j].cluster_pads[3];
		}
	}
	//padtype 42
	for(int i=0;i<6;i++){
		for(int j=0;j<25;j++){
				for(int k=0;k<24;k++){
					container[type42][i][j].th_par[k][0] = container[type34][i][j].th_par[k][0];
					container[type42][i][j].th_par[k][1] = container[type34][i][j].th_par[k][1];
					if(i==0&&j>=16&&j<=18){
						container[type42][i][j].th_par[k][0] = par_hijing_63_08m1[k][0];
						container[type42][i][j].th_par[k][1] = par_hijing_63_08m1[k][1];
					}
					if(i==1&&j>=10&&j<=15){
						container[type42][i][j].th_par[k][0] = par_hijing_63_0m1[k][0];
						container[type42][i][j].th_par[k][1] = par_hijing_63_0m1[k][1];
					}
				}
				container[type42][i][j].cluster_pads[0]= container[type34][i][j].cluster_pads[0];
				container[type42][i][j].cluster_pads[1]= container[type34][i][j].cluster_pads[1];
				container[type42][i][j].cluster_pads[2]= container[type34][i][j].cluster_pads[2];
				container[type42][i][j].cluster_pads[3]= container[type34][i][j].cluster_pads[3];
		}
	}

	if(verbose)cout<<"YWCutter::Init() end!"<<endl;
}


bool YWCutter::isGoodProjection(YWParticle &particle){
	if(fabs(particle.get_locx())>26.5) return false;
	float pphi = particle.get_hbdpphi();
	float locx = particle.get_locx();
	float pphi_d = pphi * 180./3.14159265;
	if((pphi_d>180.)&& (pphi_d<202.5)&&(locx>0)) return false;
	if (pphi_d>67.5 && pphi_d<112.5) return false;
	if (pphi_d>225. && pphi_d<270.) return false;
	if (pphi_d>-90. && pphi_d<-45.) return false;
	if (fabs ( locx ) > 26.5 ) return false;

	return true;
}

bool YWCutter::isCAElectron(YWParticle &particle){
	return true;
}

bool YWCutter::isHBDElectron(YWParticle &particle){
	float rt_padx =0;
	float rt_pady =0;
	double angle = atan2(particle.get_pady(), particle.get_padx()) * r2d;
	if(angle<0) angle += 360;
	int seg = (int) (angle/60.);
	rotate(seg, particle.get_padx(), particle.get_pady(), rt_padx, rt_pady);
	int ClassifyError = 0;

	//Do not classify region
	if(particle.get_padtype()==35 ||particle.get_padtype()==36||particle.get_padtype()==37||particle.get_padtype()==38){
	  particle.set_hbdcharge(particle.get_padcharge0());
	  particle.set_hbdsize(1);
	  particle.set_usedpad(0,particle.get_padkey0());
		if(particle.get_padcharge0()<get_threshold(particle.get_bbcq(),container[type35][0][0].th_par[particle.get_hbdsector()*2+particle.get_hbdside()][0],container[type35][0][0].th_par[particle.get_hbdsector()*2+particle.get_hbdside()][1])){
			return false;
		}else{
			return true;
		}
	}
	if(particle.get_padtype()==61 || particle.get_padtype()==63){
	  particle.set_hbdcharge(particle.get_padcharge0());
	  particle.set_hbdsize(1);
	  particle.set_usedpad(0,particle.get_padkey0());
		if(particle.get_padcharge0()<get_threshold(particle.get_bbcq(),container[type61][0][0].th_par[particle.get_hbdsector()*2][0],container[type61][0][0].th_par[particle.get_hbdsector()*2][1])){
			return false;
		}else{
			return true;
		}
	}

	if(particle.get_padtype()==62 || particle.get_padtype()==64){
	  particle.set_hbdcharge(particle.get_padcharge0());
	  particle.set_hbdsize(1);
	  particle.set_usedpad(0,particle.get_padkey0());
		if(particle.get_padcharge0()<get_threshold(particle.get_bbcq(),container[type62][0][0].th_par[particle.get_hbdsector()*2][0],container[type62][0][0].th_par[particle.get_hbdsector()*2][1])){
			return false;
		}else{
			return true;
		}
	}

	if(fabs(particle.get_locx())>26&&particle.get_hbdside()==1)
	{
	  particle.set_hbdcharge(particle.get_padcharge0());
	  particle.set_hbdsize(1);
	  particle.set_usedpad(0,particle.get_padkey0());
		if(particle.get_padcharge0()<get_threshold(particle.get_bbcq(),container[type31][0][0].th_par[particle.get_hbdsector()*2+particle.get_hbdside()][0],container[type31][0][0].th_par[particle.get_hbdsector()*2+particle.get_hbdside()][1])){
			return false;
		}else{
			return true;
		}
	}
	if(fabs(particle.get_locx())>26&&particle.get_hbdside()==0)
	{
	  particle.set_hbdcharge(particle.get_padcharge0());
	  particle.set_hbdsize(1);
	  particle.set_usedpad(0,particle.get_padkey0());
		if(particle.get_padcharge0()<get_threshold(particle.get_bbcq(),container[type34][0][0].th_par[particle.get_hbdsector()*2+particle.get_hbdside()][0],container[type34][0][0].th_par[particle.get_hbdsector()*2+particle.get_hbdside()][1])){
			return false;
		}else{
			return true;
		}
	}

	//Classify Area threshold 
	int area_bin = _classifyArea(particle.get_padtype(), seg,rt_padx,rt_pady,ClassifyError);
	if(ClassifyError == 1) {
		cout<<"YWCutter::isHBDElectron(): ClassifyError: sec "<<particle.get_hbdsector()<<" pdnm "<<particle.get_hbdpadnum()<<" type "<<particle.get_padtype()<<" "<<seg<<" "<<rt_padx<<" "<<rt_pady<<" locx "<<particle.get_locx()<<" locy "<<particle.get_locy()<<endl;
	}

	if(verbose) cout<<"YWCutter::isHBDElectron() sector "<<particle.get_hbdsector()<<" side "<<particle.get_hbdside()<<endl;
	if(verbose) cout<<"YWCutter::isHBDElectron() pphi "<<particle.get_hbdpphi()<<" pz "<<particle.get_hbdpz()<<endl;
	//charge_sum calculation
	float par0, par1;
	int u_pads[4];
	int t_mod = particle.get_hbdsector()*2 + particle.get_hbdside();
	int t_type = -999;
	switch(particle.get_padtype()){
		case 0: t_type = type0; break;
		case 1: t_type = type1; break;
		case 2: t_type = type2; break;
		case 11: t_type = type11; break;
		case 12: t_type = type12; break;
		case 31: t_type = type31; break;
		case 32: t_type = type32; break;
		case 33: t_type = type33; break;
		case 34: t_type = type34; break;
		case 35: t_type = type35; break;
		case 36: t_type = type36; break;
		case 37: t_type = type37; break;
		case 38: t_type = type38; break;
		case 61: t_type = type61; break;
		case 62: t_type = type62; break;
		case 63: t_type = type63; break;
		case 64: t_type = type64; break;
		default: cout<<"no such type"<<particle.get_padtype()<<endl;
	}
	int tmp_padnum = particle.get_hbdpadnum();
	if(tmp_padnum==20||tmp_padnum==37||tmp_padnum==54||tmp_padnum==71||tmp_padnum==105||tmp_padnum==122||tmp_padnum==139||tmp_padnum==156||tmp_padnum==173){
		t_type = type3;
	}
	if(tmp_padnum==26||tmp_padnum==43||tmp_padnum==60||tmp_padnum==77||tmp_padnum==111||tmp_padnum==128||tmp_padnum==145||tmp_padnum==162||tmp_padnum==180){
		t_type = type4;
	}
	if(tmp_padnum==88) t_type = type39;
	if(tmp_padnum==94) t_type = type40;
	if(tmp_padnum==3) t_type = type41;
	if(tmp_padnum==9) t_type = type42;

	if(verbose)cout<<"YWCutter:isHBDElectron() t_type "<<t_type<<endl;
	par0 = container[t_type][seg][area_bin].th_par[t_mod][0];
	par1 = container[t_type][seg][area_bin].th_par[t_mod][1];
	u_pads[0] = container[t_type][seg][area_bin].cluster_pads[0];
	u_pads[1] = container[t_type][seg][area_bin].cluster_pads[1];
	u_pads[2] = container[t_type][seg][area_bin].cluster_pads[2];
	u_pads[3] = container[t_type][seg][area_bin].cluster_pads[3];
	if(verbose)
	{
		cout<<"YWCutter::isHBDElectron() ";
		cout<<t_type<<" ";
		cout<<seg<<" ";
		cout<<area_bin<<" ";
		cout<<par0<<" ";
		cout<<par1<<" ";
		cout<<u_pads[0]<<" ";
		cout<<u_pads[1]<<" ";
		cout<<u_pads[2]<<" ";
		cout<<u_pads[3]<<endl;
	}

	float charge_sum = 0;
	int sum_counter = 0;
	int size_counter = 0;
	float charge[10];
	charge[0] = particle.get_padcharge0();
	charge[1] = particle.get_padcharge1();
	charge[2] = particle.get_padcharge2();
	charge[3] = particle.get_padcharge3();
	charge[4] = particle.get_padcharge4();
	charge[5] = particle.get_padcharge5();
	charge[6] = particle.get_padcharge6();
	charge[7] = particle.get_padcharge7();
	charge[8] = particle.get_padcharge8();
	charge[9] = particle.get_padcharge9();
	for(int i=0;i<4;i++){
		if(u_pads[i] == -999) break;
		size_counter++;
		if (u_pads[i] == 0) particle.set_usedpad(u_pads[i],particle.get_padkey0());
		if (u_pads[i] == 1) particle.set_usedpad(u_pads[i],particle.get_padkey1());
		if (u_pads[i] == 2) particle.set_usedpad(u_pads[i],particle.get_padkey2());
		if (u_pads[i] == 3) particle.set_usedpad(u_pads[i],particle.get_padkey3());
		if(charge[u_pads[i]]>-999 ){
			charge_sum += charge[u_pads[i]];
			sum_counter ++;
		}	
	}
	if(sum_counter==0) charge_sum = -999;
	particle.set_hbdcharge(charge_sum);
	particle.set_hbdsize(size_counter);
	
	if(verbose) cout<<"YWCutter::isHbdElectron() charge_sum "<<charge_sum<<endl;
	if(verbose) cout<<"YWCutter::isHbdElectron() threshold "<<get_threshold(particle.get_bbcq(),par0,par1)<<endl;
	if(charge_sum <=  get_threshold(particle.get_bbcq(),par0,par1)) return false;

	return true;
}

float YWCutter::get_threshold(float bbcq,float par0,float par1){
	float threshold = -9999;	
	if(bbcq<2000)threshold = par0 + par1 * bbcq;
	else threshold = par0 + par1 * 2000;
	//convert according to rejection
	float correction = 1;
	correction  = 0.1 * rejection;
	threshold = threshold * correction;

	return threshold;
}

void YWCutter::rotate(int seg, float t_padx, float t_pady, float &rt_padx, float &rt_pady){
	rt_padx = t_padx*cos(-60.*seg*d2r - 30.*d2r) - t_pady*sin(-60.*seg*d2r -30*d2r);
	rt_pady = t_padx*sin(-60.*seg*d2r - 30.*d2r) + t_pady*cos(-60.*seg*d2r -30*d2r);
}

bool YWCutter::RejectBothParticle(YWParticle &particle, YWParticle &sparticle){
	if(verbose) cout<<"YWCutter::RejectBothParticle() begin"<<endl;
		
	return false;

	if(fabs(particle.get_hbdpz())>26.5) return true;
	if(fabs(sparticle.get_hbdpz())>26.5) return true;

	float pphi = particle.get_hbdpphi();
	float pz = particle.get_hbdpz();
	if((pphi*180./3.1416>225.)&&(pphi*180./3.1416<247.5)) return true;
	if((pphi*180./3.1416>-67.5)&&(pphi*180./3.1416<-45)) return true;
	if((pphi*180./3.1416>180.)&&(pphi*180./3.1416<202.5)&&(pz>0)) return true;
	float pphi_s = sparticle.get_hbdpphi();
	float pz_s = sparticle.get_hbdpz();
	if((pphi_s*180./3.1416>225.)&&(pphi_s*180./3.1416<247.5)) return true;
	if((pphi_s*180./3.1416>-67.5)&&(pphi_s*180./3.1416<-45)) return true;
	if((pphi_s*180./3.1416>180.)&&(pphi_s*180./3.1416<202.5)&&(pz_s>0)) return true;

	float edge_phi[14]={-67.5,-45,-22.5,0,22.5,45,67.5,112.5,135,157.5,180,202.5,225,247.5};
	for(int i=0;i<14;i++){
		edge_phi[i] = edge_phi[i]*3.141592/180.;
	}
	float edge_z[3]={-26.5,0,26.5};
	//Cut in phi direction
	int reject_flag_phi =0;
	for(int j=0;j<14;j++){
		if(fabs(pphi-edge_phi[j])<hbdpphi_cut)
		{
					reject_flag_phi =1;
					break;
		}
	}
	if(reject_flag_phi==1) return true;
	
	//Cut in z direction
	int reject_flag_z =0;
	for(int j=0;j<3;j++){
		if(fabs(pz-edge_z[j])<hbdpz_cut)
		{
					reject_flag_z =1;
					break;
		}
	}
	if(reject_flag_z==1) return true;


	return false;
}
int YWCutter::_classifyArea(int pad_type, int seg, float rt_padx, float rt_pady, int &ClassifyError){
	if(verbose) cout<<"_classifyArea(): x "<<rt_padx<<" y "<<rt_pady<<endl;

	int index =0;
	int AREA_BINS = 4;
	int RAREA_BINS = 5;
	
	double typ_length;
	int TMP_BINS;
	if((pad_type==32||pad_type==31)&&(seg==0||seg==5)){
		TMP_BINS = RAREA_BINS;
		typ_length = 1.865;
	}else if((pad_type==33||pad_type==34)&&(seg==2||seg==3)){
		TMP_BINS = RAREA_BINS;
		typ_length = 1.865;
	}else if(pad_type==11 &&(seg==0||seg==2)){
		TMP_BINS = RAREA_BINS;
		typ_length = 1.785;
	}else if(pad_type==11 &&(seg==3||seg==4||seg==5)){
		TMP_BINS = AREA_BINS;
		typ_length = 1.7;
	}else if(pad_type==12 &&(seg==3||seg==5)){
		TMP_BINS = RAREA_BINS;
		typ_length = 1.785;
	}else if(pad_type==12 &&(seg==0||seg==1||seg==2)){
		TMP_BINS = AREA_BINS;
		typ_length = 1.7;
	}else if(pad_type==2 &&seg==1){
		TMP_BINS = RAREA_BINS;
		typ_length = 2.0;
	}else if(pad_type==2 &&(seg==0||seg==2)){
		TMP_BINS = AREA_BINS;
		typ_length = 1.452;
	}else if(pad_type==1 &&seg==4){
		TMP_BINS = RAREA_BINS;
		typ_length = 2.0;
	}else if(pad_type==1 &&(seg==5||seg==3)){
		TMP_BINS = AREA_BINS;
		typ_length = 1.452;
	}else{
		TMP_BINS = AREA_BINS;
		typ_length = 1.356;
	}
	double grid_size = typ_length/sqrt(3)*2/TMP_BINS;
	int N_TOT = TMP_BINS* TMP_BINS;

	//Modules are tilted in data, so the padx, pady which did not 
	//exist in simulation appeares.. Correct for it.
	double correction_out = 1;
	if(rt_padx >= typ_length){
		correction_out = (typ_length-0.01)/rt_padx;
		rt_padx = correction_out*rt_padx;
		rt_pady = correction_out*rt_pady;
	}


	int bin_x = (int)(rt_padx/(grid_size*sqrt(3)/2.));
	if(verbose) cout<<"_classifyArea(): bin_x "<<bin_x<<endl;
	double ux = sqrt(3)/2.*grid_size*bin_x - rt_padx;
	double uy = 1./2*grid_size*bin_x - rt_pady;
	double vx = sqrt(3)/2.*grid_size*(bin_x+1) - rt_padx;
	double vy = 1./2*grid_size*(bin_x+1) - rt_pady;
	int bin_diamond = (int)(1./2*fabs(ux*vy - uy*vx)/(sqrt(3)/4.*grid_size*grid_size));
	if(verbose) cout<<"_classifyArea(): bin_diamond "<<bin_diamond<<endl;

	double wx = rt_padx - sqrt(3)/2.*grid_size*bin_x;
	double wy = rt_pady - (1./2* grid_size*bin_x - grid_size*bin_diamond);
	double tmp = sqrt(3)/2.*wy + 1./2*wx;
	int which_half;
	if(tmp > 0) which_half = 0;
	else which_half =1;
	if(verbose) cout<<"_classifyArea(): which_half "<<which_half<<endl;
	index = bin_x*bin_x + 2 * bin_diamond + which_half;
	if(verbose) cout<<"_classifyArea(): index "<<index<<endl;

	if(index<0 || index > N_TOT ){
		ClassifyError = 1;
	}
	return index;
}

void YWCutter::SetRejection(float a){
	if(a < 1 || a > 25){
		cerr<<"YWCutter::SetRejection(): such rejection is not supported..."<<endl;
		exit(1);
	}
	rejection = a;
}

void YWCutter::SetRunnumber(int run){
  
  runnumber = run;
  cout << "YWCutter::SetRunnumber: runnumber = " << runnumber <<endl;
  
}

void YWCutter::SetMCFlag(int mcflag){
  
  mc_flag = mcflag;
  cout << "YWCutter::SetMCFlag: mc_flag = " << mc_flag <<endl;
  
}

void YWCutter::SetEventRejection(int centrality){
  
  int rej = 1;
  if      ( centrality >= 60 ) rej = 25;//eff>= 90%
  else if ( centrality >= 50 ) rej = 20;//eff = 90%
  else if ( centrality >= 40 ) rej = 15;//eff = 90%
  else if ( centrality >= 30 ) rej = 11;//eff = 89%
  else if ( centrality >= 20 ) rej = 10;//eff = 84%
  else if ( centrality >= 10 ) rej = 9; //eff = 77%
  else if ( centrality >=  0 ) rej = 8; //eff = 71%
  else rej = 25;

  rejection = rej;
}

bool YWCutter::isSingleElectron(int centrality,int size,float charge){

  float threshold = -9999;
  bool single = false;

  switch (size){
  
  case 1:
    if      ( centrality >= 60 ) threshold = 26.0;
    else if ( centrality >= 50 ) threshold = 25.0;
    else if ( centrality >= 40 ) threshold = 24.0;
    else if ( centrality >= 30 ) threshold = 23.0;
    else if ( centrality >= 20 ) threshold = 23.0;
    else if ( centrality >= 10 ) threshold = 25.0;
    else if ( centrality >= 0  ) threshold = 26.0;
    break;
    
  case 2:
    if      ( centrality >= 70 ) threshold = 30.0;
    else if ( centrality >= 60 ) threshold = 28.0;
    else if ( centrality >= 50 ) threshold = 28.0;
    else if ( centrality >= 40 ) threshold = 27.0;
    else if ( centrality >= 30 ) threshold = 26.0;
    else if ( centrality >= 20 ) threshold = 26.0;
    else if ( centrality >= 10 ) threshold = 29.0;
    else if ( centrality >= 0  ) threshold = 35.0;
    break;

  default:
    if      ( centrality >= 80 ) threshold = 33.0;
    else if ( centrality >= 70 ) threshold = 33.0;
    else if ( centrality >= 60 ) threshold = 32.0;
    else if ( centrality >= 50 ) threshold = 32.0;
    else if ( centrality >= 40 ) threshold = 30.0;
    else if ( centrality >= 30 ) threshold = 30.0;
    else if ( centrality >= 20 ) threshold = 30.0;
    else if ( centrality >= 10 ) threshold = 36.0;
    else if ( centrality >= 0  ) threshold = 42.0;
    break;
  }

  if (charge < threshold) single = true;
  
  return single;
}

bool YWCutter::isMissingElectron(int centrality,int size,float charge,int neighbor){
  //having two categories of the first neighbors
  
  float threshold = 9999;
  bool missing = false;

  switch (neighbor){

  case 0:  //closer than 2.6 cm
   
    switch (size){
    case 1:
      if      ( centrality >= 40 ) threshold = 4.0;
      else if ( centrality >= 30 ) threshold = 4.0;
      else if ( centrality >= 20 ) threshold = 5.0;
      else if ( centrality >= 10 ) threshold = 7.0;
      else if ( centrality >= 0  ) threshold = 9.0;
      break;
      
    case 2:
      if      ( centrality >= 60 ) threshold = 4.0;
      else if ( centrality >= 50 ) threshold = 4.0;
      else if ( centrality >= 40 ) threshold = 4.0;
      else if ( centrality >= 30 ) threshold = 4.0;
      else if ( centrality >= 20 ) threshold = 5.0;
      else if ( centrality >= 10 ) threshold = 7.0;
      else if ( centrality >= 0  ) threshold = 9.0;
      break;
      
    default:
      if      ( centrality >= 70 ) threshold = 5.0;
      else if ( centrality >= 60 ) threshold = 5.0;
      else if ( centrality >= 50 ) threshold = 5.0;
      else if ( centrality >= 40 ) threshold = 5.0;
      else if ( centrality >= 30 ) threshold = 6.0;
      else if ( centrality >= 20 ) threshold = 8.0;
      else if ( centrality >= 10 ) threshold = 8.0;
      else if ( centrality >= 0  ) threshold = 9.0;
      break;
    }
    break;

  case 1: // between 2.6 - 4.0 cm
    
    switch (size){
    case 1:
      if      ( centrality >= 40 ) threshold = 5.0;
      else if ( centrality >= 30 ) threshold = 5.0;
      else if ( centrality >= 20 ) threshold = 6.0;
      else if ( centrality >= 10 ) threshold = 8.0;
      else if ( centrality >= 0  ) threshold = 11.0;
      break;
      
    case 2:
      if      ( centrality >= 60 ) threshold = 5.0;
      else if ( centrality >= 50 ) threshold = 5.0;
      else if ( centrality >= 40 ) threshold = 5.0;
      else if ( centrality >= 30 ) threshold = 5.0;
      else if ( centrality >= 20 ) threshold = 6.0;
      else if ( centrality >= 10 ) threshold = 9.0;
      else if ( centrality >= 0  ) threshold = 12.0;
      break;
      
    default:
      if      ( centrality >= 70 ) threshold = 6.0;
      else if ( centrality >= 60 ) threshold = 6.0;
      else if ( centrality >= 50 ) threshold = 6.0;
      else if ( centrality >= 40 ) threshold = 6.0;
      else if ( centrality >= 30 ) threshold = 6.0;
      else if ( centrality >= 20 ) threshold = 7.0;
      else if ( centrality >= 10 ) threshold = 8.0;
      else if ( centrality >= 0  ) threshold = 9.0;
      break;
    }
    break;

  case 2: //second neighbor
    
    switch (size){
    case 1:
      if      ( centrality >= 40 ) threshold = 6.0;
      else if ( centrality >= 30 ) threshold = 8.0;
      else if ( centrality >= 20 ) threshold = 11.0;
      else if ( centrality >= 10 ) threshold = 13.0;
      else if ( centrality >= 0  ) threshold = 16.0;
      break;
      
    case 2:
      if      ( centrality >= 60 ) threshold = 6.0;
      else if ( centrality >= 50 ) threshold = 6.0;
      else if ( centrality >= 40 ) threshold = 6.0;
      else if ( centrality >= 30 ) threshold = 7.0;
      else if ( centrality >= 20 ) threshold = 8.0;
      else if ( centrality >= 10 ) threshold = 11.0;
      else if ( centrality >= 0  ) threshold = 15.0;
      break;
      
    default:
      if      ( centrality >= 70 ) threshold = 6.0;
      else if ( centrality >= 60 ) threshold = 6.0;
      else if ( centrality >= 50 ) threshold = 6.0;
      else if ( centrality >= 40 ) threshold = 6.0;
      else if ( centrality >= 30 ) threshold = 7.0;
      else if ( centrality >= 20 ) threshold = 9.0;
      else if ( centrality >= 10 ) threshold = 13.0;
      else if ( centrality >= 0  ) threshold = 14.0;
      break;
    }
    break;

  default:
    break;
  }

  if (charge > threshold) missing = true;
  
  return missing;
}
