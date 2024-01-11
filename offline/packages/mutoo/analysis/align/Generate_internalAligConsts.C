//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//internal alignment file generation code by MinJung Kweon
//read original internal alignment file and two new alignment file including ds, dr offsets for each arms
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 

#include <iomanip>

void Generate_internalAligConsts(){
	gROOT->Reset();
	//gStyle->SetOptStat(0000);
	

	ifstream fin0("mut.internalAligConsts.dat.run4AuAu.1st");
	ifstream fin1("south_centroid_newcorrection.txt");
	ifstream fin2("north_centroid_newcorrection.txt");

	//used for run3pp test repass
	//ifstream fin1("run3pp_south_centroid.txt");
	//ifstream fin2("run3pp_north_centroid.txt");

	ofstream txtout1("mut.internalAligConsts.dat.run4AuAu.2nd");

        int arm=0, sta=0, oct=0, halfoct=0, gap=0, plane=0;
	double ds=0, dr=0, dz=0, roll=0, pitch=0, yaw=0, dExpansion=0; 
	double align_val[2][3][8][2][2][3][7] = {0};

	while ( fin0 >> arm >> sta >> oct >> halfoct >> plane >> gap >> ds >> dr >> dz >> roll >> pitch >> yaw >> dExpansion){
	        cout << arm <<setw(3)<< sta <<setw(3)<< oct <<setw(3)<< halfoct <<setw(3)<< plane <<setw(3)<< gap <<setw(3)<< ds <<setw(10)<< dr <<setw(10)<< dz <<setw(10)<< roll <<setw(10)<< pitch <<setw(10)<< yaw <<setw(10)<< dExpansion <<endl;
		align_val[arm][sta][oct][halfoct][plane][gap][0] = ds;         // ds
		align_val[arm][sta][oct][halfoct][plane][gap][1] = dr;         // dr
		//align_val[arm][sta][oct][halfoct][plane][gap][2] = dz;         // dz
		//align_val[arm][sta][oct][halfoct][plane][gap][3] = roll;       // roll 
		//align_val[arm][sta][oct][halfoct][plane][gap][4] = pitch;      // pitch
		//align_val[arm][sta][oct][halfoct][plane][gap][5] = yaw;        // yaw
		//align_val[arm][sta][oct][halfoct][plane][gap][6] = dExpansion; // dExpansion
	}

	char halfnameS[50], halfnameN[50];
	int ith_south=0, ith_north=0;
	double dsdr=0;
        double align_val_new[2][3][8][2][2] = {0};

	//******************************************************************
	//Since we are applying the shifts at station 3 and the
	//	fit residuals are computed at station 2, a scale factor has
	//	to be applied to the residuals. This factor is somewhat
	//	larger than 2X and is computed by : factor = (Z3-Z1)/(Z2-Z1)
	//
	//	south :
	//	I put z1=-189.0, z2=-309.0, z3=-466.2
	//	(Z3-Z1)/(Z2-Z1)=2.310
	//	north :
	//	I put z1=189.0, z2=362.0, z3=616.5
	//	(Z3-Z1)/(Z2-Z1)=2.471
	//
	//******************************************************************

	while ( fin1 >> halfnameS >> dsdr ){
		if(ith_south>=0 && ith_south<8)        align_val_new[0][2][ith_south][0][0] = 2.310*dsdr;    // ds for halfoct=0
		else if(ith_south>=8 && ith_south<16)  align_val_new[0][2][ith_south-8][1][0] = 2.310*dsdr;  // ds for halfoct=1
		else if(ith_south>=16 && ith_south<24) align_val_new[0][2][ith_south-16][0][1] = 2.310*dsdr; // dr for halfoct=0
		else if(ith_south>=24 && ith_south<32) align_val_new[0][2][ith_south-24][1][1] = 2.310*dsdr; // dr for halfoct=1
			
		ith_south++;
	}
	while ( fin2 >> halfnameN >> dsdr ){
		if(ith_north>=0 && ith_north<8)        align_val_new[1][2][ith_north][0][0] = -2.471*dsdr;   // north arm ds(dphi*r) sign correction
		else if(ith_north>=8 && ith_north<16)  align_val_new[1][2][ith_north-8][1][0] = -2.471*dsdr; // north arm ds(dphi*r) sign correction
		else if(ith_north>=16 && ith_north<24) align_val_new[1][2][ith_north-16][0][1] = 2.471*dsdr;   // dr for halfoct=0
		else if(ith_north>=24 && ith_north<32) align_val_new[1][2][ith_north-24][1][1] = 2.471*dsdr;   // dr for halfoct=1
			
		ith_north++;
	}

	double new_ds=0, new_dr=0;

	for (int iarm=0; iarm<2; iarm++){
		for (int ioct=0; ioct<8; ioct++){ // no station loop since now we just take care of station2 offset
			for (int ihalfoct=0; ihalfoct<2; ihalfoct++){
				for (int iplane=0; iplane<2; iplane++) {
					for (int igap=0; igap<3; igap++){
						new_ds = align_val[iarm][2][ioct][ihalfoct][iplane][igap][0] + align_val_new[iarm][2][ioct][ihalfoct][0];
						new_dr = align_val[iarm][2][ioct][ihalfoct][iplane][igap][1] + align_val_new[iarm][2][ioct][ihalfoct][1];

						cout << iarm <<setw(2)<< 2 <<setw(2)<< ioct <<setw(2)<< ihalfoct <<setw(2)<< iplane <<setw(2)<< igap <<setw(15)<< new_ds <<setw(15)<< new_dr <<setw(15)<< "0.000000e+00" <<setw(15)<< "0.000000e+00" <<setw(15)<< "0.000000e+00" <<setw(15)<< "0.000000e+00" <<setw(15)<< "0.000000e+00"<<endl;
						txtout1 << iarm <<setw(2)<< 2 <<setw(2)<< ioct <<setw(2)<< ihalfoct <<setw(2)<< iplane <<setw(2)<< igap <<setw(15)<< new_ds <<setw(15)<< new_dr <<setw(15)<< "0.000000e+00" <<setw(15)<< "0.000000e+00" <<setw(15)<< "0.000000e+00" <<setw(15)<< "0.000000e+00" <<setw(15)<< "0.000000e+00"<<endl;



					}
				}
			}
		}
	}

	txtout1->close();

}
