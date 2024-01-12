#include <Run12CuAu200GeVTOF_PC3_Matching.h>
#include <getClass.h>
#include <PHGlobal.h>
#include <PHCompositeNode.h>
#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <RunHeader.h>
#include <iostream>



using namespace std;


    static int bad_tof_slat_east[269] = {  0, 15, 47, 48, 85, 97, 98, 99,102,103,  111,125,142,143,151,175,214,248,251,253,  255,292,304,305,306,310,311,335,339,362,  368,392,393,394,395,396,397,398,399,401,  409,410,411,412,413,414,415,447,465,479,
					 480,481,482,483,484,485,486,487,488,489,  490,491,492,493,494,495,496,497,498,499,  500,501,502,503,504,505,506,507,508,509,  510,511,519,539,543,575,576,577,578,579,  580,581,582,583,584,585,586,587,588,589,
					 590,591,592,593,594,595,596,597,598,599,  600,601,602,603,604,605,606,607,623,628,  639,655,672,673,674,675,676,677,678,679,  680,681,682,683,684,685,686,687,688,689,  690,691,692,693,694,695,696,697,698,699,
					 700,701,702,703,719,728,729,730,732,734,  735,739,742,751,764,765,767,800,802,803,  820,831,835,904,905,906,907,908,909,910,  911,919,930,943,                               // missing or poor timing characteristics
					   1,  2, 16, 17, 18, 19, 32, 33, 34, 35,   49, 50, 51, 64, 65, 66, 79, 80, 81, 82,   83,112,144,176,179,192,208,224,240,272,  302,303,319,334,336,351,367,383,384,400,  416,431,432,433,448,463,464,528,560,716,
					 717,718,720,733,748,749,750,752,766,768,  783,784,785,798,799,814,815,816,830,832,  846,847,848,862,863,864,879,880,881,896,  912,913,928,944,945                            // poor matching characteristics
				   
                                };


    static int bad_tof_slat_west[102] = {  3,  4,  5,  6,  7, 12, 13, 14, 15, 19,   20, 21, 22, 23, 28, 29, 30, 31, 36, 37,   38, 39, 44, 45, 46, 47, 52, 53, 54, 55,   60, 61, 62, 63,161,225,320,372,394,460,  498, // TOFW missing slats
					  79,140,143,172,175,376,377,378,381,382,  383,                                                                                                                               // TOFW slats with very poor timing characteristics
					   1,  2, 64, 65, 66, 67, 68, 69, 79,101,  128,140,143,187,188,189,190,191,192,251,  252,253,254,255,256,257,258,259,260,261,  320,321,322,323,324,325,376,377,384,443,       // TOFW bad matching characteristics
					 444,445,446,447,448,507,508,509,510,511                                                                                                                                      // TOFW bad matching characteristics
                                };



Run12CuAu200GeVTOF_PC3_Matching::Run12CuAu200GeVTOF_PC3_Matching(const string &name) : Recalibrator(name)
{   baseclasses.insert("PHCentralTrackv23");
    baseclasses.insert("PHCentralTrackv24");
    return;
}


int Run12CuAu200GeVTOF_PC3_Matching::isValidRun(const int run_num) const
{   if (run_num >= 372403  &&  run_num <= 377310) {   return 1;    }  //valid runs are from 372403 to 377310
    return 0;
}


int Run12CuAu200GeVTOF_PC3_Matching::InitRun(PHCompositeNode *topNode)
{   RunHeader* d_run = findNode::getClass<RunHeader>(topNode, "RunHeader");
    if(!d_run)                            {   cout<<PHWHERE<<" RunHeader not found"<< endl;	         return  0;   }
    run_num = d_run->get_RunNumber();
    if(!isValidRun(run_num))              {   cout<<" Not Valid Run! "<<endl;	                         return -1;   }


    init_pc3_fit_pars(run_num);
    init_tofe_fit_pars(run_num);
    init_tofw_fit_pars(run_num);

//    init_pc3_fit_pars_II();
//    init_tof_fit_pars_II();

    return 0;
}


int Run12CuAu200GeVTOF_PC3_Matching::process_event(PHCompositeNode *topNode)
{   PHGlobal *global = findNode::getClass<PHGlobal>(topNode,"PHGlobal");                     if(!global) return  0;
    int cent = (int)global->getCentrality();
    PHCentralTrack *track = findNode::getClass<PHCentralTrack>(topNode, inputnodename.c_str());
    if(!track)                            {   cout<<PHWHERE<<"Could not find PHCentralTrack!"<<endl;	 return -1;    }
  
    int npart = track->get_npart();
  
    for(int i=0;i < npart; i++)
    {    
	PHSnglCentralTrack* sngltrk = track->get_track(i);
    
	float mom       = -9999;
	float pt        = -9999;
	float zed       = -9999;
	int   dcarm     = -9999;
	int   charge    = -9999;
	float pc3dphi   = -9999;
	float pc3dz     = -9999;
	float tofdphi   = -9999;
	float tofdz     = -9999;
	int   slat      = -9999;

	float tofsdphi  = -9999;
	float tofsdz    = -9999;
	float pc3sdphi  = -9999;
	float pc3sdz    = -9999;



	mom       = sngltrk->get_mom();
	pt        = mom*sin(sngltrk->get_the0());
	zed       = sngltrk->get_zed();
	dcarm     = sngltrk->get_dcarm();
	charge    = sngltrk->get_charge();
	pc3dphi   = sngltrk->get_pc3dphi();
	pc3dz     = sngltrk->get_pc3dz();



	if(dcarm==0)
	{
	    tofdphi    = sngltrk->get_tofdphi();
	    tofdz      = sngltrk->get_tofdz();
	    slat       = sngltrk->get_slat();
	}
	if(dcarm==1)
	{
	    tofdphi    = sngltrk->get_tofwdphi();
	    tofdz      = sngltrk->get_tofwdz();
	    slat       = sngltrk->get_striptofw();
	}
	


	int iZed    = -9999;

	if     (zed> -70 && zed<=-56)     iZed = 0; 		else if(zed> -56 && zed<=-42)     iZed = 1;
	else if(zed> -42 && zed<=-28)     iZed = 2;		else if(zed> -28 && zed<=-14)     iZed = 3;
	else if(zed> -14 && zed<   0)     iZed = 4;		else if(zed>=  0 && zed<  14)     iZed = 5;
	else if(zed>= 14 && zed<  28)     iZed = 6;		else if(zed>= 28 && zed<  42)     iZed = 7;
	else if(zed>= 42 && zed<  56)     iZed = 8; 		else if(zed>= 56 && zed<  70)     iZed = 9;



	int iCent   = -9999;

	     if(cent > -1  && cent < 10) iCent = 0;
	else if(cent >= 10 && cent < 20) iCent = 1;		else if(cent >= 20 && cent < 30) iCent = 2;
	else if(cent >= 30 && cent < 40) iCent = 3;	 	else if(cent >= 40 && cent < 50) iCent = 4;
	else if(cent >= 50 && cent < 60) iCent = 5;		else if(cent >= 60 && cent < 70) iCent = 6;
	else if(cent >= 70 && cent < 80) iCent = 7;	 	else if(cent >= 80 && cent <101) iCent = 8;



	if (iZed>-1  &&  iZed<10  &&  iCent>-1  &&  iCent<9)
	{
	    if(pc3dphi > -9999  &&  pc3dz > -9999)
	    {
		pc3sdphi = pc3sdphi_func(charge, dcarm, iCent, iZed, pt, pc3dphi);
		pc3sdz   = pc3sdz_func  (charge, dcarm, iCent, iZed, pt, pc3dz);
		
//		pc3sdphi = pc3sdphi_func_II(charge, dcarm, iCent, iZed, pt, pc3sdphi);
//		pc3sdz   = pc3sdz_func_II  (charge, dcarm, iCent, iZed, pt, pc3sdz);
		
		sngltrk->set_pc3sdphi(pc3sdphi);
		sngltrk->set_pc3sdz(pc3sdz);			
	    }
	    else
	    {
		sngltrk->set_pc3sdphi(-9999);
		sngltrk->set_pc3sdz(-9999);
	    }

	    
	    if(tofdphi > -9999  &&  tofdz > -9999)
	    {	
		if((dcarm==0  &&  !(SlatCheck(bad_tof_slat_east,slat,0,268)))  ||  (dcarm==1  &&  !(SlatCheck(bad_tof_slat_west,slat,0,101))))
		{

		    if(charge > 0)
		    {
			if(dcarm==0)
			{
			    if(tofdphi>-9999)    tofdphi = tofdphi - mean_tofe_offset[0*960+slat];    //pos east
			    if(tofdz  >-9999)    tofdz   = tofdz   - mean_tofe_offset[1*960+slat];
			}
			else if(dcarm==1)
			{
			    if(tofdphi>-9999)    tofdphi = tofdphi - mean_tofw_offset[0*512+slat];    //pos west
			    if(tofdz  >-9999)    tofdz   = tofdz   - mean_tofw_offset[1*512+slat];
			}
		    }
		    else if(charge < 0  &&  charge > -999)
		    {
			if(dcarm==0)
			{
			    if(tofdphi>-9999)    tofdphi = tofdphi - mean_tofe_offset[2*960+slat];    //neg east
			    if(tofdz  >-9999)    tofdz   = tofdz   - mean_tofe_offset[3*960+slat];
			}
			else if(dcarm==1)
			{
			    if(tofdphi>-9999)    tofdphi = tofdphi - mean_tofw_offset[2*512+slat];    //neg west
			    if(tofdz  >-9999)    tofdz   = tofdz   - mean_tofw_offset[3*512+slat];
			}
		    }

		    tofsdphi = tofsdphi_func(charge, dcarm, iCent, iZed, pt, tofdphi);
		    tofsdz   = tofsdz_func  (charge, dcarm, iCent, iZed, pt, tofdz);
		    
//		    tofsdphi = tofsdphi_func_II(charge, dcarm, iCent, iZed, pt, tofsdphi);  //second pass
//		    tofsdz   = tofsdz_func_II  (charge, dcarm, iCent, iZed, pt, tofsdz);
		    
		    if(dcarm==0)
		    {
			sngltrk->set_tofsdphi(tofsdphi);
			sngltrk->set_tofsdz(tofsdz);
		    }
		    else if(dcarm==1)
		    {
			sngltrk->set_tofwsdphi(tofsdphi);
			sngltrk->set_tofwsdz(tofsdz);
		    }
		}
	    }
	    else
	    {
		sngltrk->set_tofsdphi(-9999);
		sngltrk->set_tofsdz(-9999);
		sngltrk->set_tofwsdphi(-9999);
		sngltrk->set_tofwsdz(-9999);
	    }
	}
	else
	{
	    sngltrk->set_pc3sdphi(-9999);
	    sngltrk->set_pc3sdz(-9999);
	    sngltrk->set_tofsdphi(-9999);
	    sngltrk->set_tofsdz(-9999);
	}
    }       //npart
    return 0;
}




bool Run12CuAu200GeVTOF_PC3_Matching::SlatCheck(int array[], int slat, int first, int last)  // searches 'array[]' for value 'slat' using [double] recursion, very fast method
{
    int midPoint;
    if (first > last)	                return false;
    midPoint = (first + last) / 2;
    if (array[midPoint] == slat)	return true;
    if (array[midPoint] < slat)	        return SlatCheck(array, slat, midPoint + 1, last);
    else	                        return SlatCheck(array, slat, first, midPoint - 1);
}



double Run12CuAu200GeVTOF_PC3_Matching::pc3sdphi_func(float charge, short dcarm, int iCent, int iZed, float pt, float pc3dphi)
{
    if(pc3dphi>-9999)
    {

	int iPt = -9999;

	     if(pt>=0.20 && pt<0.35) iPt=0;
	else if(pt>=0.35 && pt<0.45) iPt=1;
	else if(pt>=0.45 && pt<0.55) iPt=2;
	else if(pt>=0.55 && pt<0.70) iPt=3;
	else if(pt>=0.70 && pt<0.90) iPt=4;
	else if(pt>=0.90 && pt<1.10) iPt=5;
	else if(pt>=1.10 && pt<1.30) iPt=6;
	else if(pt>=1.30 && pt<1.50) iPt=7;
	else if(pt>=1.50 && pt<1.70) iPt=8;
	else if(pt>=1.70 && pt<1.90) iPt=9;
	else if(pt>=1.90 && pt<2.10) iPt=10;
	else if(pt>=2.10 && pt<2.30) iPt=11;
	else if(pt>=2.30 && pt<2.50) iPt=12;
	else if(pt>=2.50 && pt<2.80) iPt=13;
	else if(pt>=2.80 && pt<3.20) iPt=14;
	else if(pt>=3.20           ) iPt=15;

	     if(pt>4.6) pt = 4.6;
	
	int iDet = -9999;
	
	if     (dcarm==0 && charge>0)   {   iDet = 0;	}
	else if(dcarm==0 && charge<0)   {   iDet = 4;   }
	else if(dcarm==1 && charge>0)   {   iDet = 1;	}
	else if(dcarm==1 && charge<0)   {   iDet = 5;	}
	else return -9999;
	

	if(iPt<15 && iPt > -1)
	{
	    double mean  = pc3_point_lines[iCent][iDet][iZed][iPt][0]*pt + pc3_point_lines[iCent][iDet][iZed][iPt][1];
	    double sigma = pc3_point_lines[iCent][iDet][iZed][iPt][2]*pt + pc3_point_lines[iCent][iDet][iZed][iPt][3];
	    return (pc3dphi-mean)/sigma;   //return the linear piece-wise curve fit
	}
	else if(iPt == 15)
	{
	    double mean  = pc3_point_lines[iCent][iDet][iZed][iPt][0]*pt + pc3_point_lines[iCent][iDet][iZed][iPt][1];
	    pt = 3.2;
	    iPt = 14; // use the mean out to the full pT range, only use the sigma out to 3.2 GeV/c.
	    double sigma = pc3_point_lines[iCent][iDet][iZed][iPt][2]*pt + pc3_point_lines[iCent][iDet][iZed][iPt][3];  // only use the sigma values out to 3.2
	    return (pc3dphi-mean)/sigma;
	}
	else return -9999;
    }
    else   return -9999;
}


/*
float Run12CuAu200GeVTOF_PC3_Matching::pc3sdphi_func_II(float charge, short dcarm, int iCent, int iZed, float pt, float pc3dphi)
{
    if(pc3dphi>-9999)
    {
	int iPt = -9999;
	
	     if(pt< 0.55          ) iPt= 0;    else if(pt>=0.55 && pt<0.7) iPt= 1;
	else if(pt>=0.7  && pt<0.9) iPt= 2;    else if(pt>=0.9  && pt<1.1) iPt= 3;
	else if(pt>=1.1  && pt<1.3) iPt= 4;    else if(pt>=1.3  && pt<1.5) iPt= 5;
	else if(pt>=1.5  && pt<1.7) iPt= 6;    else if(pt>=1.7  && pt<1.9) iPt= 7;
	else if(pt>=1.9  && pt<2.1) iPt= 8;    else if(pt>=2.1  && pt<2.3) iPt= 9;
	else if(pt>=2.3  && pt<2.5) iPt=10;    else if(pt>=2.5  && pt<2.8) iPt=11;
	else if(pt>=2.8  && pt<3.2) iPt=12;    else if(pt>=3.2  && pt<3.8) iPt=13;
	else if(pt> 3.8           ) iPt=14;
	
	
	int iDet = -9999;
	
	if     (dcarm==0 && charge>0)   {   iDet = 0;	}
	else if(dcarm==0 && charge<0)   {   iDet = 4;   }
	else if(dcarm==1 && charge>0)   {   iDet = 1;	}
	else if(dcarm==1 && charge<0)   {   iDet = 5;	}
	else return -9999;
	
	
	float mean  = pc3_point_lines_II[iCent][iDet][iZed][iPt][0]*pt + pc3_point_lines_II[iCent][iDet][iZed][iPt][1];
	float sigma = pc3_point_lines_II[iCent][iDet][iZed][iPt][2]*pt + pc3_point_lines_II[iCent][iDet][iZed][iPt][3];
	
	return (pc3dphi-mean)/sigma;   //return the linear piece-wise curve fit
    }
    else   return -9999;
}
*/


double Run12CuAu200GeVTOF_PC3_Matching::pc3sdz_func(float charge, short dcarm, int iCent, int iZed, float pt, float pc3dz)
{
    if(pc3dz>-9999)
    {
	double sigma_factor = 1.0000;  // this factor only changes things for the most peripheral bin (bin 8), which uses the next most peripheral bin's mean parameters (bin 7)
	
	if(iCent==7 && pt>3.2)
	{   pt = 3.20;
	}
	else if(iCent==8)
	{
	    iCent=7;
	    sigma_factor = 1.35;
	}
	
	int iPt = -9999;
	
	     if(pt>=0.20 && pt<0.35) iPt=0;
	else if(pt>=0.35 && pt<0.45) iPt=1;
	else if(pt>=0.45 && pt<0.55) iPt=2;
	else if(pt>=0.55 && pt<0.70) iPt=3;
	else if(pt>=0.70 && pt<0.90) iPt=4;
	else if(pt>=0.90 && pt<1.10) iPt=5;
	else if(pt>=1.10 && pt<1.30) iPt=6;
	else if(pt>=1.30 && pt<1.50) iPt=7;
	else if(pt>=1.50 && pt<1.70) iPt=8;
	else if(pt>=1.70 && pt<1.90) iPt=9;
	else if(pt>=1.90 && pt<2.10) iPt=10;
	else if(pt>=2.10 && pt<2.30) iPt=11;
	else if(pt>=2.30 && pt<2.50) iPt=12;
	else if(pt>=2.50 && pt<2.80) iPt=13;
	else if(pt>=2.80           ) iPt=14;
 	     if(pt>=3.20           ) pt = 3.2;
	
	int iDet = -9999;
	
	if     (dcarm==0 && charge>0)   {   iDet = 2;	}
	else if(dcarm==0 && charge<0)   {   iDet = 6;   }
	else if(dcarm==1 && charge>0)   {   iDet = 3;	}
	else if(dcarm==1 && charge<0)   {   iDet = 7;	}
	else return -9999;
	
	
	if(iPt<15 && iPt > -1)
	{
	    double mean  = pc3_point_lines[iCent][iDet][iZed][iPt][0]*pt + pc3_point_lines[iCent][iDet][iZed][iPt][1];
	    double sigma = pc3_point_lines[iCent][iDet][iZed][iPt][2]*pt + pc3_point_lines[iCent][iDet][iZed][iPt][3];
	    return (pc3dz-mean)/(sigma*sigma_factor);   // return the linear piece-wise curve fit
	}
	
	else return -9999;
    }
    else return -9999;
}

/*
float Run12CuAu200GeVTOF_PC3_Matching::pc3sdz_func_II(float charge, short dcarm, int iCent, int iZed, float pt, float pc3dz)
{
    int iPt = -9999;

         if(pt< 0.55          ) iPt= 0;    else if(pt>=0.55 && pt<0.7) iPt= 1;
    else if(pt>=0.7  && pt<0.9) iPt= 2;    else if(pt>=0.9  && pt<1.1) iPt= 3;
    else if(pt>=1.1  && pt<1.3) iPt= 4;    else if(pt>=1.3  && pt<1.5) iPt= 5;
    else if(pt>=1.5  && pt<1.7) iPt= 6;    else if(pt>=1.7  && pt<1.9) iPt= 7;
    else if(pt>=1.9  && pt<2.1) iPt= 8;    else if(pt>=2.1  && pt<2.3) iPt= 9;
    else if(pt>=2.3  && pt<2.5) iPt=10;    else if(pt>=2.5  && pt<2.8) iPt=11;
    else if(pt>=2.8  && pt<3.2) iPt=12;    else if(pt>=3.2  && pt<3.8) iPt=13;
    else if(pt> 3.8           ) iPt=14;


    int iDet = -9999;
	 
    if     (dcarm==0 && charge>0)   {   iDet = 2;	}
    else if(dcarm==0 && charge<0)   {   iDet = 6;       }
    else if(dcarm==1 && charge>0)   {   iDet = 3;	}
    else if(dcarm==1 && charge<0)   {   iDet = 7;	}
    else return -9999;



    if(pc3dz>-9999)
    {
	float mean  = pc3_point_lines_II[iCent][iDet][iZed][iPt][0]*pt + pc3_point_lines_II[iCent][iDet][iZed][iPt][1];
	float sigma = pc3_point_lines_II[iCent][iDet][iZed][iPt][2]*pt + pc3_point_lines_II[iCent][iDet][iZed][iPt][3];
	
	return (pc3dz-mean)/sigma;   //return the linear piece-wise curve fit
    }
    else return -9999;
}
*/




double Run12CuAu200GeVTOF_PC3_Matching::tofsdphi_func(float charge, short dcarm, int iCent, int iZed, float pt, float tofdphi)
{
    if(tofdphi>-9999)
    {

	int iPt = -9999;
	
	     if(pt>=0.20 && pt<0.35) iPt=0;
	else if(pt>=0.35 && pt<0.45) iPt=1;
	else if(pt>=0.45 && pt<0.55) iPt=2;
	else if(pt>=0.55 && pt<0.70) iPt=3;
	else if(pt>=0.70 && pt<0.90) iPt=4;
	else if(pt>=0.90 && pt<1.10) iPt=5;
	else if(pt>=1.10 && pt<1.30) iPt=6;
	else if(pt>=1.30 && pt<1.50) iPt=7;
	else if(pt>=1.50 && pt<1.70) iPt=8;
	else if(pt>=1.70 && pt<1.90) iPt=9;
	else if(pt>=1.90 && pt<2.10) iPt=10;
	else if(pt>=2.10 && pt<2.30) iPt=11;
	else if(pt>=2.30 && pt<2.50) iPt=12;
	else if(pt>=2.50 && pt<2.80) iPt=13;
	else if(pt>=2.80           ) iPt=14;
 	     if(pt>=3.20           ) pt = 3.2;
	

	int iDet = -9999;
	
	if     (dcarm==0 && charge>0)   {   iDet = 0;	}
	else if(dcarm==0 && charge<0)   {   iDet = 4;   }
	else if(dcarm==1 && charge>0)   {   iDet = 1;	}
	else if(dcarm==1 && charge<0)   {   iDet = 5;	}
	else return -9999;

	if(iPt<15 && iPt>-1)
	{
	    double mean  = tof_point_lines[iCent][iDet][iZed][iPt][0]*pt + tof_point_lines[iCent][iDet][iZed][iPt][1];
	    double sigma = tof_point_lines[iCent][iDet][iZed][iPt][2]*pt + tof_point_lines[iCent][iDet][iZed][iPt][3];
	    
	    return (tofdphi-mean)/sigma;   //return the linear piece-wise curve fit
	}
	else   return -9999;
    }
    else   return -9999;

}



/*
float Run12CuAu200GeVTOF_PC3_Matching::tofsdphi_func_II(float charge, short dcarm, int iCent, int iZed, float pt, float tofdphi)
{
    if(tofdphi>-9999)
    {
	int iPt = -9999;
	
	     if(pt< 0.55          ) iPt= 0;    else if(pt>=0.55 && pt<0.7) iPt= 1;
	else if(pt>=0.7  && pt<0.9) iPt= 2;    else if(pt>=0.9  && pt<1.1) iPt= 3;
	else if(pt>=1.1  && pt<1.3) iPt= 4;    else if(pt>=1.3  && pt<1.5) iPt= 5;
	else if(pt>=1.5  && pt<1.7) iPt= 6;    else if(pt>=1.7  && pt<1.9) iPt= 7;
	else if(pt>=1.9  && pt<2.1) iPt= 8;    else if(pt>=2.1  && pt<2.3) iPt= 9;
	else if(pt>=2.3  && pt<2.5) iPt=10;    else if(pt>=2.5  && pt<2.8) iPt=11;
	else if(pt>=2.8  && pt<3.2) iPt=12;    else if(pt>=3.2  && pt<3.8) iPt=13;
	else if(pt> 3.8           ) iPt=14;
	
	
	
	int iDet = -9999;
	
	if     (dcarm==0 && charge>0)   {   iDet = 0;	}
	else if(dcarm==0 && charge<0)   {   iDet = 4;   }
	else if(dcarm==1 && charge>0)   {   iDet = 1;	}
	else if(dcarm==1 && charge<0)   {   iDet = 5;	}
	else return -9999;
	
	

	float mean  = tof_point_lines_II[iCent][iDet][iZed][iPt][0]*pt + tof_point_lines_II[iCent][iDet][iZed][iPt][1];
	float sigma = tof_point_lines_II[iCent][iDet][iZed][iPt][2]*pt + tof_point_lines_II[iCent][iDet][iZed][iPt][3];
	
	return (tofdphi-mean)/sigma;   //return the linear piece-wise curve fit
    }
    else   return -9999;

}
*/




double Run12CuAu200GeVTOF_PC3_Matching::tofsdz_func(float charge, short dcarm, int iCent, int iZed, float pt, float tofdz)
{
    if(tofdz>-9999)
    {

	double sigma_factor = 1.0000;  // this factor only changes things for the most peripheral bin (bin 8), which uses the next most peripheral bin's parameters (bin 7)
	if(iCent == 7 && pt>2.5)
	{	pt = 2.50;
	}
	else if(iCent == 8)
	{
	    iCent=7;
	    sigma_factor = 1.35;
	    if(pt>2.50)   {   pt = 2.50;   }
	}
	
	int iPt = -9999;


	if(pt>=3.50)  pt = 3.5;

	     if(pt>=0.20 && pt<0.35) iPt=0;
	else if(pt>=0.35 && pt<0.45) iPt=1;
	else if(pt>=0.45 && pt<0.55) iPt=2;
	else if(pt>=0.55 && pt<0.70) iPt=3;
	else if(pt>=0.70 && pt<0.90) iPt=4;
	else if(pt>=0.90 && pt<1.10) iPt=5;
	else if(pt>=1.10 && pt<1.30) iPt=6;
	else if(pt>=1.30 && pt<1.50) iPt=7;
	else if(pt>=1.50 && pt<1.70) iPt=8;
	else if(pt>=1.70 && pt<1.90) iPt=9;
	else if(pt>=1.90 && pt<2.10) iPt=10;
	else if(pt>=2.10 && pt<2.30) iPt=11;
	else if(pt>=2.30 && pt<2.50) iPt=12;
	else if(pt>=2.50 && pt<2.80) iPt=13;
	else if(pt>=2.80 && pt<3.20) iPt=14;
	else if(pt>=3.20 && pt<3.80) iPt=15;


	int iDet = -9999;
	
	if     (dcarm==0 && charge>0)   {   iDet = 2;	}
	else if(dcarm==0 && charge<0)   {   iDet = 6;   }
	else if(dcarm==1 && charge>0)   {   iDet = 3;	}
	else if(dcarm==1 && charge<0)   {   iDet = 7;	}
	else return -9999;


	if(iPt<16 && iPt>-1)
	{
	    double mean  = tof_point_lines[iCent][iDet][iZed][iPt][0]*pt + tof_point_lines[iCent][iDet][iZed][iPt][1];
	    double sigma = tof_point_lines[iCent][iDet][iZed][iPt][2]*pt + tof_point_lines[iCent][iDet][iZed][iPt][3];
	    
	    return (tofdz-mean)/(sigma*sigma_factor);   // return the linear piece-wise curve fit
	}

	else return -9999;
    }
    else return -9999;

}


/*
float Run12CuAu200GeVTOF_PC3_Matching::tofsdz_func_II(float charge, short dcarm, int iCent, int iZed, float pt, float tofdz)
{
    if(tofdz>-9999)
    {
	int iPt = -9999;
	
	     if(pt< 0.55          ) iPt= 0;    else if(pt>=0.55 && pt<0.7) iPt= 1;
	else if(pt>=0.7  && pt<0.9) iPt= 2;    else if(pt>=0.9  && pt<1.1) iPt= 3;
	else if(pt>=1.1  && pt<1.3) iPt= 4;    else if(pt>=1.3  && pt<1.5) iPt= 5;
	else if(pt>=1.5  && pt<1.7) iPt= 6;    else if(pt>=1.7  && pt<1.9) iPt= 7;
	else if(pt>=1.9  && pt<2.1) iPt= 8;    else if(pt>=2.1  && pt<2.3) iPt= 9;
	else if(pt>=2.3  && pt<2.5) iPt=10;    else if(pt>=2.5  && pt<2.8) iPt=11;
	else if(pt>=2.8  && pt<3.2) iPt=12;    else if(pt>=3.2  && pt<3.8) iPt=13;
	else if(pt> 3.8           ) iPt=14;
	
	
	int iDet = -9999;
	
	if     (dcarm==0 && charge>0)   {   iDet = 2;	}
	else if(dcarm==0 && charge<0)   {   iDet = 6;   }
	else if(dcarm==1 && charge>0)   {   iDet = 3;	}
	else if(dcarm==1 && charge<0)   {   iDet = 7;	}
	else return -9999;
	
	
	float mean  = tof_point_lines_II[iCent][iDet][iZed][iPt][0]*pt + tof_point_lines_II[iCent][iDet][iZed][iPt][1];
	float sigma = tof_point_lines_II[iCent][iDet][iZed][iPt][2]*pt + tof_point_lines_II[iCent][iDet][iZed][iPt][3];
	
	return (tofdz-mean)/sigma;   //return the linear piece-wise curve fit
    }
    else return -9999;

}
*/







void Run12CuAu200GeVTOF_PC3_Matching::init_pc3_fit_pars(int run_num)
{
//  RunToTime      *rt          = RunToTime::instance();
//  PHTimeStamp    *BorTime     = rt->getBeginTime(run_num);
//  PHTimeStamp    *EorTime     = rt->getEndTime(run_num);
    
    PdbBankManager *BankManager = PdbBankManager::instance();
    PHTimeStamp now;
    PdbBankID BankID(0);
    PdbApplication* application = BankManager->getApplication();


    if (!application->startRead())
    {
        cout << PHWHERE << ": Aborting ... Database not readable" << endl;
        application->abort();
	cout << PHWHERE << "Failed to get PC3 matching parameters from DB" << endl;
	return;
    }
    application->startRead();



    PdbCalBank* bank = BankManager->fetchBank("PdbFloatVectorBank",BankID,"pc3_match", run_num);
    std::vector<float> sector_escale;
    
    if(bank)
    {
	PdbFloatVector *fvec = (PdbFloatVector *) &(bank->getEntry(0));
	sector_escale = fvec->getVector();

	int read_index = 0;
	for(int iCent=0; iCent<pc3_cent_bins; iCent++)
	{
	    for(int iDet=0; iDet<pc3_num_det; iDet++)
	    {
		for(int iZed=0; iZed<pc3_zed_bins; iZed++)
		{
		    for(int iPt=0; iPt<pc3_pt_bins-1; iPt++)
		    {
			pc3_point_lines[iCent][iDet][iZed][iPt][0] = sector_escale[read_index];   read_index++;
			pc3_point_lines[iCent][iDet][iZed][iPt][1] = sector_escale[read_index];   read_index++;
			pc3_point_lines[iCent][iDet][iZed][iPt][2] = sector_escale[read_index];   read_index++;
			pc3_point_lines[iCent][iDet][iZed][iPt][3] = sector_escale[read_index];   read_index++;
		    }
		}
	    }
	}

	delete bank;
    }
}


void Run12CuAu200GeVTOF_PC3_Matching::init_tofe_fit_pars(int run_num)
{
//  RunToTime      *rt          = RunToTime::instance();
//  PHTimeStamp    *BorTime     = rt->getBeginTime(run_num);
//  PHTimeStamp    *EorTime     = rt->getEndTime(run_num);
    
    PdbBankManager *BankManager = PdbBankManager::instance();
    PHTimeStamp now;
    PdbBankID BankID(0);
    PdbApplication* application = BankManager->getApplication();


    if (!application->startRead())
    {
        cout << PHWHERE << ": Aborting ... Database not readable" << endl;
        application->abort();
	cout << PHWHERE << "Failed to get TOFE matching parameters from DB" << endl;
	return;
    }
    application->startRead();



    PdbCalBank* bank = BankManager->fetchBank("PdbFloatVectorBank",BankID,"tofe_match", run_num);
    std::vector<float> sector_escale;
    
    if(bank)
    {
	PdbFloatVector *fvec = (PdbFloatVector *) &(bank->getEntry(0));
	sector_escale = fvec->getVector();

	int read_index = 0;



	for(int iOffset=0; iOffset<4*960; iOffset++)
	{
	    mean_tofe_offset[iOffset] = sector_escale[read_index];   read_index++;
	}


	for(int iCent=0; iCent<tof_cent_bins; iCent++)
	{
	    for(int iDet=0; iDet<tof_num_det; iDet++)
	    {
		if(iDet == 0  ||  iDet == 2  ||  iDet == 4  ||  iDet == 6)
		{
		    for(int iZed=0; iZed<tof_zed_bins; iZed++)
		    {
			for(int iPt=0; iPt<tof_pt_bins-1; iPt++)
			{
			    tof_point_lines[iCent][iDet][iZed][iPt][0] = sector_escale[read_index];   read_index++;
			    tof_point_lines[iCent][iDet][iZed][iPt][1] = sector_escale[read_index];   read_index++;
			    tof_point_lines[iCent][iDet][iZed][iPt][2] = sector_escale[read_index];   read_index++;
			    tof_point_lines[iCent][iDet][iZed][iPt][3] = sector_escale[read_index];   read_index++;
			}
		    }
		}
	    }
	}

	delete bank;
    }

}


void Run12CuAu200GeVTOF_PC3_Matching::init_tofw_fit_pars(int run_num)
{

//  RunToTime      *rt          = RunToTime::instance();
//  PHTimeStamp    *BorTime     = rt->getBeginTime(run_num);
//  PHTimeStamp    *EorTime     = rt->getEndTime(run_num);
    
    PdbBankManager *BankManager = PdbBankManager::instance();
    PHTimeStamp now;
    PdbBankID BankID(0);
    PdbApplication* application = BankManager->getApplication();


    if (!application->startRead())
    {
        cout << PHWHERE << ": Aborting ... Database not readable" << endl;
        application->abort();
	cout << PHWHERE << "Failed to get TOFW matching parameters from DB" << endl;
	return;
    }
    application->startRead();



    PdbCalBank* bank = BankManager->fetchBank("PdbFloatVectorBank",BankID,"tofw_match", run_num);
    std::vector<float> sector_escale;
    
    if(bank)
    {
	PdbFloatVector *fvec = (PdbFloatVector *) &(bank->getEntry(0));
	sector_escale = fvec->getVector();

	int read_index = 0;


	for(int iOffset=0; iOffset<4*512; iOffset++)
	{
	    mean_tofw_offset[iOffset] = sector_escale[read_index];   read_index++;
	}


	for(int iCent=0; iCent<tof_cent_bins; iCent++)
	{
	    for(int iDet=0; iDet<tof_num_det; iDet++)
	    {
		if(iDet == 1  ||  iDet == 3  ||  iDet == 5  ||  iDet == 7)
		{
		    for(int iZed=0; iZed<tof_zed_bins; iZed++)
		    {
			for(int iPt=0; iPt<tof_pt_bins-1; iPt++)
			{
			    tof_point_lines[iCent][iDet][iZed][iPt][0] = sector_escale[read_index];   read_index++;
			    tof_point_lines[iCent][iDet][iZed][iPt][1] = sector_escale[read_index];   read_index++;
			    tof_point_lines[iCent][iDet][iZed][iPt][2] = sector_escale[read_index];   read_index++;
			    tof_point_lines[iCent][iDet][iZed][iPt][3] = sector_escale[read_index];   read_index++;
			}
		    }
		}
	    }
	}

	delete bank;
    }
}
