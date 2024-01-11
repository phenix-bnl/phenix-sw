// Author:	Jeong Hwan Park
// Revision:	Sangsu Ryu	Mar/18/2001
//			Adds short in "maxadc_ver   =(short)full_scale"
//			Makes operator<< return os
//		Sangsu Ryu	Jun/12/2001
//			Adds strip_sigma_cut, pad_sigma_cut, clump_cut	
//

#include "MvdParameter.hh"
#include "gsl/gsl_math.h"
#include <iostream>

using namespace std;

void 
MvdParameter :: Init() 
{
	nzdim=3072;
        nzbig=3200;
	nchdim=12;
	nstrip=256;
	nphuse=6;
        nphdim=6;
        nshell=2;
        nsegment=4;
	nhole=8;
	
	nend=2;
        nvpad_segs  =12;
	nvpad_segs2 =12;
	nvpad_phi   =nvpad_segs*nvpad_segs2;
	nvpad_rbins =21 ;
	nvpad_rbins1=22;
	vpad_delphi = 2.0 * M_PI / nvpad_segs;
	vpad_phi1   =vpad_delphi/2.;
	vpad_delphi2=0.041888;

	mvd_type=0;
	barrel_type=0;
	endcap_type=1;
	
	packet_id=2001;

	barrel_gain=3.733333;
	endcap_gain=3.733333;
	
        iphlim=2;
        p_iexpnd=1; 
        z_iexpnd=3;

	choice=1;
        ilayer=0;
        nhitmin=-1;   // changed to -1 for run 4 -- this is a threshold on
	              // hits in the barrel, but there are none (because there
	              // is no barrel) in run-4.
	occ_cut=0.450000;

        avgmip = 31.875000;
        nbunch = 32; 
        tooclose =  5.000000; /* cm */
	
	zoff         =31.800000;
        xnum_mip     =25000.000000;
        smax_mip     =8.000000;
        skev_mip     =119.00000;
        smax_kev=smax_mip*skev_mip;
        skev_noise   =0.1*skev_mip;
	thresh_kev   =25.0000;
        full_scale   =1023.000000;
        ver_thrsh_mip=0.250000;
        ver_thrsh    =ver_thrsh_mip*skev_mip;
        maxadc_ver   =(short)full_scale;
        factor_noise =5.000000;
	cross_talk=0;

        software_pversion=1.01;
        software_zversion= 2.01;
        soft_id_whole=1.01;
        soft_id_row=2.01;
        maxiter=2;
        soft_id_avgdedx=3.0;
        soft_id_deconv=4.0;
        eta_min_hist=-7;
        eta_max_hist=7;
        n_eta_bins_in_hist=56;

   strip_sigma_cut=3.0;
   pad_sigma_cut=4.0;
   clump_cut=0.3;

}

ostream& 
operator<<(ostream& os, const MvdParameter& mvdparameter) 
{

   os<<endl<<"MvdParameter";    
   os<<endl<<"nzdim="<<mvdparameter.nzdim;
   os<<endl<<"nzbig="<<mvdparameter.nzbig;
   os<<endl<<"nchdim="<<mvdparameter.nchdim;
   os<<endl<<"nstrip="<<mvdparameter.nstrip;
   os<<endl<<"nphuse="<<mvdparameter.nphuse;
   os<<endl<<"nphdim="<<mvdparameter.nphdim;
   os<<endl<<"nshell="<<mvdparameter.nshell;
   os<<endl<<"nsegment="<<mvdparameter.nsegment;
   os<<endl<<"nhole="<<mvdparameter.nhole;

   os<<endl<<"nend="<<mvdparameter.nend;
   os<<endl<<"nvpad_segs="<<mvdparameter.nvpad_segs;
   os<<endl<<"nvpad_segs2="<<mvdparameter.nvpad_segs2;
   os<<endl<<"nvpad_phi="<<mvdparameter.nvpad_phi;
   os<<endl<<"nvpad_rbins="<<mvdparameter.nvpad_rbins;
   os<<endl<<"nvpad_rbins1="<<mvdparameter.nvpad_rbins1;
   os<<endl<<"vpad_delphi="<<mvdparameter.vpad_delphi;
   os<<endl<<"vpad_phi1="<<mvdparameter.vpad_phi1;
   os<<endl<<"vpad_delphi2="<<mvdparameter.vpad_delphi2;

   os<<endl<<"mvd_type="<<mvdparameter.mvd_type;
   os<<endl<<"barrel_type="<<mvdparameter.barrel_type;
   os<<endl<<"endcap_type="<<mvdparameter.endcap_type;

   os<<endl<<"packet_id="<<mvdparameter.packet_id;

   os<<endl<<"barrel_gain="<<mvdparameter.barrel_gain;
   os<<endl<<"endcap_gain="<<mvdparameter.endcap_gain;

   os<<endl<<"iphlim="<<mvdparameter.iphlim;
   os<<endl<<"p_iexpnd="<<mvdparameter.p_iexpnd;
   os<<endl<<"z_iexpnd="<<mvdparameter.z_iexpnd;

   os<<endl<<"choice="<<mvdparameter.choice;
   os<<endl<<"ilayer="<<mvdparameter.ilayer;
   os<<endl<<"nhitmin="<<mvdparameter.nhitmin;
   os<<endl<<"occ_cut="<<mvdparameter.occ_cut;

   os<<endl<<"avgmip="<<mvdparameter.avgmip;
   os<<endl<<"nbunch="<<mvdparameter.nbunch;
   os<<endl<<"tooclose="<<mvdparameter.tooclose;

   os<<endl<<"zoff="<<mvdparameter.zoff;
   os<<endl<<"xnum_mip="<<mvdparameter.xnum_mip;
   os<<endl<<"smax_mip="<<mvdparameter.smax_mip;
   os<<endl<<"skev_mip="<<mvdparameter.skev_mip;
   os<<endl<<"smax_kev="<<mvdparameter.smax_kev;
   os<<endl<<"skev_noise="<<mvdparameter.skev_noise;
   os<<endl<<"thresh_kev="<<mvdparameter.thresh_kev;
   os<<endl<<"full_scale="<<mvdparameter.full_scale;
   os<<endl<<"ver_thrsh_mip="<<mvdparameter.ver_thrsh_mip;
   os<<endl<<"ver_thrsh="<<mvdparameter.ver_thrsh;
   os<<endl<<"maxadc_ver="<<mvdparameter.maxadc_ver;
   os<<endl<<"factor_noise="<<mvdparameter.factor_noise;
   os<<endl<<"cross_talk="<<mvdparameter.cross_talk;

   os<<endl<<"software_pversion="<<mvdparameter.software_pversion;
   os<<endl<<"software_zversion="<<mvdparameter.software_zversion;
   os<<endl<<"soft_id_whole="<<mvdparameter.soft_id_whole;
   os<<endl<<"soft_id_row="<<mvdparameter.soft_id_row;
   os<<endl<<"maxiter="<<mvdparameter.maxiter;

   os<<endl<<"soft_id_avgdedx="<<mvdparameter.soft_id_avgdedx;
   os<<endl<<"soft_id_deconv="<<mvdparameter.soft_id_deconv;
   os<<endl<<"eta_min_hist="<<mvdparameter.eta_min_hist;
   os<<endl<<"eta_max_hist="<<mvdparameter.eta_max_hist;
   os<<endl<<"n_eta_bins_in_hist="<<mvdparameter.n_eta_bins_in_hist;
   return os;
}


