// $Id: MuonAlign.cxx,v 1.12 2011/07/14 22:27:09 pinkenbu Exp $

/*!
  \file    MuonAlign.cxx
  \ingroup supermodules 
  \brief   muon alignment module [old]
  \author  S. Kelly
  \version $Revision: 1.12 $
  \date    $Date: 2011/07/14 22:27:09 $
*/

#include <PHTFileServer.h>
#include <TNtuple.h>

// MUIOO includes
//
#include<TMuiRoadMapO.h>
#include<TMui1DRoadMapO.h>
#include<TMuiClusterMapO.h>
#include<TMuiPseudoBLTMapO.h>

// MUTOO includes
//
#include<TMutTrackUtil.h>
#include<TMutTrkMap.h>
#include<TMutVtxMap.h>
#include<TMutCoordMap.h>

// MUIOO includes
//
#include<TMuiRoadMapO.h>
#include<MutGeom.h>
#include<MuiGeomClasses.hh>

#include "MuonAlign.h"

#include<MutGeom.h>
#include<PhMutooDisplay.h>
#include<PHTimer.h>
#include<bitset>
#include<sstream>

// Database
//
#include<TMutDatabaseInit.h>

//___________________________________________________________________
MuonAlign::MuonAlign( const char* name, const char* file ) : 
	SubsysReco( name ),
	_filename( file ? file:"muon_align_ntuples.root" ),
  _timer( PHTimeServer::get()->insert_new( name ) )
{}

//___________________________________________________________________
int MuonAlign::InitRun(PHCompositeNode *topNode)
{
  PHTFileServer::get().open( _filename,"RECREATE");

/*
	_align_vars1 = new TNtuple("align_vars1","align_vars1","arm:sta:oct:hoct:gap:cath:"
		  "q_peak:q_tot:cos_ac:cos_wz:w_trk:w_meas:r_trk:"
		  "chi_w:chi_r:n_coord:n_gap:w_fit_err:clus_width:"
		  "theta:dxdz:dydz:x:y:cos_th_r:z"); 
*/
  _align_vars2 = new TNtuple("align_vars2","align_vars2","x1:y1:z1:x2:y2:z2:x3:y3:z3:"  //9
		  "theta1:theta2:theta3:phi1:phi2:phi3:" //9+6=15
		  "r1:r2:r3:rPhi1:rPhi2:rPhi3:" //15+6=21
		  "x1Proj:y1Proj:r1Proj:phi1Proj:" //21+4=25
		  "x2Proj:y2Proj:r2Proj:phi2Proj:" //25+4=29
		  "x3Proj:y3Proj:r3Proj:phi3Proj:" //29+4=33
		  "x1Proj1:x1Proj2:x1Proj3:y1Proj1:y1Proj2:y1Proj3:r1Proj1:r1Proj2:r1Proj3:" //33+9=42
		  "x2Proj1:x2Proj2:x2Proj3:y2Proj1:y2Proj2:y2Proj3:r2Proj1:r2Proj2:r2Proj3:" //42+9=51
		  "x3Proj1:x3Proj2:x3Proj3:y3Proj1:y3Proj2:y3Proj3:r3Proj1:r3Proj2:r3Proj3:" //51+9=60
		  "phi2Proj1:phi2Proj2:phi2Proj3:" //60+3=63
		  "x1Gap1:x1Gap2:x1Gap3:x2Gap1:x2Gap2:x2Gap3:x3Gap1:x3Gap2:x3Gap3:" //63+9=72
		  "y1Gap1:y1Gap2:y1Gap3:y2Gap1:y2Gap2:y2Gap3:y3Gap1:y3Gap2:y3Gap3:" //72+9=81
		  "z1Gap1:z1Gap2:z1Gap3:z2Gap1:z2Gap2:z2Gap3:z3Gap1:z3Gap2:z3Gap3:" //81+9=90
		  "r1Gap1:r1Gap2:r1Gap3:r2Gap1:r2Gap2:r2Gap3:r3Gap1:r3Gap2:r3Gap3:" //90+9=99
		  "phi2Gap1:phi2Gap2:phi2Gap3:" //99+3=102
		  "slope1:slope2:slope3:road_depth:" //102+4=106
		  "oct1:oct2:oct3:" //106+3=109
		  "half1:half2:half3:trk_arm");//109+4=113

  _align_vars2->SetAutoSave(10000);
  return 0;
}

//___________________________________________________________________
int MuonAlign::process_event(PHCompositeNode *top_node)
{
  _timer.get()->restart();  

  write_align_ntuple(top_node);

  static ULong_t auto_save=0;
  if(auto_save++%10 == 0) {
     _align_vars2->AutoSave();
     PHTFileServer::get().flush( _filename );
  }


  _timer.get()->stop();

  return 1;
}

//___________________________________________________________________
int MuonAlign::End(PHCompositeNode* top_node) 
{
//   _timer.get()->print_stat();
  PHTFileServer::get().write( _filename );
  return 0;
}

//___________________________________________________________________
void MuonAlign::write_align_ntuple(PHCompositeNode* top_node)
{

    std::cout << "started write_align_ntuple" <<std::endl;

    static int ievent=0;

    ievent++;

    TMutTrkMap*   trk_map  = TMutNode<TMutTrkMap>::find_node(top_node,"TMutTrkMap");
    TMutTrkMap::const_iterator trk_iter = trk_map->range();

    float ntracks=0;

    while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next()){

	 ntracks++;   
	 std::cout << "ntracks: "<<ntracks<<" trk_ptr: "<<trk_ptr<< std::endl;

	 TMutStubMap::const_key_iterator stub_iter = trk_ptr->get()->get_associated<TMutStub>();

	 float road_depth = 0;
	 float trk_arm = trk_ptr->get()->get_arm();
	 TMuiRoadMapO::const_key_iterator road_iter = trk_ptr->get()->get_associated<TMuiRoadO>();
	 if(!road_iter.at_end()) road_depth = road_iter->get()->get_depth();



    //setting align variables
    //
    float x1 = -1000, y1 = -1000, z1 = -1000;
    float x2 = -1000, y2 = -1000, z2 = -1000;
    float x3 = -1000, y3 = -1000, z3 = -1000;
    float dx1 = -1000, dy1 = -1000, dx2 = -1000, dy2 = -1000, dx3 = -1000, dy3 = -1000;
    float slope1 = -1000, slope2 = -1000, slope3 = -1000;
    float r1 = -1000, r2 = -1000, r3 = -1000;
    float phi1 = -1000, phi2 = -1000, phi3 = -1000;
    float oct1 = -1000, oct2 = -1000, oct3 = -1000;
    float half1 = -1000, half2 = -1000, half3 = -1000;
    float theta1 = -1000, theta2 = -1000, theta3 = -1000;
    float rPhi1 = -1000, rPhi2 = -1000, rPhi3 = -1000;
    float x1Gap1 = -1000, y1Gap1 = -1000, z1Gap1 = -1000;
    float x2Gap1 = -1000, y2Gap1 = -1000, z2Gap1 = -1000;
    float x3Gap1 = -1000, y3Gap1 = -1000, z3Gap1 = -1000;
    float x1Gap2 = -1000, y1Gap2 = -1000, z1Gap2 = -1000;
    float x2Gap2 = -1000, y2Gap2 = -1000, z2Gap2 = -1000;
    float x3Gap2 = -1000, y3Gap2 = -1000, z3Gap2 = -1000;
    float phi2Gap1 = -1000, phi2Gap2 = -1000, phi2Gap3 = -1000;
    float r1Gap1 = -1000, r1Gap2 = -1000, r1Gap3 = -1000;
    float r2Gap1 = -1000, r2Gap2 = -1000, r2Gap3 = -1000;
    float r3Gap1 = -1000, r3Gap2 = -1000, r3Gap3 = -1000;
    float x1Gap3 = -1000, y1Gap3 = -1000, z1Gap3 = -1000;
    float x2Gap3 = -1000, y2Gap3 = -1000, z2Gap3 = -1000;
    float x3Gap3 = -1000, y3Gap3 = -1000, z3Gap3 = -1000;



    float stub=0;

    while(TMutStubMap::const_pointer stub_ptr = stub_iter.next()){

		    // Loop over TMutStub residuals and dump data into ntuple
		    //

		   
		    stub++;

		    TMutCoordMap::const_key_iterator coord_iter = stub_ptr->get()->get_associated<TMutCoord>();
		    TMutGapCoordMap::const_key_iterator gap_iter =stub_ptr->get()->get_associated<TMutGapCoord>();


		    // go through the gap coordinates associated with stubs
		    //

		    while(TMutGapCoordMap::const_pointer gap_ptr = gap_iter.next()){

			    if(gap_ptr->get()->get_station() == MUTOO::Station1){
				    if(gap_ptr->get()->get_gap() == MUTOO::Gap1){

					    x1Gap1 = gap_ptr->get()->get_coord().getX();
					    y1Gap1 = gap_ptr->get()->get_coord().getY();
					    z1Gap1 = gap_ptr->get()->get_coord().getZ();

					    r1Gap1 = sqrt(x1Gap1*x1Gap1+y1Gap1*y1Gap1);

				    }
				    if(gap_ptr->get()->get_gap() == MUTOO::Gap2){

					    x1Gap2 = gap_ptr->get()->get_coord().getX();
					    y1Gap2 = gap_ptr->get()->get_coord().getY();
					    z1Gap2 = gap_ptr->get()->get_coord().getZ();

					    r1Gap2 = sqrt(x1Gap2*x1Gap2+y1Gap2*y1Gap2);
				    }
				    if(gap_ptr->get()->get_gap() == MUTOO::Gap3){
					    x1Gap3 = gap_ptr->get()->get_coord().getX();
					    y1Gap3 = gap_ptr->get()->get_coord().getY();
					    z1Gap3 = gap_ptr->get()->get_coord().getZ();

					    r1Gap3 = sqrt(x1Gap3*x1Gap3+y1Gap3*y1Gap3);
				    }
			    }

			    if(gap_ptr->get()->get_station() == MUTOO::Station2){

				    if(gap_ptr->get()->get_gap() == MUTOO::Gap1){

					    x2Gap1 = gap_ptr->get()->get_coord().getX();
					    y2Gap1 = gap_ptr->get()->get_coord().getY();
					    z2Gap1 = gap_ptr->get()->get_coord().getZ();

					    if(fabs(x2Gap1)>0.001){
						    phi2Gap1=atan(y2Gap1/x2Gap1);
					    }
					    else{
						    phi2Gap1=-999;
					    }
					    r2Gap1=sqrt(x2Gap1*x2Gap1+y2Gap1*y2Gap1);

				    }
				    if(gap_ptr->get()->get_gap() == MUTOO::Gap2){

					    x2Gap2 = gap_ptr->get()->get_coord().getX();
					    y2Gap2 = gap_ptr->get()->get_coord().getY();
					    z2Gap2 = gap_ptr->get()->get_coord().getZ();

					    if(fabs(x2Gap2)>0.001){
						    phi2Gap2=atan(y2Gap2/x2Gap2);
					    }
					    else{
						    phi2Gap2=-999;
					    }

					    r2Gap2=sqrt(x2Gap2*x2Gap2+y2Gap2*y2Gap2);
				    }
				    if(gap_ptr->get()->get_gap() == MUTOO::Gap3){

					    x2Gap3 = gap_ptr->get()->get_coord().getX();
					    y2Gap3 = gap_ptr->get()->get_coord().getY();
					    z2Gap3 = gap_ptr->get()->get_coord().getZ();

					    if(fabs(x2Gap3)>0.001){
						    phi2Gap3=atan(y2Gap3/x2Gap3);
					    }
					    else{
						    phi2Gap3=-999;
					    }

					    r2Gap3=sqrt(x2Gap3*x2Gap3+y2Gap3*y2Gap3);
				    }
			    }
			    if(gap_ptr->get()->get_station() == MUTOO::Station3){
				    if(gap_ptr->get()->get_gap() == MUTOO::Gap1){

					    x3Gap1 = gap_ptr->get()->get_coord().getX();
					    y3Gap1 = gap_ptr->get()->get_coord().getY();
					    z3Gap1 = gap_ptr->get()->get_coord().getZ();

					    r3Gap1 = sqrt(x3Gap1*x3Gap1+y3Gap1*y3Gap1);
				    }
				    if(gap_ptr->get()->get_gap() == MUTOO::Gap2){

					    x3Gap2 = gap_ptr->get()->get_coord().getX();
					    y3Gap2 = gap_ptr->get()->get_coord().getY();
					    z3Gap2 = gap_ptr->get()->get_coord().getZ();

					    r3Gap2 = sqrt(x3Gap2*x3Gap2+y3Gap2*y3Gap2);
				    }
				    if(gap_ptr->get()->get_gap() == MUTOO::Gap3){

					    x3Gap3 = gap_ptr->get()->get_coord().getX();
					    y3Gap3 = gap_ptr->get()->get_coord().getY();
					    z3Gap3 = gap_ptr->get()->get_coord().getZ();

					    r3Gap3=sqrt(x3Gap3*x3Gap3+y3Gap3*y3Gap3);
				    }
			    }
		    }//end of gap coord loop

		    float stub_stat = stub_ptr->get()->get_station();

		    if(stub_stat==0){

			    x1 = stub_ptr->get()->get_fit_par()->get_x();
			    y1 = stub_ptr->get()->get_fit_par()->get_y();
			    z1 = stub_ptr->get()->get_fit_par()->get_z();

			    dx1 = stub_ptr->get()->get_fit_par()->get_dxdz();
			    dy1 = stub_ptr->get()->get_fit_par()->get_dydz();
			    slope1 = sqrt(dx1*dx1 + dy1*dy1);

			    oct1 = stub_ptr->get()->get_octant();
			    half1 = stub_ptr->get()->get_half_octant();

			    r1=sqrt(x1*x1+y1*y1);
			    theta1=atan(r1/z1);

			    if(fabs(x1)>0.001){
				    phi1=atan(y1/x1);
			    }
			    else{
				    phi1=-999;
			    }
		    }

		    if(stub_stat==1){

			    x2 = stub_ptr->get()->get_fit_par()->get_x();
			    y2 = stub_ptr->get()->get_fit_par()->get_y();
			    z2 = stub_ptr->get()->get_fit_par()->get_z();

			    dx2 = stub_ptr->get()->get_fit_par()->get_dxdz();
			    dy2 = stub_ptr->get()->get_fit_par()->get_dydz();
			    slope2 = sqrt(dx2*dx2 + dy2*dy2);

			    oct2 = stub_ptr->get()->get_octant();
			    half2 = stub_ptr->get()->get_half_octant();

			    r2=sqrt(x2*x2+y2*y2);
			    theta2=atan(r2/z2);
			     
			    if(fabs(x2)>0.001){
				    phi2=atan(y2/x2);
			    }
			    else{
				    phi2=-999;
			    }
		    }

		    if(stub_stat==2){

			    x3 = stub_ptr->get()->get_fit_par()->get_x();
			    y3 = stub_ptr->get()->get_fit_par()->get_y();
			    z3 = stub_ptr->get()->get_fit_par()->get_z();

			    dx3 = stub_ptr->get()->get_fit_par()->get_dxdz();
			    dy3 = stub_ptr->get()->get_fit_par()->get_dydz();
			    slope3 = sqrt(dx3*dx3 + dy3*dy3);

			    oct3 = stub_ptr->get()->get_octant();
			    half3 = stub_ptr->get()->get_half_octant();

			    r3=sqrt(x3*x3+y3*y3);
			    theta3=atan(r3/z3);

			    if(fabs(x3)>0.001){
				    phi3=atan(y3/x3);
			    }
			    else{
				    phi3=-999;
			    }
		    }

		    rPhi1=r1*phi1;
		    rPhi2=r2*phi2;
		    rPhi3=r3*phi3;


/*
                    TMutStub::const_residual_iterator res_iter = residuals->begin();

                    for(;res_iter!=residuals->end();++res_iter){
			    
                            nt_vars[0] = res_iter->get_arm();
        		    nt_vars[1] = res_iter->get_station();
        		    nt_vars[2] = res_iter->get_octant();
        		    nt_vars[3] = res_iter->get_half_octant();
        		    nt_vars[4] = res_iter->get_gap();
        		    nt_vars[5] = res_iter->get_cathode();
        		    nt_vars[6] = res_iter->get_q_peak();
        		    nt_vars[7] = res_iter->get_q_tot();
        		    nt_vars[8] = res_iter->get_cos_theta_ac();
        		    nt_vars[9] = res_iter->get_cos_theta_wz();
        		    nt_vars[10] = res_iter->get_w_trk();
        		    nt_vars[11] = res_iter->get_w_meas();
          		    nt_vars[12] = res_iter->get_r_trk();
        		    nt_vars[13] = stub_ptr->get()->get_w_chi_square();
        		    nt_vars[14] = stub_ptr->get()->get_r_chi_square();
        		    nt_vars[15] = stub_ptr->get()->get_n_w_residual();
        		    nt_vars[16] = stub_ptr->get()->get_n_r_residual();
        		    nt_vars[17] = res_iter->get_w_fit_error();
        		    nt_vars[18] = res_iter->get_clus_width();
         		    double tan_theta = std::sqrt(MUTOO::SQUARE(res_iter->get_fit_par()->get_dydz()) + MUTOO::SQUARE(res_iter->get_fit_par()->get_dxdz()));
        		    nt_vars[19] = std::atan(tan_theta);
        		    nt_vars[20] = res_iter->get_fit_par()->get_dxdz();
        		    nt_vars[21] = res_iter->get_fit_par()->get_dydz();
        		    nt_vars[22] = res_iter->get_fit_par()->get_x();
        		    nt_vars[23] = res_iter->get_fit_par()->get_y();
        		    nt_vars[24] = res_iter->get_cos_theta_r();
        		    nt_vars[25] = res_iter->get_fit_par()->get_z();
        		    _align_vars1->Fill(nt_vars);
		    }
*/


    }//end of the stub loop

    //projections using stubs inform. of each track

    if (x1> -1000 && x2> -1000 && x3 > -1000){
	    //if (slope1 < 3 && slope2 < 3 && slope3 < 3){
		    

		    float x1Proj=x2+(x3-x2)/(z3-z2)*(z1-z2);
		    float y1Proj=y2+(y3-y2)/(z3-z2)*(z1-z2);
		    float r1Proj=sqrt(x1Proj*x1Proj+y1Proj*y1Proj);
		    float phi1Proj=-999;
		    if(fabs(x1Proj)>0.0001){
			    phi1Proj=atan(y1Proj/x1Proj);
		    }

		    float x2Proj=x1+(x3-x1)/(z3-z1)*(z2-z1);
		    float y2Proj=y1+(y3-y1)/(z3-z1)*(z2-z1);
		    float r2Proj=sqrt(x2Proj*x2Proj+y2Proj*y2Proj);
		    float phi2Proj = -999;
		    if(fabs(x2Proj)>0.0001){
			    phi2Proj = atan(y2Proj/x2Proj);
		    }

		    float x3Proj=x1+(x2-x1)/(z2-z1)*(z3-z1);
		    float y3Proj=y1+(y2-y1)/(z2-z1)*(z3-z1);
		    float r3Proj=sqrt(x3Proj*x3Proj+y3Proj*y3Proj);
		    float phi3Proj=-999;
		    if(fabs(x3Proj)>0.0001){
			    phi3Proj = atan(y3Proj/x3Proj);
		    }


                    float x1Proj1=x2+(x3-x2)/(z3-z2)*(z1Gap1-z2);
                    float x1Proj2=x2+(x3-x2)/(z3-z2)*(z1Gap2-z2);
                    float x1Proj3=x2+(x3-x2)/(z3-z2)*(z1Gap3-z2);
      		    float y1Proj1=y2+(y3-y2)/(z3-z2)*(z1Gap1-z2);
      		    float y1Proj2=y2+(y3-y2)/(z3-z2)*(z1Gap2-z2);
      		    float y1Proj3=y2+(y3-y2)/(z3-z2)*(z1Gap3-z2);
      
      		    float r1Proj1=sqrt(x1Proj1*x1Proj1+y1Proj1*y1Proj1);
	            float r1Proj2=sqrt(x1Proj2*x1Proj2+y1Proj2*y1Proj2);
      		    float r1Proj3=sqrt(x1Proj3*x1Proj3+y1Proj3*y1Proj3);
      
      		    float x2Proj1=x1+(x3-x1)/(z3-z1)*(z2Gap1-z1);
      		    float x2Proj2=x1+(x3-x1)/(z3-z1)*(z2Gap2-z1);
      		    float x2Proj3=x1+(x3-x1)/(z3-z1)*(z2Gap3-z1);
      		    float y2Proj1=y1+(y3-y1)/(z3-z1)*(z2Gap1-z1);
      		    float y2Proj2=y1+(y3-y1)/(z3-z1)*(z2Gap2-z1);
      		    float y2Proj3=y1+(y3-y1)/(z3-z1)*(z2Gap3-z1);
      
      		    float r2Proj1=sqrt(x2Proj1*x2Proj1+y2Proj1*y2Proj1);
	            float r2Proj2=sqrt(x2Proj2*x2Proj2+y2Proj2*y2Proj2);
      		    float r2Proj3=sqrt(x2Proj3*x2Proj3+y2Proj3*y2Proj3);
  

      		    float x3Proj1=x1+(x2-x1)/(z2-z1)*(z3Gap1-z1);
      		    float x3Proj2=x1+(x2-x1)/(z2-z1)*(z3Gap2-z1);
      		    float x3Proj3=x1+(x2-x1)/(z2-z1)*(z3Gap3-z1);
      		    float y3Proj1=y1+(y2-y1)/(z2-z1)*(z3Gap1-z1);
      		    float y3Proj2=y1+(y2-y1)/(z2-z1)*(z3Gap2-z1);
      		    float y3Proj3=y1+(y2-y1)/(z2-z1)*(z3Gap3-z1);
      
      		    float r3Proj1=sqrt(x3Proj1*x3Proj1+y3Proj1*y3Proj1);
      		    float r3Proj2=sqrt(x3Proj2*x3Proj2+y3Proj2*y3Proj2);
      		    float r3Proj3=sqrt(x3Proj3*x3Proj3+y3Proj3*y3Proj3);
      

      		    float phi2Proj1=-999;     
      		    if(fabs(x2Proj1)>0.0001){
			    phi2Proj1=atan(y2Proj1/x2Proj1);
                    }
      
      		    float phi2Proj2=-999;
      		    if(fabs(x2Proj2)>0.0001){
			    phi2Proj2=atan(y2Proj2/x2Proj2);
      		    }
      
      		    float phi2Proj3=-999;
      		    if(fabs(x2Proj3)>0.0001){
	      		    phi2Proj3=atan(y2Proj3/x2Proj3);
      		    }

      		    //fill in ntuple for stub variable


    
   		    float ntpvar[120]={0};

    		    ntpvar[0] = x1; 
    		    ntpvar[1] = y1;
    		    ntpvar[2] = z1;

		    ntpvar[3] = x2;
		    ntpvar[4] = y2;
		    ntpvar[5] = z2;
   
		    ntpvar[6] = x3; 
		    ntpvar[7] = y3;
		    ntpvar[8] = z3;

		    ntpvar[9] = theta1; 
		    ntpvar[10] = theta2;
		    ntpvar[11] = theta3;

		    ntpvar[12] = phi1; 
		    ntpvar[13] = phi2;
		    ntpvar[14] = phi3;
		    
		    ntpvar[15] = r1; 
		    ntpvar[16] = r2;
		    ntpvar[17] = r3;
    
		    ntpvar[18] = rPhi1;
		    ntpvar[19] = rPhi2;
		    ntpvar[20] = rPhi3;

		    ntpvar[21] = x1Proj;
		    ntpvar[22] = y1Proj;
		    ntpvar[23] = r1Proj;
		    ntpvar[24] = phi1Proj;

		    ntpvar[25] = x2Proj;
		    ntpvar[26] = y2Proj;
		    ntpvar[27] = r2Proj;
		    ntpvar[28] = phi2Proj;

		    ntpvar[29] = x3Proj;
		    ntpvar[30] = y3Proj;
		    ntpvar[31] = r3Proj;
		    ntpvar[32] = phi3Proj;
    
                    ntpvar[33] = x1Proj1;
                    ntpvar[34] = x1Proj2;
                    ntpvar[35] = x1Proj3;
                    ntpvar[36] = y1Proj1;
                    ntpvar[37] = y1Proj2;
                    ntpvar[38] = y1Proj3;
                    ntpvar[39] = r1Proj1;
                    ntpvar[40] = r1Proj2;
                    ntpvar[41] = r1Proj3;

                    ntpvar[42] = x2Proj1;
                    ntpvar[43] = x2Proj2;
                    ntpvar[44] = x2Proj3;
                    ntpvar[45] = y2Proj1;
                    ntpvar[46] = y2Proj2;
                    ntpvar[47] = y2Proj3;
                    ntpvar[48] = r2Proj1;
                    ntpvar[49] = r2Proj2;
                    ntpvar[50] = r2Proj3;

                    ntpvar[51] = x3Proj1;
                    ntpvar[52] = x3Proj2;
                    ntpvar[53] = x3Proj3;
                    ntpvar[54] = y3Proj1;
                    ntpvar[55] = y3Proj2;
                    ntpvar[56] = y3Proj3;
                    ntpvar[57] = r3Proj1;
                    ntpvar[58] = r3Proj2;
                    ntpvar[59] = r3Proj3;

                    ntpvar[60] = phi2Proj1;
                    ntpvar[61] = phi2Proj2;
                    ntpvar[62] = phi2Proj3;

		    ntpvar[63] = x1Gap1;
		    ntpvar[64] = x1Gap2;
		    ntpvar[65] = x1Gap3;
		    ntpvar[66] = x2Gap1;
		    ntpvar[67] = x2Gap2;
		    ntpvar[68] = x2Gap3;
		    ntpvar[69] = x3Gap1;
                    ntpvar[70] = x3Gap2;
                    ntpvar[71] = x3Gap3;

		    ntpvar[72] = y1Gap1;
		    ntpvar[73] = y1Gap2;
		    ntpvar[74] = y1Gap3;
		    ntpvar[75] = y2Gap1;
		    ntpvar[76] = y2Gap2;
		    ntpvar[77] = y2Gap3;
		    ntpvar[78] = y3Gap1;
		    ntpvar[79] = y3Gap2;
		    ntpvar[80] = y3Gap3;

		    ntpvar[81] = z1Gap1;
		    ntpvar[82] = z1Gap2;
		    ntpvar[83] = z1Gap3;
		    ntpvar[84] = z2Gap1;
		    ntpvar[85] = z2Gap2;
		    ntpvar[86] = z2Gap3;
		    ntpvar[87] = z3Gap1;
		    ntpvar[88] = z3Gap2;
		    ntpvar[89] = z3Gap3;
    
		    ntpvar[90] = r1Gap1;
		    ntpvar[91] = r1Gap2;
		    ntpvar[92] = r1Gap3;
		    ntpvar[93] = r2Gap1;
		    ntpvar[94] = r2Gap2;
		    ntpvar[95] = r2Gap3;
		    ntpvar[96] = r3Gap1;
		    ntpvar[97] = r3Gap2;
		    ntpvar[98] = r3Gap3;

		    ntpvar[99] = phi2Gap1;
		    ntpvar[100] = phi2Gap2;
		    ntpvar[101] = phi2Gap3;

		    ntpvar[102] = slope1;
		    ntpvar[103] = slope2;
		    ntpvar[104] = slope3;
		    ntpvar[105] = road_depth;

		    ntpvar[106] = oct1;
		    ntpvar[107] = oct2;   
		    ntpvar[108] = oct3;

		    ntpvar[109] = half1;
		    ntpvar[110] = half2;   
		    ntpvar[111] = half3;

		    ntpvar[112] = trk_arm;
    
		    _align_vars2->Fill(ntpvar);
	    //}//end of slope <3
    }//end of if x>0 - stubs in all 3 stations
  }

    std::cout << "I'm done with one event in zerofield data" << std::endl; 
    std::cout << "ended write_align_ntuple" << std::endl;
}

