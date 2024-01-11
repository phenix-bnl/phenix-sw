/*! 
  @defgroup analysis Analysis
  Analysis Event Loops. Routines in this packages contain pdst/ezdst style
  event loops for iterating throught MUTOO DST output.
*/

/*! ingroup analysis */
/*! \file ana_dst.cxx 
  Prototype ezdst type analysis routine.  It demos how to access the 
  data in MUTOO Interface Object Containers from the disk resident DST.  
  This a a shameless hack some code Chris wrote.
*/

#include <iostream>
#include "ana_dst.h"
#include "ezdst.h"
#include "ZdcOut.h"
#include "BbcOut.h"
#include "VtxOut.h"

#include "TMutNode.h"
#include "TMutMuiRoadMap.h"
#include "TMutHitMap.h"
#include "TMutClusMap.h"
#include "TMutCoordMap.h"
#include "TMutTrkMap.h"
#include "TMutStubMap.h"
#include "TMutClusMap.h"
#include "TMutMCHitMap.h"

#include "TrigLvl1.h"
#include "TrigRunLvl1.h"
#include "TrigRunLvl2.h"

#include "TSystem.h"
#include "PhenixRun.hh"
#include "PhCanvas.hh"
#include "PhDchDisplay.hh"
#include "PhPadDisplay.hh"
#include "PhTecDisplay.hh"
#include "PhMuiDisplay.hh"
#include "PhMutDisplay.hh"
#include "PhEventDisplay.hh"
#include "PhMutooDisplay.h"

#include "TriggerHelper.h"
#include "utiCentrality.h"

#include "TFile.h"
#include "TH1.h"
#include "TNtuple.h"
#include "TH1.h"

TFile*   ana_file;
TNtuple* nt1;
TNtuple* nt_alig;
TH1F* prox_h;
PHCompositeNode* mutoo_node;
PHCompositeNode* top_node;
std::string output_ntuple_name;
static bool init_done = false;
PhMutooDisplay *display = 0;

static bool display_mode = false;

int process_event (DstContent *dst)
{
  try {  
    
    PHCompositeNode* dst_node = dst->get_topNode();

    static int ievent=0;
    if(!init_done) { 
      setup_all(dst);
      if(display_mode) setup_display();
      top_node->addNode(mutoo_node);
      top_node->addNode(dst->get_topNode());
      top_node->print();
    }  
    // Clear maps (IOCs) from last event
    //
    PHMapManager::clear();

    // This call fills the IOCs from the DST-resident objects
    //
    PHMapManager::read(dst_node);

    // Get the TMutStubMap pointer from the node tree
    //
    TMutStubMap* stub_map = TMutNode<TMutStubMap>::find_node(mutoo_node,"TMutStubMap");    



    // Loop over TMutStub
    //   

      float x1 = -1000;
      float y1 = -1000;
      float z1 = -1000;
	
      float x2 = -1000;
      float y2 = -1000;
      float z2 = -1000;
	
      float x3 = -1000;
      float y3 = -1000;
      float z3 = -1000;

      float dx1 = -1000;
      float dy1 = -1000;
      float dx2 = -1000;
      float dy2 = -1000;
      float dx3 = -1000;
      float dy3 = -1000;
      float slope1 = -1000;
      float slope2 = -1000;
      float slope3 = -1000;
	
      float r1 = -1000;
      float r2 = -1000;
      float r3 = -1000;
	
      float phi1 = -1000;
      float phi2 = -1000;
      float phi3 = -1000;

      float oct1 = -1000;
      float oct2 = -1000;
      float oct3 = -1000;
      float half1 = -1000;
      float half2 = -1000;
      float half3 = -1000;



	
      float theta1 = -1000;
      float theta2 = -1000;
      float theta3 = -1000;
      float rPhi1 = -1000;
      float rPhi2 = -1000;
      float rPhi3 = -1000;


      float x1Gap1 = -1000;
      float y1Gap1 = -1000;
      float z1Gap1 = -1000;
      float x2Gap1 = -1000;
      float y2Gap1 = -1000;
      float z2Gap1 = -1000;
      float x3Gap1 = -1000;
      float y3Gap1 = -1000;
      float z3Gap1 = -1000;
      
      
      float x1Gap2 = -1000;
      float y1Gap2 = -1000;
      float z1Gap2 = -1000;
      float x2Gap2 = -1000;
      float y2Gap2 = -1000;
      float z2Gap2 = -1000;
      float x3Gap2 = -1000;
      float y3Gap2 = -1000;
      float z3Gap2 = -1000;
 
      float phi2Gap1 = -1000;
      float phi2Gap2 = -1000;
      float phi2Gap3 = -1000;
      float r1Gap1 = -1000;
      float r1Gap2 = -1000;
      float r1Gap3 = -1000;
      float r2Gap1 = -1000;
      float r2Gap2 = -1000;
      float r2Gap3 = -1000;
      float r3Gap1 = -1000;
      float r3Gap2 = -1000;
      float r3Gap3 = -1000;
      
      float x1Gap3 = -1000;
      float y1Gap3 = -1000;
      float z1Gap3 = -1000;
      float x2Gap3 = -1000;
      float y2Gap3 = -1000;
      float z2Gap3 = -1000;
      float x3Gap3 = -1000;
      float y3Gap3 = -1000;
      float z3Gap3 = -1000;
      
      //float phi2Proj1, phi2Proj2, phi2Proj3 ;
      //float r2Proj1,r2Proj2, r2Proj3; 

      //float r1Proj1,r1Proj2, r1Proj3; 
      //float r3Proj1,r3Proj2, r3Proj3; 
  

    TMutStubMap::const_iterator stub_iter = stub_map->range();
    //stub_map->print();
    
    while(TMutStubMap::const_pointer stub_ptr = stub_iter.next()){
     
      //
      //stub_ptr->get()->print(std::cout,true);


      // Only do the south arm for now

      float stub_arm = stub_ptr->get()->get_arm();
      if(stub_arm==0){



      // Loop over TMutStub residuals and dump data into ntuple
      //      

      float nt_vars[100]={0};
      

      const TMutStub::residual_list* residuals = stub_ptr->get()->get_w_residual_list();


      TMutCoordMap::const_key_iterator coord_iter = stub_ptr->get()->get_associated<TMutCoord>();

      TMutGapCoordMap::const_key_iterator gap_iter =stub_ptr->get()->get_associated<TMutGapCoord>();
      
    
      
      // go through the gap coordinates associated with stubs

      while(TMutGapCoordMap::const_pointer gap_ptr = gap_iter.next()){
	//gap_ptr->get()->print();
	
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
	     }else{
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
	 //cout<<"Now check z1 again"<<z1<<endl;
	
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

	 //	 cout<<"Now check z2 again"<<z2<<endl;

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
	 //cout<<"Now check z3 again"<<z3<<endl;

	 r3=sqrt(x3*x3+y3*y3);
	 theta3=atan(r3/z3);

	 if(fabs(x3)>0.001){
	   phi3=atan(y3/x3);
	 }else{
	   phi3=-999;
	 }	   

      }


      rPhi1=r1*phi1;
      rPhi2=r2*phi2;
      rPhi3=r3*phi3;
      

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
	double tan_theta = std::sqrt(MUTOO::SQUARE(res_iter->get_fit_par()->get_dydz()) +
				     MUTOO::SQUARE(res_iter->get_fit_par()->get_dxdz()));
	nt_vars[19] = std::atan(tan_theta);
	nt_vars[20] = res_iter->get_fit_par()->get_dxdz();
	nt_vars[21] = res_iter->get_fit_par()->get_dydz();
	nt_vars[22] = res_iter->get_fit_par()->get_x();
	nt_vars[23] = res_iter->get_fit_par()->get_y();
	nt_vars[24] = res_iter->get_cos_theta_r();
	nt1->Fill(nt_vars);
      }

      }//end condition that arm == 0

    }//end of the stub loop

    //projections using stubs inform. of each evt

    if (x1> -1000 && x2> -1000 && x3 > -1000){
      if (slope1 < 3 && slope2 < 3 && slope3 < 3){ 
      
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
      
      
      float x1Proj=x2+(x3-x2)/(z3-z2)*(z1-z2);
      float y1Proj=y2+(y3-y2)/(z3-z2)*(z1-z2);
      float r1Proj=sqrt(x1Proj*x1Proj+y1Proj*y1Proj);
      float phi1Proj=-999;
      if(fabs(x1Proj)>0.0001){
	phi1Proj=atan(y1Proj/x1Proj);
      }
      
      //cout<<"**********************Now check z1 again"<<z1<<endl;
      //cout<<"**********************Now check z3 again"<<z3<<endl;

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
      
    
    float ntpvar[100]={0};
 
    ntpvar[0] = x1;
 

    ntpvar[1] = y1;
 
    ntpvar[2] = z1;

    ntpvar[3] = x2;
    ntpvar[4] = y2;
    ntpvar[85] = z2;
   
    ntpvar[5] = x3;
    ntpvar[6] = y3;
    ntpvar[7] = z3;
    ntpvar[8] = theta1;
    ntpvar[9] = theta2;
    ntpvar[10] = theta3;
    ntpvar[11] = phi1;

    ntpvar[12] = phi2;
    ntpvar[13] = phi3;
    ntpvar[14] = r1;
    ntpvar[15] = r2;
    ntpvar[16] = r3;
    
    ntpvar[17] = rPhi1;
    ntpvar[18] = rPhi2;
    ntpvar[19] = rPhi3;
  
    
    ntpvar[20] = x2Proj;
    ntpvar[21] = y2Proj;
    ntpvar[22] = r2Proj;
    ntpvar[23] = phi2Proj;
    ntpvar[24] = x1Proj;
    ntpvar[25] = y1Proj;
    ntpvar[26] = r1Proj;
    ntpvar[27] = phi1Proj;
    ntpvar[28] = x3Proj;
    ntpvar[29] = y3Proj;
    ntpvar[30] = r3Proj;
    ntpvar[31] = phi3Proj;
    
    ntpvar[32] = x2Proj1;
    ntpvar[33] = y2Proj1;
    ntpvar[34] = r2Proj1;
    ntpvar[35] = phi2Proj1;
    
    ntpvar[36] = x2Proj2;
    ntpvar[37] = y2Proj2;
    ntpvar[38] = r2Proj2;
    ntpvar[39] = phi2Proj2;
    //
    ntpvar[40] = x2Proj3;
    ntpvar[41] = y2Proj3;
    ntpvar[42] = r2Proj3;
    ntpvar[43] = phi2Proj3;
    //
    ntpvar[44] = phi2Gap1;
    ntpvar[45] = phi2Gap2;
    ntpvar[46] = phi2Gap3;
    //
    ntpvar[47] = z2Gap1;
    ntpvar[48] = z2Gap2;
	     
    ntpvar[49] = z2Gap3;
    //
    //
    ntpvar[50] = r2Gap1;
    ntpvar[51] = r2Gap2;
    ntpvar[52] = r2Gap3;
    ntpvar[53] = x2Gap1;
    ntpvar[54] = x2Gap2;
    ntpvar[55] = x2Gap3;
    ntpvar[56] = y2Gap1;
    ntpvar[57] = y2Gap2;
    ntpvar[58] = y2Gap3;
    //
    ntpvar[59] = r3Gap1;
    ntpvar[60] = r3Gap2;
    ntpvar[61] = r3Gap3;
    ntpvar[62] = r3Proj1;
    ntpvar[63] = r3Proj2;   
    ntpvar[64] = r3Proj3;
    //
    ntpvar[65] = r1Gap1;
    ntpvar[66] = r1Gap2;
    ntpvar[67] = r1Gap3;
    ntpvar[68] = r1Proj1;
    ntpvar[69] = r1Proj2;   
    ntpvar[70] = r1Proj3;
    ntpvar[71] = y3Gap1;
    ntpvar[72] = y3Gap2;
    ntpvar[73] = x3Gap1;
    ntpvar[74] = x3Gap2;
    ntpvar[75] = y1Gap1;
    ntpvar[76] = y1Gap2;
    ntpvar[77] = x1Gap1;
    ntpvar[78] = x1Gap2;

    ntpvar[79] = oct1;
    ntpvar[80] = oct2;   
    ntpvar[81] = oct3;

    ntpvar[82] = half1;
    ntpvar[83] = half2;   
    ntpvar[84] = half3;
  
    
    nt_alig->Fill(ntpvar);

    }//end of slope <3
    }//end of if x>0 - stubs in all 3 stations





    // Do the display
    //
    if(display_mode) {
      gPhenix->SetRunNum(0);
      gPhenix->event(top_node);  
      gPhenix->Draw();
    }
    
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
  
  return 0;  
}

int setup_all(DstContent *dst) {
  ana_file = new TFile(output_ntuple_name.c_str(),"recreate");
  nt1 = new TNtuple("nt1","nt1","arm:sta:oct:hoct:gap:cath:q_peak:q_tot:cos_ac:cos_wz:w_trk:w_meas:r_trk:chi_w:chi_r:n_coord:n_gap:w_fit_err:clus_width:theta:dxdz:dydz:x:y:cos_th_r:z");

  nt_alig = new TNtuple("nt_alig","nt_alig","x1:y1:z1:x2:y2:x3:y3:z3\
:theta1:theta2:theta3:phi1:phi2:phi3\
:r1:r2:r3:rPhi1:rPhi2:rPhi3\
:x2Proj:y2Proj:r2Proj:phi2Proj\
:x1Proj:y1Proj:r1Proj:phi1Proj\
:x3Proj:y3Proj:r3Proj:phi3Proj\
:x2Proj1:y2Proj1:r2Proj1:phi2Proj1\
:x2Proj2:y2Proj2:r2Proj2:phi2Proj2\
:x2Proj3:y2Proj3:r2Proj3:phi2Proj3\
:phi2Gap1:phi2Gap2:phi2Gap3\
:z2Gap1:z2Gap2:z2Gap3\
:r2Gap1:r2Gap2:r2Gap3\
:x2Gap1:x2Gap2:x2Gap3\
:y2Gap1:y2Gap2:y2Gap3\
:r3Gap1:r3Gap2:r3Gap3\
:r3Proj1:r3Proj2:r3Proj3\
:r1Gap1:r1Gap2:r1Gap3\
:r1Proj1:r1Proj2:r1Proj3\
:y3Gap1:y3Gap2:x3Gap1:x3Gap2\
:y1Gap1:y1Gap2:x1Gap1:x1Gap2\
:oct1:oct2:oct3:half1:half2:half3:z2");


  prox_h = new TH1F("prox_h","prox_h",100,0,50);

  PHCompositeNode* dst_node = dst->get_topNode();

  // create a new top node
  //
  top_node = new PHCompositeNode("NEWTOP");

  // create a node for MUTOO interface object containers
  //
  mutoo_node = new PHCompositeNode("MUTOO");

  try {
  // Create the the IOCs here and couple to input DST
  //
    //TMutNode<TMutMuiRoadMap>::new_dst_input_node(mutoo_node,"TMutMuiRoadMap", dst_node,"TMutMuiRoad" );
  TMutNode<TMutHitMap>::new_dst_input_node(mutoo_node,"TMutHitMap", dst_node,"TMutHit" );
  TMutNode<TMutClusMap>::new_dst_input_node(mutoo_node,"TMutClusMap", dst_node,"TMutClus");
  TMutNode<TMutCoordMap>::new_dst_input_node(mutoo_node,"TMutCoordMap", dst_node,"TMutCoord");
  TMutNode<TMutGapCoordMap>::new_dst_input_node(mutoo_node,"TMutGapCoordMap", dst_node,"TMutGapCoord");
  TMutNode<TMutStubMap>::new_dst_input_node(mutoo_node, "TMutStubMap",dst_node,"TMutStub");
  TMutNode<TMutTrkMap>::new_dst_input_node(mutoo_node, "TMutTrkMap",dst_node,"TMutTrk");

  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
  init_done = true;
  std::cout<<"Haven't crashed yet\n" << std::endl;
  return 0;
};

int end_all() {
  cout<<"Writing output histograms (goodbye)\n";
  cout<<"  --> Root output file (top.root) created.\n";
  ana_file->Write();
  return 0;
}

void setup_display() {
  
  if(!gClient){
    cout<<"you are running in batch"<<endl;
    return;
  }
  
  // This creates the gPhenix pointer
  //
  new PhenixRun("Phenix-Run","Phenix-Run",top_node);
  
  // Add MUTOO event display
  //
  display = new PhMutooDisplay();
  gPhenix->GetDisplay()->AddSubsystem(display);
  
  // Hack from dispfuncs.C
  //
  PhEventDisplay* disp = new PhEventDisplay("Main",
					    gClient->GetRoot(),
					    1000,
					    1000,
					    gPhenix->GetDisplay()->GetList());
  
  PhCanvas* can = (PhCanvas*)(disp->GetCanvas());
  can->DefaultRange(-550,-550,-550,550,550,550); //in cm
  can->ResetRange();
  disp->Popup();  
}

void draw_plane(UShort_t arm, UShort_t octant){
  display->paint_plane_view(arm,octant);
}

void
set_ntuple_name(char* ntuple_filename)
{
  output_ntuple_name = std::string(ntuple_filename);
}









