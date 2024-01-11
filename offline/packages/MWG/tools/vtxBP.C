#include <PHMuoTracksOut.h>
#include "Tools.h"

//============================================================================
// compute DCA and BP vertex à la Mike
void Tools::vtxBP(PHMuoTracksOut* &muo, Int_t idx1, Int_t idx2, Float_t &dca, Float_t &xvtx, Float_t &yvtx, Float_t &zvtx)
{
    // calculate dca from bend plane momenta and geane positions at sta-1
    Float_t xbp0[3]={muo->get_st1_bp_pos(0,idx1),
		     muo->get_st1_bp_pos(1,idx1),
		     muo->get_st1_bp_pos(2,idx1)}; 
    Float_t pbp0[3]={muo->get_st1_bp_P(0,idx1),
		     muo->get_st1_bp_P(1,idx1),
		     muo->get_st1_bp_P(2,idx1)};
    Float_t tan0[2]; 
    tan0[0]=pbp0[0]/pbp0[2]; 
    tan0[1]=pbp0[1]/pbp0[2]; 
    //
    Float_t xbp1[3]={muo->get_st1_bp_pos(0,idx2),
		     muo->get_st1_bp_pos(1,idx2),
		     muo->get_st1_bp_pos(2,idx2)}; 
    Float_t pbp1[3]={muo->get_st1_bp_P(0,idx2),
		     muo->get_st1_bp_P(1,idx2),
		     muo->get_st1_bp_P(2,idx2)};
    Float_t tan1[2]; 
    tan1[0]=pbp1[0]/pbp1[2]; 
    tan1[1]=pbp1[1]/pbp1[2]; 
    //
    Float_t x0c[3], x1c[3]; int ifail = 0;
    Tools::distcls(xbp0,xbp1,tan0,tan1,x0c,x1c,ifail);
    
    if(ifail==0) {
	dca = sqrt(pow((x0c[0]-x1c[0]),2) + pow((x0c[1]-x1c[1]),2) + pow((x0c[2]-x1c[2]),2));
	xvtx = (x0c[0]+x1c[0])/2;
	yvtx = (x0c[1]+x1c[1])/2;
	zvtx = (x0c[2]+x1c[2])/2;
    }else{dca = xvtx = yvtx = zvtx = 0;}
}
void Tools::vtxBP(Float_t xbp0[3], Float_t pbp0[3], Float_t xbp1[3], Float_t pbp1[3], // input variables
		  Float_t &dca, Float_t &xvtx, Float_t &yvtx, Float_t &zvtx) // output variables
{
  // calculate dca from bend plane momenta and geane positions at sta-1
    Float_t tan0[2]; 
    tan0[0]=pbp0[0]/pbp0[2]; 
    tan0[1]=pbp0[1]/pbp0[2]; 
    Float_t tan1[2]; 
    tan1[0]=pbp1[0]/pbp1[2]; 
    tan1[1]=pbp1[1]/pbp1[2]; 
    //
    Float_t x0c[3], x1c[3]; int ifail = 0;
    Tools::distcls(xbp0,xbp1,tan0,tan1,x0c,x1c,ifail);
    
    if(ifail==0) {
	dca = sqrt(pow((x0c[0]-x1c[0]),2) + pow((x0c[1]-x1c[1]),2) + pow((x0c[2]-x1c[2]),2));
	xvtx = (x0c[0]+x1c[0])/2;
	yvtx = (x0c[1]+x1c[1])/2;
	zvtx = (x0c[2]+x1c[2])/2;
    }else{dca = xvtx = yvtx = zvtx = 0;} 
}
