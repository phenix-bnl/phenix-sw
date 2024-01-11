///////////////////////////////////////////////////////////
//							 //
//     Lvl2 two point tracker tracks in RUN3 
//							 //
//      Author:  Tony Frawley (from template by Wei) //
//							 //
///////////////////////////////////////////////////////////

#ifndef __L2SingleTrackEMCLowOcupyMicroV1_H__
#define __L2SingleTrackEMCLowOcupyMicroV1_H__

#include <PHObject.h>

class L2SingleTrackEMCLowOcupyMicroV1 : public TObject 
{

 protected:

  int arm;
  float alpha;
  float beta;
  float p;
  float pxdet;
  float pydet;
  float pzdet;
  float pxphys;
  float pyphys;
  float pzphys;
  float zvertex;
  float xdet;
  float ydet;
  float zdet;
  float xdetout;
  float ydetout;
  float zdetout;
  float theta0;
  float phi0;
  float charge;
  int tileID;
  float energy;
  int CNTmatch;

  public:

    L2SingleTrackEMCLowOcupyMicroV1();
    ~L2SingleTrackEMCLowOcupyMicroV1(){}

     void    set_arm(int input) {arm=input;}
     void    set_alpha(float input) {alpha=input;} 
     void    set_beta(float input) {beta=input;} 
     void    set_p(float input) {p=input;}
     void    set_pxdet(float input) {pxdet=input;}
     void    set_pydet(float input) {pydet=input;}
     void    set_pzdet(float input) {pzdet=input;}
     void    set_pxphys(float input) {pxphys=input;}
     void    set_pyphys(float input) {pyphys=input;}
     void    set_pzphys(float input) {pzphys=input;}
     void    set_zvertex(float input) {zvertex=input;}
     void    set_xdet(float input) {xdet=input;}
     void    set_ydet(float input) {ydet=input;}
     void    set_zdet(float input) {zdet=input;}
     void    set_xdetout(float input) {xdetout=input;}
     void    set_ydetout(float input) {ydetout=input;}
     void    set_zdetout(float input) {zdetout=input;}
     void    set_theta0(float input) {theta0=input;}
     void    set_phi0(float input) {phi0=input;}
     void    set_charge(float input) {charge=input;}
     void    set_tileID(int input) {tileID=input;}
     void    set_energy(float input) {energy=input;}
     void    set_CNTmatch(int input) {CNTmatch=input;}
     
     int      get_arm() {return arm;}
     float    get_alpha() {return alpha;}
     float    get_beta()  {return beta;}
     float    get_p() {return p;}
     float    get_pxdet() {return pxdet;}
     float    get_pydet() {return pydet;}
     float    get_pzdet() {return pzdet;}
     float    get_pxphys() {return pxphys;}
     float    get_pyphys() {return pyphys;}
     float    get_pzphys() {return pzphys;}
     float    get_zvertex() {return zvertex;}
     float    get_xdet() {return xdet;}
     float    get_ydet() {return ydet;}
     float    get_zdet() {return zdet;}
     float    get_xdetout() {return xdetout;}
     float    get_ydetout() {return ydetout;}
     float    get_zdetout() {return zdetout;}
     float    get_theta0() {return theta0;}
     float    get_phi0() {return phi0;}
     float    get_charge() {return charge;}
     int      get_tileID() {return tileID;}
     float    get_energy() {return energy;}
     int      get_CNTmatch() {return CNTmatch;}

     ClassDef (L2SingleTrackEMCLowOcupyMicroV1, 1)
};
#endif	// __ L2SingleTrackEMCLowOcupyMicroV1_H__


