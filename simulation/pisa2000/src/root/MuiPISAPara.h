#ifndef _MuiPISAPara_
#define _MuiPISAPara_

//////////////////////////////////////////////////////////////////////////
//      T. K. Ghosh, vanderbilt, 07. 23.99                                                                 //
//////////////////////////////////////////////////////////////////////////
#include "TObject.h"
#include "TClonesArray.h"


class MuiPISAPara : public TObject {


  Int_t nr_muon_arms ;
  Int_t nr_muid_planes ;
  Int_t ndet_per_pl ;
  Int_t mu_floor_flg ;
  Float_t muid_zmin ;
  Float_t muid_zmax ;
  Float_t muid_ang ;
  Float_t beam_height;
  Float_t mu_top_height;
  Float_t mu_floor_height;
  Float_t mu_strp_thick;
  Float_t mu_gas_thick ;
  Float_t mu_plas_thick ;
  Float_t mu_yoke_thick[2];
  Float_t mu_donut_thick1[2];
  Float_t mu_donut_thick2[2];
  Float_t rmax1_donut1[2];
  Float_t rmax1_donut2[2];
  Float_t rmax2_donut1[2];
  Float_t rmax2_donut2[2];
  Float_t mu_floor_thick ;
  Float_t mu_abs_thick[11];
  Float_t mu_hi_abs[11];
  Float_t mu_hi_med[11];
  Int_t mu_lo_med[11] ;
  Int_t nmed_mu_ps ;
  Int_t nmed_mu_gas;
  Int_t nmed_mu_cs ;
  Int_t nmed_mu_sh ;
  Int_t nmed_mu_sd ;
  Int_t nmed_mudn ;
  Int_t nmed_muhl ;
  Int_t nmed_mufl ;
  Int_t nmed_yoke ;
  Int_t mu_hi_color;
  Int_t mu_lo_color ;
  Int_t color_muid ;
  Int_t color_hole ;
  Int_t color_dont ;
  Int_t color_floor;
  Int_t color_strd ;
  Int_t color_yoke ;
  Float_t rykmin1[2];
  Float_t rykmin2[2];
  Float_t rmin_donut[2];
  Float_t zyoke[2];
  Float_t zgap_yoke_abs ;
  Float_t zgap_labs_ldet;
  Float_t muid_delx ;
  Float_t str_xstag ;
  Int_t muhl_shld_flag ;
  Int_t color_muhl_shld ;
  Int_t nmed_muhl_shld ;
  Float_t z_muhl_shld[2] ;
  Float_t thick_muhl_shld ;
  Int_t muhl_config_flag ;
  Int_t muabs_config_flag ;
  Int_t npl_muhl ; 
  Float_t z_muhl[6];
  Float_t rmin_muhl[6];
  Float_t rmax_muhl[6];

public:
   MuiPISAPara() { }  // Default constructor (needed for arrays of objects)
   MuiPISAPara(const Int_t i[], const Float_t f[]);  // Main constructor

   virtual ~MuiPISAPara() { }

   ClassDef(MuiPISAPara,1)  // A MUI  parameter instance
};

#endif




















