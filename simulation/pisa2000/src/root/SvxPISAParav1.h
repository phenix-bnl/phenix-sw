#ifndef _SVXPISAPARAV1_
#define _SVXPISAPARAV1_

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Description of the SVX parameters (event independent)                //
// Version 1                                                            //  
// Sasha Lebedev (lebedev@iastate.edu)                                  //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "SvxPISAPara.h"

class SvxPISAParav1 : public SvxPISAPara {

public:

  SvxPISAParav1() {initialized=0;}  
  SvxPISAParav1(const Int_t i[], const Float_t f[]); 

  virtual ~SvxPISAParav1() {}

  void identify(std::ostream& os = std::cout) const;
  void Reset();
  int  isValid() const;

  int  get_sili_br_nlayers()       {return sili_br_nlayers;}
  int  get_sili_sideLayers()       {return sili_sideLayers;}
  int  get_nhh()                   {return nhh;}
  int  get_nbrv()                  {return nbrv;}
  int  get_necv()                  {return necv;}
  void set_sili_br_nlayers(int a)  {sili_br_nlayers=a; return;}
  void set_sili_sideLayers(int a)  {sili_sideLayers=a; return;}
  void set_nhh(int a)              {nhh=a; return;}
  void set_nbrv(int a)             {nbrv=a; return;}
  void set_necv(int a)             {necv=a; return;}

// Envelope/Cage parameters
  float get_sili_cg_rmn()        {return sili_cg_rmn;}
  float get_sili_cg_thck()       {return sili_cg_thck;}
  float get_sili_cg_inthck()     {return sili_cg_inthck;}
  float get_sili_cg_tempc()      {return sili_cg_tempc;}
  int   get_sili_cg_npcon()      {return sili_cg_npcon;}
  float get_sili_cg_z(int i)     {return sili_cg_z[i];}
  float get_sili_cg_rmx(int i)   {return sili_cg_rmx[i];}
  float get_sili_cg_xdisp()      {return sili_cg_xdisp;}
  float get_sili_cg_ydisp()      {return sili_cg_ydisp;}
  float get_sili_cg_zdisp()      {return sili_cg_zdisp;}
  void  set_sili_cg_rmn(float a)        {sili_cg_rmn=a; return;}
  void  set_sili_cg_thck(float a)       {sili_cg_thck=a; return;}
  void  set_sili_cg_inthck(float a)     {sili_cg_inthck=a; return;}
  void  set_sili_cg_tempc(float a)      {sili_cg_tempc=a; return;}
  void  set_sili_cg_npcon(int a)        {sili_cg_npcon=a; return;}
  void  set_sili_cg_z(int i, float a)   {sili_cg_z[i]=a; return;}
  void  set_sili_cg_rmx(int i, float a) {sili_cg_rmx[i]=a; return;}
  void  set_sili_cg_xdisp(float a)      {sili_cg_xdisp=a; return;}
  void  set_sili_cg_ydisp(float a)      {sili_cg_ydisp=a; return;}
  void  set_sili_cg_zdisp(float a)      {sili_cg_zdisp=a; return;}

// Barrel parameters
  float get_sili_br_snhalfx(int i)      {return sili_br_snhalfx[i];}
  float get_sili_br_snhalfy(int i)      {return sili_br_snhalfy[i];}
  float get_sili_br_snhalfz(int i)      {return sili_br_snhalfz[i];}
  float get_sili_br_x0add(int i)        {return sili_br_x0add[i];}
  float get_sili_br_snzgap(int i)       {return sili_br_snzgap[i];}
  float get_sili_br_tilt(int i)         {return sili_br_tilt[i];}
  int   get_sili_br_nsn(int i)          {return sili_br_nsn[i];}
  float get_sili_br_r(int i)            {return sili_br_r[i];}
  float get_sili_br_z(int i)            {return sili_br_z[i];}
  float get_sili_br_dphi(int i)         {return sili_br_dphi[i];}
  int   get_sili_br_nsec(int i)         {return sili_br_nsec[i];}
  float get_sili_br_phic(int i, int j)  {return sili_br_phic[i][j];}
  int   get_sili_br_nlad(int i, int j)  {return sili_br_nlad[i][j];}
  void  set_sili_br_snhalfx(int i, float a)     {sili_br_snhalfx[i]=a; return;}
  void  set_sili_br_snhalfy(int i, float a)     {sili_br_snhalfy[i]=a; return;}
  void  set_sili_br_snhalfz(int i, float a)     {sili_br_snhalfz[i]=a; return;}
  void  set_sili_br_x0add(int i, float a)       {sili_br_x0add[i]=a; return;}
  void  set_sili_br_snzgap(int i, float a)      {sili_br_snzgap[i]=a; return;}
  void  set_sili_br_tilt(int i, float a)        {sili_br_tilt[i]=a; return;}
  void  set_sili_br_nsn(int i, int a)           {sili_br_nsn[i]=a; return;}
  void  set_sili_br_r(int i, float a)           {sili_br_r[i]=a; return;}
  void  set_sili_br_z(int i, float a)           {sili_br_z[i]=a; return;}
  void  set_sili_br_dphi(int i, float a)        {sili_br_dphi[i]=a; return;}
  void  set_sili_br_nsec(int i, int a)          {sili_br_nsec[i]=a; return;}
  void  set_sili_br_phic(int i, int j, float a) {sili_br_phic[i][j]=a; return;}
  void  set_sili_br_nlad(int i, int j, int a)   {sili_br_nlad[i][j]=a; return;}

// Endcap parameters
  float get_sili_phi1_side(int i)     {return sili_phi1_side[i];}
  float get_sili_dph_side(int i)      {return sili_dph_side[i];}
  float get_sili_z1_side(int i)       {return sili_z1_side[i];}
  float get_sili_rmin1_side(int i)    {return sili_rmin1_side[i];}
  float get_sili_rmax1_side(int i)    {return sili_rmax1_side[i];}
  float get_sili_z2_side(int i)       {return sili_z2_side[i];}
  float get_sili_rmin2_side(int i)    {return sili_rmin2_side[i];}
  float get_sili_rmax2_side(int i)    {return sili_rmax2_side[i];}
  float get_sili_npdv_side(int i)     {return sili_npdv_side[i];}
  float get_sili_nz_side(int i)       {return sili_nz_side[i];}
  float get_sili_zCenter_side(int i)  {return sili_zCenter_side[i];}
  void  set_sili_phi1_side(int i, float a)    {sili_phi1_side[i]=a; return;}
  void  set_sili_dph_side(int i, float a)     {sili_dph_side[i]=a; return;}
  void  set_sili_z1_side(int i, float a)      {sili_z1_side[i]=a; return;}
  void  set_sili_rmin1_side(int i, float a)   {sili_rmin1_side[i]=a; return;}
  void  set_sili_rmax1_side(int i, float a)   {sili_rmax1_side[i]=a; return;}
  void  set_sili_z2_side(int i, float a)      {sili_z2_side[i]=a; return;}
  void  set_sili_rmin2_side(int i, float a)   {sili_rmin2_side[i]=a; return;}
  void  set_sili_rmax2_side(int i, float a)   {sili_rmax2_side[i]=a; return;}
  void  set_sili_npdv_side(int i, float a)    {sili_npdv_side[i]=a; return;}
  void  set_sili_nz_side(int i, float a)      {sili_nz_side[i]=a; return;}
  void  set_sili_zCenter_side(int i, float a) {sili_zCenter_side[i]=a; return;}

private:

  int initialized;

  int sili_br_nlayers;	// number of layers in barrel = 4
  int sili_sideLayers;	// number of layers in endcaps = 20
  int nhh;			// number of hit components
  int nbrv;			// number of barrel volume descriptors
  int necv;			// number of endcap volume descriptors

// Envelope/Cage parameters
  float sili_cg_rmn;
  float sili_cg_thck;
  float sili_cg_inthck;
  float sili_cg_tempc;
  int   sili_cg_npcon;  	// =2
  float sili_cg_z[2];		// dimension = sili_cg_npcon
  float sili_cg_rmx[2];       // dimension = sili_cg_npcon
  float sili_cg_xdisp;
  float sili_cg_ydisp;
  float sili_cg_zdisp;

// Barrel parameters
  float sili_br_snhalfx[4];	// dimension: sili_br_nlayers
  float sili_br_snhalfy[4];	
  float sili_br_snhalfz[4];	
  float sili_br_x0add[4];
  float sili_br_snzgap[4];
  float sili_br_tilt[4];
  int   sili_br_nsn[4];	// number of sensors: 4, 4, 5, 6
  float sili_br_r[4];
  float sili_br_z[4];
  float sili_br_dphi[4];
  int   sili_br_nsec[4];  	// number of sectors: 2, 2, 2, 2
  float sili_br_phic[2][4];	// dimensions: sili_br_nsn x sili_br_nlayers
  int sili_br_nlad[2][4];	// dimensions: sili_br_nsn x sili_br_nlayers
	
// Endcap parameters
  float sili_phi1_side[20];	// dimension: sili_sideLayers
  float sili_dph_side[20];
  float sili_z1_side[20];
  float sili_rmin1_side[20];
  float sili_rmax1_side[20];
  float sili_z2_side[20];
  float sili_rmin2_side[20];
  float sili_rmax2_side[20];
  float sili_npdv_side[20];
  float sili_nz_side[20];
  float sili_zCenter_side[20];

   ClassDef(SvxPISAParav1,1) 
};

#endif

