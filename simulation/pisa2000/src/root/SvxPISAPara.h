#ifndef _SVXPISAPARA_
#define _SVXPISAPARA_

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Bas virtual class.                                                   //
// Description of the SVX parameters (event independent).               //
// Sasha Lebedev (lebedev@iastate.edu)                                  //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <iostream>

#include "TObject.h"

class SvxPISAPara : public TObject {

public:

 virtual ~SvxPISAPara() { }

 virtual void identify(std::ostream& os = std::cout) const; 
 virtual void Reset();
 virtual int  isValid() const;

 virtual int  get_sili_br_nlayers() 	  {virtual_warning("get_sili_br_nlayers"); return -9999;}	
 virtual int  get_sili_sideLayers() 	  {virtual_warning("get_sili_sideLayers"); return -9999;}
 virtual int  get_nhh()			  {virtual_warning("get_nhh"); return -9999;}		
 virtual int  get_nbrv() 		  {virtual_warning("get_nbrv"); return -9999;}		
 virtual int  get_necv() 		  {virtual_warning("get_necv"); return -9999;}		
 virtual void set_sili_br_nlayers(int a)  {virtual_warning("set_sili_br_nlayers"); return;}
 virtual void set_sili_sideLayers(int a)  {virtual_warning("set_sili_sideLayers"); return;}
 virtual void set_nhh(int a) 		  {virtual_warning("set_nhh"); return;}	
 virtual void set_nbrv(int a) 		  {virtual_warning("set_nbrv"); return;}	
 virtual void set_necv(int a) 		  {virtual_warning("set_necv"); return;}

// Envelope/Cage parameters
 virtual float get_sili_cg_rmn()        {virtual_warning("get_sili_cg_rmn"); return -9999.;}
 virtual float get_sili_cg_thck()       {virtual_warning("get_sili_cg_thck"); return -9999.;}
 virtual float get_sili_cg_inthck()     {virtual_warning("get_sili_cg_inthck"); return -9999.;}
 virtual float get_sili_cg_tempc()      {virtual_warning("get_sili_cg_tempc"); return -9999.;}
 virtual int   get_sili_cg_npcon()      {virtual_warning("get_sili_cg_npcon"); return -9999;}
 virtual float get_sili_cg_z(int i)     {virtual_warning("get_sili_cg_z"); return -9999.;}
 virtual float get_sili_cg_rmx(int i)   {virtual_warning("get_ili_cg_rmx"); return -9999.;}
 virtual float get_sili_cg_xdisp()      {virtual_warning("get_sili_cg_xdisp"); return -9999.;}
 virtual float get_sili_cg_ydisp()      {virtual_warning("get_sili_cg_ydisp"); return -9999.;}
 virtual float get_sili_cg_zdisp()      {virtual_warning("get_sili_cg_zdisp"); return -9999.;}
 virtual void  set_sili_cg_rmn(float a)        {virtual_warning("set_sili_cg_rmn"); return;}
 virtual void  set_sili_cg_thck(float a)       {virtual_warning("set_sili_cg_thck"); return;}
 virtual void  set_sili_cg_inthck(float a)     {virtual_warning("set_sili_cg_inthck"); return;}
 virtual void  set_sili_cg_tempc(float a)      {virtual_warning("set_sili_cg_tempc"); return;}
 virtual void  set_sili_cg_npcon(int a)        {virtual_warning("set_sili_cg_npcon"); return;}
 virtual void  set_sili_cg_z(int i, float a)   {virtual_warning("set_sili_cg_z"); return;}
 virtual void  set_sili_cg_rmx(int i, float a) {virtual_warning("set_sili_cg_rmx"); return;}
 virtual void  set_sili_cg_xdisp(float a)      {virtual_warning("set_sili_cg_xdisp"); return;}
 virtual void  set_sili_cg_ydisp(float a)      {virtual_warning("set_sili_cg_ydisp"); return;}
 virtual void  set_sili_cg_zdisp(float a)      {virtual_warning("set_sili_cg_zdisp"); return;}

// Barrel parameters
 virtual float get_sili_br_snhalfx(int i)      {virtual_warning("get_sili_br_snhalfx"); return -9999.;}
 virtual float get_sili_br_snhalfy(int i)      {virtual_warning("get_sili_br_snhalfy"); return -9999.;}	
 virtual float get_sili_br_snhalfz(int i)      {virtual_warning("get_sili_br_snhalfz"); return -9999.;}
 virtual float get_sili_br_x0add(int i)        {virtual_warning("get_sili_br_x0add"); return -9999.;}
 virtual float get_sili_br_snzgap(int i)       {virtual_warning("get_sili_br_snzgap"); return -9999.;}
 virtual float get_sili_br_tilt(int i)         {virtual_warning("get_sili_br_tilt"); return -9999.;}
 virtual int   get_sili_br_nsn(int i)          {virtual_warning("get_sili_br_nsn"); return -9999;}
 virtual float get_sili_br_r(int i)            {virtual_warning("get_sili_br_r"); return -9999.;}
 virtual float get_sili_br_z(int i)            {virtual_warning("get_sili_br_z"); return -9999.;}
 virtual float get_sili_br_dphi(int i)         {virtual_warning("get_sili_br_dphi"); return -9999.;}
 virtual int   get_sili_br_nsec(int i)         {virtual_warning("get_sili_br_nsec"); return -9999;}
 virtual float get_sili_br_phic(int i, int j)  {virtual_warning("get_sili_br_phic"); return -9999.;}
 virtual int   get_sili_br_nlad(int i, int j)  {virtual_warning("get_sili_br_nlad"); return -9999;}
 virtual void  set_sili_br_snhalfx(int i, float a)     {virtual_warning("set_sili_br_snhalfx"); return;}
 virtual void  set_sili_br_snhalfy(int i, float a)     {virtual_warning("set_sili_br_snhalfy"); return;}
 virtual void  set_sili_br_snhalfz(int i, float a)     {virtual_warning("set_sili_br_snhalfz"); return;}	
 virtual void  set_sili_br_x0add(int i, float a)       {virtual_warning("set_sili_br_x0add"); return;}
 virtual void  set_sili_br_snzgap(int i, float a)      {virtual_warning("set_sili_br_snzgap"); return;}
 virtual void  set_sili_br_tilt(int i, float a)        {virtual_warning("set_sili_br_tilt"); return;}
 virtual void  set_sili_br_nsn(int i, int a)           {virtual_warning("set_sili_br_nsn"); return;}
 virtual void  set_sili_br_r(int i, float a)           {virtual_warning("set_sili_br_r"); return;}
 virtual void  set_sili_br_z(int i, float a)           {virtual_warning("set_sili_br_z"); return;}
 virtual void  set_sili_br_dphi(int i, float a)        {virtual_warning("set_sili_br_dphi"); return;}
 virtual void  set_sili_br_nsec(int i, int a)          {virtual_warning("set_sili_br_nsec"); return;}
 virtual void  set_sili_br_phic(int i, int j, float a) {virtual_warning("set_sili_br_phic"); return;}
 virtual void  set_sili_br_nlad(int i, int j, int a)   {virtual_warning("set_sili_br_nlad"); return;}
	
// Endcap parameters
 virtual float get_sili_phi1_side(int i)     {virtual_warning("get_sili_phi1_side"); return -9999.;}
 virtual float get_sili_dph_side(int i)      {virtual_warning("get_sili_dph_side"); return -9999.;}
 virtual float get_sili_z1_side(int i)       {virtual_warning("get_sili_z1_side"); return -9999.;}
 virtual float get_sili_rmin1_side(int i)    {virtual_warning("get_sili_rmin1_side"); return -9999.;}
 virtual float get_sili_rmax1_side(int i)    {virtual_warning("get_sili_rmax1_side"); return -9999.;}
 virtual float get_sili_z2_side(int i)       {virtual_warning("get_sili_z2_side"); return -9999.;}
 virtual float get_sili_rmin2_side(int i)    {virtual_warning("get_sili_rmin2_side"); return -9999.;}
 virtual float get_sili_rmax2_side(int i)    {virtual_warning("get_sili_rmax2_side"); return -9999.;}
 virtual float get_sili_npdv_side(int i)     {virtual_warning("get_sili_npdv_side"); return -9999.;}
 virtual float get_sili_nz_side(int i)       {virtual_warning("get_sili_nz_side"); return -9999.;}
 virtual float get_sili_zCenter_side(int i)  {virtual_warning("get_sili_zCenter_side"); return -9999.;}
 virtual void  set_sili_phi1_side(int i, float a)    {virtual_warning("set_sili_phi1_side"); return;}
 virtual void  set_sili_dph_side(int i, float a)     {virtual_warning("set_sili_dph_side"); return;}
 virtual void  set_sili_z1_side(int i, float a)      {virtual_warning("set_sili_z1_side"); return;}
 virtual void  set_sili_rmin1_side(int i, float a)   {virtual_warning("set_sili_rmin1_side"); return;}
 virtual void  set_sili_rmax1_side(int i, float a)   {virtual_warning("set_sili_rmax1_side"); return;}
 virtual void  set_sili_z2_side(int i, float a)      {virtual_warning("set_sili_z2_side"); return;}
 virtual void  set_sili_rmin2_side(int i, float a)   {virtual_warning("set_sili_rmin2_side"); return;}
 virtual void  set_sili_rmax2_side(int i, float a)   {virtual_warning("set_sili_rmax2_side"); return;}
 virtual void  set_sili_npdv_side(int i, float a)    {virtual_warning("set_sili_npdv_side"); return;}
 virtual void  set_sili_nz_side(int i, float a)      {virtual_warning("set_sili_nz_side"); return;}
 virtual void  set_sili_zCenter_side(int i, float a) {virtual_warning("set_sili_zCenter_side"); return;}

private:
  void virtual_warning(const char *funcname) const;

   ClassDef(SvxPISAPara,1) 
};

#endif

