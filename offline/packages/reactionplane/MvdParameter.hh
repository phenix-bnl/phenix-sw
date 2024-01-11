#ifndef MVDPARAMETER
#define MVDPARAMETER

#include <iostream>

/** composite of STAF tables dMvbPar, dMvcPar, dMvdDbasePar, dMvdPseudoPar
  * dMvdTrigPar, dMvddNdEtaPar, dMvdPar and some other parameters
  */
class MvdParameter  {
 public :
  ///default constructor
  MvdParameter() {}
  ///default destructor
  ~MvdParameter() {}
  ///initialize data members using hardwired values
  void Init();
  friend std::ostream& operator<<(std::ostream&, const MvdParameter&);
  
   public :
//dMvbPar.idl   

      short nzdim;
      short nzbig;
      short nchdim;
      short nstrip;
      short nphuse;
      short nphdim;
      short nshell;
      short nsegment;
      short nhole;

//dMvcpar.idl
      short nvpad_segs;
      short nvpad_segs2;
      short nvpad_phi;
      short nvpad_rbins;
      short nvpad_rbins1;
      short nend;
      float vpad_phi1;
      float vpad_delphi;
      float vpad_delphi2;

//dMvdDbasePar.idl
      short mvd_type;
      short barrel_type;
      short endcap_type;

//I do not know what this is 
      short packet_id;

      float barrel_gain;
      float endcap_gain;

//dMvdPseudoPar.idl
      short iphlim;
      short p_iexpnd;
      short z_iexpnd;

//dMvdTrigPar.idl
      short choice;
      short ilayer;
      short nhitmin;
      float thresh_kev;
      float occ_cut;

//dMvddNdEtaPar.idl
      /** AVeraGe signal of a Minimum Ionizing Particle
        * at normal incidence angle in chan. This should be
        * about 32chan, but it might be necessary to adjust
        * this parameter upwards a little since the average
        * energy loss should be greater than the average
        * for a mip. The value is calculated from the scale
        * parameters given in dMvdPar table. 
        */
      float avgmip;   
      /** Used to control the number of channels which
        * are grouped together in the dN/dEta calculation 
        */
      short nbunch;   
      /** parameter which prevents the calculation of
        * dN/deta from a pad plane when the calculated vertex
        * is too close to the pad plane.
        */
      float tooclose;

//dMvdPar.idl
      /** Lots of the MVD code uses ZOFF, the half-length of
        * a ladder -- for convenience, this variable is included
        * here, but it is actually intended to be the same as 
        * dvisl[3] -- the initialization should be in ver_ini.
        */
      float zoff;      
      /** the (approximate) number of electron-hole pairs
        * produced in Si for one mip passing through 300 microns.
        */
      float xnum_mip;
      /** the (approximate) energy deposition in keV for one
        * mip passing through 300 microns of Si. 
        */
      float skev_mip; 
      /** the average noise level in units of keV deposited
        * by a mip passing through 300 microns of Si
        * Expressed as a fraction of skev_mip.
        */
      float skev_noise;
      /** the "fudge-factor" by which the sigma of the noise
        * distribution is increased above a simple estimate 
        * based of the square-root of the number of electron-hole
        * pairs. 
        */
      float factor_noise;
      /** the full scale signal, when digitized, in units
        * of the signal deposited by a mip in 300 microns of Si.
        * For example, smax_mip=8. means that 8-mips = the full
        * scale signal when digitized.
        */
      float smax_mip;
      /** The equivalent of smax_mip, but expressed in keV
        * posited in Si.     
        */
      float smax_kev; 
      /** The number of channels full-scale used for the
        *digitized signal.
        */
      float full_scale;
      /// an integer version of full_scale  
      float ver_thrsh_mip; 
      ///the threshold in mip's
      float ver_thrsh;
      ///the value of cross talk between channels     
      float cross_talk; 
      ///the threshold in KeV
      short maxadc_ver;   
      
//dMvdVertexOut.idl?   
      float software_pversion;
      float software_zversion;

      float soft_id_whole;
      float soft_id_row;

      short maxiter; 
      float soft_id_avgdedx;
      float soft_id_deconv;

      float eta_min_hist;
      float eta_max_hist;
      short n_eta_bins_in_hist;

//new entry
   float strip_sigma_cut;
   float pad_sigma_cut;
   float clump_cut;

};

#endif


