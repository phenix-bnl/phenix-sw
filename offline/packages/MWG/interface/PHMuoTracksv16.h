#ifndef __PHMuoTracksv16_H
#define __PHMuoTracksv16_H
#include <iostream>
#include "PHObject.h"
#include "TClonesArray.h"
#include "PHMuoTracksOut.h"
#include "PHMuoTrackv16.h"
#include "PHdiMuoTrackv5_Container.h"

/*! Event muon tracks container

This class contains all the muon tracks stored for each event. Single track variable
descriptions are given in section 4.2.
*/
class PHMuoTracksv16 : public PHMuoTracksOut
{

  public:
  
  //! constructor
  PHMuoTracksv16();
  
  //! constructor
  PHMuoTracksv16(TClonesArray* MuT);
  
  //! destructor
  virtual ~PHMuoTracksv16();
  
  //! reset container
  virtual void Reset()
  {
    nMuoTracks = 0; 
    Clear();
  }
  
  //! identify object
  virtual void identify(std::ostream &os=std::cout) const
  {
      os << "identify yourself:  PHMuoTracksv16 object" << std::endl;
      return;
  }
  
  //! clear container
  virtual void Clear(Option_t *option = "")
  {
    if( MuoTracks ) MuoTracks->Delete();
  }
  
  //! object validity
  int IsValid() const {return 1;}

  //====================================== Particle Fillers
  int set_TClonesArraySize(const unsigned int npart){
      MuoTracks->Expand(npart);
      return npart;
  }
  
  void AddPHParticle(const unsigned int itrk)
  {
      new ((*MuoTracks)[itrk]) PHMuoTrackv16();
  }
  
  void RemovePHParticle(const unsigned int itrk)
  {
      MuoTracks->RemoveAt(itrk);
      return;
  }

  //================================= PHParticle cloning/copying etc...
  TObject* GetSingleParticle(unsigned int ipart);
  void AddPHParticle(unsigned int ipart, TObject *);
  virtual PHMuoTracksv16* clone() const;

  //=================================== Particle Accessors
  //!@name Access methods
  //@{
  unsigned int get_npart() const {return nMuoTracks;}

  //! unique ID
  ULong_t get_uid( const unsigned int itrk ) const
  { 
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    return (trk) ? trk->get_uid() : 0;
  }
  
  float get_px(const unsigned int itrk) const
  {return get_px(0,(const unsigned int)itrk);}

  float get_py(const unsigned int itrk) const 
  {return get_py(0,(const unsigned int)itrk);} 

  float get_pz(const unsigned int itrk) const 
  {return get_pz(0,(const unsigned int)itrk);}

  /// Return {\bf itrk}$^{th}$ track {\bf charge}.
  short get_charge(const unsigned int itrk) const;

  /// Return {\bf itrk}$^{th}$ track {\bf px[arrayid]}.
  float get_px(short arrayid, const unsigned int itrk) const;

  /// Return {\bf itrk}$^{th}$ track {\bf py[arrayid]}.
  float get_py(short arrayid, const unsigned int itrk) const;

  /// Return {\bf itrk}$^{th}$ track {\bf pz[arrayid]}.
  float get_pz(short arrayid, const unsigned int itrk) const;

  /// Return {\bf itrk}$^{th}$ track {\bf st1\_bp\_P[arrayid]}.
  float get_st1_bp_P(short arrayid, const unsigned int itrk) const;

  /// Return {\bf itrk}$^{th}$ track {\bf st1\_bp\_pos[arrayid]}.
  float get_st1_bp_pos(short arrayid, const unsigned int itrk) const;

  /// Return {\bf itrk}$^{th}$ track {\bf xpos[arrayid]}.
  float get_xpos(short arrayid, const unsigned int itrk) const;

  /// Return {\bf itrk}$^{th}$ track {\bf ypos[arrayid]}.
  float get_ypos(short arrayid, const unsigned int itrk) const;

  /// Return {\bf itrk}$^{th}$ track {\bf zpos[arrayid]}.
  float get_zpos(short arrayid, const unsigned int itrk) const;

  /// Return {\bf itrk}$^{th}$ track {\bf nhits}.
  short get_nhits(const unsigned int itrk) const;  

  /// Return {\bf itrk}$^{th}$ track {\bf cov[arrayid1][arrayid2]}.
  float get_cov(short arrayid1, short arrayid2, const unsigned int itrk) const;
  
  /// Return {\bf itrk}$^{th}$ track {\bf number of degrees of freedom}.
  int get_ndf( const unsigned int itrk ) const ;
  
  /// Return {\bf itrk}$^{th}$ track {\bf reduced chisquare}.
  float get_chisquare(const unsigned int itrk) const ;
  
  /// Return {\bf itrk}$^{th}$ track {\bf ghostflag}.
  float get_ghostflag(const unsigned int itrk) const;

  /// Return {\bf itrk}$^{th}$ track {\bf hits}.
  int get_muTRhits(const unsigned int itrk) const;

  /// Return {\bf itrk}$^{th}$ road {\bf chi_square}.
  float get_muIDOOchi(const short iroad, const unsigned int itrk ) const;

  /// Return {\bf itrk}$^{th}$ road {\bf hits}.
  int get_muIDOOhits(const short iroad, const unsigned int itrk ) const;

  /// Return road position and direction at gap0 (x,y,z,dxdz,dydz)
  float get_muIDOO_gap0(const short arrayid, const short iroad, const unsigned int itrk) const;

  /// Return the (x,y) of the hits associated with the deepest road
  float get_muid_hit_x(const short gap, const unsigned int itrk) const;
  
  float get_muid_hit_y(const short gap, const unsigned int itrk) const;

  /// Return MutOO track status
  int get_TMutTrk_status(const unsigned int itrk) const;
  
  /// return gap_coordinate charge difference
  float get_delta_q( const unsigned int coord_id, const unsigned int itrk ) const;
  
  /// return error on gap_coordinate charge difference
  float get_delta_q_error( const unsigned int coord_id, const unsigned int itrk ) const;

  //! return bit pattern for size==1 clusters
  unsigned short get_clusters_size1(short itrk) const;

  //! return 3-vector FVTX vertex position of the track 
  float get_fvtx_vtx(const unsigned int itrk, const size_t coord) const;

  //! return 3-vector FVTX momentum assuming total momentum from MuTr
  float get_fvtx_p(const unsigned int itrk, const size_t coord) const;

  //! return 3-vector FVTX+MuTr refit vertex position of the track 
  float get_fvtxmutr_vtx(const unsigned int itrk, const size_t coord) const;

  //! return 3-vector FVTX+MuTr refit momentum assuming total momentum from MuTr
  float get_fvtxmutr_p(const unsigned int itrk, const size_t coord) const;

  //! return chi2/NDF FVTX+MuTr refit momentum assuming total momentum from MuTr
  float get_fvtxmutr_chi2(const unsigned int itrk) const;

  //! return bit pattern with cluster size in each FVTX plane (assuming 8 planes and cluster size up to 15)
  int get_fvtx_cluster_size_word(const unsigned int itrk) const;

  //! return cluster size for each FVTX plane [0,7]
  //! plane = station*2;
  //! if (!(sector%2) && arm==MUTOO::South) plane++;
  //! if ((sector%2) && arm==MUTOO::North) plane++;
  size_t get_fvtx_cluster_size(const unsigned int itrk, const size_t plane) const;

  //! return cluster charge in each FVTX station
  float get_fvtx_cluster_charge(const unsigned int itrk, const size_t station) const;

  //! return phi residual btw. MuTr and FVTX track in sigmas considering fvtx track window
  float get_fvtx_dphi(const unsigned int itrk) const;

  //! return theta residual btw. MuTr and FVTX track
  float get_fvtx_dtheta(const unsigned int itrk) const;

  //! return radius residual btw. MuTr and FVTX track at Z=+/-40 cm
  float get_fvtx_dr(const unsigned int itrk) const;

  //! return chi^2 for MuTr+FVTX track
  float get_fvtx_chi2(const unsigned int itrk) const;

  //! return offset from peak strip (cm)
  float get_fvtx_w(const unsigned int itrk, const size_t station) const;

  //! return index of best matching fvtx strip
  unsigned long get_fvtx_global_strip(const unsigned int itrk, const size_t station) const;

  //! return index of best matching fvtx strip
  short unsigned int get_fvtx_strip(const unsigned int itrk, const size_t station) const;

  //! return column of best matching fvtx strip
  unsigned int get_fvtx_column(const unsigned int itrk, const size_t station) const;

  //! return sector of best matching fvtx strip
  unsigned int get_fvtx_sector(const unsigned int itrk, const size_t station) const;

  //! return cage of best matching fvtx strip
  unsigned int get_fvtx_cage(const unsigned int itrk, const size_t station) const;

  //! return arm of fvtx track
  unsigned int get_fvtx_arm(const unsigned int itrk) const;

  //! return covariant matrix of FVTX track
  float get_fvtx_cov(const unsigned int itrk, const size_t i, const size_t j) const;

  //! return covariant matrix of FVTX+MuTr refit track
  float get_fvtxmutr_cov(const unsigned int itrk, const size_t i, const size_t j) const;

  //! return the bit word with the number of FVTX tracklets around dR cone bins
  unsigned long  get_fvtx_tracklets_cone(const unsigned int itrk) const;

  //! return the number of FVTX clusters around the track in a dR bin cone
  size_t get_nfvtx_tracklets_conerange(const unsigned int itrk, size_t dtheta_bin) const;

  //! return the bit word with the number of FVTX clusters around dR cone bins
  unsigned long  get_fvtx_clusters_cone(const unsigned int itrk) const;

  //! return the number of FVTX clusters around the track in a dr bin cone
  size_t get_nfvtx_clusters_conerange(const unsigned int itrk, size_t dtheta_bin) const;

  //!overall number of cluster, which point to vertex of this track
  unsigned int get_nfvtx_tracklets(const unsigned int itrk) const;

  //! return VTX+FVTX hit pattern
  //! NA NA NA NA VTX4 VTX3 VTX2 VTX1 FVTX4_1 FVTX_4_0 FVTX3_1 FVTX3_0 FVTX2_1 FVTX2_0 FVTX1_1 FVTX1_0
  unsigned short get_fvtx_hits(const unsigned int itrk) const;

  /*
    //! return 3-vector swapped FVTX vertex position of the track 
  float get_sfvtx_vtx(const unsigned int itrk, const size_t coord) const;

  //! return 3-vector swapped FVTX momentum assuming total momentum from MuTr
  float get_sfvtx_p(const unsigned int itrk, const size_t coord) const;

  //! return 3-vector swapped FVTX+MuTr refit vertex position of the track 
  float get_sfvtxmutr_vtx(const unsigned int itrk, const size_t coord) const;

  //! return 3-vector swapped FVTX+MuTr refit momentum assuming total momentum from MuTr
  float get_sfvtxmutr_p(const unsigned int itrk, const size_t coord) const;

  //! return chi2/NDF swapped FVTX+MuTr refit momentum assuming total momentum from MuTr
  float get_sfvtxmutr_chi2(const unsigned int itrk) const;

  //! return phi residual btw. MuTr and swapped FVTX track in sigmas considering fvtx track window
  float get_sfvtx_dphi(const unsigned int itrk) const;

  //! return theta residual btw. MuTr and swapped FVTX track
  float get_sfvtx_dtheta(const unsigned int itrk) const;

  //! return radius residual btw. MuTr and swapped FVTX track at Z=+/-40 cm
  float get_sfvtx_dr(const unsigned int itrk) const;

  //! return chi^2 for MuTr+ swapped FVTX track
  float get_sfvtx_chi2(const unsigned int itrk) const;

  //! return covariant matrix of swapped FVTX track
  float get_sfvtx_cov(const unsigned int itrk, const size_t i, const size_t j) const;

  //! return covariant matrix of swapped FVTX+MuTr refit track
  float get_sfvtxmutr_cov(const unsigned int itrk, const size_t i, const size_t j) const;
  */
  //@}

  //!@name Particle Mutators
  //@{
  
  void set_npart(const unsigned int npart) 
  {nMuoTracks=npart;}
  
  //! unique ID
  void set_uid( const unsigned int itrk, const ULong_t value )
  {
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    if(trk) trk->set_uid(value); 
  }
  
  // These are the sets appropriate for the PHParticle base class.
  void set_px(const unsigned int itrk, const float newVal)
  {set_px(0,(const unsigned short)itrk,newVal);}

  void set_py(const unsigned int itrk, const float newVal)
  {set_py(0,(const unsigned short)itrk,newVal);}

  void set_pz(const unsigned int itrk, const float newVal)
  {set_pz(0,(const unsigned short)itrk,newVal);}

  void set_nhits(const unsigned int itrk, const short newVal);
  
  void set_charge(const unsigned int itrk, const short newVal);

  void set_px(short arrayid, const unsigned int itrk, float newVal);
  
  void set_py(short arrayid, const unsigned int itrk, float newVal);
  
  void set_pz(short arrayid, const unsigned int itrk, float newVal);
  
  void set_st1_bp_P(short arrayid, const unsigned int itrk, float newVal);
  
  void set_st1_bp_pos(short arrayid, const unsigned int itrk, float newVal);
  
  void set_xpos(short arrayid, const unsigned int itrk, float newVal);
  
  void set_ypos(short arrayid, const unsigned int itrk, float newVal);
  
  void set_zpos(short arrayid, const unsigned int itrk, float newVal);
  
  void set_cov(short arrayid1, short arrayid2, const unsigned int itrk, float newVal);
  
  //! changes track number of degrees of freedom
  void set_ndf(const unsigned int itrk, int newVal) ;
  
  //! changes track reduced chisquare
  void set_chisquare(const unsigned int itrk, float newVal) ;
  
  void set_ghostflag(const unsigned int itrk, float newVal);
  
  void set_muTRhits(const unsigned int itrk, int newVal);
  
  void set_muIDOOchi(const short iroad, const unsigned int itrk, float newVal);

  void set_muIDOOhits(const short iroad, const unsigned int itrk, int newVal);

  void set_muIDOO_gap0(const short arrayid, const short iroad, const unsigned int itrk, float newVal);

  
  void set_muid_hit_x(const short gap, const unsigned int itrk, const float newVal);

  void set_muid_hit_y(const short gap, const unsigned int itrk, const float newVal);

  void set_TMutTrk_status(const unsigned int itrk, int newVal);
  
  //! set gap_coordinate charge difference
  void set_delta_q( const unsigned int coord_id, const unsigned int itrk, float value );
  
  //! set error on gap_coordinate charge difference
  void set_delta_q_error( const unsigned int coord_id, const unsigned int itrk, float value );

  //! set the number of clusters with size==1
  void set_clusters_size1(const unsigned int itrk, const unsigned short n);

  //! set 3-vector FVTX vertex position of the track
  void set_fvtx_vtx(const unsigned int itrk, const size_t coord, const float x);

  //! set 3-vector FVTX momentum
  void set_fvtx_p(const unsigned int itrk, const size_t coord, const float p);

  //! set 3-vector FVTX+MuTr refit vertex position of the track
  void set_fvtxmutr_vtx(const unsigned int itrk, const size_t coord, const float x);

  //! set 3-vector FVTX+MuTr momentum
  void set_fvtxmutr_p(const unsigned int itrk, const size_t coord, const float p);

  //! set chi2/NDF FVTX+MuTr momentum
  void set_fvtxmutr_chi2(const unsigned int itrk, const float a);

  //! set bit pattern with cluster size in each FVTX plane
  void set_fvtx_cluster_size(const unsigned int itrk, const int word);

  //! set bit pattern with cluster size in each FVTX plane (assuming 8 planes and cluster size up to 15)
  void set_fvtx_cluster_size(const unsigned int itrk, const size_t plane, const size_t cluster_size);

  //! set cluster charge in each FVTX station
  void set_fvtx_cluster_charge(const unsigned int itrk, const size_t station, const float charge);

  //! set phi residual btw. MuTr and FVTX track
  void set_fvtx_dphi(const unsigned int itrk, const float dphi);

  //! set theta residual btw. MuTr and FVTX track
  void set_fvtx_dtheta(const unsigned int itrk, const float dtheta);

  //! set radius residual btw. MuTr and FVTX track at Z=+/-40 cm
  void set_fvtx_dr(const unsigned int itrk, const float dr);

  //! set chi^2 for MuTr+FVTX track
  void set_fvtx_chi2(const unsigned int itrk, const float chi2);

  //! set offset from peak strip in each FVTX station
  void set_fvtx_w(const unsigned int itrk, const size_t station, const float proj);

  //! set global index of best matching fvtx strip
  void set_fvtx_strip(const unsigned int itrk,
		      const size_t station, 
		      const bool arm,
		      const bool cage,
		      const unsigned short sector,
		      const bool column,
		      const unsigned short strip);

  //! set covariant matrix of FVTX track
  void set_fvtx_cov(const unsigned int itrk, const size_t i, const size_t j, const float cov);

  //! set covariant matrix of FVTX+MuTr track
  void set_fvtxmutr_cov(const unsigned int itrk, const size_t i, const size_t j, const float cov);

  //! set number of FVTX tracklets in a dthetabin around the track
  void set_nfvtx_tracklets_conerange(const unsigned int itrk, const size_t dthetabin, const size_t ntracklets);

  //! set number of FVTX clusters in a dr-bin around the track
  void set_nfvtx_clusters_conerange(const unsigned int itrk, const size_t dthetabin, const size_t nclusters);

  //!overall number of cluster, which point to vertex of this track
  void set_nfvtx_tracklets(const unsigned int itrk, const unsigned int a);

  //! set VTX+FVTX hit pattern
  //! NA NA NA NA VTX4 VTX3 VTX2 VTX1 FVTX4_1 FVTX_4_0 FVTX3_1 FVTX3_0 FVTX2_1 FVTX2_0 FVTX1_1 FVTX1_0
  void set_fvtx_hits(const unsigned int itrk, const unsigned short a);

  /*
   //! set 3-vector swapped FVTX vertex position of the track
  void set_sfvtx_vtx(const unsigned int itrk, const size_t coord, const float x);

  //! set 3-vector swapped FVTX momentum
  void set_sfvtx_p(const unsigned int itrk, const size_t coord, const float p);

  //! set 3-vector swapped FVTX+MuTr refit vertex position of the track
  void set_sfvtxmutr_vtx(const unsigned int itrk, const size_t coord, const float x);

  //! set 3-vector swapped FVTX+MuTr momentum
  void set_sfvtxmutr_p(const unsigned int itrk, const size_t coord, const float p);

  //! set chi2/NDF swapped FVTX+MuTr momentum
  void set_sfvtxmutr_chi2(const unsigned int itrk, const float a);

  //! set phi residual btw. MuTr and swapped FVTX track
  void set_sfvtx_dphi(const unsigned int itrk, const float dphi);

  //! set theta residual btw. MuTr and swapped FVTX track
  void set_sfvtx_dtheta(const unsigned int itrk, const float dtheta);

  //! set radius residual btw. MuTr and swapped FVTX track at Z=+/-40 cm
  void set_sfvtx_dr(const unsigned int itrk, const float dr);

  //! set chi^2 for MuTr+ swapped FVTX track
  void set_sfvtx_chi2(const unsigned int itrk, const float chi2);

  //! set covariant matrix of swapped FVTX track
  void set_sfvtx_cov(const unsigned int itrk, const size_t i, const size_t j, const float cov);

  //! set covariant matrix of swapped FVTX+MuTr track
  void set_sfvtxmutr_cov(const unsigned int itrk, const size_t i, const size_t j, const float cov);
  */
  //@}
  
  /*
  Following accessors and modifiers are for hidden (that is: non-persistent)
  varibles. Variables are set, and used, at run-time. They are not stored into
  the output MWG root file
  */
  
  //!@name association to level2 muid primitives
  //@{
 
  //! number of associated primitives
  virtual unsigned int get_n_primitives( const unsigned int itrk ) const
  { 
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    return (trk) ? trk->get_n_primitives():0; 
  }
  
  //! set number of primitives
  virtual void set_n_primitives( const unsigned int itrk, const unsigned int& value )
  {
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    if( trk ) trk->set_n_primitives( value );
  }
  
  //! associated primitive level2 angle
  virtual double get_level2_phi( const unsigned int itrk, const unsigned int& i ) const
  { 
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    return (trk) ? trk->get_level2_phi(i):0; 
  }

  //! associated primitive level2 angle
  virtual void set_level2_phi( const unsigned int itrk, const unsigned int&i, const double& value )
  { 
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    if( trk ) trk->set_level2_phi( i, value );
  }  
  
  //! associated primitive level2 angle
  virtual double get_level2_theta( const unsigned int itrk, const unsigned int& i ) const
  { 
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    return (trk) ? trk->get_level2_theta(i):0; 
  }

  //! associated primitive level2 angle
  virtual void set_level2_theta( const unsigned int itrk, const unsigned int& i, const double& value )
  { 
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    if( trk ) trk->set_level2_theta( i, value );
  }
  
  //@}
  
  //!@name association to level2 mutr primitives
  //@{
  
  //! number of associated primitives
  virtual unsigned int get_n_mutr_primitives( const unsigned int itrk ) const
  { 
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    return (trk) ? trk->get_n_mutr_primitives():0; 
  }
  
  //! set number of primitives
  virtual void set_n_mutr_primitives( const unsigned int itrk, const unsigned int& value )
  {
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    if( trk ) trk->set_n_mutr_primitives( value );
  }

  //! associated level2 primitive min momentum
  virtual void set_level2_pmin_x( const unsigned int itrk, const unsigned int& i, const double& value )
  {
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    if( trk ) trk->set_level2_pmin_x( i, value );
  }  

  //! associated level2 primitive min momentum
  virtual void set_level2_pmin_y( const unsigned int itrk, const unsigned int& i, const double& value )
  {
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    if( trk ) trk->set_level2_pmin_y( i, value );
  }  

  //! associated level2 primitive min momentum
  virtual void set_level2_pmin_z( const unsigned int itrk, const unsigned int& i, const double& value )
  {
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    if( trk ) trk->set_level2_pmin_z( i, value );
  }  

  //! associated level2 primitive min momentum
  virtual double get_level2_pmin_x( const unsigned int itrk, const unsigned int& i ) const
  {
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    return trk ? trk->get_level2_pmin_x( i ):0;
  }  

  //! associated level2 primitive min momentum
  virtual double get_level2_pmin_y( const unsigned int itrk, const unsigned int& i ) const
  {
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    return trk ? trk->get_level2_pmin_y( i ):0;
  }  

    //! associated level2 primitive min momentum
  virtual double get_level2_pmin_z( const unsigned int itrk, const unsigned int& i ) const
  {
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    return trk ? trk->get_level2_pmin_z( i ):0;
  }  
  
  //! associated level2 primitive max momentum
  virtual void set_level2_pmax_x( const unsigned int itrk, const unsigned int& i, const double& value )
  {
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    if( trk ) trk->set_level2_pmax_x( i, value );
  }  

  //! associated level2 primitive max momentum
  virtual void set_level2_pmax_y( const unsigned int itrk, const unsigned int& i, const double& value )
  {
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    if( trk ) trk->set_level2_pmax_y( i, value );
  }  

  //! associated level2 primitive max momentum
  virtual void set_level2_pmax_z( const unsigned int itrk, const unsigned int& i, const double& value )
  {
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    if( trk ) trk->set_level2_pmax_z( i, value );
  }  

  //! associated level2 primitive max momentum
  virtual double get_level2_pmax_x( const unsigned int itrk, const unsigned int& i ) const
  {
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    return trk ? trk->get_level2_pmax_x( i ):0;
  }  

  //! associated level2 primitive max momentum
  virtual double get_level2_pmax_y( const unsigned int itrk, const unsigned int& i ) const
  {
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    return trk ? trk->get_level2_pmax_z( i ):0;
  }  

    //! associated level2 primitive max momentum
  virtual double get_level2_pmax_z( const unsigned int itrk, const unsigned int& i ) const
  {
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    return trk ? trk->get_level2_pmax_z( i ):0;
  }  
  
  //@}
  
  //!@name event vertex information
  //@{
 
  //! event vertex z
  /*! 
  it needs to be stored on a track by track basis in order to be able
  to fit together tracks that do not belong to the same event, when 
  performing event mixing
  */
  virtual double get_event_vertex_z( const unsigned int itrk ) const
  { 
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    return (trk) ? trk->get_event_vertex_z():0; 
  }
  
  //! event vertex z error
  /*! 
  it needs to be stored on a track by track basis in order to be able
  to fit together tracks that do not belong to the same event, when 
  performing event mixing
  */
  virtual double get_event_vertex_z_error( const unsigned int itrk ) const
  { 
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    return (trk) ? trk->get_event_vertex_z_error():0; 
  }
  
  //! event vertex z
  virtual void set_event_vertex_z( const unsigned int itrk, const double& value )
  { 
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    if( trk ) trk->set_event_vertex_z( value );
  }

  //! event vertex z error
  virtual void set_event_vertex_z_error( const unsigned int itrk, const double& value )
  { 
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    if( trk ) trk->set_event_vertex_z_error( value );
  }
  
  //@}  

  //!@name event reaction plane information
  //@{
  
  //! event reaction plane
  /*! 
  it needs to be stored on a track by track basis in order to be able
  to calculate the "average reaction plane angle for muons that do not belong to the same event
  */
  virtual double get_event_rp_angle( const unsigned int itrk, const unsigned int& arm ) const
  { 
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    return trk ? trk->get_event_rp_angle( arm ):0;
  }
 
  //! event reaction plane
  /*! 
  it needs to be stored on a track by track basis in order to be able
  to calculate the "average reaction plane angle for muons that do not belong to the same event
  */
  virtual void set_event_rp_angle( const unsigned int itrk, const unsigned int& arm, const double& value )
  { 
    PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
    if( trk ) trk->set_event_rp_angle( arm, value );
  }

  //@}
  
protected:

  //!@name Variables
  //@{
  /// Number of Muon Tracks in the event.
  int nMuoTracks;
  /// Muon Tracks Container.
  TClonesArray* MuoTracks;
  //@}

ClassDef(PHMuoTracksv16,1)
};



#endif

