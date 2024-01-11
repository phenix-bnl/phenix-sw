#ifndef __PHMuoTrackv15_H
#define __PHMuoTrackv15_H

#include "PHMuoTrackPrivate.h"

#include <PHObject.h>
#include <phool.h>

#include <cassert>
#include <stdint.h>

/** Single Muon Track Container

This class contains information about each track found in muon arms. This 
information is stored in the $MuoTracks$ container located in the $PHMuoTracks$
within the output rootuple. 
*/
class PHMuoTrackv15 : public TObject
{
 public:

  enum
  {
    
    /*! 
      number of momumta and positions measurement points 
      are: vertex, station1, station2, station3 and muid gap0
    */
    _maxpoints=5, 
  
    //! Kalman covariance matrix dimension (currently 5 $\times$ 5).
    _covdim=5,
  
    //! number of muid roads associated with track.
    _maxroads=3,
  
    //! number of gap coordinates associated to the track
    _max_gap_coords=8,
    
    //! max number of muid hits in projection
    _max_mui_hits = 10
    
  };
                
  //! constructor
  PHMuoTrackv15();
  
  //! destructor
  ~PHMuoTrackv15(){};

  void identify(std::ostream& os = std::cout) const;

  // Accessors
  /**@name Access methods

  */
  //@{
  
  //! unique ID
  ULong_t get_uid( void ) const
  { return uid; }
  
  //! Return {\bf px[arrayid]}.
  float get_px(short arrayid) const;
  
  //! Return {\bf py[arrayid]}.
  float get_py(short arrayid) const;
  
  //! Return {\bf pz[arrayid]}.
  float get_pz(short arrayid) const;
  
  //! Return {\bf st1_bp_P[arrayid]}.
  float get_st1_bp_P(short arrayid) const;
  
  //! Return {\bf st1_bp_pos[arrayid]}.
  float get_st1_bp_pos(short arrayid) const;
  
  //! Return {\bf xpos[arrayid]}.
  float get_xpos(short arrayid) const;
  
  //! Return {\bf ypos[arrayid]}.
  float get_ypos(short arrayid) const;
  
  //! Return {\bf zpos[arrayid]}.
  float get_zpos(short arrayid) const;
  
  //! Return {\bf nhits}.
  short get_nhits() const {return nhits;}  
  
  //! Return {\bf cov[arrayid1][arrayid2]}.
  float get_cov(short arrayid1, short arrayid2) const ;

  //! Return {\bf charge}.
  int get_charge() const 
  {return charge;}

  //! Return {\bf number of degrees of freedom}.
  int get_ndf() const
  {return ndf;}

  //! Return {\bf chisquare}.
  float get_chisquare() const 
  {return chisquare;}

  //! Return {\bf ghostflag}.
  float get_ghostflag() const 
  {return 0;}

  //! Return {\bf muTRhits}.
  int get_muTRhits() const 
  {return muTRhits;}

  //! Return road position and direction at gap0 (x,y,z,dxdz,dydz)
  float get_muIDOO_gap0(const short arrayid, const short iroad) const;

  //! Return chi square for each muid road.
  float get_muIDOOchi(const short iroad) const 
  {return muIDOOchi[iroad];}

  //! Return {\bf muIDhits}. 
  int get_muIDOOhits(const short iroad) const 
  {return muIDOOhits[iroad];}

  // return the x,y of the associated hits in the deepest road
  float get_muid_hit_x(short gap) const;
  
  float get_muid_hit_y(short gap) const;

  //! Return MutOO track status
  int get_TMutTrk_status() const {return TMutTrk_status;}

  //! return charge difference for a given coordinate
  float get_delta_q( unsigned int coord_id ) const
  { 
    if( coord_id >= _max_gap_coords ) 
    {
      std::cout<<PHWHERE<<" PHMuoTrackv15::get_delta_q id out of range.\n";
      return 0;
    }    
    return _delta_q[ coord_id ]; 
  }
  
  //! return charge different for a given coordinate
  float get_delta_q_error( unsigned int coord_id ) const
  { 
    if( coord_id >= _max_gap_coords ) 
    {
      std::cout<<PHWHERE<<" PHMuoTrackv15::get_delta_q_error id out of range.\n";
      return 0;
    }    
    return _delta_q_error[ coord_id ]; 
  }

  //! get bit pattern with size==1 clusters
  unsigned short get_clusters_size1() const
  {
    return clusters_size1;
  }
  
  //! return 3-vector FVTX vertex position
  float get_fvtx_vtx(size_t coord) const
  {
    if (coord >= 3)
      {
      std::cout<<PHWHERE<<" PHMuoTrackv15::get_fvtx_x coordinate out of range \n";
      return 0;
      }
    return fvtx_vtx[coord];
  }

  //! return 3-vector FVTX momentum
  float get_fvtx_p(size_t coord) const
  {
    if (coord >= 3)
      {
      std::cout<<PHWHERE<<" PHMuoTrackv15::get_fvtx_p coordinate out of range \n";
      return 0;
      }
    return fvtx_p[coord];
  }

  //! return 3-vector FVTX+MuTr refit vertex position
  float get_fvtxmutr_vtx(size_t coord) const
  {
    if (coord >= 3)
      {
      std::cout<<PHWHERE<<" PHMuoTrackv15::get_fvtxmutr_x coordinate out of range \n";
      return 0;
      }
    return fvtxmutr_vtx[coord];
  }

  //! return 3-vector FVTX+MuTr refit momentum
  float get_fvtxmutr_p(size_t coord) const
  {
    if (coord >= 3)
      {
      std::cout<<PHWHERE<<" PHMuoTrackv15::get_fvtxmutr_p coordinate out of range \n";
      return 0;
      }
    return fvtxmutr_p[coord];
  }

  //! return chi2/NDF for FVTX+MuTr refit momentum
  float get_fvtxmutr_chi2() const
  {
    return fvtxmutr_chi2;;
  }

  //! return bit pattern with cluster size in each FVTX plane (assuming 8 planes and cluster size up to 15)
  int get_fvtx_cluster_size_word() const
  {
    return fvtx_cluster_size;
  }

  //! return cluster size for each FVTX plane [0,7]
  //! plane = station*2;
  //! if (!(sector%2) && arm==MUTOO::South) plane++;
  //! if ((sector%2) && arm==MUTOO::North) plane++;
  size_t get_fvtx_cluster_size(size_t plane) const
  {
    if (plane>7)
      {
	std::cout<<PHWHERE<<" PHMuoTrackv15::get_fvtx_cluster_size plane should be in [0,8] \n";
	return 0;
      }
    return ((fvtx_cluster_size >> (plane*4)) & 0xf);
  }

  //! return the cluster charge for each FVTX station
  float get_fvtx_cluster_charge(size_t station) const
  {
    if (station>3)
      {
	std::cout<<PHWHERE<<"PHMuoTrackv15::get_fvtx_cluster_charge station should be in [0,3] \n";
	return -999.;
      }
    return fvtx_cluster_charge[station];
  }

  //! return phi residual btw. MuTr and FVTX track in sigmas considering fvtx track window
  float get_fvtx_dphi() const
  {
    return fvtx_dphi;
  }

  //! return theta residual btw. MuTr and FVTX track
  float get_fvtx_dtheta() const
  {
    return fvtx_dtheta;
  }
  
  //! return radius residual btw. MuTr and FVTX track projection at Z=+/-40cm
  float get_fvtx_dr() const
  {
    return fvtx_dr;
  }

  //! return chi^2 for MuTr+FVTX track
  float get_fvtx_chi2() const
  {
    return fvtx_chi2;
  }

  //! return offset from peak strip (cm)
  float get_fvtx_w(size_t station) const
  {
    return fvtx_w[station];
  }

  //! return global index of best matching fvtx strip
  unsigned long get_fvtx_global_strip(size_t station) const
  {
    return fvtx_strip[station];
  }

  //! return index of best matching fvtx strip 
  unsigned short get_fvtx_strip(size_t station) const
  {
    return fvtx_strip[station] & 0x3ff;
  }

  //! return column of best matching fvtx strip
  unsigned int get_fvtx_column(const size_t station) const
  {
    return ((fvtx_strip[station] >> 10)&0x01);
  }

  //! return sector(wedge) of best matching fvtx strip
  unsigned int get_fvtx_sector(const size_t station) const
  {
    return ((fvtx_strip[station] >> 11)&0x3f);
  }

  //! return cage of best matching fvtx strip
  unsigned int get_fvtx_cage(const size_t station) const
  {
    return ((fvtx_strip[station] >> 19)&0x01);
  }

  //! return arm of track
  unsigned int get_fvtx_arm() const
  {
    return ((fvtx_strip[0] >> 20)&0x01);
  }

  //! return covariant matrix of FVTX track
  float get_fvtx_cov(size_t i, size_t j) const
  {
    return fvtx_cov[i][j];
  }

  //! return covariant matrix of FVTX+MuTr refit track
  float get_fvtxmutr_cov(size_t i, size_t j) const
  {
    return fvtxmutr_cov[i][j];
  }

  //! get word bit with number of tracklets around a binned cone size
  uint32_t get_fvtx_tracklets_cone() const
  {
    return fvtx_tracklets_cone;
  }

  //! get number of FVTX tracklets inside a dR bin cone around the track 
  //! dR = sqrt(dEta^2 + dPhi^2)
  size_t get_nfvtx_tracklets_conerange(size_t dthetabin) const;

  //! get word bit with number of tracklets around a binned cone size
  uint32_t get_fvtx_clusters_cone() const
  {
    return fvtx_clusters_cone;
  }

  //! get number of FVTX clusters inside a dR bin cone around the track
  //! dR = sqrt(dEta^2 + dPhi^2)
  size_t get_nfvtx_clusters_conerange(size_t dthetabin) const;


  //! get number of FVTX clusters inside a dR bin cone around the track
  //! dR = sqrt(dEta^2 + dPhi^2)
  size_t get_nfvtx_tracklets_conerange(size_t dthetabin_min, size_t dthetabin_max) const
  {
    size_t cnt = 0;
    for (size_t bin = dthetabin_min; bin<=dthetabin_max; bin++)
      cnt+= get_nfvtx_tracklets_conerange(bin);
    return cnt;
  }

  //! get number of FVTX clusters inside a dR bin cone around the track
  //! dR = sqrt(dEta^2 + dPhi^2)
  size_t get_nfvtx_clusters_conerange(size_t dthetabin_min, size_t dthetabin_max) const
  {
    size_t cnt = 0;
    for (size_t bin = dthetabin_min; bin<=dthetabin_max; bin++)
      cnt+= get_nfvtx_clusters_conerange(bin);
    return cnt;
  }


  //!overall number of cluster, which point to vertex of this track
  uint8_t get_nfvtx_tracklets() const
  {
    return nfvtx_tracklets;
  }
  //@}

  //!@name mutators
  
  //! unique ID
  void set_uid( ULong_t value )
  { uid = value; }
  
  void set_px(short arrayid, float newVal);
  
  void set_py(short arrayid, float newVal);
  
  void set_pz(short arrayid, float newVal);
  
  void set_st1_bp_P(short arrayid, float newVal);
  
  void set_st1_bp_pos(short arrayid, float newVal);
  
  void set_xpos(short arrayid, float newVal);
  
  void set_ypos(short arrayid, float newVal);
  
  void set_zpos(short arrayid, float newVal);
  
  void set_nhits(short newVal){ nhits = newVal;}
  
  void set_cov(short arrayid1, short arrayid2, float newVal);
  
  void set_charge(short newVal){ charge = newVal;}
  
  //! changes track number of degrees of freedom
  void set_ndf( int newVal )
  { ndf = newVal; }

  //! changes track reduced chi_square
  void set_chisquare(float newVal)
  { chisquare = newVal;}

  void set_ghostflag(float){}

  void set_muTRhits(int newVal){ muTRhits = newVal;}
  
  void set_muIDOOchi(const short iroad, float newVal){ muIDOOchi[iroad] = newVal;}

  void set_muIDOOhits(const short iroad, int newVal){ muIDOOhits[iroad] = newVal;}

  void set_muIDOO_gap0(const short arrayid, const short iroad, float newVal);

  void set_muIDhits(int) {}

  void set_muID_gap0(const short, float) {}

  void set_muid_hit_x(short gap, float newVal);
  void set_muid_hit_y(short gap, float newVal);

  void set_TMutTrk_status(int newVal) {TMutTrk_status=newVal;}

  //! set charge correlations for a given coordinate
  void set_delta_q( unsigned int coord_id, float value )
  { 
    if( coord_id >= _max_gap_coords ) 
    {
      std::cout<<PHWHERE<<" PHMuoTrackv15::set_delta_q id out of range.\n";
      return;
    }    
    _delta_q[coord_id] = value; 
  }

  //! set charge correlations for a given coordinate
  void set_delta_q_error( unsigned int coord_id, float value )
  { 
    if( coord_id >= _max_gap_coords ) 
    {
      std::cout<<PHWHERE<<" PHMuoTrackv15::set_delta_q_error id out of range.\n";
      return;
    }    
    _delta_q_error[coord_id] = value; 
  }

  //! number of clusters with just one hit on it
  void set_clusters_size1( unsigned short n)
  {
    clusters_size1 = n;
  }
  
  //! set 3-vector FVTX vertex position
  void set_fvtx_vtx(size_t coord, float x)
  {
    if (coord >= 3)
      {
      std::cout<<PHWHERE<<" PHMuoTrackv15::set_fvtx_x coordinate out of range \n";
      return;
      }
    fvtx_vtx[coord] = x;
  }

  //! set 3-vector FVTX momentum
  void set_fvtx_p(size_t coord, float p)
  {
    if (coord >= 3)
      {
      std::cout<<PHWHERE<<" PHMuoTrackv15::set_fvtx_p coordinate out of range \n";
      return;
      }
    fvtx_p[coord] = p;
  }

  //! set 3-vector FVTX+MuTr refit vertex position
  void set_fvtxmutr_vtx(size_t coord, float x)
  {
    if (coord >= 3)
      {
      std::cout<<PHWHERE<<" PHMuoTrackv15::set_fvtx_x coordinate out of range \n";
      return;
      }
    fvtxmutr_vtx[coord] = x;
  }

  //! set 3-vector FVTX+MuTr refit momentum
  void set_fvtxmutr_p(size_t coord, float p)
  {
    if (coord >= 3)
      {
      std::cout<<PHWHERE<<" PHMuoTrackv15::set_fvtx_p coordinate out of range \n";
      return;
      }
    fvtxmutr_p[coord] = p;
  }

  //! set chi2/NDF for FVTX+MuTr refit momentum
  void set_fvtxmutr_chi2(float a)
  {
    fvtxmutr_chi2 = a;
  }

  //! set bit pattern with cluster size in each FVTX plane
  void set_fvtx_cluster_size(int word)
  {
    fvtx_cluster_size = word;
  }

  //! set bit pattern with cluster size in each FVTX plane (assuming 8 planes and cluster size up to 15)
  void set_fvtx_cluster_size(size_t plane, size_t cluster_size)
  {
    if (plane>7)
      {
	std::cout<<PHWHERE<<" PHMuoTrackv15::set_fvtx_cluster_size plane should be in [0,8] \n";
	return;
      }
    if (cluster_size>15)
      {
	std::cout<<PHWHERE<<" PHMuoTrackv15::set_fvtx_cluster_size cluster size shoudl be <16 \n";
	return;
      }
    fvtx_cluster_size |= (cluster_size << (plane*4));
  }

  //! set the cluster charge in each station
  void set_fvtx_cluster_charge(size_t station, float charge)
  {
    if (station>3)
      {
	std::cout<<PHWHERE<<"PHMuoTrackv15::set_fvtx_cluster_charge station should be in [0,3] \n";
	return;
      }
    fvtx_cluster_charge[station] = charge;
  }
  
  //! set phi residual btw. MuTr and FVTX track
  void set_fvtx_dphi(float dphi)
  {
    fvtx_dphi = dphi;
  }

  //! set theta residual btw. MuTr and FVTX track
  void set_fvtx_dtheta(float dtheta)
  {
    fvtx_dtheta = dtheta;
  }
  
  //! set radius residual btw. MuTr and FVTX track projected at Z=+/-40cm
  void set_fvtx_dr(float dr)
  {
    fvtx_dr = dr;
  }

  //! set chi^2 for MuTr+FVTX track
  void set_fvtx_chi2(float chi2)
  {
    fvtx_chi2 = chi2;
  }

  //! set offset from peak strip in each FVTX station
  void set_fvtx_w(size_t station, float proj)
  {
    fvtx_w[station] = proj;
  }

  //! set index of best matching fvtx strip
  void set_fvtx_strip(size_t station,
		      bool arm,
		      bool cage,
		      unsigned short sector,
		      bool column,
		      unsigned short strip)
  {
    fvtx_strip[station] = (strip&0x3ff)
      | ((column&0x1) << 10)
      | ((sector&0x3f) << (1+10))
      | ((station&0x03) << (6+1+10))
      | ((cage&0x01) << (2+6+1+10))
      | ((arm&0x01) << (1+2+6+1+10));
  }

  //! set covariant matrix of FVTX track
  void set_fvtx_cov(size_t i, size_t j, float cov)
  {
    fvtx_cov[i][j] = cov;
  }

  //! set covariant matrix of FVTX+MuTr refit track
  void set_fvtxmutr_cov(size_t i, size_t j, float cov)
  {
    fvtxmutr_cov[i][j] = cov;
  }

  //! set bit word for the number of FVTX tracklets in a cone around the track
  void set_fvtx_tracklets_cone(uint32_t a)
  {
    fvtx_tracklets_cone = a;
  }

  //! set number of FVTX tracklets in a specific dtheta bin
  void set_nfvtx_tracklets_conerange(size_t dthetabin, size_t ntracklets);
 
  //! set bit word for the number of FVTX clusters in a cone around the track
  void set_fvtx_clusters_cone(uint32_t a)
  {
    fvtx_clusters_cone = a;
  }

  //! set number of FVTX clusters in a specific dtheta bin
  void set_nfvtx_clusters_conerange(size_t dthetabin, size_t nclusters);

  //!overall number of cluster, which point to vertex of this track
  void set_nfvtx_tracklets(unsigned int a)
  {
    if (a >= 0xFF) a = 0xFF; //overflow

    nfvtx_tracklets = (uint8_t) a;
  }

  //@}
  
  /*
  Following accessors and modifiers are for hidden (that is: non-persistent)
  varibles. Variables are set, and used, at run-time. They are not stored into
  the output MWG root file
  */
  
  //!@name association to level2 primitives
  //@{
 
  //! number of associated primitives
  virtual unsigned int get_n_primitives( void ) const
  { return _private._n_primitives; }
  
  //! set number of primitives
  virtual void set_n_primitives( const unsigned int& value )
  {
    assert( value <= PHMuoTrackPrivate::_lvl2dim );
    _private._n_primitives = value;  
  }
  
  //! associated level2 primitive angle
  virtual double get_level2_phi( const unsigned int& i ) const
  { 
    assert( i < PHMuoTrackPrivate::_lvl2dim );
    if( i >= get_n_primitives() ) { std::cout << "PHMuoTrack::get_level2_phi - value not set for index " << i << std::endl; }    
    return _private._level2_phi[i];
  }

  //! associated level2 primitive angle
  virtual void set_level2_phi( const unsigned int&i, const double& value )
  { 
    assert( i < PHMuoTrackPrivate::_lvl2dim );
    _private._level2_phi[i] = value;
  }
  
  //! associated level2 primitive angle
  virtual double get_level2_theta( const unsigned int& i ) const
  { 
    assert( i < PHMuoTrackPrivate::_lvl2dim );
    if( i >= get_n_primitives() ) { std::cout << "PHMuoTrack::get_level2_theta - value not set for index " << i << std::endl; }    
    return _private._level2_theta[i];
  }

  //! associated level2 primitive angle
  virtual void set_level2_theta( const unsigned int& i, const double& value )
  { 
    assert( i < PHMuoTrackPrivate::_lvl2dim );
    _private._level2_theta[i] = value;
  }
  
  //!@name level2 mutr primitives
  //@{

  //! number of associated primitives
  virtual unsigned int get_n_mutr_primitives( void ) const
  { return _private._n_mutr_primitives; }
  
  //! set number of primitives
  virtual void set_n_mutr_primitives( const unsigned int& value )
  {
    assert( value <= PHMuoTrackPrivate::_lvl2dim );
    _private._n_mutr_primitives = value;  
  }
  
  //! associated level2 primitive min momentum
  virtual void set_level2_pmin_x( const unsigned int& i, const double& value )
  {
    assert( i < PHMuoTrackPrivate::_lvl2dim );
    _private._level2_pmin_x[i] = value;
  }
  
  //! associated level2 primitive min momentum
  virtual void set_level2_pmin_y( const unsigned int& i, const double& value )
  {
    assert( i < PHMuoTrackPrivate::_lvl2dim );
    _private._level2_pmin_y[i] = value;
  }

  //! associated level2 primitive min momentum
  virtual void set_level2_pmin_z( const unsigned int& i, const double& value )
  {
    assert( i < PHMuoTrackPrivate::_lvl2dim );
    _private._level2_pmin_z[i] = value;
  }

  //! associated level2 primitive min momentum
  virtual double get_level2_pmin_x( const unsigned int& i ) const
  {
    assert( i < PHMuoTrackPrivate::_lvl2dim );
    if( i >= get_n_mutr_primitives() ) { std::cout << "PHMuoTrack::get_level2_pmin_x - value not set for index " << i << std::endl; }    
    return _private._level2_pmin_x[i];
  }


  //! associated level2 primitive min momentum
  virtual double get_level2_pmin_y( const unsigned int& i ) const
  {
    assert( i < PHMuoTrackPrivate::_lvl2dim );
    if( i >= get_n_mutr_primitives() ) { std::cout << "PHMuoTrack::get_level2_pmin_y - value not set for index " << i << std::endl; }    
    return _private._level2_pmin_y[i];
  }

  //! associated level2 primitive min momentum
  virtual double get_level2_pmin_z( const unsigned int& i ) const
  {
    assert( i < PHMuoTrackPrivate::_lvl2dim );
    if( i >= get_n_mutr_primitives() ) { std::cout << "PHMuoTrack::get_level2_pmin_z - value not set for index " << i << std::endl; }    
    return _private._level2_pmin_z[i];
  }

  //! associated level2 primitive max momentum
  virtual void set_level2_pmax_x( const unsigned int& i, const double& value )
  {
    assert( i < PHMuoTrackPrivate::_lvl2dim );
    _private._level2_pmax_x[i] = value;
  }
  
  //! associated level2 primitive max momentum
  virtual void set_level2_pmax_y( const unsigned int& i, const double& value )
  {
    assert( i < PHMuoTrackPrivate::_lvl2dim );
    _private._level2_pmax_y[i] = value;
  }

  //! associated level2 primitive max momentum
  virtual void set_level2_pmax_z( const unsigned int& i, const double& value )
  {
    assert( i < PHMuoTrackPrivate::_lvl2dim );
    _private._level2_pmax_z[i] = value;
  }

  //! associated level2 primitive max momentum
  virtual double get_level2_pmax_x( const unsigned int& i ) const
  {
    assert( i < PHMuoTrackPrivate::_lvl2dim );
    if( i >= get_n_mutr_primitives() ) { std::cout << "PHMuoTrack::get_level2_pmax_x - value not set for index " << i << std::endl; }    
    return _private._level2_pmax_x[i];
  }

  //! associated level2 primitive max momentum
  virtual double get_level2_pmax_y( const unsigned int& i ) const
  {
    assert( i < PHMuoTrackPrivate::_lvl2dim );
    if( i >= get_n_mutr_primitives() ) { std::cout << "PHMuoTrack::get_level2_pmax_y - value not set for index " << i << std::endl; }    
    return _private._level2_pmax_y[i];
  }

  //! associated level2 primitive max momentum
  virtual double get_level2_pmax_z( const unsigned int& i ) const
  {
    assert( i < PHMuoTrackPrivate::_lvl2dim );
    if( i >= get_n_mutr_primitives() ) { std::cout << "PHMuoTrack::get_level2_pmax_z - value not set for index " << i << std::endl; }    
    return _private._level2_pmax_z[i];
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
  virtual double get_event_vertex_z( void ) const
  { return _private._event_vertex_z; }
  
  //! event vertex z error
  /*! 
  it needs to be stored on a track by track basis in order to be able
  to fit together tracks that do not belong to the same event, when 
  performing event mixing
  */
  virtual double get_event_vertex_z_error( void ) const
  { return _private._event_vertex_z_error; }
  
  //! event vertex z
  virtual void set_event_vertex_z( const double& value )
  { _private._event_vertex_z = value; }

  //! event vertex z error
  virtual void set_event_vertex_z_error( const double& value )
  { _private._event_vertex_z_error = value; }
  
  //@}  

  //!@name event reaction plane information
  //@{
  
  //! event reaction plane
  /*! 
  it needs to be stored on a track by track basis in order to be able
  to calculate the "average reaction plane angle for muons that do not belong to the same event
  */
  virtual double get_event_rp_angle( const unsigned int& arm ) const
  { return _private._event_rp_angle[arm]; }
 
  //! event reaction plane
  /*! 
  it needs to be stored on a track by track basis in order to be able
  to calculate the "average reaction plane angle for muons that do not belong to the same event
  */
  virtual void set_event_rp_angle( const unsigned int& arm, const double& value )
  { _private._event_rp_angle[arm] = value; }

  //@}

  
  private:
  
  /**@name Variables

   */
  //@{

  //! unique ID
  /*! 
  it is used to uniquely identify track in events and 
  is a copy of mutoo track TMutTrk::get_obj_key 
  */
  ULong_t uid;
  
  //! Track P$_x$ at (respectively) vertex, station 1, station 2 and station 3. 
  float px[_maxpoints];

  //! Track P$_y$ at (respectively) vertex, station 1, station 2 and station 3.
  float py[_maxpoints];

  //! Track P$_z$ at (respectively) vertex, station 1, station 2 and station 3.
  float pz[_maxpoints];

  //! Track X position at (respectively) vertex, station 1, station 2 and station 3.
  float xpos[_maxpoints];

  //! Track Y position at (respectively) vertex, station 1, station 2 and station 3.
  float ypos[_maxpoints];

  //! Track Z position at (respectively) vertex, station 1, station 2 and station 3.
  float zpos[_maxpoints];

  //! Number of hits used to build the track.
  short nhits;

  //! Track charge.
  short charge;

  //! Kalman covariance matrix.
  float cov[_covdim][_covdim];
      
  //! track number of degrees of freedom
  int ndf;

  //! reduced $\chi^2$ of fitted track.
  float chisquare;   

  //! Track Bend Plane momentum at station 1 (P[0]=P$_x$, P[1]=P$_y$, P[2]=P$_z$).
  float st1_bp_P[3];

  //! Track Bend Plane position at station 1 ([0]=$x$, [1]=$y$, [2]=$z$).
  float st1_bp_pos[3];

  //! hitplans.
  int muTRhits;

  //! road chi_squre for MUIOO.
  float muIDOOchi[_maxroads];

  //! road hit description for MUIOO
  int muIDOOhits[_maxroads];

  //! road position and direction at gap0 (x,y,z,dxdz,dydz) for MUIOO
  float muIDOO_gap0[5][_maxroads];

  //! MutOO track status
  int TMutTrk_status;
  
  //! charge difference for a given gap coordinate
  float _delta_q[ _max_gap_coords ];
  
  //! error on charge difference for a given gap coordinate
  float _delta_q_error[ _max_gap_coords ];

  //! hit coordinate in each gap for the deepest road
  float _muid_hit_x[5];

  //! hit coordinate in each gap for the deepest road
  float _muid_hit_y[5];
  
  //!number of clusters with size=1
  unsigned short clusters_size1;

  //! 3-vector FVTX momentum assuming total momentum from MuTr
  float fvtx_p[3];

  //! 3-vector FVTX vertex position
  float fvtx_vtx[3];

  //! covariant matrix of FVTX track
  float fvtx_cov[_covdim][_covdim];

  //! 3-vector FVTX+MuTr refit momentum assuming total momentum from MuTr
  float fvtxmutr_p[3];

  //! 3-vector FVTX+MuTr refit vertex position
  float fvtxmutr_vtx[3];

  //! chi2/NDF for refit vertex position
  float fvtxmutr_chi2;

  //! covariant matrix of FVTX+MuTr refit track
  float fvtxmutr_cov[_covdim][_covdim];

  //! bit pattern with cluster size in each FVTX plane (assuming 8 planes and cluster size up to 15)
  int fvtx_cluster_size;

  //! cluster charge in each FVTX station
  float fvtx_cluster_charge[4];

  //! delta_phi residual btw. MuTr and FVTX track
  float fvtx_dphi;

  //! delta_theta residual btw. MuTr tand FVTX track
  float fvtx_dtheta;

  //! delta_r residual  btw. MuTr tand FVTX track at z=+/-40cm 
  float fvtx_dr;

  //! track chi^2 for MuTr+FVTx track
  float fvtx_chi2;

  //! offset from peak FVTX strip
  float fvtx_w[4];

  //!number of tracklets around a binned cone size
  uint32_t fvtx_tracklets_cone;

  //!number of cluster around a binned cone size
  uint32_t fvtx_clusters_cone;

  //!overall number of cluster, which point to vertex of this track
  uint8_t nfvtx_tracklets;

  //! index of best matching FVTX strip in each station
  unsigned long fvtx_strip[4];

  //@}
   
  //! hidden variables
  /*!
  Contains variables needed as members of PHMuoTrack object to easier event mixing.
  however they must not get written to the MWG output root file since they are redundant with information
  stored elsewhere and would therefore artificially increase the file size, for nothing.
  IMPORTANT NOTE: the "//!" symbols following the member declaration means that it is not stored in the root file
  IT MUTS NOT BE REMOVER
  */
  PHMuoTrackPrivate _private; //!
  
  //! root dictionarization class definition
  ClassDef(PHMuoTrackv15,1)
};
#endif
	

