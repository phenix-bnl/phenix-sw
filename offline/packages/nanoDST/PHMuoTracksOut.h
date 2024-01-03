#ifndef __PHMUOTRACKSOUT_H
#define __PHMUOTRACKSOUT_H

#include <iostream>
#include <phool.h>
#include <PHParticle.h>

class PHMuoTracksOut : public PHParticle
{
  public:
  virtual ~PHMuoTracksOut(){}
  
  //========== Standard functions of all virtual classes...
  virtual void Reset() {
    //std::cout<<PHWHERE<<"ERROR: Reset() not implemented by daughter function"<<std::endl;
    return;
  }
  
  virtual int IsValid() const {
    std::cout<<PHWHERE<<"ERROR: isValid() not implemented by daughter function"<<std::endl;
    return 0;
  }
  
  virtual void identify(std::ostream &os=std::cout) const {
    os << "identify yourself: virtual PHMuonTrack object\n";
    return;
  }
  
  //================================= Particle Fillers
  virtual int set_TClonesArraySize(const unsigned int /*npart*/)
  {warning("set_TClonesArraySize"); return 0;}

  virtual void AddPHParticle(const unsigned int /*itrk*/)
  {warning("AddPHParticle"); return;}

  virtual void RemovePHParticle(const unsigned int /*itrk*/)
  {warning("RemovePHParticle"); return;}

  //=================================== Dimuon Fillers
  virtual int Set_DimuArraySize(const unsigned int /*ndimu*/)
  {warning("Set_DimuArraySize"); return 0;}

  virtual void AddPHDimuon(const unsigned int /*idimu*/)
  {warning("AddPHDimuon"); return;}

  virtual void RemovePHDimuon(const unsigned int /*idimu*/)
  {warning("RemovePHDimuon"); return;}

  //!@name single muon accessors
  //@{

  virtual unsigned int get_npart() const
  {warning("get_npart"); return 0;}

  //! single muon unique ID
  virtual ULong_t get_uid( const unsigned int /*itrk */) const
  {
    //warning("get_uid");
    return 0;
  }

  virtual short get_PID(const unsigned int /*itrk*/) const
  {warning("get_PID"); return 0;}

  virtual short get_charge(const unsigned int /*itrk*/) const
  {warning("get_charge"); return 0;}

  virtual float get_px(short /*arrayid*/, const unsigned int /*itrk*/) const
  {warning("get_px"); return 0;}

  virtual float get_py(short /*arrayid*/, const unsigned int /*itrk*/) const
  {warning("get_py"); return 0;}

  virtual float get_pz(short /*arrayid*/, const unsigned int /*itrk*/) const
  {warning("get_pz"); return 0;}

  virtual float get_px(unsigned int itrk) const { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual float get_py(unsigned int itrk) const { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual float get_pz(unsigned int itrk) const { PHOOL_VIRTUAL_WARNING; return 0; }

  virtual float get_st1_bp_P(short /*arrayid*/, const unsigned int /*itrk*/) const
  {warning("get_st1_bp_P"); return 0;}

  virtual float get_st1_bp_pos(short /*arrayid*/, const unsigned int /*itrk*/) const
  {warning("get_st1_bp_pos"); return 0;}

  virtual float get_xpos(short /*arrayid*/, const unsigned int /*itrk*/) const
  {warning("get_xpos"); return 0;}

  virtual float get_ypos(short /*arrayid*/, const unsigned int /*itrk*/) const
  {warning("get_ypos"); return 0;}

  virtual float get_zpos(short /*arrayid*/, const unsigned int /*itrk*/) const
  {warning("get_zpos"); return 0;}

  virtual short get_nhits(const unsigned int /*itrk*/) const
  {warning("get_nhits"); return 0; }

  virtual float get_cov(short /*arrayid1*/, short /*arrayid2*/, const unsigned int /*itrk*/) const
  {warning("get_cov"); return 0;}

  virtual float get_MuonConfidence(const unsigned int /*itrk*/) const
  {warning("get_MuonConfidence"); return 0;}

  virtual float get_PionConfidence(const unsigned int /*itrk*/) const
  {warning("get_PionConfidence"); return 0;}

  //! returns track reduced chisquare
  virtual float get_chisquare(const unsigned int /*itrk*/) const
  {warning("get_chisquare"); return 0;}

  //! returns track number of degrees of freedom
  virtual int get_ndf(const unsigned int /*itrk*/) const
  {warning("get_ndf"); return 0;}

  virtual float get_ghostflag(const unsigned int /*itrk*/) const
  {warning("get_ghostflag"); return 0;}

  //! this function does not exist anymore. Keep the declaration for backward compatibility
  virtual int get_hitplans(const unsigned int /*itrk*/) const
  {return 0;}

  virtual int get_muTRhits(const unsigned int /*itrk*/) const
  {warning("get_muTRhits"); return 0;}

  //! returns true if the requested track fired the requested road
  virtual bool is_muTRhit(const unsigned int itrk, int station, int gap, int plane) const
  { 
    // there are 16 bits here. 
    // First 6 are for station1, 
    // next are station2 
    // and remaining 4 bits are station3
    // For a given station, bits are ordered first gap, plane 0 and 1, second gap, plane 0 and 1, 
    // then third gap (except for station3)
    return get_muTRhits( itrk ) & (1<<(station*6 + gap*2 + plane)); 
  }

  virtual float get_muIDOOchi(const short /*iroad*/, const unsigned int /*itrk*/) const
  {warning("get_muIDOOchi"); return 0;}

  virtual int get_muIDOOhits(const short /*iroad*/, const unsigned int /*itrk*/) const
  {warning("get_muIDhits"); return 0;}

  //! returns true if the requested muid road associated to the track 
  //! has a hit in the required 
  virtual bool is_muIDOOhit( const unsigned int iroad, const unsigned int itrk, int plane, int orientation) const
  {

    // first 5 bits are for horizontal views
    // next 5 bits are for vertical views
    return get_muIDOOhits( iroad, itrk ) & (1<<(plane + 5*orientation)); 
    
  }

  //! returns true if any of the 3 possible muid road associated to the track 
  //! has a hit in the required 
  virtual bool is_muIDOOhit(const unsigned int itrk, int plane, int orientation) const
  { 
    return 
      is_muIDOOhit( 0, itrk, plane, orientation ) ||
      is_muIDOOhit( 1, itrk, plane, orientation ) ||
      is_muIDOOhit( 2, itrk, plane, orientation );
  }

  virtual float get_muIDOO_gap0(const short /*arrayid*/, const short /*iroad*/, const unsigned int /*itrk*/) const
  {warning("get_muIDOO_gap0"); return 0;}

  //! old framework muid hit pattern.
  virtual int get_muIDhits(const unsigned int /*itrk*/) const
  {warning("get_muIDhit"); return 0;}

  //! true if muid road fired specified plane/orientation
  virtual bool is_muIDhit(const unsigned int itrk, int plane, int orientation) const
  {
    
    // bits are packed 2 by 2. 
    // First 2 bits are first gap, vertical then horizontal. 
    // Next 2 bits are second gap, etc. 
    return get_muIDhits( itrk ) & (1<<(2*plane + orientation ));    
    
  }

  virtual float get_muID_gap0(const short /*arrayid*/, const unsigned int /*itrk*/) const
  {warning("get_muID_gap0"); return 0;}

  virtual float get_muID_proj_hit_dist(short /*gap*/, short /*orient*/, short /*hit*/, const unsigned int /*itrk*/) const
  {warning("get_muID_proj_hit_dist"); return 0;}

  virtual short get_muID_proj_hit_size(short /*gap*/, short /*orient*/, short /*hit*/, const unsigned int /*itrk*/) const
  {warning("get_muID_proj_hit_size"); return 0;}

  virtual float get_muid_hit_x(const short /*gap*/, const unsigned int /*itrk*/) const
  {
    //warning("get_muid_hit_x"); 
    return 0;
  }

  virtual float get_muid_hit_y(const short /*gap*/, const unsigned int /*itrk*/) const
  {
    //warning("get_muid_hit_y");
    return 0;
  }

  virtual int get_TMutTrk_status(const unsigned int /*itrk*/) const
  {warning("get_TMutTrk_status"); return 0;}

  //}

  //!@name Dimuon Accessors
  //@{
  
  virtual bool do_dimu() const 
  {warning("do_dimu"); return false;}
  
  virtual int get_ndimu() const 
  {warning("get_ndimu"); return 0;}
  
  virtual ULong_t get_dimuon_uid( const unsigned int ) const
  {
    //warning("get_dimuon_uid"); 
    return 0;
  }

  virtual int get_ditrkIndex(short /*arrayid*/, const unsigned int /*idimu*/) const
  {warning("get_ditrkIndex"); return 0;}

  virtual float get_dimass(const unsigned int) const
  {warning("get_dimass"); return 0;}

  virtual int get_dicharge(const unsigned int) const
  {warning("get_dicharge"); return 0;} 
  
  virtual float get_dipx(const unsigned int) const 
  {warning("get_dipx"); return 0;} 
  
  virtual float get_dipy(const unsigned int) const 
  {warning("get_dipy"); return 0;} 
  
  virtual float get_dipz(const unsigned int) const 
  {warning("get_dipz"); return 0;} 
  
  //! returns vertex bendplane x position
  virtual float get_vtx_bp_xpos(const unsigned int) const
  {warning("get_vtx_bp_xpos"); return 0;}
  
  //! returns vertex bendplane y position
  virtual float get_vtx_bp_ypos(const unsigned int) const
  {warning("get_vtx_bp_ypos"); return 0;}
  
  //! returns vertex bendplane z position
  virtual float get_vtx_bp_zpos(const unsigned int) const
  {warning("get_vtx_bp_zpos"); return 0;}
  
  //! returns vertex bendplane distance of closest approach
  virtual float get_vtx_bp_dca(const unsigned int) const
  {warning("get_vtx_bp_dca"); return 0;}
  
  //! returns vertex fit x position
  virtual float get_vtx_xpos(const unsigned int) const
  {warning("get_vtx_xpos"); return 0;}
  
  //! returns vertex fit y position
  virtual float get_vtx_ypos(const unsigned int) const
  {warning("get_vtx_ypos"); return 0;}
  
  //! returns vertex fit z position
  virtual float get_vtx_zpos(const unsigned int) const
  {warning("get_vtx_zpos"); return 0;}
  
  virtual float get_vtx_chrg_1(const unsigned int) const
  {warning("get_vtx_chrg_1"); return 0;}
  
  virtual float get_vtx_px_1(const unsigned int) const
  {warning("get_vtx_px_1"); return 0;}
  
  virtual float get_vtx_py_1(const unsigned int) const
  {warning("get_vtx_py_1"); return 0;}
  
  virtual float get_vtx_pz_1(const unsigned int) const
  {warning("get_vtx_pz_1"); return 0;}
  
  virtual float get_vtx_chrg_2(const unsigned int) const
  {warning("get_vtx_chrg_2"); return 0;}
  
  virtual float get_vtx_px_2(const unsigned int) const
  {warning("get_vtx_px_2"); return 0;}
  
  virtual float get_vtx_py_2(const unsigned int) const
  {warning("get_vtx_py_2"); return 0;}
  
  virtual float get_vtx_pz_2(const unsigned int) const
  {warning("get_vtx_pz_2"); return 0;}
  
  //! returns vertex number of degrees of freedom
  virtual int get_vtx_ndf(const unsigned int) const
  {warning("get_vtx_ndf"); return 0;}
  
  //! returns vertex reduced chi_square from vtx fit
  virtual float get_vtx_chisquare(const unsigned int) const
  {warning("get_vtx_chisquare"); return 0;}

  //! returns vertex covariance matrix element
  virtual float get_vtx_cov(short /*arrayid1*/, short /*arrayid2*/, const unsigned int) const
  {warning("get_vtx_cov"); return 0;}

  /// return gap_coordinate charge difference
  virtual float get_delta_q( const unsigned int /*coord_id*/, const unsigned int /*itrk */) const
  {warning("get_delta_q"); return 0;}

  /// return error on gap_coordinate charge difference
  virtual float get_delta_q_error( const unsigned int /*coord_id*/, const unsigned int /*itrk */) const
  {warning("get_delta_q_error"); return 0;}
  //@}

  //! return bit pattern for size==1 clusters
  virtual unsigned short get_clusters_size1(const short itrk) const
  {warning("get_clusters_size1"); return 0;}
  
    //! return 3-vector FVTX vertex position of the track
  virtual float get_fvtx_vtx(const unsigned int itrk, const size_t coord) const
  {warning("get_fvtx_vtx"); return 0;}

  //! return 3-vector FVTX momentum
  virtual float get_fvtx_p(const unsigned int itrk, const size_t coord) const
  {warning("get_fvtx_p"); return -9999.;}

    //! return 3-vector FVTX+MuTr refit vertex position of the track
  virtual float get_fvtxmutr_vtx(const unsigned int itrk, const size_t coord) const
  {warning("get_fvtxmutr_vtx"); return -9999.;}

  //! return 3-vector FVTX+MuTr refit momentum
  virtual float get_fvtxmutr_p(const unsigned int itrk, const size_t coord) const
  {warning("get_fvtxmutr_p"); return -9999.;}

  //! return chi^2 per degree of freedom FVTX+MuTr refit momentum
  virtual float get_fvtxmutr_chi2(const unsigned int itrk) const
  {warning("get_fvtxmutr_chi2"); return -9999.;}

  //! return bit pattern with cluster size in each FVTX plane (assuming 8 planes and cluster size up to 15)
  virtual int get_fvtx_cluster_size_word(const unsigned int itrk) const
  {warning("get_fvtx_cluster_size_word"); return 0;}

  //! return cluster size for each FVTX plane [0,7]
  //! plane = station*2;
  //! if (!(sector%2) && arm==MUTOO::South) plane++;
  //! if ((sector%2) && arm==MUTOO::North) plane++;
  virtual size_t get_fvtx_cluster_size(const unsigned int itrk, const size_t plane) const
  {warning("get_fvtx_cluster_size"); return 0;}

  //! return phi residual btw. MuTr and FVTX track in sigmas considering fvtx track window
  virtual float get_fvtx_dphi(const unsigned int itrk) const
  {warning("get_fvtx_dphi"); return -9999.;}

  //! return theta residual btw. MuTr and FVTX track
  virtual float get_fvtx_dtheta(const unsigned int itrk) const
  {warning("get_fvtx_dtheta"); return -9999.;}

  //! return radius residual btw. MuTr and FVTX track projection at Z=+/-40cm
  virtual float get_fvtx_dr(const unsigned int itrk) const
  {warning("get_fvtx_dr"); return -9999.;}

  //! return chi^2 for MuTr+FVTX track
  virtual float get_fvtx_chi2(const unsigned int itrk) const
  {warning("get_fvtx_chi2"); return -9999.;}

  //! return offset from peak strip (cm)
  virtual float get_fvtx_w(const unsigned int itrk, const size_t station) const
  {warning("get_fvtx_w"); return -9999.;}

  //! return global index of best matching fvtx strip
  virtual unsigned long get_fvtx_global_strip(const unsigned int itrk, const size_t station) const
  {warning("get_fvtx_global_strip"); return 0;}

  //! return index of best matching fvtx strip
  virtual short unsigned int get_fvtx_strip(const unsigned int itrk, const size_t station) const
  {warning("get_fvtx_strip"); return 0;}

  //! return column of best matching fvtx strip
  virtual unsigned int get_fvtx_column(const unsigned int itrk, const size_t station) const
  {warning("get_fvtx_column"); return 0;}

  //! return sector(wedge) of best matching fvtx strip
  virtual unsigned int get_fvtx_sector(const unsigned int itrk, const size_t station) const
  {warning("get_fvtx_sector"); return 0;}

  //! return cage of best matching fvtx strip
  virtual unsigned int get_fvtx_cage(const unsigned int itrk, const size_t station) const
  {warning("get_fvtx_cage"); return 0;}

  //! return arms of the fvtx track
  virtual unsigned int get_fvtx_arm(const unsigned int itrk) const
  {warning("get_fvtx_arm"); return 0;}

  //! return covariant matrix of FVTX track
  virtual float get_fvtx_cov(const unsigned int itrk, const size_t i, const size_t j) const
  {warning("get_fvtx_cov"); return 0;}

  //! return covariant matrix of FVTX+MuTr refit track
  virtual float get_fvtxmutr_cov(const unsigned int itrk, const size_t i, const size_t j) const
  {warning("get_fvtxmutr_cov"); return 0;}

  //! return charge in FVTX cluster
  virtual float get_fvtx_cluster_charge(const unsigned int itrk, const size_t station) const
  {warning("get_fvtx_cluster_charge"); return 0;}

  //! return the bit word with the number of FVTX tracklets around dR cone bins
  virtual unsigned long get_fvtx_tracklets_cone(const unsigned int itrk) const
  {warning("get_fvtx_tracklets_cone"); return 0;}

  //! return the number of FVTX tracklets around the track in a dR bin cone
  virtual size_t get_nfvtx_tracklets_conerange(const unsigned int itrk, size_t dtheta_bin) const
  {warning("get_nfvtx_tracklets_conerange"); return 0;}

  //! return the bit word with the number of FVTX clusters around dR cone bins
  virtual unsigned long get_fvtx_clusters_cone(const unsigned int itrk) const
  {warning("get_fvtx_clusters_cone"); return 0;}

  //! return the number of FVTX clusters around the track in a dr bin cone
  virtual size_t get_nfvtx_clusters_conerange(const unsigned int itrk, size_t dtheta_bin) const
  {warning("get_nfvtx_clusters_conerange"); return 0;}

  //!overall number of cluster, which point to vertex of this track
  virtual unsigned int get_nfvtx_tracklets(const unsigned int itrk) const
  {warning("get_nfvtx_tracklets"); return 0;}

  //! set VTX+FVTX hit pattern
  //! NA NA NA NA VTX4 VTX3 VTX2 VTX1 FVTX4_1 FVTX_4_0 FVTX3_1 FVTX3_0 FVTX2_1 FVTX2_0 FVTX1_1 FVTX1_0
  virtual unsigned short get_fvtx_hits(const unsigned int itrk) const
  {warning("get_fvtx_hits"); return 0;}

    //! return 3-vector swapped FVTX vertex position of the track 
  virtual float get_sfvtx_vtx(const unsigned int itrk, const size_t coord) const
  {warning("get_sfvtx_vtx"); return -999.9;}

  //! return 3-vector swapped FVTX momentum assuming total momentum from MuTr
  virtual float get_sfvtx_p(const unsigned int itrk, const size_t coord) const
  {warning("get_sfvtx_p"); return -999.9;}

  //! return 3-vector swapped FVTX+MuTr refit vertex position of the track 
  virtual float get_sfvtxmutr_vtx(const unsigned int itrk, const size_t coord) const
  {warning("get_sfvtxmutr_vtx"); return -999.9;}

  //! return 3-vector swapped FVTX+MuTr refit momentum assuming total momentum from MuTr
  virtual float get_sfvtxmutr_p(const unsigned int itrk, const size_t coord) const
  {warning("get_sfvtxmutr_p"); return -999.9;}

  //! return chi2/NDF swapped FVTX+MuTr refit momentum assuming total momentum from MuTr
  virtual float get_sfvtxmutr_chi2(const unsigned int itrk) const
  {warning("get_sfvtxmutr_chi2"); return -999.9;}

  //! return phi residual btw. MuTr and swapped FVTX track in sigmas considering fvtx track window
  virtual float get_sfvtx_dphi(const unsigned int itrk) const
  {warning("get_sfvtx_dphi"); return -999.9;}

  //! return theta residual btw. MuTr and swapped FVTX track
  virtual float get_sfvtx_dtheta(const unsigned int itrk) const
  {warning("get_sfvtx_dtheta"); return -999.9;}

  //! return radius residual btw. MuTr and swapped FVTX track at Z=+/-40 cm
  virtual float get_sfvtx_dr(const unsigned int itrk) const
  {warning("get_sfvtx_dr"); return -999.9;}

  //! return chi^2 for MuTr+ swapped FVTX track
  virtual float get_sfvtx_chi2(const unsigned int itrk) const
  {warning("get_sfvtx_chi2"); return -999.9;}

  //! return covariant matrix of swapped FVTX track
  virtual float get_sfvtx_cov(const unsigned int itrk, const size_t i, const size_t j) const
  {warning("get_sfvtx_cov"); return -999.9;}

  //! return covariant matrix of swapped FVTX+MuTr refit track
  virtual float get_sfvtxmutr_cov(const unsigned int itrk, const size_t i, const size_t j) const
  {warning("get_sfvtxmutr_cov"); return -999.9;}

  //!@name single muon mutator
  //@{

  virtual void set_npart(const unsigned int /*npart*/)
  {warning("set_npart");}

  virtual void set_uid(const unsigned int /*itrk*/, const ULong_t /*Value*/)
  {}
  //{warning("set_uid");}

  virtual void set_PID(const unsigned int /*itrk*/, const short /*newVal*/)
  {warning("set_PID");}

  virtual void set_charge(const unsigned int /*itrk*/, const short /*newVal*/)
  {warning("set_charge");}

  virtual void set_px(short /*arrayid*/, const unsigned int /*itrk*/, float /*newVal*/)
  {warning("set_px");}

  virtual void set_py(short /*arrayid*/, const unsigned int /*itrk*/, float /*newVal*/)
  {warning("set_py");}

  virtual void set_pz(short /*arrayid*/, const unsigned int /*itrk*/, float /*newVal*/)
  {warning("set_pz");}

  virtual void set_px(unsigned int itrk, float val) { PHOOL_VIRTUAL_WARNING; }
  virtual void set_py(unsigned int itrk, float val) { PHOOL_VIRTUAL_WARNING; }
  virtual void set_pz(unsigned int itrk, float val) { PHOOL_VIRTUAL_WARNING; }

  virtual void set_st1_bp_P(short /*arrayid*/, const unsigned int /*itrk*/, float /*newVal*/)
  {warning("set_st1_bp_P");}

  virtual void set_st1_bp_pos(short /*arrayid*/, const unsigned int /*itrk*/, float /*newVal*/)
  {warning("set_st1_bp_pos");}

  virtual void set_xpos(short /*arrayid*/, const unsigned int /*itrk*/, float /*newVal*/)
  {warning("set_xpos");}

  virtual void set_ypos(short /*arrayid*/, const unsigned int /*itrk*/, float /*newVal*/)
  {warning("set_ypos");}

  virtual void set_zpos(short /*arrayid*/, const unsigned int /*itrk*/, float /*newVal*/)
  {warning("set_zpos");}

  virtual void set_nhits(const unsigned int /*itrk*/, const short /*newVal*/)
  {warning("set_nhits");}

  virtual void set_cov(short /*arrayid1*/, short /*arrayid2*/, const unsigned int /*itrk*/, float /*newVal*/)
  {warning("set_cov");}

  virtual void set_MuonConfidence(const unsigned int /*itrk*/, float /*newVal*/)
  {warning("set_MuonConfidence");}

  virtual void set_PionConfidence(const unsigned int /*itrk*/, float /*newVal*/)
  {warning("set_PionConfidence");}

  //! sets track reduced chisquare
  virtual void set_chisquare(const unsigned int /*itrk*/, float /*newVal*/)
  {warning("set_chisquare");}

  //! sets track number of degrees of freedom
  virtual void set_ndf(const unsigned int /*itrk*/, int /*newVal*/)
  {warning("set_ndf");}

  virtual void set_ghostflag(const unsigned int /*itrk*/, float /*newVal*/)
  {warning("set_ghostflag");}

  // this function does not exist anymore. Keep the declaration for backward compatibility
  virtual void set_hitplans(const unsigned int /*itrk*/, int /*newVal*/)
  {;}

  virtual void set_muTRhits(const unsigned int /*itrk*/, int /*newVal*/)
  {warning("set_muTRhits");}

  virtual void set_muIDOOchi(const short /*iroad*/, const unsigned int /*itrk*/, float /*newVal*/)
  {warning("set_muIDOOchi");}

  virtual void set_muIDOOhits(const short /*iroad*/, const unsigned int /*itrk*/, int /*newVal*/)
  {warning("set_muIDOOhits");}

  virtual void set_muIDOO_gap0(const short /*arrayid*/, const short /*iroad*/, const unsigned int /*itrk*/, float /*newVal*/)
  {warning("set_muIDOO_gap0");}

  virtual void set_muIDhits(const unsigned int /*itrk*/, int /*newVal*/)
  {warning("set_muIDhits");}

  virtual void set_muID_gap0(const short /*arrayid*/, const unsigned int /*itrk*/, float /*newVal*/)
  {warning("set_muID_gap0");}

  virtual void set_muID_proj_hit_dist(short /*gap*/, short /*orient*/, short /*hit*/, const unsigned int /*itrk*/, float /*newVal*/)
  {warning("set_muID_proj_hit_dist");}

  virtual void set_muID_proj_hit_size(short /*gap*/, short /*orient*/, short /*hit*/, const unsigned int /*itrk*/, short /*newVal*/)
  {warning("set_muID_proj_hit_size");}

  virtual void set_muid_hit_x(const short /*gap*/, const unsigned int /*itrk*/, const float /*newVal*/)
  {}
  //{warning("set_muid_hit_x");}

  virtual void set_muid_hit_y(const short /*gap*/, const unsigned int /*itrk*/, const float /*newVal*/)
  {}
  //{warning("set_muid_hit_y");}

  virtual void set_TMutTrk_status(const unsigned int /*itrk*/, int /*newVal*/)
  {warning("set_TMutTrk_status");}

  //@}

  //!@name dimuon mutators
  //@{
  virtual void set_ndimu(const unsigned int /*ndimu*/)
  {warning("set_ndimu");}

  virtual void set_dimuon_uid( const unsigned int /*idimu*/, ULong_t /*value*/)
  {}
  //{ warning( "set_dimuon_uid" ); }

  virtual void set_dimass(const unsigned int /*idimu*/, float /*newVal*/)
  {warning("set_dimass");}

  virtual void set_ditrkIndex(short /*arrayid*/, const unsigned int /*idimu*/, int /*newVal*/)
  {warning("set_ditrkIndex");}

  virtual void set_dicharge(const unsigned int /*idimu*/, int /*newVal*/)
  {warning("set_dicharge");}

  virtual void set_dipx(const unsigned int /*idimu*/, float /*newVal*/)
  {warning("set_dipx");}

  virtual void set_dipy(const unsigned int /*idimu*/, float /*newVal*/)
  {warning("set_dipy");}

  virtual void set_dipz(const unsigned int /*idimu*/, float /*newVal*/)
  {warning("set_dipz");}

  //! changes vertex bend plane x position
  virtual void set_vtx_bp_xpos(const unsigned int /*idimu*/, float /*newVal*/)
  {warning("set_vtx_bp_xpos");}

  //! changes vertex bend plane y position
  virtual void set_vtx_bp_ypos(const unsigned int /*idimu*/, float /*newVal*/)
  {warning("set_vtx_bp_ypos");}

  //! changes vertex bend plane z position
  virtual void set_vtx_bp_zpos(const unsigned int /*idimu*/, float /*newVal*/)
  {warning("set_vtx_bp_zpos");}

  //! changes vertex bend plane distance of closest approach
  virtual void set_vtx_bp_dca(const unsigned int /*idimu*/, float /*newVal*/)
  {warning("set_vtx_bp_dca");}

  //! changes vertex fit x position
  virtual void set_vtx_xpos(const unsigned int /*idimu*/, float /*newVal*/)
  {warning("set_vtx_xpos");}

  //! changes vertex fit y position
  virtual void set_vtx_ypos(const unsigned int /*idimu*/, float /*newVal*/)
  {warning("set_vtx_ypos");}

  //! changes vertex fit z position
  virtual void set_vtx_zpos(const unsigned int /*idimu*/, float /*newVal*/)
  {warning("set_vtx_zpos");}

  virtual void set_vtx_chrg_1(const unsigned int /*idimu*/, float /*newVal*/)
  {warning("set_vtx_chrg_1");}

  virtual void set_vtx_px_1(const unsigned int /*idimu*/, float /*newVal*/)
  {warning("set_vtx_px_1");}

  virtual void set_vtx_py_1(const unsigned int /*idimu*/, float /*newVal*/)
  {warning("set_vtx_py_1");}

  virtual void set_vtx_pz_1(const unsigned int /*idimu*/, float /*newVal*/)
  {warning("set_vtx_pz_1");}

  virtual void set_vtx_chrg_2(const unsigned int /*idimu*/, float /*newVal*/)
  {warning("set_vtx_chrg_2");}

  virtual void set_vtx_px_2(const unsigned int /*idimu*/, float /*newVal*/)
  {warning("set_vtx_px_2");}

  virtual void set_vtx_py_2(const unsigned int /*idimu*/, float /*newVal*/)
  {warning("set_vtx_py_2");}

  virtual void set_vtx_pz_2(const unsigned int /*idimu*/, float /*newVal*/)
  {warning("set_vtx_pz_2");}

  //! sets vertex number of degrees of freedom
  virtual void set_vtx_ndf( const unsigned int /*idimu*/, int /*newVal*/)
  {warning("set_vtx_ndf");}

  //! sets vertex reduced chisquare
  virtual void set_vtx_chisquare(const unsigned int /*idimu*/, float /*newVal*/)
  {warning("set_vtx_chisquare");}

  virtual void set_vtx_cov(short /*arrayid1*/, short /*arrayid2*/, const unsigned int /*idimu*/, float /*newVal*/)
  {warning("set_vtx_cov");}

  /// set gap_coordinate charge difference
  virtual void set_delta_q( const unsigned int /*coord_id*/, const unsigned int /*itrk*/, float /*value*/)
  {warning("set_delta_q");}

  /// set error on gap_coordinate charge difference
  virtual void set_delta_q_error( const unsigned int /*coord_id*/, const unsigned int /*itrk*/, float /*value*/)
  {warning("set_delta_q_error");}

  //! set bit patter for size==1 clusters
  virtual void set_clusters_size1(const unsigned int /*itrk*/, const unsigned short /* x */)
  {warning("set_clusters_size1");}

  //! set 3-vector FVTX vertex position of the track
  virtual void set_fvtx_vtx(const unsigned int /*itrk*/, const size_t /* coord */, const float /* x */)
  {warning("set_fvtx_vtx");}

  //! set 3-vector FVTX momentum
  virtual void set_fvtx_p(const unsigned int itrk, const size_t coord, const float p)
  {warning("set_fvtx_p");}

    //! set 3-vector FVTX+MuTr refit vertex position of the track
  virtual void set_fvtxmutr_vtx(const unsigned int /*itrk*/, const size_t /* coord */, const float /* x */)
  {warning("set_fvtxmutr_vtx");}

  //! set 3-vector FVTX+MuTr refit momentum
  virtual void set_fvtxmutr_p(const unsigned int itrk, const size_t coord, const float p)
  {warning("set_fvtxmutr_p");}

  //! set chi2 for combined FVTX+MuTr refit momentum
  virtual void set_fvtxmutr_chi2(const unsigned int itrk, const float a)
  {warning("set_fvtxmutr_chi2");}

  //! set bit pattern with cluster size in each FVTX plane
  virtual void set_fvtx_cluster_size(const unsigned int itrk, const int word)
  {warning("set_fvtx_cluster_size");}

  //! set bit pattern with cluster size in each FVTX plane (assuming 8 planes and cluster size up to 15)
  virtual void set_fvtx_cluster_size(const unsigned int itrk, const size_t plane, const size_t cluster_size)
  {warning("set_fvtx_cluster_size");}

  //! set phi residual btw. MuTr and FVTX track
  virtual void set_fvtx_dphi(const unsigned int itrk, const float dphi)
  {warning("set_fvtx_dphi");}

  //! set theta residual btw. MuTr and FVTX track
  virtual void set_fvtx_dtheta(const unsigned int itrk, const float dtheta)
  {warning("set_fvtx_dtheta");}

  //! set radius residual btw. MuTr and FVTX track projection at Z=+/-40cm
  virtual void set_fvtx_dr(const unsigned int itrk, const float dr)
  {warning("set_fvtx_dr");}

  //! set chi^2 for MuTr+FVTX track
  virtual void set_fvtx_chi2(const unsigned int itrk, const float chi2)
  {warning("set_fvtx_chi2");}

  //! set offset from peak strip in each FVTX station
  virtual void set_fvtx_w(const unsigned int itrk, const size_t station, const float proj)
  {warning("set_fvtx_w");}

  //! set index of best matching fvtx strip
  virtual void set_fvtx_strip(const unsigned int itrk, 
			      const size_t station, 
			      const bool arm,
			      const bool cage,
			      const unsigned short sector,
			      const bool column,
			      const unsigned short strip)
  {warning("set_fvtx_strip");}

  //! set index of best matching fvtx strip
  virtual void set_fvtx_strip(const unsigned int itrk, const size_t station, const unsigned short index)
  {warning("set_fvtx_strip");}

  //! set covariant matrix of FVTX track
  virtual void set_fvtx_cov(const unsigned int itrk, const size_t i, const size_t j, const float cov)
  {warning("set_fvtx_cov");}

  //! set covariant matrix of FVTX+MuTr refit track
  virtual void set_fvtxmutr_cov(const unsigned int itrk, const size_t i, const size_t j, const float cov)
  {warning("set_fvtxmutr_cov");}

  //! set charge in FVTX cluster
  virtual void set_fvtx_cluster_charge(const unsigned int itrk, const size_t station, const float charge)
  {warning("set_fvtx_cluster_charge");}

  //! set number of FVTX tracklets (more than one coordinate projected to vertex) in a dR around the track
  virtual void set_nfvtx_tracklets_conerange(const unsigned int itrk, const size_t dthetabin, const size_t nclusters)
  {warning("set_nfvtx_tracklets_conerange");}

  //! set number of FVTX clusters in a dr-bin around the track
  virtual void set_nfvtx_clusters_conerange(const unsigned int itrk, const size_t dthetabin, const size_t nclusters)
  {warning("set_nfvtx_clusters_conerange");}

  //!overall number of cluster, which point to vertex of this track
  virtual void set_nfvtx_tracklets(const unsigned int itrk, const unsigned int a)
  {warning("set_nfvtx_tracklets");}

  //! set 3-vector swapped FVTX vertex position of the track
  virtual void set_sfvtx_vtx(const unsigned int itrk, const size_t coord, const float x)
  {warning("set_sfvtx_vtx");}

  //! set 3-vector swapped FVTX momentum
  virtual void set_sfvtx_p(const unsigned int itrk, const size_t coord, const float p)
  {warning("set_sfvtx_p");}

  //! set VTX+FVTX hit pattern
  //! NA NA NA NA VTX4 VTX3 VTX2 VTX1 FVTX4_1 FVTX_4_0 FVTX3_1 FVTX3_0 FVTX2_1 FVTX2_0 FVTX1_1 FVTX1_0
  virtual void set_fvtx_hits(const unsigned int itrk, unsigned short a)
  {warning("set_fvtx_hits");}

  //! set 3-vector swapped FVTX+MuTr refit vertex position of the track
  virtual void set_sfvtxmutr_vtx(const unsigned int itrk, const size_t coord, const float x)
  {warning("set_sfvtxmutr_vtx");}

  //! set 3-vector swapped FVTX+MuTr momentum
  virtual void set_sfvtxmutr_p(const unsigned int itrk, const size_t coord, const float p)
  {warning("set_sfvtxmutr_p");}

  //! set chi2/NDF swapped FVTX+MuTr momentum
  virtual void set_sfvtxmutr_chi2(const unsigned int itrk, const float a)
  {warning("set_sfvtxmutr_chi2");}
  
  //! set phi residual btw. MuTr and swapped FVTX track
  virtual void set_sfvtx_dphi(const unsigned int itrk, const float dphi)
  {warning("set_sfvtx_dphi");}

  //! set theta residual btw. MuTr and swapped FVTX track
  virtual void set_sfvtx_dtheta(const unsigned int itrk, const float dtheta)
  {warning("set_sfvtx_dtheta");}

  //! set radius residual btw. MuTr and swapped FVTX track at Z=+/-40 cm
  virtual void set_sfvtx_dr(const unsigned int itrk, const float dr)
  {warning("set_sfvtx_dr");}

  //! set chi^2 for MuTr+ swapped FVTX track
  virtual void set_sfvtx_chi2(const unsigned int itrk, const float chi2)
  {warning("set_sfvtx_chi2");}
  
  //! set covariant matrix of swapped FVTX track
  virtual void set_sfvtx_cov(const unsigned int itrk, const size_t i, const size_t j, const float cov)
  {warning("set_sfvtx_cov");}
  
  //! set covariant matrix of swapped FVTX+MuTr track
  virtual void set_sfvtxmutr_cov(const unsigned int itrk, const size_t i, const size_t j, const float cov)
  {warning("set_sfvtxmutr_cov");}
  
  //@}

  /*
  Following accessors and modifiers are for hidden (that is: non-persistent)
  varibles. Variables are set, and used, at run-time. They are not stored into
  the output MWG root file.
  The default implementation overwrites other 'persistent' variables in the same way
  as previous DimuonMixer implementation was doing. 
  This way one ensures that the code reproduces the same behavior whatever the underlying
  version of PHMuoTrack is used.
  */
  
  //!@name association to muid level2 primitives
  //@{
 
  //! number of associated primitives
  virtual unsigned int get_n_primitives( const unsigned int itrk ) const;

  //! set number of primitives
  virtual void set_n_primitives( const unsigned int itrk, const unsigned int& value );
  
  //! associated primitive level2 angle
  virtual double get_level2_phi( const unsigned int itrk, const unsigned int& i ) const;

  //! associated primitive level2 angle
  virtual void set_level2_phi( const unsigned int itrk, const unsigned int&i, const double& value );
  
  //! associated primitive level2 angle
  virtual double get_level2_theta( const unsigned int itrk, const unsigned int& i ) const;

  //! associated primitive level2 angle
  virtual void set_level2_theta( const unsigned int itrk, const unsigned int& i, const double& value );
  
  //@}
  
  //!@name association to mutr level2 primitives
  //@{
 
  //! number of associated primitives
  virtual unsigned int get_n_mutr_primitives( const unsigned int itrk ) const;
  
  //! set number of primitives
  virtual void set_n_mutr_primitives( const unsigned int itrk, const unsigned int& value );

  //! associated level2 primitive min momentum
  virtual void set_level2_pmin_x( const unsigned int itrk, const unsigned int& i, const double& value );

  //! associated level2 primitive min momentum
  virtual void set_level2_pmin_y( const unsigned int itrk, const unsigned int& i, const double& value );

  //! associated level2 primitive min momentum
  virtual void set_level2_pmin_z( const unsigned int itrk, const unsigned int& i, const double& value );

  //! associated level2 primitive min momentum
  virtual double get_level2_pmin_x( const unsigned int itrk, const unsigned int& i ) const;

  //! associated level2 primitive min momentum
  virtual double get_level2_pmin_y( const unsigned int itrk, const unsigned int& i ) const;

  //! associated level2 primitive min momentum
  virtual double get_level2_pmin_z( const unsigned int itrk, const unsigned int& i ) const;
  
  //! associated level2 primitive max momentum
  virtual void set_level2_pmax_x( const unsigned int itrk, const unsigned int& i, const double& value );

  //! associated level2 primitive max momentum
  virtual void set_level2_pmax_y( const unsigned int itrk, const unsigned int& i, const double& value );

  //! associated level2 primitive max momentum
  virtual void set_level2_pmax_z( const unsigned int itrk, const unsigned int& i, const double& value );

  //! associated level2 primitive max momentum
  virtual double get_level2_pmax_x( const unsigned int itrk, const unsigned int& i ) const;

  //! associated level2 primitive max momentum
  virtual double get_level2_pmax_y( const unsigned int itrk, const unsigned int& i ) const;

  //! associated level2 primitive max momentum
  virtual double get_level2_pmax_z( const unsigned int itrk, const unsigned int& i ) const;
  
  //@}
  
  //!@name event vertex information
  //@{
 
  //! event vertex z
  /*! 
  it needs to be stored on a track by track basis in order to be able
  to fit together tracks that do not belong to the same event, when 
  performing event mixing
  */
  virtual double get_event_vertex_z( const unsigned int itrk ) const;
  
  //! event vertex z error
  /*! 
  it needs to be stored on a track by track basis in order to be able
  to fit together tracks that do not belong to the same event, when 
  performing event mixing
  */
  virtual double get_event_vertex_z_error( const unsigned int itrk ) const;
  
  //! event vertex z
  virtual void set_event_vertex_z( const unsigned int itrk, const double& value );

  //! event vertex z error
  virtual void set_event_vertex_z_error( const unsigned int itrk, const double& value );
  
  //@}  

  //!@name event reaction plane information
  //@{
  
  //! event reaction plane
  /*! 
  it needs to be stored on a track by track basis in order to be able
  to calculate the "average reaction plane angle for muons that do not belong to the same event
  */
  virtual double get_event_rp_angle( const unsigned int itrk, const unsigned int& arm ) const;
 
  //! event reaction plane
  /*! 
  it needs to be stored on a track by track basis in order to be able
  to calculate the "average reaction plane angle for muons that do not belong to the same event
  */
  virtual void set_event_rp_angle( const unsigned int itrk, const unsigned int& arm, const double& value );

  //@}

  
  //================================= PHParticle cloning/copying etc...
  virtual TObject* GetSingleParticle(unsigned int ipart);
  virtual TObject* GetSingleDimuon(unsigned int ipart);
  virtual void AddPHParticle(unsigned int ipart, TObject *);
  virtual void AddPHDimuon(unsigned int ipart, TObject *);
  virtual PHMuoTracksOut* clone() const;

  virtual void ShutUp(const int i = 1);
  
  private:
  void warning(const char* what) const;
  
  ClassDef(PHMuoTracksOut,4)
};
#endif  
