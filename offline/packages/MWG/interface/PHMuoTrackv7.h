#ifndef __PHMuoTrackv7_H
#define __PHMuoTrackv7_H
#include "PHObject.h"

/** Single Muon Track Container

This class contains information about each track found in muon arms. This 
information is stored in the $MuoTracks$ container located in the $PHMuoTracks$
within the output rootuple. 
*/
class PHMuoTrackv7 : public TObject
{
 public:

  enum{
    /// number of momumta and positions measurement points (currently 4).
    _maxpoints=5, //The fith index represents the kalman projection to MuID gap0
      /// Kalman covariance matrix dimension (currently 5 $\times$ 5).
      _covdim=5,
      /// number of muid roads associated with track.
      _maxroads=3};
                

    // constructor/destructor
    PHMuoTrackv7();
    ~PHMuoTrackv7(){};

    void identify(std::ostream& os = std::cout) const;

    // Accessors
    /**@name Access methods

    */
    //@{
    /// Return {\bf px[arrayid]}.
    float get_px(short arrayid) const;
    /// Return {\bf py[arrayid]}.
    float get_py(short arrayid) const;
    /// Return {\bf pz[arrayid]}.
    float get_pz(short arrayid) const;
    /// Return {\bf st1_bp_P[arrayid]}.
    float get_st1_bp_P(short arrayid) const;
    /// Return {\bf st1_bp_pos[arrayid]}.
    float get_st1_bp_pos(short arrayid) const;
    /// Return {\bf xpos[arrayid]}.
    float get_xpos(short arrayid) const;
    /// Return {\bf ypos[arrayid]}.
    float get_ypos(short arrayid) const;
    /// Return {\bf zpos[arrayid]}.
    float get_zpos(short arrayid) const;
    /// Return {\bf nhits}.
    short get_nhits() const {return nhits;}  
    /// Return {\bf cov[arrayid1][arrayid2]}.
    float get_cov(short arrayid1, short arrayid2) const ;
    /// Return {\bf charge}.
    int get_charge() const {return charge;}
    /// Return {\bf PID}.
    int get_PID() const {return PID;}
    /// Return {\bf MuonConfidence}.
    float get_MuonConfidence() const {return MuonConfidence;}
    /// Return {\bf PionConfidence}.
    float get_PionConfidence() const {return PionConfidence;}
    
    /// Return {\bf number of degrees of freedom}.
    int get_ndf() const {return ndf;}
    
    /// Return {\bf chisquare}.
    float get_chisquare() const {return chisquare;}
    
    /// Return {\bf ghostflag}.
    float get_ghostflag() const {return ghostflag;}
    /// Return {\bf muTRhits}.
    int get_muTRhits() const {return muTRhits;}
    /// Return road position and direction at gap0 (x,y,z,dxdz,dydz)
    float get_muIDOO_gap0(const short arrayid, const short iroad) const;
    /// Return chi square for each muid road.
    float get_muIDOOchi(const short iroad) const {return muIDOOchi[iroad];}
    /// Return {\bf muIDhits}. 
    int get_muIDOOhits(const short iroad) const {return muIDOOhits[iroad];}
    /// Return {\bf muIDhits}. 
    int get_muIDhits() const {return muIDhits;}
    /// Return road position and direction at gap0 (x,y,z,dxdz,dydz)
    float get_muID_gap0(const short arrayid) const;
    /// return distance of the muID hit from the projected track/muID point
    float get_muID_proj_hit_dist(short gap, short orient, short hit) const;
    short get_muID_proj_hit_size(short gap, short orient, short hit) const;
    /// Return MutOO track status
    int get_TMutTrk_status() const {return TMutTrk_status;}
    //@}

    // Mutators
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
    void set_PID(short newVal) { PID = newVal;}
    void set_MuonConfidence(float newVal){ MuonConfidence = newVal;}
    void set_PionConfidence(float newVal){ PionConfidence = newVal;}
    
    //! changes track number of degrees of freedom
    void set_ndf( int newVal )
    { ndf = newVal; }
    
    //! changes track reduced chi_square
    void set_chisquare(float newVal)
    { chisquare = newVal;}
    
    void set_ghostflag(float newVal){ ghostflag = newVal;}
    void set_muTRhits(int newVal){ muTRhits = newVal;}
    void set_muIDOOchi(const short iroad, float newVal){ muIDOOchi[iroad] = newVal;}
    void set_muIDOOhits(const short iroad, int newVal){ muIDOOhits[iroad] = newVal;}
    void set_muIDOO_gap0(const short arrayid, const short iroad, float newVal);
    void set_muIDhits(int newVal){ muIDhits = newVal;}
    void set_muID_gap0(const short arrayid, float newVal);
    void set_muID_proj_hit_dist(short gap, short orient, short hit, float newVal);
    void set_muID_proj_hit_size(short gap, short orient, short hit, short newVal);
    void set_TMutTrk_status(int newVal) {TMutTrk_status=newVal;}

 private:
    /**@name Variables

     */
    //@{
    /// Track P$_x$ at (respectively) vertex, station 1, station 2 and station 3. 
    float px[_maxpoints];
    /// Track P$_y$ at (respectively) vertex, station 1, station 2 and station 3.
    float py[_maxpoints];
    /// Track P$_z$ at (respectively) vertex, station 1, station 2 and station 3.
    float pz[_maxpoints];
    /// Track X position at (respectively) vertex, station 1, station 2 and station 3.
    float xpos[_maxpoints];
    /// Track Y position at (respectively) vertex, station 1, station 2 and station 3.
    float ypos[_maxpoints];
    /// Track Z position at (respectively) vertex, station 1, station 2 and station 3.
    float zpos[_maxpoints];
    /// Number of hits used to build the track.
    short nhits;
    /// Track charge.
    short charge;
    /// Kalman covariance matrix.
    float cov[_covdim][_covdim];
    /// Particle ID. {\bf Not filled yet.}
    short PID;
    /// Probability to be a Muon. {\bf Not filled yet.}
    float MuonConfidence;
    /// Probability to be a Pion. {\bf Not filled yet.}
    float PionConfidence;
        
    /// track number of degrees of freedom
    int ndf;

    /// reduced $\chi^2$ of fitted track.
    float chisquare;
     
    /// Probability to be a Ghost track (1(0) = is (not) a ghost)
    float ghostflag;
    /// Track Bend Plane momentum at station 1 (P[0]=P$_x$, P[1]=P$_y$, P[2]=P$_z$).
    float st1_bp_P[3];
    /// Track Bend Plane position at station 1 ([0]=$x$, [1]=$y$, [2]=$z$).
    float st1_bp_pos[3];
    /// hitplans.
    int muTRhits;
    /// road chi_squre for MUIOO.
    float muIDOOchi[_maxroads];
    /// road hit description for MUIOO
    int muIDOOhits[_maxroads];
    /// road position and direction at gap0 (x,y,z,dxdz,dydz) for MUIOO
    float muIDOO_gap0[5][_maxroads];
    /// road hit description
    int muIDhits;
    /// road position and direction at gap0 (x,y,z,dxdz,dydz)
    float muID_gap0[5];
    /// muID hits within R~50cm distance from the projected track/muID point
    float muID_proj_hit_dist[5][2][10]; // 5 gaps; 2 orientations (h/v); 10 closest hits
    short muID_proj_hit_size[5][2][10];
    /// MutOO track status
    int TMutTrk_status;
    //@}

ClassDef(PHMuoTrackv7,1)	
};
#endif
	

