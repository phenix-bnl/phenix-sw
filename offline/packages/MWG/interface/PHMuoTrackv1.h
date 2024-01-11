#ifndef __PHMUOTRACKV1_H
#define __PHMUOTRACKV1_H
#include "PHObject.h"

/** Single Muon Track Container

This class contains information about each track found in muon arms. This 
information is stored in the $MuoTracks$ container located in the $PHMuoTracks$
within the output rootuple. 
*/
class PHMuoTrackv1 : public TObject
{
 public:

    enum{
	/// number of momumta and positions measurement points (currently 4).
	_maxpoints=4, 
	    /// Kalman covariance matrix dimension (currently 5 $\times$ 5).
	    _covdim=5};

    // constructor/destructor
    PHMuoTrackv1();
    ~PHMuoTrackv1(){};

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
    /// Return {\bf chisquare}.
    float get_chisquare() const {return chisquare;}
    /// Return {\bf ghostflag}.
    float get_ghostflag() const {return ghostflag;}
    //@}

    // Mutators
    void set_px(short arrayid, float newVal);
    void set_py(short arrayid, float newVal);
    void set_pz(short arrayid, float newVal);
    void set_st1_bp_P(short arrayid, float newVal);
    void set_xpos(short arrayid, float newVal);
    void set_ypos(short arrayid, float newVal);
    void set_zpos(short arrayid, float newVal);
    void set_nhits(short newVal){ nhits = newVal;}
    void set_cov(short arrayid1, short arrayid2, float newVal);
    void set_charge(short newVal){ charge = newVal;}
    void set_PID(short newVal) { PID = newVal;}
    void set_MuonConfidence(float newVal){ MuonConfidence = newVal;}
    void set_PionConfidence(float newVal){ PionConfidence = newVal;}
    void set_chisquare(float newVal){ chisquare = newVal;}
    void set_ghostflag(float newVal){ ghostflag = newVal;}

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
    /// $\chi^2$ of fitted track.
    float chisquare;
    /// Probability to be a Ghost track (1(0) = is (not) a ghost)
    float ghostflag;
    /// Track Bent Plane momentum at station 1 (P[0]=P$_x$, P[1]=P$_y$, P[2]=P$_z$).
    float st1_bp_P[3];
    //@}

ClassDef(PHMuoTrackv1,1)	
};
#endif
	
