#ifndef __PHDIMUOTRACKV3_H
#define __PHDIMUOTRACKV3_H
#include "PHObject.h"

/** Single Dimuon Candidate Container

This class contains information about each dimuon candidate. This 
information is stored in the $diMuoTracks$ container 
located in the $PHMuoTracks$ branch within the output rootuple. 
*/
class PHdiMuoTrackv3 : public TObject
{
 public:

    enum{/// number of momenta and positions measurement points (currently 4).
	_maxpoints=4};

    // constructor/destructor
    PHdiMuoTrackv3();
    ~PHdiMuoTrackv3(){};

    void identify(std::ostream& os = std::cout) const;

    // Accessors
    /**@name Access methods

    */
    //@{
    /// Return {\bf mass}.
    float get_mass() const{return mass;}
    /// Return {\bf charge}.
    short get_charge() const{return charge;} 
    /// Return {\bf px}.
    float get_px() const{return px;}
    /// Return {\bf py}.
    float get_py() const{return py;}
    /// Return {\bf pz}.
    float get_pz() const{return pz;}
    /// Return {\bf trkIndex[arrayid]}.
    int get_trkIndex(short arrayid) const;
    /// Return {\bf vtx_xpos}.
    float get_vtx_xpos() const { return vtx_xpos;}
    /// Return {\bf vtx_ypos}.
    float get_vtx_ypos() const { return vtx_ypos;}
    /// Return {\bf vtx_zpos}.
    float get_vtx_zpos() const { return vtx_zpos;} 
    /// Return {\bf vtx_chrg_1}.
    float get_vtx_chrg_1() const { return vtx_chrg_1;}
    /// Return {\bf vtx_px_1}.
    float get_vtx_px_1() const { return vtx_px_1;}
    /// Return {\bf vtx_py_1}.
    float get_vtx_py_1() const { return vtx_py_1;}
    /// Return {\bf vtx_pz_1}.
    float get_vtx_pz_1() const { return vtx_pz_1;}
    /// Return {\bf vtx_chrg_2}.
    float get_vtx_chrg_2() const { return vtx_chrg_2;}
    /// Return {\bf vtx_px_2}.
    float get_vtx_px_2() const { return vtx_px_2;}
    /// Return {\bf vtx_py_2}.
    float get_vtx_py_2() const { return vtx_py_2;}
    /// Return {\bf vtx_pz_2}.
    float get_vtx_pz_2() const { return vtx_pz_2;}
    /// Return {\bf vtx_chisquare}.
    float get_vtx_chisquare() const {return vtx_chisquare;}
    /// Return {\bf vtx_cov[arrayid1,arrayid2]}.
    float get_vtx_cov(short arrayid1, short arrayid2) const;
    //@}

    // Mutators
    void set_mass(float newVal){mass = newVal;}
    void set_charge(short newVal){charge = newVal;}
    void set_px(float newVal){px = newVal;}
    void set_py(float newVal){py = newVal;}
    void set_pz(float newVal){pz = newVal;}
    void set_trkIndex(short arrayid, int newVal);
    void set_vtx_xpos(float newVal) {vtx_xpos = newVal;}
    void set_vtx_ypos(float newVal) {vtx_ypos = newVal;}
    void set_vtx_zpos(float newVal) {vtx_zpos = newVal;}
    void set_vtx_chrg_1(float newVal) {vtx_chrg_1 = newVal;}
    void set_vtx_px_1(float newVal) {vtx_px_1 = newVal;}
    void set_vtx_py_1(float newVal) {vtx_py_1 = newVal;}
    void set_vtx_pz_1(float newVal) {vtx_pz_1 = newVal;}
    void set_vtx_chrg_2(float newVal) {vtx_chrg_2 = newVal;}
    void set_vtx_px_2(float newVal) {vtx_px_2 = newVal;}
    void set_vtx_py_2(float newVal) {vtx_py_2 = newVal;}
    void set_vtx_pz_2(float newVal) {vtx_pz_2 = newVal;}
    void set_vtx_chisquare(float newVal){vtx_chisquare=newVal;}
    void set_vtx_cov(short arrayid1, short arrayid2, float newVal);

 private:
    /**@name Variables

     */
    //@{
    /// Invariant mass of muon pair candidate.
    float mass;
    /// Charge of muon pair candidate (1 = $\mu^+\mu^+$, -1 = $\mu^-\mu^-$, 0 = $\mu^+\mu^-$).
    short charge;
    /// $P_x$ of muon pair candidate at (respectively) vertex, station 1, station 2 and station 3.
    float px;
    /// $P_y$ of muon pair candidate at (respectively) vertex, station 1, station 2 and station 3.
    float py;
    /// $P_z$ of muon pair candidate at (respectively) vertex, station 1, station 2 and station 3.
    float pz;
    /// muon track indices within MuoTracks branch.
    int trkIndex[2];
    //// vertex x position 
    float vtx_xpos;
    //// vertex x position 
    float vtx_ypos;
    //// vertex x position 
    float vtx_zpos;
    /// charge track 1
    float vtx_chrg_1;
    /// px track 1
    float vtx_px_1;
    /// py track 1
    float vtx_py_1;
    /// pz track 1
    float vtx_pz_1;
    /// charge track 2
    float vtx_chrg_2;
    /// px track 2
    float vtx_px_2;
    /// py track 2
    float vtx_py_2;
    /// pz track 2
    float vtx_pz_2;
    /// chisquare from vertex fit.
    float vtx_chisquare;
    /// covariance matrix from vertex fit.
    float vtx_cov[9][9];
    //@}

ClassDef(PHdiMuoTrackv3,1)	
};
#endif
	
