#ifndef __PHDIMUOTRACKV1_H
#define __PHDIMUOTRACKV1_H
#include "PHObject.h"

/** Single Dimuon Candidate Container

This class contains information about each dimuon candidate. This 
information is stored in the $diMuoTracks$ container 
located in the $PHMuoTracks$ branch within the output rootuple. 
*/
class PHdiMuoTrackv1 : public TObject
{
 public:

    enum{/// number of momenta and positions measurement points (currently 4).
	_maxpoints=4};

    // constructor/destructor
    PHdiMuoTrackv1();
    ~PHdiMuoTrackv1(){};

    void identify(std::ostream& os = std::cout) const;

    // Accessors
    /**@name Access methods

    */
    //@{
    /// Return {\bf mass}.
    float get_mass() const{return mass;}
    /// Return {\bf charge}.
    short get_charge() const{return charge;} 
    /// Return {\bf px[arrayid]}.
    float get_px(short arrayid) const;
    /// Return {\bf py[arrayid]}.
    float get_py(short arrayid) const;
    /// Return {\bf pz[arrayid]}.
    float get_pz(short arrayid) const;
    /// Return {\bf trkIndex[arrayid]}.
    int get_trkIndex(short arrayid) const;
    //@}

    // Mutators
    void set_mass(float newVal){mass = newVal;}
    void set_charge(short newVal){charge = newVal;}
    void set_px(short arrayid, float newVal);
    void set_py(short arrayid, float newVal);
    void set_pz(short arrayid, float newVal);
    void set_trkIndex(short arrayid, int newVal);

 private:
    /**@name Variables

     */
    //@{
    /// Invariant mass of muon pair candidate.
    float mass;
    /// Charge of muon pair candidate (1 = $\mu^+\mu^+$, -1 = $\mu^-\mu^-$, 0 = $\mu^+\mu^-$).
    short charge;
    /// $P_x$ of muon pair candidate at (respectively) vertex, station 1, station 2 and station 3.
    float px[_maxpoints];
    /// $P_y$ of muon pair candidate at (respectively) vertex, station 1, station 2 and station 3.
    float py[_maxpoints];
    /// $P_z$ of muon pair candidate at (respectively) vertex, station 1, station 2 and station 3.
    float pz[_maxpoints];
    /// muon track indices within MuoTracks branch.
    int trkIndex[2];
    //@}

ClassDef(PHdiMuoTrackv1,1)	
};
#endif
	
