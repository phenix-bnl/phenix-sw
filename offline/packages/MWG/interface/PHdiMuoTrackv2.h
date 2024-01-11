#ifndef __PHDIMUOTRACKV2_H
#define __PHDIMUOTRACKV2_H
#include "PHObject.h"

/** Single Dimuon Candidate Container

This class contains information about each dimuon candidate. This 
information is stored in the $diMuoTracks$ container 
located in the $PHMuoTracks$ branch within the output rootuple. 
*/
class PHdiMuoTrackv2 : public TObject
{
 public:

    enum{/// number of momenta and positions measurement points (currently 4).
	_maxpoints=4};

    // constructor/destructor
    PHdiMuoTrackv2();
    ~PHdiMuoTrackv2(){};

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
    float get_px() const{return px;};
    /// Return {\bf py}.
    float get_py() const{return py;};
    /// Return {\bf pz}.
    float get_pz() const{return pz;};
    /// Return {\bf trkIndex[arrayid]}.
    int get_trkIndex(short arrayid) const;
    //@}

    // Mutators
    void set_mass(float newVal){mass = newVal;}
    void set_charge(short newVal){charge = newVal;}
    void set_px(float newVal){px = newVal;}
    void set_py(float newVal){py = newVal;}
    void set_pz(float newVal){pz = newVal;}
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
    float px;
    /// $P_y$ of muon pair candidate at (respectively) vertex, station 1, station 2 and station 3.
    float py;
    /// $P_z$ of muon pair candidate at (respectively) vertex, station 1, station 2 and station 3.
    float pz;
    /// muon track indices within MuoTracks branch.
    int trkIndex[2];
    //@}

ClassDef(PHdiMuoTrackv2,1)	
};
#endif
	
