#ifndef __PHdiMuoTrackv5_H
#define __PHdiMuoTrackv5_H
#include "PHObject.h"

/** Single Dimuon Candidate Container

This class contains information about each dimuon candidate. This 
information is stored in the $diMuoTracks$ container 
located in the $PHMuoTracks$ branch within the output rootuple. 
*/
class PHdiMuoTrackv5 : public TObject
{
 public:

    enum{//! number of momenta and positions measurement points (currently 4).
	_maxpoints=4};

    // constructor/destructor
    PHdiMuoTrackv5();
    virtual ~PHdiMuoTrackv5() 
    {};

    void identify(std::ostream& os = std::cout) const;

    // Accessors
    /**@name Access methods

    */
    //@{
    
    //! unique id
    ULong_t get_uid( void ) const
    { return uid; }
    
    //! Return {\bf mass}.
    float get_mass() const{return mass;}
    //! Return {\bf charge}.
    short get_charge() const{return charge;} 
    //! Return {\bf px}.
    float get_px() const{return px;}
    //! Return {\bf py}.
    float get_py() const{return py;}
    //! Return {\bf pz}.
    float get_pz() const{return pz;}
    //! Return {\bf trkIndex[arrayid]}.
    int get_trkIndex(short arrayid) const;
    
    //! Return {\bf vtx_bp_xpos}.
    float get_vtx_bp_xpos() const 
    { return vtx_bp_xpos;}

    //! Return {\bf vtx_bp_ypos}.
    float get_vtx_bp_ypos() const 
    { return vtx_bp_ypos;}

    //! Return {\bf vtx_bp_zpos}.
    float get_vtx_bp_zpos() const 
    { return vtx_bp_zpos;} 

    //! Return {\bf vtx_bp_dca}.
    float get_vtx_bp_dca() const 
    { return vtx_bp_dca;} 
    
    //! Return {\bf vtx_xpos}.
    float get_vtx_xpos() const 
    { return vtx_xpos;}

    //! Return {\bf vtx_ypos}.
    float get_vtx_ypos() const 
    { return vtx_ypos;}

    //! Return {\bf vtx_zpos}.
    float get_vtx_zpos() const { return vtx_zpos;} 
   
    //! Return {\bf vtx_chrg_1}.
    float get_vtx_chrg_1() const { return vtx_chrg_1;}
    //! Return {\bf vtx_px_1}.
    float get_vtx_px_1() const { return vtx_px_1;}
    //! Return {\bf vtx_py_1}.
    float get_vtx_py_1() const { return vtx_py_1;}
    //! Return {\bf vtx_pz_1}.
    float get_vtx_pz_1() const { return vtx_pz_1;}
    //! Return {\bf vtx_chrg_2}.
    float get_vtx_chrg_2() const { return vtx_chrg_2;}
    //! Return {\bf vtx_px_2}.
    float get_vtx_px_2() const { return vtx_px_2;}
    //! Return {\bf vtx_py_2}.
    float get_vtx_py_2() const { return vtx_py_2;}
    
    //! Return {\bf vtx_pz_2}.
    float get_vtx_pz_2() const { return vtx_pz_2;}
    
    //! Return {\bf vtx_ndf}.
    int get_vtx_ndf() const {return vtx_ndf;}
    
    //! Return {\bf vtx_chisquare}.
    float get_vtx_chisquare() const {return vtx_chisquare;}
    
    //! Return {\bf vtx_cov[arrayid1,arrayid2]}.
    float get_vtx_cov(short arrayid1, short arrayid2) const;
    //@}

    //!@name Mutators
    //@{
    
    //! unique ID
    void set_uid( ULong_t value ) 
    { uid = value; }
    
    void set_mass(float newVal) 
    {mass = newVal;}
    
    void set_charge(short newVal) 
    {charge = newVal;}
    
    void set_px(float newVal) 
    {px = newVal;}
    
    void set_py(float newVal) 
    {py = newVal;}
    
    void set_pz(float newVal) 
    {pz = newVal;}
    
    void set_trkIndex(short arrayid, int newVal);

    void set_vtx_bp_xpos(float newVal) 
    {vtx_bp_xpos = newVal;}
    
    void set_vtx_bp_ypos(float newVal) 
    {vtx_bp_ypos = newVal;}
    
    void set_vtx_bp_zpos(float newVal) 
    {vtx_bp_zpos = newVal;}
    
    void set_vtx_bp_dca(float newVal) 
    {vtx_bp_dca = newVal;}

    void set_vtx_xpos(float newVal) 
    {vtx_xpos = newVal;}
    
    void set_vtx_ypos(float newVal) 
    {vtx_ypos = newVal;}
    
    void set_vtx_zpos(float newVal) 
    {vtx_zpos = newVal;}

    void set_vtx_chrg_1(float newVal) 
    {vtx_chrg_1 = newVal;}
    
    void set_vtx_px_1(float newVal) 
    {vtx_px_1 = newVal;}
    
    void set_vtx_py_1(float newVal) 
    {vtx_py_1 = newVal;}
    
    void set_vtx_pz_1(float newVal) 
    {vtx_pz_1 = newVal;}
    
    void set_vtx_chrg_2(float newVal) 
    {vtx_chrg_2 = newVal;}
    
    void set_vtx_px_2(float newVal) 
    {vtx_px_2 = newVal;}
    
    void set_vtx_py_2(float newVal) 
    {vtx_py_2 = newVal;}
    
    void set_vtx_pz_2(float newVal) 
    {vtx_pz_2 = newVal;}
    
    //! sets vertex number of degrees of freedom
    void set_vtx_ndf(int newVal) 
    {vtx_ndf=newVal;}
    
    //! sets vertex reduced chisquare
    void set_vtx_chisquare(float newVal) 
    {vtx_chisquare=newVal;}
    
    void set_vtx_cov(short arrayid1, short arrayid2, float newVal);

 private:
    /**@name Variables

     */
    //@{
    
    //! unique ID 
    /*! 
    it is used to uniquely identify track in events and 
    is a copy of mutoo vertex TMutVtx::get_obj_key 
    */
    ULong_t uid;
    
    //! Invariant mass of muon pair candidate.
    float mass;
    //! Charge of muon pair candidate (1 = $\mu^+\mu^+$, -1 = $\mu^-\mu^-$, 0 = $\mu^+\mu^-$).
    short charge;
    //! $P_x$ of muon pair candidate at (respectively) vertex, station 1, station 2 and station 3.
    float px;
    //! $P_y$ of muon pair candidate at (respectively) vertex, station 1, station 2 and station 3.
    float py;
    //! $P_z$ of muon pair candidate at (respectively) vertex, station 1, station 2 and station 3.
    float pz;
    //! muon track indices within MuoTracks branch.
    int trkIndex[2];
    
    //! vertex bend plane x position 
    float vtx_bp_xpos;
    
    //! vertex bend plane y position 
    float vtx_bp_ypos;
    
    //! vertex bend plane z position 
    float vtx_bp_zpos;
    
    //! vertex bend plane dca
    float vtx_bp_dca;
   
    //! vertex fit x position 
    float vtx_xpos;
    
    //! vertex  fit y position 
    float vtx_ypos;
    
    //! vertex fit z position 
    float vtx_zpos;
    
    //! charge track 1
    float vtx_chrg_1;
    //! px track 1
    float vtx_px_1;
    //! py track 1
    float vtx_py_1;
    //! pz track 1
    float vtx_pz_1;
    //! charge track 2
    float vtx_chrg_2;
    //! px track 2
    float vtx_px_2;
    //! py track 2
    float vtx_py_2;
    //! pz track 2
    float vtx_pz_2;
    
    //! vertex number of degrees of freedom
    int vtx_ndf;
    
    //! chisquare from vertex fit.
    float vtx_chisquare;
    
    //! covariance matrix from vertex fit.
    float vtx_cov[9][9];
    //@}

ClassDef(PHdiMuoTrackv5,1)	
};
#endif
	
