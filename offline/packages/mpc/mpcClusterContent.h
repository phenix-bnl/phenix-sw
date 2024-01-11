#ifndef __MPCCLUSTERCONTENT_H__
#define __MPCCLUSTERCONTENT_H__

#include "phool.h"
#include "PHObject.h"

/** (ABC) MPC cluster data.

This class is the heart of what you need to do your analysis. 
Bunch of mpcClusterContent objects are bundled into an mpcClusterContainer object (and the container is what you get when reading in DSTs).

@ingroup interface

 */
#include <cmath>

class mpcClusterContent : public PHObject
{
public:

  // CREATORS

  virtual ~mpcClusterContent(){}

  /// Returns a copy of this cluster.
  mpcClusterContent* clone(void) const { warning("clone()"); return 0; }

  /// Return an empty copy of this cluster (i.e. copy only the type)
  mpcClusterContent* create(void) const { warning("create()"); return 0; }

  // ACCESSORS.

  /** Arm number : South=0, North=1
   */
  virtual int arm() const { warning("int arm()"); return -1; }

  /// Whether this cluster can contain both a real and simulated part.
  virtual bool canBeMerged() const { return false; }

  /** Indicative of how close the shower shape is from an electromagnetic
      show. A low chi2 means high probability of being an EM shower.
      Typical chi2 is chi2<3.0
  */
  virtual float chi2() const { warning("float chi2()"); return NAN; }
  virtual float chi2core() const { warning("float chi2core()"); return NAN; }
  virtual int ndfcore() const { warning("int ndfcore()"); return 0; }
  
  /** Corrected dispersions (for simple periodic structure of the disp)
      along y and z axis. Only available if has_yz_cg()=true.
      Dimensionless.
  */
  //@{
  virtual float corrdispy() const { warning("float corrdispy()"); return NAN; }
  // the blow is deprecated
  virtual float corrdispz() const { warning("float corrdispz()"); return NAN; }
  virtual float corrdispx() const { warning("float corrdispx()"); return NAN; }

  virtual float corrlogdispy() const { warning("float corrlogdispy()"); return NAN; }
  // the blow is deprecated
  virtual float corrlogdispz() const { warning("float corrlogdispz()"); return NAN; }
  virtual float corrlogdispx() const { warning("float corrlogdispx()"); return NAN; }
  //@}

  /// Might be used to associate some flag value to this cluster.
  virtual unsigned int cutword() const { warning("unsigned int cutword()"); return 0; }

  /** A 29 bits integer indicating the error status (both w.r.t energy and tof)
      of the cluster towers. It's relative to the central tower, i.e the
      deadmap is the one of the central tower and indicates the error status
      of the neighbours of the central tower.
      For energy, the bits are:
      // ---------------------
      // |   | 18| 19| 20|   |
      // ---------------------
      // | 13| 14| 15| 16| 17|
      // ---------------------  ^ y
      // | 8 | 9 | 10| 11| 12|  |
      // ---------------------  |
      // | 3 | 4 | 5 | 6 | 7 |  |
      // ---------------------  ------> z(x)
      // |   | 0 | 1 | 2 |   |
      // ---------------------
      // as viewed from the back of the central tower (which has bit 10 set
      // to 1 if it's itself a bad module); corner towers are excluded
  
      For ToF bits are :
      // -------------
      // | 27| 28| 29|  ^ y
      // -------------  |
      // | 24| 25| 26|  |
      // -------------  |
      // | 21| 22| 23|  ------> z(x)
      // -------------
      // as viewed from the back of the central tower (which has bit 25 set
      // to 1 if it's itself a bad module)
  
      So, the central tower of the cluster has a problem with 
      amplitude measurements if deamap satisfies the 0x400 mask.

      Some other useful masks.
      The mask to look for amplitude errors or warnings in the 3x3 region
      around the central tower is:
      0x1ce70
      In the 5x5 region:
      0x1fffff
      To see if there are ToF problems for the central tower:
      0x2000000
  */
  virtual unsigned int deadmap() const { warning("unsigned int deadmap()"); return 0; }

  /** Dispersions (second moment of the tower positions distribution within
      the cluster) along y and z axis. Unit is cm^2.
  */
  //@{
  virtual float dispy() const { warning("float dispy()"); return NAN; }
  // the below is deprecated
  virtual float dispz() const { warning("float dispz()"); return NAN; }
  virtual float dispx() const { warning("float dispx()"); return NAN; }

  virtual float logdispy() const { warning("float logdispy()"); return NAN; }
  // the below is deprecated
  virtual float logdispz() const { warning("float logdispz()"); return NAN; }
  virtual float logdispx() const { warning("float logdispx()"); return NAN; }
  //@}

  /** Errors on position x,y,z of the cluster. Unit cm.*/
  //@{
  virtual float dx() const { warning("float dx()"); return NAN; }
  virtual float dy() const { warning("float dy()"); return NAN; }
  virtual float dz() const { warning("float dz()"); return NAN; }
  //@}

  // Measured energy in GeV.
  virtual float e() const { warning("float e()"); return NAN; }

  // Measured energy (GeV) in a 3x3 array around central tower.
  virtual float e9() const { warning("float e9()"); return NAN; }

  // used to be called ecorecorr for PbSc and ecorr for PbGl. GeV.
  virtual float ecore() const { warning("float ecore()"); return NAN; }

  // energy of the central tower. GeV.
  virtual float ecent() const { warning("float ecent()"); return NAN; }

  //@{
  /** Energy of the tower that has the max (min) tof. GeV.*/
  virtual float etofmin() const { warning("float etofmin()"); return NAN; }
  virtual float etofmax() const { warning("float etofmax()"); return NAN; }
  //@}

  /** whether we do have cg (center of gravity) coordinates or not.
      If we do, then we *must* also provide corrdispy,z
   */
  virtual bool has_yz_cg() const { return false; }

  /* List of nDST variables taken out in pDST
     (see offline/analysis/Run4HardpDST/containers/PhPhotonSngl.h) */
  virtual bool has_id() const { return true; }
  virtual bool has_pid() const { return true; }
//  virtual bool has_type() const { return true; }
  virtual bool has_Dxyz() const { return true; }
  virtual bool has_E9() const { return true; }
  virtual bool has_Etofmin() const { return true; }
  virtual bool has_Etofmax() const { return true; }
  virtual bool has_Quality() const { return true; }
  virtual bool has_Phi() const { return true; }
  virtual bool has_Theta() const { return true; }
  virtual bool has_Tofdisp() const { return true; }
  virtual bool has_Tofmin() const { return true; }
  virtual bool has_Tofmax() const { return true; }
  virtual bool has_Tofcorrmin() const { return true; }
  virtual bool has_Tofcorrmax() const { return true; }
   

  /// Cluster identifier.
  virtual int id() const { warning("int id()"); return -1; }

  virtual void identify(std::ostream& os=std::cout) const;

  virtual int isValid() const;
  // generic check for implemented methods
  int isValid(const float f) const;
  int isValid(const int i) const;
  int isValid(const unsigned int i) const;

  /** Whether this cluster is an embedded one (i.e. has some energy
      from real towers and from simulated towers).
  */
  virtual bool isMerged() const { return (simfrac()>0 && simfrac()<1.0); }

  /** Whether this cluster is a simulated one (i.e. simfrac()==1.0)
   */
  virtual bool isSimulated() const { return (simfrac()==1.0); }

  /** Integer position within the sector of the central tower. 
      y=0..35 and z=0..71 for PbSc,
      y=0..47 and z=0..96 for PbGl
  */
  //@{
  virtual int ixpos() const { warning("int ixpos()"); return -1; }
  virtual int iypos() const { warning("int iypos()"); return -1; }

  virtual int ixhitpos() const { warning("int ixhitpos()"); return -1; }
  virtual int iyhitpos() const { warning("int iyhitpos()"); return -1; }
  //@}

  ///
  virtual float quality() const { warning("float quality()"); return NAN; }

  /// Number of towers in this cluster.
  virtual int multiplicity() const { warning("multiplicity()"); return -1; }

  /** Dispersions along the principal axis of the clusters. Unit cm^2 */
  //{@
  virtual float padispy() const { warning("float padispy()"); return NAN; }
  // the below is deprecated
  virtual float padispz() const { warning("padispz()"); return NAN; }
  virtual float padispx() const { warning("padispx()"); return NAN; }
  //@}

  /** Partial sum of energy in the index-th tower.
      partesum(i) = sum_over_j(E_j/Ecluster) where j=0..i
      index ranges from 0 to multiplicity().
      Dimensionless.
  */
  virtual float partesum(int index) const { warning("float partesum(int index)"); return NAN; }

  /// Probability that this cluster is a photon.
  virtual float prob_photon() const { warning("float prob_photon()"); return NAN; }

  /// Phi angle of this cluster. Radians.
  virtual float phi() const { warning("float phi()"); return NAN; }

  ///
  virtual int pid() const { warning("int pid()"); return -1; }

  virtual void print(std::ostream& out=std::cout) const;

  virtual float rawtdc() const { warning("float rawtdc()"); return NAN; }

  /** Sector number (ranging from 0 to 3). Offline convention.
     SECTOR  PARM(*) ARM(**) SECTOR
     W0       1       0         0     
     W1       1       0         1
     W2       1       0         2
     W3       1       0         3

     E3       0       1         3
     E2       0       1         2
     E1       0       1         1
     E0       0       1         0

     (*) arm in PHENIX convention
     (**) arm in EMCAL wrong convention (e.g. return by the arm()
     method of this very object).
  virtual int sector() const { warning("int sector()"); return -1; }
  */

  /** Fraction of energy in this cluster that comes from simulated towers.
   */
  virtual float simfrac() const { warning("float simfrac()"); return NAN; }

  /** TOF of the central tower, minus the flash time (=sqrt(x2+y2+z2)/30.0)).
      Unit ns.
  */
  virtual float tof() const { warning("float tof()"); return NAN; }

  /// Same as above (tof()) for the moment.
  virtual float tofcorr() const { warning("float tofcorr()"); return NAN; }

  /** TOF, assuming cluster originate from a hadron
   */
  virtual float tofhad() const { warning("float tofhad()"); return NAN; }

  /// Dispersion of the TOF. Not implemented yet.
  virtual float tofdisp() const { warning(""); return NAN; }

  /** Minimun and maximum tofs of the clusters. Unit ns.
   */
  //@{
  virtual float tofmin() const { warning("float tofmin()"); return NAN; }
  virtual float tofmax() const { warning("float tofmax()"); return NAN; }
  virtual float tofcorrmin() const { warning("float tofcorrmin()"); return NAN; }
  virtual float tofcorrmax() const { warning("float tofcorrmax()"); return NAN; }
  //@}

  /// Theta angle of the cluster. Radians.
  virtual float theta() const { warning("float theta()"); return NAN; }

  /** towerid of the index-th tower of the cluster.
      towerid is the same as fee channel.
      index goes from 0 to multiplicity()-1.
      This towerid can be used to recover full tower information using
      the mpcTowerContainer object (if available in the xDST you're reading)
      and its findTower(towerid) method.
   */
  virtual int towerid(int index) const { warning("int towerid(int index)"); return -1; }

  /** For backward compatibility reason only. Multiplicity used to be called
      twrhit in old STAF tables.
  int twrhit() const { return multiplicity(); }
  */

  /// Detector type. 1 = PbSc, 2 = PbGl.
  //virtual int type() const { warning("int type()"); return -1; }

  /// Similar to deadmap, but for warning status.
  virtual unsigned int warnmap() const { warning("unsigned int warnmap()"); return 0; }

  /** Position x,y,z of the cluster. Unit cm.
   */
  //@{
  virtual float x() const { warning("float x()"); return NAN; }
  virtual float y() const { warning("float y()"); return NAN; }
  virtual float z() const { warning("float z()"); return NAN; }
  //@}

  /** Center-of-gravity coordinates (in sector plane). Only
      available if has_yz_cg()=true.
      Unit cm.
  */
  //@{
  virtual float ycg() const { warning("float ycg()"); return NAN; }
  virtual float zcg() const { warning("float zcg()"); return NAN; }
  virtual float xcg() const { warning("float xcg()"); return NAN; }
  //@}

  //v2 additions
  virtual float logycg() const { warning("float logycg()"); return NAN; }
  virtual float logxcg() const { warning("float logzcg()"); return NAN; }
  virtual float logy() const { warning("float logy()"); return NAN; }
  virtual float logx() const { warning("float logz()"); return NAN; }

  virtual float linycg() const { warning("float linycg()"); return NAN; }
  virtual float linxcg() const { warning("float linzcg()"); return NAN; }
  virtual float liny() const { warning("float liny()"); return NAN; }
  virtual float linx() const { warning("float linz()"); return NAN; }

  virtual float ycgmin() const { warning("float ycgmin()"); return NAN; }
  virtual float xcgmin() const { warning("float xcgmin()"); return NAN; }
  virtual float ymin() const { warning("float ymin()"); return NAN; }
  virtual float xmin() const { warning("float xmin()"); return NAN; }

  
  virtual bool split() const { warning("bool split()"); return 0; }
  virtual float chi2_split() const { warning("float chi2_split()"); return NAN; }
  virtual float y1() const {warning("float y1()"); return NAN; }
  virtual float x1() const {warning("float x1()"); return NAN; }
  virtual float y2() const {warning("float y2()"); return NAN; }
  virtual float x2() const {warning("float x2()"); return NAN; }
  virtual float e1() const {warning("float e1()"); return NAN; }
  virtual float e2() const {warning("float e2()"); return NAN; }



  //  Extra variables for vetoes of charged particles...
  // ********** needs to be implemented for bbc..... *********


  /// MUTATORS.

  /** Various setters to set the above values.
   */
  //@{
  virtual void set_arm(int) { warning("set_arm(int)"); }
  virtual void set_chi2(float) { warning("set_chi2(float)"); }
  virtual void set_chi2core(float) { warning("set_chi2core(float)"); }
  virtual void set_ndfcore(int) { warning("set_ndfcore(int)"); }
  virtual void set_corrdisp(float corrdispy, float corrdispx) { warning("set_corrdisp(float corrdispy, float corrdispx)"); }
  virtual void set_corrlogdisp(float corrdispy, float corrdispx) { warning("set_corrlogdisp(float corrdispy, float corrdispx)"); }
  virtual void set_cutword(unsigned int) { warning("set_cutword(unsigned int)"); }
  virtual void set_disp(float dispy, float _dispx) { warning(" void set_disp(float dispy, float _dispx)"); }
  virtual void set_logdisp(float dispy, float _dispx) { warning(" void set_logdisp(float dispy, float _dispx)"); }
  virtual void set_dxyz(float dx, float dy, float dz) 
  { warning("void set_dxyz(float dx, float dy, float dz) "); }
  virtual void set_e(float) { warning("void set_e(float)"); }
  virtual void set_e9(float) { warning("void set_e9(float)"); }
  virtual void set_ecore(float) { warning("set_ecore(float)"); }
  virtual void set_ecent(float) { warning("set_ecent(float)"); }
  virtual void set_etofmin(float) { warning("set_etofmin(float)"); }
  virtual void set_etofmax(float) { warning("set_etofmax(float)"); }
  virtual void set_id(int id) { warning("set_id(int id)"); }
  virtual void set_ipos(int ix, int iy)  { warning("set_ipos(int ix, int iy)"); }
  virtual void set_ihitpos(int ix, int iy)  { warning("set_ihitpos(int ix, int iy)"); }
  virtual void set_quality(float) { warning("set_quality(float)"); }
  virtual void set_maps(unsigned int deadmap, unsigned int warnmap)
  { warning("set_maps(unsigned int deadmap, unsigned int warnmap)"); }
  virtual void set_multiplicity(int) { warning("set_multiplicity(int)"); }
  virtual void set_padisp(float padispy, float padispx) 
  { warning("set_padisp(float padispy, float padispx)"); }
  virtual void set_partesum(int index, float value) { warning("set_partesum(int index, float value)"); }
  virtual void set_prob_photon(float) { warning("set_prob_photon(float)"); }
  virtual void set_phi(float) { warning("set_phi(float)"); }
  virtual void set_pid(int) { warning("set_pid(int)"); }
  virtual void set_rawtdc(float) { warning("set_rawtdc(float)"); }
//  virtual void set_sector(int) { warning("set_sector(int)"); }
  virtual void set_simfrac(float) { warning("set_simfrac(float)"); }
  virtual void set_tof(float) { warning("set_tof(float)"); }
  virtual void set_tofhad(float) { warning("set_tofhad(float) "); }
  virtual void set_tofdisp(float) { warning("set_tofdisp(float)"); }
  virtual void set_tofmin(float) { warning("set_tofmin(float)"); }
  virtual void set_tofmax(float) { warning("set_tofmax(float)"); }
  virtual void set_tofcorr(float) { warning("set_tofcorr(float)"); }
  virtual void set_tofcorrmin(float) { warning("set_tofcorrmin(float)"); }
  virtual void set_tofcorrmax(float) { warning("set_tofcorrmax(float)"); }
  virtual void set_theta(float) { warning("set_theta(float)"); }
  virtual void set_towerid(int index, int value) { warning("set_towerid(int index, int value)"); }
//  virtual void set_type(int) { warning("set_type(int)"); }
  virtual void set_xyz(float x, float y, float z) { warning("set_xyz(float x, float y, float z)"); }

  virtual void set_yz_cg(float ycg, float zcg)   { warning("set_yz_cg(float ycg, float zcg)"); }
  virtual void set_yx_cg(float ycg, float xcg)   { warning("set_yx_cg(float ycg, float xcg)"); }
  
  virtual void set_yx_logcg(float ycg, float xcg)   { warning("set_yx_logcg(float ycg, float xcg)"); }
  virtual void set_yx_log(float y, float x)   { warning("set_yx_log(float y, float x)"); }

  virtual void set_yx_lincg(float ycg, float xcg)   { warning("set_yx_lincg(float ycg, float xcg)"); }
  virtual void set_yx_lin(float y, float x)   { warning("set_yx_lin(float y, float x)"); }

  virtual void set_yx_cgmin(float ycg, float xcg)   { warning("set_yx_cgmin(float ycg, float xcg)"); }
  virtual void set_yx_min(float y, float x)   { warning("set_yx_min(float y, float x)"); }



  virtual void set_yx1(float y, float x) { warning("set_yx1(float y, float x)"); }
  virtual void set_yx2(float y, float x) { warning("set_yx2(float y, float x)"); }
  virtual void set_e12(float e1, float e2) { warning("set_e12(float e1, float e2)"); }
  virtual void set_split(bool is_split) { warning("set_split(bool is_split)");};
  virtual void set_chi2_split(float chi2) { warning("set_chi2_split(float chi2)");}
  

  virtual void set_sim_size(int size)  { warning("set_sim_size()"); }
  virtual void set_sim_type(short type)  { warning("set_sim_type()"); }
  virtual void set_sim(int index, int it, int id, int pri, float edep, 
		       float frac, int type, int node,
		       int parenttype, int parentid)  { warning("set_sim()"); }

  virtual int get_sim_size()  const{ warning("get_sim_size()");return -9999; }
  virtual short get_sim_type()  const{ warning("get_sim_type()");return -9999; }
  virtual void get_sim(int index, int& it, int& id, int& pri,float& edep,
		       float& frac, int& type, int& node, 
		       int& parenttype, int& parentid) const{ warning("get_sim()"); }
  

  //  Extra variables for vetoes of charged particles...
  //  ************** needs to be implemented for bbc **************

  //@}
  void ShutUp(const int i = 1);
  // this should be a generic copy content of any mpcClusterContent to any mpcClusterContent
  virtual void Copy(const mpcClusterContent &src);
  virtual void Copy(TObject&) const { PHOOL_VIRTUAL_WARNING; }

protected:
  /** This one is protected on purpose. 
      See e.g. Scott Meyers' More Effective C++ Item 33.*/
  mpcClusterContent& operator=(const mpcClusterContent& rhs);

private: 
  void warning(const char* field) const;

  ClassDef(mpcClusterContent,0) // MPC Cluster data (ABC)
    
};

inline 
std::ostream& operator<<(std::ostream& out, const mpcClusterContent& cc)
{
  cc.print(out); 
  return out;
}

#endif	// __MPCCLUSTERCONTENT_H__
