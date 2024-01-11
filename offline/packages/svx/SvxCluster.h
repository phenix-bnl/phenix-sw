// ==================
// FILE: SvxCluster.h
// ==================

#ifndef __SVXCLUSTER_HH_
#define __SVXCLUSTER_HH_

#include <SvxHit.h>

/**
 * @brief  Temporary(?) implementation of Silicon clusters
 *
 * This will be superceded by additions to offline/packages/gea.
 * @date  Created by V. L. Rykov on 04-Mar-2004
 */
class SvxCluster : public SvxHit
{

 public:
  SvxCluster(SvxCluster* cluster = NULL) : SvxHit(cluster)
    { /*std::cout << "SvxCluster object created" << std::endl;*/ }
  virtual ~SvxCluster()
    { /*std::cout << "SvxCluster object destroyed" << std::endl;*/ }

  // Standard functions of all inheritors of PHObject classes...
  // """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  virtual void identify(std::ostream &os=std::cout) const {
    os << "Identify yourself: virtual SvxCluster object" << std::endl;
  }

  // Set the values in the SvxCluster...
  // """"""""""""""""""""""""""""""""""
  virtual void set_sensorType      (const int val) {PHOOL_VIRTUAL_WARN("set_sensorType");}
  virtual void set_edgeflag        (const int val) {PHOOL_VIRTUAL_WARN("set_adc"       );}
  virtual void set_adc             (const int ind,            const int   val)
    {PHOOL_VIRTUAL_WARN("set_adc"             ) ;}
  virtual void      set_xyz_local  (const int ind,            const float val)
    { PHOOL_VIRTUAL_WARN("set_xyz_local"      ) ;}
  virtual void set_size_xyz_local  (const int i, const int j, const float val)
    { PHOOL_VIRTUAL_WARN("set_size_xyz_local" ) ;}
  virtual void      set_xyz_global (const int ind,            const float val)
    { PHOOL_VIRTUAL_WARN("set_xyz_global"     ) ;}
  virtual void set_size_xyz_global (const int i, const int j, const float val)
    { PHOOL_VIRTUAL_WARN("set_size_xyz_global") ;}
  virtual void set_ambiguous       (const short val) {PHOOL_VIRTUAL_WARN("set_ambiguous");}

  // Get the values from the SvxCluster...
  // """"""""""""""""""""""""""""""""""""
  virtual int get_sensorType() const {PHOOL_VIRTUAL_WARN("get_sensorType"); return -9999;}
  virtual int get_edgeflag  () const {PHOOL_VIRTUAL_WARN("get_edgeflag"  ); return -9999;}
  /**
   * @brief  Get ADC value of a readout number "ind".
   */
  virtual int get_adc               (const int ind           ) const
    { PHOOL_VIRTUAL_WARN("get_adc"            ) ; return -9999     ;}
  /**
   * @brief  Get a local (= inside sensor) position
   *
   * @li ind=0 ... x direction (phi in global coordinate)
   * @li ind=1 ... y direction (sensor depth)
   * @li ind=2 ... z direction (z in global coordinate)
   */
  virtual float       get_xyz_local (const int ind           ) const
    { PHOOL_VIRTUAL_WARN("get_xyz_local"      ) ; return -.9999e10 ;}
  /**
   * @brief  Get a position resolution in local coordinate.
   *
   * @li In case i == j ... i=0/1/2 means x/y/z-direction.
   * @li In case i != j (covariance term) ... values are not set at present.
   */
  virtual float  get_size_xyz_local (const int i, const int j) const
    { PHOOL_VIRTUAL_WARN("get_size_xyz_local" ) ; return 0 ;}
  /**
   * @brief  Get a global (= PHENIX) position
   *
   * @li ind=0 ... x direction
   * @li ind=1 ... y direction
   * @li ind=2 ... z direction
   */
  virtual float      get_xyz_global (const int ind           ) const
    { PHOOL_VIRTUAL_WARN("get_xyz_global"     ) ; return -.9999e10 ;}
  /**
   * @brief  Get a position resolution in global coordinate.
   *
   * @li In case i == j ... i=0/1/2 means x/y/z-direction.
   * @li In case i != j (covariance term) ... values are not set at present.
   */
  virtual float get_size_xyz_global (const int i, const int j) const
    { PHOOL_VIRTUAL_WARN("get_size_xyz_global") ; return -.9999e10 ;}
  /**
  * @brief Get the number of clusters that lie along the same X and U channels
  */
  virtual short get_ambiguous () const { PHOOL_VIRTUAL_WARN("get_ambiguous") ; return 999 ;}
  
  // Methods
  // """""""
  virtual SvxHit* Clone()           { PHOOL_VIRTUAL_WARN("Clone()"); return 0; }
  virtual void    Copy(SvxHit* hit) { PHOOL_VIRTUAL_WARN("Copy()"); }

  virtual short get_size() {PHOOL_VIRTUAL_WARN("get_size()"); return 0;}
  virtual short get_AssociatedCGL() {PHOOL_VIRTUAL_WARN("get_AssociatedCGL()"); return -9999;}
  virtual short get_AssociatedStandalone() {PHOOL_VIRTUAL_WARN("get_AssociatedStandalone()"); return -9999;}
  virtual void set_size(short a) {PHOOL_VIRTUAL_WARN("set_size()");}
  virtual void set_AssociatedCGL(short a) {PHOOL_VIRTUAL_WARN("set_AssociatedCGL()");}
  virtual void set_AssociatedStandalone(short a) {PHOOL_VIRTUAL_WARN("set_AssociatedStandalone()");}
  virtual void  set_xz_size (int i, short a) { PHOOL_VIRTUAL_WARN("set_xz_size"); }
  virtual short get_xz_size (int i) { PHOOL_VIRTUAL_WARN("get_xz_size"); return 0; }
  virtual void  set_circumference (short val) { PHOOL_VIRTUAL_WARN("set_circumference"); }
  virtual short get_circumference ( ) { PHOOL_VIRTUAL_WARN("get_circumference"); return 0; }

  virtual void set_Nhot(const short val) { PHOOL_VIRTUAL_WARN("set_Nhot"); }
  virtual short get_Nhot() const { PHOOL_VIRTUAL_WARN("get_Nhot"); return 0;}
  virtual void set_Ncold(const short val) { PHOOL_VIRTUAL_WARN("set_Ncold"); }
  virtual short get_Ncold() const { PHOOL_VIRTUAL_WARN("get_Ncold"); return 0;}

  /*
   * set_xz_size(i, val), get_xz_size(i)
   * i=0 : cluster size in local-x (x-strip) direction (unit : pixel)
   * i=1 : cluster size in local-z (u-strip) direction (unit : pixel)
   */

  ClassDef(SvxCluster, 1);
};
#endif
