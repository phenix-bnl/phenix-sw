// =====================
// FILE: SvxClusterv1.h
// =====================

#ifndef __SVXCLUSTERV1_HH_
#define __SVXCLUSTERV1_HH_

#include "SvxCluster.h"
#include "SvxClusterList.h"

/**
 * Implementation class (ver. 1) of SvxCluster.
 *
 * @date  Created  by V. L. Rykov on 09-Mar-2004
 * @date  Modified by V. L. Rykov on 11-May-2004: Sorting related stuff is added.
 */
class SvxClusterv1 : public SvxCluster
{

 public:

  // Constructor(s) & destructor
  // """""""""""""""""""""""""""
  SvxClusterv1(SvxClusterList* lst = NULL, SvxCluster* cluster = NULL);
  SvxClusterv1(SvxCluster*     cluster                               );
  virtual ~SvxClusterv1()
    {/*std::cout << "SvxClusterv1 object destroyed" << std::endl;*/}

  // The "standard PHObject response" functions...
  // """""""""""""""""""""""""""""""""""""""""""""
  void Reset();
  void identify(std::ostream &os=std::cout) const;

  // Set the data members
  // """"""""""""""""""""
  void set_hitID      (const int val)
    { hitID      =         val; if ( clusterList ) clusterList->unSort() ;}
  void set_svxSection (const int val)
    { svxSection = (short) val; if ( clusterList ) clusterList->unSort() ;}
  void set_layer      (const int val)
    { layer      = (short) val; if ( clusterList ) clusterList->unSort() ;}
  void set_ladder     (const int val)
    { ladder     = (short) val; if ( clusterList ) clusterList->unSort() ;}
  void set_sensor     (const int val)
    { sensor     = (short) val; if ( clusterList ) clusterList->unSort() ;}

  void set_sensorType      (const int val) { sensorType = (short) val ;}
  void set_edgeflag        (const int val) { edgeflag   = (short) val ;}
  void set_adc             (const int ind,              const int val)
    {            adc[ind]   = val ;}
  void      set_xyz_local  (const int ind,            const float val)
    {      xyz_local[ind]   = val ;}
  void set_size_xyz_local  (const int i, const int j, const float val)
    { size_xyz_local[i][j]  = val ;}
  void      set_xyz_global (const int ind,            const float val)
    {      xyz_global[ind]  = val ;}
  void set_size_xyz_global (const int i, const int j, const float val)
    { size_xyz_global[i][j] = val ;}

  // Get the data members
  // """"""""""""""""""""
  int get_sensorType        ()              const { return (int) sensorType ;}
  int get_edgeflag          ()              const { return (int) edgeflag   ;}
  int get_adc               (const int ind) const { return       adc[ind]   ;}
  float       get_xyz_local (const int ind) const { return xyz_local[ind]   ;}
  float  get_size_xyz_local (const int i, const int j) const
    { return  size_xyz_local[i][j] ;}
  float      get_xyz_global (const int ind) const { return xyz_global[ind] ;}
  float get_size_xyz_global (const int i, const int j) const
    { return size_xyz_global[i][j] ;}

  // Sortability
  Int_t Compare(const TObject* svxhit) const { return SvxHit::Compare(svxhit);}
  void  set_ListPointer(SvxClusterList* lst) { clusterList = lst ;}

  // Methods
  // """""""
  virtual SvxHit* Clone()           { return new SvxClusterv1(this); }
  virtual void    Copy(SvxHit* hit);
  void print() const;

 protected:

  // Data member definition
  short sensorType ;      ///< Sensor type (1-9 pixel, 11-19 & 21-29 stipixel)
  short edgeflag   ;      ///< @brief  Bit is set when this cluster is at edge
                          ///<
                          ///< @li ef|1 ... low x;
                          ///< @li ef|2 ... high x;
                          ///< @li ef|4 ... low z;
                          ///< @li ef|8 ... high z;
                          ///< @li ef!16 ... adjacent to dead/hot chan., etc.
  int               adc[2]   ; ///< adc sum for each readout (x & u)
  float       xyz_local[3]   ; ///< local  position
  float  size_xyz_local[3][3]; ///< local  cluster size&orientation (covarinces)
  float      xyz_global[3]   ; ///< global position
  float size_xyz_global[3][3]; ///< global cluster size&orientation (covarinces)
  /// Pointer to the container
  SvxClusterList* clusterList; //! 

  //---
  ClassDef(SvxClusterv1,1)
};
#endif
