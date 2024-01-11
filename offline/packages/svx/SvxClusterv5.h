// =====================
// FILE: SvxClusterv5.h
// =====================

#ifndef __SVXCLUSTERV5_HH_
#define __SVXCLUSTERV5_HH_

#include "SvxCluster.h"
#include "SvxClusterList.h"

/**
 * Implementation class (version 5) of SvxCluster.
 * This is a compact version of SvxCluster
 * It does not have siz_xyz_local and size_xyz_global variables
 * just as version 2, but has additional cluster size and 
 * pointers to matched CGL and standalone tracks.
 *
 * v5 adds number of cold/hot pixels in the cluster. D. McGlinchey 10-11-2013 
 *
 * @date  Created  by Sasha Lebedev <lebedev@iastate.edu> in December 2010
 */
class SvxClusterv5 : public SvxCluster
{

 public:

  // Constructor(s) & destructor
  // """""""""""""""""""""""""""
  SvxClusterv5(SvxClusterList* lst = NULL, SvxCluster* cluster = NULL);
  SvxClusterv5(SvxCluster*     cluster                               );
  virtual ~SvxClusterv5()
    {/*std::cout << "SvxClusterv5 object destroyed" << std::endl;*/}

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
  void set_adc             (const int ind,              const int val) { adc[ind] = val ;}
  void      set_xyz_local  (const int ind,            const float val) { xyz_local[ind] = val ;}
  void      set_xyz_global (const int ind,            const float val) { xyz_global[ind] = val ;}
  void set_ambiguous       (const short val) {ambiguous   =       val ;}
  void set_Nhot            (const short val) {Nhot        =       val ;}
  void set_Ncold           (const short val) {Ncold       =       val ;}


  // Get the data members
  // """"""""""""""""""""
  int get_sensorType        ()              const { return (int) sensorType ;}
  int get_edgeflag          ()              const { return (int) edgeflag   ;}
  int get_adc               (const int ind) const { return       adc[ind]   ;}
  float       get_xyz_local (const int ind) const { return xyz_local[ind]   ;}
  float      get_xyz_global (const int ind) const { return xyz_global[ind]  ;}
  short      get_ambiguous  ()              const { return ambiguous        ;}
  short get_Nhot            ()              const { return Nhot             ;}
  short get_Ncold           ()              const { return Ncold            ;}

  // Sortability
  Int_t Compare(const TObject* svxhit) const { return SvxHit::Compare(svxhit);}
  void  set_ListPointer(SvxClusterList* lst) { clusterList = lst ;}

  /* 
   * they are not used now (akimoto 1/25/2011)
   *
   * void set_size_xyz_local  (const int i, const int j, const float val) { size_xyz_local[i][j] = val; }
   * void set_size_xyz_global (const int i, const int j, const float val) { size_xyz_global[i][j] = val; }
   * float get_size_xyz_local (const int i, const int j) const { return  0.9999e4;}
   * float get_size_xyz_global (const int i, const int j) const { return 0.9999e4;}
   */

  // Methods
  // """""""
  virtual SvxHit* Clone()           { return new SvxClusterv5(this); }
  virtual void    Copy(SvxHit* hit);
  void print() const;

  short get_size() { return size;}
  short get_xz_size (int i)            { return xz_size[i]; }
  short get_circumference( ){ return circumference; }
  short get_AssociatedCGL() {return AssociatedCGL;}
  short get_AssociatedStandalone() {return AssociatedStandalone;}
  void set_size(short a) {size=a;}
  void set_xz_size (int i, short val) { xz_size[i] = val; }
  void set_circumference (short val) { circumference = val; }
  void set_AssociatedCGL(short a) {AssociatedCGL=a;}
  void set_AssociatedStandalone(short a) {AssociatedStandalone=a;}

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
  float      xyz_global[3]   ; ///< global position
  /// Pointer to the container
  SvxClusterList* clusterList; //! 

  short size;  // number of raw hits in this cluster
  short circumference;
  short xz_size[2]   ; ///< cluster size in local x & z direction (unit : pixel)
  short AssociatedCGL;  // pointer to associated global track
  short AssociatedStandalone; // pointer to associated standalone track
  short ambiguous; //How many total clusters are found along the x and u channels in the cluster.
                   //==0 -> unique cluster, otherwise value is total number of clusters.

  //new to v5
  short Nhot;  //the number of pixels with status=SVX_PIXEL_HOT
  short Ncold; //the number of pixels with status=SVX_PIXEL_COLD

  ClassDef(SvxClusterv5,1)

};
#endif

