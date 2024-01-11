#ifndef __SVXPIXEL1V1_H__
#define __SVXPIXEL1V1_H__

// ===============================
/// \file SvxPixel1v1.h
/// \brief Pixel sensor class description & clustering
/// \author Michael P. McCumber (under the svx documentation effort)
// ===============================

// standard includes
#include <iostream>

// svx includes
#include "SvxStripixel.h"
#include "SvxCluster.h"
#include "SvxClusterList.h"
#include "SvxRawhit.h"
#include "SvxRawhitCluster.h"
#include "SvxRawhitClusterList.h"
#include "SvxGhitCluster.h"
#include "SvxGhitClusterList.h"

// forward declarations
class svxAddress;
class SvxRawhitAdapter;
struct pixel_cluster;

/// \class SvxPixel1v1
/// \brief Pixel sensor class description & clustering
///
/// This is a class for the pixel sensors, it inherits
/// from the stripixel sensor for some historical reason.
/// It appears that the template argument sets which type
/// of sensor is used (channel dimensions and such).
///
class SvxPixel1v1 : public SvxStripixel<1> {

 public:

  SvxPixel1v1(const int sc=-1,
	      const int lr=-1,
	      const int ld=-1,
	      const int sn=-1
	      );
  virtual ~SvxPixel1v1() {};

  /// print object details via PHObject method call
  virtual void identify(std::ostream &os=std::cout) const {
    os << "Identify yourself: SvxPixel1v1 object: isValid() = "
       << isValid() << std::endl;
  }

  /// clears object
  void Reset(); 

  /// pixel cluster creation
  virtual int findClusters(SvxRawhitList*        rawlist,
			   SvxClusterList*       clslist,
			   SvxRawhitClusterList* raw2cls = NULL,
			   SvxGhitRawhitList*    ght2raw = NULL,
			   SvxGhitClusterList*   ght2cls = NULL,
                           svxAddress*           address = NULL);

  /// \todo removal candidate (fixed return only)
  /// also, noise should be added in SvxSimulator not in Reco!
  int AddNoise(const int rhit_first, 
	       const int rhit_last, 
	       SvxRawhitList* rawlist, 
               SvxGhitRawhitList* g2raw, int g2rFirst) const {
     return 0;
  };

 protected:

  typedef std::vector<SvxRawhit*> RawhitGroup_t;
  typedef std::vector<RawhitGroup_t> RawhitGroupList_t;

  /// deprecated virtual method call from SvxStripixel inheritance
  virtual int makeMosaic(RawhitGroupList_t**   grp_list,
			 SvxClusterList*       clslist,
			 SvxRawhitClusterList* raw2cls = NULL,
			 SvxGhitRawhitList*    ght2raw = NULL,
			 SvxGhitClusterList*   ght2cls = NULL,
                         svxAddress* address = NULL
			 ) {
    std::cout << "SvxPixel1v1::makeMosaic"
	      << " has been deprecated/removed, ERROR ERROR ERROR, do not call!"
	      << std::endl; return 0;
  }

  /// deprecated virtual method call from SvxStripixel inheritance
  virtual int makeMosaicSensor(std::multimap<int,SvxRawhitAdapter>* groups,
			       SvxClusterList* clslist,
			       SvxRawhitClusterList* raw2cls = 0, 
			       SvxGhitRawhitList* ght2raw = 0,
			       SvxGhitClusterList* ght2cls = 0
			       ) {
    std::cout << "SvxPixel1v1::makeMosaicSensor"
	      << " has been deprecated/removed, ERROR ERROR ERROR, do not call!"
	      << std::endl; return 0;
  }

  /// set hits by location into internal storage
  virtual void set_hitmap_hit(int x, int z) {
    m_hitmap[0][x][z] = m_HitDefVal;
    ++m_hitcount;
    is_HitColumn[z] = true;
  };

  /// set hit ids by location into internal storage
  virtual void set_hitmap_rawhitID(int x, int z, int rawhitID) {
    m_hitmap[1][x][z] = rawhitID;
  };

  /// instumented channel cut-off_set flag
  /// true if chanCutOff[][] has been set
  static bool cut_off_set_flag; //! omit from ROOT IO, why?

private:

  /// recursive call to add adjacent pixels to cluster
  void growPixelCluster(pixel_cluster &cluster, int icluster, int x, int z);

  /// return the local x position from the sensor ix value
  double get_sensorXpos(const int section, const int readout, int ix) const;

  /// return the local x position from the sensor dx value
  double get_sensorXpos(const int section, const int readout, double dx) const;

  /// return the local z position from the sensor iz value
  double get_sensorZpos(const int section, const int readout, int iz) const;

  /// return the local z position from the sensor dz value
  double get_sensorZpos(const int section, const int readout, double dz) const;

  /// defines an unclustered hit
  static const int m_HitDefVal = -1;

  /// defines an pixel without signal
  static const int m_unHitDefVal = -2;

  /// number of pixels in the x-dimension on a sensor
  static const int NX = 256;

  /// number of pixels in the z-dimension on a sensor
  static const int NZ = 128; // 128 = 32columns * 4chips

  /// records if there is any hit in the z-column (speed optimization)
  bool is_HitColumn[NZ];

  /// holds the hit information on a given sensor
  /// first level [0][][] holds m_unHitDefVal,m_HitDefVal, or cluster id
  /// second level [1][][] holds rawhit ids for ancestry tracing
  int m_hitmap[2][NX][NZ];
  
  /// total hit count within sensor
  int m_hitcount;
  
  ClassDef(SvxPixel1v1,1)
};

#endif // __SVXPIXEL1V1_H__
