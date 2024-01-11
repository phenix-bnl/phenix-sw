#ifndef __EMC_TYPES_H__
#define __EMC_TYPES_H__





#include <set>





/// this type is used to represent track number through libemc. track numbers
/// <= 0 are reserved for special purposes.
typedef long emc_trkno_t;

/// represents "no track" for example when a track has no parent, than it's 
/// parent track number is set to EMC_NONE_TRKNO.
const static emc_trkno_t EMC_NONE_TRKNO = 0;
/// represents an invalid track.
const static emc_trkno_t EMC_INVALID_TRKNO = -1;
/// represents special track that contains energy deposit from a real particles.
/// for example all deposit from an imported real DST get addigned to this 
/// track.
const static emc_trkno_t EMC_REALDATA_TRKNO = -2;
/// represents special track that contains energy deposit from a simulated 
/// tracks. no further geant info is available on the track (for example because 
/// we don't have the demcGeaTrackContent table available when a simulated DST is 
/// imported).
const static emc_trkno_t EMC_SIMDATA_TRKNO = -3;

/// type for storing list of tower ids
typedef std::set<emc_trkno_t> emc_tracklist_t;

/// determines if a track number belongs to the special track numbers.
/// (for example energy deposits from special tracks, like tracks with 
/// EMC_REALDATA_TRKNO and EMC_SIMDATA_TRKNO can be and must be added during 
/// mergeing). 
///
/// @param x is the trackno in question
/// @returns true if x is track number of a special trackno
inline bool isSpecialTrkno(emc_trkno_t x){ return x <= 0; }

/// determines if a track number represents a simulated track.
///
/// @param x is the track number in question
/// @returns true if x is track number of a simulated track
inline bool isSimulatedTrkno(emc_trkno_t x){ return (x == EMC_SIMDATA_TRKNO) || (x >= 0); }





/// this type is used to represent towerid through libemc. tower
/// ids < 0 are reserved for special purposes.
typedef long emc_towerid_t;

/// -1 represents invaid towerid.
const static emc_towerid_t EMC_INVALID_TOWERID = -1;

/// type for storing list of track numbers
typedef std::set<emc_towerid_t> emc_towerlist_t;

/// determines if a towerid represents a special tower.
///
/// @param x is the towerid in question
/// @returns true is x is id of a special tower
inline bool isSpecialTowerid(emc_towerid_t x){ return x < 0; }





/// this type is used to represent clusterid through libemc. cluster ids
/// < 0 are reserved for slepcial purposes.
typedef long emc_clusterid_t;

/// -1 represents invalid cluseterid, i.e. energy deposits that do not belong
/// to any cluster has their clusterid set to -1.
const static emc_clusterid_t EMC_INVALID_CLUSTERID = -1;

/// type for storing list of cluster ids
typedef std::set<emc_clusterid_t> emc_clusterlist_t;

/// determines if a clusterid represents a special cluster id.
///
/// @param x is the tcluster id in question
/// @returns true if x is id of a special cluster
inline bool isSpecialClusterid(emc_towerid_t x){ return x < 0; }








typedef enum { ARM_WEST = 0, ARM_EAST = 1 } arm_t;
typedef enum { SECTOR_W0 = 0, SECTOR_W1 = 1, SECTOR_W2 = 2, SECTOR_W3 = 3,
	       SECTOR_E2 = 4, SECTOR_E3 = 5, SECTOR_E0 = 6, SECTOR_E1 = 7 } sector_t;
typedef enum { SECTOR_TYPE_PBSC = 1, SECTOR_TYPE_PBGL = 2 } sector_type_t;






#endif /* ! __EMC_TYPES_H__ */

