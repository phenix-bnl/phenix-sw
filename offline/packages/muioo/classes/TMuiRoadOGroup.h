#ifndef __TMUIROADOGROUP_HH__
#define __TMUIROADOGROUP_HH__

#include <TMuiRoadMapO.h>

#include <cstddef>


class PHPoint;  //forward declaration

/**
 * To Group roads by checking their hit sharing, projection at station 3
 * road direction, etc and then mark the "golden" road in a group
 * 
 *
 * @author Jason Newby \URL{mailto:rjnewby@utk.edu}
 *
 */

class TMuiRoadOGroup
{
public:
  // CONSTRUCTOR/DESTRUCTOR METHODS ===================================

  /// Constructor.
  TMuiRoadOGroup(const short& arm, const float& z, const float& MutrWin, const float& MuidWin);

  /// Copy constructor.
  TMuiRoadOGroup(const TMuiRoadOGroup& source);

  /// Destructor.
  virtual ~TMuiRoadOGroup();

  // MUTATOR METHODS ==================================================
 
  /// set the group index for this road
  void SetGroup(const short& Group);

  /// Attach the given road to this group.
  void AttachRoad(TMuiRoadMapO::pointer road);

  /// Merge roads from another group to this group
  void MergeGroup(TMuiRoadOGroup* sourceGroup);

  /// Remove the given road from this group.
  void RemoveRoad(TMuiRoadMapO::pointer road);

  // Clear the list of roads
  void Clear();

  // ACCESSOR METHODS =================================================

  /// Get the road index for "Golden" road in this group
  size_t GoldenIndex() const;

  /// unique index of of this group
  short Group() const;

/// How many roads in all does this group contain?
  short TotalRoads() const;

  /// Pointer to the Nth road of this group
  TMuiRoadMapO::pointer Road(int roadIndex) const;

  /// Does the road belong to this group.
  bool IsGroup(TMuiRoadMapO::pointer road);

  /// Project a TMutFitPar object to the the specified ZPlane
  PHPoint ProjectToZ(TMutFitPar fit_par, double ZPlane);

private:
  // PRIVATE METHODS ==================================================

  /// Mark the "Golden" road in this group
  void MarkGolden();

  // FIELDS ===========================================================

  float fZPlane;           // The z value of a given plane
  float fMutrWin;          // The window cut in a given z plane
  float fMuidWin;          // The window cut in a gap of MuID

  short fGroup;           // the unique index of this group 
  size_t fGoldenIndex;      // the index of "golden" road in this group 
  float fChi2DOF;           // The reduced chi square of "golden" road.
  short fArm;               // Arm in which the group of roads is.
  short fTotalRoads;        // Total number of roads in the group
  public:
  std::vector<TMuiRoadMapO::pointer> f_pRoads;  // Pointers to attached roads.
};


#endif  /* __TMUIROADOGROUP_HH__ */
