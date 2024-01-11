// Class: cglHitList
// Created by: Jeffery T. Mitchell
// 
// Description: Makes list of detector hits for hit association
//              cglHitList methods allow you to construct it from
//              a detector's hit DST information.  You can then 
//              sort the hit list if desired, or return hit or hits
//              in proximity to a given input coordinate.
//
// Special note:  When using this class, take care to key off of the
//                index data member rather than the list entry number.
//                Doing this will insure that you write robust code
//                since some methods will move members of the list around
//                for you.
//
// Updated to use proper EMC clusters, not obsolete STAF tables...
//                             TKH 2-26-2003
//
// Updated to be able to use new EMC clusters. L. Aphecetche (LA) 2-27-2003 
//
// Devolved to semi-obsolete STAF tables when newest EMC clusters not found.
//                             TKH 2-28-2003
//
#ifndef __CGLHITLIST_HH__
#define __CGLHITLIST_HH__

class dDchTracksWrapper;
class dPadClusterWrapper;
class SvxCluster;
class SvxClusterList;
class TecOutV1;
class dTofReconstructedWrapper;
class dEmcClusterLocalExtWrapper;  // TKH
class emcClusterContainer; 
class PHAngle;
class AccRaw;
class AccGeometry;
class TofwHit;
class HbdBlobList;

#include <algorithm>
#include <vector>

#include <PHPoint.h>
#include <PHCylPoint.h>

class cglHitList { 

public:

  // Default constructor
  cglHitList();

  // Copy constructor
  cglHitList(const cglHitList &);
  // operator =
  cglHitList& operator= (const cglHitList&);
  // Constructor from drift chamber tracks
  cglHitList(short,dDchTracksWrapper *);

  // Constructor from PC1, PC2, or PC3 clusters
  // Choose the pad chamber with the first input (0,1, or 2)
  cglHitList(short,short,dPadClusterWrapper *);

  // Constructor from time expansion chamber tracks
  cglHitList(short,TecOutV1 *);

  // Constructor from time-of-flight hits
  cglHitList(short,dTofReconstructedWrapper *);

  // Constructor from EMCAL clusters
  // Choose the EMCAL type with the first input parameter
  //   (0=PbSc, 1=PbGl, 2=All)
  cglHitList(short,short,dEmcClusterLocalExtWrapper *);  // TKH

  // Constructor from EMCAL clusters (new version)
  // Choose the EMCAL type with the first input parameter
  //   (0=PbSc, 1=PbGl, 2=All)
  cglHitList(short,short,emcClusterContainer*);


  //constructor from acc hits
  cglHitList(short,AccRaw *,AccGeometry *);
  //constructor from tofw hits
  cglHitList(short,TofwHit *);

  // Constructor from SVX clusters
  // Choose the svx chamber with the first input (0)
  cglHitList(short,short,SvxClusterList *);

  //constructor from hbd blobs
  cglHitList(short,HbdBlobList *);

  // Destructor
  virtual ~cglHitList() {}
  
public:

  // Data member access methods
  void set_n(long sn) {n = sn;}   // Set the number of hits in the list
  long get_n() const {return n;}   // Get the number of hits in the list
  void set_detid(short did) {detid = did;}    // Set the detector ID
  short get_detid() const {return detid;}     // Get the detector ID
  void set_arm(short sarm) {arm = sarm;}      // Set the arm number
  short get_arm() const {return arm;}         // Get the arm number
  void set_index(long,long);       // Set an index number
  long get_index(long);            // Get an index number
  void set_Verbose(short);         // Set Verbose
  short get_Verbose();             // Get Verbose
  long get_nFromIndex(long);       // Given an index, return its place in list
  void set_coord(long,const PHPoint &p);    // Set a cartesian coordinate
  PHPoint get_coord(long);         // Get a cartesian coordinate

  // The coordinates are stored as PHPoints, but these methods will allow
  // you to work with cylindrical coordinates in the list.
  void set_cyl(long,const PHCylPoint &p); // Set a cylindrical coordinate
  PHCylPoint get_cyl(long);        // Get a cylindrical coordinate

  // Print the list with cartesian coordinate output
  void Print();

  // Print the list with cylindrical coordinate output
  void PrintCyl();

  // Sort the list in order of increasing phi coordinate
  void SortInPhi();

  // Sort the list in order of increasing z coordinate
  void SortInZ();

  // Remove a specific element from the list
  void Remove(long);

  // Clear all elements from the list
  void Clear();

  // Add a specific element to the list
  void Add(long,long,PHPoint &p);

  // Append an element to the list
  void Append(long,PHPoint &p);

  // Fetch all indices that are within the specified z range
  // The returned value is the number of matches.
  long ZRange(double, double, std::vector<long>&);

  // Fetch all indices that are within the specified phi range
  // The returned value is the number of matches.
  long PhiRange(PHAngle &pLo, PHAngle &pHi, std::vector<long>&);

  // Fetch all indices that are within the specifiec z,phi window
  // The returned value is the number of matches.
  long PhiZRange(PHAngle &pLo, PHAngle &pHi, double, double, std::vector<long>&);

  // Return the index of the closest hit in phi to the input point.
  // The input parameters can define a phi and z window.  
  // If they are equal, then there is no window criteria applied.
  long PhiClose(const PHPoint &p, PHAngle &pLo, PHAngle &pHi, 
		double, double);

  // Return the index of the closest hit in z to the input point.
  // The input parameters can define a phi and z window.  If they are equal,
  // then there is no window criteria applied.
  long ZClose(const PHPoint &p, PHAngle &pLo, PHAngle &pHi, 
	      double, double);

  // Return the index of the closest hit to the center of the window
  // in phi and z.  Again, if the lo and hi values for each coordinate
  // are equal, then there is no restriction on the range in that
  // coordinate.
  long PhiZClose(const PHPoint &p, PHAngle &pLo, PHAngle &pHi, 
		 double, double);

private:
  void initHitList();
  // The number of entries in the list
  long n;

  // Flag for the type of sorting applied to the list
  // 0 = no sort, 1 = increasing phi sort, 2 = increasing z sort
  short sortflag;

  // Detector type identifier ....
  // these are off by 1 from the definition in the track models
  // where we start from vtx , being 0
  // 1 = DCH, 2 = PC1, 3 = PC2, 4 = PC3, 5 = CRK, 6 = TEC,
  // 7 = TOF, 8 = PbSc, 9 = PbGl , 10 = Tzr, 11 = Pcr 
  // 12 = ACC, 13 = TOFW, 14~17 = SVX, 18 = HBD
 
  short detid;

  // The arm number of the hits in the list.
  // Arm number 2 indicates both arms
  // Arm number -1 indicates that the arm number is not specified
  short arm;

  // The array of DST indices.
  std::vector<long> index;

  // The array of hit coordinates for easy access.
  std::vector<PHPoint> coord;

  // Verbosity level.
  short Verbose;
}; 

#endif /* __CGLHITLIST_HH__ */
