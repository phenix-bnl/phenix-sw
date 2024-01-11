// Class: cglHitList (implementation)
// Created by: Jeffery T. Mitchell
// Description: Class that lists hits from a given detector.  The list
//              explicitly includes pointers to the DST information and
//              the coordinate information.
//
//  Changed to new EMC objects (not STAF tables).
//                          TKH 2-26-2003
//
// Adapted to new (latest) EMC objects LA 2-27-2003
//
// Devolved to semi-obsolete STAF when latest EMC not found.
//                          TKH 2-28-2003

#include <iostream>
#include <cmath>

#include <cglHitList.hh>
#include <dDchTracksWrapper.h>
#include <dPadClusterWrapper.h>
#include <SvxCluster.h>
#include <SvxClusterList.h>
#include <TecOutV1.hh>
#include <AccRaw.h>
#include <AccGeometry.h>
#include <TofwHit.h>
#include <HbdBlobList.h>
#include <dTofReconstructedWrapper.h>
#include <dEmcClusterLocalExtWrapper.h>
#include <emcClusterContainer.h>
#include <emcClusterContent.h>

using namespace std;

#define SVXLAYERNUMBER 4

// Default Constructor for cglHitList
cglHitList::cglHitList()
{
  initHitList();
  return;
}

void cglHitList::initHitList()
{
  n = 0;
  sortflag = 0;
  detid = -1;
  arm = -1;

  index.clear();
  coord.clear();

  return;
}

// Copy Constructor for cglHitList
cglHitList::cglHitList(const cglHitList &hitList)
{
  n = hitList.n;
  detid = hitList.detid;
  sortflag = hitList.sortflag;
  arm = hitList.arm;

    for(unsigned int i=0; i<(hitList.index).size(); i++) {
        index.push_back(hitList.index[i]);
        coord.push_back(hitList.coord[i]);
    }

}

// Construct cglHitList for a given arm from drift chamber tracks If
// the arm number is 2, then construct the list out of both arms
cglHitList::cglHitList(short iarm,
                       dDchTracksWrapper* dchTrack)
{
  int jarm;
  long thisindex, narm;
  double xyz[3];

  PHPoint dchPoint;

  // Initialize the hit list
  initHitList();
  detid = 1;
  arm = iarm;

  // Count the number of tracks in the arm
  narm = 0;
  if(dchTrack){
    // Loop over all drift chamber tracks
    for (unsigned int i = 0; i < dchTrack->RowCount(); i++)
      {
	thisindex = i;
	jarm = 0;
	for (int j = 0; j < 3; j++)
	  {
	    xyz[j] = dchTrack->get_point(j, i);
	  }
	if (xyz[0] > 0.0)
	  {
	    jarm = 1;
	  }
	dchPoint.setX(xyz[0]);
	dchPoint.setY(xyz[1]);
	dchPoint.setZ(xyz[2]);
	if (iarm == jarm || iarm == 2)
	  {
		Add(narm, thisindex, dchPoint);
		narm++;
	  }
      }
  }
  // Set the number of entries in the list to the number of hits in
  // the arm
  n = narm;
  return;
}

// Construct cglHitList for a given arm from pad chamber clusters ipc
// is the PC number (0,1, or 2) If the arm number is 2, then construct
// the list out of both arms
cglHitList::cglHitList(short ipc, short iarm,
		       dPadClusterWrapper* padClus)
{
  int jarm;
  double xyz[3];
  long thisindex, narm;

  PHPoint padPoint;

  // Initialize the hit list
  initHitList();
  detid = ipc + 2;
  arm = iarm;

  // Count the number of clusters in the arm
  narm = 0;
  if(padClus){
    // Loop over all pad chamber clusters
    for (unsigned int i = 0; i < padClus->RowCount(); i++)
      {
	thisindex = i;
	jarm = 0;
	for (int j = 0; j < 3; j++)
	  {
	    xyz[j] = padClus->get_xyz(j, i);
	  }
	if (xyz[0] > 0.0)
	  {
	    jarm = 1;
	  }
	padPoint.setX(xyz[0]);
	padPoint.setY(xyz[1]);
	padPoint.setZ(xyz[2]);
	if (iarm == jarm || iarm == 2)
	  {
	      Add(narm, thisindex, padPoint);
	      narm++;
	  }
      }
    
    // Set the number of entries in the list to the number of hits in
    // the arm
  }
  n = narm;
  return;
}

// Construct cglHitList for a given layer from svx chamber clusters 
// ilayer decides the detid
cglHitList::cglHitList(short ilayer, short iarm,
		       SvxClusterList* svxClus)
{
  double xyz[3];
  long thisindex, nlayer;

  PHPoint svxPoint;

  // Initialize the hit list
  initHitList();
  detid = 14 + ilayer;

  // Count the number of clusters in the layer
  nlayer = 0;
  if(svxClus){
    // Loop over all svx chamber clusters
    for (int i = 0; i < svxClus->get_nClusters(); i++)
      {
	thisindex = i;
	for (int j = 0; j < 3; j++)
	  {
	    xyz[j] = svxClus->get_Cluster(i)->get_xyz_global(j);
	  }
	const int jlayer = svxClus->get_Cluster(i)->get_layer();
        short int svxarm = 0;
        if(xyz[0]>0) {svxarm = 1;}

	svxPoint.setX(xyz[0]);
	svxPoint.setY(xyz[1]);
	svxPoint.setZ(xyz[2]);

	if(jlayer==ilayer){
              if(svxarm==iarm || iarm==2) {
	        Add(nlayer, thisindex, svxPoint);
	        nlayer++;
              }
	}
      }
    
    // Set the number of entries in the list to the number of hits in
    // the layer
  }
  n = nlayer;

  return;
}

// Construct cglHitList for a given arm from time expansion chamber
// tracks If the arm number is 2, then construct the list out of both
// arms
cglHitList::cglHitList(short iarm,
                       TecOutV1* tecout)
{
  int jarm;
  long thisindex, narm;
  double xyz[3];

  PHPoint tecPoint;

  // Initialize the hit list
  initHitList();
  detid = 6;
  arm = iarm;
  // Count the number of tracks in the arm
  narm = 0;
  if(tecout){
    // Loop over all TEC tracks
    for (int i = 0; i < tecout->getNTracks(); i++)
      {
	thisindex = i;
	xyz[0] = (double)tecout->getTrackXin(i);
	xyz[1] = (double)tecout->getTrackYin(i);
	xyz[2] = 1.;
	if (tecout->getTrackSide(i) == 0)
	  {
	    xyz[2] = -1.;
	  }
	// Determine the arm number
	jarm = 0;
	if (xyz[0] > 0.0)
	  {
	    jarm = 1;
	  }
	tecPoint.setX(xyz[0]);
	tecPoint.setY(xyz[1]);
	tecPoint.setZ(xyz[2]);
	if (iarm == jarm || iarm == 2)
	  {
		Add(narm, thisindex, tecPoint);
		narm++;
	  }
      }
  }
  // Set the number of entries in the list to the number of hits in
  // the arm

  n = narm;
  return;
}

// Construct cglHitList for a given arm from time-of-flight hits If
// the arm number is 2, then construct the list out of both arms
cglHitList::cglHitList(short iarm,
                       dTofReconstructedWrapper* tofRec)
{
  int jarm;
  long thisindex, narm;
  double xyz[3];

  PHPoint tofPoint;

  // Initialize the hit list
  initHitList();
  detid = 7;
  arm = iarm;

  // Count the number of tracks in the arm
  narm = 0;
  if(tofRec){
    // Loop over all TOF hits
    for (unsigned int i = 0; i < tofRec->RowCount(); i++)
      {
	thisindex = i;
	for (int j = 0; j < 3; j++)
	  {
	    xyz[j] = tofRec->get_xtof(j, i);
	  }
	// Determine the arm number
	jarm = 0;
	if (xyz[0] > 0.0)
	  {
	    jarm = 1;
	  }
	tofPoint.setX(xyz[0]);
	tofPoint.setY(xyz[1]);
	tofPoint.setZ(xyz[2]);
	if (iarm == jarm || iarm == 2)
	  {
		Add(narm, thisindex, tofPoint);
		narm++;
	  }
      }
  }

  // Set the number of entries in the list to the number of hits in
  // the arm
  n = narm;
  return;
}

// Construct cglHitList for a given arm from EMCAL clusters iemc is
// the EMCAL type (0=PbSc,1=PbGl, or 2=All) If the arm number is 2,
// then construct the list out of both arms
cglHitList::cglHitList(short iemc, short iarm,
                       dEmcClusterLocalExtWrapper* emcClus)
{
  int jarm, icont;
  double xyz[3];
  long thisindex, narm;
  short emcType;

  PHPoint emcPoint;

  // Initialize the hit list
  initHitList();
  detid = iemc + 8;
  arm = iarm;

  // Count the number of clusters in the arm
  narm = 0;
  if(emcClus){
    // Loop over all EMCAL chamber clusters

    for (unsigned int i = 0; i < emcClus->RowCount(); i++)
      {
	thisindex = i;
	emcType = emcClus->get_type(i);
	for (int j = 0; j < 3; j++)
	  {
	    xyz[j] = emcClus->get_xyz(j, i);
	  }
	jarm = 0;
	if (xyz[0] > 0.0)
	  {
	    jarm = 1;
	  }
	emcPoint.setX(xyz[0]);
	emcPoint.setY(xyz[1]);
	emcPoint.setZ(xyz[2]);
	if (iarm == jarm || iarm == 2)
	  {
	    icont = 0;
	    if (emcType == 1 && iemc == 0)
	      {
		icont = 1;
	      }
	    if (emcType == 2 && iemc == 1)
	      {
		icont = 1;
	      }
	    if (iemc == 2)
	      {
		icont = 1;
	      }
	    if (icont == 1)
	      {
		    Add(narm, thisindex, emcPoint);
		    narm++;
	      }
	  }
      }
  }
  // Set the number of entries in the list to the number of hits in
  // the arm
  n = narm;
  return;
}

// Same as above but with new EMCAL clusters object.
// Construct cglHitList for a given arm from EMCAL clusters 
// iemc is the EMCAL type (0=PbSc,1=PbGl, or 2=All) 
// If the arm number is 2,
// then construct the list out of both arms
cglHitList::cglHitList(short iemc, short iarm,
                       emcClusterContainer* emcClus)
{
  // Initialize the hit list
  initHitList();
  detid = iemc + 8;
  arm = iarm;

  if (!emcClus) return;

  int jarm, icont;
  double xyz[3];
  long thisindex;
  short emcType;
  long narm = 0;

  PHPoint emcPoint;

  // Loop over all EMCAL clusters
  for (unsigned int i = 0; i < emcClus->size(); i++)
    {
      emcClusterContent* cluster = emcClus->getCluster(i);
      thisindex = i;
      emcType = cluster->type();
      xyz[0] = cluster->x();
      xyz[1] = cluster->y();
      xyz[2] = cluster->z();
      jarm = 0;
      if (xyz[0] > 0.0)
	{
	  jarm = 1;
	}
      emcPoint.setX(xyz[0]);
      emcPoint.setY(xyz[1]);
      emcPoint.setZ(xyz[2]);
      if (iarm == jarm || iarm == 2)
	{
	  icont = 0;
	  if (emcType == 1 && iemc == 0)
	    {
	      icont = 1;
	    }
	  if (emcType == 2 && iemc == 1)
	    {
	      icont = 1;
	    }
	  if (iemc == 2)
	    {
	      icont = 1;
	    }
	  if (icont == 1)
	    {
		  Add(narm, thisindex, emcPoint);
		  narm++;
	    }
	}
    }
  // Set the number of entries in the list to the number of hits in
  // the arm
  n = narm;
}


cglHitList::cglHitList(short iarm,  AccRaw* accRaw, AccGeometry *accGeo )
{
  int jarm;
  long thisindex, narm;
  double xyz[3];

  PHPoint accPoint;

  // Initialize the hit list
  initHitList();
  detid = 12;
  arm = iarm;

  // Count the number of tracks in the arm
  narm = 0;
  if(accRaw){
    // Loop over all Acc hits
    for (int i = 0; i < accRaw->get_nraw(); i++)
      {
	thisindex = i;
	xyz[0] = accGeo->getX(accRaw->get_boxid(i));
	xyz[1] = accGeo->getY(accRaw->get_boxid(i));
	xyz[2] = accGeo->getZ(accRaw->get_boxid(i));
	// Determine the arm number
	jarm = 0;
	if (xyz[0] > 0.0)
	  {
	    jarm = 1;
	  }
	accPoint.setX(xyz[0]);
	accPoint.setY(xyz[1]);
	accPoint.setZ(xyz[2]);
	if (iarm == jarm || iarm == 2)
	  {
		Add(narm, thisindex, accPoint);
		narm++;
	  }
      }
  }
  // Set the number of entries in the list to the number of hits in
  // the arm
  n = narm;
  return;
}

cglHitList::cglHitList(short iarm,  TofwHit* tofwHit)
{
  int jarm;
  long thisindex, narm;
  double xyz[3];

  PHPoint tofwPoint;

  // Initialize the hit list
  initHitList();
  detid = 13;
  arm = iarm;

  // Count the number of tracks in the arm
  narm = 0;
  if(tofwHit){
    // Loop over all Tofw hits
    for (int i = 0; i < tofwHit->get_nhit(); i++)
      {
	thisindex = i;
	int imax = tofwHit->get_max(i);
	xyz[0] = tofwHit->get_xyz(i,imax,0);
	xyz[1] = tofwHit->get_xyz(i,imax,1);
	xyz[2] = tofwHit->get_xyz(i,imax,2);
	// Determine the arm number
	jarm = 0;
	if (xyz[0] > 0.0)
	  {
	    jarm = 1;
	  }
	tofwPoint.setX(xyz[0]);
	tofwPoint.setY(xyz[1]);
	tofwPoint.setZ(xyz[2]);
	if (iarm == jarm || iarm == 2)
	  {
		Add(narm, thisindex, tofwPoint);
		narm++;
	  }
      }
  }
  // Set the number of entries in the list to the number of hits in
  // the arm
  n = narm;
  return;
}


cglHitList::cglHitList(short iarm,  HbdBlobList* hbdBlobList)
{
  int jarm;
  long thisindex, narm;
  double xyz[3];

  PHPoint hbdPoint;

  // Initialize the hit list
  initHitList();
  detid = 18;
  arm = iarm;

  // Count the number of tracks in the arm
  narm = 0;
  if(hbdBlobList){
    // Loop over all hbd blobs
    for (unsigned int i = 0; i < hbdBlobList->get_nBlobs(); i++)
      {
	thisindex = i;
	xyz[0] = hbdBlobList->get_blob(i)->get_blobx();
	xyz[1] = hbdBlobList->get_blob(i)->get_bloby();
	xyz[2] = hbdBlobList->get_blob(i)->get_blobz();
	// Determine the arm number
	jarm = 0;
	if (xyz[0] > 0.0)
	  {
	    jarm = 1;
	  }
	hbdPoint.setX(xyz[0]);
	hbdPoint.setY(xyz[1]);
	hbdPoint.setZ(xyz[2]);
	if (iarm == jarm || iarm == 2)
	  {
		Add(narm, thisindex, hbdPoint);
		narm++;
	  }
      }
  }
  // Set the number of entries in the list to the number of hits in
  // the arm
  n = narm;
  return;
}

// Set the index number of an entry in the list
void 
cglHitList::set_index(long iset, long indset)
{
//  index[iset] = indset;
  if(iset<(long)index.size() && iset>-1) {index[iset]=indset;}

  return;
}

// Get the index number of an entry in the list
long 
cglHitList::get_index(long iget)
{
  return index[iget];
}

// Set the verbosity level
void 
cglHitList::set_Verbose(short vset)
{
  Verbose = vset;
  return;
}

// Get the verbosity level
short 
cglHitList::get_Verbose()
{
  return Verbose;
}

// Given an input index number, return its place in the list This will
// return only the first match to the index number.  Return -1 if
// there is no match.
long 
cglHitList::get_nFromIndex(long indIn)
{
  long indOut, i;
  int itest;

  i = 0;
  itest = 0;
  indOut = -1;
  while (itest == 0 && i < n)
    {
      if (index[i] == indIn)
        {
          itest = 1;
          indOut = i;
        }
      i++;
    }

  return indOut;
}

// Set the cartesian coordinate of an entry in the list
void 
cglHitList::set_coord(long jset, const PHPoint& p)
{
  if(jset<(long)coord.size() && jset>-1) { coord[jset] = p; }
    else { cerr << PHWHERE << "ERROR: index out of range!" << endl;}
}

// Get the cartesian coordinate of an entry in the list
PHPoint 
cglHitList::get_coord(long jget)
{
  PHPoint gcoord;

  gcoord = coord[jget];

  return gcoord;
}

// Set the cylindrical coordinate of an entry in the list
void 
cglHitList::set_cyl(long jset, const PHCylPoint& p)
{
  if(jset<(long)coord.size() && jset>-1) {coord[jset] = p;}
    else { cerr << PHWHERE << "ERROR: index out of range!" << endl;}
}

// Get the cartesian coordinate of an entry in the list
PHCylPoint 
cglHitList::get_cyl(long jget)
{
  PHCylPoint gcoord = coord[jget];

  return gcoord;
}

// Print out the contents of the list
void 
cglHitList::Print()
{
  cout << "cglHitList:\n";
  cout << "  Number of elements: " << n << "\n";
  cout << "  Sort flag = " << sortflag << "\n";
  cout << "  Detector ID = " << detid << "\n";
  cout << "  Arm number = " << arm << "\n";
  for (int i = 0; i < n; i++)
    {
      cout << "   i = " << i << ": ind = " << index[i] << ": ";
      coord[i].print();
    }
}

// Print out the contents of the list in cylindrical coordinates
void 
cglHitList::PrintCyl()
{
  int i;
  PHCylPoint coordCyl;
  PHAngle tempAngle;
  double phiDeg;

  cout << "cglHitList:\n";
  cout << "  Number of elements: " << n << "\n";
  cout << "  Sort flag = " << sortflag << "\n";
  cout << "  Detector ID = " << detid << "\n";
  cout << "  Arm number = " << arm << "\n";
  for (i = 0; i < n; i++)
    {
      coordCyl = coord[i];
      tempAngle = coordCyl.getPhi();
      phiDeg = tempAngle.degree();
      cout << "   i = " << i << ": ind = " << index[i] << ": ";
      cout << "phi = " << phiDeg << ": ";
      coordCyl.print();
    }
}

// Sort the list in order of increasing phi This algorithm uses a
// shell sort from Numerical Recipes
void 
cglHitList::SortInPhi()
{
  int i, j, inc;
  long w;
  double v;
  vector<double> phiCoord;
  PHCylPoint tempPoint;
  PHAngle tempAngle;
  PHPoint vCoord;

  // Extract the cylindrical coordinates for each hit
  for (i = 0; i < n; i++)
    {
      tempPoint = coord[i];
      tempAngle = tempPoint.getPhi();
      phiCoord.push_back(tempAngle);
    }

  // Determine the starting increment
  inc = 1;
  do
    {
      inc *= 3;
      inc++;
    }
  while (inc <= n);

  // Loop over the partial sorts
  do
    {
      inc /= 3;
      // outer loop of straight insertion
      for (i = inc + 1; i <= n; i++)
        {
          v = phiCoord[i - 1];
          vCoord = coord[i - 1];
          w = index[i - 1];
          j = i;
          // inner loop of straight insertion
          while (phiCoord[j - inc - 1] > v)
            {
              phiCoord[j - 1] = phiCoord[j - inc - 1];
              coord[j - 1] = coord[j - inc - 1];
              index[j - 1] = index[j - inc - 1];
              j -= inc;
              if (j <= inc)
                {
		  break;
		}
            }
          phiCoord[j - 1] = v;
          coord[j - 1] = vCoord;
          index[j - 1] = w;
        }
    }
  while (inc > 0);

  // Set the sort flag to show this is phi sorted
  sortflag = 1;
}

// Sort the list in order of increasing z This algorithm uses a shell
// sort from Numerical Recipes
void 
cglHitList::SortInZ()
{
  int i, j, inc;
  long w;
  double v;
  vector<double> zCoord;
  PHPoint vCoord;

  // Extract the z only for each hit
  for (i = 0; i < n; i++)
    {
      zCoord.push_back(coord[i].getZ());
    }

  // Determine the starting increment
  inc = 1;
  do
    {
      inc *= 3;
      inc++;
    }
  while (inc <= n);

  // Loop over the partial sorts
  do
    {
      inc /= 3;
      // outer loop of straight insertion
      for (i = inc + 1; i <= n; i++)
        {
          v = zCoord[i - 1];
          vCoord = coord[i - 1];
          w = index[i - 1];
          j = i;
          // inner loop of straight insertion
          while (zCoord[j - inc - 1] > v)
            {
              zCoord[j - 1] = zCoord[j - inc - 1];
              coord[j - 1] = coord[j - inc - 1];
              index[j - 1] = index[j - inc - 1];
              j -= inc;
              if (j <= inc)
                {
		  break;
		}
            }
          zCoord[j - 1] = v;
          coord[j - 1] = vCoord;
          index[j - 1] = w;
        }
    }
  while (inc > 0);

  // Set the sort flag to show this is z sorted
  sortflag = 2;
}

// Remove an element from the list If iout is 0 or less than 0, remove
// the first item in the list.  If iout is greater than n, remove the
// last item in the list.
void 
cglHitList::Remove(long iout)
{
  // Check for out-of-bounds.
  if (iout < 0) 
    {
      iout = 0;
    }
  else if (iout > n)
    {
      iout = n;
    }

  // Make sure there is something in the list to remove.
  if (n <= 0)
    {
      return;
    }

  // Bump up the entries in the list
  for (int i = iout; i < n - 1; i++)
    {
      index[i] = index[i + 1];
      coord[i] = coord[i + 1];
    }

  // Reset the back of the list
  index[n - 1] = 0;
  coord[n - 1].setX(0.0);
  coord[n - 1].setY(0.0);
  coord[n - 1].setZ(0.0);

  // We now have one less entry in the list
  n--;

  // Reset the sort flag to show this is no longer sorted
  sortflag = 0;
}

// Clear all elements from the list
void 
cglHitList::Clear()
{
//  for (int i = 0; i < n; i++)
//    {
//      index[i] = -1;
//      coord[i].setX(0.0);
//      coord[i].setY(0.0);
//      coord[i].setZ(0.0);
//    }
  index.clear();
  coord.clear();

  n = 0;
  sortflag = 0;
  detid = -1;
  arm = -1;
}

// Add an element to the list To add an element to the start of the
// list, feed iin as a negative number.  To add an element to the end
// of the list, feed iin >= n.
void 
cglHitList::Add(long iin, long indadd, PHPoint& xadd)
{
  // Check for out-of-bounds.  Set the input index to the proper range
  // if it is to add to beginning or end of the list.
  if (iin < 0) 
    {
      iin = 0;
    }
  else if (iin > n)
    {
      iin = n;
    }

  index.push_back(indadd);
  coord.push_back(xadd);

  // Start at the back of the list and move everything down
  for (int i = n - 1; i >= iin; i--)
    {
      index[i + 1] = index[i];
      coord[i + 1] = coord[i];
    }

  // Now place the new values in slot iin
  index[iin] = indadd;
  coord[iin] = xadd;

  // Increment the number of entries
  n++;

  // Reset the sort flag to show this is no longer sorted
  sortflag = 0;
}

// Append an element to the list
void 
cglHitList::Append(long indapp, PHPoint& xapp)
{
  index.push_back(indapp);
  coord.push_back(xapp);
  n++;
  sortflag = 0;
}

// Given a z range, this method will return all indices (not list
// entry numbers) within the z range, inclusive.  The list will be
// sorted in z to perform this operation.  The returned value will be
// the number of hits in the z range.
long 
cglHitList::ZRange( double zLo,
                         double zHi,
                         vector<long> &indOut)
{
  int i, itest, indTemp;
  long nOut = 0;  // the return value
  long zLoInd, zHiInd;
  vector<double> r;
  vector<double> z;
  vector<PHAngle> phi;
  PHCylPoint tempPoint;
  PHAngle tempAngle;

  // Check that the range is valid
  if (zLo >= zHi)
    {
      return nOut;
    }

  // Sort it in z if not already done
  if (sortflag != 2)
    {
      SortInZ();
    }

  // Extract the cylindrical coordinates for each hit
  for (i = 0; i < n; i++)
    {
      tempPoint = coord[i];
      tempAngle = tempPoint.getPhi();
      r.push_back(tempPoint.getR());
      phi.push_back(tempAngle);
      z.push_back(tempPoint.getZ());
    }

  // Look for the low z index within the input z range
  indTemp = 0;
  i = 0;
  itest = 0;
  while (i < n && itest == 0)
    {
      if (z[i] >= zLo)
        {
          itest = 1;
          indTemp = i;
        }
      i++;
    }
  if (itest == 0)
    {
      return nOut;  // This should never be executed, in theory
    }
  // Set the low index
  zLoInd = indTemp;

  // Before moving to the high index, make sure that the input z range
  // does not lie between 2 points in the list.  If it does, return -1
  // index.
  if (z[zLoInd] > zHi)
    {
      return nOut;
    }

  // Look for the high z index
  indTemp = n;
  i = zLoInd;
  itest = 0;
  while (i < n && itest == 0)
    {
      if (z[i] >= zHi)
        {
          itest = 1;
          indTemp = i;
        }
      i++;
    }
  // Set the high index
  if (itest == 1)
    {
      zHiInd = indTemp - 1;
    }
  else
    {
      zHiInd = n - 1;
    }
  // Finally, look at the hits in the z window.  Count the number of
  // hits and put the index numbers in indOut.
  nOut = 0;
  for (i = zLoInd; i <= zHiInd; i++)
    {
      //indOut[nOut] = index[i];
      indOut.push_back(index[i]);
      nOut++;
    }

  return nOut;
}

// Given a phi range, this method will return all indices (not list
// entry numbers) within the phi range, inclusive.  The list will be
// sorted in phi to perform this operation.  The returned value will
// be the number of hits in the phi range.
long 
cglHitList::PhiRange( PHAngle& phiLoIn,
                           PHAngle& phiHiIn,
                           vector<long> &indOut)
{
  long nOut = 0;  // the return value
  // if no hits in this detector, return right away
  if (n == 0)
    {
      return nOut;
    }
  int i, itest, indTemp;
  long phiLoInd, phiHiInd;
  vector<double> r;
  vector<double> z;
  vector<PHAngle> phi;
  PHCylPoint tempPoint;
  PHAngle tempAngle;

  // Sort it in phi if not already done
  if (sortflag != 1)
    {
      SortInPhi();
    }
  // Extract the cylindrical coordinates for each hit
  for (i = 0; i < n; i++)
    {
      tempPoint = coord[i];
      tempAngle = tempPoint.getPhi();
      r.push_back(tempPoint.getR());
      phi.push_back(tempAngle);
      z.push_back(tempPoint.getZ());
    }

  // Look for the low phi index within the input phi range
  indTemp = 0;
  i = 0;
  itest = 0;
  while (i < n && itest == 0)
    {
      if (phi[i] >= phiLoIn)
        {
          itest = 1;
          indTemp = i;
        }
      i++;
    }
  // Set the low index
  if (itest == 1)
    {
      phiLoInd = indTemp;
    }
  else
    {
      phiLoInd = 0;
    }
  // Look for the high phi index
  indTemp = 0;
  i = 0;
  itest = 0;
  while (i < n && itest == 0)
    {
      if (phi[i] >= phiHiIn)
        {
          itest = 1;
          indTemp = i;
        }
      i++;
    }
  // Set the high index
  if (itest == 1)
    {
      if (phi[0] > phiHiIn)
        {
          phiHiInd = n - 1;
        }
      else
        {
          phiHiInd = indTemp - 1;
        }
    }
  else
    {
      phiHiInd = n - 1;
    }
  // Make sure that the input phi range does not lie between 2 points
  // in the list.  If it does, return -1 index.
  indTemp = phiHiInd + 1;
  if (phiLoInd == indTemp && phiLoIn < phiHiIn)
    {
      return nOut;
    }
  if (phiLoInd == 0 && phiHiInd == n - 1)
    {
      if (phiLoIn < phi[0] && phiHiIn < phi[0])
        {
	  return nOut;
	}
      if (phiLoIn > phi[n - 1] && phiHiIn > phi[n - 1])
        {
	  return nOut;
	}
      if (phiLoIn > phiHiIn)
        {
	  return nOut;
	}
    }

  // Finally, look at the hits in the phi window.  Count the number of
  // hits and put the index numbers in indOut.
  nOut = 0;
  i = phiLoInd;
  itest = 0;
  while (itest == 0)
    {
      //indOut[nOut] = index[i];
      indOut.push_back(index[i]);
      nOut++;
      if (i == phiHiInd)
        {
	  itest = 1;
	}
      i++;
      if (i >= n)
        {
	  i = 0;
	}
    }

  return nOut;
}

// Given a phi and z range, this method will return all indices (not
// list entry numbers) within the window, inclusive.  The list will be
// sorted in phi to perform this operation.  The returned value will
// be the number of hits in the phi,z window.
long 
cglHitList::PhiZRange( PHAngle& phiLoIn, PHAngle& phiHiIn,
                            double zLo, double zHi,
                            vector<long> &indOut)
{
  long nOut = 0;  // the return value
  long iTemp, indTemp;
  double zCheck;
  long nPhi;
  vector<long> iPhi;

  // Check that the z range is valid
  if (zLo >= zHi)
    {
      return nOut;
    }

  // Fetch the list of indices that are within the phi range.
  nPhi = PhiRange(phiLoIn, phiHiIn, iPhi);

  // Loop over hits in the phi range and check the z boundaries
  nOut = 0;
  for (int i = 0; i < nPhi; i++)
    {
      indTemp = iPhi[i];
      iTemp = get_nFromIndex(indTemp);
      zCheck = coord[iTemp].getZ();
      if (zCheck >= zLo && zCheck <= zHi)
        {
          //indOut[nOut] = indTemp;
          indOut.push_back(indTemp);
          nOut++;
        }
    }

  return nOut;
}

// Given a z coordinate, and a phi range, this will return the index
// of the closest z coordinate within range.  No test on phi closeness
// is made.  If there is nothing in range, a -1 is returned.
long 
cglHitList::ZClose(const PHPoint& zPoint,
                        PHAngle& phiLoIn, PHAngle& phiHiIn,
                        double zLo, double zHi)
{
  PHAngle phi;
  short iRange;
  PHCylPoint tempPoint;
  double z, zCoord, zDiff, zDiffClose;
  long IndOut = -1, nPhi = 0, iTemp, iTempInd;
  vector<long> iPhi;

  // Fetch the input coordinates
  tempPoint = zPoint;
  phi = tempPoint.getPhi();
  z = zPoint.getZ();

  // Check that the input phi coordinate is in the phi range
  if (phi < phiLoIn || phi > phiHiIn)
    {
      return IndOut;
    }
  if (z < zLo || z > zHi)
    {
      return IndOut;
    }

  // Fetch the hits within the phi range
  nPhi = PhiRange(phiLoIn, phiHiIn, iPhi);

  // Finally, look at the hits in the phi window.  Find the closest
  // one in z within the phi window and return it.  Again, there is no
  // check on the closeness of phi.  The hit just has to be within the
  // phi range.
  zDiffClose = 99999.99;
  IndOut = -1;
  for (int i = 0; i < nPhi; i++)
    {
      iTempInd = iPhi[i];
      iRange = 0;
      iTemp = get_nFromIndex(iTempInd);
      zCoord = coord[iTemp].getZ();
      // Is the coordinate within the z range?
      if (zCoord >= zLo && zCoord <= zHi)
        {
	  iRange = 1;
	}
      zDiff = fabs(zCoord - z);
      if (zDiff < zDiffClose && iRange == 1)
        {
          zDiffClose = zDiff;
          IndOut = iTempInd;
        }
    }

  return IndOut;
}

// Given a phi coordinate, a phi range, and a z range, this will
// return the index of the closest phi coordinate within range.  No
// test on z closeness is made.  If there is nothing in range, a -1 is
// returned.
long 
cglHitList::PhiClose(const PHPoint& phiPoint,
		     PHAngle& phiLoIn, PHAngle& phiHiIn,
		     double zLo, double zHi)
{
  double z;
  short iRange;
  long IndOut = -1, nZ = 0, iTemp, iTempInd;
  vector<long> iZ;
  PHCylPoint tempPoint;
  PHAngle phi, phiCoord, phiDiff, phiDiffClose;

  // Fetch the input z coordinate
  tempPoint = phiPoint;
  phi = tempPoint.getPhi();
  z = phiPoint.getZ();

  // Check that the z range is valid
  if (zLo >= zHi)
    {
      return IndOut;
    }

  // Check that the input z coordinate is in the z range
  if (phi < phiLoIn || phi > phiHiIn)
    {
      return IndOut;
    }
  if (z < zLo || z > zHi)
    {
      return IndOut;
    }

  // Fetch the hits within the z range
  nZ = ZRange(zLo, zHi, iZ);

  // Finally, look at the hits in the z window.  Find the closest one
  // in phi within the phi and z window and return it.  Again, there
  // is no check on the closeness of z.  The hit just has to be within
  // the z range.
  phiDiffClose = phiHiIn - phiLoIn;
  IndOut = -1;
  for (int i = 0; i < nZ; i++)
    {
      iTempInd = iZ[i];
      iRange = 0;
      iTemp = get_nFromIndex(iTempInd);
      tempPoint = coord[iTemp];
      phiCoord = tempPoint.getPhi();
      if (phiCoord >= phiLoIn && phiCoord <= phiHiIn)
        {
	  iRange = 1;
	}
      phiDiff = fabs(phiCoord - phi);
      if (phiDiff < phiDiffClose && iRange == 1)
        {
          phiDiffClose = phiDiff;
          IndOut = iTempInd;
        }
    }
  return IndOut;
}

// Given a coordinate, a phi range, and a z range, this will return
// the index of the closest coordinate within range.  Closeness is
// determined by distance calculated in Cartesian space from the input
// point.  If there is nothing in range, a -1 is returned.

long 
cglHitList::PhiZClose(const PHPoint& rPoint,
		      PHAngle& phiLoIn, PHAngle& phiHiIn,
		      double zLo, double zHi)
{
  double z, xCoord, yCoord, zCoord, x, y, rDiff, rDiffClose;
  long IndOut = -1, nPhi = 0, iTemp, iTempInd;
  vector<long> iPhi;
  PHCylPoint tempPoint;
  PHAngle phi;

  // Fetch the input coordinates
  tempPoint = rPoint;
  phi = tempPoint.getPhi();
  x = rPoint.getX();
  y = rPoint.getY();
  z = rPoint.getZ();
  // Check that the input z range is valid
  if (zLo >= zHi)
    {
      return IndOut;
    }

  // Check that the input coordinate is in the input range
  if (phi < phiLoIn || phi > phiHiIn)
    {
      return IndOut;
    }
  if (z < zLo || z > zHi)
    {
      return IndOut;
    }

  // Fetch the hits within the phi range
  nPhi = PhiRange(phiLoIn, phiHiIn, iPhi);

  // Finally, look at the hits in the phi window.  Find the closest
  // one in distance within the phi and z window and return it.
  rDiffClose = 99999.99;
  IndOut = -1;
  for (int i = 0; i < nPhi; i++)
    {
      iTempInd = iPhi[i];
      iTemp = get_nFromIndex(iTempInd);
      zCoord = coord[iTemp].getZ();

      // Is this coordinate in the z window?
      if (zCoord >= zLo && zCoord <= zHi)
        {
          xCoord = coord[iTemp].getX();
          yCoord = coord[iTemp].getY();
          rDiff = sqrt((x - xCoord) * (x - xCoord) +
                       (y - yCoord) * (y - yCoord) +
                       (z - zCoord) * (z - zCoord));
          if (rDiff < rDiffClose)
            {
              rDiffClose = rDiff;
              IndOut = iTempInd;
            }
        }
    }
  return IndOut;
}

cglHitList& cglHitList::operator=(const cglHitList &hitList) 

{
  n = hitList.n;
  detid = hitList.detid;
  sortflag = hitList.sortflag;
  arm = hitList.arm;

  for (int i = 0; i < n; i++)
    {
      index.push_back(hitList.index[i]);
      coord.push_back(hitList.coord[i]);
    }
  return *this;
}


