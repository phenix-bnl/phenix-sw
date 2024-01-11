// $Id: TMuiGeometry.cc,v 1.20 2017/07/11 03:47:48 phnxbld Exp $
/*!
  \file TMuiGeometry.cc
  \brief muon identifier geometry description
  \author <a href="mailto:pope@phy.ornl.gov">Kyle Pope </a>
  \version $Revision: 1.20 $
  \date $Date: 2017/07/11 03:47:48 $
*/

#include <string>
#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh>
#include <PdbCoordinate.hh>
#include <PdbMuiPanelGeo.hh>
#include <PdbMuiTubeGeo.hh>

#include "MuiGeomClasses.hh"
#include "MUIGEOM.h"

using namespace std;

//________________________________________
const float TMuiGeometry::dx_twopack = 8.9;

//________________________________________
TMuiGeometry* TMuiGeometry::Geom ()
{
  static TMuiGeometry singleton;
  return &singleton;
}

//________________________________________
void TMuiGeometry::Clear ()
{

  // delete TMuiPanelGeo objects
  // No need to destroy two-pack objects because they are deleted via the above
  for (HashPanelVector::iterator iter = _mui_panels.begin (); iter != _mui_panels.end (); iter++)
  { if (*iter) delete *iter; }

  // clear vectors
  fill( _mui_panels.begin (), _mui_panels.end (), (TMuiPanelGeo *) 0);
  fill( _mui_twopacks.begin (), _mui_twopacks.end (), (TMuiTwoPackGeo *) 0);
  fInit = false;

}

//________________________________________
TMuiGeometry::TMuiGeometry ():
  _mui_panels(TMuiChannelId::kPanelsTotal, &PanelHash),
  _mui_twopacks(TMuiChannelId::kTwoPacksMaxTotal, &TwoPackHash),
  fInit( false )
{

  // initialize vectors
  fill( _mui_panels.begin (), _mui_panels.end (), (TMuiPanelGeo *) 0);
  fill( _mui_twopacks.begin (), _mui_twopacks.end (), (TMuiTwoPackGeo *) 0);

  // array initialization
  for( short arm = 0; arm < TMuiChannelId::kArmsTotal; arm++)
  for( short gap = 0; gap < TMuiChannelId::kPlanesPerArm; gap++)
  {
    _gap_z[arm][gap] = 0;
    _gap_x_min[arm][gap] = 0;
    _gap_x_max[arm][gap] = 0;
    _gap_y_min[arm][gap] = 0;
    _gap_y_max[arm][gap] = 0;
  }
}

//___________________________________________________________
void TMuiGeometry::ReadRunAndVersion (const char *file, long &run, float &version)
{
  ifstream s (file);
  if (!s)
  {
    cout << "TMuiGeometry::ReadRunAndVersion -  error opening file" << endl;
    return;
  }

  // Assume that the run and version numbers are on the first line of the file.
  if (!(s >> run >> version)) cout << "TMuiGeometry::ReadRunAndVersion - error reading" << endl;

  return;
}

//___________________________________________________________
void TMuiGeometry::initialize()
{
  MUIGEOM::PRINT( cout, "TMuiGeometry::initialize" );

  if( fInit )
  {

    cout << "TMuiGeometry::initialize - already initialized" << endl;
    return;

  } else {

    cout << "TMuiGeometry::initialize - initializing geometry from files" << endl;

  }

  // Get the version numbers (if available) of the input files.
  char panel_geo_file[] = "mui-panel-geom.dat";
  char panel_size_file[] = "mui-panel-size.dat";
  char tube_geo_file[] = "mui-tube-geom.dat";
  char tube_size_file[] = "mui-tube-size.dat";

  long panel_geo_run = -1, panel_size_run = -1;
  float panel_geo_version = 1.0, panel_size_version = 1.0;
  long tube_geo_run = -1, tube_size_run = -1;
  float tube_geo_version = 1.0, tube_size_version = 1.0;

  ReadRunAndVersion (panel_geo_file, panel_geo_run, panel_geo_version);
  ReadRunAndVersion (panel_size_file, panel_size_run, panel_size_version);
  ReadRunAndVersion (tube_geo_file, tube_geo_run, tube_geo_version);
  ReadRunAndVersion (tube_size_file, tube_size_run, tube_size_version);

  cout << "MuID panel size version " << panel_size_version  << "  run " << panel_size_run << "\n";
  cout << "MuID tube size version " << tube_size_version  << "  run " << tube_size_run << "\n";
  cout << "MuID panel geo version " << panel_geo_version << "  run " << panel_geo_run << "\n";
  cout << "MuID tube geo version " << tube_geo_version  << "  run " << tube_geo_run << "\n";
  cout << endl;

  // File #1 contains the panel sizes.
  ifstream instr1 (panel_size_file);
  if (!instr1)
  {
    cout << "error opening panel size data file" << endl;
    return;
  }

  // panel sizes
  vector<float> xPanelSize(TMuiChannelId::kPanelsPerPlane,0);
  vector<float> yPanelSize(TMuiChannelId::kPanelsPerPlane,0);
  vector<float> zPanelSize(TMuiChannelId::kPanelsPerPlane,0);

  // Read the data line by line until we reach EOF.
  static const int bufsize = 256;
  char linebuf[bufsize];
  while (instr1.getline (linebuf, 256, '\n'))
  {
    if (instr1.gcount () > 256)
    {
      cout << "input buffer too small!  gcount = " << instr1.gcount () << endl;
      return;
    }

    unsigned int panel( 0 );
    float xs(0), ys(0), zs(0);
    istringstream stringbuf (string (linebuf, strlen (linebuf)));
    stringbuf >> panel >> xs >> ys >> zs;
    if( !stringbuf ) continue;
    if( panel >= TMuiChannelId::kPanelsPerPlane ) throw runtime_error( "invalid panel" );

    xPanelSize[panel] = xs;
    yPanelSize[panel] = ys;
    zPanelSize[panel] = zs;

  }
  instr1.close ();

  // File #2 contains the tube sizes.
  ifstream instr2 (tube_size_file);
  if (!instr2)
  {
    cout << "error opening tube size data file: " << tube_size_file << endl;
    return;
  }

  // panel geometry
  float Length[TMuiChannelId::kPanelsPerPlane][TMuiChannelId::kOrientations];
  float Width[TMuiChannelId::kPanelsPerPlane][TMuiChannelId::kOrientations];
  float Thick[TMuiChannelId::kPanelsPerPlane][TMuiChannelId::kOrientations];
  for( unsigned int panel = 0; panel < TMuiChannelId::kPanelsPerPlane; panel++ )
  for( unsigned int orient = 0; orient < TMuiChannelId::kOrientations; orient++ )
  {
    Length[panel][orient] = 0;
    Width[panel][orient] = 0;
    Thick[panel][orient] = 0;
  }

  // Read the data line by line until we reach EOF.
  while (instr2.getline (linebuf, 256, '\n'))
  {

    if (instr2.gcount () > 256)
    {
      cout << "input buffer too small!  gcount = " << instr2.gcount () << endl;
      return;
    }

    // parse line
    unsigned int panel(0), orient(0);
    float l(0), w(0), t(0);
    istringstream stringbuf (string (linebuf, strlen (linebuf)));
    stringbuf >> panel >> orient >> l >> w >> t;
    if( !stringbuf ) continue;
    if( panel >= TMuiChannelId::kPanelsPerPlane || orient >= TMuiChannelId::kOrientations )  throw runtime_error( "invalid panel/orientation" );
    Length[panel][orient] = l;
    Width[panel][orient] = w;
    Thick[panel][orient] = t;

  }

  instr2.close ();

  // File #3 contains the panel positions, etc.
  ifstream instr3(panel_geo_file);
  if (!instr3)
  {
    cout << "error opening panel data file " << panel_geo_file << endl;
    return;
  }

  // Read the panel data line by line until we reach EOF.
  while (instr3.getline (linebuf, 256, '\n'))
  {

    if (instr3.gcount () > 256)
    {
      cout << "input buffer too small!  gcount = " << instr3.gcount () << endl;
      return;
    }

    unsigned int arm( 0 ), gap( 0 ), panel( 0 );
    float xTarg1( 0 ), yTarg1( 0 ), zTarg1( 0 ), xTarg2( 0 ), yTarg2( 0 ), zTarg2( 0 );
    float dxT1Fid( 0 ), dyT1Fid( 0 ), dxFidC( 0 ), dyFidC( 0 ), dzH0( 0 ), dzH1( 0 ), dzV0( 0 ), dzV1( 0 );

    istringstream stringbuf (string (linebuf, strlen (linebuf)));
    stringbuf
      >> arm >> gap >> panel
      >> xTarg1 >> yTarg1 >> zTarg1 >> xTarg2 >> yTarg2 >> zTarg2
      >> dxT1Fid >> dyT1Fid >> dxFidC >> dyFidC
      >> dzH0 >> dzH1 >> dzV0 >> dzV1;

    if( !stringbuf ) continue;

    TMuiChannelId id(arm, gap, panel);
    if (_mui_panels[id])
    {
      cout << "panel geometry already defined - deleting" << endl;
      delete _mui_panels[id];
    }

    _mui_panels[id] = new TMuiPanelGeo (arm, gap, panel,
      TMuiChannelId::
      kTwoPacksPerPanelMax,
      TMuiChannelId::
      kTwoPacksPerPanelMax,
      xPanelSize[panel],
      yPanelSize[panel],
      zPanelSize[panel],
      xTarg1, yTarg1, zTarg1,
      xTarg2, yTarg2, zTarg2,
      dxT1Fid, dyT1Fid, dxFidC, dyFidC,
      dzH0, dzH1, dzV0, dzV1);

  }

  instr3.close ();

  // File #4 contains the tube positions.
  ifstream instr4 (tube_geo_file);
  if (!instr4)
  {
    cout << "error openg tube data file: " << tube_geo_file << endl;
    return;
  }

  // Read the tube data until we reach EOF.
  while (instr4.getline (linebuf, 256, '\n'))
  {

    if (instr4.gcount () > 256)
    {
      cout << "input buffer too small!  gcount = " << instr4.gcount () << endl;
      return;
    }

    unsigned int panel(0), orient(0), layer(0), twopack(0);
    float x1(0), x2(0), y1(0), y2(0);
    istringstream stringbuf (string (linebuf, strlen (linebuf)));
    stringbuf >> panel >> orient >> layer >> twopack >> x1 >> x2 >> y1 >> y2;
    if( !stringbuf ) continue;

    // The position of a tube within the panel doesn't depend on the
    // arm or gap
    for( unsigned int arm = 0; arm < TMuiChannelId::kArmsTotal; arm++)
    {
      for( unsigned int gap = 0; gap < TMuiChannelId::kPlanesPerArm; gap++)
      {

        TMuiChannelId id( arm, gap, panel, (EOrient_t) orient, twopack);
        if (!_mui_twopacks[id])
        {
          if (_mui_panels[id]) _mui_twopacks[id] = _mui_panels[id]->AddTwoPack (orient, twopack);
          else {
            cout << "missing panel" << id << endl;
            continue;
          }
        }
        _mui_twopacks[id]->SetTubeEdges(layer, x1, x2, y1, y2, Length[panel][orient], Width[panel][orient], Thick[panel][orient]);
      }
    }
  }

  instr4.close ();

  // Calculate the approximate "Z" position of each gap.
  for (short arm = 0; arm < TMuiChannelId::kArmsTotal; arm++)
  {
    for (short gap = 0; gap < TMuiChannelId::kPlanesPerArm; gap++)
    {
      float zmin = 1.0e30, zmax = 0.0;
      float xmin = 1.0e30, xmax = -1.0e30;
      float ymin = 1.0e30, ymax = -1.0e30;
      for (short panel = 0; panel < TMuiChannelId::kPanelsPerPlane; panel++)
      {
        TMuiChannelId id( arm, gap, panel);
        if (_mui_panels[id])
        {
          // Save panel center "z" value as gap position.
          float xpanel, ypanel, zpanel, dx, dy, dz;
          _mui_panels[id]->CenterPos (xpanel, ypanel, zpanel);
          _mui_panels[id]->Size (dx, dy, dz);
          if (fabs (zpanel) < fabs (zmin)) zmin = zpanel;
          if (fabs (zpanel) > fabs (zmax)) zmax = zpanel;

          // Save min. and max. "x" and "y" values as gap boundaries.
          // For the purpose of the estimate, there should be no need
          // to account for the possible rotation of the panel
          // relative to the global coordinate system, since the
          // panel has is largest apparent transverse size for a null
          // rotation, and therefore the xpanel +/- dx and ypanel +/-
          // dy ranges are as large as possible.
          if (xpanel - dx < xmin) xmin = xpanel - dx;
          if (xpanel + dx > xmax) xmax = xpanel + dx;
          if (ypanel - dy < ymin) ymin = ypanel - dy;
          if (ypanel + dy > ymax) ymax = ypanel + dy;

        }
      }
      _gap_z[arm][gap] = 0.5 * (zmin + zmax);
      _gap_x_min[arm][gap] = xmin;
      _gap_x_max[arm][gap] = xmax;
      _gap_y_min[arm][gap] = ymin;
      _gap_y_max[arm][gap] = ymax;

    }
  }
  fInit = true;

  MUIGEOM::PRINT( cout, "**" );

}

//___________________________________________________________
float TMuiGeometry::GapZPosition (const short &arm, const short &gap) const
{
  // check boundaries
  if ((arm < 0) || (arm >= TMuiChannelId::kArmsTotal))
  {
    cout << "TMuiGeometry::GapZPosition - invalid arm = " << arm << endl;
    return 0;
  }

  if ((gap < 0) || (gap >= TMuiChannelId::kPlanesPerArm))
  {
    cout << "TMuiGeometry::GapZPosition - invalid gap = " << gap << endl;
    return 0;
  }

  return _gap_z[arm][gap];
}

//___________________________________________________________
void TMuiGeometry::GapXYBoundaries (
  const short &arm, const short &gap,
  float &xmin, float &xmax,
  float &ymin, float &ymax) const
{
  // check boundaries
  if ((arm < 0) || (arm >= TMuiChannelId::kArmsTotal))
  {
    cout << "TMuiGeometry::GapXYBoundaries - invalid arm = " << arm << endl;
    return;
  }

  if ((gap < 0) || (gap >= TMuiChannelId::kPlanesPerArm))
  {
    cout << "TMuiGeometry::GapXYBoundaries - invalid gap = " << gap << endl;
    return;
  }

  xmin = _gap_x_min[arm][gap];
  xmax = _gap_x_max[arm][gap];
  ymin = _gap_y_min[arm][gap];
  ymax = _gap_y_max[arm][gap];

  return;
}

//___________________________________________________________
vector < TMuiChannelId > TMuiGeometry::findPanels (
  const short &Arm,
  const short &Plane,
  const PHVector & GVect,
  const PHVector & DirVect)
{
  vector < TMuiChannelId > panel_list;

  // "Brute-force" algorithm.
  //  For each panel,
  //  1a. Find intersection with panel.
  //  1b. Transform intersection position to panel coords.
  //  2. Check intersection position against panel boundaries.
  //     Add panel to list if within boundaries.

  PHPoint intersection, local_intersect;
  PHPoint x(GVect);
  PHVector v(DirVect);

  for (short Panel = 0; Panel < TMuiChannelId::kPanelsPerPlane; Panel++)
  {
    TMuiPanelGeo *p = getPanel (Arm, Plane, Panel);
    if (p)
    {
      intersection = p->ProjectToPanel (x, v);
      local_intersect = p->TransformToPanel (intersection);
      if (p->IsInPanel (local_intersect.getX (), local_intersect.getY (), local_intersect.getZ ()))
        panel_list.push_back (TMuiChannelId (Arm, Plane, Panel));
    } else {
      cout << "TMuiGeometry::findPanels - bad panel pointer!" << "  A" << Arm << " G" << Plane << " P" << Panel << endl;
    }

  }

  // "Almost-optimal" algorithm.
  //       1.     Find intersection with geometrical plane defined by a center panel.
  //       2.     Determine whether intersection point is in left or right half-plane.
  //       For each of the (4) panels in the half-plane,
  //       5a. Find intersection with panel.
  //       5b. Transform intersection position to panel coords.
  //       6.     Check intersection position against panel boundaries.
  //                       Add panel to list if within boundaries.

  // "Optimal" algorithm.
  //       1.     Find intersection with geometrical plane defined by a center panel.
  //       2.     Calculate rough panel id(s) from this intersection position.
  //       3a. Find intersection with panel given by this guess.
  //       3b. Transform intersection position to panel coords.
  //       4.     Check intersection position against panel boundaries.
  //                       Add panel to list if within boundaries.
  //       For each neighbor panel,
  //       5a. Find intersection with panel.
  //       5b. Transform intersection position to panel coords.
  //       6.     Check intersection position against panel boundaries.
  //                       Add panel to list if within boundaries.

  return panel_list;
}

//______________________________________________________________________
vector < TMuiChannelId > TMuiGeometry::findTwoPacksNoProjection (
  const short &Arm,
  const short &Plane,
  const PHVector& GVect,
  const PHVector& DirVect)
{

  PHPoint r(GVect);
  PHVector v(DirVect);

  vector < TMuiChannelId > twopack_list;
  vector < TMuiChannelId > panel_list = findPanels (Arm, Plane, GVect, DirVect);
  for (unsigned int i = 0; i < panel_list.size (); i++)
  {
    // Get the panel data ...
    short Panel = panel_list[i].Panel ();
    TMuiPanelGeo *p = getPanel (Arm, Plane, Panel);
    if (!p)
    {
      cout << "TMuiGeometry::findTwoPacksNoProjection - bad panel pointer!" << endl;
      return twopack_list;
    }

    PHPoint intersection = p->ProjectToPanel (r, v);
    PHPoint local_intersect = p->TransformToPanel (intersection);

    // Do horizontal first ...
    // ==============================================================
    EOrient_t Orient = kHORIZ;

    // Come up with an initial guess for the two-pack number that
    // was hit.
    short nTwoPackMax = p->getTwoPackCount (Orient) - 1;
    short iTwoPackGuess = p->GuessTwoPack (Orient,
      local_intersect.getX (),
      local_intersect.getY (),
      local_intersect.getZ ());

    // Search window is 5 two-packs: assumes that the guess can only
    // be wrong by +/- 1 two-pack.
    //
    // Note that, if X indicates a position where there are no
    // tubes, the search range will reflect this.   For example,
    // if X is more than two tube lengths outside of the tube volume,
    // none of the tubes will be searched.

    short iTwoPackLow = max < short >(0, iTwoPackGuess - 2);
    short iTwoPackHigh = min < short >(nTwoPackMax, iTwoPackGuess + 2);
    for (short j = iTwoPackLow; j <= iTwoPackHigh; j++)
    {
      // Check each two-pack in the search range; if the point is
      // within the boundaries, add the two-pack to the list.
      TMuiTwoPackGeo *t = p->TwoPackPointer (Orient, j);
      if (t->IsInTwoPack (local_intersect.getX (), local_intersect.getY (), local_intersect.getZ ()))
      { twopack_list.push_back (t->Channel ()); }
    }

    // Do vertical ...
    // ==============================================================
    Orient = kVERT;

    // Come up with an initial guess for the two-pack number that
    // was hit.
    nTwoPackMax = p->getTwoPackCount (Orient) - 1;
    iTwoPackGuess = p->GuessTwoPack (Orient,
      local_intersect.getX (),
      local_intersect.getY (),
      local_intersect.getZ ());

    // Search window is 5 two-packs:        assumes that the guess can only
    // be wrong by +/- 1 two-pack.
    //
    // Note that, if X indicates a position where there are no
    // tubes, the search range will reflect this.   For example,
    // if X is more than two tube lengths outside of the tube volume,
    // none of the tubes will be searched.

    iTwoPackLow = max < short >(0, iTwoPackGuess - 2);
    iTwoPackHigh = min < short >(nTwoPackMax, iTwoPackGuess + 2);
    for (short j = iTwoPackLow; j <= iTwoPackHigh; j++)
    {
      // Check each two-pack in the search range; if the point is
      // within the boundaries, add the two-pack to the list.
      TMuiTwoPackGeo *t = p->TwoPackPointer (Orient, j);
      if (t->IsInTwoPack (local_intersect.getX (), local_intersect.getY (), local_intersect.getZ ()))
      { twopack_list.push_back (t->Channel ()); }
    }
  }

  return twopack_list;

}

//______________________________________________________________________
vector < TMuiChannelId > TMuiGeometry::findTwoPacks (
  const short &Arm,
  const short &Plane,
  const PHVector & GVect,
  const PHVector & DirVect )
{

  PHPoint r(GVect);
  PHVector v(DirVect);

  vector < TMuiChannelId > twopack_list;
  vector < TMuiChannelId > panel_list = findPanels (Arm, Plane, GVect, DirVect);
  for (unsigned int i = 0; i < panel_list.size (); i++)
  {
    // Get the panel data ...
    short Panel = panel_list[i].Panel ();
    TMuiPanelGeo *p = getPanel (Arm, Plane, Panel);
    if (!p)
    {
      cout << "TMuiGeometry::findTwoPacks - bad panel pointer!" << endl;
      return twopack_list;
    }

    PHPoint intersection = p->ProjectToPanel (r, v);
    PHPoint local_intersect = p->TransformToPanel (intersection);
    PHVector local_direction = p->RotateToPanel (v);

    // Do horizontal first ...
    // ==============================================================
    EOrient_t
    Orient = kHORIZ;

    // Come up with an initial guess for the two-pack number that
    // was hit.
    short nTwoPackMax = p->getTwoPackCount (Orient) - 1;
    p->GuessTwoPack (
      Orient,
      local_intersect.getX (),
      local_intersect.getY (),
      local_intersect.getZ ());

    // Search window is 5 two-packs: assumes that the guess can only
    // be wrong by +/- 1 two-pack.
    //
    // Note that, if X indicates a position where there are no
    // tubes, the search range will reflect this.   For example,
    // if X is more than two tube lengths outside of the tube volume,
    // none of the tubes will be searched.
    // For now we loop over every two pack in the panel and see if it works.
    short iTwoPackLow = 0;
    short iTwoPackHigh = nTwoPackMax;
    for (short j = iTwoPackLow; j <= iTwoPackHigh; j++)
    {
      // Check each two-pack in the search range; if the point is
      // within the boundaries, add the two-pack to the list.
      TMuiTwoPackGeo *t = p->TwoPackPointer (Orient, j);
      if (t->IsInTwoPack (local_intersect, local_direction))
      { twopack_list.push_back (t->Channel ()); }
    }

    // Do vertical ...
    // ==============================================================
    Orient = kVERT;

    // Come up with an initial guess for the two-pack number that
    // was hit.
    nTwoPackMax = p->getTwoPackCount (Orient) - 1;
    p->GuessTwoPack(
      Orient,
      local_intersect.getX (),
      local_intersect.getY (),
      local_intersect.getZ ());

    // Search window is 5 two-packs: assumes that the guess can only
    // be wrong by +/- 1 two-pack.
    //
    // Note that, if X indicates a position where there are no
    // tubes, the search range will reflect this.   For example,
    // if X is more than two tube lengths outside of the tube volume,
    // none of the tubes will be searched.
    // For now we loop over every two pack in the panel and see if it works.
    iTwoPackLow = 0;
    iTwoPackHigh = nTwoPackMax;
    for (short j = iTwoPackLow; j <= iTwoPackHigh; j++)
    {
      // Check each two-pack in the search range; if the point is
      // within the boundaries, add the two-pack to the list.
      TMuiTwoPackGeo *t = p->TwoPackPointer (Orient, j);
      if (t->IsInTwoPack (local_intersect, local_direction))
      { twopack_list.push_back (t->Channel ()); }
    }
  }

  return twopack_list;
}


//______________________________________________________________________
PHPoint TMuiGeometry::FindIntersection (
  const short &Arm, const short &Plane,
  const short &Panel,
  const PHPoint & GVect,
  const PHVector & DirVect)
{

  TMuiPanelGeo *p = getPanel (Arm, Plane, Panel);
  if (p) return p->ProjectToPanel (GVect, DirVect);
  else cout << "TMuiGeometry::FindIntersection - Panel geometry is not available" << endl;
  return PHPoint ();

}

//______________________________________________________________________
PHPoint TMuiGeometry::FindIntersection (
  const short &Arm,
  const short &Plane,
  const PHVector & GVect,
  const PHVector & DirVect)
{

  // "Brute-force" algorithm.
  // For each panel in the plane,
  // 1a. Find intersection with panel.
  // 1b. Transform intersection position to panel coords.
  // 2. Check intersection position against panel boundaries.
  // Save the intersection position and panel id if within
  // boundaries.

  PHPoint r(GVect);
  PHVector v(DirVect);
  PHPoint intersection;
  bool found (false);

  for (short Panel = 0; Panel < TMuiChannelId::kPanelsPerPlane; Panel++)
  {
    TMuiPanelGeo *p = getPanel (Arm, Plane, Panel);
    if (p)
    {
      PHPoint x = p->ProjectToPanel (r, v);
      PHPoint local_intersect = p->TransformToPanel (x);

      if (p->IsInPanel (local_intersect.getX (), local_intersect.getY (), local_intersect.getZ ()) && ((!found) || fabs (x.getZ ()) < fabs (intersection.getZ ())))
      {
        found = true;
        intersection = x;
      }

    } else {
      cout << "TMuiGeometry::FindIntersection - bad panel pointer!" << "  A" << Arm << " G" << Plane << " P" << Panel << endl;
    }
  }

  return intersection;

}

//______________________________________________________________________
PHBoolean TMuiGeometry::fetch( PHTimeStamp & Tsearch, const char *calibname, PdbBankID bankID )
{

  if( fInit )
  {

    cout << "TMuiGeometry::fetch - geometry already initialized" << endl;
    return True;

  } else {

    cout << "TMuiGeometry::fetch - initializing geometry using timestamp " << Tsearch << endl;

  }

  if( committed == 1 )
  {
    if (!application->startRead ())
    {
      cout << "TMuiGeometry::fetch - Database not readable" << endl;
      application->abort ();
    } else committed = 0;
  }

  // clear panels/tubes
  Clear();

  // fetch sizes
  static const int kSizeBankID = 12010;
  bankID.setInternalValue (kSizeBankID);

  ostringstream sizename;
  sizename << calibname << "mui_size";
  PdbCalBank *sizeBank = bankManager->fetchBank( "PdbCoordinateBank", bankID, sizename.str().c_str(), Tsearch);
  sizeBank->print();

  // check bank length
  if (sizeBank->getLength () != (unsigned int) (2 * TMuiChannelId::kPanelsPerPlane))
  {
    delete sizeBank;
    return False;
  }

  // The first entries are the panel sizes.
  size_t count = 0;
  vector<float> xPanelSize( TMuiChannelId::kPanelsPerPlane, 0 );
  vector<float> yPanelSize( TMuiChannelId::kPanelsPerPlane, 0 );
  vector<float> zPanelSize( TMuiChannelId::kPanelsPerPlane, 0 );
  for( unsigned int panel = 0; panel < TMuiChannelId::kPanelsPerPlane; panel++)
  {
    PdbCoordinate *size_entry = (PdbCoordinate *) & sizeBank->getEntry (count++);
    xPanelSize[panel] = size_entry->getParameter (PdbCoordinate::x);
    yPanelSize[panel] = size_entry->getParameter (PdbCoordinate::y);
    zPanelSize[panel] = size_entry->getParameter (PdbCoordinate::z);
  }

  // The remaining entries are the tube sizes.
  vector<float> TubeLengthH( TMuiChannelId::kPanelsPerPlane, 0 );
  vector<float> TubeWidthH( TMuiChannelId::kPanelsPerPlane, 0 );
  vector<float> TubeThickH( TMuiChannelId::kPanelsPerPlane, 0 );
  vector<float> TubeLengthV( TMuiChannelId::kPanelsPerPlane, 0 );
  vector<float> TubeWidthV( TMuiChannelId::kPanelsPerPlane, 0 );
  vector<float> TubeThickV( TMuiChannelId::kPanelsPerPlane, 0 );
  for( unsigned int panel = 0; panel < TMuiChannelId::kPanelsPerPlane; panel++)
  {
    PdbCoordinate *size_entry = (PdbCoordinate *) & sizeBank->getEntry (count++);
    TubeLengthH[panel] = size_entry->getParameter (PdbCoordinate::x);
    TubeLengthV[panel] = size_entry->getParError (PdbCoordinate::x);
    TubeWidthH[panel] = size_entry->getParameter (PdbCoordinate::y);
    TubeWidthV[panel] = size_entry->getParError (PdbCoordinate::y);
    TubeThickH[panel] = size_entry->getParameter (PdbCoordinate::z);
    TubeThickV[panel] = size_entry->getParError (PdbCoordinate::z);
  }

  delete sizeBank;

  // panel geometry bank
  static const int kPanelGeoBankID = 12020;
  bankID.setInternalValue (kPanelGeoBankID);
  ostringstream panelname;
  panelname << calibname << "mui_panel";
  PdbCalBank *panelGeoBank = bankManager->fetchBank( "PdbMuiPanelGeoBank", bankID, panelname.str ().c_str (), Tsearch);
  panelGeoBank->print();

  for (unsigned int i = 0; i < panelGeoBank->getLength (); i++)
  {

    short arm(0), gap(0), panel(0);
    PdbMuiPanelGeo *panel_entry = (PdbMuiPanelGeo *) & panelGeoBank->getEntry (i);
    panel_entry->Channel (arm, gap, panel);

    TMuiChannelId id(arm, gap, panel);
    if (_mui_panels[id])
    {
      cout << "TMuiGeometry::fetch - panel geometry already defined" << endl;
      cout << "TMuiGeometry::fetch - deleting" << endl;
      delete _mui_panels[id];
    }

    _mui_panels[id] = new TMuiPanelGeo(
      *panel_entry,
      TMuiChannelId::kTwoPacksPerPanelMax,
      TMuiChannelId::kTwoPacksPerPanelMax,
      xPanelSize[panel],
      yPanelSize[panel],
      zPanelSize[panel]);
  }

  start = panelGeoBank->getStartValTime ();
  stop = panelGeoBank->getEndValTime ();
  delete panelGeoBank;

  // tube geometry bank
  static const int kTubeGeoBankID = 12030;
  bankID.setInternalValue (kTubeGeoBankID);

  ostringstream tubename;
  tubename << calibname << "mui_tube";
  PdbCalBank *tubeGeoBank = bankManager->fetchBank ("PdbMuiTubeGeoBank", bankID, tubename.str ().c_str (), Tsearch);
  tubeGeoBank->print();

  for (unsigned int j = 0; j < tubeGeoBank->getLength (); j++)
  {

    PdbMuiTubeGeo *tube_entry = (PdbMuiTubeGeo *) & tubeGeoBank->getEntry (j);
    if (!tube_entry)
    {
      cout << "TMuiGeometry::fetch - null tube geometry record " << j << endl;
      break;
    }

    short arm(0), gap(0), panel(0), orient(0), layer(0), twopack(0);
    tube_entry->Channel (arm, gap, panel, orient, layer, twopack);
    TMuiChannelId id( arm, gap, panel, (EOrient_t) orient, twopack);
    if (!_mui_twopacks[id])
    {
      if (_mui_panels[id]) _mui_twopacks[id] = _mui_panels[id]->AddTwoPack (orient, twopack);
      else {
        cout << "missing panel" << id << endl;
        continue;
      }
    }

    if (orient == kHORIZ) _mui_twopacks[id]->SetTubeEdges (*tube_entry, TubeLengthH[panel], TubeWidthH[panel], TubeThickH[panel]);
    else _mui_twopacks[id]->SetTubeEdges (*tube_entry, TubeLengthV[panel], TubeWidthV[panel], TubeThickV[panel]);

  }

  delete tubeGeoBank;

  // Calculate the approximate "Z" position of each gap.
  for (short arm = 0; arm < TMuiChannelId::kArmsTotal; arm++)
  {
    for (short gap = 0; gap < TMuiChannelId::kPlanesPerArm; gap++)
    {
      float zmin = 1.0e30, zmax = 0.0;
      float xmin = 1.0e30, xmax = -1.0e30;
      float ymin = 1.0e30, ymax = -1.0e30;
      for (short panel = 0; panel < TMuiChannelId::kPanelsPerPlane; panel++)
      {
        TMuiChannelId id( arm, gap, panel );
        if (_mui_panels[id])
        {

          // Save panel center "z" value as gap position.
          float xpanel, ypanel, zpanel, dx, dy, dz;
          _mui_panels[id]->CenterPos (xpanel, ypanel, zpanel);
          _mui_panels[id]->Size (dx, dy, dz);
          if (fabs (zpanel) < fabs (zmin)) zmin = zpanel;
          if (fabs (zpanel) > fabs (zmax)) zmax = zpanel;

          // Save min. and max. "x" and "y" values as gap boundaries.
          // For the purpose of the estimate, there should be no need
          // to account for the possible rotation of the panel
          // relative to the global coordinate system, since the
          // panel has is largest apparent transverse size for a null
          // rotation, and therefore the xpanel +/- dx and ypanel +/-
          // dy ranges are as large as possible.
          if (xpanel - dx < xmin) xmin = xpanel - dx;
          if (xpanel + dx > xmax) xmax = xpanel + dx;
          if (ypanel - dy < ymin) ymin = ypanel - dy;
          if (ypanel + dy > ymax) ymax = ypanel + dy;

        }
      }

      _gap_z[arm][gap] = 0.5 * (zmin + zmax);
      _gap_x_min[arm][gap] = xmin;
      _gap_x_max[arm][gap] = xmax;
      _gap_y_min[arm][gap] = ymin;
      _gap_y_max[arm][gap] = ymax;
    }
  }
  commit();
  fInit = true;
  return True;
}

//______________________________________________________________________
PHBoolean TMuiGeometry::update(PHTimeStamp & Tstart, PHTimeStamp & Tstop, const char *calibname, PdbBankID bankID, const char *descrip)
{

  ostringstream sizename;
  sizename << calibname << "mui_size";
  if( !update_size( Tstart, Tstop, sizename.str().c_str(), bankID, descrip ) ) return False;

  ostringstream panelname;
  panelname << calibname << "mui_panel";
  if( !update_panel_geom( Tstart, Tstop, panelname.str().c_str(), bankID, descrip ) ) return False;

  ostringstream tubename;
  tubename << calibname << "mui_tube";
  if( !update_tube_geom( Tstart, Tstop, tubename.str().c_str(), bankID, descrip ) ) return False;

  return True;
}

//______________________________________________________________________
PHBoolean TMuiGeometry::update_size(PHTimeStamp & Tstart, PHTimeStamp & Tstop, const char *calibname, PdbBankID bankID, const char *descrip)
{

  MUIGEOM::PRINT( cout, "TMuiGeometry::update_size" );
  if( committed == 1 )
  {
    if (!application->startUpdate ())
    {
      cout << "Database not writable" << endl;
      application->abort ();
    } else committed = 0;
  }

  // pannel size from first file
  ifstream instr1 ("mui-panel-size.dat");
  if (!instr1)
  {
    cout << "error opening panel size data file: mui-panel-size.dat" << endl;
    return False;
  }

  vector<float> xPanelSize(TMuiChannelId::kPanelsPerPlane,0);
  vector<float> yPanelSize(TMuiChannelId::kPanelsPerPlane,0);
  vector<float> zPanelSize(TMuiChannelId::kPanelsPerPlane,0);

  // Read the data line by line until we reach EOF.
  static const int bufsize = 256;
  char linebuf[bufsize];
  while (instr1.getline (linebuf, 256, '\n'))
  {

    if (instr1.gcount () > 256)
    {
      cout << "input buffer too small!  gcount = " << instr1.gcount () << endl;
      return False;
    }

    istringstream stringbuf (string (linebuf, strlen (linebuf)));
    short panel(0);
    float xs(0), ys(0), zs(0);
    stringbuf >> panel >> xs >> ys >> zs;
    if( !stringbuf ) continue;
    if( panel >= TMuiChannelId::kPanelsPerPlane ) throw runtime_error( "invalid panel" );

    xPanelSize[panel] = xs;
    yPanelSize[panel] = ys;
    zPanelSize[panel] = zs;

  }
  instr1.close ();

  // tube sizes
  ifstream instr2 ("mui-tube-size.dat");
  if (!instr2)
  {
    cout << "error opening tube size data file" << endl;
    return False;
  }

  float Length[TMuiChannelId::kPanelsPerPlane][TMuiChannelId::kOrientations];
  float Width[TMuiChannelId::kPanelsPerPlane][TMuiChannelId::kOrientations];
  float Thick[TMuiChannelId::kPanelsPerPlane][TMuiChannelId::kOrientations];
  for( unsigned int panel = 0; panel < TMuiChannelId::kPanelsPerPlane; panel++ )
  for( unsigned int orient = 0; orient < TMuiChannelId::kOrientations; orient++ )
  {
    Length[panel][orient] = 0;
    Width[panel][orient] = 0;
    Thick[panel][orient] = 0;
  }

  // Read the data line by line until we reach EOF.
  while (instr2.getline (linebuf, 256, '\n'))
  {

    if (instr2.gcount () > 256)
    {
      cout << "input buffer too small!  gcount = " << instr2.gcount () << endl;
      return False;
    }

    // parse line
    unsigned int panel(0), orient(0);
    float l(0), w(0), t(0);
    istringstream stringbuf (string (linebuf, strlen (linebuf)));
    stringbuf >> panel >> orient >> l >> w >> t;
    if( !stringbuf ) continue;
    if( panel >= TMuiChannelId::kPanelsPerPlane || orient >= TMuiChannelId::kOrientations )  throw runtime_error( "invalid panel/orientation" );

    Length[panel][orient] = l;
    Width[panel][orient] = w;
    Thick[panel][orient] = t;

  }

  instr2.close ();

  // update database with read values
  static const int kSizeBankID = 12010;
  bankID.setInternalValue (kSizeBankID);
  PdbCalBank *sizeBank = bankManager->createBank ("PdbCoordinateBank", bankID, descrip, Tstart, Tstop, calibname);

  // set bank length
  sizeBank->setLength (2 * TMuiChannelId::kPanelsPerPlane);
  sizeBank->print ();

  // The first entries (one per panel) are the panel sizes.
  int record = 0;
  for( unsigned int panel = 0; panel < TMuiChannelId::kPanelsPerPlane; panel++)
  {
    PdbCoordinate *size_entry = (PdbCoordinate *) & sizeBank->getEntry (record++);
    size_entry->setAllParameters (xPanelSize[panel], yPanelSize[panel], zPanelSize[panel]);
    size_entry->print ();
  }

  // The remaining entries (one per panel) are the tube sizes.
  for( unsigned int panel = 0; panel < TMuiChannelId::kPanelsPerPlane; panel++)
  {
    PdbCoordinate *size_entry = (PdbCoordinate *) & sizeBank->getEntry (record++);
    size_entry->setAllParameters (Length[panel][kHORIZ], Width[panel][kHORIZ], Thick[panel][kHORIZ]);
    size_entry->setAllParErrors (Length[panel][kVERT], Width[panel][kVERT], Thick[panel][kVERT]);
    size_entry->print ();
  }

  sizeBank->print();
  application->commit();
  MUIGEOM::PRINT( cout, "**" );
  return True;

}

//______________________________________________________________________
PHBoolean TMuiGeometry::update_panel_geom(PHTimeStamp & Tstart, PHTimeStamp & Tstop, const char *calibname, PdbBankID bankID, const char *descrip)
{

  MUIGEOM::PRINT( cout, "TMuiGeometry::update_panel_geom" );
  if( committed == 1 )
  {
    if (!application->startUpdate ())
    {
      cout << "Database not writable" << endl;
      application->abort ();
    } else committed = 0;
  }

  ifstream in("mui-panel-geom.dat");
  if (!in)
  {
    cout << "error opening panel data file" << endl;
    return False;
  }

  static const int kPanelGeoBankID = 12020;
  bankID.setInternalValue (kPanelGeoBankID);
  PdbCalBank *panelGeoBank = bankManager->createBank ("PdbMuiPanelGeoBank", bankID, descrip, Tstart, Tstop, calibname);
  panelGeoBank->setLength(TMuiChannelId::kPanelsTotal);

  // Read the panel data line by line until we reach EOF.
  int record = 0;
  static const int bufsize = 256;
  char linebuf[bufsize];
  while (in.getline (linebuf, 256, '\n'))
  {

    if (in.gcount () > 256)
    {
      cout << "input buffer too small!  gcount = " << in.gcount () << endl;
      return False;
    }

    short arm, gap, panel;
    float xTarg1, yTarg1, zTarg1, xTarg2, yTarg2, zTarg2;
    float dxT1Fid, dyT1Fid, dxFidC, dyFidC, dzH0, dzH1, dzV0, dzV1;
    istringstream stringbuf (string (linebuf, strlen (linebuf)));
    stringbuf >> arm >> gap >> panel
      >> xTarg1 >> yTarg1 >> zTarg1 >> xTarg2 >> yTarg2 >> zTarg2
      >> dxT1Fid >> dyT1Fid >> dxFidC >> dyFidC
      >> dzH0 >> dzH1 >> dzV0 >> dzV1;

    if( !stringbuf ) continue;

    // Fill the panel geometry bank as we go along.
    PdbMuiPanelGeo *panel_entry = (PdbMuiPanelGeo *) & panelGeoBank->getEntry (record++);
    panel_entry->SetChannel (arm, gap, panel);
    panel_entry->SetTargetPosition (1, xTarg1, yTarg1, zTarg1);
    panel_entry->SetTargetPosition (2, xTarg2, yTarg2, zTarg2);
    panel_entry->SetTarget1ToFiducial (dxT1Fid, dyT1Fid);
    panel_entry->SetFiducialToCenter (dxFidC, dyFidC);
    panel_entry->SetTubeDisplacement (kHORIZ, dzH0, dzH1);
    panel_entry->SetTubeDisplacement (kVERT, dzV0, dzV1);

    panel_entry->print ();

  }
  in.close ();
  application->commit();
  MUIGEOM::PRINT( cout, "**" );
  return True;
}


//______________________________________________________________________
PHBoolean TMuiGeometry::update_tube_geom(PHTimeStamp & Tstart, PHTimeStamp & Tstop, const char *calibname, PdbBankID bankID, const char *descrip)
{

  MUIGEOM::PRINT( cout, "TMuiGeometry::update_tube_geom" );
  if( committed == 1 )
  {
    if (!application->startUpdate ())
    {
      cout << "Database not writable" << endl;
      application->abort ();
    } else committed = 0;
  }

  ifstream in("mui-tube-geom.dat");
  if (!in)
  {
    cout << "error opening tube data file" << endl;
    return False;
  }

  static const int kTubeGeoBankID = 12030;
  bankID.setInternalValue (kTubeGeoBankID);
  PdbCalBank *tubeGeoBank = bankManager->createBank ("PdbMuiTubeGeoBank", bankID, descrip, Tstart, Tstop, calibname );
  tubeGeoBank->setLength (2 * TMuiChannelId::kTwoPacksMaxTotal);

  // Read the tube data until we reach EOF.
  int record = 0;
  static const int bufsize = 256;
  char linebuf[bufsize];
  while (in.getline (linebuf, 256, '\n'))
  {

    if (in.gcount () > 256)
    {
      cout << "input buffer too small!  gcount = " << in.gcount () << endl;
      return False;
    }

    short panel, orient, layer, tube;
    float x1, x2, y1, y2;
    istringstream stringbuf (string (linebuf, strlen (linebuf)));
    stringbuf >> panel >> orient >> layer >> tube >> x1 >> x2 >> y1 >> y2;
    if( !stringbuf ) continue;

    // The position of a tube within the panel doesn't depend on the
    // arm or gap (for now).
    for( unsigned int arm = 0; arm < TMuiChannelId::kArmsTotal; arm++)
    {
      for( unsigned int gap = 0; gap < TMuiChannelId::kPlanesPerArm; gap++)
      {

        // Fill the tube geometry bank as we go along.
        PdbMuiTubeGeo *tube_entry = (PdbMuiTubeGeo *) & tubeGeoBank->getEntry (record++);
        if (!tube_entry) cout << "null tube geometry record " << record << endl;

        tube_entry->SetChannel (arm, gap, panel, orient, layer, tube);
        tube_entry->SetEndPositionLo (x1, y1);
        tube_entry->SetEndPositionHi (x2, y2);
        tube_entry->print();
      }
    }
  }

  in.close ();

  // Resize the tube geo bank to actual number of records.
  tubeGeoBank->setLength(record);
  application->commit();
  MUIGEOM::PRINT( cout, "**" );
  return True;
}

//______________________________________________________________________
void TMuiGeometry::dump_panel_geometry (void) const
{
  // dump panel geometry
  for (short arm = 0; arm < TMuiChannelId::kArmsTotal; arm++)
  for (short plane = 0; plane < TMuiChannelId::kPlanesPerArm; plane++)
  for (short panel = 0; panel < TMuiChannelId::kPanelsPerPlane; panel++)
  {
    float x, y, z, dx, dy, dz;
    TMuiChannelId id (arm, plane, panel);
    if (!_mui_panels[id])
    {
      cout
        << "TMuiGeometry::dump_panel_geometry -"
        << " arm: " << arm << " plane: " << plane << " panel: " << panel
        << " panel is not initialized" << endl;
      continue;
    }

    _mui_panels[id]->CenterPos (x, y, z);
    _mui_panels[id]->Size (dx, dy, dz);
    cout
      << "TMuiGeometry::dump_panel_geometry -"
      << " arm: " << arm << " plane: " << plane << " panel: " << panel
      << " center: (" << x << "," << y << "," << z << ") "
      << " size: (" << dx << "," << dy << "," << dz << ")"
      << endl;
  }
}


//________________________________________________________________
ostream & operator << (ostream & s, const TMuiGeometry & g)
{

  for (short arm = 0; arm < TMuiChannelId::kArmsTotal; arm++)
  for (short gap = 0; gap < TMuiChannelId::kPlanesPerArm; gap++)
  for (short panel = 0; panel < TMuiChannelId::kPanelsPerPlane; panel++)
  {
    TMuiPanelGeo *p = g.getPanel (arm, gap, panel);
    s << *p;

    // print horizontal tubes
    for (short twopack = 0; twopack < p->getTwoPackCount (kHORIZ); twopack++)
    {
      TMuiTwoPackGeo *t = p->TwoPackPointer (kHORIZ, twopack);
      if (t) s << *t;
    }

    // print vertical tubes
    for (short twopack = 0; twopack < p->getTwoPackCount (kVERT); twopack++)
    {
      TMuiTwoPackGeo *t = p->TwoPackPointer (kVERT, twopack);
      if (t) s << *t;
    }

    s << endl;
  }

  return s;
}
