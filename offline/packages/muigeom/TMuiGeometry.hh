#ifndef MUI_GEOMETRY_H
#define MUI_GEOMETRY_H

// $Id: TMuiGeometry.hh,v 1.17 2009/09/18 16:03:35 hpereira Exp $
/*!
  \file TMuiGeometry.hh
  \brief muon identifier geometry description
  \author <a href="mailto:pope@phy.ornl.gov">Kyle Pope </a>
  \version $Revision: 1.17 $
  \date $Date: 2009/09/18 16:03:35 $
*/

#include <phool.h>
#include <PdbCalBank.hh>
#include <PHGeometryObject.h>

#include <iosfwd>
#include <iostream>
#include <vector>

#include "MuiCommon.hh"
#include "MuiGeomClasses.hh"
#include "hash_vector.hh"

// forward declarations ...
class PHFrame;
class PHPoint;
class PHVector;

//! muon identifier geometry description
/*!
 muon identifier geometry description

 To invoke any of the TMuiGeometry methods, use the Geom() method
 to obtain a pointer to the single instance of TMuiGeometry.
 For example:


<pre>TMuiTwoPackGeo* t = TMuiGeometry::Geom()->GetTwoPack(kSOUTH,4,0,kVERT,12);</pre>

@author <a href="mailto:pope@phy.ornl.gov">Kyle Pope </a>

*/

class TMuiGeometry : public PHGeometryObject
{
  private:

  //! Constructor.
  TMuiGeometry();

  //! Copy constructor.
  TMuiGeometry(const TMuiGeometry& source);

  //! Assignment operator.
  TMuiGeometry& operator=(const TMuiGeometry& source);

  //! Destructor.
  virtual ~TMuiGeometry()
  { Clear(); }

  //! Get run and version numbers from a text file.
  static void ReadRunAndVersion(const char* file, long& run, float& version);

  //! update (does nothing here)
  PHBoolean update(PHTimeStamp&, PHTimeStamp&, const char*, PdbBankID, char*)
  { return True; }

  public:

  //! Points to the *single* instance of TMuiGeometry.
  static TMuiGeometry* Geom();

  //! Initializes the instance of TMuiGeometry (from file or database).
  static void Init()
  { Geom()->initialize(); }

  //! Nominal separation of a pair of two-packs perpendicular to their length (8.4 + 0.5 = 8.9 cm).
  static const float dx_twopack;

  //! Get a pointer to the panel identified by a TMuiChannelId object.
  TMuiPanelGeo* getPanel(const TMuiChannelId& ident) const
  { return _mui_panels[ident]; }

  //! Get a pointer to the panel identified by (arm,plane,panel).
  TMuiPanelGeo* getPanel (const short& arm, const short& plane, const short& panel) const
  { return _mui_panels[ TMuiChannelId(arm,plane,panel) ]; }

  //! Get a pointer to the two-pack identified by a TMuiChannelId object.
  TMuiTwoPackGeo* getTwoPack(const TMuiChannelId& ident) const
  { return _mui_twopacks[ident]; }

  //! Get a pointer to the two-pack identified by (arm,plane,panel,orient,twopack).
  TMuiTwoPackGeo* getTwoPack(
    const short& arm, const short& plane, const short& panel,
    const EOrient_t& orient, const short& twopack) const
  {  return _mui_twopacks[TMuiChannelId(arm,plane,panel,orient,twopack)]; }


  //! Find panels in the specified plane that lie along a trajectory.
  /*!
    The trajectory is given by the position GVect and the unit vector
    DirVect in global coordinates.
  */
  std::vector<TMuiChannelId> findPanels(
      const short& Arm,
      const short& Plane,
      const PHVector &GVect,
      const PHVector &DirVect);

  //! Find two-packs in the specified plane that lie along a trajectory.
  /*!
    The trajectory is given by the position GVect and the unit vector
    DirVect in global coordinates.  Not good for particles coming
    from the beam pipe, use findTwoPacks instead.
  */
  std::vector<TMuiChannelId> findTwoPacksNoProjection(
      const short& Arm,
      const short& Plane,
      const PHVector& GVect,
      const PHVector &DirVect);

  //! Find two-packs in the specified plane that lie along a trajectory.
  /*!
    The trajectory is given by the position GVect and the unit vector
    DirVect in global coordinates.
  */
  std::vector<TMuiChannelId> findTwoPacks(
      const short& Arm,
      const short& Plane,
      const PHVector& GVect,
      const PHVector &DirVect);

  //! Get the (approximate) "z" position of the gap.
  float GapZPosition(const short& arm, const short& gap) const;

  //! Get approximate values of the "x" and "y" boundaries of the gap.
  void GapXYBoundaries(
      const short& arm,
      const short& gap,
      float& xmin, float& xmax,
      float& ymin, float& ymax) const;

  /*!
    Given a position GVect and a unit vector DirVect in global
    coordinates, find the intersection position with the given panel.
    returned PHPoint is not necessarily inside the panel
  */
  PHPoint FindIntersection(const short& Arm, const short& Plane,
    const short& Panel,
    const PHPoint &GVect,
    const PHVector &DirVect
    );

  //! Find the intersection position of a trajectory with the given plane.
  /*!
    The trajectory is given by the position GVect and the unit vector
    DirVect in global coordinates.
    If more than one panel lies along the trajectory, take the one
    with the minimum "z" value.
  */
  PHPoint FindIntersection(
    const short& Arm,
    const short& Plane,
    const PHVector &GVect,
    const PHVector &DirVect);

  //! clear all members
  void Clear();

  //! Fetch geometry data from the database.
  virtual PHBoolean fetch(PHTimeStamp &Tsearch, const char *calibname, PdbBankID);

  //! Update database from geometry files.
  virtual PHBoolean update( PHTimeStamp &Tstart, PHTimeStamp &Tstop, const char *calibname, PdbBankID, const char *descrip);

  //! dump panel geometry
  void dump_panel_geometry( void ) const;

  //! initialization (overloaded)
  virtual void initialize();

  //!@name useless methods from base class
  //@{

  //! Not sure what this method is for (from base class) !TO BE REMOVED!
  virtual PHBoolean translateAndRotate( PHTimeStamp &Tsearch, const char *calibname, PdbBankID)
  { return true; }

  //! Not sure what this method is for (from base class)
  virtual PHBoolean rotateAndTranslate( PHTimeStamp &Tsearch, const char *calibname, PdbBankID)
  { return true; }

  //! Not sure what this method is for (from base class)
  virtual PHBoolean rotateAndTranslate( PHFrame initialE, PHFrame finalE,  PHFrame initialW, PHFrame finalW)
  { return true; }

  //@}

  private:

  //! Update database from geometry files.
  virtual PHBoolean update_size( PHTimeStamp &Tstart, PHTimeStamp &Tstop, const char *calibname, PdbBankID, const char *descrip);

  //! Update database from geometry files.
  virtual PHBoolean update_panel_geom( PHTimeStamp &Tstart, PHTimeStamp &Tstop, const char *calibname, PdbBankID, const char *descrip);

  //! Update database from geometry files.
  virtual PHBoolean update_tube_geom( PHTimeStamp &Tstart, PHTimeStamp &Tstop, const char *calibname, PdbBankID, const char *descrip);

  //! panel geometry
  typedef hashVector<TMuiChannelId,TMuiPanelGeo*> HashPanelVector;

  //! two pack geometry
  typedef hashVector<TMuiChannelId,TMuiTwoPackGeo*> HashTwoPackVector;

  //! panel geometry
  HashPanelVector _mui_panels;

  //! two pack geometry
  HashTwoPackVector _mui_twopacks;

  //! approx. z center position of [arm][gap]
  float _gap_z[TMuiChannelId::kArmsTotal][TMuiChannelId::kPlanesPerArm];

  //! approx. x min.   position of [arm][gap]
  float _gap_x_min[TMuiChannelId::kArmsTotal][TMuiChannelId::kPlanesPerArm];

  //! approx. x max.   position of [arm][gap]
  float _gap_x_max[TMuiChannelId::kArmsTotal][TMuiChannelId::kPlanesPerArm];

  //! approx. y min.   position of [arm][gap]
  float _gap_y_min[TMuiChannelId::kArmsTotal][TMuiChannelId::kPlanesPerArm];

  //! approx. y max.   position of [arm][gap]
  float _gap_y_max[TMuiChannelId::kArmsTotal][TMuiChannelId::kPlanesPerArm];

  //! Have we initialized the data?
  bool fInit;

  //! streamer.
  friend std::ostream& operator << (std::ostream& s, const TMuiGeometry& g);

};

#endif     /* MUI_GEOMETRY_H */
