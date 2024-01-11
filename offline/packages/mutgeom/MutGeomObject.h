#ifndef __MUTGEOMOBJECT_HH__
#define __MUTGEOMOBJECT_HH__
// $Id: MutGeomObject.h,v 1.27 2009/01/14 23:58:32 hpereira Exp $ 

/*!
  \file MutGeomObject.h
  \brief Describes a GeomObject of the muon tracker system. 
  \author Douglas Fields, Nicki Bruner, Hugo Pereira
  \version $Revision: 1.27 $
  \date $Date: 2009/01/14 23:58:32 $
*/

#include <string>
#include <PHVector.h>
#include <PHPoint.h>
#include <PHFrame.h>
#include <PHGeometry.h>
#include <PdbBankID.hh>

#include "MUTGEOM.h"

class PHTimeStamp;
class PdbBankManager;
class PdbCalBank;
class PdbApplication;
class PdbMutGeom;

//! Describes a GeomObject of the muon tracker system. 
class MutGeomObject 
{
  public:

  //! Default constructor.
  MutGeomObject();

  //! Copy constructor.
  MutGeomObject(const MutGeomObject& rhs);

  //! Destructor.
  virtual ~MutGeomObject();

  //! database access state 
  enum DBState {
    UNINITIALIZED,
    FETCH_OPEN,
    UPDATE_OPEN
  };

  protected:
  
  //! x,y,z position of a point on the object surface
  PHPoint fGlobalPosition;  
  
  //! vector normal to the object surface
  PHVector fGlobalVector;   
  
  //! application state
  DBState dbState;        

  //! pointer to bank manager
  PdbBankManager *bankManager;  
  
  //! pointer to the application
  PdbApplication *application;  
  
  //! pointer to the geometry bank  
  PdbCalBank *geometryBank; 

  public:
      
  //! object name
  std::string name;

  //! retrieve arm number
  virtual MUTGEOM::ArmNumber getArm() const
  {return MUTGEOM::South;}
  
  //! retrieve station number
  virtual MUTGEOM::StationNumber getStation() const 
  {return MUTGEOM::Station1;}
  
  //! retrieve octant number
  virtual MUTGEOM::OctantNumber getOctant() const
  {return MUTGEOM::Octant1;}

  //! return x,y,z global position
  const PHPoint& getGlobalPosition() const 
  {return fGlobalPosition;}   
  
  //! return x,y,z global orientation
  const PHVector& getGlobalVector() const 
  {return fGlobalVector;} 

  //! changes x,y,z global position
  void setGlobalPosition( const PHPoint& i) 
  {fGlobalPosition=i;}
  
  //! changes x,y,z global orientation
  void setGlobalVector( const PHVector& i) 
  {fGlobalVector=i;}

  //! fill the global coord variables at one time...
  virtual void SetGlobalGeom(
      const double& p_x, const double& p_y, const double& p_z,
      const double& v_x, const double& v_y, const double& v_z)
  { 
    fGlobalPosition = PHPoint( p_x, p_y, p_z);
    fGlobalVector = PHVector(v_x, v_y, v_z);
  }


  //! fetch or update banks in the database.
  bool fetch(
    const char* className,
    PHTimeStamp &Tsearch, const char *calibname, const PdbBankID& bankID=0);

  //! create a new bank, store it internaly (in geometryBank)
  bool update( 
    const char* className, 
    PHTimeStamp &Tstart, PHTimeStamp &Tstop,
    const char *bankName, const PdbBankID& bankID, 
    const char *descrip);

  //! commit current changes to the database
  bool commit();

  //! printout bank summary
  void print_bank_summary( void ) const;
  
  //! returns current bank length, if any
  unsigned int getBankLength() const;

  /* 
    The following functions should more accurately be labeled "transform",
    but, since they are inherited anyway, they've been implemented as 
    coordinate system transformations.
  */
  
  //! transform vector coordinated from old to new frame
  static PHVector rotateAndTranslate(const PHFrame& oldFrame, const PHVector& v, const PHFrame& newFrame)
  { return PHGeometry::transformVector(oldFrame, v, newFrame); }
  
  //! transform point coordinated from old to new frame
  static PHPoint rotateAndTranslate(const PHFrame& oldFrame, const PHPoint& p, const PHFrame& newFrame)
  { return PHGeometry::transformPoint(oldFrame, p, newFrame); }

  //! rotate axis 
  virtual void rotateThis(float rotAngle, char axisLabel);

  //! dump object
  virtual void print( void ) const;

  //! streamer
  friend std::ostream& operator << ( std::ostream &s, const MutGeomObject & g )
  {
    s << g.name<<"/n";
    s << " position = " << g.getGlobalPosition() <<"/n";
    s << " orientation vector = " << g.getGlobalVector() <<"/n";
    return s;
  }
    
  
};

#endif   /* __MutGeomObject_HH__ */
