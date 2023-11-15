// $Id: PdbMutGeom.hh,v 1.5 2004/07/27 17:50:34 irina Exp $

#ifndef __PDBMUTGEOM_DDL__
#define __PDBMUTGEOM_DDL__

#include "PdbCalChan.hh"
#include "PHVector.h"
#include "PHPoint.h"

/* \brief
  This is the base object for mutr calibrations.
  It should only contains members which are either basic types or
  objects deriving from TObject
*/
class PdbMutGeom : public PdbCalChan
{
public:
  
  //! constructor
  PdbMutGeom ();
  
  /*! 
    copy constructor
    should be useless since all members are non pointers
  */
  PdbMutGeom( const PdbMutGeom& base_ref );

  /*! 
    equal to operator
    should be useless since all members are non pointers
  */
	PdbMutGeom& operator = ( const PdbMutGeom& base_ref );
  
  //! constructor
  ~PdbMutGeom();

  //! print method
  virtual void print() const;
 
  //! retrieves Global Position
  PHPoint getGlobalPosition() const 
  {
    return PHPoint( 
      fGlobalPosition_x,
      fGlobalPosition_y,
      fGlobalPosition_z );
  }
  
  //! retrieves Global vector
  PHVector getGlobalVector() const 
  {
    return PHVector( 
      fGlobalVector_x,
      fGlobalVector_y,
      fGlobalVector_z );
  }
  
  int getId() const 
  {return _id;}
  
  int getArm() const 
  {return ArmNum;} 
  
  int getStation() const 
  {return StationNum;}
  
  int getOctant() const 
  {return OctantNum;}
  
  //! changes global position coordinates
  void setGlobalPosition(double x, double y, double z)
  {
    fGlobalPosition_x = x;
    fGlobalPosition_y = y;
    fGlobalPosition_z = z;
  }

  //! changes global position coordinates
  void setGlobalPosition(const PHPoint& i) 
  { setGlobalPosition( i.getX(), i.getY(), i.getZ() ); }

  //! changes global vector coordinates
  void setGlobalVector(double x, double y, double z)
  {
    fGlobalVector_x = x;
    fGlobalVector_y = y;
    fGlobalVector_z = z;
  }

  //! changes global vector coordinates
  void setGlobalVector(const PHVector& i) 
  { setGlobalVector( i.getX(), i.getY(), i.getZ() ); }

  void setId(int id) 
  { _id = id; }
  
  void setArmStationOctant(int arm, int station, int oct)
  {
    ArmNum = arm;
    StationNum = station;
    OctantNum = oct;
  }

protected:
  int _id;
  int ArmNum; 
  int StationNum;
  int OctantNum;
  
  double fGlobalPosition_x; // geom object global position
  double fGlobalPosition_y; // geom object global position
  double fGlobalPosition_z; // geom object global position
  
   
  double fGlobalVector_x;     // geom object global vector
  double fGlobalVector_y;   // geom object global vector
  double fGlobalVector_z;   // geom object global vector
 
  ClassDef(PdbMutGeom,1);
};

#endif /* __PDBMUTGEOM_DDL__ */
