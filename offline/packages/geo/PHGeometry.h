#ifndef __PHGEOMETRY_H__
#define __PHGEOMETRY_H__

// Geometry utilities, embedded in a namespace.  
// To use do something
// like this:

/*
 #include "PHGeometry.h"
 #include "PHPoint.h"

 PHPoint a(1,2,3);
 PHPoint b(3,2,1);

 cout << "PHGeometry::distancePointToPoint(a,b) = " << 
   PHGeometry::distancePointToPoint(a,b) << endl;

*/

#include "PHCylPoint.h"
#include "PHSphPoint.h"
#include "PHPlane.h"
#include "PHCylinder.h"
#include "PHCylinderSection.h"
#include "PHSphere.h"
#include "PHSphereSection.h"
#include "PHFrame.h"
#include "PHPolyLine.h"
#include "PHLine.h"
#include "PHPanel.h"
#include "PHPolyPanel.h"
#include "PHMatrix.h"

namespace PHGeometry 
{
  void cartesianToCylindrical(const PHPoint &cart,
			      PHCylPoint& cyl);
  void cylindricalToCartesian(const PHCylPoint& cyl,
			      PHPoint& cart);
  void cartesianToSpherical(const PHPoint &cart,
			    PHSphPoint& sph);
  void sphericalToCartesian(const PHSphPoint& sph,
			    PHPoint& cart);
  void cylindricalToSpherical(const PHCylPoint& cyl,
			      PHSphPoint &sph);
  void sphericalToCylindrical(const PHSphPoint &sph,
			      PHCylPoint& cyl);
  //
  // distance Point to Point
  //
  double distancePointToPoint(const PHPoint& ,
			      const PHPoint &);
  double distancePointToPoint(const PHSphPoint&,
			      const PHSphPoint&);
  double distancePointToPoint(const PHCylPoint&,
			      const PHCylPoint&);
  //
  // vectorOperation
  //
  inline double dot(const PHVector& v1, const PHVector& v2) { return v1.dot(v2); }
  PHVector cross(const PHVector&,
		 const PHVector&);
  PHAngle  angle(const PHVector&,
		 const PHVector&);
  //
  // Line and Points
  //
  PHBoolean intersectionLinePlane(const PHLine&,
				  const PHPlane&,
				  PHLine&);
  PHBoolean intersectionLinePlane(const PHLine&,
				  const PHPlane&,
				  PHPoint&);
  PHBoolean intersectionLineTriPanel(const PHLine&,
				     const PHTriPanel&,
				     const PHPoint&);
  PHBoolean intersectionLineTriPanel(const PHLine&,
				     const PHTriPanel&, PHLine&);
  PHBoolean intersectionLinePanel(const PHLine&,
				  const PHPanel&,
				  PHPoint&);
  PHBoolean intersectionLinePanel(const PHLine&,
				  const PHPanel&,
				  PHLine&);
  
  double distanceLinePoint(const PHLine&,
			   const PHPoint &);
  double distanceLineLine(const PHLine&,
			  const PHLine &);
  PHPoint closestApproachLinePoint(const PHLine&,
				   const PHPoint &);
  PHPoint closestApproachLineLine(const PHLine&,
				  const PHLine &);
 
  PHBoolean intersectionLineLineOnPlane(const PHLine&,
					const PHLine&,
					const PHPlane&,
					PHPoint&);

  PHLine   projectLineIntoPlane(const PHLine&,
				const PHPlane&);
  PHPoint  transformPoint(const PHFrame&,
			  const PHPoint&,
			  const PHFrame&);
  PHPoint  transformPoint(const PHMatrix&,
			  const PHVector&,
			  const PHPoint&);
  PHVector transformVector(const PHFrame&,
			   const PHVector&,
			   const PHFrame&);
  PHVector transformVector(const PHMatrix&,
			   const PHVector&);
  PHLine   transformLine(const PHFrame&,
			 const PHLine&,
			 const PHFrame&);
  PHLine   transformLine(const PHMatrix&,
			 const PHVector&,
			 const PHLine&);
  PHPanel  transformPanel(const PHFrame&,
			  const PHPanel&,
			  const PHFrame&);
  PHPanel  transformPanel(const PHMatrix&,
			  const PHVector&,
			  const PHPanel&);
  PHPanel  rotatePanelAboutCenter(const double&,
			          const PHVector&,
			          const PHPanel&);
  PHPanel  expandPanel(const PHPanel&,
                       const double);
  PHCylinder transformCylinder(const PHMatrix&,
			       const PHVector&,
			       const PHCylinder&);
  PHCylinder transformCylinder(const PHFrame&,
			       const PHCylinder&,
			       const PHFrame&);
  PHCylinderSection transformCylinderSection(const PHMatrix&,
					     const PHVector&,
					     const PHCylinderSection&);
  PHCylinderSection transformCylinderSection(const PHFrame&,
					     const PHCylinderSection&,
					     const PHFrame&);
  // a rotation in 3 dimentions to an angle (double - in radians) 
  // about an axis (vector) in positive direction is represented by 
  // the matrix below
  PHMatrix rotationMatrix(const double&,
			  const PHVector &);
  // frames2MatrixAndVector  provides the rotation matrix 
  // and the translation vector which applied to a point in frame F1 
  // produce the equivalent coordinate transformation as changing the 
  // reference frame from frame F1 to frame F2
  void frames2MatrixAndVector(const PHFrame&,
			      const PHFrame&,
			      PHMatrix &,
			      PHVector &);
  PHFrame MatrixAndVector2frames(const PHFrame &,
				 const PHMatrix &,
				 const PHVector &);
 
  //TO BE IMPLEMENTED
  // short  intersectionLineLine(const PHLine&, const PHLine &, PHPoint& );
  // double distToPointXY(PHPoint &) const;
  // double distToLineXY(PHLine &) const;
  //
  // Sphere and Cylinder
  //
  short  intersectionLineCylinder(const PHLine&,
				  const PHCylinder&,
				  PHPoint&,
				  PHPoint&); 
  short  intersectionLineCylinder(const PHLine&,
				  const PHCylinder&,
				  PHLine&,
				  PHLine&);
  short  intersectionLineCylinderSection(const PHLine&,
					 const PHCylinderSection&,
					 PHPoint&,
					 PHPoint&); 
  short  intersectionLineCylinderSection(const PHLine&,
					 const PHCylinderSection&,
					 PHLine&,
					 PHLine&);
  
  short  intersectionLineSphere(const PHLine&,
				const PHSphere&,
				PHPoint&,
				PHPoint&);  
  short  intersectionLineSphere(const PHLine&,
				const PHSphere&,
				PHLine&,
				PHLine&);  
  short  intersectionLineSphereSection(const PHLine&,
				       const PHSphereSection&,
				       PHPoint&,
				       PHPoint&);  
  short  intersectionLineSphereSection(const PHLine&,
				       const PHSphereSection&,
				       PHLine&,
				       PHLine&);  
  //
  // PolyLines and objects
  //
  PHBoolean intersectionPolyLinePlane(const PHPolyLine & ,
				      const PHPlane&,
				      PHPoint&); 
  PHBoolean intersectionPolyLinePlane(const PHPolyLine & ,
				      const PHPlane&,
				      PHLine&); 
  PHBoolean intersectionPolyLinePanel(const PHPolyLine &PolyLine, 
				      const PHPanel &Panel,
				      PHLine &line);
  PHBoolean intersectionPolyLinePanel(const PHPolyLine &PolyLine, 
				      const PHPanel &Panel,
				      PHPoint &point);

  short intersectionPolyLineCylinder(const PHPolyLine&,
				     const PHCylinder&,
				     PHPoint&,
				     PHPoint&);
  short intersectionPolyLineCylinder(const PHPolyLine&,
				     const PHCylinder&,
				     PHLine&,
				     PHLine&);
  short intersectionPolyLineCylinderSection(const PHPolyLine&,
					    const PHCylinderSection&,
					    PHPoint&,
					    PHPoint&);
  short intersectionPolyLineCylinderSection(const PHPolyLine&,
					    const PHCylinderSection&,
					    PHLine&,
					    PHLine&);
  short intersectionPolyLineSphere(const PHPolyLine&,
				   const PHSphere&,
				   PHPoint&,
				   PHPoint&);  
  short intersectionPolyLineSphere(const PHPolyLine&,
				   const PHSphere&,
				   PHLine&, PHLine&);  
};

#endif /* __PHGEOMETRY_H__ */
