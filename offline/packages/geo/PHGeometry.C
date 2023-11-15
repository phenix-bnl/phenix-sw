// Geometry utilities, embedded in a namespace.
// See comments and
// example in PHGeometry.h for more details.

#include "PHGeometry.h"
#include <iostream>
#include <cmath>

template <typename T>
static inline T
sqr (const T& x)
{
  return x * x;
}

using namespace std;

namespace PHGeometry
{
  void 
  cartesianToCylindrical (const PHPoint & cart, PHCylPoint & cyl)
  {
    double r;
    PHAngle phi;

    r = sqrt (cart.getX () * cart.getX () + cart.getY () * cart.getY ());
    if (cart.getX () != 0)
      {
	phi = atan2 (cart.getY (), cart.getX ());
      }
    else
      {
	if (cart.getY () > 0.0)
	  {
	    phi = M_PI / 2.0;
	  }
	else if (cart.getY () < 0.0)
	  {
	    phi = -M_PI / 2.0;
	  }
	else
	  {
	    phi = NAN;
	  }
      }
    cyl.setR (r);
    cyl.setPhi (phi);
    cyl.setZ (cart.getZ ());
  }

  void 
  cylindricalToCartesian (const PHCylPoint & cyl, PHPoint & cart)
  {
    double x, y;

    x = cyl.getR () * cos (cyl.getPhi ());
    y = cyl.getR () * sin (cyl.getPhi ());

    cart.setX (x);
    cart.setY (y);
    cart.setZ (cyl.getZ ());
  }

  void 
  cartesianToSpherical (const PHPoint & cart, PHSphPoint & sph)
  {
    double r;
    PHAngle phi, theta;

    r = sqrt (cart.getX () * cart.getX () 
	      + cart.getY () * cart.getY () 
	      + cart.getZ () * cart.getZ ());

    if (cart.getX () != 0)
      {
	phi = atan2 (cart.getY (), cart.getX ());
      }
    else
      {
	if (cart.getY () > 0)
	  {
	    phi = M_PI / 2;
	  }
	else if (cart.getY () < 0)
	  {
	    phi = -M_PI / 2;
	  }
	else
	  {
	    phi = NAN;
	  }
      }

    if (r != 0.0)
      {
	theta = acos (cart.getZ () / r);
      }
    else
      {
	theta = 0;
      }
    sph.setR (r);
    sph.setPhi (phi);
    sph.setTheta (theta);
  }

  void 
  sphericalToCartesian (const PHSphPoint & sph, PHPoint & cart)
  {
    cart.setX (sph.getR () * sin (sph.getTheta ()) * cos (sph.getPhi ()));
    cart.setY (sph.getR () * sin (sph.getTheta ()) * sin (sph.getPhi ()));
    cart.setZ (sph.getR () * cos (sph.getTheta ()));
  }

  void 
  cylindricalToSpherical (const PHCylPoint & cyl, PHSphPoint & sph)
  {
    PHPoint cart;
    cylindricalToCartesian (cyl, cart);
    cartesianToSpherical (cart, sph);
  }

  void 
  sphericalToCylindrical (const PHSphPoint & sph, PHCylPoint & cyl)
  {
    PHPoint cart;
    sphericalToCartesian (sph, cart);
    cartesianToCylindrical (cart, cyl);
  }

  double 
  distancePointToPoint (const PHPoint & c1, const PHPoint & c2)
  {
    return sqrt (sqr(c1.getX () - c2.getX ()) +
		 sqr(c1.getY () - c2.getY ()) +
		 sqr(c1.getZ () - c2.getZ ()));
  }

  double 
  distancePointToPoint (const PHSphPoint & s1, const PHSphPoint & s2)
  {
    PHPoint c1 = s1;
    PHPoint c2 = s2;

    return distancePointToPoint (c1, c2);
  }

  double 
  distancePointToPoint (const PHCylPoint & cy1, const PHCylPoint & cy2)
  {
    PHPoint c1 = cy1;
    PHPoint c2 = cy2;

    return distancePointToPoint (c1, c2);
  }

  PHVector 
  cross (const PHVector & v1, const PHVector & v2)
  {
    double x, y, z;

    x = v1.getY () * v2.getZ () - v1.getZ () * v2.getY ();
    y = -v1.getX () * v2.getZ () + v1.getZ () * v2.getX ();
    z = v1.getX () * v2.getY () - v1.getY () * v2.getX ();
    PHVector out (x, y, z);

    return out;
  }

  PHAngle 
  angle (const PHVector & v1, const PHVector & v2)
  {
    PHAngle phi = NAN;
    double l1sq, l2sq;

    if (((l1sq = v1.lengthSqr ()) != 0.0) && 
	((l2sq = v2.lengthSqr ()) != 0.0))
      {
	double tmp = v1.dot (v2) / sqrt (l1sq * l2sq);
	tmp = max(min(tmp, 1.0), -1.0);
	phi = acos (tmp);
      }
    else
      {
	PHMessage ("angle", PHError, "Length of one vector is zero ");
      }

    return phi;
  }
  
  PHBoolean
  intersectionLinePlane (const PHLine & line,
			 const PHPlane & plane, 
			 PHPoint & cross)
  {
    PHVector lineVector = line.getDirection ();
    PHVector planeVector = plane.getNormal ();
    
    // vector connecting the basepoints
    PHVector baseVector = plane.getOrigin() - line.getBasepoint();

    double distance, denominator;

    denominator = planeVector.dot (lineVector);

    if (denominator != 0)
      {
	// Line and plane are not parallel
	distance =
	  (planeVector.dot (baseVector)) / denominator;
	cross = line.getBasepoint() + (PHPoint) (lineVector * distance);
	return True;
      }

    // Line and plane are parallel
    return False;
  }

  PHBoolean
  intersectionLinePlane (const PHLine & line,
			 const PHPlane & plane, 
			 PHLine & cross)
  {
    PHPoint crossPoint;

    if (intersectionLinePlane (line, plane, crossPoint))
      {
	cross.setBasepoint (crossPoint);
	cross.setDirection (line.getDirection ());
	return True;
      }

    return False;
  }

  PHBoolean
    intersectionLineTriPanel (const PHLine & line,
			      const PHTriPanel & panel, const PHPoint & cross)
  {
    const PHPoint& p0 = panel.getPoint (0);
    const PHPoint& p1 = panel.getPoint (1);

    const PHVector& v1 = panel.getVectorV1();
    const PHVector& v2 = panel.getVectorV2();
    PHVector tmp1 = cross - p0;

    double angleVector12 = panel.getAngle12();
    double angleVector1tmp = tmp1.angle (v1);
    double angleVector2tmp = tmp1.angle (v2);

    if (fabs (angleVector1tmp) > fabs (angleVector12) ||
	fabs (angleVector2tmp) > fabs (angleVector12))
      {
	return False;
      }

    const PHVector& v1bis = panel.getVectorV1bis();
    const PHVector& v2bis = panel.getVectorV2bis();
    PHVector tmp1bis = cross - p1;
	
    double angleVector12bis = panel.getAngle12bis();
    double angleVector1tmpbis = tmp1bis.angle (v1bis);
    double angleVector2tmpbis = tmp1bis.angle (v2bis);

    if (fabs (angleVector1tmpbis) > fabs (angleVector12bis) ||
	fabs (angleVector2tmpbis) > fabs (angleVector12bis))
      {
	return False;
      }

    return True;
  }

  PHBoolean
  intersectionLineTriPanel (const PHLine & line,
			    const PHTriPanel & panel, PHLine & cross)
  {
    PHPoint crossPoint;

    /*
      because of the new, streamlined intersectionLineTriPanel above
      [with arg list (const PHLine &, const PHTriPanel&, const PHPoint&),
      we must now add the call to fine the plane intersection point 
      ourselves.  Afterwards, pass it along to the new function.
    */

    if(intersectionLinePlane(line, (const PHPlane &)panel, crossPoint)) {
      if (intersectionLineTriPanel (line, panel, crossPoint))
	{
	  cross.setBasepoint (crossPoint);
	  cross.setDirection (line.getDirection ());

	  /*
	    This was missing when I made the above change...which leads
	    me to believe this function is hardly ever called.  I hope...
	  */
	  return True;
	}
    }

    return False;
  }

  // For intersectionLinePanel we use the algorithm of Lagae and Dutre
  // http://www.acm.org/jgt/papers/LagaeDutre05/

  // It's supposed to be at least as fast as two ray-triangle
  // intersection calculations, so there's no need to split the panel
  // into triangles any longer.

  typedef double real;

  class vector
  {
  public:
    vector(real x, real y, real z) { xyz[0] = x; xyz[1] = y; xyz[2] = z; }
    vector(const PHVector &v)
    {
      xyz[0] = v.getX();
      xyz[1] = v.getY();
      xyz[2] = v.getZ();
    }
    
    real x() const { return xyz[0]; }
    real y() const { return xyz[1]; }
    real z() const { return xyz[2]; }
  private:
    real xyz[3];
  };

  static inline real dot(const vector& lhs, const vector& rhs)
  {
    return (lhs.x() * rhs.x()) +  (lhs.y() * rhs.y()) +  (lhs.z() * rhs.z());
  }

  static inline vector cross(const vector& lhs, const vector& rhs)
  {
    return vector((lhs.y() * rhs.z()) - (lhs.z() * rhs.y()),
		  (lhs.z() * rhs.x()) - (lhs.x() * rhs.z()),
		  (lhs.x() * rhs.y()) - (lhs.y() * rhs.x()));
  }

  class point
  {
  public:
    point() {}
    point(real x, real y, real z) { xyz[0] = x; xyz[1] = y; xyz[2] = z; }
    point(const PHPoint &p) {
      xyz[0] = p.getX();
      xyz[1] = p.getY();
      xyz[2] = p.getZ();
    }
    real x() const { return xyz[0]; }  
    real y() const { return xyz[1]; }
    real z() const { return xyz[2]; }
  private:
    real xyz[3];
  };

  static inline vector operator-(const point& lhs, const point& rhs)
  {
    return vector(lhs.x() - rhs.x(), lhs.y() - rhs.y(), lhs.z() - rhs.z());
  }

  class ray
  {
  public:
    ray(const point& origin, const vector& direction)
      : origin_(origin), direction_(direction) {}
    ray(const PHLine &l)
      : origin_(l.getBasepoint()), direction_(l.getDirection()) {}
      
    const point& origin() const { return origin_; }
    const vector& direction() const { return direction_; }
  private:
    point origin_;
    vector direction_;
  };

  class quadrilateral
  {
  public:
    quadrilateral(const point& v_00, const point& v_10,
		  const point& v_11, const point& v_01)
    {
      vertices[0] = v_00;
      vertices[1] = v_10;
      vertices[2] = v_11;
      vertices[3] = v_01;
    }
    quadrilateral(const PHPanel &p)
    {
      vertices[0] = p.getPoint(0);
      vertices[1] = p.getPoint(1);
      vertices[2] = p.getPoint(3);
      vertices[3] = p.getPoint(2);
    }      
    const point& v_00() const { return vertices[0]; }
    const point& v_10() const { return vertices[1]; }
    const point& v_11() const { return vertices[2]; }
    const point& v_01() const { return vertices[3]; }
  private:
    point vertices[4];
  };

  static 
  bool 
  intersect_quadrilateral_ray(const quadrilateral& q,
			      const ray& r, 
			      real& u, 
			      real& v, 
			      real& t)
  {
    static const real eps = real(10e-6);
    
    // Rejects rays that are parallel to Q, and rays that intersect
    // the plane of Q either on the left of the line V00V01 or on the
    // right of the line V00V10.
    
    vector E_01 = q.v_10() - q.v_00();
    vector E_03 = q.v_01() - q.v_00();
    vector P = cross(r.direction(), E_03);
    real det = dot(E_01, P);
    if (std::abs(det) < eps) return false;
    real inv_det = real(1.0) / det;
    vector T = r.origin() - q.v_00();
    real alpha = dot(T, P) * inv_det;
    if (alpha < real(0.0)) return false;
    if (alpha > real(1.0)) return false; // Uncomment if VR is used.
    vector Q = cross(T, E_01);
    real beta = dot(r.direction(), Q) * inv_det;
    if (beta < real(0.0)) return false; 
    if (beta > real(1.0)) return false; // Uncomment if VR is used.
    
    if ((alpha + beta) > real(1.0)) {
      
      // Rejects rays that intersect the plane of Q either on the
      // left of the line V11V10 or on the right of the line V11V01.
      
      vector E_23 = q.v_01() - q.v_11();
      vector E_21 = q.v_10() - q.v_11();
      vector P_prime = cross(r.direction(), E_21);
      real det_prime = dot(E_23, P_prime);
      if (std::abs(det_prime) < eps) return false;
      real inv_det_prime = real(1.0) / det_prime;
      vector T_prime = r.origin() - q.v_11();
      real alpha_prime = dot(T_prime, P_prime) * inv_det_prime;
      if (alpha_prime < real(0.0)) return false;
      vector Q_prime = cross(T_prime, E_23);
      real beta_prime = dot(r.direction(), Q_prime) * inv_det_prime;
      if (beta_prime < real(0.0)) return false;
    }
    
    // Compute the ray parameter of the intersection point, and
    // reject the ray if it does not hit Q.
    
    t = dot(E_03, Q) * inv_det;
    //    if (t < real(0.0)) return false; 
    
    // Compute the barycentric coordinates of the fourth vertex.
    // These do not depend on the ray, and can be precomputed
    // and stored with the quadrilateral.  
    
    real alpha_11, beta_11;
    vector E_02 = q.v_11() - q.v_00();
    vector n = cross(E_01, E_03);
    
    if ((std::abs(n.x()) >= std::abs(n.y()))
	&& (std::abs(n.x()) >= std::abs(n.z()))) {
      
      alpha_11 = ((E_02.y() * E_03.z()) - (E_02.z() * E_03.y())) / n.x();
      beta_11  = ((E_01.y() * E_02.z()) - (E_01.z() * E_02.y())) / n.x();
    }
    else if ((std::abs(n.y()) >= std::abs(n.x()))
	     && (std::abs(n.y()) >= std::abs(n.z()))) {  
      
      alpha_11 = ((E_02.z() * E_03.x()) - (E_02.x() * E_03.z())) / n.y();
      beta_11  = ((E_01.z() * E_02.x()) - (E_01.x() * E_02.z())) / n.y();
    }
    else {
      
      alpha_11 = ((E_02.x() * E_03.y()) - (E_02.y() * E_03.x())) / n.z();
      beta_11  = ((E_01.x() * E_02.y()) - (E_01.y() * E_02.x())) / n.z();
    }
    
    // Compute the bilinear coordinates of the intersection point.
    
    if (std::abs(alpha_11 - real(1.0)) < eps) {    
      
      // Q is a trapezium.
      u = alpha;
      if (std::abs(beta_11 - real(1.0)) < eps) v = beta; // Q is a parallelogram.
      else v = beta / ((u * (beta_11 - real(1.0))) + real(1.0)); // Q is a trapezium.
    }
    else if (std::abs(beta_11 - real(1.0)) < eps) {
      
      // Q is a trapezium.
      v = beta;
      u = alpha / ((v * (alpha_11 - real(1.0))) + real(1.0));
    }
    else {
      
      real A = real(1.0) - beta_11;
      real B = (alpha * (beta_11 - real(1.0)))
	- (beta * (alpha_11 - real(1.0))) - real(1.0);
      real C = alpha;
      real D = (B * B) - (real(4.0) * A * C);
      real Q = real(-0.5) * (B + ((B < real(0.0) ? real(-1.0) : real(1.0))
				  * std::sqrt(D)));
      u = Q / A;
      if ((u < real(0.0)) || (u > real(1.0))) u = C / Q;
      v = beta / ((u * (beta_11 - real(1.0))) + real(1.0)); 
    }
    
    return true;
  }
  
  PHBoolean
  intersectionLinePanel (const PHLine & line,
			 const PHPanel & panel,
			 PHPoint & cross)
  {
    quadrilateral q(panel);
    ray r(line);
    real u, v, t;

    if (intersect_quadrilateral_ray(q, r, u, v, t))
      {
	cross = line.getBasepoint() + line.getDirection() * t;
	return True;
      }
    
    return False;
  }

  PHBoolean
  intersectionLinePanel (const PHLine & line,
			 const PHPanel & panel,
			 PHLine & cross)
  {
    PHPoint crossPoint;

    if (intersectionLinePanel (line, panel, crossPoint))
      {
	cross.setBasepoint (crossPoint);
	cross.setDirection (line.getDirection ());
	return True;
      }

    return False;
  }

  double 
  distanceLinePoint (const PHLine & l, const PHPoint & p)
  {
    const PHPoint &x1 = l.getBasepoint();
    const PHVector &v1 = l.getDirection();
    
    PHVector v2(x1.getX() - p.getX(), 
		x1.getY() - p.getY(),
		x1.getZ() - p.getZ());
    
    return sqrt((v1.cross(v2)).lengthSqr()/v1.lengthSqr());
  }

  PHPoint
  closestApproachLinePoint (const PHLine & line, const PHPoint & point)
  {
    //      /
    //     / .   returns the coordinates of th point on the line that is closest
    //    /      to the input point

    PHPlane myplane (point, line.getDirection ());	// plane perp. to line
    PHPoint cross, out (0.0, 0.0, 0.0);

    if (intersectionLinePlane (line, myplane, cross))
      {
	out = cross;
      }
    else
      {
	PHMessage ("closestApproachLinePoint", PHError,
		   "Plane and Line are parallel or vector length is zero !!!");
	// if an error, plane and line are  parallel or vector length = 0.
	// -->check into intersectionLinePlane
      }
    return out;
  }

  double 
  distanceLineLine (const PHLine &l1, const PHLine &l2)
  {
    static const real eps = real(10e-6);

    const PHVector &u = l1.getDirection ();
    const PHVector &v = l2.getDirection ();
    const PHVector &w = l1.getBasepoint() - l2.getBasepoint();
    
    double a = dot(u,u);
    double b = dot(u,v);
    double c = dot(v,v);
    double d = dot(u,w);
    double e = dot(v,w);

    double D = a*c - b*b;
    double sc, tc;

    // compute the line parameters of the two closest points
    if (D < eps) {         // the lines are almost parallel
      sc = 0.0;
      tc = (b>c ? d/b : e/c);   // use the largest denominator
    }
    else {
      sc = (b*e - c*d) / D;
      tc = (a*e - b*d) / D;
    }
    
    // get the difference of the two closest points
    PHVector   dP = w + (u * sc) - (v * tc);  // = L1(sc) - L2(tc)
    
    return dP.length();   // return the closest distance   
  }

  PHPoint 
  closestApproachLineLine (const PHLine & line1, const PHLine & line2)
  {
    /*
        \(1)  | (2)    returns the point on line2 that is closest to line1
         \    |        if the lines are parallel - prints out the distance
          \   |
           \  |
            \ |
             \
    */

    PHPoint out (-999, -999, -999);
    PHPoint cross;
    double distance;
    PHVector vector1 = line1.getDirection ();
    PHVector vector2 = line2.getDirection ();
    PHAngle angleBetweenLines = vector1.angle (vector2);

    if (angleBetweenLines == 0.0 || angleBetweenLines == M_PI)
      {				// if lines are parallel
	PHPlane myplane (line2.getBasepoint (), line2.getDirection ());
	if (intersectionLinePlane (line1, myplane, cross))
	  {
	    distance = (PHVector (cross - line2.getBasepoint ())).length ();
	    PHMessage ("closestApproachLineLine", PHError,
		       "lines are parallel");
	    cout << "distance between lines  " << distance << endl;
	  }
	else
	  {
	    PHMessage ("closestApproachLineLine", PHError, "no intersection");
	  }
      }
    else
      {
	PHVector connection = vector1.cross (vector2);
	PHVector normal = vector1.cross (connection);
	PHPlane myplane (line1.getBasepoint (), normal);
	if (intersectionLinePlane (line2, myplane, cross))
	  {
	    out = cross;

	  }
	else
	  {
	    PHMessage ("closestApproachLineLine", PHError, "no intersection");
	  }
      }
    return out;
  }

  PHLine 
  projectLineIntoPlane (const PHLine & line, const PHPlane & plane)
  {
    PHVector vector = line.getDirection ();
    PHPoint point = line.getBasepoint ();
    PHVector normal = plane.getNormal ();
    PHPoint origin = plane.getOrigin ();

    // Project direction vector of line onto plane

    PHVector v_projected = vector - normal * (vector.dot (normal));

    //Project basepoint of line onto plane

    PHPoint p_projected, p_ontonormal;
    PHVector newvector = point - origin;

    p_ontonormal = normal * newvector.dot (normal);
    p_projected = point - p_ontonormal;

    PHLine newline (p_projected, v_projected);

    return newline;
  }

  PHPoint 
  transformPoint (const PHFrame & F1, 
		  const PHPoint & p1,
		  const PHFrame & F2)
    // written by Julia Velkovska : email julia@bnl.gov Transform a
    // point in one coordinate frame, F2 to another coordinate frame,
    // F2.  Return the transformed point.
  {
    PHMatrix rotation;
    PHVector translation;
    frames2MatrixAndVector (F1, F2, rotation, translation);
    PHPoint outPoint = transformPoint (rotation, translation, p1);

    if (fabs (outPoint.getX ()) < 1.0e-14)
      outPoint.setX (0);
    if (fabs (outPoint.getY ()) < 1.0e-14)
      outPoint.setY (0);
    if (fabs (outPoint.getZ ()) < 1.0e-14)
      outPoint.setZ (0);

    return outPoint;
  }

  PHPoint 
  transformPoint (const PHMatrix & m, 
		  const PHVector & t,
		  const PHPoint & p)
    // written by Julia Velkovska : email   julia@bnl.gov
  {
    PHVector p_v = p;
    PHVector out_v = m * p_v + t;
    PHPoint outPoint = out_v;

    if (fabs (outPoint.getX ()) < 1.0e-14)
      outPoint.setX (0);
    if (fabs (outPoint.getY ()) < 1.0e-14)
      outPoint.setY (0);
    if (fabs (outPoint.getZ ()) < 1.0e-14)
      outPoint.setZ (0);

    return outPoint;
  }

  PHVector transformVector (const PHFrame & F1, const PHVector & v,
			    const PHFrame & F2)
  {
    PHPoint origin = F1.getOrigin ();
    PHVector transformed;

    PHPoint point = origin + (PHPoint) v;
    PHPoint newOrigin = transformPoint (F1, origin, F2);
    PHPoint newPoint = transformPoint (F1, point, F2);

    transformed = (PHVector) (newPoint - newOrigin);
    if (fabs (transformed.getX ()) < 1.0e-14)
      transformed.setX (0);
    if (fabs (transformed.getY ()) < 1.0e-14)
      transformed.setY (0);
    if (fabs (transformed.getZ ()) < 1.0e-14)
      transformed.setZ (0);

    return transformed;
  }

  PHVector transformVector (const PHMatrix & m, const PHVector & v)
    // written by Julia Velkovska : email   julia@bnl.gov
  {
    PHVector out = m * v;
    return out;
  }

  PHLine transformLine (const PHFrame & F1, const PHLine & line,
			const PHFrame & F2)
  {
    PHPoint point = line.getBasepoint ();
    PHVector vector = line.getDirection ();

    PHPoint transformed_point = transformPoint (F1, point, F2);
    PHVector transformed_vector = transformVector (F1, vector, F2);

    PHLine transformed (transformed_point, transformed_vector);

    return transformed;
  }

  PHLine transformLine (const PHMatrix & m, const PHVector & translation,
			const PHLine & line)
    // written by Julia Velkovska : email   julia@bnl.gov
  {
    PHPoint point = line.getBasepoint ();
    PHVector vector = line.getDirection ();

    PHPoint transformed_point = transformPoint (m, translation, point);
    PHVector transformed_vector = transformVector (m, vector);

    PHLine transformed (transformed_point, transformed_vector);

    return transformed;
  }

  PHPanel transformPanel (const PHMatrix & m, const PHVector & translation,
			  const PHPanel & panelIn)
    // written by Julia Velkovska : email   julia@bnl.gov
  {
    PHPoint p0, p1, p2, p0out, p1out, p2out;
    p0 = panelIn.getPoint (0);
    p1 = panelIn.getPoint (1);
    p2 = panelIn.getPoint (2);
    p0out = transformPoint (m, translation, p0);
    p1out = transformPoint (m, translation, p1);
    p2out = transformPoint (m, translation, p2);
    PHPanel panelOut (p0out, p1out, p2out);
    return panelOut;
  }

  PHPanel transformPanel (const PHFrame & F1, const PHPanel & panelIn,
			  const PHFrame & F2)
    // written by Julia Velkovska : email   julia@bnl.gov
  {
    PHPoint p0, p1, p2, p0out, p1out, p2out;
    p0 = panelIn.getPoint (0);
    p1 = panelIn.getPoint (1);
    p2 = panelIn.getPoint (2);
    p0out = transformPoint (F1, p0, F2);
    p1out = transformPoint (F1, p1, F2);
    p2out = transformPoint (F1, p2, F2);
    PHPanel panelOut (p0out, p1out, p2out);
    return panelOut;
  }

 PHPanel  rotatePanelAboutCenter(const double &angle,const PHVector& axis,
			  const PHPanel& panelIn)
    // written by Julia Velkovska : email   julia@bnl.gov
 {
   // this function rotates the panel in positive direction to an angle  
   // about an axis that is passing through
   // its center in direction of the input vector axis  

   PHPoint center = panelIn.getCenter();
   // 1)translate the panel so that its center sits at (0,0,0)
   // 2)get the rotation matrix for rotation about an axis passing through
   // the origin of the coordinate system in direction axis
   // 3) rotate the panel
   // 4) translate the panel back
   PHVector translation(-center.getX(),-center.getY(),-center.getZ());
   PHMatrix dontRotate;  // no rotation for now
   PHPanel temp1 = transformPanel(dontRotate,translation,panelIn);
   PHMatrix rotation = rotationMatrix(angle,axis);
   PHVector dontTranslate;
   PHPanel temp2 =  transformPanel(rotation,dontTranslate,temp1); 
   PHVector translateBack = center;
   PHPanel panelOut =  transformPanel(dontRotate,translateBack,temp2);
   return panelOut;
 } 

  PHPanel  expandPanel(const PHPanel& panel, const double factor)
    // "factor" is the factor by which the magnitude of the vector from
    // the center of the panel to one corner is increased.  For example,
    // factor = 1.2 will increase the vector's magnitude 120%. In order
    // to make the panel smaller, use values of "factor" < 1.  For factor = 1 the
    // size of the panel remains unchanged. --BL (brian.t.love@vanderbilt.edu).
  {
    PHPanel panelOut;
    if(factor==1)
      { 
	panelOut = panel; 
      }
    else
      {
	PHPoint p[3];
	p[0] = panel.getPoint(0);
	p[1] = panel.getPoint(1);
	p[2] = panel.getPoint(2);
	
	PHPoint center = panel.getCenter();
	PHVector vC(center.getX(),center.getY(),center.getZ());
	
	for(int i=0; i<3; i++)
	  {
	    double x, y, z;
	    x = p[i].getX();
	    y = p[i].getY();
	    z = p[i].getZ();
	    PHVector temp(x, y, z);
	    PHVector diff = vC - temp;
	    PHVector newdiff = diff * factor;
	    PHVector newcorner  = vC - newdiff;
	    PHPoint newpoint(newcorner.getX(),newcorner.getY(),newcorner.getZ());
	    p[i] = newpoint;
	  }

	panelOut = PHPanel(p[0], p[1], p[2]);
      }
    
    return panelOut;
  }

  PHCylinder transformCylinder (const PHFrame & F1, const PHCylinder & cylIn,
				const PHFrame & F2)
    // written by Julia Velkovska : email   julia@bnl.gov
  {
    PHMatrix rotation;
    PHVector translation;
    frames2MatrixAndVector (F1, F2, rotation, translation);
    PHCylinder newCylinder = transformCylinder (rotation, translation, cylIn);
    return newCylinder;
  }

  PHCylinder transformCylinder (const PHMatrix & rotation,
				const PHVector & translation,
				const PHCylinder & cylIn)
    // written by Julia Velkovska : email   julia@bnl.gov
  {
    PHVector axis = cylIn.getAxis ();
    PHPoint center = cylIn.getCenter ();
    double radius = cylIn.getRadius ();
    PHVector newAxis = transformVector (rotation, axis);
    PHPoint newCenter = transformPoint (rotation, translation, center);
    PHCylinder newCylinder (newCenter, radius, newAxis);
    return newCylinder;
  }

  PHCylinderSection transformCylinderSection (const PHFrame & F1,
					      const PHCylinderSection & cylIn,
					      const PHFrame & F2)
    // written by Julia Velkovska : email   julia@bnl.gov
  {
    PHMatrix rotation;
    PHVector translation;
    frames2MatrixAndVector (F1, F2, rotation, translation);
    PHCylinderSection newCylinderSection =
      transformCylinderSection (rotation, translation, cylIn);
    return newCylinderSection;
  }

  PHCylinderSection transformCylinderSection (const PHMatrix & rotation,
					      const PHVector & translation,
					      const PHCylinderSection & cylIn)
    // written by Julia Velkovska : email   julia@bnl.gov
  {
    // the angle range is not subject to transformation - 
    //only the frame associated with the cylinder section which determines how the angle is measured
    PHVector axis = cylIn.getAxis ();
    PHPoint center = cylIn.getCenter ();
    double radius = cylIn.getRadius ();
    PHFrame cylFrame = cylIn.getFrame ();
    PHAngle cylPhiLower = cylIn.getPhiLower ();
    PHAngle cylPhiUpper = cylIn.getPhiUpper ();

    PHVector newAxis = transformVector (rotation, axis);
    PHPoint newCenter = transformPoint (rotation, translation, center);

    PHCylinderSection newCylinderSection (newCenter, radius, newAxis);

    PHVector newX = transformVector (rotation, (PHVector) cylFrame.getU ());
    PHVector newY = transformVector (rotation, (PHVector) cylFrame.getV ());
    PHVector newZ = transformVector (rotation, (PHVector) cylFrame.getW ());
    PHFrame newFrame (newCenter, newX, newY, newZ);
    newCylinderSection.setFrame (newFrame);
    newCylinderSection.setPhiRange (cylPhiLower, cylPhiUpper);

    return newCylinderSection;
  }
  PHMatrix rotationMatrix (const double &angle, const PHVector & axis)
    // written by Julia Velkovska : email   julia@bnl.gov
  {
    PHVector Xaxis (1, 0, 0);
    PHVector Yaxis (0, 1, 0);
    PHVector Zaxis (0, 0, 1);

    double c0 = Xaxis.dot (axis) / axis.length ();
    double c1 = Yaxis.dot (axis) / axis.length ();
    double c2 = Zaxis.dot (axis) / axis.length ();

    PHMatrix one (Xaxis, Yaxis, Zaxis);

    PHVector two0 (c0 * c0, c0 * c1, c0 * c2);
    PHVector two1 (c1 * c0, c1 * c1, c1 * c2);
    PHVector two2 (c2 * c0, c2 * c1, c2 * c2);
    PHMatrix two (two0, two1, two2);

    PHVector three0 (0, -c2, c1);
    PHVector three1 (c2, 0, -c0);
    PHVector three2 (-c1, c0, 0);
    PHMatrix three (three0, three1, three2);

    PHMatrix rotation =
      one * cos (angle) + two * (1 - cos (angle)) + three * sin (angle);
    return rotation;
  }

  void frames2MatrixAndVector (const PHFrame& F1, const PHFrame& F2,
			       PHMatrix& rotation, PHVector& translation)
  {
    const PHVector Xaxis (1, 0, 0);
    const PHVector Yaxis (0, 1, 0);
    const PHVector Zaxis (0, 0, 1);

    const PHVector u1(F1.getU().normalized());
    const PHVector v1(F1.getV().normalized());
    const PHVector w1(F1.getW().normalized());
    const PHVector u2(F2.getU().normalized());
    const PHVector v2(F2.getV().normalized());
    const PHVector w2(F2.getW().normalized());

    //Calculate the direction cosines:
    const PHVector dir_cosX12(u2.dot(u1), u2.dot(v1), u2.dot(w1));
    const PHVector dir_cosY12(v2.dot(u1), v2.dot(v1), v2.dot(w1));
    const PHVector dir_cosZ12(w2.dot(u1), w2.dot(v1), w2.dot(w1));

    rotation = PHMatrix(dir_cosX12, dir_cosY12, dir_cosZ12);

    const PHVector dir_cosX20(Xaxis.dot(u2), Xaxis.dot(v2), Xaxis.dot(w2));
    const PHVector dir_cosY20(Yaxis.dot(u2), Yaxis.dot(v2), Yaxis.dot(w2));
    const PHVector dir_cosZ20(Zaxis.dot(u2), Zaxis.dot(v2), Zaxis.dot(w2));
    const PHMatrix r02 = PHMatrix(dir_cosX20, dir_cosY20, dir_cosZ20).transpose();

    const PHVector origin1(F1.getOrigin());
    const PHVector origin2(F2.getOrigin());

    translation = r02 * (origin1 - origin2);
  }

  PHFrame MatrixAndVector2frames (const PHFrame & F1,
				  const PHMatrix & rotation,
				  const PHVector & translation)
    // written by Julia Velkovska : email   julia@bnl.gov
  {
    PHVector u1, v1, w1;
    u1 = F1.getU ();
    v1 = F1.getV ();
    w1 = F1.getW ();

    PHMatrix r = rotation.transpose ();
    PHVector origin1 = F1.getOrigin ();

    PHVector u2 = r * (u1);
    PHVector v2 = r * (v1);
    PHVector w2 = r * (w1);

    PHVector dir_cosX20;
    PHVector dir_cosY20;
    PHVector dir_cosZ20;

    PHVector Xaxis (1, 0, 0);
    PHVector Yaxis (0, 1, 0);
    PHVector Zaxis (0, 0, 1);

    dir_cosX20.setX(Xaxis.dot(u2));
    dir_cosX20.setY(Xaxis.dot(v2));
    dir_cosX20.setZ(Xaxis.dot(w2));
		    	          
    dir_cosY20.setX(Yaxis.dot(u2));
    dir_cosY20.setY(Yaxis.dot(v2));
    dir_cosY20.setZ(Yaxis.dot(w2));
		    	          
    dir_cosZ20.setX(Zaxis.dot(u2));
    dir_cosZ20.setY(Zaxis.dot(v2));
    dir_cosZ20.setZ(Zaxis.dot(w2));

    PHMatrix r20 (dir_cosX20, dir_cosY20, dir_cosZ20);
    PHVector origin2 = origin1 - r20 * translation;
    PHPoint o2 = origin2;

    PHFrame F2 (o2, u2, v2);

    return F2;
  }

  PHBoolean intersectionLineLineOnPlane (const PHLine & line1,
					 const PHLine & line2,
					 const PHPlane & plane,
					 PHPoint & transformcross)
  {
    //Rotate the lines and plane from the XYZ frame to the plane's reference frame.

    PHFrame XYZ;

    PHVector w = plane.getNormal ();
    PHVector u = plane.getNormal ().orthogonal ();
    PHVector v = w.cross (u);

    PHFrame planeFrame (plane.getOrigin (), u, v, w);

    PHLine newLine1 = transformLine (XYZ, line1, planeFrame);
    PHLine newLine2 = transformLine (XYZ, line2, planeFrame);

    PHLine planeLine (plane.getOrigin (), plane.getNormal ());
    PHLine newPlaneLine = transformLine (XYZ, planeLine, planeFrame);
    PHPlane newPlane (newPlaneLine.getBasepoint (),
		      newPlaneLine.getDirection ());

    PHLine projected1 = projectLineIntoPlane (newLine1, newPlane);
    PHLine projected2 = projectLineIntoPlane (newLine2, newPlane);

    PHVector vector1 = projected1.getDirection ();
    PHVector vector2 = projected2.getDirection ();

    PHPoint point1 = projected1.getBasepoint ();
    PHPoint point2 = projected2.getBasepoint ();

    //The code that follows assumes XY plane (the lines have a form of y=mx+n)

    double m1, m2;		//slopes of lines
    double n1, n2;		//y-intercepts of lines

    PHBoolean flag = False;
    PHPoint cross;

    double tmpDistance;

    if (vector1.length () == 0 && vector2.length () == 0)
      {
	tmpDistance = distancePointToPoint (point1, point2);
	if (tmpDistance == 0)
	  {
	    cross = point1;
	    flag = True;
	  }
      }
    else if (vector1.length () == 0)
      {
	PHLine tmpline (point2, vector2);
	PHPoint tmppoint = point1;
	tmpDistance = distanceLinePoint (tmpline, tmppoint);
	if (tmpDistance == 0)
	  {
	    cross = tmppoint;
	    flag = True;
	  }
      }
    else if (vector2.length () == 0)
      {
	PHLine tmpline (point1, vector1);
	PHPoint tmppoint = point2;
	tmpDistance = distanceLinePoint (tmpline, tmppoint);
	if (tmpDistance == 0)
	  {
	    cross = tmppoint;
	    flag = True;
	  }
      }
    else if (vector1.getX () != 0 && vector2.getX () != 0)
      {
	m1 = vector1.getY () / vector1.getX ();
	m2 = vector2.getY () / vector2.getX ();
	n1 = point1.getY () - m1 * (point1.getX ());
	n2 = point2.getY () - m2 * (point2.getX ());
	if (m1 != m2)
	  {			// not parallel case
	    cross.setX (-(n2 - n1) / (m2 - m1));
	    cross.setY ((n1 * m2 - m1 * n2) / (m2 - m1));
	    flag = True;
	  }
	else if (m1 == m2)
	  {
	    if (n1 == n2)
	      {
		PHMessage ("intersectionLineLineOnPlane", PHWarning,
			   "Infinite intersections ");
		flag = False;
	      }
	    else
	      {
		flag = False;
	      }
	  }
      }
    else if (vector1.getX () != 0 && vector2.getX () == 0)
      {
	m1 = vector1.getY () / vector1.getX ();
	n1 = point1.getY () - m1 * (point1.getX ());
	// m2 infinity
	n2 = point2.getX ();
	cross.setX (n2);
	cross.setY (m1 * n2 + n1);
	flag = True;
      }
    else if (vector1.getX () == 0 && vector2.getX () != 0)
      {
	// m1 infinity
	n1 = point1.getX ();
	m2 = vector2.getY () / vector2.getX ();
	n2 = point2.getY () - m2 * (point2.getX ());
	cross.setX (n1);
	cross.setY (m2 * n1 + n2);
	flag = True;
      }
    else if (vector1.getX () == 0 && vector2.getX () == 0)
      {
	n1 = point1.getX ();
	n2 = point2.getX ();
	if (n1 == n2)
	  {
	    PHMessage ("intersectionLineLineOnPlane", PHWarning,
		       "Infinite intersections ");
	    flag = False;
	  }
	else
	  {
	    flag = False;
	  }
      }
    if (flag)
      {				// go back to the plane's reference frame 
	transformcross = transformPoint (planeFrame, cross, XYZ);
      }
    return flag;
  }

  PHBoolean intersectionPolyLinePlane (const PHPolyLine & PolyLine,
				       const PHPlane & Plane, PHPoint & point)
  {
    // For simplicity, we assume slowly varying polylines that onlyintersect the plane at a single point.

    PHBoolean ifIntersect = False;
    PHPoint potentialPoint;

    PHPoint p1, p2;
    double dist1, dist2, length;

    p1 = *(PolyLine.getPoint (0));
    for (int i = 1; i < (int) PolyLine.numberOfPoints (); i++)
      {
	p2 = *(PolyLine.getPoint ((size_t) i));
	PHLine thisLine (p1, p2);
	length = thisLine.length ();
	if (intersectionLinePlane (thisLine, Plane, potentialPoint))
	  {
	    dist1 = distancePointToPoint (p1, potentialPoint);
	    dist2 = distancePointToPoint (p2, potentialPoint);
	    if (dist1 <= length && dist2 <= length)
	      {
		point = potentialPoint;
		ifIntersect = True;
		return ifIntersect;
	      }
	  }
	p1 = p2;
      }
    // return only one intersection (the first one)
    return ifIntersect;

  }
  PHBoolean intersectionPolyLinePlane (const PHPolyLine & PolyLine,
				       const PHPlane & Plane, PHLine & line)
  {
    // For simplicity, we assume slowly varying polylines that only intersect the plane at a single point.

    PHBoolean ifIntersect = False;
    PHPoint potentialPoint;
    PHLine potentialLine;

    PHPoint p1, p2;
    double dist1, dist2, length;

    p1 = *(PolyLine.getPoint (0));
    for (int i = 1; i < (int) PolyLine.numberOfPoints (); i++)
      {
	p2 = *(PolyLine.getPoint ((size_t) i));
	PHLine thisLine (p1, p2);
	length = thisLine.length ();
	if (intersectionLinePlane (thisLine, Plane, potentialLine))
	  {
	    potentialPoint = potentialLine.getBasepoint ();
	    dist1 = distancePointToPoint (p1, potentialPoint);
	    dist2 = distancePointToPoint (p2, potentialPoint);
	    if (dist1 <= length && dist2 <= length)
	      {
		line = potentialLine;
		ifIntersect = True;
		return ifIntersect;
	      }
	  }
	p1 = p2;
      }
    // return only one intersection (the first one)
    return ifIntersect;

  }

  PHBoolean intersectionPolyLinePanel (const PHPolyLine & PolyLine,
				       const PHPanel & Panel, PHPoint & point)
  {
    PHLine tempLine;
    if (intersectionPolyLinePanel (PolyLine, Panel, tempLine))
      {
	point = tempLine.getBasepoint ();
	return True;
      }
    else
      {
	return False;
      }

  }
  PHBoolean intersectionPolyLinePanel (const PHPolyLine & PolyLine,
				       const PHPanel & Panel, PHLine & line)
  {
    long numOfPLPoints = PolyLine.numberOfPoints ();

    for (long i = 0; i < numOfPLPoints - 1; i++)
      {
	PHLine tempLine (*(PolyLine.getPoint (i)),
			 *(PolyLine.getPoint (i + 1)));

	PHPoint tempPoint;
	if (intersectionLinePanel (tempLine, Panel, tempPoint))
	  {
	    double length = tempLine.getDirection ().length ();
	    double dist1 =
	      distancePointToPoint (*(PolyLine.getPoint (i)), tempPoint);
	    double dist2 =
	      distancePointToPoint (*(PolyLine.getPoint (i + 1)), tempPoint);

	    if (dist1 <= length && dist2 <= length)
	      {
		line.setBasepoint (tempPoint);
		line.setDirection (tempLine.getDirection ());
		return True;
	      }

	  }
      }
    return False;

  }

  short intersectionPolyLineCylinder (const PHPolyLine & PolyLine,
				      const PHCylinder & Cylinder,
				      PHPoint & point1, PHPoint & point2)
  {
    // For simplicity, we assume slowly varying polylines that only intersect the cylinder at two points.
    // Define line-cylinder plane normal vector in algorithm for each line but here
    // set origin to cylinder's center.

    PHPoint p1, p2;
    double dist1Cross1, dist2Cross1;
    double dist1Cross2, dist2Cross2;
    double length;

    short flag = 0;

    p1 = *(PolyLine.getPoint (0));
    for (int i = 1; i < (int) PolyLine.numberOfPoints (); i++)
      {
	p2 = *(PolyLine.getPoint ((size_t) i));
	PHPoint cross1;
	PHPoint cross2;

	PHLine thisLine (p1, p2);	// p1 and p2 are the end point of this  line segment
	length = thisLine.length ();

	short num =
	  intersectionLineCylinder (thisLine, Cylinder, cross1, cross2);
	if (num == 1)
	  {
	    dist1Cross1 = distancePointToPoint (p1, cross1);
	    dist2Cross1 = distancePointToPoint (p2, cross1);
	    if (dist1Cross1 <= length && dist2Cross1 <= length)
	      {
		if (flag == 0)
		  {
		    point1 = cross1;
		  }
		else if (flag == 1)
		  {
		    point2 = cross1;
		  }
		else
		  {
		    PHMessage ("intersectionPolyLineCylinder", PHWarning,
			       "you have already two points");
		  }
		flag++;
	      }
	  }
	else if (num == 2)
	  {
	    dist1Cross1 = distancePointToPoint (p1, cross1);
	    dist2Cross1 = distancePointToPoint (p2, cross1);

	    dist1Cross2 = distancePointToPoint (p1, cross2);
	    dist2Cross2 = distancePointToPoint (p2, cross2);
	    if (dist1Cross1 <= length && dist2Cross1 <= length)
	      {
		if (flag == 0)
		  {
		    point1 = cross1;
		  }
		else if (flag == 1)
		  {
		    point2 = cross1;
		  }
		else
		  {
		    PHMessage ("intersectionPolyLineCylinder", PHWarning,
			       "you have already two points");
		  }
		flag++;
	      }
	    if (dist1Cross2 <= length && dist2Cross2 <= length)
	      {
		if (flag == 0)
		  {
		    point1 = cross2;
		  }
		else if (flag == 1)
		  {
		    point2 = cross2;
		  }
		else
		  {
		    PHMessage ("intersectionPolyLineCylinder", PHWarning,
			       "you have already two points");
		  }
		flag++;
	      }
	  }
	p1 = p2;
      }
    // you return a maximum of two points (the first 2) but the flag will indicate the correct number of them
    return flag;
  }

  short intersectionPolyLineCylinder (const PHPolyLine & PolyLine,
				      const PHCylinder & Cylinder,
				      PHLine & line1, PHLine & line2)
  {
    // For simplicity, we assume slowly varying polylines that only intersect the cylinder at two points.
    // Define line-cylinder plane normal vector in algorithm for each line but here
    // set origin to cylinder's center.

    PHPoint p1, p2;
    double dist1Cross1, dist2Cross1;
    double dist1Cross2, dist2Cross2;
    double length;

    short flag = 0;

    p1 = *(PolyLine.getPoint (0));
    for (int i = 1; i < (int) PolyLine.numberOfPoints (); i++)
      {
	p2 = *(PolyLine.getPoint ((size_t) i));
	PHPoint cross1;
	PHPoint cross2;
	PHLine crossLine1;
	PHLine crossLine2;

	PHLine thisLine (p1, p2);	// p1 and p2 are the end point of this  line segment
	length = thisLine.length ();

	short num =
	  intersectionLineCylinder (thisLine, Cylinder, crossLine1,
				    crossLine2);
	if (num == 1)
	  {
	    cross1 = crossLine1.getBasepoint ();
	    dist1Cross1 = distancePointToPoint (p1, cross1);
	    dist2Cross1 = distancePointToPoint (p2, cross1);
	    if (dist1Cross1 <= length && dist2Cross1 <= length)
	      {
		if (flag == 0)
		  {
		    line1 = crossLine1;
		  }
		else if (flag == 1)
		  {
		    line2 = crossLine1;
		  }
		else
		  {
		    PHMessage ("intersectionPolyLineCylinder", PHWarning,
			       "you have already two points");
		  }
		flag++;
	      }
	  }
	else if (num == 2)
	  {
	    cross1 = crossLine1.getBasepoint ();
	    cross2 = crossLine2.getBasepoint ();
	    dist1Cross1 = distancePointToPoint (p1, cross1);
	    dist2Cross1 = distancePointToPoint (p2, cross1);

	    dist1Cross2 = distancePointToPoint (p1, cross2);
	    dist2Cross2 = distancePointToPoint (p2, cross2);
	    if (dist1Cross1 <= length && dist2Cross1 <= length)
	      {
		if (flag == 0)
		  {
		    line1 = crossLine1;
		  }
		else if (flag == 1)
		  {
		    line2 = crossLine1;
		  }
		else
		  {
		    PHMessage ("intersectionPolyLineCylinder", PHWarning,
			       "you have already two points");
		  }
		flag++;
	      }

	    if (dist1Cross2 <= length && dist2Cross2 <= length)
	      {
		if (flag == 0)
		  {
		    line1 = crossLine2;
		  }
		else if (flag == 1)
		  {
		    line2 = crossLine2;
		  }
		else
		  {
		    PHMessage ("intersectionPolyLineCylinder", PHWarning,
			       "you have already two points");
		  }
		flag++;
	      }
	  }
	p1 = p2;
      }
    // you return a maximum of two points (the first 2) but the flag will indicate the correct number of them
    return flag;
  }

  short intersectionPolyLineSphere (const PHPolyLine & PolyLine,
				    const PHSphere & Sphere, PHPoint & point1,
				    PHPoint & point2)
  {
    // For simplicity, we assume slowly varying polylines that only intersect the cylinder at two points.
    // Define line-cylinder plane normal vector in algorithm for each line but here
    // set origin to cylinder's center.

    PHPoint p1, p2;
    double dist1Cross1, dist2Cross1;
    double dist1Cross2, dist2Cross2;
    double length;

    short flag = 0;

    p1 = *(PolyLine.getPoint (0));
    for (int i = 1; i < (int) PolyLine.numberOfPoints (); i++)
      {
	p2 = *(PolyLine.getPoint ((size_t) i));
	PHPoint cross1;
	PHPoint cross2;
	PHLine thisLine (p1, p2);
	length = thisLine.length ();
	// p1 and p2 are the end points which define the segment line

	short num = intersectionLineSphere (thisLine, Sphere, cross1, cross2);
	if (num == 1)
	  {
	    // only one (cross1) intersection point with the sphere
	    dist1Cross1 = distancePointToPoint (p1, cross1);
	    dist2Cross1 = distancePointToPoint (p2, cross1);
	    if (dist1Cross1 <= length && dist2Cross1 <= length)
	      {
		if (flag == 0)
		  {
		    point1 = cross1;
		  }
		else if (flag == 1)
		  {
		    point2 = cross1;
		  }
		else
		  {
		    PHMessage ("intersectionPolyLineSphere", PHWarning,
			       "you have already two points");
		  }
		flag++;		//
	      }
	  }
	else if (num == 2)
	  {
	    dist1Cross1 = distancePointToPoint (p1, cross1);
	    dist2Cross1 = distancePointToPoint (p2, cross1);
	    dist1Cross2 = distancePointToPoint (p1, cross2);
	    dist2Cross2 = distancePointToPoint (p2, cross2);

	    if (dist1Cross1 <= length && dist2Cross1 <= length)
	      {
		if (flag == 0)
		  {
		    point1 = cross1;
		  }
		else if (flag == 1)
		  {
		    point2 = cross1;
		  }
		else
		  {
		    PHMessage ("intersectionPolyLineSphere", PHWarning,
			       "you have already two points");
		  }
		flag++;		//       
	      }
	    if (dist1Cross2 <= length && dist2Cross2 <= length)
	      {
		if (flag == 0)
		  {
		    point1 = cross2;
		  }
		else if (flag == 1)
		  {
		    point2 = cross2;
		  }
		else
		  {
		    PHMessage ("intersectionPolyLineSphere", PHWarning,
			       "you have already two points");
		  }
		flag++;		//       

	      }
	  }
	p1 = p2;
      }
    // you return a maximum of two points (the first 2) but the flag will indicate the correct number of them.
    return flag;

  }
  short intersectionPolyLineSphere (const PHPolyLine & PolyLine,
				    const PHSphere & Sphere, PHLine & line1,
				    PHLine & line2)
  {
    // For simplicity, we assume slowly varying polylines that only intersect the cylinder at two points.
    // Define line-cylinder plane normal vector in algorithm for each line but here
    // set origin to cylinder's center.

    PHPoint p1, p2;
    double dist1Cross1, dist2Cross1;
    double dist1Cross2, dist2Cross2;
    double length;

    short flag = 0;

    p1 = *(PolyLine.getPoint (0));
    for (int i = 1; i < (int) PolyLine.numberOfPoints (); i++)
      {
	p2 = *(PolyLine.getPoint ((size_t) i));
	PHPoint cross1;
	PHPoint cross2;
	PHLine crossLine1, crossLine2;
	PHLine thisLine (p1, p2);
	length = thisLine.length ();
	// p1 and p2 are the end points which define the segment line

	short num =
	  intersectionLineSphere (thisLine, Sphere, crossLine1, crossLine2);
	if (num == 1)
	  {
	    // only one (cross1) intersection point with the sphere
	    cross1 = crossLine1.getBasepoint ();
	    dist1Cross1 = distancePointToPoint (p1, cross1);
	    dist2Cross1 = distancePointToPoint (p2, cross1);
	    if (dist1Cross1 <= length && dist2Cross1 <= length)
	      {
		if (flag == 0)
		  {
		    line1 = crossLine1;
		  }
		else if (flag == 1)
		  {
		    line2 = crossLine1;
		  }
		else
		  {
		    PHMessage ("intersectionPolyLineSphere", PHWarning,
			       "you have already two points");
		  }
		flag++;		//
	      }
	  }
	else if (num == 2)
	  {
	    cross1 = crossLine1.getBasepoint ();
	    cross2 = crossLine2.getBasepoint ();
	    dist1Cross1 = distancePointToPoint (p1, cross1);
	    dist2Cross1 = distancePointToPoint (p2, cross1);
	    dist1Cross2 = distancePointToPoint (p1, cross2);
	    dist2Cross2 = distancePointToPoint (p2, cross2);

	    if (dist1Cross1 <= length && dist2Cross1 <= length)
	      {
		if (flag == 0)
		  {
		    line1 = crossLine1;
		  }
		else if (flag == 1)
		  {
		    line2 = crossLine1;
		  }
		else
		  {
		    PHMessage ("intersectionPolyLineSphere", PHWarning,
			       "you have already two points");
		  }
		flag++;		//       
	      }
	    if (dist1Cross2 <= length && dist2Cross2 <= length)
	      {
		if (flag == 0)
		  {
		    line1 = crossLine2;
		  }
		else if (flag == 1)
		  {
		    line2 = crossLine2;
		  }
		else
		  {
		    PHMessage ("intersectionPolyLineSphere", PHWarning,
			       "you have already two points");
		  }
		flag++;		//       

	      }
	  }
	p1 = p2;
      }
    // you return a maximum of two points (the first 2) but the flag will indicate the correct number of them.
    return flag;

  }

  short intersectionLineSphere (const PHLine & line, const PHSphere & sph,
				PHPoint & cross1, PHPoint & cross2)
  {
    PHPoint linePoint = line.getBasepoint ();
    PHVector lineVector = line.getDirection ();
    lineVector.normalize ();

    PHPoint center = sph.getCenter ();
    double radius = sph.getRadius ();

    // Calculate the coefficient of the quadratic equation
    double a = lineVector.dot (lineVector);
    PHVector tmpVector = linePoint - center;
    double b = 2.0 * (lineVector.dot (tmpVector));
    double c = tmpVector.dot (tmpVector) - radius * radius;
    double d = b * b - 4.0 * a * c;

    // if d <0 or a = 0, there is no intercept
    if (d < 0.0 || a == 0)
      return 0;

    // if d = 0; there is only one intercept at  -b/2a
    if (d == 0)
      {
	cross1 = linePoint + PHPoint (lineVector * (-b / (2 * a)));
	return 1;
      }
    // if d > 0, there are 2 intercept
    if (d > 0)
      {
	cross1 =
	  linePoint + PHPoint (lineVector * ((-b + sqrt (d)) / (2. * a)));
	cross2 =
	  linePoint + PHPoint (lineVector * ((-b - sqrt (d)) / (2. * a)));
	return 2;
      }

    return True;
  }
  short intersectionLineSphere (const PHLine & line, const PHSphere & sph,
				PHLine & cross1, PHLine & cross2)
  {
    PHPoint crossPoint1, crossPoint2;
    short flag = intersectionLineSphere (line, sph, crossPoint1, crossPoint2);
    if (flag == 1)
      {
	cross1.setBasepoint (crossPoint1);
	cross1.setDirection (line.getDirection ());
      }
    else if (flag == 2)
      {
	cross1.setBasepoint (crossPoint1);
	cross1.setDirection (line.getDirection ());
	cross2.setBasepoint (crossPoint2);
	cross2.setDirection (line.getDirection ());
      }

    return flag;

  }

  short
    intersectionLineCylinder (const PHLine & line,
			      const PHCylinder & cyl,
			      PHPoint & cross1, PHPoint & cross2)
  {
    short flag = 0;
    PHFrame XYZ;
    PHPlane plane (cyl.getCenter (), cyl.getAxis ());

    PHVector w = plane.getNormal ();
    PHVector u = plane.getNormal ().orthogonal ();
    PHVector v = w.cross (u);

    PHFrame planeFrame (plane.getOrigin (), u, v, w);
    PHLine newLine = transformLine (XYZ, line, planeFrame);

    double r = cyl.getRadius ();
    PHPoint origin;
    PHVector normal (0, 0, 1);
    PHPlane XYplane (origin, normal);

    PHLine projectedLine = projectLineIntoPlane (newLine, XYplane);

    PHVector vector1 = projectedLine.getDirection ();
    PHPoint point1 = projectedLine.getBasepoint ();

    PHPoint tmp2dPoint1, tmp2dPoint2;	// 2d points
    PHPoint tmp3dPoint1, tmp3dPoint2;	// 3d points

    // intersection line 2D - circle

    double m1, n1;
    if (vector1.length () == 0)
      {
	// if a point
	double distanceFromOrigin = distancePointToPoint (point1, origin);
	if (distanceFromOrigin == cyl.getRadius ())
	  {
	    PHMessage ("intersectionLineCylinder",
		       PHWarning, "infinite intersections");
	    flag = 0;
	  }
	else
	  {
	    PHMessage ("intersectionLineCylinder",
		       PHHullo, "no intersection");
	    flag = 0;
	  }
      }
    else if (vector1.getX () != 0)
      {
	// not vertical line  
	m1 = vector1.getY () / vector1.getX ();
	n1 = point1.getY () - m1 * (point1.getX ());
	double a = (1 + m1 * m1);
	double b = (2 * m1 * n1);
	double c = (n1 * n1 - r * r);
	double d = b * b - 4 * a * c;

	if (d < 0)
	  {
	    return flag;
	  }
	else if (d == 0)
	  {
	    double x = (-b + sqrt (d)) / (2 * a);
	    double y = x * m1 + n1;
	    tmp2dPoint1.setX (x);
	    tmp2dPoint1.setY (y);
	    double ntx = (x - newLine.getBasepoint ().getX ()) / newLine.getDirection ().getX ();	//
	    double z = newLine.getBasepoint ().getZ () + ntx * (newLine.getDirection ().getZ ());	//
	    tmp3dPoint1 = tmp2dPoint1;
	    tmp3dPoint1.setZ (z);
	    flag = 1;
	  }
	else if (d > 0)
	  {
	    double x1 = (-b + sqrt (d)) / (2 * a);
	    double x2 = (-b - sqrt (d)) / (2 * a);
	    double y1 = x1 * m1 + n1;
	    double y2 = x2 * m1 + n1;
	    tmp2dPoint1.setX (x1);
	    tmp2dPoint1.setY (y1);
	    tmp2dPoint2.setX (x2);
	    tmp2dPoint2.setY (y2);
	    tmp3dPoint1 = tmp2dPoint1;
	    tmp3dPoint2 = tmp2dPoint2;

	    double ntx1 =
	      (x1 -
	       newLine.getBasepoint ().getX ()) /
	      newLine.getDirection ().getX ();
	    double ntx2 =
	      (x2 -
	       newLine.getBasepoint ().getX ()) /
	      newLine.getDirection ().getX ();
	    double z1 =
	      newLine.getBasepoint ().getZ () +
	      ntx1 * (newLine.getDirection ().getZ ());
	    double z2 =
	      newLine.getBasepoint ().getZ () +
	      ntx2 * (newLine.getDirection ().getZ ());
	    tmp3dPoint1.setZ (z1);
	    tmp3dPoint2.setZ (z2);
	    flag = 2;
	  }
      }
    else
      {
	// vertical line: m1 infinity
	n1 = point1.getX ();
	if (fabs (n1) > r)
	  {
	    return flag;
	  }
	else if (fabs (n1) == r)
	  {
	    tmp2dPoint1.setX (n1);
	    tmp2dPoint1.setY (0);
	    double nty =
	      (0 -
	       newLine.getBasepoint ().getY ()) /
	      newLine.getDirection ().getY ();
	    tmp3dPoint1 = tmp2dPoint1;
	    double z =
	      newLine.getBasepoint ().getZ () +
	      nty * (newLine.getDirection ().getZ ());
	    tmp3dPoint1.setZ (z);
	    flag = 1;
	  }
	else if (fabs (n1) < r)
	  {
	    double y1 = sqrt (r * r - n1 * n1);
	    double y2 = -(sqrt (r * r - n1 * n1));
	    tmp2dPoint1.setX (n1);
	    tmp2dPoint1.setY (y1);
	    tmp2dPoint2.setX (n1);
	    tmp2dPoint2.setY (y2);
	    double nty1 =
	      (y1 -
	       newLine.getBasepoint ().getY ()) /
	      newLine.getDirection ().getY ();
	    double nty2 =
	      (y2 -
	       newLine.getBasepoint ().getY ()) /
	      newLine.getDirection ().getY ();
	    double z1 =
	      newLine.getBasepoint ().getZ () +
	      nty1 * (newLine.getDirection ().getZ ());
	    double z2 =
	      newLine.getBasepoint ().getZ () +
	      nty2 * (newLine.getDirection ().getZ ());
	    tmp3dPoint1 = tmp2dPoint1;
	    tmp3dPoint2 = tmp2dPoint2;
	    tmp3dPoint1.setZ (z1);
	    tmp3dPoint2.setZ (z2);
	    flag = 2;
	  }
      }

    PHBoolean tmpFlag = 0;
    if (flag == 1)
      {
	// check if inside the cylinder
	if (fabs (tmp3dPoint1.getZ ()) < cyl.getAxis ().length ())
	  {
	    cross1 = transformPoint (planeFrame, tmp3dPoint1, XYZ);
	    return flag;
	  }
	else
	  {
	    flag = 0;
	    return flag;
	  }
      }
    else if (flag == 2)
      {
	if (fabs (tmp3dPoint1.getZ ()) < cyl.getAxis ().length ())
	  {
	    cross1 = transformPoint (planeFrame, tmp3dPoint1, XYZ);
	    tmpFlag = 1;
	  }
	else
	  {
	    flag--;
	  }
	if (fabs (tmp3dPoint2.getZ ()) < cyl.getAxis ().length ())
	  {
	    if (tmpFlag == 1)
	      {
		cross2 = transformPoint (planeFrame, tmp3dPoint2, XYZ);
	      }
	    else
	      {
		cross1 = transformPoint (planeFrame, tmp3dPoint2, XYZ);
	      }
	  }
	else
	  {
	    flag--;
	  }
	return flag;
      }

    return 0;
  }

  short
    intersectionLineCylinder (const PHLine & line,
			      const PHCylinder & cyl,
			      PHLine & cross1, PHLine & cross2)
  {
    PHPoint crossPoint1, crossPoint2;
    short flag =
      intersectionLineCylinder (line, cyl, crossPoint1, crossPoint2);

    if (flag == 1)
      {
	cross1.setBasepoint (crossPoint1);
	cross1.setDirection (line.getDirection ());
      }
    else if (flag == 2)
      {
	cross1.setBasepoint (crossPoint1);
	cross1.setDirection (line.getDirection ());
	cross2.setBasepoint (crossPoint2);
	cross2.setDirection (line.getDirection ());
      }

    return flag;
  }

  short 
  intersectionLineSphereSection(const PHLine & line, 
				const PHSphereSection & sphsec,
				PHPoint & cross1, PHPoint & cross2)
  {
    short myflag = 0;
    PHPoint crossPoint1, crossPoint2;

    PHSphere sph (sphsec.getCenter (), sphsec.getRadius ());
    short flag = intersectionLineSphere (line, sph, crossPoint1, crossPoint2);

    if (flag == 1)
      {
	// Subtract the center to calculate phi
	PHSphPoint sphCross1 = (crossPoint1 - sphsec.getCenter ());
	PHAngle phi = sphCross1.getPhi ();
	PHAngle theta = sphCross1.getTheta ();
	if (sphsec.ifInsideRange (phi, theta))
	  {
	    cross1 = crossPoint1;
	    myflag++;
	  }
      }
    else if (flag == 2)
      {
	// Subtract the center to calculate phi
	PHSphPoint sphCross1 = (crossPoint1 - sphsec.getCenter ());
	PHSphPoint sphCross2 = (crossPoint2 - sphsec.getCenter ());
	PHAngle phi1 = sphCross1.getPhi ();
	PHAngle theta1 = sphCross1.getTheta ();
	PHAngle phi2 = sphCross2.getPhi ();
	PHAngle theta2 = sphCross2.getTheta ();
	if (sphsec.ifInsideRange (phi1, theta1))
	  {
	    cross1 = crossPoint1;
	    myflag++;
	  }
	if (sphsec.ifInsideRange (phi2, theta2))
	  {
	    if (myflag == 1)
	      {
		cross2 = crossPoint2;
	      }
	    else
	      {
		cross1 = crossPoint2;
	      }
	    myflag++;
	  }
      }

    return myflag;

  }

  short intersectionLineSphereSection
    (const PHLine & line, const PHSphereSection & sphsec,
     PHLine & cross1, PHLine & cross2)
  {
    PHPoint crossPoint1, crossPoint2;
    short flag =
      intersectionLineSphereSection (line, sphsec, crossPoint1, crossPoint2);
    if (flag == 1)
      {
	cross1.setBasepoint (crossPoint1);
	cross1.setDirection (line.getDirection ());
      }
    else if (flag == 2)
      {
	cross1.setBasepoint (crossPoint1);
	cross1.setDirection (line.getDirection ());
	cross2.setBasepoint (crossPoint2);
	cross2.setDirection (line.getDirection ());
      }

    return flag;
  }

  short intersectionLineCylinderSection (const PHLine & line,
					 const PHCylinderSection & cylsec,
					 PHPoint & cross1, PHPoint & cross2)
  {
    short myflag = 0;
    PHPoint crossPoint1, crossPoint2;

    PHCylinder cyl (cylsec.getCenter (), cylsec.getRadius (),
		    cylsec.getAxis ());
    short flag =
      intersectionLineCylinder (line, cyl, crossPoint1, crossPoint2);
    PHFrame cylFrame = cylsec.getFrame ();
    PHFrame XYZ;

    if (flag == 1)
      {
	PHPoint cross1temp = transformPoint (XYZ, crossPoint1, cylFrame);
	PHCylPoint cylCross1 = cross1temp;
	PHAngle phi = cylCross1.getPhi ();
	if (cylsec.ifInsidePhiRange (phi))
	  {
	    cross1 = crossPoint1;

	    myflag++;
	  }
      }
    else if (flag == 2)
      {
	PHPoint cross1temp = transformPoint (XYZ, crossPoint1, cylFrame);
	PHCylPoint cylCross1 = cross1temp;
	PHPoint cross2temp = transformPoint (XYZ, crossPoint2, cylFrame);
	PHCylPoint cylCross2 = cross2temp;
	PHAngle phi1 = cylCross1.getPhi ();
	PHAngle phi2 = cylCross2.getPhi ();
	if (cylsec.ifInsidePhiRange (phi1))
	  {
	    cross1 = crossPoint1;
	    myflag++;
	  }
	if (cylsec.ifInsidePhiRange (phi2))
	  {
	    if (myflag == 1)
	      {
		cross2 = crossPoint2;
	      }
	    else
	      {
		cross1 = crossPoint2;
	      }
	    myflag++;
	  }
      }

    return myflag;
  }

  short intersectionLineCylinderSection (const PHLine & line,
					 const PHCylinderSection & cylsec,
					 PHLine & cross1, PHLine & cross2)
  {
    PHPoint crossPoint1, crossPoint2;
    short flag =
      intersectionLineCylinderSection (line, cylsec, crossPoint1,
				       crossPoint2);
    if (flag == 1)
      {
	cross1.setBasepoint (crossPoint1);
	cross1.setDirection (line.getDirection ());
      }
    else if (flag == 2)
      {
	cross1.setBasepoint (crossPoint1);
	cross1.setDirection (line.getDirection ());
	cross2.setBasepoint (crossPoint2);
	cross2.setDirection (line.getDirection ());
      }

    return flag;
  }

  short intersectionPolyLineCylinderSection (const PHPolyLine & PolyLine,
					     const PHCylinderSection & cylsec,
					     PHLine & line1, PHLine & line2)
  {
    // For simplicity, we assume slowly varying polylines that only intersect the cylinder at two points.
    // Define line-cylinder plane normal vector in algorithm for each line but here
    // set origin to cylinder's center.

    PHPoint p1, p2;
    double dist1Cross1, dist2Cross1;
    double dist1Cross2, dist2Cross2;
    double length;

    short flag = 0;

    p1 = *(PolyLine.getPoint (0));
    for (int i = 1; i < (int) PolyLine.numberOfPoints (); i++)
      {
	p2 = *(PolyLine.getPoint ((size_t) i));
	PHPoint cross1;
	PHPoint cross2;
	PHLine crossLine1, crossLine2;
	PHLine thisLine (p1, p2);	// p1 and p2 are the end point of this  line segment
	length = thisLine.length ();

	short num =
	  intersectionLineCylinderSection (thisLine, cylsec, crossLine1,
					   crossLine2);
	if (num == 1)
	  {
	    cross1 = crossLine1.getBasepoint ();
	    dist1Cross1 = distancePointToPoint (p1, cross1);
	    dist2Cross1 = distancePointToPoint (p2, cross1);
	    if (dist1Cross1 <= length && dist2Cross1 <= length)
	      {
		if (flag == 0)
		  {
		    line1 = crossLine1;
		  }
		else if (flag == 1)
		  {
		    line2 = crossLine1;
		  }
		else
		  {
		    PHMessage ("intersectionPolyLineCylinderSection",
			       PHWarning, "you have already two points");
		  }
		flag++;
	      }
	  }
	else if (num == 2)
	  {
	    cross1 = crossLine1.getBasepoint ();
	    cross2 = crossLine2.getBasepoint ();
	    dist1Cross1 = distancePointToPoint (p1, cross1);
	    dist2Cross1 = distancePointToPoint (p2, cross1);

	    dist1Cross2 = distancePointToPoint (p1, cross2);
	    dist2Cross2 = distancePointToPoint (p2, cross2);
	    if (dist1Cross1 <= length && dist2Cross1 <= length)
	      {
		if (flag == 0)
		  {
		    line1 = crossLine1;
		  }
		else if (flag == 1)
		  {
		    line2 = crossLine1;
		  }
		else
		  {
		    PHMessage ("intersectionPolyLineCylinderSection",
			       PHWarning, "you have already two points");
		  }
		flag++;
	      }
	    if (dist1Cross2 <= length && dist2Cross2 <= length)
	      {
		if (flag == 0)
		  {
		    line1 = crossLine2;
		  }
		else if (flag == 1)
		  {
		    line2 = crossLine2;
		  }
		else
		  {
		    PHMessage ("intersectionPolyLineCylinder", PHWarning,
			       "you have already two points");
		  }
		flag++;
	      }
	    else
	      {
	      }
	  }
	p1 = p2;
      }
    // you return a maximum of two points (the first 2) but the flag
    // will indicate the correct number of them
    return flag;
  }

  short 
  intersectionPolyLineCylinderSection (const PHPolyLine & PolyLine,
				       const PHCylinderSection & cylsec,
				       PHPoint & point1,
				       PHPoint & point2)
  {
    PHLine line1, line2;
    short flag =
      intersectionPolyLineCylinderSection (PolyLine, cylsec, line1, line2);
    if (flag == 1)
      {
	point1 = line1.getBasepoint ();
      }
    else if (flag == 2)
      {
	point1 = line1.getBasepoint ();
	point2 = line2.getBasepoint ();
      }
    return flag;

  }

};
