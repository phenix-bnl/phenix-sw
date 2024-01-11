#ifndef MVDGEOMETRY
#define MVDGEOMETRY

#include "MvdParameter.hh"
#include "MvdGeoParameter.hh"

#include <vector>


/** base class of MvbGeometry and MvcGeometry
 */
class MvdGeometry
{
 public :
  virtual float Radius(short)const = 0;
  virtual float Phi(short, short) = 0;
  virtual float Z(short, short)const = 0;
  virtual ~MvdGeometry()
  {}
};

#endif


#ifndef MVBGEOMETRY
#define MVBGEOMETRY

/** z and phi of strip detectors
 */
class MvbGeometry : public MvdGeometry
{

 public:
  MvbGeometry(MvdParameter , MvdGeoParameter);
  MvbGeometry() {}
  ~MvbGeometry() {}

  /** initialize Barrel Geometry quantities such as Z,Radius,
   * Phi values of strips.
   */
  void Init();
  /// update Barrel Geometry quantities.
  void Show();
  void Update();
  /// returns radius of shell
  float Radius(short shell)const;
  /// returns phi of rows
  float Phi(short row, short ext = -1);
  /// returns z of strips and panels
  float Z(short panel, short strip = -1)const;

 private :
  ///radius of shells
  std::vector<float> radius;
  ///phi of rows
  std::vector<float> phi;
  ///z of panels
  std::vector<float> zpanel;
  ///z of strips
  std::vector<float> zstrip;

 private :
  MvdParameter par;
  MvdGeoParameter geo;

};

#endif

#ifndef  MVCGEOMETRY
#define  MVCGEOMETRY

/** z and phi of pad detectors
 */
class MvcGeometry : public MvdGeometry
{

 public:
  MvcGeometry(MvdParameter , MvdGeoParameter);
  MvcGeometry()
  {}
  ~MvcGeometry()
  {}
  ///calculate data members using par and geopar
  void Init();
  ///does nothing
  void Update()
  {}
	static MvcGeometry *instance();
  ///returns radius of row
  float Radius(short)const;
  ///returns phi at given wedge and column
  float Phi(short wedge, short column = -1);
  ///returns z of ends
  float Z(short end, short ext = -1)const;
  ///returns rcust[i]
  float Rcuts(int i)const;
  ///returns phicent[i]
  float PhiCent(int i)const;
  ///returns phivals[i]
  float Phivals(int i)const;
  void Show();

 private :
	// global instance of the geometry
	static MvcGeometry *sInstance; 
  ///radius of boundrary of rows
  std::vector<float> rcuts;
  ///radius of rows
  std::vector<float> rvals;
  ///phi at the center of pads
  std::vector<float> phicent;
  ///
  std::vector<float> phivals;
  ///z value of ends
  std::vector<float> zend;

 private :
  MvdParameter par;
  MvdGeoParameter geo;
};

#endif

