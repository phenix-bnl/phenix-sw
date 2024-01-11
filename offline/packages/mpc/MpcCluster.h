#ifndef __MPCCLUSTER_H__
#define __MPCCLUSTER_H__

// Name: MpcCluster.h

// Standard includes
#include <TObject.h>
#include <MpcMap.h>

#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <vector>
// Forward declarations
class MpcSectorRec;
class MpcCluster;
class MpcPeakarea;
class MpcEmshower;

/** One tower information for internal clustering use.
@ingroup clustering
*/

struct MpcModule
{
  MpcModule();
  MpcModule(int ich_, float amp_, float tof_,
	    int deadmap_, int warnmap_, float adc_, float tac_);

  int ich;   // module id (linear)
  float amp; // module signal
  float tof; // module time-of-flight
  int deadmap; // module dead map: see emcCalibratedDataObject.h
  int warnmap; // MV 2001/12/06
  float adc; // ADC amplitude
  float tac; // TAC amplitude
};

// ///////////////////////////////////////////////////////////////////////////

/** The 1-st level of the EMCal clustering: cluster is a set of contiguous
    towers. 

    Only used internally by clustering routines.
    @ingroup clustering
*/

class MpcCluster : public TObject
{

public:

  /// Constructor (zero Hit List)
  MpcCluster():fOwner(NULL)
  { mpcmap = MpcMap::instance(); }

  MpcCluster(MpcSectorRec *sector): fOwner(sector)
  { mpcmap = MpcMap::instance(); }

  /// Constructor (inputs Hit List)
  MpcCluster(const std::vector<MpcModule>& hlist,
	     MpcSectorRec *sector)
    : fOwner(sector)
  {
    fHitList = hlist;
  }

  ///
  virtual ~MpcCluster()
  {mpcmap = NULL;}

  /// Reinitializes MpcCluster supplying new Hit List.

  void ReInitialize( const std::vector<MpcModule>& hlist )
  {
    fHitList = hlist;
  }

  /// Returns number of MpcModules in MpcCluster
  int GetNofHits()
  {
    return fHitList.size();
  }
  /// Returns n MpcModules (sorted) with the maximum energy
  void GetHits(MpcModule* phit, int n);

  /// Returns MpcCluster fHitList
  void GetHitList(std::vector<MpcModule> *&plist)
  {
    plist = &fHitList;
  };

  /// Returns the MpcModule with the maximum energy
  MpcModule GetMaxTower();
  /// Returns the MpcModule corresponding to the reconstructed impact tower
  MpcModule GetImpactTower();
  /// Returns the energy of the ich-tower (numbering from 0)
  float GetTowerEnergy( int ich );
  /// Returns the energy of the tower ix,iy (numbering from 0)
  float GetTowerEnergy( int ix, int iy );
  /// Returns the ToF of the ich-tower (numbering from 0)
  float GetTowerToF( int ich );
  /// Returns the Dead Map of the ich-tower (numbering from 0)
  int GetTowerDeadMap( int ich );
  /// Returns the Warning Map of the ich-tower (numbering from 0)
  int GetTowerWarnMap( int ich ); // MV 2002/02/18 bugfix
  float GetTowerADC( int ich ); // MV 2002/03/12 bugfix
  float GetTowerTAC( int ich ); // MV 2002/03/12 bugfix
  /// Returns the number of dead channels around MaxTower
  int GetNDead();
  int GetDeadMap(); // MV 2001/12/06
  int GetWarnMap(); // MV 2001/12/06
  /// Returns the energy in 2x2 towers around the cluster Center of Gravity
  float GetE4();
  /// Returns the energy in 3x3 towers around the cluster Center of Gravity
  float GetE9();
  /// Returns the energy in 3x3 towers around the tower ich
  float GetE9( int ich );
  /// Returns the cluster energy taking into account towers with E>Ethresh
  float GetECore();
  float GetECoreV2();
  /// Returns the MpcCluster total energy
  float GetTotalEnergy();
  /// Returns MpcCluster 1-st (pxcg,pycg) and 2-d momenta (pxx,pxy,pyy)
  void GetMoments( float* pxcg, float* pycg,
		   float* pxx, float* pxy, float* pyy );
  void GetMomentsV2( float* pxcg, float* pycg,
		   float* pxx, float* pxy, float* pyy );
  void GetLinearMoments( float* pxcg, float* pycg,
			 float* pxx, float* pxy, float* pyy);
  void GetLogMoments( float* pxcg, float* pycg,
		      float* pxx, float* pxy, float* pyy,
		      float weight);
  /// Returns the MpcCluster corrected position in Sector (SM) frame
  void GetCorrPos( float* pxc, float* pyc );
  /// Returns the MpcCluster position in PHENIX global coord system
  void GetGlobalPos( float* pxg, float* pyg, float* pzg );
  /// Returns the errors for the reconstructed energy and position
  void GetErrors( float* pde, float* pdx, float* pdy, float* pdz);
  /// Substitutes a number of functions above (to save CPU time)
  void GetChar( float* pe,
		float* pxcg, float* pysg,
		float* pxc, float* pyc,
		float* pxg, float* pyg, float* pzg,
		float* pxx, float* pxy, float* pyy,
		float* pde, float* pdx, float* pdy, float* pdz );


  /// Splits the MpcCluster onto peakarea's; also returns peak tower array corresponding to peakarea array
  int GetPeaks(MpcPeakarea*, MpcModule*);
  int GetPeaks_Mpc(MpcPeakarea*, MpcModule*);
  int Get3x3CG(float &x, float &y,int xpk,int ypk,float e[][3]);
  int Get3x3LogCG(float &x,float &y,int xpk,int ypk,float e[][3]);
  int Get3x3LinearCG(float &x,float &y,int xpk,int ypk,float e[][3]);
  int Get3x3GridCG(float &x,float &y,int xpk,int ypk,float e[][3]);
    
protected:

#ifndef __CINT__

  std::vector<MpcModule> fHitList;
#endif

  MpcSectorRec *fOwner; // what sector it belongs to
  MpcMap *mpcmap;

  // static members
  static int const fgMaxNofPeaks;
  static int const fgPeakIterations;
  static float const fgEmin;
  static float const fgEminV2;
  static float const fgChisq; 
  static float const fgXABSURD;
  static float const fgYABSURD;
  static int moment_type; //this is for which type of moments are used in calculations
  //0 for old calculation
  //1 for linear
  //2 for log
  
public:

  // MV 2002/02/28 moved these functions here from #define's
  
  static void SetMomentType(int type)
    { 
      moment_type = (type<1 || type>2)?0:type;
    }
  

  static int max(int a, int b)
  {
    return a > b ? a : b;
  }
  static float max(float a, float b)
  {
    return a > b ? a : b;
  }
  static double max(double a, double b)
  {
    return a > b ? a : b;
  }

  static int min(int a, int b)
  {
    return a < b ? a : b;
  }
  static float min(float a, float b)
  {
    return a < b ? a : b;
  }
  static double min(double a, double b)
  {
    return a < b ? a : b;
  }

  static int ABS(int x)
  {
    return abs(x);
  }
  static float ABS(float x)
  {
    return fabsf(x);
  }
  static double ABS(double x)
  {
    return fabs(x);
  }

  static int lowint(float x)
  {
    return x < 0. ? int(x - 1) : int(x);
  }

};

// ///////////////////////////////////////////////////////////////////////////

/** The 2-d level of the EMCal clustering. 
    Every local maximum in cluster gives peakarea.
    @ingroup clustering
*/

class MpcPeakarea: public MpcCluster
{

public:

  /// Constructor (zero Hit List)
  MpcPeakarea(): fNdf(0), fCL(1.)
  {}

  MpcPeakarea(MpcSectorRec *sector):
    MpcCluster(sector), fNdf(0), fCL(1.)
  {}

  /// Constructor (inputs Hit List)
#ifndef __CINT__

  MpcPeakarea(const std::vector<MpcModule>& hlist, MpcSectorRec *sector):
    MpcCluster(hlist, sector), fNdf(0), fCL(1.)
  {}
#endif

  virtual ~MpcPeakarea()
  {}

  /// Returns Chi2
  float GetChi2();
  float GetChi2New();
  int GetNdf() const
  {
    return fNdf;
  } 
  // fetch number of degrees of freedom
  float GetCL() const
  {
    return fCL;
  } 
  // get CL (call only after GetChar())
  float GetCLNew();

  /// Returns peakarea's 1st momentum (COG) after Shower Shape fit
  void GetCGmin( float* pxcgmin, float* pycgmin );

  /// Get Center of Gravity before shower shape fit
  void GetLinearCG( float* px, float* py );
  void GetLogWeightCG( float* px, float* py, const double w0 = 3.0 );

  /// Substitutes a number of functions above (to save CPU time)
  void GetChar( float* pe, float* pec, float* pecore, float* pecorec, float *pe9,
		float* pxcg, float* pysg,
		float* pxcgmin, float* pysgmin,
		float* pxc, float* pyc,
		float* pxg, float* pyg, float* pzg,
		float* pxx, float* pxy, float* pyy,
		float* pchi,
		float* pde, float* pdx, float* pdy, float* pdz );
 
  void GetCharV2( float* pe, float* pec, 
		  float* pecore, float* pecorec, 
		  float* pe9,
		  float* pxcg, float* pycg, 		// log center of gravity
		  float* pxcgmin, float* pycgmin, 	// chi2 min cg
		  float* pxlcg, float* pylcg, 	// linear cg
		  float* pxg, float* pyg,float* pzg, // log pos, z
		  float* pxmin, float* pymin,	// lin pos
		  float* pxl, float* pyl,	        // chi2 min pos
		  float* pxx, float* pxy, float* pyy,	// moments
		  float* pchi,  float* pchicore,int* pndfcore,
		  float* pde, float* pdx, float* pdy, float* pdz,
		  float* plogxx, float* plogxy, float* plogyy,
		  bool &split, float &chi2_split, float &se1, float &sx1, float &sy1,
		  float &se2, float &sx2, float &sy2);
  

  /// Splits the peakarea onto 1 or 2 MpcEmshower's
  int GetGammas( MpcEmshower* );

protected:

  int fNdf; // Number of degrees of freedom
  float fCL; // Confidence level

};

// ///////////////////////////////////////////////////////////////////////////

/** The 3-d level of the EMCal clustering: peakarea with bad Chi2 is splitted
    onto two MpcEmshowers. 
    @ingroup clustering
*/
class MpcEmshower
{

public:

  /// Constructor (with energy and position zeroed)
  MpcEmshower();
  MpcEmshower(MpcSectorRec *sector);

  /// Constructor (inputs energy, position and Chi2)
  MpcEmshower(float e, float x, float y, float chi, int ndf,
	      MpcSectorRec *sector);

  /// Reinitializes MpcEmshower
  void ReInitialize(float e, float x, float y, float chi, int ndf)
  {
    fEnergy = e;
    fXcg = x;
    fYcg = y;
    fChisq = chi;
    fNdf = ndf;
  }

  /// Returns MpcEmshower total energy
  float GetTotalEnergy()
  {
    return fEnergy;
  }

  /// Returns MpcEmshower 1-at momentum (Center of Gravity)
  void GetCG(float* px, float* py)
  {
    *px = fXcg;
    *py = fYcg;
  }

  /// Returns MpcEmshower corrected position in Sector (SM) frame
  void GetCorrPos(float*, float* );

  /// Returns MpcEmshower position in PHENIX global coord system
  void GetGlobalPos(float*, float*, float*);

  /// Returns MpcEmshower Chi2
    float GetChi2() const
      {
        return fChisq;
      }

  /// Returns CL
  float GetCL() const
  {
    return fCL;
  } 
  // get CL (call only after GetChar())

  /// Returns the errors for the reconstructed energy and position
  void GetErrors( float* pde, float* pdx, float* pdy, float* pdz);
  
  /// Substitutes a number of functions above (to save CPU time)
  void GetChar( float* pe,
		float* pxcg, float* pysg,
		float* pxc, float* pyc,
		float* pxg, float* pyg, float* pzg,
		float* pchi,
		float* pde, float* pdx, float* pdy, float* pdz );
  
protected:

    MpcSectorRec* fOwner; // what sector it belongs to
    int fNdf; // Number of degrees of freedom
    float fCL; // Confidence level
    float fEnergy;
    float fXcg;
    float fYcg;



    float fChisq;

};

#endif // __MPCCLUSTER_H__
