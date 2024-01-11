#ifndef PHDCHTRACKFITTER_H
#define PHDCHTRACKFITTER_H

//  Purpose:  header file for PHDchTrackFitter class       
//  Created by:  Jane M. Burward-Hoy and Stephen Johnson

#include "PHDchTrackFitterPar.hh"

class PHDchTrackFitter
{
protected:
  PHDchTrackFitter();
  virtual ~PHDchTrackFitter() {}

public:
  static PHDchTrackFitter * instance();

  // For backward-compatibility only 
  PHBoolean calcInterpF(const double &zvtxin,
			const int &zOrder,
			const double &theta0in,const int &theta0Order,
			const double &rin, 
			const int &rOrder,
			const double &thetain,
			const double &pin, 
			const int &pOrder,
			double &interpF);   

  PHBoolean calcInterpF(const double &zvtxin,
			const double &theta0in,
			const double &rin,
			const double &thetain,
			const double &pin,
			double &interpF);

  PHBoolean calcInterpDelta(const double &zvtxin,
			    const double &theta0in,
			    const double &rin,
			    const double &thetain,
			    const double &pin,
			    double &interpDelta);

  PHBoolean calcInterpDelta(const double &zvtxin,
			    const int &zOrder,
			    const double &theta0in, 
			    const int &theta0Order,
			    const double &rin, 
			    const int &rOrder,
			    const double &thetain,
			    const double &pin, 
			    const int &pOrder,
			    double &interpDelta);

  PHBoolean calcInterpG(const double &zvtxin,
			const int &zOrder,
			const double &theta0in, 
			const int &theta0Order,
			const double &rin, 
			const int &rOrder,
			const double &thetain,
			const double &pin, 
			const int &pOrder,
			double &interpG);


  PHBoolean calcInterpG(const double &zvtxin,
			const double &theta0in,
			const double &rin,
			const double &thetain,
			const double &pin,
			double &interpG);

  // Functions needed for polynomial interpolation
  void setPolyOrders(const int &rOrder,
		     const int &pOrder,
		     const int &zOrder,
		     const int &theta0Order);
  PHBoolean calcPolyInterpF(const double &zvtxin,
			    const int &zOrder,
			    const double &theta0in,
			    const int &theta0Order,
			    const double &rin, 
			    const int &rOrder,
			    const double &thetain,
			    const double &pin, 
			    const int &pOrder,
			    double &interpF);   
  PHBoolean calcPolyInterpF(const double &zvtxin,
			    const double &theta0in,
			    const double &rin,
			    const double &thetain,
			    const double &pin,
			    double &interpF);
  PHBoolean calcPolyInterpG(const double &zvtxin,
			    const int &zOrder,
			    const double &theta0in, 
			    const int &theta0Order,
			    const double &rin, 
			    const int &rOrder,
			    const double &thetain,
			    const double &pin, 
			    const int &pOrder,
			    double &interpG);
  PHBoolean calcPolyInterpG(const double &zvtxin,
			    const double &theta0in,
			    const double &rin,
			    const double &thetain,
			    const double &pin,
			    double &interpG);
  PHBoolean calcPolyInterpDelta(const double &zvtxin,
				const double &theta0in,
				const double &rin,
				const double &thetain,
				const double &pin,
				double &interpDelta);
  PHBoolean calcPolyInterpDelta(const double &zvtxin,
				const int &zOrder,
				const double &theta0in, 
				const int &theta0Order,
				const double &rin, 
				const int &rOrder,
				const double &thetain,
				const double &pin, 
				const int &pOrder,
				double &interpDelta);
  
  int getErrF() const {return errF;}
  int getErrG() const {return errG;}
  int getErrDelta() const { return errDelta;}

  // For creating 'smoothed' field integral files to be stored in
  // database and NOT to be created by user at run-time
  PHBoolean writeFieldIntegralMassagedFile(const char *outfile = "fieldIntegralMassaged.dat");

  // for testing purposes
  double getGridP(const int &i) const {return p[i];}
  double getGridR(const int &i) const {return r[i];}
  double getGridTheta0(const int& i) const { return initialTheta[i];}
  double getGridZ(const int &i) const { return z[i];}

  double getGridF(int &pi, int &ri, int &ti, int &zi) const { 
    return f[pi][ri][ti][zi]; 
  }
  double getGridDelta(int &pi, int &ri, int &ti, int &zi) const { 
    return delta[pi][ri][ti][zi]; 
  }
  double getGridG(int &pi, int &ri, int &ti, int &zi) const { 
    return g[pi][ri][ti][zi]; 
  }

private:
  void massageFieldIntegralArray();
  void massageDeltaFieldIntegralArray();
  void massageGFieldIntegralArray();
  PHBoolean readFieldIntegralFromFile();
  int getStartIndex(const double array[],
		    const int &n, 
		    const double &val, 
		    const int &polyOrder) const;
  PHBoolean setIndices(const double &trkMomentum, 
		       const double &trackZ, 
		       const double &thetaTrack, 
		       const double &initThetaTrack);
  int polyOrderZ;
  int polyOrderInitialTheta; 
  int polyOrderP; 
  int polyOrderR; 
  double f[numP][numR][numInitialTheta][numZ];
  double g[numP][numR][numInitialTheta][numZ];
  double delta[numP][numR][numInitialTheta][numZ];
  double p[numP], r[numR], initialTheta[numInitialTheta], z[numZ];
  int startP, stopP;
  int startZ, stopZ;
  int startInitialTheta, stopInitialTheta;
  int errF, errG, errDelta;
 
  static PHDchTrackFitter* _instance;
};

#endif // PHDCHTRACKFITTER_H
