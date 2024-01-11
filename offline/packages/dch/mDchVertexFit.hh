#ifndef __MDCHVERTEXFIT_H__
#define __MDCHVERTEXFIT_H__

#define MAXDIM 10000

#include <cmath>

#include "phool.h"
#include "PHNode.h"
#include "PHPoint.h"
#include "PHCompositeNode.h"
#include "PHPointerList.h"

#include "TObject.h"
#include "TMinuit.h"
void MinimizationFCN(int &, double *, double &, double *, int);

class mDchVertexFit: public TObject {
public:
  mDchVertexFit();
  virtual ~mDchVertexFit();
       
  PHPoint getVertex() {return vtxPoint;}
  float getVertexX() {return vtxPoint.getX();}
  float getVertexY() {return vtxPoint.getY();}
  float getVertexZ() {return vtxPoint.getZ();}

  PHBoolean event(PHCompositeNode *);
  void initialize();
  const TMinuit* getMinuit() const { return gMinuit;}
  void setMinuit(TMinuit* minuit) {gMinuit = minuit;}

  double sarm[MAXDIM];
  double sx[MAXDIM],sy[MAXDIM],sz[MAXDIM];
  double ax[MAXDIM],ay[MAXDIM],az[MAXDIM];
  double xini,yini,zini;
  double vx,vy,vz;
  double distr[MAXDIM],distx[MAXDIM],disty[MAXDIM],distz[MAXDIM];
  double accdistr[MAXDIM],accdistx[MAXDIM],accdisty[MAXDIM],accdistz[MAXDIM];
  double phi[MAXDIM],theta[MAXDIM],eta[MAXDIM];
  int    nhits[MAXDIM],track_ok[MAXDIM],track_prim[MAXDIM];
  int    npart,ngood,nbad,ngeogood,ngeobad,nvtx;
  double px1[MAXDIM],py1[MAXDIM],pz1[MAXDIM];
  double px2[MAXDIM],py2[MAXDIM],pz2[MAXDIM];
  double chisquare;

// cuts on track acceptance
  double DISTPRIMR;    // max. distance all tracks - primary vertex
  double DISTPRIMX;    // max. distance all tracks - primary vertex
  double DISTPRIMY;    // max. distance all tracks - primary vertex
  double DISTPRIMZ;    // max. distance all tracks - primary vertex
  double DISTRECR;     // max. distance accepted tracks - reconstructed  vertex
  double DISTRECX;     // max. distance accepted tracks - reconstructed  vertex
  double DISTRECY;     // max. distance accepted tracks - reconstructed  vertex
  double DISTRECZ;     // max. distance accepted tracks - reconstructed  vertex
  int    HITSMIN, HITSMAX;  // min. and max. number of DC hits per track

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
private:
  PHPoint vtxPoint;
  TMinuit* gMinuit;
};
#endif /*__MDCHVERTEXFIT_H__*/



















