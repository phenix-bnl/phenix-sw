//////////////////////////////////////////////////////////////////////////////
//
// Class:  DchLineTrackFit implementation file
//         inherits from TObject and utilizes TMinuit
//
// By:  Julia Velkovska and Jane Burward-Hoy
//
// Description:  Uses matrix class and "mini" from TMinuit 
//               to determine the best fit basepoint and direction vector 
//               of the track given initial guesses of these six parameters.
//
// Date:  07/10/00
//
//////////////////////////////////////////////////////////////////////////////


//INCLUDECHECKER: Removed this line: #include "DchAnaPar.h"
#include "TMinuit.h"
#include "DchLineTrackFit.hh"

#include <iostream>

using namespace std;

DchLineTrackFit::DchLineTrackFit(const PHLine &guess,PHPointerList<PHLine> *hits) 
{
  hitLines = hits;

  previous = guess;
  basepoint = guess.getBasepoint();
  direction = guess.getDirection();
  sigma = RESO;

  redchi2 = -1;
  iter = -1;
  edm = -1;
  // by default, set par step size to 0.01
  for (int i = 0; i<6; i++) {
    par[i] = 0.0;
    parStep[i] = 0.01;
  }
  
}
DchLineTrackFit::~DchLineTrackFit() {}

PHBoolean DchLineTrackFit::fitTrack()
{
  gMinuit = new TMinuit(6);
  this->setMinuit(gMinuit);
  gMinuit->SetFCN(minimizeME);
  gMinuit->SetObjectFit(this); 
  gMinuit->Command("SET PRINT -1");
  gMinuit->Command("SET NOW -1");
  gMinuit->SetMaxIterations(500);

    // set parameter limits/initial guesses

  PHPoint base = basepoint;
  PHVector vector = direction;

  par[0] = base.getX();
  par[1] = base.getY();
  par[2] = base.getZ();
  par[3] = vector.getX();
  par[4] = vector.getY();
  par[5] = vector.getZ();

  gMinuit->DefineParameter(0,"x",par[0],parStep[0],0,0);
  gMinuit->DefineParameter(1,"y",par[1],parStep[1],0,0);
  gMinuit->DefineParameter(2,"z",par[2],parStep[2],0,0);
  gMinuit->DefineParameter(3,"vx",par[3],parStep[3],0,0);
  gMinuit->DefineParameter(4,"vy",par[4],parStep[4],0,0);
  gMinuit->DefineParameter(5,"vz",par[5],parStep[5],0,0);

  // call mini minimization routine
  Int_t iflag = 0;
  Double_t arglist[2];
  arglist[0] = 5000;
  arglist[1] = 0.01;
  gMinuit->mnexcm("MINI",arglist,3,iflag);  // check
 
  // get resulting fitted parameters after minimization
  Double_t min[6];
  Double_t max[6];
  Double_t err[6];
  TString name[6];

  gMinuit->mnpout(0,name[0],par[0],err[0],min[0],max[0],iflag);
  gMinuit->mnpout(1,name[1],par[1],err[1],min[1],max[1],iflag);
  gMinuit->mnpout(2,name[2],par[2],err[2],min[2],max[2],iflag);
  gMinuit->mnpout(3,name[3],par[3],err[3],min[3],max[3],iflag);
  gMinuit->mnpout(4,name[4],par[4],err[4],min[4],max[4],iflag);
  gMinuit->mnpout(5,name[5],par[5],err[5],min[5],max[5],iflag);

  Double_t fcnmin,errdef,edmval;
  Int_t nvpar,nparx;
  gMinuit->mnstat(fcnmin,edmval,errdef,nvpar,nparx,iflag);
  
  int mini = getFitStatus((char *)gMinuit->fCstatu.Data());
  if (mini == 0 || mini == 1 || mini == -1) {
    fitOkay = False;
  } else if (mini == 2 || mini == 3) {
    fitOkay = True;
  }
    
  if (fitOkay) {
    redchi2 = fcnmin/(hitLines->length()-gMinuit->fNpar);
    iter = gMinuit->fNfcn;
    edm = edmval;

    basepoint.setX(par[0]);
    basepoint.setY(par[1]);
    basepoint.setZ(par[2]);

    direction.setX(par[3]);
    direction.setY(par[4]);
    direction.setZ(par[5]);
    direction.normalize();

    track.setBasepoint(basepoint);
    track.setDirection(direction);

  }
  
  gMinuit->mncler();  
  delete gMinuit;
  return fitOkay;
  
}

void DchLineTrackFit::print() const 
{
  cout << "******************************************************"<< endl;
  cout << "FIT RESULTS: " << endl;
  cout << "  iterations = " << iter << endl;
  cout << "  reduced chi2 = " << redchi2<< endl;
  cout << "  estimated distance to minimum = " << edm << endl;
  cout << "  fitted track = " << track.getBasepoint() << " "<< track.getDirection() << endl;
  cout << "GUESS TRACK: " << previous.getBasepoint() <<" "<< track.getDirection() << endl;
  cout << "HITS: " << endl;
  cout << "  sigma = " << sigma << endl;
  cout << "  num hits = " << hitLines->length() << endl;
  cout << "******************************************************"<< endl;

}

void minimizeME(Int_t &npar, 
		Double_t *grad, Double_t &chi2, 
		Double_t *par, Int_t iflag)
{
  DchLineTrackFit *straightline = (DchLineTrackFit*)gMinuit->GetObjectFit();

  // the base point of the line of closest approach to all hit lines
  double x1=par[0];
  double y1=par[1];
  double z1=par[2]; 

  // hit resolution
  double sigma = (*straightline).getSigma();

  //the direction of the line of closest approach to all hit lines
  double ax1=par[3];
  double ay1=par[4];
  double az1=par[5];
  PHVector lineDir(ax1,ay1,az1);
 
  PHMatrix distanceMatrix;

  chi2=0; 
  for (int i=0;i<(*straightline).getNumOfHits();i++)
    {
      PHPoint hitPoint = (*straightline).getHitBasepoint(i);
      PHVector hitDir = (*straightline).getHitDirection(i);

      // build the matrix for the distance calculation
      distanceMatrix.setA00(hitPoint.getX()-x1);
      distanceMatrix.setA01(hitPoint.getY()-y1);
      distanceMatrix.setA02(hitPoint.getZ()-z1);
      distanceMatrix.setA10(hitDir.getX());
      distanceMatrix.setA11(hitDir.getY());
      distanceMatrix.setA12(hitDir.getZ());
      distanceMatrix.setA20(ax1);
      distanceMatrix.setA21(ay1);
      distanceMatrix.setA22(az1);
      double determinant=distanceMatrix.det();
      PHVector tmpVector = hitDir.cross(lineDir);
      chi2 = chi2+(determinant*determinant)/(sigma*sigma*tmpVector.lengthSqr());

    }
}

int DchLineTrackFit::getFitStatus (char *Migrad) 
{
  /* Transforming the MIGRAD char[]-status to a int-status */

  int i ;
  char Status[][12]={"FAILED","PROBLEMS","CONVERGED","SUCCESSFUL"} ;

  for(i=0 ; i < 4 ; i++) 
    if (strstr(Migrad,Status[i]) != NULL) return i ;

  return -1  ;
}

