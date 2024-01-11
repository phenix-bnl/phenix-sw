/* Copyright 2008-2009, Technische Universitaet Muenchen,
   Authors: Christian Hoeppner & Sebastian Neubert

   This file is part of GENFIT.

   GENFIT is free software: you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   GENFIT is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with GENFIT.  If not, see <http://www.gnu.org/licenses/>.
*/

/* The Runge Kutta implementation stems from GEANT3 originally (R. Brun et al.)
   Porting to C goes back to Igor Gavrilenko @ CERN
   The code was taken from the Phast analysis package of the COMPASS experiment
   (Sergei Gerrassimov @ CERN)
*/

#include "RKTrackRep.h"

#include "PHField3D.h"
#include "GFException.h"

//root stuff
#include <TMath.h>
#include <TDatabasePDG.h>

#define MINSTEP 0.00001   // minimum step [cm] for Runge Kutta and iteration to POCA
// #define DEBUG
// #define iverbose 5

ostream & operator<<(ostream &os, const TVector3& p) {
  return os << "(" << p.X() << ", " << p.Y() << ", " << p.Z() << ")";
}

ostream & operator<<(ostream &os, const TMatrixD& p) {
  std::string s;
  char temp[100];

  s += "\n";
  for(int i=0; i<p.GetNrows(); ++i) {
    s += " [";

    for(int j=0; j<p.GetNcols(); ++j) {
      sprintf(temp, "%11lg", p(i,j));
      s += temp;
      if(j<(p.GetNcols()-1))
        s += ", ";
    }

    s += "]\n";
  }

  return os << s;
}

PHField3D* RKTrackRep::_field = 0;
TVector3 RKTrackRep::_uniform_field_tesla(0,0,0);

// field in 10^-1 Tesla (!!!)
TVector3 RKTrackRep::getFieldValKiloGauss(const TVector3 &x)
{
  if(_field) {
    double pos[4] = {x(0), x(1), x(2), 0};
    double B[6] = {0};
    _field->GetFieldValueKiloGauss(pos, B);
    return TVector3(B[0], B[1], B[2]);
  } else {
    return (_uniform_field_tesla*10.);
  }
}

TVector3 RKTrackRep::getFieldValTesla(const TVector3 &x)
{
  if(_field) {
    double pos[4] = {x(0), x(1), x(2), 0};
    double B[6] = {0};
    _field->GetFieldValueKiloGauss(pos, B);
    return (0.1*TVector3(B[0], B[1], B[2]));
  } else {
    return (_uniform_field_tesla);
  }
}

void RKTrackRep::set_field_scale_factor(const float scale_factor)
{
  if(_field)
    _field->set_scale_factor(scale_factor);
  else
    std::cout << "YOU CALLED RKTrackRep::set_field_scale_factor() WHEN THERE IS NO FIELD OBJECT! OH NO!!!" << std::endl;
  return;
}

void RKTrackRep::load_field(const char* filename, const double scale_factor, const int verb)
{
  if(_field) delete _field;
  _field = new PHField3D(filename, verb);
  _field->set_scale_factor(scale_factor);
  return;
}

void RKTrackRep::set_uniform_field_tesla(const TVector3 &B)
{
  if(_field) delete _field;
  _field = 0;
  _uniform_field_tesla = B;
  return;
}

void RKTrackRep::delete_field()
{
  if(_field) delete _field;
  return;
}

RKTrackRep::~RKTrackRep(){}

void RKTrackRep::Streamer(TBuffer &R__b)
{
  // Stream an object of class RKTrackRep.

  if (R__b.IsReading()) {
    R__b.ReadClassBuffer(RKTrackRep::Class(),this);

    initArrays();

    fCachePlane = fRefPlane;
    fCacheSpu = fSpu;

    fAuxInfo.ResizeTo(1,2);
    fAuxInfo(0,0) = fCacheSpu;
    fAuxInfo(0,1) = fDirection;

  } else {
    R__b.WriteClassBuffer(RKTrackRep::Class(),this);
  }
}


RKTrackRep::RKTrackRep() : GFAbsTrackRep(5), fDirection(0), fNoMaterial(false), fPdg(0), fCharge(0), fSpu(1), fCachePlane(), fCacheSpu(1), fAuxInfo(1,2) {
  initArrays();
}


RKTrackRep::RKTrackRep(const TVector3& pos,
                       const TVector3& mom,
                       const TVector3& poserr,
                       const TVector3& momerr,
                       const int& PDGCode) :
  GFAbsTrackRep(5), fDirection(0), fNoMaterial(false), fCachePlane(), fCacheSpu(1), fAuxInfo(1,2) {

  initArrays();
  setPDG(PDGCode); // also sets charge and mass
  calcStateCov(pos, mom, poserr, momerr);
}



RKTrackRep::RKTrackRep(const TVector3& pos,
                       const TVector3& mom,
                       const int& PDGCode) :
  GFAbsTrackRep(5), fDirection(0), fNoMaterial(false), fCachePlane(), fCacheSpu(1), fAuxInfo(1,2) {

  initArrays();
  setPDG(PDGCode); // also sets charge and mass
  calcState(pos, mom);

  // set covariance diagonal elements to large number
  static const double value(1.E4);

  fCov(0,0) = value;
  fCov(1,1) = value;
  fCov(2,2) = value;
  fCov(3,3) = value;
  fCov(4,4) = value;
}


// RKTrackRep::RKTrackRep(const GFTrackCand* const aGFTrackCandPtr, int pdgCode) :
//   GFAbsTrackRep(5), fDirection(0), fNoMaterial(false), fCachePlane(), fCacheSpu(1), fAuxInfo(1,2) {

// 	if (pdgCode == 0){
// 		pdgCode = aGFTrackCandPtr->getPdgCode();
// 	}
//   setPDG(pdgCode); // also sets charge and mass

//   initArrays();

//   TMatrixDSym cov6D = aGFTrackCandPtr->getCovSeed();
//   setPosMomCov(aGFTrackCandPtr->getPosSeed(),
//                aGFTrackCandPtr->getMomSeed(),
//                cov6D);

//   if( cov6D[0][0] < 0.0 ){ // no valid cov was set in the trackCand so just set a large one
//     fCov.Zero();
//     static const double value(1.E4);
//     fCov(0,0) = value;
//     fCov(1,1) = value;
//     fCov(2,2) = value;
//     fCov(3,3) = value;
//     fCov(4,4) = value;
//   }
// }

RKTrackRep::RKTrackRep(const TVector3& pos, const TVector3& mom, const TMatrixDSym cov, const int& pdgCode) :
  GFAbsTrackRep(5), fDirection(0), fNoMaterial(false), fCachePlane(), fCacheSpu(1), fAuxInfo(1,2) {

  setPDG(pdgCode); // also sets charge and mass
  initArrays();
  setPosMomCov(pos, mom, cov);
}


void RKTrackRep::initArrays(){
  memset(fNoise,0x00,7*7*sizeof(double));
  memset(fOldCov,0x00,7*7*sizeof(double));

  memset(fJ_pM_5x7,0x00,5*7*sizeof(double));
  memset(fJ_pM_5x6,0x00,5*6*sizeof(double));
  memset(fJ_Mp_7x5,0x00,7*5*sizeof(double));
  memset(fJ_Mp_6x5,0x00,6*5*sizeof(double));

  totalTransfMat.ResizeTo(5,5);
  totalTransfMat.UnitMatrix();
  fError.ResizeTo(5,5);
}


void RKTrackRep::setData(const TVectorD& st, const GFDetPlane& pl, const TMatrixDSym* cov, const TMatrixD* aux){
  if(aux != NULL) {
    fCacheSpu = (*aux)(0,0);
    fDirection = (*aux)(0,1);
  }
  else {
    if(pl!=fCachePlane){
      GFException exc("RKTrackRep::setData() was called with a reference plane which is not the same as the one from the last extrapolate(plane,state,cov).",__LINE__,__FILE__);
      throw exc;
    }
  }
  GFAbsTrackRep::setData(st,pl,cov);
  if (fCharge*fState(0) < 0) fCharge *= -1; // set charge accordingly! (fState[0] = q/p)
  fSpu = fCacheSpu;
}


const TMatrixD* RKTrackRep::getAuxInfo(const GFDetPlane& pl) {

  if(pl!=fCachePlane) {
    GFException exc("RKTrackRep::getAuxInfo() - Trying to get auxiliary information with planes mismatch (Information returned does not belong to requested plane)!",__LINE__,__FILE__);
	  throw exc;
  }
  fAuxInfo.ResizeTo(1,2);
  fAuxInfo(0,0) = fCacheSpu;
  fAuxInfo(0,1) = fDirection;
  return &fAuxInfo;
}


void RKTrackRep::setPDG(int i){
  fPdg = i;
  TParticlePDG * part = TDatabasePDG::Instance()->GetParticle(fPdg);
#ifdef DEBUG
  std::cout << "RKTrackRep::setPDG(" << i << ") - particle: " << part->GetName() << std::endl;
#endif // DEBUG
  if(part == 0){
    GFException exc("RKTrackRep::setPDG ==> particle id not known to TDatabasePDG",__LINE__,__FILE__);
    throw exc;
  }
  fCharge = part->Charge()/(3.);
}



void RKTrackRep::calcStateCov(const TVector3& pos,
                              const TVector3& mom,
                              const TVector3& poserr,
                              const TVector3& momerr){

  calcState(pos, mom);

  double pw = mom.Mag();
  double pu = 0.;
  double pv = 0.;

  const TVector3& U(fRefPlane.getU());
  const TVector3& V(fRefPlane.getV());
  TVector3 W(fRefPlane.getNormal());


  fCov(0,0) = fCharge*fCharge/pow(mom.Mag(),6.) *
    (mom.X()*mom.X() * momerr.X()*momerr.X()+
     mom.Y()*mom.Y() * momerr.Y()*momerr.Y()+
     mom.Z()*mom.Z() * momerr.Z()*momerr.Z());

  fCov(1,1) = pow((U.X()/pw - W.X()*pu/(pw*pw)),2.) * momerr.X()*momerr.X() +
    pow((U.Y()/pw - W.Y()*pu/(pw*pw)),2.) * momerr.Y()*momerr.Y() +
    pow((U.Z()/pw - W.Z()*pu/(pw*pw)),2.) * momerr.Z()*momerr.Z();

  fCov(2,2) = pow((V.X()/pw - W.X()*pv/(pw*pw)),2.) * momerr.X()*momerr.X() +
    pow((V.Y()/pw - W.Y()*pv/(pw*pw)),2.) * momerr.Y()*momerr.Y() +
    pow((V.Z()/pw - W.Z()*pv/(pw*pw)),2.) * momerr.Z()*momerr.Z();

  fCov(3,3) = poserr.X()*poserr.X() * U.X()*U.X() +
    poserr.Y()*poserr.Y() * U.Y()*U.Y() +
    poserr.Z()*poserr.Z() * U.Z()*U.Z();

  fCov(4,4) = poserr.X()*poserr.X() * V.X()*V.X() +
    poserr.Y()*poserr.Y() * V.Y()*V.Y() +
    poserr.Z()*poserr.Z() * V.Z()*V.Z();
}


void RKTrackRep::calcState(const TVector3& pos,
                           const TVector3& mom){

  fRefPlane.setON(pos, mom);
  fSpu=1.;

  fState(0) = fCharge/mom.Mag();

  //u' and v'
  fState(1) = 0.;
  fState(2) = 0.;

  //u and v
  fState(3) = 0.;
  fState(4) = 0.;
}



void RKTrackRep::getState7(M1x7& state7) {
  getState7(state7, fState, fRefPlane, fSpu);
}


void RKTrackRep::getState7(M1x7& state7, const TVectorD& state5, const GFDetPlane& pl, const double& spu) {

  const TVector3& U(pl.getU());
  const TVector3& V(pl.getV());
  const TVector3& O(pl.getO());
  TVector3 W(pl.getNormal());

  state7[0] = O.X() + state5(3)*U.X() + state5(4)*V.X(); // x
  state7[1] = O.Y() + state5(3)*U.Y() + state5(4)*V.Y(); // y
  state7[2] = O.Z() + state5(3)*U.Z() + state5(4)*V.Z(); // z

  state7[3] = spu * (W.X() + state5(1)*U.X() + state5(2)*V.X()); // a_x
  state7[4] = spu * (W.Y() + state5(1)*U.Y() + state5(2)*V.Y()); // a_y
  state7[5] = spu * (W.Z() + state5(1)*U.Z() + state5(2)*V.Z()); // a_z

  // normalize dir
  double norm = 1. / sqrt(state7[3]*state7[3] + state7[4]*state7[4] + state7[5]*state7[5]);
  for (unsigned int i=3; i<6; ++i) state7[i] *= norm;

  state7[6] = state5(0); // q/p
}


TVectorD RKTrackRep::getState5(const M1x7& state7, const GFDetPlane& pl, double& spu) {

  const TVector3& U(pl.getU());
  const TVector3& V(pl.getV());

  fPos.SetXYZ(state7[0], state7[1], state7[2]);
  fPos -= pl.getO();

  fDir.SetXYZ(state7[3], state7[4], state7[5]);

  // force A to be in normal direction and set spu accordingly
  double AtW = fDir * pl.getNormal();
  spu = 1.;
  if (AtW < 0) {
    fDir *= -1.;
    AtW *= -1.;
    spu = -1.;
  }

  TVectorD state5(5);
  state5(0) = state7[6];
  state5(1) = fDir*U / AtW;
  state5(2) = fDir*V / AtW;
  state5(3) = fPos*U;
  state5(4) = fPos*V;

  return state5;
}



void RKTrackRep::transformPM7(const TMatrixD& in5x5, M7x7& out7x7,
                              const GFDetPlane& pl, const TVectorD& state5, const double&  spu,
                              TMatrixD* Jac) {

  // get vectors and aux variables
  const TVector3& U(pl.getU());
  const TVector3& V(pl.getV());
  TVector3 W(pl.getNormal());

  fpTilde.SetXYZ(spu * (W.X() + state5(1)*U.X() + state5(2)*V.X()), // a_x
                 spu * (W.Y() + state5(1)*U.Y() + state5(2)*V.Y()), // a_y
                 spu * (W.Z() + state5(1)*U.Z() + state5(2)*V.Z()));// a_z


  const double pTildeMag = fpTilde.Mag();
  const double pTildeMag2 = pTildeMag*pTildeMag;

  const double utpTildeOverpTildeMag2 = U*fpTilde / pTildeMag2;
  const double vtpTildeOverpTildeMag2 = V*fpTilde / pTildeMag2;

  //J_pM matrix is d(x,y,z,ax,ay,az,q/p) / d(q/p,u',v',u,v)   (out is 7x7)

  // d(x,y,z)/d(u)
  fJ_pM_5x7[21] = U.X(); // [3][0]
  fJ_pM_5x7[22] = U.Y(); // [3][1]
  fJ_pM_5x7[23] = U.Z(); // [3][2]
  // d(x,y,z)/d(v)
  fJ_pM_5x7[28] = V.X(); // [4][2]
  fJ_pM_5x7[29] = V.Y(); // [4][2]
  fJ_pM_5x7[30] = V.Z(); // [4][2]
  // d(q/p)/d(q/p)
  fJ_pM_5x7[6] = 1.; // not needed for array matrix multiplication
  // d(ax,ay,az)/d(u')
  double fact = spu / pTildeMag;
  fJ_pM_5x7[10] = fact * ( U.X() - fpTilde.X()*utpTildeOverpTildeMag2 ); // [1][3]
  fJ_pM_5x7[11] = fact * ( U.Y() - fpTilde.Y()*utpTildeOverpTildeMag2 ); // [1][4]
  fJ_pM_5x7[12] = fact * ( U.Z() - fpTilde.Z()*utpTildeOverpTildeMag2 ); // [1][5]
  // d(ax,ay,az)/d(v')
  fJ_pM_5x7[17] = fact * ( V.X() - fpTilde.X()*vtpTildeOverpTildeMag2 ); // [2][3]
  fJ_pM_5x7[18] = fact * ( V.Y() - fpTilde.Y()*vtpTildeOverpTildeMag2 ); // [2][4]
  fJ_pM_5x7[19] = fact * ( V.Z() - fpTilde.Z()*vtpTildeOverpTildeMag2 ); // [2][5]


  // since the Jacobian contains a lot of zeros, and the resulting cov has to be symmetric,
  // the multiplication can be done much faster directly on array level
  // out = J_pM^T * in5x5 * J_pM
  const M5x5& in5x5_ = *((M5x5*) in5x5.GetMatrixArray());
  J_pMTxcov5xJ_pM(fJ_pM_5x7, in5x5_, out7x7);

  if (Jac!=NULL){
    Jac->ResizeTo(5,7);
    *Jac = TMatrixD(5,7, &(fJ_pM_5x7[0]));
  }
}


void RKTrackRep::transformPM6(const TMatrixDSym& in5x5, M6x6& out6x6,
                              const GFDetPlane& pl, const TVectorD& state5, const double&  spu,
                              TMatrixD* Jac) {

  // get vectors and aux variables
  const TVector3& U(pl.getU());
  const TVector3& V(pl.getV());
  TVector3 W(pl.getNormal());

  fpTilde.SetXYZ(spu * (W.X() + state5(1)*U.X() + state5(2)*V.X()), // a_x
                 spu * (W.Y() + state5(1)*U.Y() + state5(2)*V.Y()), // a_y
                 spu * (W.Z() + state5(1)*U.Z() + state5(2)*V.Z()));// a_z

  const double pTildeMag = fpTilde.Mag();
  const double pTildeMag2 = pTildeMag*pTildeMag;

  const double utpTildeOverpTildeMag2 = U*fpTilde / pTildeMag2;
  const double vtpTildeOverpTildeMag2 = V*fpTilde / pTildeMag2;

  //J_pM matrix is d(x,y,z,px,py,pz) / d(q/p,u',v',u,v)       (out is 6x6)

  const double qop = state5(0);
  const double p = fCharge/qop; // momentum

  // d(px,py,pz)/d(q/p)
  double fact = -1. * p / (pTildeMag * qop);
  fJ_pM_5x6[3] = fact * fpTilde.X(); // [0][3]
  fJ_pM_5x6[4] = fact * fpTilde.Y(); // [0][4]
  fJ_pM_5x6[5] = fact * fpTilde.Z(); // [0][5]
  // d(px,py,pz)/d(u')
  fact = p * spu / pTildeMag;
  fJ_pM_5x6[9]  = fact * ( U.X() - fpTilde.X()*utpTildeOverpTildeMag2 ); // [1][3]
  fJ_pM_5x6[10] = fact * ( U.Y() - fpTilde.Y()*utpTildeOverpTildeMag2 ); // [1][4]
  fJ_pM_5x6[11] = fact * ( U.Z() - fpTilde.Z()*utpTildeOverpTildeMag2 ); // [1][5]
  // d(px,py,pz)/d(v')
  fJ_pM_5x6[15] = fact * ( V.X() - fpTilde.X()*vtpTildeOverpTildeMag2 ); // [2][3]
  fJ_pM_5x6[16] = fact * ( V.Y() - fpTilde.Y()*vtpTildeOverpTildeMag2 ); // [2][4]
  fJ_pM_5x6[17] = fact * ( V.Z() - fpTilde.Z()*vtpTildeOverpTildeMag2 ); // [2][5]
  // d(x,y,z)/d(u)
  fJ_pM_5x6[18] = U.X(); // [3][0]
  fJ_pM_5x6[19] = U.Y(); // [3][1]
  fJ_pM_5x6[20] = U.Z(); // [3][2]
  // d(x,y,z)/d(v)
  fJ_pM_5x6[24] = V.X(); // [4][0]
  fJ_pM_5x6[25] = V.Y(); // [4][1]
  fJ_pM_5x6[26] = V.Z(); // [4][2]


  // do the transformation
  // out = J_pM^T * in5x5 * J_pM
  const M5x5& in5x5_ = *((M5x5*) in5x5.GetMatrixArray());
  J_pMTxcov5xJ_pM(fJ_pM_5x6, in5x5_, out6x6);

  if (Jac!=NULL){
    Jac->ResizeTo(5,6);
    *Jac = TMatrixD(5,6, &(fJ_pM_5x6[0]));
  }
}


void RKTrackRep::transformM7P(const M7x7& in7x7, TMatrixDSym& out5x5,
                              const GFDetPlane& pl, const M1x7& state7,
                              TMatrixD* Jac) {

  out5x5.ResizeTo(5, 5);

  // get vectors and aux variables
  const TVector3& U(pl.getU());
  const TVector3& V(pl.getV());
  TVector3 W(pl.getNormal());

  fDir.SetXYZ(state7[3], state7[4], state7[5]);

  const double AtU = fDir*U;
  const double AtV = fDir*V;
  const double AtW = fDir*W;

  // J_Mp matrix is d(q/p,u',v',u,v) / d(x,y,z,ax,ay,az,q/p)   (in is 7x7)

  // d(u')/d(ax,ay,az)
  double fact = 1./(AtW*AtW);
  fJ_Mp_7x5[16] = fact * (U.X()*AtW - W.X()*AtU); // [3][1]
  fJ_Mp_7x5[21] = fact * (U.Y()*AtW - W.Y()*AtU); // [4][1]
  fJ_Mp_7x5[26] = fact * (U.Z()*AtW - W.Z()*AtU); // [5][1]
  // d(v')/d(ax,ay,az)
  fJ_Mp_7x5[17] = fact * (V.X()*AtW - W.X()*AtV); // [3][2]
  fJ_Mp_7x5[22] = fact * (V.Y()*AtW - W.Y()*AtV); // [4][2]
  fJ_Mp_7x5[27] = fact * (V.Z()*AtW - W.Z()*AtV); // [5][2]
  // d(q/p)/d(q/p)
  fJ_Mp_7x5[30] = 1.; // [6][0]  - not needed for array matrix multiplication
  //d(u)/d(x,y,z)
  fJ_Mp_7x5[3]  = U.X(); // [0][3]
  fJ_Mp_7x5[8]  = U.Y(); // [1][3]
  fJ_Mp_7x5[13] = U.Z(); // [2][3]
  //d(v)/d(x,y,z)
  fJ_Mp_7x5[4]  = V.X(); // [0][4]
  fJ_Mp_7x5[9]  = V.Y(); // [1][4]
  fJ_Mp_7x5[14] = V.Z(); // [2][4]


  // since the Jacobian contains a lot of zeros, and the resulting cov has to be symmetric,
  // the multiplication can be done much faster directly on array level
  // out5x5 = J_Mp^T * in * J_Mp
  M5x5& out5x5_ = *((M5x5*) out5x5.GetMatrixArray());
  J_MpTxcov7xJ_Mp(fJ_Mp_7x5, in7x7, out5x5_);

  if (Jac!=NULL){
    Jac->ResizeTo(7,5);
    *Jac = TMatrixD(7,5, &(fJ_Mp_7x5[0]));
  }
}


void RKTrackRep::transformM6P(const M6x6& in6x6, TMatrixDSym& out5x5,
                              const GFDetPlane& pl, const M1x7& state7,
                              TMatrixD* Jac) {

  out5x5.ResizeTo(5, 5);

  // get vectors and aux variables
  const TVector3& U(pl.getU());
  const TVector3& V(pl.getV());
  TVector3 W(pl.getNormal());

  fDir.SetXYZ(state7[3], state7[4], state7[5]);

  const double AtU = fDir*U;
  const double AtV = fDir*V;
  const double AtW = fDir*W;

  // J_Mp matrix is d(q/p,u',v',u,v) / d(x,y,z,px,py,pz)       (in is 6x6)

  const double qop = state7[6];
  const double p = fCharge/qop; // momentum

  //d(u)/d(x,y,z)
  fJ_Mp_6x5[3]  = U.X(); // [0][3]
  fJ_Mp_6x5[8]  = U.Y(); // [1][3]
  fJ_Mp_6x5[13] = U.Z(); // [2][3]
  //d(v)/d(x,y,z)
  fJ_Mp_6x5[4]  = V.X(); // [0][4]
  fJ_Mp_6x5[9]  = V.Y(); // [1][4]
  fJ_Mp_6x5[14] = V.Z(); // [2][4]
  // d(q/p)/d(px,py,pz)
  double fact = (-1.) * qop / p;
  fJ_Mp_6x5[15] = fact * fDir.X(); // [3][0]
  fJ_Mp_6x5[20] = fact * fDir.Y(); // [4][0]
  fJ_Mp_6x5[25] = fact * fDir.Z(); // [5][0]
  // d(u')/d(px,py,pz)
  fact = 1./(p*AtW*AtW);
  fJ_Mp_6x5[16] = fact * (U.X()*AtW - W.X()*AtU); // [3][1]
  fJ_Mp_6x5[21] = fact * (U.Y()*AtW - W.Y()*AtU); // [4][1]
  fJ_Mp_6x5[26] = fact * (U.Z()*AtW - W.Z()*AtU); // [5][1]
  // d(v')/d(px,py,pz)
  fJ_Mp_6x5[17] = fact * (V.X()*AtW - W.X()*AtV); // [3][2]
  fJ_Mp_6x5[22] = fact * (V.Y()*AtW - W.Y()*AtV); // [4][2]
  fJ_Mp_6x5[27] = fact * (V.Z()*AtW - W.Z()*AtV); // [5][2]

  // do the transformation
  // out5x5 = J_Mp^T * in * J_Mp
  M5x5& out5x5_ = *((M5x5*) out5x5.GetMatrixArray());
  J_MpTxcov6xJ_Mp(fJ_Mp_6x5, in6x6, out5x5_);

  if (Jac!=NULL){
    Jac->ResizeTo(6,5);
    *Jac = TMatrixD(6,5, &(fJ_Mp_6x5[0]));;
  }
}



TVector3 RKTrackRep::getPos(const GFDetPlane& pl){
#ifdef DEBUG
  std::cout << "RKTrackRep::getPos()\n";
#endif
  M1x7 state7;
  getState7(state7);
  if(pl!=fRefPlane) Extrap(pl, state7);
  return TVector3(state7[0], state7[1], state7[2]);
}


TVector3 RKTrackRep::getMom(const GFDetPlane& pl){
#ifdef DEBUG
  std::cout << "RKTrackRep::getMom()\n";
#endif
  M1x7 state7;
  getState7(state7);
  if(pl!=fRefPlane) Extrap(pl, state7);

  TVector3 mom(state7[3], state7[4], state7[5]);
  mom.SetMag(fCharge/state7[6]);
  return mom;
}


void RKTrackRep::getPosMom(const GFDetPlane& pl,TVector3& pos, TVector3& mom){
#ifdef DEBUG
  std::cout << "RKTrackRep::getPosMom()\n";
#endif
  M1x7 state7;
  getState7(state7);
  if(pl!=fRefPlane) Extrap(pl, state7);

  pos.SetXYZ(state7[0], state7[1], state7[2]);
  mom.SetXYZ(state7[3], state7[4], state7[5]);
  mom.SetMag(fCharge/state7[6]);
}


void RKTrackRep::getPosMomCov(const GFDetPlane& pl, TVector3& pos, TVector3& mom, TMatrixDSym& cov6x6){
  TVectorD statePred(fState);
  TMatrixDSym covPred(fCov);
  double spu(fSpu);

  if(pl != fRefPlane) {
    extrapolate(pl, statePred, covPred);
    spu = fCacheSpu;
  }

  M1x7 state7;
  getState7(state7, statePred, pl, spu);

  // cov
  cov6x6.ResizeTo(6, 6); // make sure cov has correct dimensions
  M6x6& cov6x6_ = *((M6x6*) cov6x6.GetMatrixArray());
  transformPM6(covPred, cov6x6_, pl, statePred, spu);

  pos.SetXYZ(state7[0], state7[1], state7[2]);
  mom.SetXYZ(state7[3], state7[4], state7[5]);
  mom.SetMag(fCharge/state7[6]);
}


void RKTrackRep::setPosMomCov(const TVector3& pos, const TVector3& mom, const TMatrixDSym& cov6x6){

  if (cov6x6.GetNcols()!=6 || cov6x6.GetNrows()!=6){
    GFException exc("RKTrackRep::setPosMomCov ==> cov has to be 6x6 (x, y, z, px, py, pz)",__LINE__,__FILE__);
    throw exc;
  }

  if (fCharge == 0){
    GFException exc("RKTrackRep::setPosMomCov ==> charge is 0. setPosMomCov cannot work with fCharge == 0 ",__LINE__,__FILE__);
    throw exc;
  }

  // fCharge does not change!
  calcState(pos, mom);

  fCachePlane = fRefPlane;
  fCacheSpu = 1.;

  M1x7 state7;
  getState7(state7);

  const M6x6& cov6x6_ = *((M6x6*) cov6x6.GetMatrixArray());

  transformM6P(cov6x6_, fCov, fRefPlane, state7);
}



double RKTrackRep::extrapolateToPoint(const TVector3& pos,
                                      TVector3& poca,
                                      TVector3& dirInPoca){

#ifdef DEBUG
  std::cout << "RKTrackRep::extrapolateToPoint()\n";
#endif

  static const unsigned int maxIt(1000);

  M1x7 state7;
  getState7(state7);
  fDir.SetXYZ(state7[3], state7[4], state7[5]);

  double step(0.), lastStep(0.), maxStep(1.E99), angle(0), distToPoca(0), tracklength(0);
  TVector3 lastDir(0,0,0);
  TVector3 lastPos(0,0,0);

  double momentum = fabs(fCharge/state7[6]);// momentum [GeV]
  TVector3 initmom(momentum*fDir);
  TVector3 initpos(pos);
  double errorS = 0;

  GFDetPlane pl(pos, fDir);
  unsigned int iterations(0);

  while(true){
    lastStep = step;
    lastDir = fDir;
    lastPos = poca;

    step =  this->Extrap(pl, state7, NULL, true, maxStep);
    tracklength += step;
    errorS += step;

    fDir.SetXYZ(state7[3], state7[4], state7[5]);
    poca.SetXYZ(state7[0], state7[1], state7[2]);

    if(step>0. && lastPos!=initpos) {
      TVector3 finalpos = lastPos;
      TVector3 finalmom = momentum*lastDir;
      PropagateError(initpos, initmom,
                     finalpos, finalmom,
                     errorS, fCharge);

      initpos = poca;
      initmom = momentum*fDir;
      errorS = 0.;
    }

    // check break conditions
    angle = fabs(fDir.Angle((pos-poca))-TMath::PiOver2()); // angle between direction and connection to point - 90 deg
    distToPoca = (pos-poca).Mag();
    if (angle*distToPoca < 0.1*MINSTEP) break;
    if(++iterations == maxIt) {
      GFException exc("RKTrackRep::extrapolateToPoint ==> extrapolation to point failed, maximum number of iterations reached",__LINE__,__FILE__);
      throw exc;
    }

    // if lastStep and step have opposite sign, the real normal vector lies somewhere between the last two normal vectors (i.e. the directions)
    // -> try mean value of the two (normalization not needed)
    if (lastStep*step < 0){
      fDir += lastDir;
      maxStep = 0.5*fabs(lastStep); // make it converge!
    }

    pl.setNormal(fDir);
  }

  dirInPoca.SetXYZ(state7[3], state7[4], state7[5]);

#ifdef DEBUG
  std::cout << "RKTrackRep::extrapolateToPoint(): Reached POCA after " << iterations+1 << " iterations. Distance: " << (pos-poca).Mag() << " cm. Angle deviation: " << dirInPoca.Angle((pos-poca))-TMath::PiOver2() << " rad \n";
#endif

  return tracklength;
}


TVector3 RKTrackRep::poca2Line(const TVector3& extr1,const TVector3& extr2,const TVector3& point) const {
  
  TVector3 pocaOnLine(extr2);
  pocaOnLine -= extr1; // wireDir

  if(pocaOnLine.Mag()<1.E-8){
    GFException exc("RKTrackRep::poca2Line ==> try to find POCA between line and point, but the line is really just a point",__LINE__,__FILE__);
    throw exc;
  }

  double t = 1./(pocaOnLine.Mag2()) * ((point*pocaOnLine) + extr1.Mag2() - (extr1*extr2));
  pocaOnLine *= t;
  pocaOnLine += extr1;
  return pocaOnLine; // = extr1 + t*wireDir
}


double RKTrackRep::extrapolateToLine(const TVector3& point1,
                                     const TVector3& point2,
                                     TVector3& poca,
                                     TVector3& dirInPoca,
                                     TVector3& poca_onwire){

#ifdef DEBUG
  std::cout << "RKTrackRep::extrapolateToLine(), (x,y) = (" << point1.X() << ", " << point1.Y() << ")\n";
#endif

  static const unsigned int maxIt(1000);

  M1x7 state7;
  getState7(state7);
  fDir.SetXYZ(state7[3], state7[4], state7[5]);

  double step(0.), lastStep(0.), maxStep(1.E99), angle(0), distToPoca(0), tracklength(0);
  TVector3 wireDir(point2);
  wireDir -= point1;
  wireDir.SetMag(1.);
  TVector3 lastDir(0,0,0);

  GFDetPlane pl(point1, fDir.Cross(wireDir), wireDir);
  unsigned int iterations(0);

  while(true){
    lastStep = step;
    lastDir = fDir;

    step = this->Extrap(pl, state7, NULL, true, maxStep);
    tracklength += step;

    fDir.SetXYZ(state7[3], state7[4], state7[5]);
    poca.SetXYZ(state7[0], state7[1], state7[2]);
    poca_onwire = poca2Line(point1, point2, poca);

    // check break conditions
    angle = fabs(fDir.Angle((poca_onwire-poca))-TMath::PiOver2()); // angle between direction and connection to point - 90 deg
    distToPoca = (poca_onwire-poca).Mag();
    if (angle*distToPoca < 0.1*MINSTEP) break;
    if(++iterations == maxIt) {
      GFException exc("RKTrackRep::extrapolateToLine ==> extrapolation to line failed, maximum number of iterations reached",__LINE__,__FILE__);
      throw exc;
    }

    // if lastStep and step have opposite sign, the real normal vector lies somewhere between the last two normal vectors (i.e. the directions)
    // -> try mean value of the two (normalization not needed)
    if (lastStep*step < 0){
      fDir += lastDir;
      maxStep = 0.5*fabs(lastStep); // make it converge!
    }

    pl.setU(fDir.Cross(wireDir));
  }

  dirInPoca.SetXYZ(state7[3], state7[4], state7[5]);

#ifdef DEBUG
  std::cout << "RKTrackRep::extrapolateToLine(): Reached POCA after " << iterations+1 << " iterations. Distance: " << (poca_onwire-poca).Mag() << " cm. Angle deviation: " << dirInPoca.Angle((poca_onwire-poca))-TMath::PiOver2() << " rad \n";
#endif

  return tracklength;
}


double RKTrackRep::extrapolate(const GFDetPlane& pl, 
                               TVectorD& statePred,
                               TMatrixDSym& covPred){
  
#ifdef DEBUG
  std::cout << "RKTrackRep::extrapolate(pl, statePred, covPred)\n";
#endif

  M1x7 state7;
  getState7(state7);
  M7x7 cov7x7;

  transformPM7(fCov, cov7x7, fRefPlane, fState, fSpu);

  double coveredDistance = Extrap(pl, state7, &cov7x7);
  
  statePred.ResizeTo(5);
  statePred = getState5(state7, pl, fCacheSpu);
  fCachePlane = pl;

  covPred.ResizeTo(5, 5);
  transformM7P(cov7x7, covPred, pl, state7);

  return coveredDistance;
}


double RKTrackRep::extrapolate(const GFDetPlane& pl, 
                               TVectorD& statePred){

#ifdef DEBUG
  std::cout << "RKTrackRep::extrapolate(pl, statePred)\n";
#endif

  M1x7 state7;
  getState7(state7);
  double coveredDistance = Extrap(pl, state7);
  double spu;
  statePred.ResizeTo(5);
  statePred = getState5(state7, pl, spu);

  return coveredDistance;
}


double RKTrackRep::stepalong(double h, TVector3& pos, TVector3& dir){

#ifdef DEBUG
  std::cout << "RKTrackRep::stepalong()\n";
#endif

  TVector3 dest;

  static const unsigned int maxIt(30);
  double coveredDistance(0.);

  M1x7 state7;
  getState7(state7);

  GFDetPlane pl;
  unsigned int iterations(0);

  while(true){
    pos.SetXYZ(state7[0], state7[1], state7[2]);
    dir.SetXYZ(state7[3], state7[4], state7[5]);
    dir.SetMag(1.);

    dest = pos + (h - coveredDistance) * dir;

    pl.setON(dest, dir);
    coveredDistance += this->Extrap(pl, state7);

    if(fabs(h - coveredDistance)<MINSTEP) break;
    if(++iterations == maxIt) {
      GFException exc("RKTrackRep::stepalong ==> maximum number of iterations reached",__LINE__,__FILE__);
      throw exc;
    }
  }

  pos.SetXYZ(state7[0], state7[1], state7[2]);
  dir.SetXYZ(state7[3], state7[4], state7[5]);

  return coveredDistance;
}



double RKTrackRep::Extrap( const GFDetPlane& plane, M1x7& state7, M7x7* cov, bool onlyOneStep, double maxStep) {

  static const unsigned int maxNumIt(500);
  unsigned int numIt(0);

  const bool calcCov(cov!=NULL);
  double coveredDistance(0.);
  double sumDistance(0.);

  while(true){

#ifdef DEBUG
    std::cout << "\n============ RKTrackRep::Extrap loop nr. " << numIt << " ============\n";
#endif

    if(numIt++ > maxNumIt){
      GFException exc("RKTrackRep::Extrap ==> maximum number of iterations exceeded",__LINE__,__FILE__);
      exc.setFatal();
      throw exc;
    }

    // initialize cov with unit matrix
    if(calcCov){
      memcpy(fOldCov, cov, 7*7*sizeof(double));
      memset(cov,0x00,49*sizeof(double));
      for(int i=0; i<7; ++i) (*cov)[8*i] = 1.;
    }

    fDirectionBefore.SetXYZ(state7[3], state7[4], state7[5]); // direction before propagation

    // propagation
    std::vector<GFPointPath> points;
    points.reserve(50);

    bool checkJacProj = true;

    if( ! this->RKutta(plane, state7, cov, coveredDistance, points, checkJacProj, onlyOneStep, maxStep) ) {
      GFException exc("RKTrackRep::Extrap ==> Runge Kutta propagation failed",__LINE__,__FILE__);
      exc.setFatal();
      throw exc;
    }

    fPos.SetXYZ(state7[0], state7[1], state7[2]);
    if (!fNoMaterial) points.push_back(GFPointPath(fPos, 0)); // add last point

#ifdef DEBUG
    std::cout<<"Original points \n";
    for (unsigned int i=0; i<points.size(); ++i){
      points[i].Print();
    }
    std::cout<<"\n";
#endif

    fDirectionAfter.SetXYZ(state7[3], state7[4], state7[5]); // direction after propagation

    sumDistance+=coveredDistance;

    // filter points
    if (!fNoMaterial) { // points are only filled if mat fx are on
      if(points.size() > 2){ // check if there are at least three points
        double firstStep(points[0].getPath());
        for (unsigned int i=points.size()-2; i>0; --i){
          if (points[i].getPath() * firstStep < 0 || fabs(points[i].getPath()) < MINSTEP){
            points[i-1].addToPath(points[i].getPath());
            points.erase(points.begin()+i);
          }
        }
      }
#ifdef DEBUG
      std::cout<<"Filtered points \n";
      for (unsigned int i=0; i<points.size(); ++i){
        points[i].Print();
      }
      std::cout<<"\n";
#endif
    }


    if(calcCov) memset(fNoise,0x00,7*7*sizeof(double)); // set fNoise to 0


    // call MatFX
    unsigned int nPoints(points.size());
//     if (!fNoMaterial && nPoints>0){
//       // momLoss has a sign - negative loss means momentum gain
//       double momLoss = GFMaterialEffects::getInstance()->effects(points,
//                                                                  fabs(fCharge/state7[6]), // momentum
//                                                                  fPdg,
//                                                                  fXX0,
//                                                                  fNoise,
//                                                                  (double *)cov,
//                                                                  &fDirectionBefore,
//                                                                  &fDirectionAfter);

// #ifdef DEBUG
//       std::cout << "momLoss: " << momLoss << " GeV; relative: " << momLoss/fabs(fCharge/state7[6]) << "\n";
// #endif

//       // do momLoss only for defined 1/momentum .ne.0
//       if(fabs(state7[6])>1.E-10) state7[6] = fCharge/(fabs(fCharge/state7[6])-momLoss);
//     } // finished MatFX

    if(calcCov){ // propagate cov and add noise
      // numerical check:
      for(unsigned int i=0; i<7*7; ++i){
        if(fabs((*cov)[i]) > 1.E100){
          GFException exc("RKTrackRep::Extrap ==> covariance matrix exceeds numerical limits",__LINE__,__FILE__);
          exc.setFatal();
          throw exc;
        }
      }

      // cov = Jac^T * oldCov * Jac;
      // last column of jac is [0,0,0,0,0,0,1]
      // cov is symmetric
      J_MMTxcov7xJ_MM(*cov, fOldCov);
      memcpy(cov, fOldCov, 7*7*sizeof(double));

      // add noise to cov
      for (int i=0; i<7*7; ++i) (*cov)[i] += fNoise[i];

    } // finished propagate cov and add noise

    if (onlyOneStep) break;

    //we arrived at the destination plane, if we point to the active area of the plane (if it is finite), and the distance is below threshold
    if( plane.distance(fPos) < MINSTEP  &&  plane.inActive(fPos, fDirectionAfter)) {
      // check if Jacobian has been projected onto plane; Otherwise make another iteration
      if (calcCov && !checkJacProj && nPoints>0){
#ifdef DEBUG
        std::cout << "Jacobian was not projected onto destination plane -> one more iteration. \n";
#endif
        continue;
      }
#ifdef DEBUG
      std::cout << "arrived at plane with a distance of  " << plane.distance(fPos) << " cm left. \n";
#endif
      break;
    }

  }

  return sumDistance;
}


//
// Runge-Kutta method for tracking a particles through a magnetic field.            
// Uses Nystroem algorithm (See Handbook Nat. Bur. of Standards, procedure 25.5.20)  
//                                                                                  
// Input parameters:                                                               
//    SU     - plane parameters                                                                                                                         
//    SU[0]  - direction cosines normal to surface Ex                               
//    SU[1]  -          -------                    Ey                               
//    SU[2]  -          -------                    Ez; Ex*Ex+Ey*Ey+Ez*Ez=1          
//    SU[3]  - distance to surface from (0,0,0) > 0 cm                                 
//
//    state7 - initial parameters (coordinates(cm), direction,
//             charge/momentum (Gev-1) 
//    cov      and derivatives this parameters  (7x7)            
//         
//    X        	Y        	Z        	Ax       	Ay       	Az       	q/P                   
//    state7[0] state7[1] state7[2] state7[3] state7[4] state7[5] state7[6]  
//
//    dX/dp    	dY/dp    	dZ/dp    	dAx/dp   	dAy/dp   	dAz/dp   	d(q/P)/dp
//    cov[ 0]   cov[ 1]   cov[ 2]   cov[ 3]   cov[ 4]   cov[ 5]   cov[ 6]   			      d()/dp1  
//
//    cov[ 7]   cov[ 8]   cov[ 9]   cov[10]   cov[11]   cov[12]   cov[13]   		      	d()/dp2        
//    ............................................................................		d()/dpND       
//                                                                                  
// Authors: R.Brun, M.Hansroul, V.Perevoztchikov (Geant3)                           
//  
bool RKTrackRep::RKutta (const GFDetPlane& plane,
                         M1x7& state7,
                         M7x7* cov,
                         double& coveredDistance,
                         std::vector<GFPointPath>& points,
                         bool& checkJacProj,
                         bool onlyOneStep,
                         double maxStep) {

  // limits, check-values, etc. Can be tuned!
  static const double Wmax   = 3000.;           // max. way allowed [cm]
  static const double AngleMax = 6.3;           // max. total angle change of momentum. Prevents extrapolating a curler round and round if no active plane is found.
  static const double Pmin   = 4.E-3;           // minimum momentum for propagation [GeV]
  static const unsigned int maxNumIt = 1000;    // maximum number of iterations in main loop
  // Aux parameters
  M1x3&   R           = *((M1x3*) &state7[0]);  // Start coordinates  [cm] 	(x,  y,  z)
  M1x3&   A           = *((M1x3*) &state7[3]);  // Start directions 	      (ax, ay, az); 	ax^2+ay^2+az^2=1
  M1x3    SA          = {0.,0.,0.};             // Start directions derivatives dA/S
  double  Way         = 0.;                     // Sum of absolute values of all extrapolation steps [cm]
  bool    atPlane = false;                      // stepper thinks that the plane will be reached in that step -> linear extrapolation and projection of jacobian
  bool    momLossExceeded = false;              // stepper had to limit stepsize due to momentum loss -> no next RKutta loop, no linear extrapolation
  fPos.SetXYZ(R[0],R[1],R[2]);                  // position
  fDir.SetXYZ(A[0],A[1],A[2]);                  // direction
  double   momentum   = fabs(fCharge/state7[6]);// momentum [GeV]
  double   relMomLoss = 0;                      // relative momentum loss in RKutta
  double   deltaAngle = 0.;                     // total angle by which the momentum has changed during extrapolation
  double   An(0), S(0), Sl(0), CBA(0);
  M1x4     SU = {0.,0.,0.,0.};


#ifdef DEBUG
  std::cout << "RKTrackRep::RKutta \n";
  std::cout << "position: "; fPos.Print();
  std::cout << "direction: "; fDir.Print();
  std::cout << "momentum: " << momentum << " GeV\n";
  std::cout << "destination: "; plane.Print();
  std::cout << "charge: " << fCharge << std::endl;
#endif

  checkJacProj = false;

  // check momentum
  if(momentum < Pmin){
    std::ostringstream sstream;
    sstream << "RKTrackRep::RKutta ==> momentum too low: " << momentum*1000. << " MeV";
    GFException exc(sstream.str(),__LINE__,__FILE__);
    exc.setFatal();
    throw exc;
  }
  

  // make SU vector point away from origin
  const TVector3 W(plane.getNormal());
  if(W*plane.getO() > 0){SU[0] =     W.X();  SU[1] =     W.Y();  SU[2] =     W.Z();}
  else                  {SU[0] = -1.*W.X();  SU[1] = -1.*W.Y();  SU[2] = -1.*W.Z();  }
  SU[3] = plane.distance(0., 0., 0.);

  unsigned int counter(0);

  // Step estimation (signed)
  S = estimateStep(points, fPos, fDir, SU, plane, momentum, relMomLoss, deltaAngle, momLossExceeded, atPlane, maxStep);
  if (fabs(S) < 0.001*MINSTEP) {
#ifdef DEBUG
    std::cout << " RKutta - step too small -> break \n";
#endif
    ++counter; // skip the main loop, go to linear extrapolation step (will be skipped) and just project jacobian
  }


  TVector3 initpos = fPos;
  TVector3 initmom = momentum*fDir;
  double errorS = 0.;

  //
  // Main loop of Runge-Kutta method
  //
  while (fabs(S) >= MINSTEP || counter == 0) {

    if(++counter > maxNumIt){
      GFException exc("RKTrackRep::RKutta ==> maximum number of iterations exceeded",__LINE__,__FILE__);
      exc.setFatal();
      throw exc;
    }

#ifdef DEBUG
    std::cout << "------ RKutta main loop nr. " << counter-1 << " ------\n";
#endif

    // update paths
    coveredDistance += S;				// add stepsize to way (signed)
    Way  += fabs(S);

    // check way limit
    if(Way > Wmax){
      std::ostringstream sstream;
      sstream << "RKTrackRep::RKutta ==> Total extrapolation length is longer than length limit : " << Way << " cm !";
      GFException exc(sstream.str(),__LINE__,__FILE__);
      exc.setFatal();
      throw exc;
    }

    RKPropagate(state7, cov, SA, S); // the actual Runkge Kutta propagation
    fPos.SetXYZ(R[0],R[1],R[2]);
    fDir.SetXYZ(A[0],A[1],A[2]);

    if (onlyOneStep) return(true);

    // if stepsize has been limited by material, break the loop and return. No linear extrapolation!
    if (momLossExceeded) {
#ifdef DEBUG
      std::cout<<" momLossExceeded -> return(true); \n";
#endif
      return(true);
    }

    // estimate Step for next loop or linear extrapolation
    Sl = S;	// last S used
    S = estimateStep(points, fPos, fDir, SU, plane, momentum, relMomLoss, deltaAngle, momLossExceeded, atPlane, maxStep);

    errorS += Sl;

    if(S>0.) {
      TVector3 finalpos = fPos;
      TVector3 finalmom = momentum*fDir;
      PropagateError(initpos, initmom,
                     finalpos, finalmom,
                     errorS, fCharge);

      initpos = fPos;
      initmom = momentum*fDir;
      errorS = 0.;
    }

    if (atPlane && fabs(S) < MINSTEP) {
#ifdef DEBUG
      std::cout<<" (atPlane && fabs(S) < MINSTEP) -> break; \n";
#endif
      break; // else if at plane: do linear extrapolation
    }
    if (momLossExceeded && fabs(S) < MINSTEP) {
#ifdef DEBUG
      std::cout<<" (momLossExceeded && fabs(S) < MINSTEP) -> return(true); \n";
#endif
      return(true); // no linear extrapolation!
    }

    // check if total angle is bigger than AngleMax. Can happen if a curler should be fitted and it does not hit the active area of the next plane.
    if (fabs(deltaAngle) > AngleMax){
      std::ostringstream sstream;
      sstream << "RKTrackRep::RKutta ==> Do not get to an active plane! Already extrapolated " << deltaAngle * 180 / TMath::Pi() << "°.";
      GFException exc(sstream.str(),__LINE__,__FILE__);
      exc.setFatal();
      throw exc;
    }

    // check if we went back and forth multiple times -> we don't come closer to the plane!
    if (counter > 3){
      if (S                          *points[counter-1].getPath() < 0 &&
          points[counter-1].getPath()*points[counter-2].getPath() < 0 &&
          points[counter-2].getPath()*points[counter-3].getPath() < 0){
        GFException exc("RKTrackRep::RKutta ==> Do not get closer to plane!",__LINE__,__FILE__);
        exc.setFatal();
        throw exc;
      }
    }

  } //end of main loop

  if(initpos != fPos) {
    TVector3 finalpos = fPos;
    TVector3 finalmom = momentum*fDir;
    PropagateError(initpos, initmom,
                   finalpos, finalmom,
                   errorS, fCharge);
  }
  
  
  //
  // linear extrapolation to surface
  //
  if (atPlane) {

    if (fabs(Sl) > 0.001*MINSTEP){
#ifdef DEBUG
      std::cout << " RKutta - linear extrapolation to surface\n";
#endif
      Sl = 1./Sl;        // Sl = inverted last Stepsize Sl
      
      // normalize SA
      SA[0]*=Sl;  SA[1]*=Sl;  SA[2]*=Sl; // SA/Sl = delta A / delta way; local derivative of A with respect to the length of the way

      // calculate A
      A[0] += SA[0]*S;   	// S  = distance to surface
      A[1] += SA[1]*S;	  // A = A + S * SA*Sl
      A[2] += SA[2]*S;

      // normalize A
      CBA = 1./sqrt(A[0]*A[0]+A[1]*A[1]+A[2]*A[2]);  // 1/|A|
      A[0] *= CBA; A[1] *= CBA; A[2] *= CBA;

      R[0] += S*(A[0]-0.5*S*SA[0]);    // R = R + S*(A - 0.5*S*SA); approximation for final point on surface
      R[1] += S*(A[1]-0.5*S*SA[1]);
      R[2] += S*(A[2]-0.5*S*SA[2]);
    }
#ifdef DEBUG
    else {
      std::cout << " RKutta - last stepsize too small -> can't do linear extrapolation! \n";
    }
#endif

    //
    // Project Jacobian of extrapolation onto destination plane
    //
    if(cov != NULL){
      if (checkJacProj && points.size()>0){
        GFException exc("RKTrackRep::Extrap ==> covariance is projected onto destination plane again",__LINE__,__FILE__);
        throw exc;
      }
      checkJacProj = true;
#ifdef DEBUG
      std::cout << "  Project Jacobian of extrapolation onto destination plane\n";
#endif
      An = A[0]*SU[0] + A[1]*SU[1] + A[2]*SU[2];
      fabs(An) > 1.E-7 ? An=1./An : An = 0; // 1/A_normal
      double norm;
      for(int i=0; i<49; i+=7) {
        norm = ((*cov)[i]*SU[0] + (*cov)[i+1]*SU[1] + (*cov)[i+2]*SU[2])*An;	// dR_normal / A_normal
        (*cov)[i]   -= norm*A [0];   (*cov)[i+1] -= norm*A [1];   (*cov)[i+2] -= norm*A [2];
        (*cov)[i+3] -= norm*SA[0];   (*cov)[i+4] -= norm*SA[1];   (*cov)[i+5] -= norm*SA[2];
      }
    }

    coveredDistance += S;
    Way  += fabs(S);
  } // end of linear extrapolation to surface
  if(Way<0) printf("Way is negative : %.2f", Way);

  return(true);
}


void
RKTrackRep::RKPropagate(M1x7& state7,
                        M7x7* cov,
                        M1x3& SA,
                        double S,
                        bool varField) const {

  // important fixed numbers
  static const double EC     = 0.000149896229;  // c/(2*10^12) resp. c/2Tera
  static const double P3     = 1./3.;           // 1/3
  // Aux parameters
  M1x3&   R           = *((M1x3*) &state7[0]);       // Start coordinates  [cm]  (x,  y,  z)
  M1x3&   A           = *((M1x3*) &state7[3]);       // Start directions         (ax, ay, az);   ax^2+ay^2+az^2=1
  double  S3(0), S4(0), PS2(0);
  M1x3     H0 = {0.,0.,0.}, H1 = {0.,0.,0.}, H2 = {0.,0.,0.}, r = {0.,0.,0.};
  // Variables for RKutta main loop
  double   A0(0), A1(0), A2(0), A3(0), A4(0), A5(0), A6(0);
  double   B0(0), B1(0), B2(0), B3(0), B4(0), B5(0), B6(0);
  double   C0(0), C1(0), C2(0), C3(0), C4(0), C5(0), C6(0);

  bool calcCov(cov != NULL);

  //
  // Runge Kutta Extrapolation
  //
  S3 = P3*S;
  S4 = 0.25*S;
  PS2 = state7[6]*EC * S;

  // First point
  r[0] = R[0];           r[1] = R[1];           r[2]=R[2];
  TVector3 pos(r[0], r[1], r[2]);// vector of start coordinates R0  (x, y, z)
  TVector3 field(getFieldValKiloGauss(pos));       // magnetic field in 10^-4 T = kGauss
  H0[0] = PS2*field.X(); H0[1] = PS2*field.Y(); H0[2] = PS2*field.Z();     // H0 is PS2*(Hx, Hy, Hz) @ R0
  A0 = A[1]*H0[2]-A[2]*H0[1]; B0 = A[2]*H0[0]-A[0]*H0[2]; C0 = A[0]*H0[1]-A[1]*H0[0]; // (ax, ay, az) x H0
  A2 = A[0]+A0              ; B2 = A[1]+B0              ; C2 = A[2]+C0              ; // (A0, B0, C0) + (ax, ay, az)
  if (varField) {
    A1 = A2+A[0]            ; B1 = B2+A[1]              ; C1 = C2+A[2]              ; // (A0, B0, C0) + 2*(ax, ay, az)
  }

  // Second point
  if (varField) {
    r[0] += A1*S4;         r[1] += B1*S4;         r[2] += C1*S4;
    pos.SetXYZ(r[0], r[1], r[2]);
    field = getFieldValKiloGauss(pos);
    H1[0] = field.X()*PS2; H1[1] = field.Y()*PS2; H1[2] = field.Z()*PS2; // H1 is PS2*(Hx, Hy, Hz) @ (x, y, z) + 0.25*S * [(A0, B0, C0) + 2*(ax, ay, az)]
  }
  else if (calcCov) memcpy(H1, H0, 3*sizeof(double));
  A3 = B2*H1[2]-C2*H1[1]+A[0]; B3 = C2*H1[0]-A2*H1[2]+A[1]; C3 = A2*H1[1]-B2*H1[0]+A[2]; // (A2, B2, C2) x H1 + (ax, ay, az)
  A4 = B3*H1[2]-C3*H1[1]+A[0]; B4 = C3*H1[0]-A3*H1[2]+A[1]; C4 = A3*H1[1]-B3*H1[0]+A[2]; // (A3, B3, C3) x H1 + (ax, ay, az)
  A5 = A4-A[0]+A4            ; B5 = B4-A[1]+B4            ; C5 = C4-A[2]+C4            ; //    2*(A4, B4, C4) - (ax, ay, az)

  // Last point
  if (varField) {
    r[0]=R[0]+S*A4;         r[1]=R[1]+S*B4;         r[2]=R[2]+S*C4;  //setup.Field(r,H2);
    pos.SetXYZ(r[0], r[1], r[2]);
    field = getFieldValKiloGauss(pos);
    H2[0] = field.X()*PS2;  H2[1] = field.Y()*PS2;  H2[2] = field.Z()*PS2; // H2 is PS2*(Hx, Hy, Hz) @ (x, y, z) + 0.25*S * (A4, B4, C4)
  }
  else if (calcCov) memcpy(H2, H0, 3*sizeof(double));
  A6 = B5*H2[2]-C5*H2[1]; B6 = C5*H2[0]-A5*H2[2]; C6 = A5*H2[1]-B5*H2[0]; // (A5, B5, C5) x H2

  //
  // Derivatives of track parameters
  //
  if(calcCov){
    double   dA0(0), dA2(0), dA3(0), dA4(0), dA5(0), dA6(0);
    double   dB0(0), dB2(0), dB3(0), dB4(0), dB5(0), dB6(0);
    double   dC0(0), dC2(0), dC3(0), dC4(0), dC5(0), dC6(0);

    // d(x, y, z)/d(x, y, z) submatrix is unit matrix
    (*cov)[0] = 1;  (*cov)[8] = 1;  (*cov)[16] = 1;
    // d(ax, ay, az)/d(ax, ay, az) submatrix is 0
    // start with d(x, y, z)/d(ax, ay, az)
    for(int i=3*7; i<49; i+=7) {

      if(i==42) {(*cov)[i+3]*=state7[6]; (*cov)[i+4]*=state7[6]; (*cov)[i+5]*=state7[6];}

      //first point
      dA0 = H0[2]*(*cov)[i+4]-H0[1]*(*cov)[i+5];    // dA0/dp }
      dB0 = H0[0]*(*cov)[i+5]-H0[2]*(*cov)[i+3];    // dB0/dp  } = dA x H0
      dC0 = H0[1]*(*cov)[i+3]-H0[0]*(*cov)[i+4];    // dC0/dp }

      if(i==42) {dA0+=A0; dB0+=B0; dC0+=C0;}     // if last row: (dA0, dB0, dC0) := (dA0, dB0, dC0) + (A0, B0, C0)

      dA2 = dA0+(*cov)[i+3];        // }
      dB2 = dB0+(*cov)[i+4];        //  } = (dA0, dB0, dC0) + dA
      dC2 = dC0+(*cov)[i+5];        // }

      //second point
      dA3 = (*cov)[i+3]+dB2*H1[2]-dC2*H1[1];    // dA3/dp }
      dB3 = (*cov)[i+4]+dC2*H1[0]-dA2*H1[2];    // dB3/dp  } = dA + (dA2, dB2, dC2) x H1
      dC3 = (*cov)[i+5]+dA2*H1[1]-dB2*H1[0];    // dC3/dp }

      if(i==42) {dA3+=A3-A[0]; dB3+=B3-A[1]; dC3+=C3-A[2];} // if last row: (dA3, dB3, dC3) := (dA3, dB3, dC3) + (A3, B3, C3) - (ax, ay, az)

      dA4 = (*cov)[i+3]+dB3*H1[2]-dC3*H1[1];    // dA4/dp }
      dB4 = (*cov)[i+4]+dC3*H1[0]-dA3*H1[2];    // dB4/dp  } = dA + (dA3, dB3, dC3) x H1
      dC4 = (*cov)[i+5]+dA3*H1[1]-dB3*H1[0];    // dC4/dp }

      if(i==42) {dA4+=A4-A[0]; dB4+=B4-A[1]; dC4+=C4-A[2];} // if last row: (dA4, dB4, dC4) := (dA4, dB4, dC4) + (A4, B4, C4) - (ax, ay, az)

      //last point
      dA5 = dA4+dA4-(*cov)[i+3];      // }
      dB5 = dB4+dB4-(*cov)[i+4];      //  } =  2*(dA4, dB4, dC4) - dA
      dC5 = dC4+dC4-(*cov)[i+5];      // }

      dA6 = dB5*H2[2]-dC5*H2[1];      // dA6/dp }
      dB6 = dC5*H2[0]-dA5*H2[2];      // dB6/dp  } = (dA5, dB5, dC5) x H2
      dC6 = dA5*H2[1]-dB5*H2[0];      // dC6/dp }

      if(i==42) {dA6+=A6; dB6+=B6; dC6+=C6;}     // if last row: (dA6, dB6, dC6) := (dA6, dB6, dC6) + (A6, B6, C6)

      if(i==42) {
        (*cov)[i]   += (dA2+dA3+dA4)*S3/state7[6];  (*cov)[i+3] = (dA0+dA3+dA3+dA5+dA6)*P3/state7[6]; // dR := dR + S3*[(dA2, dB2, dC2) +   (dA3, dB3, dC3) + (dA4, dB4, dC4)]
        (*cov)[i+1] += (dB2+dB3+dB4)*S3/state7[6];  (*cov)[i+4] = (dB0+dB3+dB3+dB5+dB6)*P3/state7[6]; // dA :=     1/3*[(dA0, dB0, dC0) + 2*(dA3, dB3, dC3) + (dA5, dB5, dC5) + (dA6, dB6, dC6)]
        (*cov)[i+2] += (dC2+dC3+dC4)*S3/state7[6];  (*cov)[i+5] = (dC0+dC3+dC3+dC5+dC6)*P3/state7[6];
      }
      else {
        (*cov)[i]   += (dA2+dA3+dA4)*S3;  (*cov)[i+3] = (dA0+dA3+dA3+dA5+dA6)*P3; // dR := dR + S3*[(dA2, dB2, dC2) +   (dA3, dB3, dC3) + (dA4, dB4, dC4)]
        (*cov)[i+1] += (dB2+dB3+dB4)*S3;  (*cov)[i+4] = (dB0+dB3+dB3+dB5+dB6)*P3; // dA :=     1/3*[(dA0, dB0, dC0) + 2*(dA3, dB3, dC3) + (dA5, dB5, dC5) + (dA6, dB6, dC6)]
        (*cov)[i+2] += (dC2+dC3+dC4)*S3;  (*cov)[i+5] = (dC0+dC3+dC3+dC5+dC6)*P3;
      }
    }
  }

  //
  // Track parameters in last point
  //
  R[0] += (A2+A3+A4)*S3;   A[0] += (SA[0]=(A0+A3+A3+A5+A6)*P3-A[0]);  // R  = R0 + S3*[(A2, B2, C2) +   (A3, B3, C3) + (A4, B4, C4)]
  R[1] += (B2+B3+B4)*S3;   A[1] += (SA[1]=(B0+B3+B3+B5+B6)*P3-A[1]);  // A  =     1/3*[(A0, B0, C0) + 2*(A3, B3, C3) + (A5, B5, C5) + (A6, B6, C6)]
  R[2] += (C2+C3+C4)*S3;   A[2] += (SA[2]=(C0+C3+C3+C5+C6)*P3-A[2]);  // SA = A_new - A_old

  // normalize A
  double CBA = 1./sqrt(A[0]*A[0]+A[1]*A[1]+A[2]*A[2]); // 1/|A|
  A[0] *= CBA; A[1] *= CBA; A[2] *= CBA;
}


double RKTrackRep::estimateStep(std::vector<GFPointPath>& points,
                                const TVector3& pos,
                                const TVector3& dir,
                                const M1x4& SU,
                                const GFDetPlane& plane,
                                const double& momentum,
                                double& relMomLoss,
                                double& deltaAngle,
                                bool& momLossExceeded,
                                bool& atPlane,
                                double maxStep) const {

  static const double Smax      = 10.;          // max. step allowed [cm]
  static const double dAngleMax = 0.05;         // max. deviation of angle between direction before and after the step [rad]
  double Step;
  bool improveEstimation (true);

  momLossExceeded = false;
  atPlane = false;

#ifdef DEBUG
  std::cout << " RKTrackRep::estimateStep \n";
  std::cout << "  points.size(): " << points.size() << std::endl;
  std::cout << "  position: "; pos.Print();
  std::cout << "  direction: "; dir.Print();
#endif


  // calculate distance to surface
  double Dist = SU[3] - (pos.X()*SU[0] + pos.Y()*SU[1] + pos.Z()*SU[2]);  // Distance between start coordinates and surface
  double An = dir.X()*SU[0] + dir.Y()*SU[1] + dir.Z()*SU[2];              // An = dir * N;  component of dir normal to surface

  if (fabs(An) > 1.E-10) Step = Dist/An;
  else {
    Step = Dist*1.E10;
    if (An<0) Step *= -1.;
  }

  // // MGW - never propagate the particle backwards on first step!
  // if(points.size()==0 && Step<0.) Step = -Step;

  // see if dir points towards surface (1) or not (-1)
  double StepSign;

#ifdef DEBUG
  StepSign = (Step<0) ? -1 : 1;
  std::cout << "  Distance to plane: " << Dist << "\n";
  std::cout << "  guess for Step: " << Step << "\n";
  if (StepSign>0) std::cout << "  Direction is  pointing towards surface.\n";
  else  std::cout << "  Direction is pointing away from surface.\n";
#endif

  // calculate way SmaxAngle after which momentum angle has changed AngleMax
  TVector3 MagField(getFieldValKiloGauss(pos));
  double Hmag(MagField.Mag()), SmaxAngle(maxStep), radius(0);
  if (Hmag > 1E-5){
    double cosAngle = (dir.Dot(MagField))/Hmag;
    radius = momentum/(0.299792458E-3*Hmag) *
      sqrt( pow(dir.X() - cosAngle/Hmag * MagField.X(), 2) +
            pow(dir.Y() - cosAngle/Hmag * MagField.Y(), 2) +
            pow(dir.Z() - cosAngle/Hmag * MagField.Z(), 2)); // [cm]
    double sinAngle = sqrt(1 - cosAngle*cosAngle);
    if (sinAngle > 1E-10) SmaxAngle = fabs(dAngleMax * radius / sinAngle); // [cm]
  }


  //
  // Select direction
  //
  // auto select
  if (fDirection == 0 || !plane.isFinite()){
#ifdef DEBUG
    std::cout << "  auto select direction";
    if (!plane.isFinite()) std::cout << ", plane is not finite";
    std::cout << ".\n";
#endif
  }
  // see if straight line approximation is ok
  else if ( fabs(Step) < 0.2*SmaxAngle ){
#ifdef DEBUG
    std::cout << "  straight line approximation is fine. Delta angle until surface is reached is approx " << Step/SmaxAngle * dAngleMax * 180 / TMath::Pi()  << " deg \n";
#endif

    // if direction is pointing to active part of surface
    if( plane.inActive(pos, dir) ) {
#ifdef DEBUG
      std::cout << "  direction is pointing to active part of surface. \n";
#endif
    }
    // if we are near the plane, but not pointing to the active area, make a big step!
    else {
      Step = fDirection*SmaxAngle;
      improveEstimation = false;
#ifdef DEBUG
      std::cout << "  we are near the plane, but not pointing to the active area. make a big step! \n";
#endif
    }
  }
  // fDirection decides!
  else {
    if (Step * fDirection < 0){
      Step = fDirection*SmaxAngle;
      improveEstimation = false;
#ifdef DEBUG
      std::cout << "  invert Step according to fDirection and set Step to fDirection*SmaxAngle. \n";
#endif
    }
  }

#ifdef DEBUG
  std::cout << "  guess for Step (signed): " << Step << "\n";
#endif

  // re-check sign of Step
  if (Step>=0) StepSign = 1;
  else StepSign = -1;

  //
  // Limit stepsize
  //
  // reduce maximum stepsize Step to Smax and maxStep
  if (fabs(Step) > Smax) {
    Step = StepSign*Smax;
    improveEstimation = false;
  }
  if (fabs(Step) > maxStep) {
    Step = StepSign*maxStep;
    improveEstimation = false;
  }

  // also limit stepsize according to the change of the momentum direction!
  if (fabs(Step) > SmaxAngle) {
    Step = StepSign*SmaxAngle;
    improveEstimation = false;
  }

#ifdef DEBUG
  std::cout << "  limit from maxangle: " << SmaxAngle << ", radius: " << radius << "\n";
#endif


//   // call stepper and reduce stepsize if step not too small
//   if (!fNoMaterial){

//     if(fabs(Step) > MINSTEP){ // only call stepper if step estimation big enough
//       double StepMat = GFMaterialEffects::getInstance()->stepper(fabs(Step),
//                                                                  SmaxAngle,
//                                                                  pos,
//                                                                  StepSign*dir,
//                                                                  momentum,
//                                                                  relMomLoss,
//                                                                  fPdg);
//       if (fabs(Step) > StepMat) {
//         Step = StepSign*StepMat;
//         momLossExceeded = true;
//       }

// #ifdef DEBUG
//       std::cout << "  limit from stepper: " << Step << "\n";
// #endif
//     }
//   }
  

  if (!momLossExceeded && improveEstimation){
    atPlane = true;
    
    // improve step estimation to surface according to curvature
    if (Hmag > 1E-5 && fabs(Step) > 0.1*SmaxAngle){

      M1x7 state7;
      state7[0] = pos.X();  state7[1] = pos.Y(); state7[2] = pos.Z();
      state7[3] = dir.X();  state7[4] = dir.Y(); state7[5] = dir.Z();
      state7[6] = fCharge/momentum;
      M1x3 SA;

      RKPropagate(state7, NULL, SA, Step, false);

      // calculate distance to surface
      Dist = SU[3] - (state7[0] * SU[0] +
                      state7[1] * SU[1] +
                      state7[2] * SU[2]); // Distance between position and surface

      An = state7[3] * SU[0] +
        state7[4] * SU[1] +
        state7[5] * SU[2];    // An = dir * N;  component of dir normal to surface

      Step += Dist/An;

#ifdef DEBUG
      std::cout << "  Improved step estimation taking curvature into account: " << Step << "\n";
#endif
    }

  }

  deltaAngle += Step/SmaxAngle * dAngleMax;
  points.push_back(GFPointPath(pos, Step));

#ifdef DEBUG
  std::cout << "  --> Step to be used: " << Step << "\n";
#endif

  return Step;
}


void RKTrackRep::setPropDir(int dir){
  // make sure fDirection is -1, 0 or 1
  if (dir>0) fDirection = 1;
  else if (dir<0) fDirection = -1;
  else fDirection = 0;
}


#include <iostream>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

void RKTrackRep::J_pMTxcov5xJ_pM(const M5x7& J_pM, const M5x5& cov5, M7x7& out7){

  // it is assumed that J_pM is only non-zero here:
  // [3*7+0]
  // [3*7+1]
  // [3*7+2]

  // [4*7+2]
  // [4*7+2]
  // [4*7+2]

  // [6] = 1

  // [1*7+3]
  // [1*7+4]
  // [1*7+5]

  // [2*7+3]
  // [2*7+4]
  // [2*7+5]

  double JTC0  = J_pM[21] * cov5[18] + J_pM[28] * cov5[23];
  double JTC1  = J_pM[21] * cov5[23] + J_pM[28] * cov5[24];
  double JTC2  = J_pM[21] * cov5[16] + J_pM[28] * cov5[21];
  double JTC3  = J_pM[21] * cov5[17] + J_pM[28] * cov5[22];
  double JTC4  = J_pM[22] * cov5[18] + J_pM[29] * cov5[23];
  double JTC5  = J_pM[22] * cov5[23] + J_pM[29] * cov5[24];
  double JTC6  = J_pM[22] * cov5[16] + J_pM[29] * cov5[21];
  double JTC7  = J_pM[22] * cov5[17] + J_pM[29] * cov5[22];
  double JTC8  = J_pM[23] * cov5[18] + J_pM[30] * cov5[23];
  double JTC9  = J_pM[23] * cov5[23] + J_pM[30] * cov5[24];
  double JTC10 = J_pM[23] * cov5[16] + J_pM[30] * cov5[21];
  double JTC11 = J_pM[23] * cov5[17] + J_pM[30] * cov5[22];
  double JTC12 = J_pM[10] * cov5[6]  + J_pM[17] * cov5[11];
  double JTC13 = J_pM[10] * cov5[11] + J_pM[17] * cov5[12];
  double JTC14 = J_pM[11] * cov5[6]  + J_pM[18] * cov5[11];
  double JTC15 = J_pM[11] * cov5[11] + J_pM[18] * cov5[12];

  // loops are vectorizable by the compiler!
  for (int i=0; i<3; ++i) out7[i]  = JTC0 * J_pM[21+i] + JTC1 * J_pM[28+i];
  for (int i=0; i<3; ++i) out7[3+i]  = JTC2 * J_pM[10+i] + JTC3 * J_pM[17+i];
  out7[6]  =  J_pM[21] * cov5[15] + J_pM[28] * cov5[20];

  for (int i=0; i<2; ++i) out7[8+i]  = JTC4 * J_pM[22+i] + JTC5 * J_pM[29+i];
  for (int i=0; i<3; ++i) out7[10+i] = JTC6 * J_pM[10+i] + JTC7 * J_pM[17+i];
  out7[13] = J_pM[22] * cov5[15] + J_pM[29] * cov5[20];

  out7[16] = JTC8 * J_pM[23] + JTC9 * J_pM[30];
  for (int i=0; i<3; ++i) out7[17+i] = JTC10 * J_pM[10+i] + JTC11 * J_pM[17+i];
  out7[20] = J_pM[23] * cov5[15] + J_pM[30] * cov5[20];

  for (int i=0; i<3; ++i) out7[24+i] = JTC12 * J_pM[10+i] + JTC13 * J_pM[17+i];
  out7[27] = J_pM[10] * cov5[5] + J_pM[17] * cov5[10];

  for (int i=0; i<2; ++i) out7[32+i] = JTC14 * J_pM[11+i] + JTC15 * J_pM[18+i];
  out7[34] = J_pM[11] * cov5[5] + J_pM[18] * cov5[10];

  out7[40] = (J_pM[12] * cov5[6] + J_pM[19] * cov5[11]) * J_pM[12] + (J_pM[12] * cov5[11] + J_pM[19] * cov5[12]) * J_pM[19];
  out7[41] = J_pM[12] * cov5[5] + J_pM[19] * cov5[10];

  out7[48] = cov5[0];

  // symmetric part
  out7[7]  = out7[1];
  out7[14] = out7[2];  out7[15] = out7[9];
  out7[21] = out7[3];  out7[22] = out7[10];  out7[23] = out7[17];
  out7[28] = out7[4];  out7[29] = out7[11];  out7[30] = out7[18];  out7[31] = out7[25];
  out7[35] = out7[5];  out7[36] = out7[12];  out7[37] = out7[19];  out7[38] = out7[26];  out7[39] = out7[33];
  out7[42] = out7[6];  out7[43] = out7[13];  out7[44] = out7[20];  out7[45] = out7[27];  out7[46] = out7[34];  out7[47] = out7[41];

}


void RKTrackRep::J_pMTxcov5xJ_pM(const M5x6& J_pM, const M5x5& cov5, M6x6& out6){

  // it is assumed that J_pM is only non-zero here:
  // [3]
  // [4]
  // [5]

  // [1*7+3]
  // [1*7+4]
  // [1*7+5]

  // [2*7+3]
  // [2*7+4]
  // [2*7+5]

  // [3*7+0]
  // [3*7+1]
  // [3*7+2]

  // [4*7+0]
  // [4*7+1]
  // [4*7+2]

  double JTC0  = J_pM[18] * cov5[15+3] + J_pM[24] * cov5[20+3];
  double JTC1  = J_pM[18] * cov5[20+3] + J_pM[24] * cov5[20+4];
  double JTC2  = J_pM[18] * cov5[15] + J_pM[24] * cov5[20];
  double JTC3  = J_pM[18] * cov5[15+1] + J_pM[24] * cov5[20+1];
  double JTC4  = J_pM[18] * cov5[15+2] + J_pM[24] * cov5[20+2];
  double JTC5  = J_pM[18+1] * cov5[15+3] + J_pM[24+1] * cov5[20+3];
  double JTC6  = J_pM[18+1] * cov5[20+3] + J_pM[24+1] * cov5[20+4];
  double JTC7  = J_pM[18+1] * cov5[15] + J_pM[24+1] * cov5[20];
  double JTC8  = J_pM[18+1] * cov5[15+1] + J_pM[24+1] * cov5[20+1];
  double JTC9  = J_pM[18+1] * cov5[15+2] + J_pM[24+1] * cov5[20+2];
  double JTC10 = J_pM[18+2] * cov5[15] + J_pM[24+2] * cov5[20];
  double JTC11 = J_pM[18+2] * cov5[15+1] + J_pM[24+2] * cov5[20+1];
  double JTC12 = J_pM[18+2] * cov5[15+2] + J_pM[24+2] * cov5[20+2];
  double JTC13 = J_pM[3] * cov5[0*5] + J_pM[6+3] * cov5[5] + J_pM[12+3] * cov5[10];
  double JTC14 = J_pM[3] * cov5[5] + J_pM[6+3] * cov5[5+1] + J_pM[12+3] * cov5[10+1];
  double JTC15 = J_pM[3] * cov5[10] + J_pM[6+3] * cov5[10+1] + J_pM[12+3] * cov5[10+2];
  double JTC16 = J_pM[4] * cov5[0*5] + J_pM[6+4] * cov5[5] + J_pM[12+4] * cov5[10];
  double JTC17 = J_pM[4] * cov5[5] + J_pM[6+4] * cov5[5+1] + J_pM[12+4] * cov5[10+1];
  double JTC18 = J_pM[4] * cov5[10] + J_pM[6+4] * cov5[10+1] + J_pM[12+4] * cov5[10+2];

  // loops are vectorizable by the compiler!
  for (int i=0; i<3; ++i) out6[i] = JTC0 * J_pM[18+i] + JTC1 * J_pM[24+i];
  for (int i=0; i<3; ++i) out6[3+i] = JTC2 * J_pM[3+i] + JTC3 * J_pM[9+i] + JTC4 * J_pM[15+i];

  for (int i=0; i<2; ++i) out6[7+i] = JTC5 * J_pM[19+i] + JTC6 * J_pM[25+i];
  for (int i=0; i<3; ++i) out6[9+i] = JTC7 * J_pM[3+i] + JTC8 * J_pM[9+i] + JTC9 * J_pM[15+i];

  out6[12+2] = (J_pM[18+2] * cov5[15+3] + J_pM[24+2] * cov5[20+3]) * J_pM[18+2] + (J_pM[18+2] * cov5[20+3] + J_pM[24+2] * cov5[20+4]) * J_pM[24+2];
  for (int i=0; i<3; ++i) out6[15+i] = JTC10 * J_pM[3+i] + JTC11 * J_pM[9+i] + JTC12 * J_pM[15+i];

  for (int i=0; i<3; ++i) out6[21+i] = JTC13 * J_pM[3+i] + JTC14 * J_pM[9+i] + JTC15 * J_pM[15+i];

  for (int i=0; i<3; ++i) out6[28+i] = JTC16 * J_pM[4+i] + JTC17 * J_pM[10+i] + JTC18 * J_pM[16+i];

  out6[30+5] = (J_pM[5] * cov5[0*5] + J_pM[6+5] * cov5[5] + J_pM[12+5] * cov5[10]) * J_pM[5] + (J_pM[5] * cov5[5] + J_pM[6+5] * cov5[5+1] + J_pM[12+5] * cov5[10+1]) * J_pM[6+5] + (J_pM[5] * cov5[10] + J_pM[6+5] * cov5[10+1] + J_pM[12+5] * cov5[10+2]) * J_pM[12+5];

  // symmetric part
  out6[6] = out6[1];
  out6[12] = out6[2];  out6[12+1] = out6[6+2];
  out6[18] = out6[3];  out6[18+1] = out6[6+3];  out6[18+2] = out6[12+3];
  out6[24] = out6[4];  out6[24+1] = out6[6+4];  out6[24+2] = out6[12+4];  out6[24+3] = out6[18+4];
  out6[30] = out6[5];  out6[30+1] = out6[6+5];  out6[30+2] = out6[12+5];  out6[30+3] = out6[18+5];  out6[30+4] = out6[24+5];

}


void RKTrackRep::J_MpTxcov7xJ_Mp(const M7x5& J_Mp, const M7x7& cov7, M5x5& out5){

  // it is assumed that J_Mp is only non-zero here:
  // [3*7+1]
  // [4*7+1]
  // [5*7+1]

  // [3*7+2]
  // [4*7+2]
  // [5*7+2]

  // [6*7+0] = 1

  // [3]
  // [1*7+3]
  // [2*7+3]

  // [4]
  // [1*7+4]
  // [2*7+4]

  double JTC0  = (J_Mp[16] * cov7[24] + J_Mp[21] * cov7[31] + J_Mp[26] * cov7[38]);
  double JTC1  = (J_Mp[16] * cov7[31] + J_Mp[21] * cov7[32] + J_Mp[26] * cov7[39]);
  double JTC2  = (J_Mp[16] * cov7[38] + J_Mp[21] * cov7[39] + J_Mp[26] * cov7[40]);
  double JTC3  = (J_Mp[16] * cov7[21] + J_Mp[21] * cov7[28] + J_Mp[26] * cov7[35]);
  double JTC4  = (J_Mp[16] * cov7[22] + J_Mp[21] * cov7[29] + J_Mp[26] * cov7[36]);
  double JTC5  = (J_Mp[16] * cov7[23] + J_Mp[21] * cov7[30] + J_Mp[26] * cov7[37]);
  double JTC6  = (J_Mp[17] * cov7[21] + J_Mp[22] * cov7[28] + J_Mp[27] * cov7[35]);
  double JTC7  = (J_Mp[17] * cov7[22] + J_Mp[22] * cov7[29] + J_Mp[27] * cov7[36]);
  double JTC8  = (J_Mp[17] * cov7[23] + J_Mp[22] * cov7[30] + J_Mp[27] * cov7[37]);
  double JTC9  = (J_Mp[3] * cov7[0] + J_Mp[8] * cov7[7] + J_Mp[13] * cov7[14]);
  double JTC10 = (J_Mp[3] * cov7[7] + J_Mp[8] * cov7[8] + J_Mp[13] * cov7[15]);
  double JTC11 = (J_Mp[3] * cov7[14] + J_Mp[8] * cov7[15] + J_Mp[13] * cov7[16]);

  out5[0] = cov7[48];
  out5[1] = J_Mp[16] * cov7[45] + J_Mp[21] * cov7[46] + J_Mp[26] * cov7[47];
  out5[2] = J_Mp[17] * cov7[45] + J_Mp[22] * cov7[46] + J_Mp[27] * cov7[47];
  out5[3] = J_Mp[3] * cov7[42] + J_Mp[8] * cov7[43] + J_Mp[13] * cov7[44];
  out5[4] = J_Mp[4] * cov7[42] + J_Mp[9] * cov7[43] + J_Mp[14] * cov7[44];

  // loops are vectorizable by the compiler!
  for (int i=0; i<2; ++i) out5[6+i] = JTC0 * J_Mp[16+i] + JTC1 * J_Mp[21+i] + JTC2 * J_Mp[26+i];
  for (int i=0; i<2; ++i) out5[8+i] = JTC3 * J_Mp[3+i] + JTC4 * J_Mp[8+i] + JTC5 * J_Mp[13+i];

  out5[12] = (J_Mp[17] * cov7[24] + J_Mp[22] * cov7[31] + J_Mp[27] * cov7[38]) * J_Mp[17] + (J_Mp[17] * cov7[31] + J_Mp[22] * cov7[32] + J_Mp[27] * cov7[39]) * J_Mp[22] + (J_Mp[17] * cov7[38] + J_Mp[22] * cov7[39] + J_Mp[27] * cov7[40]) * J_Mp[27];
  for (int i=0; i<2; ++i) out5[13+i] = JTC6 * J_Mp[3+i] + JTC7 * J_Mp[8+i] + JTC8 * J_Mp[13+i];

  for (int i=0; i<2; ++i) out5[18+i] = JTC9 * J_Mp[3+i] + JTC10 * J_Mp[8+i] + JTC11 * J_Mp[13+i];

  out5[24] = (J_Mp[4] * cov7[0] + J_Mp[9] * cov7[7] + J_Mp[14] * cov7[14]) * J_Mp[4] + (J_Mp[4] * cov7[7] + J_Mp[9] * cov7[8] + J_Mp[14] * cov7[15]) * J_Mp[9] + (J_Mp[4] * cov7[14] + J_Mp[9] * cov7[15] + J_Mp[14] * cov7[16]) * J_Mp[14];

  // symmetric part
  out5[5] = out5[1];
  out5[10] = out5[2];  out5[11] = out5[7];
  out5[15] = out5[3];  out5[16] = out5[8];  out5[17] = out5[13];
  out5[20] = out5[4];  out5[21] = out5[9];  out5[22] = out5[14];  out5[23] = out5[19];

}


void RKTrackRep::J_MpTxcov6xJ_Mp(const M6x5& J_Mp, const M6x6& cov6, M5x5& out5){

  // it is assumed that J_Mp is only non-zero here:
  // [3]
  // [1*7+3]
  // [2*7+3]

  // [4]
  // [1*7+4]
  // [2*7+4]

  // [3*7+0]
  // [4*7+0]
  // [5*7+0]

  // [3*7+1]
  // [4*7+1]
  // [5*7+1]

  // [3*7+2]
  // [4*7+2]
  // [5*7+2]

  double JTC0  = (J_Mp[15] * cov6[18+3] + J_Mp[20] * cov6[24+3] + J_Mp[25] * cov6[30+3]);
  double JTC1  = (J_Mp[15] * cov6[24+3] + J_Mp[20] * cov6[24+4] + J_Mp[25] * cov6[30+4]);
  double JTC2  = (J_Mp[15] * cov6[30+3] + J_Mp[20] * cov6[30+4] + J_Mp[25] * cov6[30+5]);
  double JTC3  = (J_Mp[15] * cov6[18] + J_Mp[20] * cov6[24] + J_Mp[25] * cov6[30]);
  double JTC4  = (J_Mp[15] * cov6[18+1] + J_Mp[20] * cov6[24+1] + J_Mp[25] * cov6[30+1]);
  double JTC5  = (J_Mp[15] * cov6[18+2] + J_Mp[20] * cov6[24+2] + J_Mp[25] * cov6[30+2]);
  double JTC6  = (J_Mp[15+1] * cov6[18+3] + J_Mp[20+1] * cov6[24+3] + J_Mp[25+1] * cov6[30+3]);
  double JTC7  = (J_Mp[15+1] * cov6[24+3] + J_Mp[20+1] * cov6[24+4] + J_Mp[25+1] * cov6[30+4]);
  double JTC8  = (J_Mp[15+1] * cov6[30+3] + J_Mp[20+1] * cov6[30+4] + J_Mp[25+1] * cov6[30+5]);
  double JTC9  = (J_Mp[15+1] * cov6[18] + J_Mp[20+1] * cov6[24] + J_Mp[25+1] * cov6[30]);
  double JTC10 = (J_Mp[15+1] * cov6[18+1] + J_Mp[20+1] * cov6[24+1] + J_Mp[25+1] * cov6[30+1]);
  double JTC11 = (J_Mp[15+1] * cov6[18+2] + J_Mp[20+1] * cov6[24+2] + J_Mp[25+1] * cov6[30+2]);
  double JTC12 = (J_Mp[15+2] * cov6[18] + J_Mp[20+2] * cov6[24] + J_Mp[25+2] * cov6[30]);
  double JTC13 = (J_Mp[15+2] * cov6[18+1] + J_Mp[20+2] * cov6[24+1] + J_Mp[25+2] * cov6[30+1]);
  double JTC14 = (J_Mp[15+2] * cov6[18+2] + J_Mp[20+2] * cov6[24+2] + J_Mp[25+2] * cov6[30+2]);
  double JTC15 = (J_Mp[3] * cov6[0] + J_Mp[5+3] * cov6[6] + J_Mp[10+3] * cov6[12]);
  double JTC16 = (J_Mp[3] * cov6[6] + J_Mp[5+3] * cov6[6+1] + J_Mp[10+3] * cov6[12+1]);
  double JTC17 = (J_Mp[3] * cov6[12] + J_Mp[5+3] * cov6[12+1] + J_Mp[10+3] * cov6[12+2]);

  // loops are vectorizable by the compiler!
  for (int i=0; i<3; ++i) out5[i] = JTC0 * J_Mp[15+i] + JTC1 * J_Mp[20+i] + JTC2 * J_Mp[25+i];
  for (int i=0; i<2; ++i) out5[3+i] = JTC3 * J_Mp[3+i] + JTC4 * J_Mp[8+i] + JTC5 * J_Mp[13+i];

  for (int i=0; i<2; ++i) out5[6+i] = JTC6 * J_Mp[16+i] + JTC7 * J_Mp[21+i] + JTC8 * J_Mp[26+i];
  for (int i=0; i<2; ++i) out5[8+i] = JTC9 * J_Mp[3+i] + JTC10 * J_Mp[8+i] + JTC11 * J_Mp[13+i];

  out5[10+2] = (J_Mp[15+2] * cov6[18+3] + J_Mp[20+2] * cov6[24+3] + J_Mp[25+2] * cov6[30+3]) * J_Mp[15+2] + (J_Mp[15+2] * cov6[24+3] + J_Mp[20+2] * cov6[24+4] + J_Mp[25+2] * cov6[30+4]) * J_Mp[20+2] + (J_Mp[15+2] * cov6[30+3] + J_Mp[20+2] * cov6[30+4] + J_Mp[25+2] * cov6[30+5]) * J_Mp[25+2];
  for (int i=0; i<2; ++i) out5[13+i] = JTC12 * J_Mp[3+i] + JTC13 * J_Mp[8+i] + JTC14 * J_Mp[13+i];

  for (int i=0; i<2; ++i) out5[18+i] = JTC15 * J_Mp[3+i] + JTC16 * J_Mp[8+i] + JTC17 * J_Mp[13+i];

  out5[20+4] = (J_Mp[4] * cov6[0] + J_Mp[5+4] * cov6[6] + J_Mp[10+4] * cov6[12]) * J_Mp[4] + (J_Mp[4] * cov6[6] + J_Mp[5+4] * cov6[6+1] + J_Mp[10+4] * cov6[12+1]) * J_Mp[5+4] + (J_Mp[4] * cov6[12] + J_Mp[5+4] * cov6[12+1] + J_Mp[10+4] * cov6[12+2]) * J_Mp[10+4];

  // symmetric part
  out5[5] = out5[1];
  out5[10] = out5[2];  out5[10+1] = out5[5+2];
  out5[15] = out5[3];  out5[15+1] = out5[5+3];  out5[15+2] = out5[10+3];
  out5[20] = out5[4];  out5[20+1] = out5[5+4];  out5[20+2] = out5[10+4];  out5[20+3] = out5[15+4];

}


void RKTrackRep::J_MMTxcov7xJ_MM(const M7x7& J_MM, M7x7& cov7){

  // it is assumed that the last column of J_MM is [0,0,0,0,0,0,1]

  double JTC0  = J_MM[0] * cov7[0] + J_MM[7] * cov7[7] + J_MM[14] * cov7[14] + J_MM[21] * cov7[21] + J_MM[28] * cov7[28] + J_MM[35] * cov7[35] + J_MM[42] * cov7[42];
  double JTC1  = J_MM[0] * cov7[7] + J_MM[7] * cov7[7+1] + J_MM[14] * cov7[14+1] + J_MM[21] * cov7[21+1] + J_MM[28] * cov7[28+1] + J_MM[35] * cov7[35+1] + J_MM[42] * cov7[42+1];
  double JTC2  = J_MM[0] * cov7[14] + J_MM[7] * cov7[14+1] + J_MM[14] * cov7[14+2] + J_MM[21] * cov7[21+2] + J_MM[28] * cov7[28+2] + J_MM[35] * cov7[35+2] + J_MM[42] * cov7[42+2];
  double JTC3  = J_MM[0] * cov7[21] + J_MM[7] * cov7[21+1] + J_MM[14] * cov7[21+2] + J_MM[21] * cov7[21+3] + J_MM[28] * cov7[28+3] + J_MM[35] * cov7[35+3] + J_MM[42] * cov7[42+3];
  double JTC4  = J_MM[0] * cov7[28] + J_MM[7] * cov7[28+1] + J_MM[14] * cov7[28+2] + J_MM[21] * cov7[28+3] + J_MM[28] * cov7[28+4] + J_MM[35] * cov7[35+4] + J_MM[42] * cov7[42+4];
  double JTC5  = J_MM[0] * cov7[35] + J_MM[7] * cov7[35+1] + J_MM[14] * cov7[35+2] + J_MM[21] * cov7[35+3] + J_MM[28] * cov7[35+4] + J_MM[35] * cov7[35+5] + J_MM[42] * cov7[42+5];
  double JTC6  = J_MM[0] * cov7[42] + J_MM[7] * cov7[42+1] + J_MM[14] * cov7[42+2] + J_MM[21] * cov7[42+3] + J_MM[28] * cov7[42+4] + J_MM[35] * cov7[42+5] + J_MM[42] * cov7[42+6];

  double JTC7  = J_MM[1] * cov7[0] + J_MM[7+1] * cov7[7] + J_MM[14+1] * cov7[14] + J_MM[21+1] * cov7[21] + J_MM[28+1] * cov7[28] + J_MM[35+1] * cov7[35] + J_MM[42+1] * cov7[42];
  double JTC8  = J_MM[1] * cov7[7] + J_MM[7+1] * cov7[7+1] + J_MM[14+1] * cov7[14+1] + J_MM[21+1] * cov7[21+1] + J_MM[28+1] * cov7[28+1] + J_MM[35+1] * cov7[35+1] + J_MM[42+1] * cov7[42+1];
  double JTC9  = J_MM[1] * cov7[14] + J_MM[7+1] * cov7[14+1] + J_MM[14+1] * cov7[14+2] + J_MM[21+1] * cov7[21+2] + J_MM[28+1] * cov7[28+2] + J_MM[35+1] * cov7[35+2] + J_MM[42+1] * cov7[42+2];
  double JTC10 = J_MM[1] * cov7[21] + J_MM[7+1] * cov7[21+1] + J_MM[14+1] * cov7[21+2] + J_MM[21+1] * cov7[21+3] + J_MM[28+1] * cov7[28+3] + J_MM[35+1] * cov7[35+3] + J_MM[42+1] * cov7[42+3];
  double JTC11 = J_MM[1] * cov7[28] + J_MM[7+1] * cov7[28+1] + J_MM[14+1] * cov7[28+2] + J_MM[21+1] * cov7[28+3] + J_MM[28+1] * cov7[28+4] + J_MM[35+1] * cov7[35+4] + J_MM[42+1] * cov7[42+4];
  double JTC12 = J_MM[1] * cov7[35] + J_MM[7+1] * cov7[35+1] + J_MM[14+1] * cov7[35+2] + J_MM[21+1] * cov7[35+3] + J_MM[28+1] * cov7[35+4] + J_MM[35+1] * cov7[35+5] + J_MM[42+1] * cov7[42+5];
  double JTC13 = J_MM[1] * cov7[42] + J_MM[7+1] * cov7[42+1] + J_MM[14+1] * cov7[42+2] + J_MM[21+1] * cov7[42+3] + J_MM[28+1] * cov7[42+4] + J_MM[35+1] * cov7[42+5] + J_MM[42+1] * cov7[42+6];

  double JTC14 = J_MM[2] * cov7[0] + J_MM[7+2] * cov7[7] + J_MM[14+2] * cov7[14] + J_MM[21+2] * cov7[21] + J_MM[28+2] * cov7[28] + J_MM[35+2] * cov7[35] + J_MM[42+2] * cov7[42];
  double JTC15 = J_MM[2] * cov7[7] + J_MM[7+2] * cov7[7+1] + J_MM[14+2] * cov7[14+1] + J_MM[21+2] * cov7[21+1] + J_MM[28+2] * cov7[28+1] + J_MM[35+2] * cov7[35+1] + J_MM[42+2] * cov7[42+1];
  double JTC16 = J_MM[2] * cov7[14] + J_MM[7+2] * cov7[14+1] + J_MM[14+2] * cov7[14+2] + J_MM[21+2] * cov7[21+2] + J_MM[28+2] * cov7[28+2] + J_MM[35+2] * cov7[35+2] + J_MM[42+2] * cov7[42+2];
  double JTC17 = J_MM[2] * cov7[21] + J_MM[7+2] * cov7[21+1] + J_MM[14+2] * cov7[21+2] + J_MM[21+2] * cov7[21+3] + J_MM[28+2] * cov7[28+3] + J_MM[35+2] * cov7[35+3] + J_MM[42+2] * cov7[42+3];
  double JTC18 = J_MM[2] * cov7[28] + J_MM[7+2] * cov7[28+1] + J_MM[14+2] * cov7[28+2] + J_MM[21+2] * cov7[28+3] + J_MM[28+2] * cov7[28+4] + J_MM[35+2] * cov7[35+4] + J_MM[42+2] * cov7[42+4];
  double JTC19 = J_MM[2] * cov7[35] + J_MM[7+2] * cov7[35+1] + J_MM[14+2] * cov7[35+2] + J_MM[21+2] * cov7[35+3] + J_MM[28+2] * cov7[35+4] + J_MM[35+2] * cov7[35+5] + J_MM[42+2] * cov7[42+5];
  double JTC20 = J_MM[2] * cov7[42] + J_MM[7+2] * cov7[42+1] + J_MM[14+2] * cov7[42+2] + J_MM[21+2] * cov7[42+3] + J_MM[28+2] * cov7[42+4] + J_MM[35+2] * cov7[42+5] + J_MM[42+2] * cov7[42+6];

  double JTC21 = J_MM[3] * cov7[0] + J_MM[7+3] * cov7[7] + J_MM[14+3] * cov7[14] + J_MM[21+3] * cov7[21] + J_MM[28+3] * cov7[28] + J_MM[35+3] * cov7[35] + J_MM[42+3] * cov7[42];
  double JTC22 = J_MM[3] * cov7[7] + J_MM[7+3] * cov7[7+1] + J_MM[14+3] * cov7[14+1] + J_MM[21+3] * cov7[21+1] + J_MM[28+3] * cov7[28+1] + J_MM[35+3] * cov7[35+1] + J_MM[42+3] * cov7[42+1];
  double JTC23 = J_MM[3] * cov7[14] + J_MM[7+3] * cov7[14+1] + J_MM[14+3] * cov7[14+2] + J_MM[21+3] * cov7[21+2] + J_MM[28+3] * cov7[28+2] + J_MM[35+3] * cov7[35+2] + J_MM[42+3] * cov7[42+2];
  double JTC24 = J_MM[3] * cov7[21] + J_MM[7+3] * cov7[21+1] + J_MM[14+3] * cov7[21+2] + J_MM[21+3] * cov7[21+3] + J_MM[28+3] * cov7[28+3] + J_MM[35+3] * cov7[35+3] + J_MM[42+3] * cov7[42+3];
  double JTC25 = J_MM[3] * cov7[28] + J_MM[7+3] * cov7[28+1] + J_MM[14+3] * cov7[28+2] + J_MM[21+3] * cov7[28+3] + J_MM[28+3] * cov7[28+4] + J_MM[35+3] * cov7[35+4] + J_MM[42+3] * cov7[42+4];
  double JTC26 = J_MM[3] * cov7[35] + J_MM[7+3] * cov7[35+1] + J_MM[14+3] * cov7[35+2] + J_MM[21+3] * cov7[35+3] + J_MM[28+3] * cov7[35+4] + J_MM[35+3] * cov7[35+5] + J_MM[42+3] * cov7[42+5];
  double JTC27 = J_MM[3] * cov7[42] + J_MM[7+3] * cov7[42+1] + J_MM[14+3] * cov7[42+2] + J_MM[21+3] * cov7[42+3] + J_MM[28+3] * cov7[42+4] + J_MM[35+3] * cov7[42+5] + J_MM[42+3] * cov7[42+6];

  double JTC28 = J_MM[4] * cov7[0] + J_MM[7+4] * cov7[7] + J_MM[14+4] * cov7[14] + J_MM[21+4] * cov7[21] + J_MM[28+4] * cov7[28] + J_MM[35+4] * cov7[35] + J_MM[42+4] * cov7[42];
  double JTC29 = J_MM[4] * cov7[7] + J_MM[7+4] * cov7[7+1] + J_MM[14+4] * cov7[14+1] + J_MM[21+4] * cov7[21+1] + J_MM[28+4] * cov7[28+1] + J_MM[35+4] * cov7[35+1] + J_MM[42+4] * cov7[42+1];
  double JTC30 = J_MM[4] * cov7[14] + J_MM[7+4] * cov7[14+1] + J_MM[14+4] * cov7[14+2] + J_MM[21+4] * cov7[21+2] + J_MM[28+4] * cov7[28+2] + J_MM[35+4] * cov7[35+2] + J_MM[42+4] * cov7[42+2];
  double JTC31 = J_MM[4] * cov7[21] + J_MM[7+4] * cov7[21+1] + J_MM[14+4] * cov7[21+2] + J_MM[21+4] * cov7[21+3] + J_MM[28+4] * cov7[28+3] + J_MM[35+4] * cov7[35+3] + J_MM[42+4] * cov7[42+3];
  double JTC32 = J_MM[4] * cov7[28] + J_MM[7+4] * cov7[28+1] + J_MM[14+4] * cov7[28+2] + J_MM[21+4] * cov7[28+3] + J_MM[28+4] * cov7[28+4] + J_MM[35+4] * cov7[35+4] + J_MM[42+4] * cov7[42+4];
  double JTC33 = J_MM[4] * cov7[35] + J_MM[7+4] * cov7[35+1] + J_MM[14+4] * cov7[35+2] + J_MM[21+4] * cov7[35+3] + J_MM[28+4] * cov7[35+4] + J_MM[35+4] * cov7[35+5] + J_MM[42+4] * cov7[42+5];
  double JTC34 = J_MM[4] * cov7[42] + J_MM[7+4] * cov7[42+1] + J_MM[14+4] * cov7[42+2] + J_MM[21+4] * cov7[42+3] + J_MM[28+4] * cov7[42+4] + J_MM[35+4] * cov7[42+5] + J_MM[42+4] * cov7[42+6];

  double out7_40 = (J_MM[5] * cov7[0] + J_MM[7+5] * cov7[7] + J_MM[14+5] * cov7[14] + J_MM[21+5] * cov7[21] + J_MM[28+5] * cov7[28] + J_MM[35+5] * cov7[35] + J_MM[42+5] * cov7[42]) * J_MM[5] + (J_MM[5] * cov7[7] + J_MM[7+5] * cov7[7+1] + J_MM[14+5] * cov7[14+1] + J_MM[21+5] * cov7[21+1] + J_MM[28+5] * cov7[28+1] + J_MM[35+5] * cov7[35+1] + J_MM[42+5] * cov7[42+1]) * J_MM[7+5] + (J_MM[5] * cov7[14] + J_MM[7+5] * cov7[14+1] + J_MM[14+5] * cov7[14+2] + J_MM[21+5] * cov7[21+2] + J_MM[28+5] * cov7[28+2] + J_MM[35+5] * cov7[35+2] + J_MM[42+5] * cov7[42+2]) * J_MM[14+5] + (J_MM[5] * cov7[21] + J_MM[7+5] * cov7[21+1] + J_MM[14+5] * cov7[21+2] + J_MM[21+5] * cov7[21+3] + J_MM[28+5] * cov7[28+3] + J_MM[35+5] * cov7[35+3] + J_MM[42+5] * cov7[42+3]) * J_MM[21+5] + (J_MM[5] * cov7[28] + J_MM[7+5] * cov7[28+1] + J_MM[14+5] * cov7[28+2] + J_MM[21+5] * cov7[28+3] + J_MM[28+5] * cov7[28+4] + J_MM[35+5] * cov7[35+4] + J_MM[42+5] * cov7[42+4]) * J_MM[28+5] + (J_MM[5] * cov7[35] + J_MM[7+5] * cov7[35+1] + J_MM[14+5] * cov7[35+2] + J_MM[21+5] * cov7[35+3] + J_MM[28+5] * cov7[35+4] + J_MM[35+5] * cov7[35+5] + J_MM[42+5] * cov7[42+5]) * J_MM[35+5] + (J_MM[5] * cov7[42] + J_MM[7+5] * cov7[42+1] + J_MM[14+5] * cov7[42+2] + J_MM[21+5] * cov7[42+3] + J_MM[28+5] * cov7[42+4] + J_MM[35+5] * cov7[42+5] + J_MM[42+5] * cov7[42+6]) * J_MM[42+5];

  // last row
  cov7[6]    = JTC6;
  cov7[7+6]  = JTC13;
  cov7[14+6] = JTC20;
  cov7[21+6] = JTC27;
  cov7[28+6] = JTC34;
  cov7[35+6] = J_MM[5] * cov7[42] + J_MM[7+5] * cov7[42+1] + J_MM[14+5] * cov7[42+2] + J_MM[21+5] * cov7[42+3] + J_MM[28+5] * cov7[42+4] + J_MM[35+5] * cov7[42+5] + J_MM[42+5] * cov7[42+6];
  //cov7[42+6] = cov7[42+6];

  // loops are vectorizable by the compiler!
  for (int i=0; i<6; ++i) cov7[i]    = JTC0  * J_MM[i] + JTC1  * J_MM[7+i] + JTC2  * J_MM[14+i] + JTC3  * J_MM[21+i] + JTC4  * J_MM[28+i] + JTC5  * J_MM[35+i] + JTC6  * J_MM[42+i];
  for (int i=1; i<6; ++i) cov7[7+i]  = JTC7  * J_MM[i] + JTC8  * J_MM[7+i] + JTC9  * J_MM[14+i] + JTC10 * J_MM[21+i] + JTC11 * J_MM[28+i] + JTC12 * J_MM[35+i] + JTC13 * J_MM[42+i];
  for (int i=2; i<6; ++i) cov7[14+i] = JTC14 * J_MM[i] + JTC15 * J_MM[7+i] + JTC16 * J_MM[14+i] + JTC17 * J_MM[21+i] + JTC18 * J_MM[28+i] + JTC19 * J_MM[35+i] + JTC20 * J_MM[42+i];
  for (int i=3; i<6; ++i) cov7[21+i] = JTC21 * J_MM[i] + JTC22 * J_MM[7+i] + JTC23 * J_MM[14+i] + JTC24 * J_MM[21+i] + JTC25 * J_MM[28+i] + JTC26 * J_MM[35+i] + JTC27 * J_MM[42+i];
  for (int i=4; i<6; ++i) cov7[28+i] = JTC28 * J_MM[i] + JTC29 * J_MM[7+i] + JTC30 * J_MM[14+i] + JTC31 * J_MM[21+i] + JTC32 * J_MM[28+i] + JTC33 * J_MM[35+i] + JTC34 * J_MM[42+i];
  cov7[35+5] = out7_40;

  // symmetric part
  cov7[7]  = cov7[1];
  cov7[14] = cov7[2];  cov7[14+1] = cov7[9];
  cov7[21] = cov7[3];  cov7[21+1] = cov7[10];  cov7[21+2] = cov7[17];
  cov7[28] = cov7[4];  cov7[28+1] = cov7[11];  cov7[28+2] = cov7[18];  cov7[28+3] = cov7[25];
  cov7[35] = cov7[5];  cov7[35+1] = cov7[12];  cov7[35+2] = cov7[19];  cov7[35+3] = cov7[26];  cov7[35+4] = cov7[33];
  cov7[42] = cov7[6];  cov7[42+1] = cov7[13];  cov7[42+2] = cov7[20];  cov7[42+3] = cov7[27];  cov7[42+4] = cov7[34];  cov7[42+5] = cov7[41];

}


void RKTrackRep::J_MMxJ_MM(M7x7& J_MM, const M7x7& J_MM_old){

  // J and J_old are
  // 1 0 0 0 0 0 0
  // 0 1 0 0 0 0 0
  // 0 0 1 0 0 0 0
  // x x x x x x 0
  // x x x x x x 0
  // x x x x x x 0
  // x x x x x x x

  M7x7 J_MM_temp;
  memcpy(J_MM_temp, J_MM, 7*7*sizeof(double));

  /*J_MM[0] = 1;
  J_MM[1] = 0;
  J_MM[2] = 0;
  J_MM[3] = 0;
  J_MM[4] = 0;
  J_MM[5] = 0;
  J_MM[6] = 0;

  J_MM[1*7+0] = 0;
  J_MM[1*7+1] = 1;
  J_MM[1*7+2] = 0;
  J_MM[1*7+3] = 0;
  J_MM[1*7+4] = 0;
  J_MM[1*7+5] = 0;
  J_MM[1*7+6] = 0;

  J_MM[2*7+0] = 0;
  J_MM[2*7+1] = 0;
  J_MM[2*7+2] = 1;
  J_MM[2*7+3] = 0;
  J_MM[2*7+4] = 0;
  J_MM[2*7+5] = 0;
  J_MM[2*7+6] = 0;*/

  J_MM[21] = J_MM_old[21] + J_MM_old[21+3] * J_MM_temp[21] + J_MM_old[21+4] * J_MM_temp[28] + J_MM_old[21+5] * J_MM_temp[35];
  J_MM[21+1] = J_MM_old[21+1] + J_MM_old[21+3] * J_MM_temp[21+1] + J_MM_old[21+4] * J_MM_temp[28+1] + J_MM_old[21+5] * J_MM_temp[35+1];
  J_MM[21+2] = J_MM_old[21+2] + J_MM_old[21+3] * J_MM_temp[21+2] + J_MM_old[21+4] * J_MM_temp[28+2] + J_MM_old[21+5] * J_MM_temp[35+2];
  J_MM[21+3] = J_MM_old[21+3] * J_MM_temp[21+3] + J_MM_old[21+4] * J_MM_temp[28+3] + J_MM_old[21+5] * J_MM_temp[35+3];
  J_MM[21+4] = J_MM_old[21+3] * J_MM_temp[21+4] + J_MM_old[21+4] * J_MM_temp[28+4] + J_MM_old[21+5] * J_MM_temp[35+4];
  J_MM[21+5] = J_MM_old[21+3] * J_MM_temp[21+5] + J_MM_old[21+4] * J_MM_temp[28+5] + J_MM_old[21+5] * J_MM_temp[35+5];
  //J_MM[21+6] = 0;

  J_MM[28] = J_MM_old[28] + J_MM_old[28+3] * J_MM_temp[21] + J_MM_old[28+4] * J_MM_temp[28] + J_MM_old[28+5] * J_MM_temp[35];
  J_MM[28+1] = J_MM_old[28+1] + J_MM_old[28+3] * J_MM_temp[21+1] + J_MM_old[28+4] * J_MM_temp[28+1] + J_MM_old[28+5] * J_MM_temp[35+1];
  J_MM[28+2] = J_MM_old[28+2] + J_MM_old[28+3] * J_MM_temp[21+2] + J_MM_old[28+4] * J_MM_temp[28+2] + J_MM_old[28+5] * J_MM_temp[35+2];
  J_MM[28+3] = J_MM_old[28+3] * J_MM_temp[21+3] + J_MM_old[28+4] * J_MM_temp[28+3] + J_MM_old[28+5] * J_MM_temp[35+3];
  J_MM[28+4] = J_MM_old[28+3] * J_MM_temp[21+4] + J_MM_old[28+4] * J_MM_temp[28+4] + J_MM_old[28+5] * J_MM_temp[35+4];
  J_MM[28+5] = J_MM_old[28+3] * J_MM_temp[21+5] + J_MM_old[28+4] * J_MM_temp[28+5] + J_MM_old[28+5] * J_MM_temp[35+5];
  //J_MM[28+6] = 0;

  J_MM[35] = J_MM_old[35] + J_MM_old[35+3] * J_MM_temp[21] + J_MM_old[35+4] * J_MM_temp[28] + J_MM_old[35+5] * J_MM_temp[35] ;
  J_MM[35+1] = J_MM_old[35+1] + J_MM_old[35+3] * J_MM_temp[21+1] + J_MM_old[35+4] * J_MM_temp[28+1] + J_MM_old[35+5] * J_MM_temp[35+1];
  J_MM[35+2] = J_MM_old[35+2] + J_MM_old[35+3] * J_MM_temp[21+2] + J_MM_old[35+4] * J_MM_temp[28+2] + J_MM_old[35+5] * J_MM_temp[35+2];
  J_MM[35+3] = J_MM_old[35+3] * J_MM_temp[21+3] + J_MM_old[35+4] * J_MM_temp[28+3] + J_MM_old[35+5] * J_MM_temp[35+3];
  J_MM[35+4] = J_MM_old[35+3] * J_MM_temp[21+4] + J_MM_old[35+4] * J_MM_temp[28+4] + J_MM_old[35+5] * J_MM_temp[35+4];
  J_MM[35+5] = J_MM_old[35+3] * J_MM_temp[21+5] + J_MM_old[35+4] * J_MM_temp[28+5] + J_MM_old[35+5] * J_MM_temp[35+5];
  //J_MM[35+6] = 0;

  J_MM[42] = J_MM_old[42] + J_MM_old[42+3] * J_MM_temp[21] + J_MM_old[42+4] * J_MM_temp[28] + J_MM_old[42+5] * J_MM_temp[35] + J_MM_old[42+6] * J_MM_temp[42];
  J_MM[42+1] = J_MM_old[42+1] + J_MM_old[42+3] * J_MM_temp[21+1] + J_MM_old[42+4] * J_MM_temp[28+1] + J_MM_old[42+5] * J_MM_temp[35+1] + J_MM_old[42+6] * J_MM_temp[42+1];
  J_MM[42+2] = J_MM_old[42+2] + J_MM_old[42+3] * J_MM_temp[21+2] + J_MM_old[42+4] * J_MM_temp[28+2] + J_MM_old[42+5] * J_MM_temp[35+2] + J_MM_old[42+6] * J_MM_temp[42+2];
  J_MM[42+3] = J_MM_old[42+3] * J_MM_temp[21+3] + J_MM_old[42+4] * J_MM_temp[28+3] + J_MM_old[42+5] * J_MM_temp[35+3] + J_MM_old[42+6] * J_MM_temp[42+3];
  J_MM[42+4] = J_MM_old[42+3] * J_MM_temp[21+4] + J_MM_old[42+4] * J_MM_temp[28+4] + J_MM_old[42+5] * J_MM_temp[35+4] + J_MM_old[42+6] * J_MM_temp[42+4];
  J_MM[42+5] = J_MM_old[42+3] * J_MM_temp[21+5] + J_MM_old[42+4] * J_MM_temp[28+5] + J_MM_old[42+5] * J_MM_temp[35+5] + J_MM_old[42+6] * J_MM_temp[42+5];
  J_MM[42+6] = J_MM_old[42+6] * J_MM_temp[42+6];

}


void RKTrackRep::printDim(const double* mat, unsigned int dimX, unsigned int dimY){

  std::cout << dimX << " x " << dimY << " matrix as follows: \n";
  for (unsigned int i=0; i< dimX; ++i){
    for (unsigned int j=0; j< dimY; ++j){
      printf("  %11.5g", mat[i*dimX+j]);
    }
    std::cout<<"\n";
  }
  std::cout<<std::endl;

}

int RKTrackRep::PropagateError(TVector3 initpos, TVector3 initmom,
                               TVector3 finalpos, TVector3 finalmom,
                               double stepLengthCm, double charge)
{
  double kCarTolerance = MINSTEP;
  if( fabs(stepLengthCm) <= kCarTolerance ) return 0;
  
#ifdef DEBUG
  if( iverbose >= 2 )std::cout << "  RKTrackRep::PropagateError " << std::endl;
#endif

  // * *** ERROR PROPAGATION ON A HELIX ASSUMING SC VARIABLES
  //correct to avoid propagation along Z 
  if( initmom.Mag() == initmom.Z() ) initmom.SetX( 1.E-9 );
  if( finalmom.Mag() == finalmom.Z() ) finalmom.SetX( 1.E-9 );

  double pPre = initmom.Mag();
  double pPost = finalmom.Mag();
#ifdef DEBUG
  if( iverbose >= 2 ) {
    std::cout << "G4EP: initpos " << initpos << std::endl
              << "G4EP: finalpos " << finalpos << std::endl;
    std::cout << "G4EP: initmom " << initmom << std::endl
              << "G4EP: finalmom " << finalmom << std::endl;
    std::cout << " err start step " << fError << std::endl;
    std::cout << "G4EP: stepLengthCm " << stepLengthCm << std::endl;
  }
#endif

  if( pPre == 0. || pPost == 0 ) return 2;
  double pInvPre = 1./pPre;
  double pInvPost = 1./pPost;
  //double deltaPInv = pInvPost - pInvPre;

  TVector3 initmomNorm = initmom * pInvPre;
  TVector3 finalmomNorm = finalmom * pInvPost;
  //  if( iverbose >= 2 ) std::cout << "G4EP: initmomNorm " << initmomNorm << " finalmomNorm " << finalmomNorm << std::endl;
  //return if propagation along Z??  
  if( 1. - fabs(finalmomNorm.Z()) < kCarTolerance ) return 4;
  double sinpPre = sin( initmomNorm.Theta() ); //cosine perpendicular to pPre = sine pPre
  double sinpPost = sin( finalmomNorm.Theta() ); //cosine perpendicular to pPost = sine pPost
  double sinpPostInv = 1./sin( initmomNorm.Theta() );

#ifdef DEBUG
  if( iverbose >= 2 ) std::cout << "G4EP: cosl " << sinpPre << " cosl0 " << sinpPost << std::endl;
#endif
  //* *** DEFINE TRANSFORMATION MATRIX BETWEEN X1 AND X2 FOR
  //* *** NEUTRAL PARTICLE OR FIELDFREE REGION
  double transf[5][5] = {{0}};

  transf[3][2] = stepLengthCm * sinpPost;
  transf[4][1] = stepLengthCm;
  for( size_t ii=0;ii < 5; ii++ ){
    transf[ii][ii] = 1.;
  }
#ifdef DEBUG
  if( iverbose >= 2 ) {
    std::cout << "G4EP: transf matrix neutral " << transf;
  }
#endif

  //t check if particle has charge
  //t  if( charge == 0 ) goto 45;
  // check if the magnetic field is = 0.

  // calculate transformation except it NEUTRAL PARTICLE OR FIELDFREE REGION
  if( charge != 0. ) {

    // same dimensions as GEANT3 (kilogauss)
    TVector3 HPre = getFieldValKiloGauss(initpos);
    TVector3 HPost = getFieldValKiloGauss(finalpos);

    double magHPre = HPre.Mag();
    double magHPost = HPost.Mag();
#ifdef DEBUG
    if( iverbose >= 2 ) std::cout << "G4EP: HPre " << HPre << std::endl
                             << "G4EP: HPost " << HPost << std::endl;
#endif
    
  if( magHPre + magHPost != 0. ) {
      
   //* *** CHECK WHETHER H*ALFA/P IS TOO DIFFERENT AT X1 AND X2
    double gam;
    if( magHPost != 0. ){ 
      gam = HPost * finalmomNorm / magHPost;
    }else {
      gam = HPre * initmomNorm / magHPre;
    }
    
    // G4eMagneticLimitsProcess will limit the step, but based on an straight line trajectory
    double alphaSqr = 1. - gam * gam;
    double diffHSqr = (HPre * pInvPre - HPost * pInvPost).Mag2();
    double delhp6Sqr = 300.*300.;
#ifdef DEBUG
    if( iverbose >= 2 ) std::cout << " G4EP: gam " << gam << " alphaSqr " << alphaSqr << " diffHSqr " << diffHSqr << std::endl;
#endif
    if( diffHSqr * alphaSqr > delhp6Sqr ) return 3;


    //* *** DEFINE AVERAGE MAGNETIC FIELD AND GRADIENT
    double pInvAver = 1./(pInvPre + pInvPost );
    double CFACT8 = 2.997925E-4; 
    //double HAver
    TVector3 vHAverNorm( (HPre*pInvPre + HPost*pInvPost ) * pInvAver * charge * CFACT8 );
    double HAver = vHAverNorm.Mag();
    double invHAver = 1./HAver;
    vHAverNorm *= invHAver;
#ifdef DEBUG
    if( iverbose >= 2 ) std::cout << " G4EP: HaverNorm " << vHAverNorm << " magHAver " << HAver << " charge " << charge<< std::endl;
#endif

    double pAver = (pPre+pPost)*0.5;
    double QAver = -HAver/pAver;
    double thetaAver = QAver * stepLengthCm;
    double sinThetaAver = sin(thetaAver);
    double cosThetaAver = cos(thetaAver);
    double gamma = vHAverNorm * finalmomNorm;
    TVector3 AN2 = vHAverNorm.Cross( finalmomNorm );
    
#ifdef DEBUG
    if( iverbose >= 2 ) std::cout << " G4EP: AN2 " << AN2 << std::endl;
#endif
    double AU = 1./initmomNorm.Perp();
    //t  G4ThreeVector vU( initmomNorm.cross( G4ThreeVector(0.,0.,1.) ) * AU );
    TVector3 vUPre( -AU*initmomNorm.Y(), 
                    AU*initmomNorm.X(), 
                    0. );
    TVector3 vVPre( -initmomNorm.Z()*vUPre.Y(), 
                    initmomNorm.Z()*vUPre.X(), 
                    initmomNorm.X()*vUPre.Y() - initmomNorm.Y()*vUPre.X() );
    
    //
    AU = 1./finalmomNorm.Perp();
    //t  G4ThreeVector vU( finalmomNorm.cross( G4ThreeVector(0.,0.,1.) ) * AU );
    TVector3 vUPost( -AU*finalmomNorm.Y(), 
                     AU*finalmomNorm.X(), 
                     0. );
    TVector3 vVPost( -finalmomNorm.Z()*vUPost.Y(), 
                     finalmomNorm.Z()*vUPost.X(), 
                     finalmomNorm.X()*vUPost.Y() - finalmomNorm.Y()*vUPost.X() );
#ifdef DEBUG
    //-    std::cout << " finalmomNorm " << finalmomNorm << std::endl;
    if( iverbose >= 2 ) std::cout << " G4EP: AU " << AU << " vUPre " << vUPre << " vVPre " << vVPre << " vUPost " << vUPost << " vVPost " << vVPost << std::endl;
#endif
    TVector3 deltaPos( initpos - finalpos );

    // * *** COMPLETE TRANSFORMATION MATRIX BETWEEN ERRORS AT X1 AND X2
    // * *** FIELD GRADIENT PERPENDICULAR TO TRACK IS PRESENTLY NOT
    // * *** TAKEN INTO ACCOUNT
    
    double QP = QAver * pAver; // = -HAver
#ifdef DEBUG
    if( iverbose >= 2) std::cout << " G4EP: QP " << QP << " QAver " << QAver << " pAver " << pAver << std::endl;
#endif
    double ANV = -( vHAverNorm.X()*vUPost.X() + vHAverNorm.Y()*vUPost.Y() );
    double ANU = ( vHAverNorm.X()*vVPost.X() + vHAverNorm.Y()*vVPost.Y() + vHAverNorm.Z()*vVPost.Z() );
    double OMcosThetaAver = 1. - cosThetaAver;
#ifdef DEBUG
    if( iverbose >= 2) std::cout << "G4EP: OMcosThetaAver " << OMcosThetaAver << " cosThetaAver " << cosThetaAver << " thetaAver " << thetaAver << " QAver " << QAver << " stepLengthCm " << stepLengthCm << std::endl;
#endif
    double TMSINT = thetaAver - sinThetaAver;
#ifdef DEBUG
    if( iverbose >= 2 ) std::cout << " G4EP: ANV " << ANV << " ANU " << ANU << std::endl;
#endif
    
    TVector3 vHUPre( -vHAverNorm.Z() * vUPre.Y(),
                     vHAverNorm.Z() * vUPre.X(),
                     vHAverNorm.X() * vUPre.Y() - vHAverNorm.Y() * vUPre.X() );
#ifdef DEBUG
    //    if( iverbose >= 2 ) std::cout << "G4EP: HUPre(1) " << vHUPre.X() << " " << vHAverNorm.Z() << " " << vUPre.Y() << std::endl;
#endif
    TVector3 vHVPre( vHAverNorm.Y() * vVPre.Z() - vHAverNorm.Z() * vVPre.Y(),
                     vHAverNorm.Z() * vVPre.X() - vHAverNorm.X() * vVPre.Z(),
                     vHAverNorm.X() * vVPre.Y() - vHAverNorm.Y() * vVPre.X() );
#ifdef DEBUG
    if( iverbose >= 2 ) std::cout << " G4EP: HUPre " << vHUPre << " HVPre " << vHVPre << std::endl;
#endif
    
    //------------------- COMPUTE MATRIX
    //---------- 1/P
    
    // MGW - I never lose energy during propagation
    transf[0][0] = 1.;

    // transf[0][0] = 1.-deltaPInv*pAver*(1.+(finalmomNorm.X()*deltaPos.X()+finalmomNorm.Y()*deltaPos.Y()+finalmomNorm.Z()*deltaPos.Z())/stepLengthCm)
    //   +2.*deltaPInv*pAver;
    
    // transf[0][1] =  -deltaPInv/thetaAver*
    //   ( TMSINT*gamma*(vHAverNorm.X()*vVPre.X()+vHAverNorm.Y()*vVPre.Y()+vHAverNorm.Z()*vVPre.Z()) +
    //     sinThetaAver*(vVPre.X()*finalmomNorm.X()+vVPre.Y()*finalmomNorm.Y()+vVPre.Z()*finalmomNorm.Z()) +
    //     OMcosThetaAver*(vHVPre.X()*finalmomNorm.X()+vHVPre.Y()*finalmomNorm.Y()+vHVPre.Z()*finalmomNorm.Z()) );
    
    // transf[0][2] =  -sinpPre*deltaPInv/thetaAver*
    //   ( TMSINT*gamma*(vHAverNorm.X()*vUPre.X()+vHAverNorm.Y()*vUPre.Y()            ) +
    //     sinThetaAver*(vUPre.X()*finalmomNorm.X()+vUPre.Y()*finalmomNorm.Y()            ) +
    //     OMcosThetaAver*(vHUPre.X()*finalmomNorm.X()+vHUPre.Y()*finalmomNorm.Y()+vHUPre.Z()*finalmomNorm.Z()) );
    
    // transf[0][3] =  -deltaPInv/stepLengthCm*(vUPre.X()*finalmomNorm.X()+vUPre.Y()*finalmomNorm.Y()            );
    
    // transf[0][4] =  -deltaPInv/stepLengthCm*(vVPre.X()*finalmomNorm.X()+vVPre.Y()*finalmomNorm.Y()+vVPre.Z()*finalmomNorm.Z());
    
    // ***   Lambda
    transf[1][0] = -QP*ANV*(finalmomNorm.X()*deltaPos.X()+finalmomNorm.Y()*deltaPos.Y()+finalmomNorm.Z()*deltaPos.Z());
    // *(1.+deltaPInv*pAver);
#ifdef DEBUG
     if(iverbose >= 3) std::cout << "ctransf10= " << transf[1][0]  << " " <<  -QP<< " " << ANV<< " " << finalmomNorm.X()<< " " << deltaPos.X()<< " " << finalmomNorm.Y()<< " " << deltaPos.Y()<< " " << finalmomNorm.Z()<< " " << deltaPos.Z()
      << " " << deltaPInv<< " " << pAver << std::endl;
#endif
    
    transf[1][1] = cosThetaAver*(vVPre.X()*vVPost.X()+vVPre.Y()*vVPost.Y()+vVPre.Z()*vVPost.Z()) +
      sinThetaAver*(vHVPre.X()*vVPost.X()+vHVPre.Y()*vVPost.Y()+vHVPre.Z()*vVPost.Z()) +
      OMcosThetaAver*(vHAverNorm.X()*vVPre.X()+vHAverNorm.Y()*vVPre.Y()+vHAverNorm.Z()*vVPre.Z())*
      (vHAverNorm.X()*vVPost.X()+vHAverNorm.Y()*vVPost.Y()+vHAverNorm.Z()*vVPost.Z()) +
      ANV*( -sinThetaAver*(vVPre.X()*finalmomNorm.X()+vVPre.Y()*finalmomNorm.Y()+vVPre.Z()*finalmomNorm.Z()) +
            OMcosThetaAver*(vVPre.X()*AN2.X()+vVPre.Y()*AN2.Y()+vVPre.Z()*AN2.Z()) -
            TMSINT*gamma*(vHAverNorm.X()*vVPre.X()+vHAverNorm.Y()*vVPre.Y()+vHAverNorm.Z()*vVPre.Z()) );
    
    transf[1][2] = cosThetaAver*(vUPre.X()*vVPost.X()+vUPre.Y()*vVPost.Y()            ) +
      sinThetaAver*(vHUPre.X()*vVPost.X()+vHUPre.Y()*vVPost.Y()+vHUPre.Z()*vVPost.Z()) +
      OMcosThetaAver*(vHAverNorm.X()*vUPre.X()+vHAverNorm.Y()*vUPre.Y()            )*
      (vHAverNorm.X()*vVPost.X()+vHAverNorm.Y()*vVPost.Y()+vHAverNorm.Z()*vVPost.Z()) +
      ANV*( -sinThetaAver*(vUPre.X()*finalmomNorm.X()+vUPre.Y()*finalmomNorm.Y()            ) +
            OMcosThetaAver*(vUPre.X()*AN2.X()+vUPre.Y()*AN2.Y()             ) -
            TMSINT*gamma*(vHAverNorm.X()*vUPre.X()+vHAverNorm.Y()*vUPre.Y()            ) );
    transf[1][2] = sinpPre*transf[1][2];
    
    transf[1][3] = -QAver*ANV*(vUPre.X()*finalmomNorm.X()+vUPre.Y()*finalmomNorm.Y()            );
    
    transf[1][4] = -QAver*ANV*(vVPre.X()*finalmomNorm.X()+vVPre.Y()*finalmomNorm.Y()+vVPre.Z()*finalmomNorm.Z());
    
    // ***   Phi
    
    transf[2][0] = -QP*ANU*(finalmomNorm.X()*deltaPos.X()+finalmomNorm.Y()*deltaPos.Y()+finalmomNorm.Z()*deltaPos.Z())*sinpPostInv;
      // *(1.+deltaPInv*pAver);
#ifdef DEBUG
   if(iverbose >= 3)std::cout <<"ctransf20= " << transf[2][0] <<" "<< -QP<<" "<<ANU<<" "<<finalmomNorm.X()<<" "<<deltaPos.X()<<" "<<finalmomNorm.Y()<<" "<<deltaPos.Y()<<" "<<finalmomNorm.Z()<<" "<<deltaPos.Z()<<" "<<sinpPostInv
         <<" "<<deltaPInv<<" "<<pAver<< std::endl;
#endif
    transf[2][1] = cosThetaAver*(vVPre.X()*vUPost.X()+vVPre.Y()*vUPost.Y()            ) +
      sinThetaAver*(vHVPre.X()*vUPost.X()+vHVPre.Y()*vUPost.Y()             ) +
      OMcosThetaAver*(vHAverNorm.X()*vVPre.X()+vHAverNorm.Y()*vVPre.Y()+vHAverNorm.Z()*vVPre.Z())*
      (vHAverNorm.X()*vUPost.X()+vHAverNorm.Y()*vUPost.Y()            ) +
      ANU*( -sinThetaAver*(vVPre.X()*finalmomNorm.X()+vVPre.Y()*finalmomNorm.Y()+vVPre.Z()*finalmomNorm.Z()) +
            OMcosThetaAver*(vVPre.X()*AN2.X()+vVPre.Y()*AN2.Y()+vVPre.Z()*AN2.Z()) -
            TMSINT*gamma*(vHAverNorm.X()*vVPre.X()+vHAverNorm.Y()*vVPre.Y()+vHAverNorm.Z()*vVPre.Z()) );
    transf[2][1] = sinpPostInv*transf[2][1];
    
    transf[2][2] = cosThetaAver*(vUPre.X()*vUPost.X()+vUPre.Y()*vUPost.Y()            ) +
      sinThetaAver*(vHUPre.X()*vUPost.X()+vHUPre.Y()*vUPost.Y()             ) +
      OMcosThetaAver*(vHAverNorm.X()*vUPre.X()+vHAverNorm.Y()*vUPre.Y()            )*
      (vHAverNorm.X()*vUPost.X()+vHAverNorm.Y()*vUPost.Y()            ) +
      ANU*( -sinThetaAver*(vUPre.X()*finalmomNorm.X()+vUPre.Y()*finalmomNorm.Y()            ) +
            OMcosThetaAver*(vUPre.X()*AN2.X()+vUPre.Y()*AN2.Y()             ) -
            TMSINT*gamma*(vHAverNorm.X()*vUPre.X()+vHAverNorm.Y()*vUPre.Y()            ) );
    transf[2][2] = sinpPostInv*sinpPre*transf[2][2];
    
    transf[2][3] = -QAver*ANU*(vUPre.X()*finalmomNorm.X()+vUPre.Y()*finalmomNorm.Y()            )*sinpPostInv;
#ifdef DEBUG
    if(iverbose >= 3)std::cout <<"ctransf23= " << transf[2][3] <<" "<< -QAver<<" "<<ANU<<" "<<vUPre.X()<<" "<<finalmomNorm.X()<<" "<< vUPre.Y()<<" "<<finalmomNorm.Y()<<" "<<sinpPostInv<<std::endl;
#endif
    
    transf[2][4] = -QAver*ANU*(vVPre.X()*finalmomNorm.X()+vVPre.Y()*finalmomNorm.Y()+vVPre.Z()*finalmomNorm.Z())*sinpPostInv;
    
    // ***   Yt
    
    transf[3][0] = pAver*(vUPost.X()*deltaPos.X()+vUPost.Y()*deltaPos.Y() );
    //  *(1.+deltaPInv*pAver);
#ifdef DEBUG
   if(iverbose >= 3) std::cout <<"ctransf30= " << transf[3][0] <<" "<< pAver<<" "<<vUPost.X()<<" "<<deltaPos.X()<<" "<<vUPost.Y()<<" "<<deltaPos.Y()  
      <<" "<<deltaPInv<<" "<<pAver<<std::endl;
#endif

    transf[3][1] = (   sinThetaAver*(vVPre.X()*vUPost.X()+vVPre.Y()*vUPost.Y()            ) +
                       OMcosThetaAver*(vHVPre.X()*vUPost.X()+vHVPre.Y()*vUPost.Y()             ) +
                       TMSINT*(vHAverNorm.X()*vUPost.X()+vHAverNorm.Y()*vUPost.Y()            )*
                       (vHAverNorm.X()*vVPre.X()+vHAverNorm.Y()*vVPre.Y()+vHAverNorm.Z()*vVPre.Z()) )/QAver;
    
    transf[3][2] = (   sinThetaAver*(vUPre.X()*vUPost.X()+vUPre.Y()*vUPost.Y()            ) +
                       OMcosThetaAver*(vHUPre.X()*vUPost.X()+vHUPre.Y()*vUPost.Y()             ) +
                       TMSINT*(vHAverNorm.X()*vUPost.X()+vHAverNorm.Y()*vUPost.Y()            )*
                       (vHAverNorm.X()*vUPre.X()+vHAverNorm.Y()*vUPre.Y()            ) )*sinpPre/QAver;
#ifdef DEBUG 
   if(iverbose >= 3) std::cout <<"ctransf32= " << transf[3][2] <<" "<< sinThetaAver<<" "<<vUPre.X()<<" "<<vUPost.X()<<" "<<vUPre.Y()<<" "<<vUPost.Y() <<" "<<
                       OMcosThetaAver<<" "<<vHUPre.X()<<" "<<vUPost.X()<<" "<<vHUPre.Y()<<" "<<vUPost.Y() <<" "<<
                       TMSINT<<" "<<vHAverNorm.X()<<" "<<vUPost.X()<<" "<<vHAverNorm.Y()<<" "<<vUPost.Y() <<" "<<
      vHAverNorm.X()<<" "<<vUPre.X()<<" "<<vHAverNorm.Y()<<" "<<vUPre.Y() <<" "<<sinpPre<<" "<<QAver<<std::endl;
#endif
   
    transf[3][3] = (vUPre.X()*vUPost.X()+vUPre.Y()*vUPost.Y()            );
    
    transf[3][4] = (vVPre.X()*vUPost.X()+vVPre.Y()*vUPost.Y()            );

    // ***   Zt
    transf[4][0] = pAver*(vVPost.X()*deltaPos.X()+vVPost.Y()*deltaPos.Y()+vVPost.Z()*deltaPos.Z());
    //  *(1.+deltaPInv*pAver);
   
    transf[4][1] = (   sinThetaAver*(vVPre.X()*vVPost.X()+vVPre.Y()*vVPost.Y()+vVPre.Z()*vVPost.Z()) +
                       OMcosThetaAver*(vHVPre.X()*vVPost.X()+vHVPre.Y()*vVPost.Y()+vHVPre.Z()*vVPost.Z()) +
                       TMSINT*(vHAverNorm.X()*vVPost.X()+vHAverNorm.Y()*vVPost.Y()+vHAverNorm.Z()*vVPost.Z())*
                       (vHAverNorm.X()*vVPre.X()+vHAverNorm.Y()*vVPre.Y()+vHAverNorm.Z()*vVPre.Z()) )/QAver;
#ifdef DEBUG
    if(iverbose >= 3)std::cout <<"ctransf41= " << transf[4][1] <<" "<< sinThetaAver<<" "<< OMcosThetaAver <<" "<<TMSINT<<" "<< vVPre <<" "<<vVPost <<" "<<vHVPre<<" "<<vHAverNorm <<" "<< QAver<<std::endl;
#endif
    
    transf[4][2] = (   sinThetaAver*(vUPre.X()*vVPost.X()+vUPre.Y()*vVPost.Y()            ) +
                       OMcosThetaAver*(vHUPre.X()*vVPost.X()+vHUPre.Y()*vVPost.Y()+vHUPre.Z()*vVPost.Z()) +
                       TMSINT*(vHAverNorm.X()*vVPost.X()+vHAverNorm.Y()*vVPost.Y()+vHAverNorm.Z()*vVPost.Z())*
                       (vHAverNorm.X()*vUPre.X()+vHAverNorm.Y()*vUPre.Y()            ) )*sinpPre/QAver;

    transf[4][3] = (vUPre.X()*vVPost.X()+vUPre.Y()*vVPost.Y()  );

    transf[4][4] = (vVPre.X()*vVPost.X()+vVPre.Y()*vVPost.Y()+vVPre.Z()*vVPost.Z()); 
    //   if(iverbose >= 3) std::cout <<"ctransf44= " << transf[4][4] <<" "<< vVPre.X()  <<" "<<vVPost.X() <<" "<< vVPre.Y() <<" "<< vVPost.Y() <<" "<< vVPre.Z() <<" "<< vVPost.Z() << std::endl;

  
#ifdef DEBUG
    if( iverbose >= 1 ) std::cout << "G4EP: transf matrix computed " << transf << std::endl;
#endif
    /*    for( G4int ii=0;ii<5;ii++){
      for( G4int jj=0;jj<5;jj++){
        std::cout << transf[ii][jj] << " ";
      }
      std::cout << std::endl;
      } */
   }
  }
  // end of calculate transformation except it NEUTRAL PARTICLE OR FIELDFREE REGION
  /*  if( iverbose >= 1 ) std::cout << "G4EP: transf not updated but initialized " << theFirstStep << std::endl;
  if( theFirstStep ) {
    theTransfMat = transf;
    theFirstStep = false;
  }else{
    theTransfMat = theTransfMat * transf;
    if( iverbose >= 1 ) std::cout << "G4EP: transf matrix accumulated" << theTransfMat << std::endl;
  } 
  */

  TMatrixD stepTransfMat(5,5);
  for(int i=0; i<5; ++i)
    for(int j=0; j<5; ++j)
      stepTransfMat(i,j) = transf[i][j];


  totalTransfMat = stepTransfMat * totalTransfMat;

#ifdef DEBUG
  if( iverbose >= 1 ) std::cout << "G4EP: error matrix before transformation " << fError << std::endl;
  if( iverbose >= 2 ) std::cout << " tf * err " << stepTransfMat * fError << std::endl
                                << " step transf matrix " << stepTransfMat
                                << " total transf matrix " << totalTransfMat << std::endl;
#endif
  
  TMatrixD stepTransfMatTranspose = stepTransfMat;
  stepTransfMatTranspose.T();

  fError = stepTransfMat * fError * stepTransfMatTranspose;
  //-    fError = transf * fError * transf.T();
#ifdef DEBUG
  if( iverbose >= 1 ) std::cout << "G4EP: error matrix propagated " << fError << std::endl;
#endif
  
  //? S = B*S*BT S.similarity(B)
  //? R = S
  // not needed * *** TRANSFORM ERROR MATRIX FROM INTERNAL TO EXTERNAL VARIABLES;
  
  return 0;
}

ClassImp(RKTrackRep)
