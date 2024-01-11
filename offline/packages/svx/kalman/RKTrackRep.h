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

/*
 */

/** @addtogroup RKTrackRep
 * @{
 */


#ifndef RKTRACKREP_H
#define RKTRACKREP_H

#include "GFPointPath.h"
//#include "RKTools.h"

#include "GFAbsTrackRep.h"
#include "GFDetPlane.h"
//#include <GFTrackCand.h>
#include <TMatrixD.h>
#include <TMatrixDSym.h>

class PHField3D;

/** @brief Track Representation module based on a Runge-Kutta algorithm including a full material model
 *
 *  @author Christian H&ouml;ppner (Technische Universit&auml;t M&uuml;nchen, original author)
 *  @author Johannes Rauch (Technische Universit&auml;t M&uuml;nchen, author)
 *
 * The Runge Kutta implementation stems from GEANT3 originally (R. Brun et al.).
 * Porting to %C goes back to Igor Gavrilenko @ CERN.
 * The code was taken from the Phast analysis package of the COMPASS experiment
 * (Sergei Gerrassimov @ CERN).
 *
 * The state is 5D: (q/p, u', v', u, v)
 */

typedef double M1x3[1*3];
typedef double M1x4[1*4];
typedef double M1x6[1*6];
typedef double M1x7[1*7];
typedef double M5x5[5*5];
typedef double M6x6[6*6];
typedef double M7x7[7*7];
typedef double M8x7[8*7];
typedef double M6x5[6*5];
typedef double M7x5[7*5];
typedef double M5x6[5*6];
typedef double M5x7[5*7];

class RKTrackRep : public GFAbsTrackRep {

 public:

  // Constructors/Destructors ---------
  RKTrackRep();
  RKTrackRep(const TVector3& pos,
             const TVector3& mom,
             const TVector3& poserr,
             const TVector3& momerr,
             const int& PDGCode);

  RKTrackRep(const TVector3& pos,
             const TVector3& mom,
             const TMatrixDSym cov,
             const int& PDGCode);

  RKTrackRep(const TVector3& pos,
             const TVector3& mom,
             const int& PDGCode);

  //  RKTrackRep(const GFTrackCand* aGFTrackCandPtr, int pdgcode = 0);

  virtual ~RKTrackRep();

  // field in 10^-1 Tesla (!!!)
  static TVector3 getFieldValKiloGauss(const TVector3 &x);
  static TVector3 getFieldValTesla(const TVector3 &x);

  static void set_field_scale_factor(const float scale);
  static void set_uniform_field_tesla(const TVector3 &B);
  static void load_field(const char* filename, const double scale_factor=1., const int verb=0);
  static void delete_field();

  virtual GFAbsTrackRep* clone() const {return new RKTrackRep(*this);}
  virtual GFAbsTrackRep* prototype()const{return new RKTrackRep();}

  //! returns the tracklength spanned in this extrapolation
  /** The covariance matrix is transformed from the plane coordinate system to the master reference system (for the propagation) and, after propagation, back to the plane coordinate system.\n
    * Also the parameter spu (which is +1 or -1 and indicates the direction of the particle) is calculated and stored in #fCacheSpu. The plane is stored in #fCachePlane.
    * \n
    *  The transformation from plane- to master reference system obeys:
    * \f{eqnarray*}{
    * \boldsymbol{\mathbf{x}}    & = \boldsymbol{\mathbf{O}}+u\boldsymbol{\mathbf{U}}+v\boldsymbol{\mathbf{V}} \\
    * \boldsymbol{\mathbf{a}}    & = \frac{\boldsymbol{\mathbf{\widetilde{p}}}}{\widetilde{p}} \\
    * \frac{q}{p} & = \frac{q}{p}
    * \f}
    * with
    * \f{eqnarray*}{
    * \boldsymbol{\mathbf{\widetilde{p}}} & = spu \cdot \left(\boldsymbol{\mathbf{N}}+u^{\prime} \boldsymbol{\mathbf{U}} + v^{\prime} \boldsymbol{\mathbf{V}}\right) \\
    * \widetilde{p}        & = \left| \boldsymbol{\mathbf{\widetilde{p}}} \right| \textrm{.}
    * \f}
    * The following equations define the transformation from master- to plane reference system:
    * \f{eqnarray*}{
    * \frac{q}{p}  & = \frac{q}{p} \\
    * u^{\prime}   & = \frac{\boldsymbol{\mathbf{a}} \boldsymbol{\mathbf{U}}}{\boldsymbol{\mathbf{a}} \boldsymbol{\mathbf{N}}} \\
    * v^{\prime}   & = \frac{\boldsymbol{\mathbf{a}} \boldsymbol{\mathbf{V}}}{\boldsymbol{\mathbf{a}} \boldsymbol{\mathbf{N}}} \\
    * u & = \left(\boldsymbol{\mathbf{x}}-\boldsymbol{\mathbf{O}}\right)\boldsymbol{\mathbf{U}}\\
    * v & = \left(\boldsymbol{\mathbf{x}}-\boldsymbol{\mathbf{O}}\right)\boldsymbol{\mathbf{V}}\\
    * \f}
    * with
    * \f{eqnarray*}{
    * spu & = \frac{\boldsymbol{\mathbf{a}}\boldsymbol{\mathbf{N}}}{\left|\boldsymbol{\mathbf{a}}\boldsymbol{\mathbf{N}} \right| }=\pm1 \textrm{.}
    * \f}
    *
    * Jacobians:\n
    * \f{eqnarray*}{
    *   C_M     & = J_{p,M} \, C_p \, J_{p,M}^T \\
    *   J_{p,M} & = \begin{pmatrix}
    *         \frac{\partial x}{\partial\frac{q}{p}}          & \frac{\partial x}{\partial u^{\prime}}          & \frac{\partial x}{\partial v^{\prime}}          & \frac{\partial x}{\partial u}           & \frac{\partial x}{\partial v}          \\
    *         \frac{\partial y}{\partial\frac{q}{p}}          & \frac{\partial y}{\partial u^{\prime}}          & \frac{\partial y}{\partial v^{\prime}}          & \frac{\partial y}{\partial u}           & \frac{\partial y}{\partial v}          \\
    *         \frac{\partial z}{\partial\frac{q}{p}}          & \frac{\partial z}{\partial u^{\prime}}          & \frac{\partial z}{\partial v^{\prime}}          & \frac{\partial z}{\partial u}           & \frac{\partial z}{\partial v}          \\
    *         \frac{\partial a_{x}}{\partial\frac{q}{p}}      & \frac{\partial a_{x}}{\partial u^{\prime}}      & \frac{\partial a_{x}}{\partial v^{\prime}}      & \frac{\partial a_{x}}{\partial u}       & \frac{\partial a_{x}}{\partial v}      \\
    *         \frac{\partial a_{y}}{\partial\frac{q}{p}}      & \frac{\partial a_{y}}{\partial u^{\prime}}      & \frac{\partial a_{y}}{\partial v^{\prime}}      & \frac{\partial a_{y}}{\partial u}       & \frac{\partial a_{y}}{\partial v}      \\
    *         \frac{\partial a_{z}}{\partial\frac{q}{p}}      & \frac{\partial a_{z}}{\partial u^{\prime}}      & \frac{\partial a_{z}}{\partial v^{\prime}}      & \frac{\partial a_{z}}{\partial u}       & \frac{\partial a_{z}}{\partial v}      \\
    *         \frac{\partial\frac{q}{p}}{\partial\frac{q}{p}} & \frac{\partial\frac{q}{p}}{\partial u^{\prime}} & \frac{\partial\frac{q}{p}}{\partial v^{\prime}} & \frac{\partial\frac{q}{p}}{\partial u}  & \frac{\partial\frac{q}{p}}{\partial v} \\
    *         \end{pmatrix} \\
    *           & = \begin{pmatrix}
    *         0 & 0 & 0 & U_{x} & V_{x} \\
    *         0 & 0 & 0 & U_{y} & V_{y} \\
    *         0 & 0 & 0 & U_{z} & V_{z} \\
    *         0 & \frac{d}{\widetilde{p}} \left( U_{x} - \frac{\widetilde{p}_{x}}{\widetilde{p}^2} \boldsymbol{\mathbf{U}} \boldsymbol{\mathbf{\widetilde{p}}}\right)  & \frac{d}{\widetilde{p}} \left( V_{x} - \frac{\widetilde{p}_{x}}{\widetilde{p}^2} \boldsymbol{\mathbf{V}} \boldsymbol{\mathbf{\widetilde{p}}}\right) & 0 & 0 \\
    *         0 & \frac{d}{\widetilde{p}} \left( U_{y} - \frac{\widetilde{p}_{x}}{\widetilde{p}^2} \boldsymbol{\mathbf{U}} \boldsymbol{\mathbf{\widetilde{p}}}\right)  & \frac{d}{\widetilde{p}} \left( V_{y} - \frac{\widetilde{p}_{x}}{\widetilde{p}^2} \boldsymbol{\mathbf{V}} \boldsymbol{\mathbf{\widetilde{p}}}\right) & 0 & 0 \\
    *         0 & \frac{d}{\widetilde{p}} \left( U_{z} - \frac{\widetilde{p}_{x}}{\widetilde{p}^2} \boldsymbol{\mathbf{U}} \boldsymbol{\mathbf{\widetilde{p}}}\right)  & \frac{d}{\widetilde{p}} \left( V_{z} - \frac{\widetilde{p}_{x}}{\widetilde{p}^2} \boldsymbol{\mathbf{V}} \boldsymbol{\mathbf{\widetilde{p}}}\right) & 0 & 0 \\
    *         1 & 0 & 0 & 0 & 0
    *         \end{pmatrix} \\
    *   C_p     & = J_{M.p} \, C_M \, J_{M,p}^T \\
    *   J_{M,p} & = \begin{pmatrix}
    *         \frac{\partial\frac{q}{p}}{\partial x} & \frac{\partial\frac{q}{p}}{\partial y} & \frac{\partial\frac{q}{p}}{\partial z} & \frac{\partial\frac{q}{p}}{\partial a_{x}} & \frac{\partial\frac{q}{p}}{\partial a_{y}} & \frac{\partial\frac{q}{p}}{\partial a_{z}} & \frac{\partial\frac{q}{p}}{\partial\frac{q}{p}} \\
    *         \frac{\partial u^{\prime}}{\partial x} & \frac{\partial u^{\prime}}{\partial y} & \frac{\partial u^{\prime}}{\partial z} & \frac{\partial u^{\prime}}{\partial a_{x}} & \frac{\partial u^{\prime}}{\partial a_{y}} & \frac{\partial u^{\prime}}{\partial a_{z}} & \frac{\partial u^{\prime}}{\partial\frac{q}{p}} \\
    *         \frac{\partial v^{\prime}}{\partial x} & \frac{\partial v^{\prime}}{\partial y} & \frac{\partial v^{\prime}}{\partial z} & \frac{\partial v^{\prime}}{\partial a_{x}} & \frac{\partial v^{\prime}}{\partial a_{y}} & \frac{\partial v^{\prime}}{\partial a_{z}} & \frac{\partial v^{\prime}}{\partial\frac{q}{p}} \\
    *         \frac{\partial u}{\partial x}          & \frac{\partial u}{\partial y}          & \frac{\partial u}{\partial z}          & \frac{\partial u}{\partial a_{x}}          & \frac{\partial u}{\partial a_{y}}          & \frac{\partial u}{\partial a_{z}}          & \frac{\partial u}{\partial\frac{q}{p}}          \\
    *         \frac{\partial v}{\partial x}          & \frac{\partial v}{\partial y}          & \frac{\partial v}{\partial z}          & \frac{\partial v}{\partial a_{x}}          & \frac{\partial v}{\partial a_{y}}          & \frac{\partial v}{\partial a_{z}}          & \frac{\partial v}{\partial\frac{q}{p}}          \\
    *         \end{pmatrix} \\
    *           & = \begin{pmatrix}
    *         0 & 0 & 0 & 0 & 0 & 0 & 1\\
    *         0 & 0 & 0 & \frac{U_{x} \boldsymbol{\mathbf{a}}\boldsymbol{\mathbf{N}} - N_{x} \boldsymbol{\mathbf{a}}\boldsymbol{\mathbf{U}}}{\left(\boldsymbol{\mathbf{a}}\boldsymbol{\mathbf{N}}\right)^{2}} & \frac{U_{y} \boldsymbol{\mathbf{a}}\boldsymbol{\mathbf{N}} - N_{y} \boldsymbol{\mathbf{a}}\boldsymbol{\mathbf{U}}}{\left(\boldsymbol{\mathbf{a}}\boldsymbol{\mathbf{N}}\right)^{2}} & \frac{U_{z} \boldsymbol{\mathbf{a}}\boldsymbol{\mathbf{N}} - N_{z} \boldsymbol{\mathbf{a}}\boldsymbol{\mathbf{U}}}{\left(\boldsymbol{\mathbf{a}}\boldsymbol{\mathbf{N}}\right)^{2}} & 0\\
    *         0 & 0 & 0 & \frac{V_{x} \boldsymbol{\mathbf{a}}\boldsymbol{\mathbf{N}} - N_{x} \boldsymbol{\mathbf{a}}\boldsymbol{\mathbf{V}}}{\left(\boldsymbol{\mathbf{a}}\boldsymbol{\mathbf{N}}\right)^{2}} & \frac{V_{y} \boldsymbol{\mathbf{a}}\boldsymbol{\mathbf{N}} - N_{y} \boldsymbol{\mathbf{a}}\boldsymbol{\mathbf{V}}}{\left(\boldsymbol{\mathbf{a}}\boldsymbol{\mathbf{N}}\right)^{2}} & \frac{V_{z} \boldsymbol{\mathbf{a}}\boldsymbol{\mathbf{N}} - N_{z} \boldsymbol{\mathbf{a}}\boldsymbol{\mathbf{V}}}{\left(\boldsymbol{\mathbf{a}}\boldsymbol{\mathbf{N}}\right)^{2}} & 0\\
    *         U_{x} & U_{y} & U_{z} & 0 & 0 & 0 & 0\\
    *         V_{x} & V_{y} & V_{z} & 0 & 0 & 0 & 0\\
    *         \end{pmatrix}
    * \f}
    */
  double extrapolate(const GFDetPlane&, 
                     TVectorD& statePred,
                     TMatrixDSym& covPred);

  //! returns the tracklength spanned in this extrapolation
  double extrapolate(const GFDetPlane&, 
                     TVectorD& statePred);

  //! This method is to extrapolate the track to point of closest approach to a point in space
  double extrapolateToPoint(const TVector3& pos,
                            TVector3& poca,
                            TVector3& dirInPoca);

  //! This method extrapolates to the point of closest approach to a line
  double extrapolateToLine(const TVector3& point1,
                           const TVector3& point2,
                           TVector3& poca,
                           TVector3& dirInPoca,
                           TVector3& poca_onwire);
  
  //! make step of h cm along the track, returns the tracklength spanned in this extrapolation
  /** Also returns the position and direction by reference.
  * It does NOT alter the state of the trackrep and starts extrapolating from #fRefPlane.
  */
  double stepalong(double h,
                   TVector3& point,
                   TVector3& dir);
  
  //! Returns position of the track in the plane  
  /** If #GFDetPlane equals the reference plane #fRefPlane, returns current position; otherwise it extrapolates 
    * the track to the plane and returns the position.
    */
  TVector3 getPos(const GFDetPlane&);
  //! Returns momentum of the track in the plane  
  /** If #GFDetPlane equals the reference plane #fRefPlane, returns current momentum; otherwise it extrapolates 
    * the track to the plane and returns the momentum.
    */
  TVector3 getMom(const GFDetPlane&);
  //! Gets position and momentum in the plane 
  /** If #GFDetPlane equals the reference plane #fRefPlane, it gets current position and momentum; otherwise it extrapolates 
    * the track to the plane and gets the position and momentum.
    */
  TVector3 getPos() {return getPos(fRefPlane);}
  TVector3 getMom() {return getMom(fRefPlane);}

  void getPosMom(const GFDetPlane&,TVector3& pos,TVector3& mom);

  void getPosMomCov(const GFDetPlane& pl,
                    TVector3& pos, TVector3& mom,
                    TMatrixDSym& cov);

  //! Returns charge
  double getCharge()const {return fCharge;}

  int getPDG() {return fPdg;};

  //! Set propagation direction. (-1, 0, 1) -> (backward prop, decide myself, forward)
  void setPropDir(int dir);

  //! Switch propagation direction. Has no effect if propdir is set to 0.
  void switchDirection(){fDirection = -1*fDirection;}

  //! Set PDG particle code
  void setPDG(int);

  //! Sets state, plane and (optionally) covariance
  /** This function also sets the parameter #fSpu to the value stored in #fCacheSpu. Therefore it has to be ensured that
    * the plane #pl is the same as the plane of the last extrapolation (i.e. #fCachePlane), where #fCacheSpu was calculated.
    * Hence, if the argument #pl is not equal to #fCachePlane, an error message is shown an an exception is thrown.
    */
  void setData(const TVectorD& st,
               const GFDetPlane& pl,
               const TMatrixDSym* cov=NULL,
               const TMatrixD* aux=NULL);

  //! Sets state, plane and covariance from position, momentum and 6x6 covariance
  /** Also sets the reference plane at position
    */
  void setPosMomCov(const TVector3& pos,
                    const TVector3& mom,
                    const TMatrixDSym& cov);

  void disableMaterialEffects(bool opt = true){fNoMaterial = opt;}

  const TMatrixD* getAuxInfo(const GFDetPlane& pl);
  
  bool hasAuxInfo() { return true; }

  TMatrixD GetTotalTransfMat() const {return totalTransfMat;}
  TMatrixD GetError() const {return fError;}
  void SetError(const TMatrixD &m) {fError = m;}


 private:

  void initArrays();

  void calcStateCov(const TVector3& pos,
                    const TVector3& mom,
                    const TVector3& poserr,
                    const TVector3& momerr);

  void calcState(const TVector3& pos,
                 const TVector3& mom);

  void getState7(M1x7& state7);
  void getState7(M1x7& state7, const TVectorD& state5, const GFDetPlane& pl, const double& spu);
  TVectorD getState5(const M1x7& state7, const GFDetPlane& pl, double& spu);

  void transformPM7(const TMatrixD& in5x5,
                    M7x7& out7x7,
                    const GFDetPlane& pl,
                    const TVectorD& state5,
                    const double& spu,
                    TMatrixD* Jac = NULL);

  void transformPM6(const TMatrixDSym& in5x5,
                    M6x6& out6x6,
                    const GFDetPlane& pl,
                    const TVectorD& state5,
                    const double& spu,
                    TMatrixD* Jac = NULL);

  void transformM7P(const M7x7& in7x7,
                    TMatrixDSym& out5x5,
                    const GFDetPlane& pl,
                    const M1x7& state7,
                    TMatrixD* Jac = NULL);

  void transformM6P(const M6x6& in6x6,
                    TMatrixDSym& out5x5,
                    const GFDetPlane& pl,
                    const M1x7& state7,
                    TMatrixD* Jac = NULL);

  //! protect from call to not yet implemented = operator
  RKTrackRep& operator=(const RKTrackRep*){
    return *this;
  };

  //! Propagates the particle through the magnetic field.
  /** If the propagation is successfull and the plane is reached, the function returns true.
    * Propagated state and the jacobian of the extrapolation are written to #state7 and #cov.
    * The jacobian is only calculated if #cov != NULL.
    * In the main loop of the Runge Kutta algorithm, the #estimateStep() is called
    * and may reduce the estimated stepsize so that a maximum momentum loss will not be exceeded.
    * If this is the case, #RKutta() will only propagate the reduced distance and then return. This is to ensure that
    * material effects, which are calculated after the propagation, are taken into account properly.
    */
  bool RKutta (const GFDetPlane& plane,
               M1x7& state7,
               M7x7* cov,
               double& coveredDistance,
               std::vector<GFPointPath>& points,
               bool& checkJacProj,
               bool onlyOneStep = false,
               double maxStep = 1.E99);

  //! The actual Runge Kutta propagation
  /** propagate #state7 with step #S. Fills #SA (Start directions derivatives dA/S).
   *  If #cov is NULL, only the state is propagated,
   *  otherwise also the 7x7 jacobian (#cov) is calculated.
   *  If #varField is false, the magnetic field will only be evaluated at the starting position.
   */
  void RKPropagate(M1x7& state7,
                   M7x7* cov,
                   M1x3& SA,
                   double S,
                   bool varField = true) const;

  double estimateStep(std::vector<GFPointPath>& points,
                      const TVector3& pos,
                      const TVector3& dir,
                      const M1x4& SU,
                      const GFDetPlane& plane,
                      const double& mom,
                      double& relMomLoss,
                      double& deltaAngle,
                      bool& momLossExceeded,
                      bool& atPlane,
                      double maxStep = 1.E99) const;

  TVector3 poca2Line(const TVector3& extr1,
                     const TVector3& extr2,
                     const TVector3& point) const;
    
  //! Handles propagation and material effects
  /** #extrapolate(), #extrapolateToPoint() and #extrapolateToLine() call this function.
    * #Extrap() needs a plane as an argument, hence #extrapolateToPoint() and #extrapolateToLine() create virtual detector planes.
    * In this function, #RKutta() is called and the resulting points and point paths are filtered
    * so that the direction doesn't change and tiny steps are filtered out.
    * After the propagation the material effects are called via the #GFMaterialEffects singleton.
    * #Extrap() will loop until the plane is reached, unless the propagation fails or the maximum number of
    * iterations is exceeded.
    * #fXX0 is also updated here.
    */
  double Extrap(const GFDetPlane& plane,
                M1x7& state7,
                M7x7* cov=NULL,
                bool onlyOneStep = false,
                double maxStep = 1.E99);
  
  
  // data members
  
  int fDirection;   // (-1, 0, 1) -> (backward prop, decide myself, forward)
  bool fNoMaterial; // don't calculate material effects if true
    
  //! PDG particle code
  int fPdg;
  //! Charge
  double fCharge;

  double fSpu;

  GFDetPlane fCachePlane; //!
  double fCacheSpu; //!
  TMatrixD fAuxInfo; //!

  // vectors for getState, transform, Extrap etc. functions. Saves a lot of TVector3 constructions/destructions
  TVector3 fPos, fDir; //!
  TVector3 fpTilde; //!
  TVector3 fDirectionBefore, fDirectionAfter; //!

  // auxiliary variables and arrays
  // needed in Extrap()
  M7x7 fNoise; //!
  M7x7 fOldCov; //!
  // needed in transform...
  M5x7 fJ_pM_5x7; //!
  M5x6 fJ_pM_5x6; //!
  M7x5 fJ_Mp_7x5; //!
  M6x5 fJ_Mp_6x5; //!


  void J_pMTxcov5xJ_pM(const M5x7& J_pM, const M5x5& cov5, M7x7& out7);
  void J_pMTxcov5xJ_pM(const M5x6& J_pM, const M5x5& cov5, M6x6& out6);

  void J_MpTxcov7xJ_Mp(const M7x5& J_Mp, const M7x7& cov7, M5x5& out5);
  void J_MpTxcov6xJ_Mp(const M6x5& J_Mp, const M6x6& cov6, M5x5& out5);

  void J_MMTxcov7xJ_MM(const M7x7& J_MM, M7x7& cov7);

  void J_MMxJ_MM(M7x7& J_MM, const M7x7& J_MM_old);

  void printDim(const double* mat, unsigned int dimX, unsigned int dimY);

  int PropagateError(TVector3 initpos, TVector3 initmom,
                     TVector3 finalpos, TVector3 finalmom,
                     double stepLengthCm, double charge);
  
  TMatrixD totalTransfMat;
  TMatrixD fError;

  static PHField3D* _field;
  static TVector3 _uniform_field_tesla;

 public:
  ClassDef(RKTrackRep, 10)
};

#endif

/** @} */