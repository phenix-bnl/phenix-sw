/****************************************************************************
 ****************************************************************************

 Accept_particle
 ---------------

DESCRIPTION: For given three momentum px,py,pz and charge of a particle
determine whether it will be accepted or not.  Return value is 0,1,2,3,4
corresponding to not accepted, accepted by one of the four half arm.

CAUTION: some particle may go pass two half arms so are discarded by this filter,
I will rewrite this filter so that the return value will tell the sector number
the half arm number, the arm number by which the particle is accepted.

ALGORITHM: For px,py,pz, first we change them into sphere varables
p,theta,phi. Then we see if p theta phi pass our filter. Filter for p and
theta are direct. Filter for phi is eight  simple function line1 - line8.
they behave as exponential function at low momentum, and p goes to infinity
when phi approching some value.

AUTHOR/CONTACT: J. Jiangyong,Stephe Johnson, StonyBrook
jjia@skipper.physics.sunysb.edu
snoopy@skipper.physics.sunysb.edu

REVISIONS:
Date            Author          Description

7/17/98         J. Jiangyong     Original

INPUT VARIABLES: px = x component of the momentum at the vertex
py = y component of the momentum at the vertex
pz = z component of the momentum at the vertex
charge = the number of charge of the particle, 1 for positron
and -1 for electron.

OUTPUT VARIABLES: px,py,pz
we use reference for px,py,pz, they will store the particle momentum components leaving the detector.
 ***************************************************************************
 ***************************************************************************/

#include <gsl/gsl_math.h>
#include <cmath>
#include <iostream>
#include <limits>
#include <string>
#include "InPHENIXAcceptance.h"
#include "Momentum.h"

#define MOM_CUT     .2
#define JMASS	3.07
#define EMASS   .000511
#define PHIMASS 1.02
#define KMASS   .49365
#define OMASS   .782
#define MODE   0
#if MODE

#define PAI     3.1415926
#define DPAI    6.2831853
#define THETA_TOP 1.9198621
#define THETA_BOT 1.2217305
#define PHI_TOP 2.6179939
#define PHI_BOT .87266463

#else
#define PAI  180
#define DPAI 360
#define THETA_TOP 110
#define THETA_BOT 70
#define PHI_TOP 150
#define PHI_BOT 50

#endif
#define NUMBER     1000000

struct CURPAR
{
  public:
    double p0;
    double p1;
    double p2;
};

struct Particletype
{
  public:
    double m;
    double p;
    double theta;
    double phi;
    int   charge;
};

struct ACCEPT {
  int arm;
  int sector;
};
ACCEPT  accept_particle(const Particletype&);
ACCEPT  accept_particle2(const Particletype&);

/***********************************************************************/
/******************* description of acceptance filter for phi************/
/************************************************************************
  We use eight curve to define our filter, they will take curve parameters
  from Struct array curpar.
  for angles we use: MODE=1  radian; MODE=0   degree
 ***********************************************************************/
#if MODE
static CURPAR curpar[20]={
  {  2.864789,       -.58904862,        .14},\
  {  2.4064227,      -.19634954,        .15},\
  {  2.4064227,       .19634954,        .16},\
  {  2.4064227,       .58904862,        .17},\
  {  2.6356059,       .9817477,         .18},\
  {  2.864789,       2.1598449,         .14},\
  {  2.4064227,      2.552544,          .15},\
  {  2.4064227,      2.9452431,         .16},\
  {  2.4064227,      3.3379422,         .17},\
  {  2.6356059,      3.7306413,         .18},\
  { -2.6356059,      -.58904862,        .14},\
  { -2.4064227,      -.19634954,        .15},\
  { -2.4064227,       .19634954,        .16},\
  { -2.4064227,       .58904862,        .17},\
  { -2.864789,        .9817477,         .18},\
  { -2.6356059,      2.1598449,         .14},\
  { -2.4064227,      2.552544,          .15},\
  { -2.4064227,      2.9452431,         .16},\
  { -2.4064227,      3.3379422,         .17},\
  { -2.864789,       3.7306413,         .18},\
};
#else
static CURPAR curpar[20]={
  {  .050,    -33.75,      .14},\
  {  .042,    -11.25,      .15},\
  {  .042,     11.25,      .16},\
  {  .042,     33.75,      .17},\
  {  .046,     56.25,      .18},\
  {  .050,     123.75,     .14},\
  {  .042,     146.25,     .15},\
  {  .042,     168.75,     .16},\
  {  .042,     191.25,     .17},\
  {  .046,     213.75,     .18},\
  { -.046,    -33.75,      .14},\
  { -.042,    -11.25,      .15},\
  { -.042,     11.25,      .16},\
  { -.042,     33.75,      .17},\
  { -.050,     56.25,      .18},\
  { -.046,     123.75,     .14},\
  { -.042,     146.25,     .15},\
  { -.042,     168.75,     .16},\
  { -.042,     191.25,     .17},\
  { -.050,     213.75,     .18},\
};
#endif

static double line1(const double &phi)
{
  if(phi>=curpar[0].p1) return NUMBER;
  return .5*(1/(1-std::exp((curpar[0].p0)*(phi-curpar[0].p1)))-1)+curpar[0].p2;
}
static double line2(const double &phi)
{
  if(phi>=curpar[1].p1) return NUMBER;
  return .5*(1/(1-std::exp((curpar[1].p0)*(phi-curpar[1].p1)))-1)+curpar[1].p2;
}
static double line3(const double &phi)
{
  if(phi>=curpar[2].p1) return NUMBER;
  return .5*(1/(1-std::exp((curpar[2].p0)*(phi-curpar[2].p1)))-1)+curpar[2].p2;
}
static double line4(const double &phi)
{
  if(phi>=curpar[3].p1) return NUMBER;
  return .5*(1/(1-std::exp((curpar[3].p0)*(phi-curpar[3].p1)))-1)+curpar[3].p2;
}
static double line5(const double &phi)
{
  if(phi>=curpar[4].p1) return NUMBER;
  return .5*(1/(1-std::exp((curpar[4].p0)*(phi-curpar[4].p1)))-1)+curpar[4].p2;
}
static double line6(const double &phi)
{
  if(phi>=curpar[5].p1) return NUMBER;
  return .5*(1/(1-std::exp((curpar[5].p0)*(phi-curpar[5].p1)))-1)+curpar[5].p2;
}
static double line7(const double &phi)
{
  if(phi>=curpar[6].p1) return NUMBER;
  return .5*(1/(1-std::exp((curpar[6].p0)*(phi-curpar[6].p1)))-1)+curpar[6].p2;
}
static double line8(const double &phi)
{
  if(phi>=curpar[7].p1) return NUMBER;
  return .5*(1/(1-std::exp((curpar[7].p0)*(phi-curpar[7].p1)))-1)+curpar[7].p2;
}
static double line9(const double &phi)
{
  if(phi>=curpar[8].p1) return NUMBER;
  return .5*(1/(1-std::exp((curpar[8].p0)*(phi-curpar[8].p1)))-1)+curpar[8].p2;
}
static double line10(const double &phi)
{
  if(phi>=curpar[9].p1) return NUMBER;
  return .5*(1/(1-std::exp((curpar[9].p0)*(phi-curpar[9].p1)))-1)+curpar[9].p2;
}

static double line11(const double &phi)
{
  if(phi<=curpar[10].p1) return -NUMBER;
  return  -.5*(1/(1-std::exp((curpar[10].p0)*(phi-curpar[10].p1)))-1)-curpar[10].p2;
}
static double line12(const double &phi)
{
  if(phi<=curpar[11].p1) return -NUMBER;
  return -.5*(1/(1-std::exp((curpar[11].p0)*(phi-curpar[11].p1)))-1)-curpar[11].p2;
}
static double line13(const double &phi)
{
  if(phi<=curpar[12].p1) return -NUMBER;
  return -.5*(1/(1-std::exp((curpar[12].p0)*(phi-curpar[12].p1)))-1)-curpar[12].p2;
}
static double line14(const double &phi)
{
  if(phi<=curpar[13].p1) return -NUMBER;
  return -.5*(1/(1-std::exp((curpar[13].p0)*(phi-curpar[13].p1)))-1)-curpar[13].p2;
}
static double line15(const double &phi)
{
  if(phi<=curpar[14].p1) return -NUMBER;
  return -.5*(1/(1-std::exp((curpar[14].p0)*(phi-curpar[14].p1)))-1)-curpar[14].p2;
}
static double line16(const double &phi)
{
  if(phi<=curpar[15].p1) return -NUMBER;
  return -.5*(1/(1-std::exp((curpar[15].p0)*(phi-curpar[15].p1)))-1)-curpar[15].p2;
}
static double line17(const double &phi)
{
  if(phi<=curpar[16].p1) return -NUMBER;
  return -.5*(1/(1-std::exp((curpar[16].p0)*(phi-curpar[16].p1)))-1)-curpar[16].p2;
}
static double line18(const double &phi)
{
  if(phi<=curpar[17].p1) return -NUMBER;
  return  -.5*(1/(1-std::exp((curpar[17].p0)*(phi-curpar[17].p1)))-1)-curpar[17].p2;
}
static double line19(const double &phi)
{
  if(phi<=curpar[18].p1) return -NUMBER;
  return -.5*(1/(1-std::exp((curpar[18].p0)*(phi-curpar[18].p1)))-1)-curpar[18].p2;
}
static double line20(const double &phi)
{
  if(phi<=curpar[19].p1) return -NUMBER;
  return  -.5*(1/(1-std::exp((curpar[19].p0)*(phi-curpar[19].p1)))-1)-curpar[19].p2;
}
/*************** defination of phi & momentum filter end here***************/

ACCEPT accept_particle2(const Particletype& x)
{
  ACCEPT Revalue={-1,-1};
  double mom=x.p,theta=x.theta,phi=x.phi,charge=x.charge;
  double eta = 0.0;

  // define function array: left and right,
  static double (*cur[20])(const double&)={line1,line2,line3,line4,line5,line6,line7,line8,line9,line10,line11,line12,line13,line14,line15,line16,line17,line18,line19,line20};
  // static double (*right[8])(const double&)={line2,line4,line6,line8,line10,line12,line14,line16};

  // check for muon arm acceptance first
  // return sector==10 for south muon arm
  // return sector==20 for north muon arm

  if ( MODE ) {
    eta = -1.0*std::log(std::tan(theta/2.0));
  } else {
    eta = -1.0*std::log(std::tan(M_PI*theta/360.0));
  }

  if ( eta>=-1.15 && eta<=-2.25 ) {
    Revalue.sector = 10;
    return Revalue;
  }
  if ( eta>=1.15 && eta<=2.44 ) {
    Revalue.sector = 20;
    return Revalue;
  }

  //deal with neutral particle.
  if(charge < std::numeric_limits<double>::epsilon())
  {
    if(mom<0)mom=-mom;
    if(mom<MOM_CUT) return Revalue;//momentum cut
    if(theta>THETA_TOP||theta<THETA_BOT) return Revalue;//theta cut
    if(phi<-PHI_BOT)phi+=DPAI;
    if(phi>curpar[0].p1&&phi<curpar[4].p1)
    {
      Revalue.arm=1;
      if(phi<curpar[1].p1)Revalue.sector=4;
      else if(phi<curpar[2].p1)Revalue.sector=5;
      else if(phi<curpar[3].p1)Revalue.sector=6;
      else Revalue.sector=7;
    }
    else if(phi>curpar[5].p1&&phi<curpar[9].p1)
    {
      Revalue.arm=0;
      if(phi<curpar[6].p1) Revalue.sector=0;
      else if(phi<curpar[7].p1)Revalue.sector=1;
      else if(phi<curpar[8].p1)Revalue.sector=2;
      else Revalue.sector=3;
    }
    return Revalue;
  }
  /*now when |charge|>1 we need to adjust mom, so we can still use the same filter**/
  mom/=charge;
  if(mom<MOM_CUT&&mom>-MOM_CUT)return Revalue;//momentum cut
  if(theta>THETA_TOP||theta<THETA_BOT)return Revalue;//theta cut

  //phi and momentum filter
  if(charge>0)
  {
    if(mom<0)mom=-mom;
    if(phi<-PHI_TOP)phi+=DPAI;
    if(mom>cur[4](phi)&&mom<cur[0](phi))
    {
      Revalue.arm=1;
      if(mom>cur[1](phi)) Revalue.sector=4;
      else if(mom>cur[2](phi)) Revalue.sector=5;
      else if(mom>cur[3](phi)) Revalue.sector=6;
      else Revalue.sector=7;
    }
    else if(mom>cur[9](phi)&&mom<cur[5](phi))
    {
      Revalue.arm=0;
      if(mom>cur[6](phi)) Revalue.sector=0;
      else if(mom>cur[7](phi)) Revalue.sector=1;
      else if(mom>cur[8](phi)) Revalue.sector=2;
      else Revalue.sector=3;
    }
    return Revalue;
  }
  else if(charge<0)
  {
    if(mom>0)mom=-mom;
    if(phi<-PHI_BOT)phi+=DPAI;
    if(mom>cur[14](phi)&&mom<cur[10](phi))
    {
      Revalue.arm=1;
      if(mom>cur[11](phi)) Revalue.sector=4;
      else if(mom>cur[12](phi)) Revalue.sector=5;
      else if(mom>cur[13](phi)) Revalue.sector=6;
      else Revalue.sector=7;
    }
    if(mom>cur[19](phi)&&mom<cur[15](phi))
    {
      Revalue.arm=0;
      if(mom>cur[16](phi)) Revalue.sector=0;
      else if(mom>cur[17](phi)) Revalue.sector=1;
      else if(mom>cur[18](phi)) Revalue.sector=2;
      else Revalue.sector=3;
    }
    return Revalue;
  }

  std::cout << "this line should never be reached!" << std::endl;
  return Revalue;

}

double arg(double x, double y)	{
  double u=180./3.1415926535897932384626;
  if (( x < std::numeric_limits<double>::epsilon() ) && (y < std::numeric_limits<double>::epsilon() )) return 0;
  if (( x < std::numeric_limits<double>::epsilon() ) && (y>0)) return 90;
  if (( x < std::numeric_limits<double>::epsilon() ) && (y<0)) return 270;
  if (x>0) {
    if (y>=0) return u*std::atan(y/x);
    return 360-u*std::atan(-y/x);
  }
  if (x<0) {
    if (y>=0) return 180-u*std::atan(y/-x);
    return 180+u*std::atan(y/x);
  }
  std::cout<<"ERROR IN arg()";
  return -1;
}

int InPHENIXAcceptance(double p, double theta, double phi,
    int charge, double m=0)
{
  Particletype teil;
  teil.m=m;
  teil.p=p;
  teil.theta=theta;
  teil.phi=phi-90.;
  teil.charge=charge;
  return accept_particle2(teil).sector;
}
