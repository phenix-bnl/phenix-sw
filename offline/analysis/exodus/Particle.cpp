//-----------------------------------------------------------------------------
//
//  Implementation of the classes Particle
//
//-----------------------------------------------------------------------------

#include <algorithm>
#include <cmath>
#include <iostream>
#include "Momentum.h"
#include "Particle.h"

//___________________________________________________________
Particle::Particle():
  itsType(-1),
  itsMom4(0,0,0,0),
  itsDecaysum(0),
  itsGeneration(0),
  itsWeight(0),
  itsAccept(0),
  itsValid(1)
{
  itsVertex[0]  = 0.;
  itsVertex[1]  = 0.;
  itsVertex[2]  = 0.;
}

//___________________________________________________________
Particle::Particle(const int inittype, const Mom4& initmom4, const double initdecaysum,
    const int initGeneration):
  itsType(inittype),
  itsMom4(initmom4),
  itsDecaysum(initdecaysum),
  itsGeneration(initGeneration),
  itsWeight(0),
  itsAccept(0),
  itsValid(0)
{
  std::fill_n(itsVertex, 3, 0);
}

//___________________________________________________________
Particle::Particle(const int inittype, const double initE,
    const double initpx, const double initpy, const double initpz,
    const double initdecaysum, const int initGeneration):
  itsType(inittype),
  itsMom4(initE, initpx, initpy, initpz),
  itsDecaysum(initdecaysum),
  itsGeneration(initGeneration),
  itsWeight(0),
  itsAccept(0),
  itsValid(0)
{
  std::fill_n(itsVertex, 3, 0);
}

//___________________________________________________________

void Particle::Show() const
{
  std::cout << "| PID: " << itsType << " |";
  std::cout.width(10);
  std::cout<<itsMom4.GetE()<<"|";
  std::cout.width(10);
  std::cout<<itsMom4.Getpx()<<"|";
  std::cout.width(10);
  std::cout<<itsMom4.Getpy()<<"|";
  std::cout.width(10);
  std::cout<<itsMom4.Getpz()<<"|";
  std::cout.width(4);
  std::cout<<itsDecaysum<<"|";
  std::cout.width(10);
  std::cout<<itsGeneration<<"|"<<std::endl;
}

//___________________________________________________________
double Particle::GetMass() const
{
  if (itsMom4*itsMom4<0)
  {
    std::cerr << "Error in relativistic kinematics: mass < 0" << std::endl;
    return 0.;
  }
  else
    return std::sqrt(itsMom4*itsMom4);
}

