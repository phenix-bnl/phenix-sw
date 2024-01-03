//-----------------------------------------------------------------------------
//
//  Read particle properties from file
//
//-----------------------------------------------------------------------------

#include "DefineParticleProperties.h"
#include <fstream>
#include <iostream>
#include <string>
#include "Particle.h"
#include "ParticleProperty.h"
#include "ParticlePropertyList.h"
#include "Tools.h"

ParticlePropertyList* DefineParticleProperties()
{
  double mass=0;
  double width=0;
  int    id=0;
  int    charge=0;
  double spin;
  char   ch;
  std::string label, name;

  ParticlePropertyList * DefinedParticles = new ParticlePropertyList;

  std::string data_file_name = getDataFileName("defined_particles.txt");
  std::cout << "Reading particle definitions from file: " << data_file_name
    << std::endl;

  std::ifstream fin(data_file_name.c_str());
  while ( fin.get(ch) )
  {
    fin >> label;
    if ( label=="@{" )
    {
      fin >> name;
      while (true)
      {
        fin >> label;
        if ( label=="ID=" )     fin >> id;
        if ( label=="Mass=" )   fin >> mass;
        if ( label=="Width=" )  fin >> width;
        if ( label=="Charge=" ) fin >> charge;
        if ( label=="Spin=" )   fin >> spin;
        if ( label=="}" )       break;
      }
    }
    else continue;
    ParticleProperty * pPart = new ParticleProperty;
    pPart->SetName(name);
    pPart->SetID(id);
    pPart->SetMass(mass);
    pPart->SetWidth(width);
    pPart->SetCharge(charge);
    DefinedParticles->Insert(pPart);
    std::cout << "Defined: " << name << std::endl;
  }
  fin.close();

  std::cout << std::endl;

  return DefinedParticles;
}
