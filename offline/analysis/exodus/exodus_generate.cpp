//-----------------------------------------------------------------------------
//
//  First-generation generator of the EXODUS package
//
//  Main program
//
//-----------------------------------------------------------------------------

#include <Rtypes.h>
#include <TROOT.h>
#include <boost/lexical_cast.hpp>
#include <iostream>
#include <string>
#include "CleanupRandom.h"
#include "DefineParticleProperties.h"
#include "GenerateParticles.h"
#include "GenerateSingleParticles.h"
#include "GenerateSingleParticlesEMCal.h"
#include "InitializeRandom.h"
#include "InitializeSetup.h"
#include "ParticleList.h"
#include "WriteParticleListToFile.h"
#include "WriteParticleListToOscarFile.h"

class ParticleGeneratorList;
class ParticlePropertyList;

int main() try
{
  gROOT->SetBatch(kTRUE);

  std::cout << std::endl << std::endl;
  std::cout << "**********************************" << std::endl;
  std::cout << "*                                *" << std::endl;
  std::cout << "*  W E L C O M E to E X O D U S  *" << std::endl;
  std::cout << "*                                *" << std::endl;
  std::cout << "*        FIRST GENERATION        *" << std::endl;
  std::cout << "*                                *" << std::endl;
  std::cout << "*           GENERATOR            *" << std::endl;
  std::cout << "*                                *" << std::endl;
  std::cout << "**********************************" << std::endl;
  std::cout << std::endl << std::endl;

  std::string line;
  int setup;
  do
  {
    std::cout << "Choose one of the predefined setups:" << std::endl;
    std::cout << "------------------------------------" << std::endl;
    std::cout << std::endl;
    std::cout << "1) CERES electron generator" << std::endl;
    std::cout << "2) ISR electron generator" << std::endl;
    std::cout << "3) PHENIX electron generator" << std::endl;
    std::cout << "4) Phi->KK" << std::endl;
    std::cout << "5) Single-particle generator" << std::endl;
    std::cout << "6) Single-particle generator (EMCal)" << std::endl;
    std::cout << std::endl;
    std::cout << "Your choice (1-6): ";
    std::getline(std::cin, line);
    setup = boost::lexical_cast<int>(line);
    std::cout << std::endl;
  } while ( setup<1 || setup>6 );

  std::cout << "How many events? ";
  std::getline(std::cin, line);
  const int events = boost::lexical_cast<int>(line);
  std::cout << std::endl;

  std::string output_file, oscar_file;
  if ( setup!=5 && setup!=6 )
  {
    std::cout << "Output file containing first-generation particles: ";
    std::getline(std::cin, output_file);
    std::cout << std::endl;
  }
  else
  {
    std::cout << "Output file containing first-generation particles (OSCAR compliant): ";
    std::getline(std::cin, oscar_file);
    std::cout << std::endl;
  }

  TROOT exodus("exodus","Initialize ROOT for exodus");
  InitializeRandom();

  ParticlePropertyList  * Species       = DefineParticleProperties();
  ParticleGeneratorList * GeneratorList = InitializeSetup(setup, *Species);
  //DecayList             * Decays        = DefineDecayProperties(*Species);

  ParticleList* FirstGeneration = new ParticleList;
  if ( setup<5  )
  {
    GenerateParticles(FirstGeneration,events,GeneratorList,Species);
  }
  else if ( setup==5 )
  {
    GenerateSingleParticles(FirstGeneration,events, *Species);
    //AdjustDecaySum(*FirstGeneration, *Decays);
    //DoAllDecays(*FirstGeneration, *Species, *Decays);
  }
  else
  {
    GenerateSingleParticlesEMCal(FirstGeneration,events,Species);
    //AdjustDecaySum(*FirstGeneration, *Decays);
    //DoAllDecays(*FirstGeneration, *Species, *Decays);
  }

  std::cout << "Length of first-generation particle list: "
    << FirstGeneration->GetLength() << " particles" << std::endl
    << std::endl;

  if ( setup<5 )  WriteParticleListToFile(output_file.c_str(), *FirstGeneration);

  if ( setup ==5 || setup==6 )  WriteParticleListToOscarFile(oscar_file.c_str(), *FirstGeneration, *Species);
  //if ( setup ==5 || setup==6 )  WriteParticleListToOscarFile_decay(oscar_file.c_str(), *FirstGeneration, *Species);

  CleanupRandom();

  return 0;
} catch (const std::exception& e) {
  std::cerr << e.what() << std::endl;
  return EXIT_FAILURE;
}
