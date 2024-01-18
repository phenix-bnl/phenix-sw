#include "PHG3toG4GeneralTrigger.h"


using namespace std;

//__________________________________________________________
PHG3toG4GeneralTrigger::PHG3toG4GeneralTrigger(const std::string &name):
_name(name)
{}

//__________________________________________________________
PHG3toG4GeneralTrigger::~PHG3toG4GeneralTrigger()
{}

std::vector<int> PHG3toG4GeneralTrigger::convertToInts(std::string s)
{
  
  vector<int> theVec;
  stringstream ss(s);
  int i;  
  while (ss >> i)
    {
      theVec.push_back(i);
      if (ss.peek() == ',' || ss.peek() == ' ' || ss.peek() == ':' || ss.peek() == ';') ss.ignore();
    }
  return theVec;


}

void PHG3toG4GeneralTrigger::AddParticles(std::string particles)
{
  std::vector<int> addedParts = convertToInts(particles);
  _theParticles.insert(_theParticles.end(),addedParts.begin(),addedParts.end());
}

void PHG3toG4GeneralTrigger::AddParticles(int particle)
{
  _theParticles.push_back(particle);
}

void PHG3toG4GeneralTrigger::AddParticles(std::vector<int> particles)
{
  _theParticles.insert(_theParticles.end(),particles.begin(),particles.end());
}

void PHG3toG4GeneralTrigger::AddParents(std::string parents)
{
  std::vector<int> addedParents = convertToInts(parents);
  _theParents.insert(_theParents.end(),addedParents.begin(),addedParents.end());
}

void PHG3toG4GeneralTrigger::AddParents(int parent)
{
  _theParents.push_back(parent);
}

void PHG3toG4GeneralTrigger::AddParents(std::vector<int> parents)
{
  _theParents.insert(_theParents.end(),parents.begin(),parents.end());
}
