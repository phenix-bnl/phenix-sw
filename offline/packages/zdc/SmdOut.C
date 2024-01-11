#include "SmdOut.h"

ClassImp(SmdOut);

using namespace std;

SmdOut::SmdOut()
{
}

void SmdOut::identify(std::ostream& os) const 
{
  os << "virtual SmdOut object" << std::endl;
  return;
}

void SmdOut::Reset() 
{
  std::cout << PHWHERE 
            << "ERROR: Reset()not implemented by daughter class" 
            << std::endl;
  return;
}

int SmdOut::isValid() const 
{
  std::cout << PHWHERE 
            << "isValid() not implemented by daughter class" 
            << std::endl;
  return 0;
}

int SmdOut::AddSmdHit(const float charge, const float time0, const float time1, const int islat)
{
  std::cout << PHWHERE
            << "ERROR: THIS IS A VIRTUAL FUNCTION WITH NO EFFECT"
            << std::endl;
  return INVALID_SHORT;
}
float SmdOut::get_Charge(const int iPmt) const {return INVALID_FLOAT;}
float SmdOut::get_Time0(const int iPmt)  const {return INVALID_FLOAT;}
float SmdOut::get_Time1(const int iPmt)  const {return INVALID_FLOAT;}


void SmdOut::AddSmdNS(const float xpos, const float ypos, const float e, const int arm)
{
  std::cout << PHWHERE
            << "ERROR: THIS IS A VIRTUAL FUNCTION WITH NO EFFECT"
            << std::endl;
  return;
}

float SmdOut::get_Xpos(const int arm) const {return INVALID_FLOAT;}
float SmdOut::get_Ypos(const int arm) const {return INVALID_FLOAT;}
float SmdOut::get_Energy(const int arm) const {return INVALID_FLOAT;}

