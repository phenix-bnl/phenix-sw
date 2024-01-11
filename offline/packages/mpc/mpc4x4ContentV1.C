#include "mpc4x4ContentV1.h"


ClassImp(mpc4x4ContentV1)

using namespace std;

mpc4x4ContentV1::mpc4x4ContentV1()
{
  esum2    = -9999.;
  esum     = -9999.;
  x4id    = -9999;
  for(int itr=0;itr<4;++itr)
    {
      evals[itr] = -9999;      
      evals2[itr] = -9999;
      chvals[itr] = -9999;
    }
}


mpc4x4ContentV1::mpc4x4ContentV1(short x4val)
{
  esum     = -9999.;
  esum2     = -9999.;
  x4id    =  x4val;
  for(int itr=0;itr<4;++itr)
    {
      evals[itr] = -9999;
      evals2[itr] = -9999;
      chvals[itr] = -9999;
    }
}

mpc4x4ContentV1::mpc4x4ContentV1(mpc4x4Content &rhs)
{
  this->esum = rhs.get_esum();
  this->esum2 = rhs.get_esum2();
  this->x4id = rhs.get_4x4id();
  for(int i=0;i<4;++i)
    {
      this->evals[i] = rhs.get_e(i);
      this->evals2[i] = rhs.get_e2(i);
      this->chvals[i] = rhs.get_2x2(i);
    }
  
}

mpc4x4Content& mpc4x4ContentV1::operator=(const mpc4x4Content &rhs)
{
  if ( ((mpc4x4Content*)this) == &rhs ) return *this;
  
  this->esum = rhs.get_esum();
  this->esum2 = rhs.get_esum2();
  this->x4id = rhs.get_4x4id();
  for(int i=0;i<4;++i)
    {
      this->evals[i] = rhs.get_e(i);
      this->evals2[i] = rhs.get_e2(i);
      this->chvals[i] = rhs.get_2x2(i);
    }
  
  return *this;
}


short mpc4x4ContentV1::get_2x2(int index) const
{
  if(index < 0 || index > 3) return -9999;  
  return chvals[index];
} 

float mpc4x4ContentV1::get_e(int index) const
{
  if(index < 0 || index > 3) return -9999;
  return evals[index];
  
}

float mpc4x4ContentV1::get_e2(int index) const
{
  if(index < 0 || index > 3) return -9999;
  return evals2[index];
  
}

int mpc4x4ContentV1::set_2x2(int index, short ch)
{
  if(index < 0 || index > 3) return 0;
  chvals[index] = ch;
  return 1;
}

int mpc4x4ContentV1::set_e(int index, float eval)
{
  if(index < 0 || index > 3) return 0;
  evals[index] = eval;
  return 1;
}


int mpc4x4ContentV1::set_e2(int index, float eval)
{
  if(index < 0 || index > 3) return 0;
  evals2[index] = eval;
  return 1;
}

int mpc4x4ContentV1::set_next_2x2(short ch)
{
  
  int index = 0;;
  for(int itr = 0;itr<4;++itr)
    {
      if(chvals[itr] < 0) break;
      ++index;
    }
  
  if(index < 0 || index > 3) return 0;
  chvals[index] = ch;
  return 1;
}

int mpc4x4ContentV1::set_next_e(float eval)
{
  int index = 0;;
  for(int itr = 0;itr<4;++itr)
    {
      if(evals[itr] < -10) break;
      ++index;
    }
  
  if(index < 0 || index > 3) return 0;
  evals[index] = eval;
  return 1;
}


int mpc4x4ContentV1::set_next_e2(float eval)
{
  int index = 0;;
  for(int itr = 0;itr<4;++itr)
    {
      if(evals2[itr] < -10) break;
      ++index;
    }
  
  if(index < 0 || index > 3) return 0;
  evals2[index] = eval;
  return 1;
}



int mpc4x4ContentV1::get_n2x2()
{
  int ntow = 0;
  for(int itr=0;itr<4;++itr)
    {
      if(evals[itr] < -10 || chvals[itr] < 0) break;
      ++ntow;
    }
  return ntow;
  
}
      
void mpc4x4ContentV1::calc_esum()
{
  esum = 0;
  for(int itr=0;itr<4;++itr)
    {
      if(evals[itr] < -10 || chvals[itr] < 0) continue;
      esum+=evals[itr];
    }
  return;
}


void mpc4x4ContentV1::calc_esum2()
{
  esum2 = 0;
  for(int itr=0;itr<4;++itr)
    {
      if(evals2[itr] < -10 || chvals[itr] < 0) continue;
      esum2+=evals2[itr];
    }
  return;
}


void mpc4x4ContentV1::print(std::ostream& out) const
{
  out << "4x4id: " << x4id  << endl;
  out << "esum\tesum2: " << esum << "\t" << esum2 << endl;
  out << "ch\te\te2" << endl;
  for(int itr=0;itr<4;++itr){
    out << chvals[itr] << "\t" << evals[itr] << "\t" << evals2[itr] << "\n";
  }
  out << endl;
}
