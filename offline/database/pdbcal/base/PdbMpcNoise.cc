#include <PdbMpcNoise.hh>

//#include <iostream>

using namespace std;

PdbMpcNoise::PdbMpcNoise()
{
  Reset();
}

void PdbMpcNoise::print() const
{
  cout << fee576 << "\t"
       << N_thr[0] << "\t"
       << N_thr[1] << "\t"
       << N_thr[2] << "\t"
       << N_thr[3] << endl;
}

void PdbMpcNoise::Reset()
{
  fee576 = -9999;
  for(int ithr=0;ithr<4;ithr++)
    {
      N_thr[ithr]= -9999;
    }
}
