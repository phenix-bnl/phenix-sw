#include "PdbTzrCalib.hh"
#include <iostream>

PdbTzrCalib::PdbTzrCalib(){
}
PdbTzrCalib::PdbTzrCalib(const PdbTzrCalib &rhs){}

PdbTzrCalib::~PdbTzrCalib(){}

void PdbTzrCalib::setPede(float *pede,int dim1,int dim2){
  for(int i=0;i<dim1;i++)
    for(int j=0;j<dim2;j++)
      Pede[i][j] = pede[i*dim2+j];
}
void PdbTzrCalib::setGain(float *gain,int dim1,int dim2){
  for(int i=0;i<dim1;i++)
    for(int j=0;j<dim2;j++)
      Gain[i][j] = gain[i*dim2+j];
}
void PdbTzrCalib::setTofs(float *tofs,int dim1,int dim2){
  for(int i=0;i<dim1;i++)
    for(int j=0;j<dim2;j++)
      Tofs[i][j] = tofs[i*dim2+j];
}
void PdbTzrCalib::setTslw(float *tslw,int dim1,int dim2){
  for(int i=0;i<dim1;i++)
    for(int j=0;j<dim2;j++)
      Tslw[i][j] = tslw[i*dim2+j];
}
void PdbTzrCalib::setTgai(float *tgai,int dim1,int dim2){
  for(int i=0;i<dim1;i++)
    for(int j=0;j<dim2;j++)
      Tgai[i][j] = tgai[i*dim2+j];
}

void PdbTzrCalib::setToft(float toft[]){
  for(int i=0;i<TOT_NSLT;i++)
    Toft[i]=toft[i];
}
void PdbTzrCalib::setYoft(float yoft[]){
  for(int i=0;i<TOT_NSLT;i++)
    Yoft[i]=yoft[i];
}
void PdbTzrCalib::setYgai(float ygai[]){
  for(int i=0;i<TOT_NSLT;i++)
    Ygai[i]=ygai[i];
}

float* PdbTzrCalib::getPede(){return &Pede[0][0] ;}
float* PdbTzrCalib::getGain(){return &Gain[0][0] ;}
float* PdbTzrCalib::getTofs(){return &Tofs[0][0] ;}
float* PdbTzrCalib::getTslw(){return &Tslw[0][0] ;}
float* PdbTzrCalib::getTgai(){return &Tgai[0][0] ;}

float* PdbTzrCalib::getToft(){return Toft ;}
float* PdbTzrCalib::getYoft(){return Yoft ;}
float* PdbTzrCalib::getYgai(){return Ygai ;}

void PdbTzrCalib::print() const
{
for(int i=0;i<TOT_NSLT;i++)std::cout<<Toft[i]<<std::endl;
for(int i=0;i<TOT_NSLT;i++)
 {
  for(int j=0;j<2;j++)
   {
    std::cout<<Pede[i][j]<<std::endl;
   }
 }
}













