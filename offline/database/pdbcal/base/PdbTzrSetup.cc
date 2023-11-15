#include "PdbTzrSetup.hh"
#include <iostream>

PdbTzrSetup::PdbTzrSetup(){
  MinCharge = 0.0;
  MaxCharge = 0.0;
  MinTime = 0.0;
  MaxTime = 0.0;
  SigmaADC = 0.0;
  SigmaTDC = 0.0;
}
PdbTzrSetup::PdbTzrSetup(const PdbTzrSetup &rhs){}

PdbTzrSetup::~PdbTzrSetup(){}

void PdbTzrSetup::setMinCharge(float minCharge){MinCharge = minCharge;}
void PdbTzrSetup::setMaxCharge(float maxCharge){MaxCharge=maxCharge; }
void PdbTzrSetup::setMinTime(float minTime){MinTime=minTime;}
void PdbTzrSetup::setMaxTime(float maxTime){MaxTime=maxTime;}
void PdbTzrSetup::setSigmaADC(float sigmaADC){SigmaADC=sigmaADC;}
void PdbTzrSetup::setSigmaTDC(float sigmaTDC){SigmaTDC=sigmaTDC;}
void PdbTzrSetup::setOrigPcrSlat(int *origPcrSlat,int dim1,int dim2){
  for(int i=0;i<dim1;i++)
    for(int j=0;j<dim2;j++)
      OrigPcrSlat[i][j] = origPcrSlat[i*dim2+j];
}
void PdbTzrSetup::setCombPcrSlat(int combPcrSlat[]){
  for(int i=0;i<TZR_NSLT;i++)
    CombPcrSlat[i]=combPcrSlat[i];
}
float PdbTzrSetup::getMinCharge(){return MinCharge ;}
float PdbTzrSetup::getMaxCharge(){return MaxCharge ;}
float PdbTzrSetup::getMinTime(){return MinTime ;}
float PdbTzrSetup::getMaxTime(){return MaxTime ;}
float PdbTzrSetup::getSigmaADC(){return SigmaADC ;}
float PdbTzrSetup::getSigmaTDC(){return SigmaTDC ;}
int* PdbTzrSetup::getOrigPcrSlat(){return &OrigPcrSlat[0][0] ;}
int* PdbTzrSetup::getCombPcrSlat(){return CombPcrSlat ;}


void PdbTzrSetup::print() const
{
  std::cout<<MinCharge<<std::endl;
  std::cout<<MaxCharge<<std::endl;
  std::cout<<MinTime<<std::endl;
  std::cout<<MaxTime<<std::endl;
  std::cout<<SigmaADC<<std::endl;
  std::cout<<SigmaTDC<<std::endl;
  
  for(int i=0;i<TZR_NSLT;i++) std::cout<<CombPcrSlat[i]<<std::endl;

  for(int i=0;i<PCR_NSLT;i++)
   for(int j=0;j<TZR_NSLT;j++)
    std::cout<<OrigPcrSlat[i][j]<<std::endl;
                                      
}









