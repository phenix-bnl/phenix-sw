#include "PdbTzrFemMap.hh"
#include <iostream>

PdbTzrFemMap::PdbTzrFemMap(){
}
PdbTzrFemMap::PdbTzrFemMap(const PdbTzrFemMap &rhs){}

PdbTzrFemMap::~PdbTzrFemMap(){}

void PdbTzrFemMap::setSlatId(int slatId[]){
  for(int i=0;i<TOT_NSLT;i++)
    SlatId[i]=slatId[i];
}
void PdbTzrFemMap::setFemId(int *femId,int dim1,int dim2){
  for(int i=0;i<dim1;i++)
    for(int j=0;j<dim2;j++)
      FemId[i][j] = femId[i*dim2+j];
}

int* PdbTzrFemMap::getSlatId(){return SlatId ;}
int* PdbTzrFemMap::getFemId(){return &FemId[0][0] ;}

void PdbTzrFemMap::print() const
{
for(int i=0;i<TOT_NSLT;i++)std::cout<<SlatId[i]<<std::endl;

for(int i=0;i<TOT_NSLT;i++)
  {
    for(int j=0;j<2;j++)
     {
      std::cout<<FemId[i][j]<<std::endl;
      }
   }
}









