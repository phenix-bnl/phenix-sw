//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Implementation of class PdbFclGain
//
//  Author: mheffner
//-----------------------------------------------------------------------------
#include "PdbFclGain.hh"
#include <string.h>
#include <iostream>

PdbFclGain::PdbFclGain()
{
  memset(northGain, 0, 10*9*sizeof(double));
  memset(southGain, 0, 10*9*sizeof(double));
  memset(northGainError, 0, 10*9*sizeof(double));
  memset(southGainError, 0, 10*9*sizeof(double));
}

PdbFclGain::~PdbFclGain()
{
}


PdbFclGain& PdbFclGain::operator=(const  PdbFclGain&p)
{
  for(int row = 0; row<10; row++){
    for(int col = 0; col<9; col++){
      northGain[row][col] = p.northGain[row][col];
      southGain[row][col] = p.southGain[row][col];
      northGainError[row][col] = p.northGainError[row][col];
      southGainError[row][col] = p.southGainError[row][col];
    }
  }
  return *this;
}

int PdbFclGain::setNorthGain(const int row,
			     const int col,
			     const double value,
			     const double error)
{
  if (row<10  && col<9)
    {
      northGain[row][col]=value;
      northGainError[row][col]=error;
      return 0;
    }else{
      return -1;
    }
}

int PdbFclGain::setSouthGain(const int row,
			     const int col,
			     const double value,
			     const double error)
{
  if (row<10  && col<9)
    {
      southGain[row][col]=value;
      southGainError[row][col]=error;
      return 0;
    }else{
      return -1;
    }
}

double PdbFclGain::getNorthGain(const int row,
				const int col) const
{
  if (row<10  && col<9)
    {
      return northGain[row][col];
    }else{
      return -1;
    }
}

double PdbFclGain::getSouthGain(const int row,
				const int col) const
{
  if (row<10  && col<9)
    {
      return southGain[row][col];
    }else{
      return -1;
    }
}

double PdbFclGain::getNorthGainError(const int row,
				     const int col) const
{
  if (row<10  && col<9)
    {
      return northGainError[row][col];
    }else{
      return -1;
    }
}

double PdbFclGain::getSouthGainError(const int row,
				     const int col) const
{
  if (row<10  && col<9)
    {
      return southGainError[row][col];
    }else{
      return -1;
    }
}



void PdbFclGain::print() const
{
  std::cout<<"Fcal Gain tables"<<std::endl;
  std::cout<<"North"<<std::endl;
  for(int row=0;row<10;row++){
    for(int col=0;col<9;col++){
      std::cout<<northGain[row][col]<<" : ";
    }
    std::cout<<std::endl;
  }

  std::cout<<std::endl;
  std::cout<<"South"<<std::endl;
  for(int row=0;row<10;row++){
    for(int col=0;col<9;col++){
      std::cout<<southGain[row][col]<<" : ";
    }
    std::cout<<std::endl;
  }
  
}
