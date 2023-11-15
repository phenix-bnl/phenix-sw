//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 2000
//
//  Implementation of class PdbPadSimGeoChamV2
//
//  Author: Debsankar Mukhopadhyay
//-----------------------------------------------------------------------------
#include "PdbPadSimGeoChamV2.hh"

#include <iostream>

PdbPadSimGeoChamV2::PdbPadSimGeoChamV2()
{
  zero();
}

PdbPadSimGeoChamV2::PdbPadSimGeoChamV2(const PdbPadSimGeoChamV2 &rhs)
{
	pc = rhs.pc;
	arm = rhs.arm;
	sect = rhs.sect;
	for(int i=0;i<3;i++){
		p0[i] = rhs.p0[i];
		p1[i] = rhs.p1[i];
		p2[i] = rhs.p2[i];
		p3[i] = rhs.p3[i];
	}
}
int  PdbPadSimGeoChamV2::get_pc(){return pc;}
int  PdbPadSimGeoChamV2::get_arm(){return arm;}
int  PdbPadSimGeoChamV2::get_sect(){return sect;}
void PdbPadSimGeoChamV2::set_pc(int ipc){pc = ipc;}
void PdbPadSimGeoChamV2::set_arm(int iarm){arm = iarm;}
void PdbPadSimGeoChamV2::set_sect(int isect){sect =isect;}
void PdbPadSimGeoChamV2::set_point1(int i, double val)
      {
              p0[i] = val;
      }
void PdbPadSimGeoChamV2::set_point2(int i,double val)
      {
	      p1[i] = val;
      }
void PdbPadSimGeoChamV2::set_point3(int i, double val)
      {
	      p2[i] = val;
      }
void PdbPadSimGeoChamV2::set_point4(int i, double val)
      {
	      p3[i] = val;
      }

double PdbPadSimGeoChamV2::get_point1(int i){return p0[i];}
double PdbPadSimGeoChamV2::get_point2(int i){return p1[i];}
double PdbPadSimGeoChamV2::get_point3(int i){return p2[i];}
double PdbPadSimGeoChamV2::get_point4(int i){return p3[i];}

void PdbPadSimGeoChamV2::zero()
{
  pc = 0;
  arm = 0;
  sect = 0;
  for(int j=0;j<3;j++){
	  p0[j] = 0;
	  p1[j] = 0;
	  p2[j] = 0;
	  p3[j] = 0;
}
}

PdbPadSimGeoChamV2::~PdbPadSimGeoChamV2()
{
}

void PdbPadSimGeoChamV2::print() const
{
 
  double  pt1[3],pt2[3],pt3[3],pt4[4];
  if(pc == 0)
    {
      std::cout<<pc<<" "<<arm<<"  "<<sect<<std::endl;
         for (int i = 0; i < 3; i++){
                 pt1[i] = p0[i];
                 pt2[i] = p1[i];
                 pt3[i] = p2[i];
                 pt4[i] = p3[i];
		 
    }
        std::cout<<pt1[0]<<" "<<pt1[1]<<" "<<pt1[2]<<std::endl;
        std::cout<<pt2[0]<<" "<<pt2[1]<<" "<<pt2[2]<<std::endl;
        std::cout<<pt3[0]<<" "<<pt3[1]<<" "<<pt3[2]<<std::endl;
        std::cout<<pt4[0]<<" "<<pt4[1]<<" "<<pt4[2]<<std::endl;
}

  if(pc == 1)
   {	
	   std::cout<<pc<<" "<<arm<<"  "<<sect<<std::endl;
            for (int i = 0; i < 3; i++){
                 pt1[i] = p0[i];
                 pt2[i] = p1[i];
                 pt3[i] = p2[i];
                 pt4[i] = p3[i];		 

	    }
        std::cout<<pt1[0]<<" "<<pt1[1]<<" "<<pt1[2]<<std::endl;
        std::cout<<pt2[0]<<" "<<pt2[1]<<" "<<pt2[2]<<std::endl;
        std::cout<<pt3[0]<<" "<<pt3[1]<<" "<<pt3[2]<<std::endl;
        std::cout<<pt4[0]<<" "<<pt4[1]<<" "<<pt4[2]<<std::endl;
}
  if(pc == 2)
   {	
	   std::cout<<pc<<"  "<<arm<<"  "<<sect<<std::endl;
            for (int i = 0; i < 3; i++){
                 pt1[i] = p0[i];
                 pt2[i] = p1[i];
                 pt3[i] = p2[i];
                 pt4[i] = p3[i];		 

	    }
        std::cout<<pt1[0]<<" "<<pt1[1]<<" "<<pt1[2]<<std::endl;
        std::cout<<pt2[0]<<" "<<pt2[1]<<" "<<pt2[2]<<std::endl;
        std::cout<<pt3[0]<<" "<<pt3[1]<<" "<<pt3[2]<<std::endl;
        std::cout<<pt4[0]<<" "<<pt4[1]<<" "<<pt4[2]<<std::endl;
}
}









