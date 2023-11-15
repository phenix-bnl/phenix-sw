//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 2000
//
//  Implementation of class PdbPadSimGeoChamV1
//
//  Author: Indrani Ojha
//-----------------------------------------------------------------------------
#include "PdbPadSimGeoChamV1.hh"

#include <iostream>

PdbPadSimGeoChamV1::PdbPadSimGeoChamV1()
{
  zero();
}

PdbPadSimGeoChamV1::PdbPadSimGeoChamV1(const PdbPadSimGeoChamV1 &rhs)
{
}
int  PdbPadSimGeoChamV1::get_pc(){return pc;}
int  PdbPadSimGeoChamV1::get_arm(){return arm;}
int  PdbPadSimGeoChamV1::get_sect(){return sect;}
void PdbPadSimGeoChamV1::set_pc(int ipc){pc = ipc;}
void PdbPadSimGeoChamV1::set_arm(int iarm){arm = iarm;}
void PdbPadSimGeoChamV1::set_sect(int isect){sect =isect;}

void PdbPadSimGeoChamV1::zero()
{
  pc = 0;
  arm = 0;
  sect = 0;
}

PHPanel PdbPadSimGeoChamV1::get_pc1Sectors(){return pc1Sectors[arm][sect];}
PHPanel PdbPadSimGeoChamV1::get_pc2Sectors(){return pc2Sectors[arm][sect];}
PHPanel PdbPadSimGeoChamV1::get_pc3Sectors(){return pc3Sectors[arm][sect];}

void PdbPadSimGeoChamV1::set_pc1Sectors(PHPanel pc1SectorfrompadDetGeo)
{
pc1Sectors[arm][sect] = pc1SectorfrompadDetGeo;
}

void PdbPadSimGeoChamV1::set_pc2Sectors(PHPanel pc2SectorfrompadDetGeo)
{
pc2Sectors[arm][sect] = pc2SectorfrompadDetGeo;
}

void PdbPadSimGeoChamV1::set_pc3Sectors(PHPanel pc3SectorfrompadDetGeo)
{
pc3Sectors[arm][sect] = pc3SectorfrompadDetGeo;
}

PdbPadSimGeoChamV1::~PdbPadSimGeoChamV1()
{
}

void PdbPadSimGeoChamV1::print() const
{
 
  PHPoint panelPoint;
  float x, y, z;
  if(pc==0)
    {
      std::cout<<pc<<"  "<<arm<<"  "<<sect<<std::endl;
      for (int ipoint = 0; ipoint < 4; ipoint++)
	{      
                 panelPoint = pc1Sectors[arm][sect].getPoint(ipoint);
                 x = panelPoint.getX();
                 y = panelPoint.getY();
                 z = panelPoint.getZ();
		 
                 std::cout << x <<"  " << y << "  " << z << "\n";
         }
    }

	  else if(pc==1)
	      {
		 std::cout<<pc<<"  "<<arm<<"  "<<sect<<std::endl;
		 for (int ipoint = 0; ipoint < 4; ipoint++)
		   {
		     panelPoint = pc2Sectors[arm][sect].getPoint(ipoint);
		     x = panelPoint.getX();
		     y = panelPoint.getY();
		     z = panelPoint.getZ();
		     std::cout << x <<"  " << y << "  " << z << "\n";
		   }
	      }
	   else if(pc==2)
	      {
		std::cout<<pc<<"  "<<arm<<"  "<<sect<<std::endl;
		for (int ipoint = 0; ipoint < 4; ipoint++)
		  {
		    panelPoint = pc3Sectors[arm][sect].getPoint(ipoint);
		    x = panelPoint.getX();
		    y = panelPoint.getY();
		    z = panelPoint.getZ();
		    std::cout << x <<"  " << y << "  " << z << "\n";
		  }
	      }

}









