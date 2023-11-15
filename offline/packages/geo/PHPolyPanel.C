// Created by:  Jane M. Burward-Hoy and Federica Messer
//
// Purpose:  The constructor, destructor, copy constructor, and assignment functions are
//           defined here for the PHPolyPanel class.

#include "PHPolyPanel.h"
#include <iostream>
using namespace std;

PHPolyPanel::~PHPolyPanel()
{
  polypanel.clearAndDestroy();
}

void PHPolyPanel::clear()
{
  polypanel.clear();
}

ostream&  
operator<< (ostream &os, PHPolyPanel &pline)
{
  os << pline ;
  return os;
}

void 
PHPolyPanel::print() const
{
  cout << polypanel << endl;
}

void 
PHPolyPanel::print(size_t index) const
{
  if (index < polypanel.length()) 
    {
      cout << *(polypanel[index]) << endl;
    }
}

PHTriPanel* 
PHPolyPanel::getTriPanel(const size_t index) const
{
  return polypanel[index];
}

PHTriPanel* 
PHPolyPanel::removeLastTriPanel()
{
  return polypanel.removeLast();
}

PHTriPanel* 
PHPolyPanel::removeTriPanelAt(size_t index) 
{
  return  polypanel.removeAt(index); 
}

void
PHPolyPanel::appendTriPanel(PHTriPanel* ptr)
{
   polypanel.append(ptr);
}

void 
PHPolyPanel::insertTriPanelAt(PHTriPanel* ptr,size_t index)
{
   polypanel.insertAt(ptr,index);
}

double
PHPolyPanel::surface() const
{
 const size_t last = (int)numberOfPanels() - 1;
 
 return surface(0,last);
}

double
PHPolyPanel::surface(const size_t n1, const size_t n2) const
{
  double surface = 0.0;
  int first, last;

  if (n1 < n2) 
    {
      first = n1;
      last = n2;
    }
  else 
    {
      first = n2;
      last = n1;
    }
  
  if (last < (int)numberOfPanels() && first >= 0) 
    {
      for (int i = first; i <= last; i++) 
	{
	  surface += polypanel[i]->surface();
	}
    } 
  else 
    {
      PHMessage("PHPolyPanel::surface", PHError, "index out of range");
    }
  
  return surface;
}






