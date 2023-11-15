#ifndef PHPOLYPANEL_H
#define PHPOLYPANEL_H

// Class:  PHPolyPanel header
//
// Created by:  Jane M. Burward-Hoy and Federica Messer
//
// Purpose:  A Poly Panel representation  (Cartesian Coordinates).
//      
// Description: A list of Panels

#include "PHTriPanel.h"
#include "PHPointerList.h"

#include <iosfwd>

class PHPolyPanel
{

 public:

  PHPolyPanel() {}
  virtual ~PHPolyPanel();

  void clear();

  friend std::ostream& operator<< (std::ostream &, PHPolyPanel &);

  void print() const;
  void print(size_t) const;

  size_t numberOfPanels() const { return polypanel.length();}
  
  double surface() const;
  double surface(const size_t,const size_t) const;
  
  PHTriPanel*  getTriPanel(size_t) const ;
  PHTriPanel*  removeLastTriPanel();
  PHTriPanel*  removeTriPanelAt(size_t);
  void  appendTriPanel(PHTriPanel* panel);
  void  insertTriPanelAt(PHTriPanel* panel,size_t);
  
 protected:
  
  PHPointerList<PHTriPanel> polypanel;

};

#endif /*PHPOLYPANEL_H*/
