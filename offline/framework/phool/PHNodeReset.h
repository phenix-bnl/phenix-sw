#ifndef __PHNODERESET_H__
#define __PHNODERESET_H__

//  Declaration of class PHNodeReset
//  Purpose: strategy which calls reset() on a PHNode 
//  Author: Matthias Messer

#include "PHNodeOperation.h"

class PHNode;

class PHNodeReset : public PHNodeOperation 
{ 
public: 
  PHNodeReset(){} 
   virtual ~PHNodeReset(){} 

protected: 
   virtual void perform(PHNode*);

}; 

#endif /* __PHNODERESET_H__ */
