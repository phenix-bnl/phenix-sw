#ifndef __REACTIONPLANESNGL_H
#define __REACTIONPLANESNGL_H

#include <iostream>
#include <string>
#include "PHObject.h"

/**
 * @brief  The abstract class for a storage of reaction plane
 *
 * Created on  / /2012 by Hiroshi Nakagomi.
 */

class ReactionPlaneSngl : public PHObject
{
  public:
  
    virtual ~ReactionPlaneSngl() {}
    virtual ReactionPlaneSngl *clone() const;
    virtual ReactionPlaneSngl& operator=(const ReactionPlaneSngl& source);
    
    virtual void SetPsi(const float psi) {;}
    virtual float GetPsi() const { return -9999.;};
    
    virtual void SetName(const char *name) {}
    virtual const char *GetName() const {return "DUMMY";}
    virtual void SetIdCode(const int idcode) {}
    virtual int GetIdCode() const {return -9999;}

  protected:
    void virtual_warning(const char *label){
      std::cout << "ReactionPlaneSngl::"<<label<<" not implemented by daughter class" << std::endl;
    }
    
    ClassDef(ReactionPlaneSngl,1)
};

#endif /* __REACTIONPLANESNGL_H */
