#ifndef __REACTIONPLANESNGLV1_H
#define __REACTIONPLANESNGLV1_H

#include "ReactionPlaneSngl.h"
#include "TString.h"

/**
 * @brief  The implement v1 class for a storage of reaction plane 
 *
 * Created on / /2012  by Hiroshi Nakagomi.
 */


class ReactionPlaneSnglv1 : public ReactionPlaneSngl
{
  public:
  
    ReactionPlaneSnglv1();
    //ReactionPlaneSnglv1(const char *name, const unsigned int id, const float qx, const float qy, const float w);
    ReactionPlaneSnglv1(const char *name, const unsigned int id, const float rp); 
    virtual ~ReactionPlaneSnglv1(){}

    /*
      virtual void QVector(const short index, const float qvec){ m_qvector[index] = qvec; } // index=0:Qx, 1:Qy
      virtual float QVector(const short index) const           { return m_qvector[index]; }
      virtual void Weight(const float weight) { m_weight = weight; }
      virtual float Weight() const            { return m_weight;};
    */
    virtual void SetPsi(const float psi) { m_psi = psi; }
    virtual float GetPsi() const            { return m_psi;}
    
    
    virtual void SetName(const char *name)     { m_name = name; }
    virtual const char *GetName() const        { return m_name.Data(); }
    virtual void SetIdCode(const int idcode) { m_idcode = idcode; }
    virtual int GetIdCode() const            { return m_idcode; }

 private:
    //float   m_qvector[2]; // 0:x, 1:y
    //float   m_weight;
    float   m_psi;
    int     m_idcode;
    TString m_name;
    
    ClassDef(ReactionPlaneSnglv1,1)
};

#endif /* __REACTIONPLANESNGLV1_H */
