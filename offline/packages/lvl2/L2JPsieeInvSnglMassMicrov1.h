///////////////////////////////////////////////////////////
//							 //
//   Lvl2 JPsi Single Pair
//							 //
//      Org:  Wei Xie (e-mail: xiewei@rcf2.rhic.bnl.gov) //
//							 //
///////////////////////////////////////////////////////////

#ifndef __L2JPsieeInvSnglMassMicrov1_H__
#define __L2JPsieeInvSnglMassMicrov1_H__

#include <PHObject.h>

class L2JPsieeInvSnglMassMicrov1 : public TObject 
{
  protected:

    int   Pair1stCandidateID;
    int   Pair2ndCandidateID;
    float   mass;

  public:

    L2JPsieeInvSnglMassMicrov1();
    virtual ~L2JPsieeInvSnglMassMicrov1(){}

    void    set_1stCandidateID(int input)  { Pair1stCandidateID = input;}
    void    set_2ndCandidateID(int input)  { Pair2ndCandidateID = input;}
    void    set_Mass(float input)  { mass = input;}

    int     get_1stCandidateID()        { return Pair1stCandidateID;}
    int     get_2ndCandidateID()        { return Pair2ndCandidateID;}
    float   get_Mass()          { return mass;}

    ClassDef(L2JPsieeInvSnglMassMicrov1, 1)
};

#endif	// __L2JPsieeInvSnglMassMicrov1_H__
