#ifndef __PDBRXNPSURVEY_HH__
#define __PDBRXNPSURVEY_HH__

// numbering convention for the 6 survey point ...
//
//  \---------------/
//   \3          0 /
//    \           /
//     \ 2     1 /
//      \-------/
//  
//      \-------/
//       \     /
//        \   /
//       5 \-/ 4


#include "PdbCalChan.hh"

class PdbRxNPSurvey : public PdbCalChan 
{
  public:
    PdbRxNPSurvey();
    virtual ~PdbRxNPSurvey() {}

    //.. phi, R and Z of each of the 6 survey point for each phi segment
    //-----------------------------------------------------------------------
    float get_X(int arm, int phi, int id) const {return X[arm][phi][id];}
    void  set_X(int arm, int phi, int id, float val) {X[arm][phi][id] = val;}

    float get_Y(int arm, int phi, int id) const {return Y[arm][phi][id];}
    void set_Y(int arm, int phi, int id, float val) {Y[arm][phi][id] = val;}

    float get_Z(int arm, int phi, int id) const {return Z[arm][phi][id];}
    void set_Z(int arm, int phi, int id, float val) {Z[arm][phi][id] = val;}

    virtual void print() const;

  private:
    float X[2][12][6];
    float Y[2][12][6];
    float Z[2][12][6];

  ClassDef(PdbRxNPSurvey,1);
};

#endif /* __PDBRXNPSURVEY_HH__ */
