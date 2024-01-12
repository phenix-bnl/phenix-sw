#ifndef __TQPYTHIA_H__
#define __TQPYTHIA_H__

#include <TPythia6Calls.h>
#include <TObjArray.h>
class TClonesArray;

extern "C" {
  void DoPHQPYINIT_();
  void phqpyinit_();
  void dophqpysetup_(int* addr1, double* addr2, double* addr3);
  void calculatestuff_(double*,double*);
};


class TQPythia6 {

 public:
   static int* _mrpy_p;
   static double* _toqpygin_p;
   static double* _qpc1_p;
   /**
    * the path length that the parton traverses
    */
   static double _pathLength;

   /**
    * the qhat parameter
    */
   static double _qhat;

   TQPythia6();
   virtual ~TQPythia6();

   static void DoQPYINIT() {phqpyinit_();}

   /**
    * @brief Initialize the generator
    */
   void        Initialize(const char *frame, const char *beam, const char *target, float win);
   void        Pyinit(char* frame, char* beam, char* target, double wint);

   /**
    * @brief Get at the pythia parameters
    */
   void        SetMSEL   (int m)           { fPysubs->MSEL      = m; }
   void        SetCKIN   (int i, double c) { fPysubs->CKIN[i-1] = c; }
   void        Pystat    (int flag);
   void        Pylist    (int flag);
   int         GetMSTI   (int i)           { return fPypars->MSTI[i-1]; }
   double      GetPARI   (int i)           { return fPypars->PARI[i-1]; }

   /**
    * @brief Get the list of generated particles
    */
   TObjArray* ImportParticles(Option_t *option="");

   /**
    * @brief Generate the origin of the hard scattering within the 
    *        nuclear overlap region in the nucleus-nucleus cm frame
    *        At the moment, just generate everything at the origin.
    */
   void GenerateHardScatteringOrigin();

   /**
    * @brief Generate an event
    */
   void GenerateEvent();

   void SetSeed(unsigned int seed) { _seed = seed; }

   /**
    * @brief Return the path length for the current event
    */
   double GetPathLength() const { return _pathLength; }

   /**
    * @brief Set the path length for the parton for this event
    */
   void SetPathLength(double pl) { _pathLength = pl; }

   /**
    * @brief Return the qhat parameter of the simulation
    */
   double GetQHat() const { return _qhat; }

   /**
    * @brief Set the qhat parameter of the simulation
    */
   void SetQHat(double qhat) { _qhat = qhat; }

 private:
   /**
    * the pythia subprocess parameters
    */
   Pysubs_t* fPysubs;

   /**
    * the pythia parameters
    */
   Pypars_t* fPypars;

   /**
    * the generated particle block
    */
   Pyjets_t* fPyjets;

   unsigned int _seed;

   /**
    * the list of generated particles
    */
   TClonesArray *fParticles;

};

#endif /*__TQPYTHIA_H__ */
