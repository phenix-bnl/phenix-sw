// Sangsu Ryu Nov/13/20000

#ifndef __PDBMVDCROSSTALK_HH__ 
#define __PDBMVDCROSSTALK_HH__

#include "PdbMvdMap.hh"
#include <iostream>

/**Persistence capable class for Mvd crosstalk database.
  *An object of this class covers one packet of type strip(=0) or pad(=1)
  *Note for pad detectors first 2 and last 2 emelemts are not used
  *@author Sangsu Ryu \URL{ryu@rcf.rhic.bnl.gov} Nov/28/2000
  *@version 1.0
  */

class PdbMvdCrosstalk : public PdbMvdMap {

public:
                               ///
            PdbMvdCrosstalk(); ///
   virtual ~PdbMvdCrosstalk();

  PdbMvdCrosstalk(const PdbMvdCrosstalk &p);
  PdbMvdCrosstalk&  operator = (const PdbMvdCrosstalk &p);

                                                    ///prints out all 256 channels to std::cout
   virtual void print()const;                       ///prints out all 256 channels to std::ostream
   virtual void print(std::ostream&)const;               ///prints out ith channel to std::ostream
   virtual void print(int i,std::ostream& os=std::cout)const; ///reads in ith channel from std::istream
   virtual void Read(int, std::istream&);                ///reads in all channels from std::istream
   virtual void Read(std::istream&);
                                                              ///
   void set_crosstalk(float temp, int i){crosstalk[i]=temp;}  ///
   void set_xspare0  (float temp, int i){  xspare0[i]=temp;}  ///
   void set_xspare1  (float temp, int i){  xspare1[i]=temp;}  ///
   void set_xspare2  (float temp, int i){  xspare2[i]=temp;}  
                                                        ///
  float get_crosstalk(int i)const{return crosstalk[i];} ///
  float get_xspare0  (int i)const{return   xspare0[i];} ///
  float get_xspare1  (int i)const{return   xspare1[i];} ///
  float get_xspare2  (int i)const{return   xspare2[i];}  

  virtual float get_crosstalk_soft(int i)const;
  virtual float get_xspare0_soft  (int i)const;
  virtual float get_xspare1_soft  (int i)const;
  virtual float get_xspare2_soft  (int i)const;

  virtual float get_crosstalk_soft(int column, int row)const;
  virtual float get_xspare0_soft  (int column, int row)const;
  virtual float get_xspare1_soft  (int column, int row)const;
  virtual float get_xspare2_soft  (int column, int row)const;

                    /// 
   enum {size=256};

   /**prints out all 256 channels to std::ostream
     *cint requires operator<< and operator>> to be friends
     */
friend std::ostream& operator<<(std::ostream& os, const PdbMvdCrosstalk&);
friend std::istream& operator>>(std::istream& is, PdbMvdCrosstalk&);

private:
                          ///
   float crosstalk[size]; ///
   float   xspare0[size]; ///
   float   xspare1[size]; ///
   float   xspare2[size];

  ClassDef(PdbMvdCrosstalk,1);
};

#endif
