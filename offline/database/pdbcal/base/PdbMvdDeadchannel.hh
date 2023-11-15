#ifndef __PDBMVDDEADCHANNEL_HH__
#define __PDBMVDDEADCHANNEL_HH__

#include "PdbMvdMap.hh"
#include <iostream>

/**Persistence capable class for Mvd deadchannel database.
  *An object of this class covers one packet of type strip(=0) or pad(=1)
  *Note for pad detectors first 2 and last 2 emelemts are not used
  *@author Sangsu Ryu \URL{ryu@rcf.rhic.bnl.gov} Nov/28/2000
  *@version 1.0
  */

class PdbMvdDeadchannel : public PdbMvdMap {

public:
                                   ///
            PdbMvdDeadchannel();   ///
   virtual ~PdbMvdDeadchannel();
  PdbMvdDeadchannel(const PdbMvdDeadchannel &p);
  PdbMvdDeadchannel&  operator = (const PdbMvdDeadchannel &p);

                                                     ///prints out all 256 channels to std::cout
   virtual void print()const;                        ///prints out all 256 channels to std::ostream
   virtual void print(std::ostream&)const;                ///prints out ith channel to std::ostream
   virtual void print(int i,std::ostream& os=std::cout)const;  ///reads in ith channel from std::istream
   virtual void Read(int, std::istream&);                 ///reads in all channels from std::istream
   virtual void Read(std::istream&);
                                                       ///
   void set_dead   (int temp, int i){dspare0[i]=temp;}    ///
   void set_dspare0(int temp, int i){dspare0[i]=temp;} ///
   void set_dspare1(int temp, int i){dspare1[i]=temp;} ///
   void set_dspare2(int temp, int i){dspare2[i]=temp;}
                                                    ///
    int  is_dead   (int i)const{return dspare0[i];}    ///
    int get_dspare0(int i)const{return dspare0[i];} ///
    int get_dspare1(int i)const{return dspare1[i];} ///
    int get_dspare2(int i)const{return dspare2[i];}

   virtual int  is_dead_soft   (int i)const;
   virtual int get_dspare0_soft(int i)const;
   virtual int get_dspare1_soft(int i)const;
   virtual int get_dspare2_soft(int i)const;

   virtual int  is_dead_soft   (int column, int row)const;
   virtual int get_dspare0_soft(int column, int row)const;
   virtual int get_dspare1_soft(int column, int row)const;
   virtual int get_dspare2_soft(int column, int row)const;

                    ///
   enum {size=256};
 
   /**prints out all 256 channels to std::ostream
     *cint requires operator<< and operator>> to be friends
     */
friend std::ostream& operator<<(std::ostream& os, const PdbMvdDeadchannel&);  ///reads in from std::istream
friend std::istream& operator>>(std::istream& is, PdbMvdDeadchannel&);

private:
     /**dead=1(dead) 
       *     0(working)
       */
     int dead[size];
     int dspare0[size];
     int dspare1[size];
     int dspare2[size];

  ClassDef(PdbMvdDeadchannel,1);
};

#endif

