// Sangsu Ryu Nov/13/2000

#ifndef __PDBMVDCALIB_HH__
#define __PDBMVDCALIB_HH__

#include <iostream>
#include "PdbMvdMap.hh"

/**Persistence capable class for Mvd calibration constant database.
  *An object of this class covers one packet of type strip(=0) or pad(=1)
  *Note for pad detectors first 2 and last 2 emelemts are not used 
  *@author Sangsu Ryu \URL{ryu@rcf.rhic.bnl.gov} Nov/28/2000
  *@version 2.0
  */

class PdbMvdCalib : public PdbMvdMap {

public:
                            ///
            PdbMvdCalib();  ///
   virtual ~PdbMvdCalib();
  PdbMvdCalib& operator=(const PdbMvdCalib& source);

                                           ///prints out all 256 channels to cout
   virtual void print()const;              ///prints out all 256 channels to ostream
   virtual void print(std::ostream&)const;      ///prints out ith channel to ostream
   virtual void print(int i, std::ostream& os = std::cout)const; ///reads in ith channel from istream
   virtual void Read(int, std::istream&);       ///reads in all channels from istream
   virtual void Read(std::istream&);
                                                                      ///
   void set_pedestal     (float temp, int i){     pedestal[i]=temp;}  ///
   void set_pedestalwidth(float temp, int i){pedestalwidth[i]=temp;}  ///
   void set_mip          (float temp, int i){          mip[i]=temp;}  ///
   void set_cspare0      (float temp, int i){      cspare0[i]=temp;}  ///
   void set_cspare1      (float temp, int i){      cspare1[i]=temp;}  ///
   void set_cspare2      (float temp, int i){      cspare2[i]=temp;}  
                                                                  ///
  float get_pedestal     (int i)const{return pedestal[i];}        /// 
  float get_pedestalwidth(int i)const{return pedestalwidth[i];}   ///
  float get_mip          (int i)const{return mip[i];}             ///
  float get_cspare0      (int i)const{return cspare0[i];}         ///
  float get_cspare1      (int i)const{return cspare1[i];}         ///
  float get_cspare2      (int i)const{return cspare2[i];}

  virtual float get_pedestal_soft     (int i)const;
  virtual float get_pedestalwidth_soft(int i)const;
  virtual float get_mip_soft          (int i)const;
  virtual float get_cspare0_soft      (int i)const;
  virtual float get_cspare1_soft      (int i)const;
  virtual float get_cspare2_soft      (int i)const;

  virtual float get_pedestal_soft     (int column, int row)const;
  virtual float get_pedestalwidth_soft(int column, int row)const;
  virtual float get_mip_soft          (int column, int row)const;
  virtual float get_cspare0_soft      (int column, int row)const;
  virtual float get_cspare1_soft      (int column, int row)const;
  virtual float get_cspare2_soft      (int column, int row)const;

  /**a packet has 256 channels, 
    *note for pad first 2 and last 2 channels are not used
    */
  enum {size=256};
  ///prints out to ostream
  friend std::ostream& operator<<(std::ostream&, const PdbMvdCalib&);  
  ///reads in from istream
  friend std::istream& operator>>(std::istream&, PdbMvdCalib&);

private:
                                ///pedestal mean
   float pedestal[size];        ///pedestal width
   float pedestalwidth[size];   ///single track peak above pedestal mean
   float mip[size];             ///spare
   float cspare0[size];         ///
   float cspare1[size];         ///
   float cspare2[size];

  ClassDef(PdbMvdCalib,1);

};

#endif

