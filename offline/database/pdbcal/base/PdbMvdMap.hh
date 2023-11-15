//Sangsu Ryu Nov/13/2000

#ifndef __PDBMVDMAP_HH__
#define __PDBMVDMAP_HH__

#include "PdbCalChan.hh"
#include <iosfwd>

/**Persistence capable mapping class for Mvd.
  * @author Sangsu Ryu \URL{ryu@rcf.rhic.bnl.gov} Nov/28/2000
  * @version 2.0
  * For channel #1 in detectors see
  * http://www.phenix.bnl.gov/~ryu/public/mvdinc++/pad.gif and
  * http://www.phenix.bnl.gov/~ryu/public/mvdinc++/strip.gif
  */

class PdbMvdMap : public PdbCalChan 
{
public:
  PdbMvdMap();
  virtual ~PdbMvdMap();
  PdbMvdMap(const PdbMvdMap & p); //copy constr

  PdbMvdMap& operator=(const PdbMvdMap& source);

                                      ///prints out to cout
  virtual void print() const;         ///prints out to ostream
  virtual void print(std::ostream&) const; ///reads in from istream
  virtual void Read(std::istream&);
  using TObject::Read;
                                                  ///
   void set_packetid (int temp){ packetid=temp;}  ///
   void set_serialid (int temp){ serialid=temp;}  ///
   void set_type     (int temp){     type=temp;}  ///
   void set_installed(int temp){installed=temp;}  ///
   void set_r        (int temp){        r=temp;}  ///
   void set_phi      (int temp){      phi=temp;}  ///
   void set_z        (int temp){        z=temp;}  ///
   void set_mspare0  (int temp){  mspare0=temp;}  ///
   void set_mspare1  (int temp){  mspare1=temp;}  ///
   void set_mspare2  (int temp){  mspare2=temp;}

                                                  ///
    int get_packetid()const{return packetid;}     ///
    int get_serialid()const{return serialid;}     ///
    int get_type    ()const{return type;}         ///
    int is_installed()const{return installed;}    ///
    int get_r       ()const{return r;}            ///
    int get_phi     ()const{return phi;}          ///
    int get_z       ()const{return z;}            ///
    int get_mspare0 ()const{return mspare0;}      ///
    int get_mspare1 ()const{return mspare1;}      ///
    int get_mspare2 ()const{return mspare2;} 

   /**convert between software index and hardware index
     *for strip and pad detectors
     *arguments are always of form (from_indices, *to_indices)
     */ 
   virtual void Soft2Hard(int index_soft, int* index_hard)const;  
   virtual void Soft2Hard(int column, int row, int* index_hard)const; 
   virtual void Hard2Soft(int index_hard, int* index_soft)const;  
   virtual void Hard2Soft(int index_hard, int* column, int* row )const;

  ///prints out to ostream
  friend std::ostream& operator<<(std::ostream&, const PdbMvdMap&); 
  ///reads in from istream
  friend std::istream& operator>>(std::istream&, PdbMvdMap&);
  virtual int operator==(const PdbMvdMap&);
  virtual int operator!=(const PdbMvdMap& mvdmap){return (*this)==mvdmap?0:1;}
private:
                   ///
    int packetid;  ///
    int serialid;  ///
    int type;      /**is detector installed? 
                     *note some packetid is signed to nonexistent detector
                     */
    int installed; ///
    int r;         ///
    int phi;       ///
    int z;         ///
    int mspare0;   ///
    int mspare1;   ///
    int mspare2;

  ClassDef(PdbMvdMap,1);
};

#endif
