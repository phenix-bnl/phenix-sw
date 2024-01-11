#ifndef __HBDBLOBV2_H_
#define __HBDBLOBV2_H_

#include <PHObject.h>
#include <vector>
#include <HbdBlob.h>
#include <HbdCell.h>

class HbdBlobv2 : public HbdBlob
{

   public:

      HbdBlobv2();
      HbdBlobv2(HbdBlobv2 *clus);  
      virtual ~HbdBlobv2() {}

      // The "standard PHObject response" functions...
      void Reset();
      void clear();
      int  isValid() const;
      void identify(std::ostream &os=std::cout) const;
      void add_cell( HbdCell *newcell, double weight=1.0);
      void print();

      //sets...
      void set_id (const int val) {id = val; return;}
      void set_sector(const int val) {sector = val; return;}
      void set_charge (const float val) {charge = val; return;}
      void set_blobx (const float val) {blobx = val; return;}
      void set_bloby (const float val) {bloby = val; return;}
      void set_blobz (const float val) {blobz = val; return;}
      void set_nlocalmax (const int val) {nlocalmax = val; return;}
      void set_localmax (const int val) {localmax = val; return;}
      void set_parentid (const int val) {parentid = val; return;}
      void set_size (const short val) {size = val; return;}
      void set_bloby_local(const float val) {bloby_local = val; return;}
      void set_blobz_local(const float val) {blobz_local = val; return;}
      void set_clustertype(const int clustype){clustertype=clustype; return;}
      void set_charge_pad(const int ind, const float val) {charge_pad[ind] = val; return;}
      void set_pady_local(const int ind, const float val) {pady_local[ind] = val; return;}
      void set_padz_local(const int ind, const float val) {padz_local[ind] = val; return;}
      void set_pad_sector(const int ind, const int val) {pad_sector[ind] = val; return;}



      //gets....
      int get_id () const { return id;}
      int get_sector () const { return sector;}
      float get_charge () const { return charge;}
      float get_blobx () const { return blobx;}
      float get_bloby () const { return bloby;}
      float get_blobz () const { return blobz;}
      int get_nlocalmax () const {return nlocalmax;}
      float get_localmax () const {return localmax;}
      int get_parentid () const {return parentid;}
      short get_size () const { return size;}
      short get_ncells() const {return size;}  
      float get_bloby_local () const {return bloby_local;}
      float get_blobz_local () const {return blobz_local;}
      int get_clustertype(){ return clustertype;}
      float get_charge_pad (const unsigned int ind) const { return charge_pad[ind]; }
      float get_pady_local (const unsigned int ind) const { return pady_local[ind]; }
      float get_padz_local (const unsigned int ind) const { return padz_local[ind]; }
      int get_pad_sector (const unsigned int ind) const { return pad_sector[ind]; }



      bool isLeft(){if (clustertype==1) return true; else return false;}
      bool isRight(){if (clustertype==2) return true; else return false;}

      HbdBlob* clone() const { return new HbdBlob(*this); }

      std::vector<HbdCell*> cluster_cells;


   protected:

      // Data member definition

      int id;                // Primary reference key
      int parentid;
      int sector;            //blob sector
      float charge;          // Charge deposit
      float time;            // time-of-flight
      float blobx;			 //global blob coordinates
      float bloby;
      float blobz;
      int nlocalmax;
      float localmax;
      short size;                // size
      float bloby_local; //blob position on PCB--direction perp. to beam dir.
      float blobz_local; //blob position on PCB--beam dir.

      float chrgthresh;				// Not used at the moment.
      int clustertype;				// left = 1  and right = 2

      float charge_pad[100];          // Charge deposit
      float pady_local[100];          // local y-coordinate of the pads in the blob
      float padz_local[100];          // local z-coordinate of the pads in the blob
      int pad_sector[100];          // Pad sector


      ClassDef(HbdBlobv2,1)

};

#endif /* __HBDBLOBV2_H_ */
