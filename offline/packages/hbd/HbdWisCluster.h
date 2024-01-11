#ifndef __HBDWISCLUSTER_H__
#define __HBDWISCLUSTER_H__
//Hbd Cluster


//c++ includes
#include <iostream>
#include <vector>
#include <deque>
#include <map>
#include <string>
#include <algorithm>


//phenix includes
#include <HbdCellList.h>
#include <HbdCell.h>
#include <HbdCellv1.h>
#include <SubsysReco.h>
#include <PHGlobal.h>

#include <TObject.h>
#include <TSystem.h>
#include <TFile.h>
#include <TTree.h>
#include <TROOT.h>



//
// Utility class to define cluster from cells
//
class HbdWisCluster: public TObject{
public:
    struct WisCluster: public TObject {
        mutable int sector;
        //mutable unsigned int size;   // area
        mutable int clusterid;
        mutable unsigned int clustertype;
        mutable double charge;
        mutable double localmax;
        mutable double PosY, PosZ;
        std::vector<HbdCell*> cluster_cells;
        
        int get_ncells(){return cluster_cells.size();};
        
        void add_cell(HbdCell *newcell, double weight=1.0){
            cluster_cells.push_back(newcell);
            charge+=(newcell->get_charge())*weight;
            
            localmax=get_localmax();
        }
        
        
        
        
        
        

        
        
        
        
        
        // Getter and Setter member functions
        // total charge for cluster
        double get_charge() const{
            return charge;
        }
        
        void set_charge(double ch){
            charge = ch;
        }
        
        double get_PosY() const{
            return PosY;
        }
        
        void set_PosY(double posY){
            PosY = posY;
        }
        
        
        double get_PosZ() const{
            return PosZ;
        }
        
        void set_PosZ(double posZ){
            PosZ = posZ;
        }
        
        int get_clusterid() const{
            return clusterid;
        }
        
        // This should be set to itrk after track association.
        void set_clusterid(int clusid){
            clusterid=clusid;
        }
        
        
        int get_sector() const{
            return sector;
        }
        
        // This should be set to itrk after track association.
        void set_sector(int sect){
            sector=sect;
        }
        
        
        double get_localmax() const{
            // Find localmax ...
            double charge_temp=0.0;
            localmax =0.0;
            for(unsigned int i=0; i<cluster_cells.size(); i++){
                charge_temp = cluster_cells.at(i)->get_charge();
                if(charge_temp>localmax){
                    localmax = charge_temp;
                }
            }
            return localmax;
        }
        
        void set_localmax(double locmax){
            localmax = locmax;
        }
        
        unsigned int get_clustertype() {

            return clustertype;
        }
        
        
        
        void set_clustertype(unsigned int clustype){
            clustertype=clustype;
        }
        
        
        
        
        void print() const {
            
            std::vector<HbdCell*>::const_iterator ic;
            for(ic=cluster_cells.begin();ic!=cluster_cells.end(); ++ic){
                if((*ic)){
                    std::cout << " padnumber " << (*ic)->get_padnum() << "  ";
                    std::cout << " sector " << (*ic)->get_sector() << "  ";
                    std::cout << " charge " << (*ic)->get_charge() << "  ";
                    std::cout << std::endl;
                }
                else{
                    std::cout << "Got a null Pointer to cell " << std::endl;
                }
            }
            std::cout << "PosY, PosZ " << PosY << " " << PosZ << " Cluster Charge " << charge << std::endl;
        }
        
        
        void clear(){
            sector = -9999;
            clusterid = -9999;
            clustertype = 0;
            charge = 0.;
            localmax = 0.;
            PosY = 0.;
            PosZ = 0.;
            cluster_cells.clear();
        }
        
        void copyto(WisCluster& dest) const
        {
            dest.clusterid=clusterid;
            dest.clustertype=clustertype;
            dest.sector=sector;
            dest.charge=charge;
            dest.localmax=localmax;
            dest.PosY=PosY;
            dest.PosZ=PosZ;
            dest.cluster_cells = cluster_cells;
            
        }
        
        WisCluster() :
        sector(-9999),
        clusterid(-9999),
        clustertype(0),
        charge(0.),
        localmax(0.),
        PosY(0.),
        PosZ(0.),
        cluster_cells(0) {}
        WisCluster( const WisCluster & rhs) :
        TObject(rhs),
        sector(rhs.sector),
        clusterid(rhs.clusterid),
        clustertype(rhs.clustertype),
        charge(rhs.charge),
        localmax(rhs.localmax),
        PosY(rhs.PosY),
        PosZ(rhs.PosZ),
        cluster_cells(rhs.cluster_cells) {}
        
        ClassDef(WisCluster, 0);
    };
    
    
    
    
    int clusterid;             // ith cluster
    int size;
    int sector;
    double TotChg;              // Sum of Charge of cells in this cluster
    double x;
    double y;
    double z;
    double PosY;
    double PosZ;          // Center-Of-gravity Position (Local)
    double localmax;
    double dphi;
    double dz;
    double sdphi;
    double sdz;
    double scharge;
    
    
    HbdWisCluster():
    clusterid(-1),
    size(0),
    sector(0),
    TotChg(0.0),
    x(-9999.),
    y(-9999.),
    z(-9999.),
    PosY(0.0),
    PosZ(0.0),
    localmax(0.0),
    dphi(-9999.),
    dz(-9999.),
    sdphi(-9999.),
    sdz(-9999.),
    scharge(-9999.)
    {
        
    }
    
    HbdWisCluster(const HbdWisCluster& rhs)
    {
        rhs.copyto(*this);
    }
    
    ~HbdWisCluster()
    {
        //cluster_cells.clear(); //
    }
    
    void copyto(HbdWisCluster& dest) const
    {
        dest.clusterid=clusterid;
        dest.size=size;
        dest.sector=sector;
        dest.TotChg=TotChg;
        dest.x=x;
        dest.y=y;
        dest.z=z;
        dest.PosY=PosY;
        dest.PosZ=PosZ;
        dest.localmax=localmax;
        dest.dphi = dphi;
        dest.dz = dz;
        dest.sdphi = sdphi;
        dest.sdz = sdz;
        dest.scharge = scharge;
        
    }
    
    void clear(){
        clusterid=-1;
        size = 0;
        sector = 0;
        TotChg=0.0;
        x=y=z=0.0;
        PosY=PosZ=0.0;
        localmax = 0.0;
        dphi = 0.0;
        dz = 0.0;
        sdphi = 0.0;
        sdz = 0.0;
        scharge = 0.0;
    }
    
    
    
    
    
    
    
    // Setter functions
    void set_hbdcharge(double charge)  {
        TotChg = charge;
    }
    
    void set_hbdsector(double sect)  {
        sector = sect;
    }
    
    void set_localmax(double lmax)  {
        localmax = lmax;
    }
    
    
    void set_hbdsize(double hbdsize)  {
        size = hbdsize;
    }
    
    
    void set_hbdx(double hbdx)  {
        x = hbdx;
    }
    
    void set_hbdy(double hbdy)  {
        y = hbdy;
    }
    
    void set_hbdz(double hbdz)  {
        z = hbdz;
    }
    
    void set_hbddz(double dz_clust)  {
        dz = dz_clust;
    }
    
    
    void set_hbddphi(double dphi_clust)  {
        dphi = dphi_clust;
    }
    
    
    void set_hbdsdphi(double sdphi_clust)  {
        sdphi = sdphi_clust;
    }
    
    void set_hbdsdz(double sdz_clust)  {
        sdz = sdz_clust;
    }
    
    
    void set_hbdscharge(double s_charge)  {
        scharge = s_charge;
    }
    
    
    // This should be set to itrk after track association.
    void set_clusterid(int clusid){
        clusterid=clusid;
    }
    
    
    
    // Return total charge for cluster
    double get_hbdcharge() const {
        return TotChg;
    }
    
    double get_localmax() const {
        return localmax;
    }
    
    double get_hbddphi() const {
        return dphi;
    }
    
    double get_hbddz() const {
        return dz;
    }
    
    int get_hbdsize()const {
        return size;
    }
    
    
    int get_hbdsector()const {
        return sector;
    }
    
    int get_clusterid()const {
        return clusterid;
    }
    
    
    double get_hbdx() const {
        return x;
    }
    
    double get_hbdy() const {
        return y;
    }
    
    
    double get_hbdz() const {
        return z;
    }
    
    
    double get_hbdsdphi() const {
        return sdphi;
    }
    
    double get_hbdsdz() const {
        return sdz;
    }
    
    double get_hbdscharge() const {
        return scharge;
    }
    
    
    
    
    HbdWisCluster * clone()
    {
        return new HbdWisCluster(*this);
    }
    
    void print() const {
        std::cout << "PosY, PosZ " << PosY << " " << PosZ << " Total Charge " << TotChg << std::endl;
    }
    
    
    
    
private:
    
    ClassDef(HbdWisCluster, 0);
    
    
};


#endif


