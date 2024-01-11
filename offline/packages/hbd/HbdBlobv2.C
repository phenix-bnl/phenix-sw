
#include "HbdBlobv2.h"

ClassImp(HbdBlobv2)

using namespace std;

HbdBlobv2::HbdBlobv2()
{
	
	id = -9999;
	sector = -9999;
	charge = -9999.;
	size = -9999;
	blobx = -9999.;
	bloby = -9999.;
	blobz = -9999.;
        nlocalmax = -9999;
	localmax = -9999.0;
	parentid = -9999;
	bloby_local = -9999.0;
	blobz_local = -9999.0;
	cluster_cells.clear();
	clustertype = -9999;
	chrgthresh=-1.0;		

	for (int i=0; i<100; i++)
	{
		charge_pad[i] = -9999.;
		pady_local[i] = -9999.;
		padz_local[i] = -9999.;
		pad_sector[i] = -9999;
	}


	
	return;
	
}

HbdBlobv2::HbdBlobv2(HbdBlobv2 *blob)
{
	
	if (!blob) return;
	
	id = blob->get_id();
	sector = blob->get_sector();
	charge = blob->get_charge();
	size = blob->get_size();
	blobx = blob->get_blobx();
	bloby = blob->get_bloby();
	blobz = blob->get_blobz();
        nlocalmax = blob->get_nlocalmax();
	localmax = blob->get_localmax();
	parentid = blob->get_parentid();
	bloby_local = blob->get_bloby_local();
	blobz_local = blob->get_blobz_local();
	cluster_cells.clear();
	cluster_cells=blob->cluster_cells;
	clustertype=blob->clustertype;


  for (int i=0; i<100; i++)
    {
      charge_pad[i] = blob->get_charge_pad(i);
      pady_local[i] = blob->get_pady_local(i);
      padz_local[i] = blob->get_padz_local(i);
      pad_sector[i] = blob->get_pad_sector(i);
    }


	
	
	return;
	
}


void HbdBlobv2::add_cell( HbdCell *newcell, double weight){
	cluster_cells.push_back(newcell);
	charge+=(newcell->get_charge())*weight;
	
	// Find localmax ...
	float charge_temp=0.0;
	for(unsigned int i=0; i<cluster_cells.size(); i++){
		charge_temp = cluster_cells.at(i)->get_charge();
		if(charge_temp>localmax){
			localmax = charge_temp;
		}
	}	
	
}

void HbdBlobv2::print(){
	std::vector<HbdCell*>::iterator ic;
	for(ic=cluster_cells.begin();ic!=cluster_cells.end(); ++ic){
		if((*ic)){
			std::cout << " padnumber " << (*ic)->get_padnum() << "  ";
			std::cout << " sector " << (*ic)->get_sector() << "  ";
			std::cout << " charge " << (*ic)->get_charge() << "  ";
			std::cout << std::endl;
		}
		else{
			cout << "Got a null Pointer to cell " << endl;
		}
	}
	std::cout << "bloby_local, blobz_local " << bloby_local << " " << blobz_local << " Total Charge " << charge << endl;		
}


void HbdBlobv2::identify(ostream& os) const
{
	os << "identify yourself: HbdBlobv2 Object\n" << std::endl;
	return;
}

void HbdBlobv2::clear(){
	
	id = -9999;
	sector = -9999;
	charge = -9999.;
	size = -9999;
	blobx = -9999.;
	bloby = -9999.;
	blobz = -9999.;
        nlocalmax = -9999;
	localmax = -9999.;
	parentid = -9999;
	bloby_local = -9999.;
	blobz_local = -9999.;
	cluster_cells.clear();
	clustertype = -9999;
	chrgthresh=0.0;
}


void HbdBlobv2::Reset()
{
	
	id = -9999;
	sector = -9999;
	charge = -9999.;
	size = -9999;
	blobx = -9999.;
	bloby = -9999.;
	blobz = -9999.;
	nlocalmax = -9999;
	localmax = -9999.;
	parentid = -9999;
	bloby_local = -9999.;
	blobz_local = -9999.;
	cluster_cells.clear();
	clustertype = -9999;
	chrgthresh=0.0;
	
	return;
}

int HbdBlobv2::isValid() const
{
	return 1;
}

