// Class: HbdPreClusterMaker (implementation)
//-------------------------------------------------------------------------
#include "HbdPreClusterMaker.h"
#include "Hbd.h"
#include "hit_pad_list.h"
#include "HbdFinalSimSupport.h"
#include "HbdCellList.h"

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <PHTimeStamp.h>
#include <PHGeometry.h>

#include <iostream>
#include <fstream>

using namespace std;

HbdPreClusterMaker::HbdPreClusterMaker(int runnum)
{
  verbose = 0;
  r2d = 180./3.14159265;
  d2r = 3.14159265/180.;
  
  simsupport = new HbdFinalSimSupport(); // included to get the pad coords.
  simsupport->Init();
  hbdgeo.fetch(runnum); // Run numbers from before 11/2006 will get the prototype geometry
  hbdgeo.fetchPad(runnum);
	for(int i=0;i<192;i++){
		if(i==0 ||i==1||i==189||i==190||i==191){
			mypad_center[i][0]=simsupport->pad_center[i][0];
			mypad_center[i][1]=simsupport->pad_center[i][1];
			mypad_type[i]=70;
		}
		else if(simsupport->pad_type[i]==12){
			mypad_center[i][0]=simsupport->pad_center[i][0];
			mypad_center[i][1]=simsupport->pad_center[i][1]-0.5921;
			mypad_type[i]=simsupport->pad_type[i];
		}
		else if(simsupport->pad_type[i]==11){
			mypad_center[i][0]=simsupport->pad_center[i][0];
			mypad_center[i][1]=simsupport->pad_center[i][1]+0.5921;
			mypad_type[i]=simsupport->pad_type[i];
		}
		else if(simsupport->pad_type[i]==31||simsupport->pad_type[i]==32){
			mypad_center[i][0]=simsupport->pad_center[i][0];
			mypad_center[i][1]=simsupport->pad_center[i][1];
			if(i==181)mypad_type[i]=37;
			else if(i==188)mypad_type[i]=38;
			else mypad_type[i]=simsupport->pad_type[i];
		}
		else if(simsupport->pad_type[i]==33||simsupport->pad_type[i]==34){
			mypad_center[i][0]=simsupport->pad_center[i][0];
			mypad_center[i][1]=simsupport->pad_center[i][1];
			if(i==96)mypad_type[i]=35;
			else if(i==103)mypad_type[i]=36;
			else mypad_type[i]=simsupport->pad_type[i];
		}
		else if(simsupport->pad_type[i]==61){
			mypad_center[i][0]=simsupport->pad_center[i][0];
			mypad_center[i][1]=simsupport->pad_center[i][1]+0.5921;
			mypad_type[i]=simsupport->pad_type[i];
		}
		else if(simsupport->pad_type[i]==62){
			mypad_center[i][0]=simsupport->pad_center[i][0];
			mypad_center[i][1]=simsupport->pad_center[i][1]-0.5921;
			mypad_type[i]=simsupport->pad_type[i];
		}
		else if(simsupport->pad_type[i]==63){
			mypad_center[i][0]=simsupport->pad_center[i][0];
			mypad_center[i][1]=simsupport->pad_center[i][1]+0.5921;
			mypad_type[i]=simsupport->pad_type[i];
		}
		else if(simsupport->pad_type[i]==64){
			mypad_center[i][0]=simsupport->pad_center[i][0];
			mypad_center[i][1]=simsupport->pad_center[i][1]-0.5921;
			mypad_type[i]=simsupport->pad_type[i];
		}
		else{
			mypad_center[i][0]=simsupport->pad_center[i][0];
			mypad_center[i][1]=simsupport->pad_center[i][1];
			if(i==11||i==28||i==45||i==62||i==79||i==113||i==130||i==147||i==164){
				mypad_type[i]=1;
			}
			else if(i==18||i==35||i==52||i==69||i==86||i==120||i==137||i==154||i==171){
				mypad_type[i]=2;
			}
			else{
				mypad_type[i]=0;
			}
		}
	}
}

HbdPreClusterMaker::~HbdPreClusterMaker()
{
	delete simsupport;
}

void 
HbdPreClusterMaker::get_padkeys(int hbd_sector,int hit_padnum,double locx, double locy,vector<PadsInCluster> &v_key)
{
		v_key.clear();
		pad_type = 0;
		pad_x = 0;
		pad_y = 0;

		pad_type =mypad_type[hit_padnum];
 		pad_x = locx - mypad_center[hit_padnum][0];
 		pad_y = locy - mypad_center[hit_padnum][1];
//		int phiframe_flag =0;
//		int zframe_flag =0;
//		if(fabs(locx)>26||fabs(locx)<0.3) phiframe_flag=1;
//		if(fabs(locy)>10.7) zframe_flag=1;

		double angle = atan2(pad_y,pad_x)*r2d;
		if(angle<0)angle += 360;
		int seg = (int)(angle/60.);
		get_padkeys_regular(hbd_sector,hit_padnum,seg, v_key);

}

void HbdPreClusterMaker::get_padkeys_regular(int hbd_sector,int hit_padnum,int seg, vector <PadsInCluster> &v_key){
		for(int k=0;k<10;k++){
			if(hit_pad_list[hit_padnum][seg][k]==-999) break;
				int tmp_key = hbd_sector*192+hit_padnum+hit_pad_list[hit_padnum][seg][k];
				if(hbd_sector<6&&(tmp_key>1152||tmp_key<0)) continue;
				if(hbd_sector>5&&(tmp_key<1152||tmp_key>2303)) continue;
				PadsInCluster tmp;
				tmp.sector = tmp_key/192; tmp.padnum = tmp_key%192;tmp.padkey=tmp_key;
				v_key.push_back(tmp);
		}

	return;
}


int HbdPreClusterMaker::region_find(int seg,double pad_x,double pad_y,double rad0,double rad1,double rad2,double rad3){

	double vertex1[2] ={0,0};
 	double vertex2[2] ={1.3515,0};
 	double vertex3[2]={1.3515,1.3515/sqrt(3)};
 	double vertex4[2] ={1.3515,-1.3515/sqrt(3)};
	double pad_x_rot = pad_x*cos(-60.*seg*d2r-30*d2r)-pad_y*sin(-60.*seg*d2r-30*d2r);
	double pad_y_rot = pad_x*sin(-60.*seg*d2r-30*d2r)+pad_y*cos(-60.*seg*d2r-30*d2r);
 	int reg;
 	if(sqrt(pow(pad_x_rot-vertex1[0],2)+pow(pad_y_rot-vertex1[1],2))<rad0) reg=0;
 	else if(sqrt(pow(pad_x_rot-vertex2[0],2)+pow(pad_y_rot-vertex2[1],2))<rad1) reg=1;
 	else if(sqrt(pow(pad_x_rot-vertex3[0],2)+pow(pad_y_rot-vertex3[1],2))<rad2&&pad_y_rot>0) reg=2;
 	else if(sqrt(pow(pad_x_rot-vertex4[0],2)+pow(pad_y_rot-vertex4[1],2))<rad3&&pad_y_rot<0) reg=3;
 	else reg=4;
 	return reg;
}

void HbdPreClusterMaker::get_padkeys_phiframe(int hbd_sector,int hit_padnum,double pad_x, double pad_y, int pad_type, vector <PadsInCluster> &v_key){
	int frame_neighbour_dif[5]={-999,-999,-999,-999,-999};
	if(pad_type==31||pad_type==34){
		if(fabs(pad_y)<0.4){
			frame_neighbour_dif[0]=0;
		}else if(pad_y>0){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=1;
		}else if(pad_y<0){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=-1;
		}
	}else if(pad_type==32){
		if(fabs(pad_y)<0.6){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=8;
			frame_neighbour_dif[2]=9;
		}else if(pad_y>0){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=9;
			frame_neighbour_dif[2]=1;
		}else if(pad_y<0){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=8;
			frame_neighbour_dif[2]=-1;
		}
	}else if(pad_type==33){
		if(fabs(pad_y)<0.6){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=-8;
			frame_neighbour_dif[2]=-9;
		}else if(pad_y>0){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=-8;
			frame_neighbour_dif[2]=1;
		}else if(pad_y<0){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=-9;
			frame_neighbour_dif[2]=-1;
		}
	}else if(pad_type==35){
		if(fabs(pad_y)<0.6){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=-8;
			frame_neighbour_dif[2]=-9;
		}else if(pad_y>0){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=-8;
			frame_neighbour_dif[2]=1;
		}else if(pad_y<0){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=-9;
		}
	}else if(pad_type==36){
		if(fabs(pad_y)<0.6){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=-8;
			frame_neighbour_dif[2]=-9;
		}else if(pad_y>0){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=-8;
		}else if(pad_y<0){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=-9;
			frame_neighbour_dif[2]=-1;
		}
	}else if(pad_type==37){
		if(fabs(pad_y)<0.4){
			frame_neighbour_dif[0]=0;
		}else if(pad_y>0){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=1;
		}else if(pad_y<0){
			frame_neighbour_dif[0]=0;
		}
	}else if(pad_type==38){
		if(fabs(pad_y)<0.4){
			frame_neighbour_dif[0]=0;
		}else if(pad_y>0){
			frame_neighbour_dif[0]=0;
		}else if(pad_y<0){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=-1;
		}
	}else if(pad_type==61){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=-1;

	}else if(pad_type==62){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=1;
	}else if(pad_type==63){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=-1;
			frame_neighbour_dif[2]=8;
	}else if(pad_type==64){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=1;
			frame_neighbour_dif[2]=9;
	}

	for(int j=0;j<5;j++){
		if(frame_neighbour_dif[j]==-999) continue;
		int tmp_key = hbd_sector*192+hit_padnum+frame_neighbour_dif[j];
		if(hbd_sector<6&&(tmp_key>1152||tmp_key<0)) continue;
		if(hbd_sector>5&&(tmp_key<1152||tmp_key>2303)) continue;
		if(tmp_key>=192*2+96&&tmp_key<=192*2+191) continue;
		PadsInCluster tmp;
		tmp.sector = tmp_key/192; tmp.padnum = tmp_key%192;tmp.padkey=tmp_key;
		v_key.push_back(tmp);
	}
	return;
}

void HbdPreClusterMaker::get_padkeys_zframe(int hbd_sector,int hit_padnum,double pad_x, double pad_y, int pad_type, vector <PadsInCluster> &v_key){
	int frame_neighbour_dif[5]={-999,-999,-999,-999,-999};
	if(pad_type==1){
		if(pad_x<=0){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=-185;
			frame_neighbour_dif[2]=-9;
			frame_neighbour_dif[3]=-193;
		}else{
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=-185;
			frame_neighbour_dif[2]=8;
			frame_neighbour_dif[3]=-176;
		}
	}else if(pad_type==12){
		if(fabs(pad_x)<0.4){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=-184;
		}else if(pad_x>0.4){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=-184;
			frame_neighbour_dif[2]=9;
			frame_neighbour_dif[3]=-176;
		}else if(pad_x<-0.4){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=-184;
			frame_neighbour_dif[2]=-8;
			frame_neighbour_dif[3]=-193;
		}
	}else if(pad_type==2){
		if(pad_x<=0){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=185;
			frame_neighbour_dif[2]=-8;
			frame_neighbour_dif[3]=176;
		}else{
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=185;
			frame_neighbour_dif[2]=9;
			frame_neighbour_dif[3]=193;
		}
	}else if(pad_type==11){
		if(fabs(pad_x)<0.4){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=184;
		}else if(pad_x>0.4){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=184;
			frame_neighbour_dif[2]=8;
			frame_neighbour_dif[3]=193;
		}else if(pad_x<-0.4){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=184;
			frame_neighbour_dif[2]=-9;
			frame_neighbour_dif[3]=176;
		}
	}else if(pad_type==35){
		if(pad_x>=0){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=-185;
			frame_neighbour_dif[2]=8;
			frame_neighbour_dif[3]=-176;
		}else{
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=-185;
		}
	}else if(pad_type==36){
		if(pad_x>=0){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=185;
			frame_neighbour_dif[2]=9;
			frame_neighbour_dif[3]=193;
		}else{
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=185;
		}
	}else if(pad_type==37){
		if(pad_x<=0){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=-185;
			frame_neighbour_dif[2]=-9;
			frame_neighbour_dif[3]=-193;
		}else{
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=-185;
		}
	}else if(pad_type==38){
		if(pad_x<=0){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=185;
			frame_neighbour_dif[2]=-8;
			frame_neighbour_dif[3]=176;
		}else{
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=185;
		}
	}else if(pad_type==61){
		if(pad_x>=0){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=184;
			frame_neighbour_dif[2]=8;
			frame_neighbour_dif[3]=193;
		}else{
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=184;
		}
	}else if(pad_type==62){
		if(pad_x>=0){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=-184;
			frame_neighbour_dif[2]=9;
			frame_neighbour_dif[3]=-176;
		}else{
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=-184;
		}
	}else if(pad_type==63){
		if(pad_x<=0){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=184;
			frame_neighbour_dif[2]=-9;
			frame_neighbour_dif[3]=176;
		}else{
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=184;
		}
	}else if(pad_type==64){
		if(pad_x<=0){
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=-184;
			frame_neighbour_dif[2]=-8;
			frame_neighbour_dif[3]=-193;
		}else{
			frame_neighbour_dif[0]=0;
			frame_neighbour_dif[1]=-184;
		}
	}
	for(int j=0;j<5;j++){
		if(frame_neighbour_dif[j]==-999) continue;
		int tmp_key = hbd_sector*192+hit_padnum+frame_neighbour_dif[j];
		if(hbd_sector<6&&(tmp_key>1152||tmp_key<0)) continue;
		if(hbd_sector>5&&(tmp_key<1152||tmp_key>2303)) continue;
		if(tmp_key>=192*2+96&&tmp_key<=192*2+191) continue;
		PadsInCluster tmp;
		tmp.sector = tmp_key/192; tmp.padnum = tmp_key%192;tmp.padkey=tmp_key;
		v_key.push_back(tmp);
	}
	return;

}

void HbdPreClusterMaker::get_padkeys_zphiframe(int hbd_sector,int hit_padnum,double pad_x, double pad_y, int pad_type, vector <PadsInCluster> &v_key){
	int frame_neighbour_dif[5]={-999,-999,-999,-999,-999};
	if(pad_type==35){
		frame_neighbour_dif[0]=0;
		frame_neighbour_dif[1]=-185;
		frame_neighbour_dif[2]=-9;
		frame_neighbour_dif[3]=-193;
	}else if(pad_type==36){
		frame_neighbour_dif[0]=0;
		frame_neighbour_dif[1]=185;
		frame_neighbour_dif[2]=-8;
		frame_neighbour_dif[3]=176;
	}else if(pad_type==63){
		frame_neighbour_dif[0]=0;
		frame_neighbour_dif[1]=184;
		frame_neighbour_dif[2]=8;
		frame_neighbour_dif[3]=193;
	}else if(pad_type==64){
		frame_neighbour_dif[0]=0;
		frame_neighbour_dif[1]=-184;
		frame_neighbour_dif[2]=9;
		frame_neighbour_dif[3]=-176;
	}else if(pad_type==37){
		frame_neighbour_dif[0]=0;
		frame_neighbour_dif[1]=-185;
	}else if(pad_type==38){
		frame_neighbour_dif[0]=0;
		frame_neighbour_dif[1]=185;
	}else if(pad_type==62){
		frame_neighbour_dif[0]=0;
		frame_neighbour_dif[1]=-184;
	}else if(pad_type==61){
		frame_neighbour_dif[0]=0;
		frame_neighbour_dif[1]=184;
	}
	for(int j=0;j<5;j++){
		if(frame_neighbour_dif[j]==-999) continue;
		int tmp_key = hbd_sector*192+hit_padnum+frame_neighbour_dif[j];
		if(hbd_sector<6&&(tmp_key>1152||tmp_key<0)) continue;
		if(hbd_sector>5&&(tmp_key<1152||tmp_key>2303)) continue;
		if(tmp_key>=192*2+96&&tmp_key<=192*2+191) continue;
		PadsInCluster tmp;
		tmp.sector = tmp_key/192; tmp.padnum = tmp_key%192;tmp.padkey=tmp_key;
		v_key.push_back(tmp);
	}
	return;

}

void HbdPreClusterMaker::Print(int hbd_sector,int hbd_padnum, int seg){
	vector<PadsInCluster> v_keys;
	get_padkeys_regular(hbd_sector, hbd_padnum, seg, v_keys);
	cout<<"hbd_sector: "<<hbd_sector<<endl;
	cout<<"hbd_padnum: "<<hbd_padnum<<endl;
	cout<<"seg: "<<seg<<endl;
	vector<PadsInCluster>::iterator it = v_keys.begin();
	while(it != v_keys.end()){
		cout<<(*it).padnum<<" ";
		it++;
	}
	cout<<endl;
}
