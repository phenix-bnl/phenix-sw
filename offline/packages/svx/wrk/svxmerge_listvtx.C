{
ifstream finsvxmerge;
ofstream foutsvxmerge;

std::string tmp;
int i_evt;
int i_nhits;
float bbccharge;
int cent;
float zvtx; 
std::string whichVtx;

    short i_section;
    short i_layer;
    short i_ladder;
    short i_sensor;
    short i_sensorSection;
    short i_sensorReadout;
    short i_sensorType;
    int   i_channel;
    int   i_adc;
    short i_pixelModule;
    short i_pixelROC;
    int   i_HDFlag;
    int   i_hitID;

int nevts=10000;
//int nevts=1500;
int count=0;

std::string infname = "/phenix/zdata03/phnxreco/VTX/lebedev/svxhits/svxhits_347129_0008_0-20cent_vtx1svx.dat";
finsvxmerge.open(infname.c_str());

foutsvxmerge.open("vtxlist_347129_0008_0-20cent.dat");

while(true) {
    //cout << "LOOP # " << count << " started..." << endl;
    finsvxmerge >> tmp >> i_evt >> i_nhits >> bbccharge >> zvtx >> whichVtx;
    if(finsvxmerge.eof()) {
      cout << "input file ended. re-opening..." << endl;
      finsvxmerge.close();
      finsvxmerge.open(infname.c_str());
      finsvxmerge >> tmp >> i_evt >> i_nhits >> bbccharge >> zvtx >> whichVtx;
    }
    cent = (int)bbccharge;
    cout << count << " " << i_nhits << " " << zvtx << " " << cent << endl;
    foutsvxmerge << count << " " << zvtx << " " << cent << endl;
    count++;
    int tmpcount=0;
    for(int i=0; i<i_nhits; i++) {
      finsvxmerge>>i_section>>i_layer>>i_ladder>>i_sensor>>i_sensorSection>>i_sensorReadout>>i_sensorType>>i_channel>>i_adc>>i_pixelModule>>i_pixelROC>>i_HDFlag>>i_hitID;
      tmpcount++;
    }
    //cout << "read " << tmpcount << " raw hits." << endl;
    //cout << "LOOP ended, total " << count << " events." << endl;
    if(count>=nevts) break;
} // infinite loop

finsvxmerge.close();

foutsvxmerge.close();

}

