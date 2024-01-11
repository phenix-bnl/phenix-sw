{
ifstream fin;    // input oscar file
ifstream finvtx; // input file with rawhits, with event vertex info
ofstream fout;   // output oscar file with shifted vertex

int nevts=10000;
//int nevts=1200;

std::string tmptmp;
std::string whichVtx;
int   i_evt;
int   i_nhits;
float bbccharge;
int   cent;
float zvtx;
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

std::string vtxinfname = "/phenix/zdata03/phnxreco/VTX/lebedev/svxhits/svxhits_347129_0008_0-20cent_vtx1svx.dat";
finvtx.open(vtxinfname.c_str());

std::string tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8;
int count=0;
int itmp,itmp1,itmp2,itmp3,itmp4,itmp5,kf,ipart,npart;
float px,py,pz,x,y,z,E,M;

//std::string infname = "/phenix/subsys/vtx/singles/oscar/bbbar/bbbar2e_109_10kevts_gt1gev.txt";
std::string infname = "/phenix/subsys/vtx/singles/oscar/ddbar/ddbar2e_108_10kevts_gt1gev.txt";
fin.open(infname.c_str());

//fout.open("/phenix/subsys/vtx/singles/oscar/bbbar/bbbar2e_109_shift_347129_09_cent.txt");
fout.open("/phenix/subsys/vtx/singles/oscar/ddbar/ddbar2e_108_shift_347129_08_cent.txt");

// read oscar input file header and write it to output file
fin >>tmp1>>tmp2;
cout << tmp1 << " " << tmp2 << endl;
fout << tmp1 << " " << tmp2 << endl;
  fin >>tmp1>>tmp2;
  cout << tmp1 << " " << tmp2 << endl;
  fout << tmp1 << " " << tmp2 << endl;
    fin >>tmp1>>tmp2>>tmp3>>tmp4>>tmp5>>tmp6>>tmp7>>tmp8;
    cout <<tmp1<<" "<<tmp2<<" "<<tmp3<<" "<<tmp4<<" "<<tmp5<<" "<<tmp6<<" "<<tmp7<<" "<<tmp8<<endl;
    fout <<tmp1<<" "<<tmp2<<" "<<tmp3<<" "<<tmp4<<" "<<tmp5<<" "<<tmp6<<" "<<tmp7<<" "<<tmp8<<endl;
      fin >>tmp1;
      cout << tmp1 << endl;
      fout << tmp1 << endl;

// infinite loop over events. stop when nevts events are processed
while(true) {

// first read event vertex from rawhit file
  finvtx >> tmptmp >> i_evt >> i_nhits >> bbccharge >> zvtx >> whichVtx; // event header
  if(finvtx.eof()) {
    cout << "Vertex input file ended. re-opening..." << endl;
    finvtx.close();
    finvtx.open(vtxinfname.c_str());
    finvtx >> tmptmp >> i_evt >> i_nhits >> bbccharge >> zvtx >> whichVtx;
  }
  cent = (int)bbccharge;
  cout << "vextex in rawhit event: " << count << " " << i_nhits << " " << zvtx << " " << cent << endl;
  int tmpcount=0;
  for(int i=0; i<i_nhits; i++) { // read rawhits (don't need them really)
    finvtx>>i_section>>i_layer>>i_ladder>>i_sensor>>i_sensorSection>>i_sensorReadout>>i_sensorType>>i_channel>>i_adc>>i_pixelModule>>i_pixelROC>>i_HDFlag>>i_hitID;
    tmpcount++;
  }

  zvtx = zvtx*1.0e+13;  // convert from cm to Fermi
  //cout << "shifting by " << zvtx << " Fm" << endl;

// now read oscar event
  fin >> itmp >> npart;   // event header 
    if(fin.eof()) { break; }
    cout << itmp1 << " " << npart << endl;
    fout << itmp1 << " " << npart << endl; // write out oscar event header
    for(int i=0; i<npart; i++) { // read particles in the oscar event
      fin >>ipart>>kf>>itmp2>>px>>py>>pz>>E>>M>>x>>y>>z>>itmp3;
      cout <<ipart<<" "<<kf<<" "<<itmp2<<" "<<px<<" "<<py<<" "<<py<<" "<<E<<" "<<M<<" "<<x<<" "<<y<<" "<<z<<" "<<itmp3<<endl;
      z = z + zvtx; // shift vertex
      fout <<ipart<<" "<<kf<<" "<<itmp2<<" "<<px<<" "<<py<<" "<<py<<" "<<E<<" "<<M<<" "<<x<<" "<<y<<" "<<z<<" "<<itmp3<<endl; // write out particles
    }
    fin >> itmp4 >> itmp5; // event trailer
    cout << itmp4 << " " << itmp5 << endl;
    fout << itmp4 << " " << itmp5 << endl; // write out oscar event trailer
      count++;
    if(count>=nevts) break;
} // end infinite loop

fin.close();
finvtx.close();
fout.close();

}

