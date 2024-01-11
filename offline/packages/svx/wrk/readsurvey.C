{
// read svxGeometry.txt file first

// ladder, point, xyz
  float perfladd3[16][4][3];
  float perfladd4[24][4][3];
  float x,y,z;
  int nLadders[4];
  unsigned int tmpnladd[4];
  tmpnladd[0]=5;
  tmpnladd[1]=10;
  tmpnladd[2]=8;
  tmpnladd[3]=12;

  std::string filenameperf = "svxGeometry.txt";
  ifstream finperf(filenameperf.c_str());
  if(!finperf) {cout << "ERROR: Cannot open svxGeometry.txt input file." << endl;}

  for(int ilayer=0; ilayer<4; ilayer++) finperf >> nLadders[ilayer];

  for(int ilayer = 0; ilayer < 4; ilayer++) {
    for(int iladder=0; iladder<tmpnladd[ilayer]; iladder++) {
      // east arm
      for(int ipoint = 0; ipoint < 4; ipoint++) {
         finperf >> x >> y >> z;
         if(ilayer==2) {
           perfladd3[iladder+8][ipoint][0] = x;
           perfladd3[iladder+8][ipoint][1] = y;
           perfladd3[iladder+8][ipoint][2] = z;
           //cout << iladder+8 << " " << ipoint << " " << x << " " << y << " " <, z << endl;
         }
         if(ilayer==3) {
           perfladd4[iladder+12][ipoint][0] = x;
           perfladd4[iladder+12][ipoint][1] = y;
           perfladd4[iladder+12][ipoint][2] = z;
           //cout << iladder+12 << " " << ipoint << " " << x << " " << y << " " <, z << endl;
         }
      }
      // west arm
      for(int ipoint = 0; ipoint < 4; ipoint++) {
         finperf >> x >> y >> z;
         if(ilayer==2) {
           perfladd3[iladder][ipoint][0] = x;
           perfladd3[iladder][ipoint][1] = y;
           perfladd3[iladder][ipoint][2] = z;
           //cout << iladder << " " << ipoint << " " << x << " " << y << " " <, z << endl;
         }
         if(ilayer==3) {
           perfladd4[iladder][ipoint][0] = x;
           perfladd4[iladder][ipoint][1] = y;
           perfladd4[iladder][ipoint][2] = z;
           //cout << iladder << " " << ipoint << " " << x << " " << y << " " <, z << endl;
         }
      }
    }
  }

finperf.close();

//--------------------------------------------------
// read survey geometry from Kieran's file
//--------------------------------------------------
//
  int layer=-1;
  int ladder=-1;
  int sensor=-1;
  char cdummy[1];
//  char cdummy[9];
//  char layerstr[2];
  int layer=-1;

// ladder, sensor, point, xyz
  float sens3[16][5][4][3];
  float sens4[24][6][4][3];
  float xyz0[3],xyz1[3],xyz2[3],xyz3[3],center[3];

  //std::string filename = "Strippixel_Survey_PHENIXCoordinates.txt";
  std::string filenamek = "Strippixel_Survey_fromKieran.txt";
  ifstream fink(filenamek.c_str());
  if(!fink) {cout << "ERROR: Cannot open input file." << endl;}

for(int itmp=0; itmp<224; itmp++) {
//for(int itmp=0; itmp<10; itmp++) {

  fink >> layer >> cdummy >> ladder >> cdummy >> sensor >> cdummy
  >> xyz0[0] >> cdummy >> xyz0[1] >> cdummy >> xyz0[2] >> cdummy
  >> xyz1[0] >> cdummy >> xyz1[1] >> cdummy >> xyz1[2] >> cdummy
  >> xyz2[0] >> cdummy >> xyz2[1] >> cdummy >> xyz2[2] >> cdummy
  >> xyz3[0] >> cdummy >> xyz3[1] >> cdummy >> xyz3[2];

  layer = layer-1;
  sensor = sensor-1;
    cout << "--------- " << layer << " " << ladder << " " << sensor << " " <<
//    xyz0[0] << " " << xyz0[1] << " " << xyz0[2] << " " <<
//    xyz1[0] << " " << xyz1[1] << " " << xyz1[2] << " " <<
//    xyz2[0] << " " << xyz2[1] << " " << xyz2[2] << " " <<
//    xyz3[0] << " " << xyz3[1] << " " << xyz3[2] << endl;
endl;

  if(layer==2 && ladder<8) {
    sens3[ladder][sensor][0][0]=xyz1[0]/10.;
    sens3[ladder][sensor][0][1]=xyz1[1]/10.;
    sens3[ladder][sensor][0][2]=xyz1[2]/10.;
    sens3[ladder][sensor][1][0]=xyz2[0]/10.;
    sens3[ladder][sensor][1][1]=xyz2[1]/10.;
    sens3[ladder][sensor][1][2]=xyz2[2]/10.;
    sens3[ladder][sensor][2][0]=xyz0[0]/10.;
    sens3[ladder][sensor][2][1]=xyz0[1]/10.;
    sens3[ladder][sensor][2][2]=xyz0[2]/10.;
    sens3[ladder][sensor][3][0]=xyz3[0]/10.;
    sens3[ladder][sensor][3][1]=xyz3[1]/10.;
    sens3[ladder][sensor][3][2]=xyz3[2]/10.;
  }
  if(layer==2 && ladder>=8 && ladder<16) {
    sens3[ladder][sensor][0][0]=xyz3[0]/10.;
    sens3[ladder][sensor][0][1]=xyz3[1]/10.;
    sens3[ladder][sensor][0][2]=xyz3[2]/10.;
    sens3[ladder][sensor][1][0]=xyz0[0]/10.;
    sens3[ladder][sensor][1][1]=xyz0[1]/10.;
    sens3[ladder][sensor][1][2]=xyz0[2]/10.;
    sens3[ladder][sensor][2][0]=xyz2[0]/10.;
    sens3[ladder][sensor][2][1]=xyz2[1]/10.;
    sens3[ladder][sensor][2][2]=xyz2[2]/10.;
    sens3[ladder][sensor][3][0]=xyz1[0]/10.;
    sens3[ladder][sensor][3][1]=xyz1[1]/10.;
    sens3[ladder][sensor][3][2]=xyz1[2]/10.;
  }
  if(layer==3 && ladder<12) {
    sens4[ladder][sensor][0][0]=xyz1[0]/10.;
    sens4[ladder][sensor][0][1]=xyz1[1]/10.;
    sens4[ladder][sensor][0][2]=xyz1[2]/10.;
    sens4[ladder][sensor][1][0]=xyz2[0]/10.;
    sens4[ladder][sensor][1][1]=xyz2[1]/10.;
    sens4[ladder][sensor][1][2]=xyz2[2]/10.;
    sens4[ladder][sensor][2][0]=xyz0[0]/10.;
    sens4[ladder][sensor][2][1]=xyz0[1]/10.;
    sens4[ladder][sensor][2][2]=xyz0[2]/10.;
    sens4[ladder][sensor][3][0]=xyz3[0]/10.;
    sens4[ladder][sensor][3][1]=xyz3[1]/10.;
    sens4[ladder][sensor][3][2]=xyz3[2]/10.;
  }
  if(layer==3 && ladder>=12 && ladder<24) {
    sens4[ladder][sensor][0][0]=xyz3[0]/10.;
    sens4[ladder][sensor][0][1]=xyz3[1]/10.;
    sens4[ladder][sensor][0][2]=xyz3[2]/10.;
    sens4[ladder][sensor][1][0]=xyz0[0]/10.;
    sens4[ladder][sensor][1][1]=xyz0[1]/10.;
    sens4[ladder][sensor][1][2]=xyz0[2]/10.;
    sens4[ladder][sensor][2][0]=xyz2[0]/10.;
    sens4[ladder][sensor][2][1]=xyz2[1]/10.;
    sens4[ladder][sensor][2][2]=xyz2[2]/10.;
    sens4[ladder][sensor][3][0]=xyz1[0]/10.;
    sens4[ladder][sensor][3][1]=xyz1[1]/10.;
    sens4[ladder][sensor][3][2]=xyz1[2]/10.;
  }

} // end loop over all sensors

fink.close();


/*
//--------------------------------------------------
// now read survey geometry (Jason's format)
//--------------------------------------------------
//
  int layer=-1;
  int ladder=-1;
  int sensor=-1;
  char cdummy[9];
  char layerstr[2];
  char side;

// ladder, sensor, point, xyz
  float sens3[16][5][4][3];
  float sens4[24][6][4][3];
  float xyz0[3],xyz1[3],xyz2[3],xyz3[3],center[3];

  std::string filename = "Strippixel_Survey_PHENIXCoordinates.txt";
  ifstream fin(filename.c_str());
  if(!fin) {cout << "ERROR: Cannot open input file." << endl;}

for(int itmp=0; itmp<(5*8+6*12+5*8+6*12); itmp++) {

  fin >> layerstr >> ladder >> cdummy >> sensor >> xyz0[0] >> xyz0[1] >> xyz0[2]
  >> xyz1[0] >> xyz1[1] >> xyz1[2] >> xyz2[0] >> xyz2[1] >> xyz2[2] >> xyz3[0] >> xyz3[1]
  >> xyz3[2] >> center[0] >> center[1] >> center[2];

  if(layerstr[0]=='3') layer = 3; if(layerstr[0]=='4') layer = 4;
  side = layerstr[1];
  sensor=sensor-1;

//  if(layer==3 && ladder==0 && sensor==0) {
//    cout << "--------- " << layerstr << " " << layer << " " << side << " " << ladder << " " << sensor << endl;
//    cout << xyz0[0] << " " << xyz0[1] << " " << xyz0[2] << endl;
//    cout << xyz1[0] << " " << xyz1[1] << " " << xyz1[2] << endl;
//    cout << xyz2[0] << " " << xyz2[1] << " " << xyz2[2] << endl;
//    cout << xyz3[0] << " " << xyz3[1] << " " << xyz3[2] << endl;
//  }
//  if(layer==3 && ladder==8 && sensor==0) {
//    cout << "--------- " << layerstr << " " << layer << " " << side << " " << ladder << " " << sensor << endl;
//    cout << xyz0[0] << " " << xyz0[1] << " " << xyz0[2] << endl;
//    cout << xyz1[0] << " " << xyz1[1] << " " << xyz1[2] << endl;
//    cout << xyz2[0] << " " << xyz2[1] << " " << xyz2[2] << endl;
//    cout << xyz3[0] << " " << xyz3[1] << " " << xyz3[2] << endl;
//  }

  if(layer==3 && side=='W') {
    sens3[ladder][sensor][0][0]=xyz1[0]/10.;
    sens3[ladder][sensor][0][1]=xyz1[1]/10.;
    sens3[ladder][sensor][0][2]=xyz1[2]/10.;
    sens3[ladder][sensor][1][0]=xyz2[0]/10.;
    sens3[ladder][sensor][1][1]=xyz2[1]/10.;
    sens3[ladder][sensor][1][2]=xyz2[2]/10.;
    sens3[ladder][sensor][2][0]=xyz0[0]/10.;
    sens3[ladder][sensor][2][1]=xyz0[1]/10.;
    sens3[ladder][sensor][2][2]=xyz0[2]/10.;
    sens3[ladder][sensor][3][0]=xyz3[0]/10.;
    sens3[ladder][sensor][3][1]=xyz3[1]/10.;
    sens3[ladder][sensor][3][2]=xyz3[2]/10.;
  }
  if(layer==4 && side=='W') {
    sens4[ladder][sensor][0][0]=xyz1[0]/10.;
    sens4[ladder][sensor][0][1]=xyz1[1]/10.;
    sens4[ladder][sensor][0][2]=xyz1[2]/10.;
    sens4[ladder][sensor][1][0]=xyz2[0]/10.;
    sens4[ladder][sensor][1][1]=xyz2[1]/10.;
    sens4[ladder][sensor][1][2]=xyz2[2]/10.;
    sens4[ladder][sensor][2][0]=xyz0[0]/10.;
    sens4[ladder][sensor][2][1]=xyz0[1]/10.;
    sens4[ladder][sensor][2][2]=xyz0[2]/10.;
    sens4[ladder][sensor][3][0]=xyz3[0]/10.;
    sens4[ladder][sensor][3][1]=xyz3[1]/10.;
    sens4[ladder][sensor][3][2]=xyz3[2]/10.;
  }

  if(layer==3 && side=='E') {
    sens3[ladder][sensor][0][0]=xyz3[0]/10.;
    sens3[ladder][sensor][0][1]=xyz3[1]/10.;
    sens3[ladder][sensor][0][2]=xyz3[2]/10.;
    sens3[ladder][sensor][1][0]=xyz0[0]/10.;
    sens3[ladder][sensor][1][1]=xyz0[1]/10.;
    sens3[ladder][sensor][1][2]=xyz0[2]/10.;
    sens3[ladder][sensor][2][0]=xyz2[0]/10.;
    sens3[ladder][sensor][2][1]=xyz2[1]/10.;
    sens3[ladder][sensor][2][2]=xyz2[2]/10.;
    sens3[ladder][sensor][3][0]=xyz1[0]/10.;
    sens3[ladder][sensor][3][1]=xyz1[1]/10.;
    sens3[ladder][sensor][3][2]=xyz1[2]/10.;
  }
  if(layer==4 && side=='E') {
    sens4[ladder][sensor][0][0]=xyz3[0]/10.;
    sens4[ladder][sensor][0][1]=xyz3[1]/10.;
    sens4[ladder][sensor][0][2]=xyz3[2]/10.;
    sens4[ladder][sensor][1][0]=xyz0[0]/10.;
    sens4[ladder][sensor][1][1]=xyz0[1]/10.;
    sens4[ladder][sensor][1][2]=xyz0[2]/10.;
    sens4[ladder][sensor][2][0]=xyz2[0]/10.;
    sens4[ladder][sensor][2][1]=xyz2[1]/10.;
    sens4[ladder][sensor][2][2]=xyz2[2]/10.;
    sens4[ladder][sensor][3][0]=xyz1[0]/10.;
    sens4[ladder][sensor][3][1]=xyz1[1]/10.;
    sens4[ladder][sensor][3][2]=xyz1[2]/10.;
  }

 
} // end loop over all sensors

fin.close();
*/

//-------------------------------------------------------------
// print out results and fill histograms
//-------------------------------------------------------------

std::string filenameout = "svxGeometry_surveyKieran.txt";
ofstream fout(filenameout.c_str());
if(!fout) {cout << "ERROR: Cannot open output file." << endl;}

TFile* outfile = new TFile("test.root","RECREATE");
TH1F* h3 = new TH1F("h3","",200,-2.,2.);
TH1F* h4 = new TH1F("h4","",200,-2.,2.);

// layer 3
for(int il=0; il<16; il++) {
  int tmp1 = 0; if(il%2==0) tmp1=8;
  int index = il/2+tmp1;
// ladder, sensor, point, xyz
// EAST: two first points of the first sensor, two last points of the last sensor
// WEST: inverted
  float diffx=99999.;
  float diffy=99999.;
  float diff=99999.;

float maxdiff=0.2;

// east
  if(tmp1==8) {
    fout << sens3[index][0][1][0] << " " << sens3[index][0][1][1] << " " << sens3[index][0][1][2] << endl;
    fout << sens3[index][0][0][0] << " " << sens3[index][0][0][1] << " " << sens3[index][0][0][2] << endl;
    fout << sens3[index][4][3][0] << " " << sens3[index][4][3][1] << " " << sens3[index][4][3][2] << endl;
    fout << sens3[index][4][2][0] << " " << sens3[index][4][2][1] << " " << sens3[index][4][2][2] << endl;
    diffx = sens3[index][0][1][0] - perfladd3[index][0][0];
    diffy = sens3[index][0][1][1] - perfladd3[index][0][1];
    diff = sqrt(diffx*diffx+diffy*diffy);
    if(diff>maxdiff) cout << "bad layer=3, ladder="<<index<<", point=0, diff="<<diff<<endl; 
    h3->Fill(diff);
    diffx = sens3[index][0][0][0] - perfladd3[index][1][0];
    diffy = sens3[index][0][0][1] - perfladd3[index][1][1];
    diff = sqrt(diffx*diffx+diffy*diffy);
    if(diff>maxdiff) cout << "bad layer=3, ladder="<<index<<", point=1, diff="<<diff<<endl; 
    h3->Fill(diff);
    diffx = sens3[index][4][3][0] - perfladd3[index][2][0];
    diffy = sens3[index][4][3][1] - perfladd3[index][2][1];
    diff = sqrt(diffx*diffx+diffy*diffy);
    if(diff>maxdiff) cout << "bad layer=3, ladder="<<index<<", point=2, diff="<<diff<<endl; 
    h3->Fill(diff);
    diffx = sens3[index][4][2][0] - perfladd3[index][3][0];
    diffy = sens3[index][4][2][1] - perfladd3[index][3][1];
    diff = sqrt(diffx*diffx+diffy*diffy);
    if(diff>maxdiff) cout << "bad layer=3, ladder="<<index<<", point=3, diff="<<diff<<endl; 
    h3->Fill(diff);
  }
//west
  if(tmp1==0) {
    fout << sens3[index][4][1][0] << " " << sens3[index][4][1][1] << " " << sens3[index][4][1][2] << endl;
    fout << sens3[index][4][0][0] << " " << sens3[index][4][0][1] << " " << sens3[index][4][0][2] << endl;
    fout << sens3[index][0][3][0] << " " << sens3[index][0][3][1] << " " << sens3[index][0][3][2] << endl;
    fout << sens3[index][0][2][0] << " " << sens3[index][0][2][1] << " " << sens3[index][0][2][2] << endl;
    diffx = sens3[index][4][1][0] - perfladd3[index][0][0];
    diffy = sens3[index][4][1][1] - perfladd3[index][0][1];
    diff = sqrt(diffx*diffx+diffy*diffy);
    if(diff>maxdiff) cout << "bad layer=3, ladder="<<index<<", point=0, diff="<<diff<<endl; 
    h3->Fill(diff);
    diffx = sens3[index][4][0][0] - perfladd3[index][1][0];
    diffy = sens3[index][4][0][1] - perfladd3[index][1][1];
    diff = sqrt(diffx*diffx+diffy*diffy);
    if(diff>maxdiff) cout << "bad layer=3, ladder="<<index<<", point=1, diff="<<diff<<endl; 
    h3->Fill(diff);
    diffx = sens3[index][0][3][0] - perfladd3[index][2][0];
    diffy = sens3[index][0][3][1] - perfladd3[index][2][1];
    diff = sqrt(diffx*diffx+diffy*diffy);
    if(diff>maxdiff) cout << "bad layer=3, ladder="<<index<<", point=2, diff="<<diff<<endl; 
    h3->Fill(diff);
    diffx = sens3[index][0][2][0] - perfladd3[index][3][0];
    diffy = sens3[index][0][2][1] - perfladd3[index][3][1];
    diff = sqrt(diffx*diffx+diffy*diffy);
    if(diff>maxdiff) cout << "bad layer=3, ladder="<<index<<", point=3, diff="<<diff<<endl; 
    h3->Fill(diff);
  }
  //cout << "------ " << perfladd3[index][0][0] << " " << perfladd3[index][0][1] << " " << perfladd3[index][0][2] << endl;
  //cout << "------ " << perfladd3[index][1][0] << " " << perfladd3[index][1][1] << " " << perfladd3[index][1][2] << endl;
  //cout << "------ " << perfladd3[index][2][0] << " " << perfladd3[index][2][1] << " " << perfladd3[index][2][2] << endl;
  //cout << "------ " << perfladd3[index][3][0] << " " << perfladd3[index][3][1] << " " << perfladd3[index][3][2] << endl;
}

// layer 4
for(int il=0; il<24; il++) {
  int tmp1 = 0; if(il%2==0) tmp1=12;
  int index = il/2+tmp1;
// ladder, sensor, point, xyz
// EAST: two first points of the first sensor, two last points of the last sensor
// WEST: inverted
  float diffx=99999.;
  float diffy=99999.;
  float diff=99999.;

// east
  if(tmp1==12) {
    fout << sens4[index][5][2][0] << " " << sens4[index][5][2][1] << " " << sens4[index][5][2][2] << endl;
    fout << sens4[index][5][3][0] << " " << sens4[index][5][3][1] << " " << sens4[index][5][3][2] << endl;
    fout << sens4[index][0][0][0] << " " << sens4[index][0][0][1] << " " << sens4[index][0][0][2] << endl;
    fout << sens4[index][0][1][0] << " " << sens4[index][0][1][1] << " " << sens4[index][0][1][2] << endl;
    diffx = sens4[index][5][2][0] - perfladd4[index][0][0];
    diffy = sens4[index][5][2][1] - perfladd4[index][0][1];
    diff = sqrt(diffx*diffx+diffy*diffy);
    if(diff>maxdiff) cout << "bad layer=4, ladder="<<index<<", point=0, diff="<<diff<<endl;
    h4->Fill(diff);
    diffx = sens4[index][5][3][0] - perfladd4[index][1][0];
    diffy = sens4[index][5][3][1] - perfladd4[index][1][1];
    diff = sqrt(diffx*diffx+diffy*diffy);
    if(diff>maxdiff) cout << "bad layer=4, ladder="<<index<<", point=1, diff="<<diff<<endl;
    h4->Fill(diff);
    diffx = sens4[index][0][0][0] - perfladd4[index][2][0];
    diffy = sens4[index][0][0][1] - perfladd4[index][2][1];
    diff = sqrt(diffx*diffx+diffy*diffy);
    if(diff>maxdiff) cout << "bad layer=4, ladder="<<index<<", point=2, diff="<<diff<<endl;
    h4->Fill(diff);
    diffx = sens4[index][0][1][0] - perfladd4[index][3][0];
    diffy = sens4[index][0][1][1] - perfladd4[index][3][1];
    diff = sqrt(diffx*diffx+diffy*diffy);
    if(diff>maxdiff) cout << "bad layer=4, ladder="<<index<<", point=3, diff="<<diff<<endl;
    h4->Fill(diff);
  }
// west
  if(tmp1==0) {
    fout << sens4[index][0][2][0] << " " << sens4[index][0][2][1] << " " << sens4[index][0][2][2] << endl;
    fout << sens4[index][0][3][0] << " " << sens4[index][0][3][1] << " " << sens4[index][0][3][2] << endl;
    fout << sens4[index][5][0][0] << " " << sens4[index][5][0][1] << " " << sens4[index][5][0][2] << endl;
    fout << sens4[index][5][1][0] << " " << sens4[index][5][1][1] << " " << sens4[index][5][1][2] << endl;
    diffx = sens4[index][0][2][0] - perfladd4[index][0][0];
    diffy = sens4[index][0][2][1] - perfladd4[index][0][1];
    diff = sqrt(diffx*diffx+diffy*diffy);
    if(diff>maxdiff) cout << "bad layer=4, ladder="<<index<<", point=0, diff="<<diff<<endl;
    h4->Fill(diff);
    diffx = sens4[index][0][3][0] - perfladd4[index][1][0];
    diffy = sens4[index][0][3][1] - perfladd4[index][1][1];
    diff = sqrt(diffx*diffx+diffy*diffy);
    if(diff>maxdiff) cout << "bad layer=4, ladder="<<index<<", point=1, diff="<<diff<<endl;
    h4->Fill(diff);
    diffx = sens4[index][5][0][0] - perfladd4[index][2][0];
    diffy = sens4[index][5][0][1] - perfladd4[index][2][1];
    diff = sqrt(diffx*diffx+diffy*diffy);
    if(diff>maxdiff) cout << "bad layer=4, ladder="<<index<<", point=2, diff="<<diff<<endl;
    h4->Fill(diff);
    diffx = sens4[index][5][1][0] - perfladd4[index][3][0];
    diffy = sens4[index][5][1][1] - perfladd4[index][3][1];
    diff = sqrt(diffx*diffx+diffy*diffy);
    if(diff>maxdiff) cout << "bad layer=4, ladder="<<index<<", point=3, diff="<<diff<<endl;
    h4->Fill(diff);
  }
  //cout << "------ " << perfladd4[index][0][0] << " " << perfladd4[index][0][1] << " " << perfladd4[index][0][2] << endl;
  //cout << "------ " << perfladd4[index][1][0] << " " << perfladd4[index][1][1] << " " << perfladd4[index][1][2] << endl;
  //cout << "------ " << perfladd4[index][2][0] << " " << perfladd4[index][2][1] << " " << perfladd4[index][2][2] << endl;
  //cout << "------ " << perfladd4[index][3][0] << " " << perfladd4[index][3][1] << " " << perfladd4[index][3][2] << endl;
}

fout.close();

outfile->Write();
outfile->Close();

}


