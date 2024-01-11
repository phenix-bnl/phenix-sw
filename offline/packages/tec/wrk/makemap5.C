{

int buf[12],buf2[3];
char ch[2];
int packetid[320];
int sect[320];
int side[320];
int plane[320];
int w1[320];
int w2[320];
int crate[320];
int slot[320];
int psadd[320];
int ch1[320];
int ch2[320];
int arm=0;

ifstream infile;
infile.open("map5.dat");

  for(int i=0; i<320; i++) {
    //if(infile.eof()) break;
    infile
    >>buf[0]>>buf[1]>>buf[2]>>buf[3]>>buf[4]>>buf[5]>>buf[6]>>buf[7]>>buf[8]>>buf[9]>>buf[10]>>buf[11]>>ch>>buf2[0]>>buf2[1]>>buf2[2];
    packetid[i]=buf[0];
    sect[i]=buf[1];
    side[i]=buf[2];
    plane[i]=buf[3];
    w1[i]=buf[4];
    w2[i]=buf[5];
    crate[i]=buf[6];
    slot[i]=buf[7];
    psadd[i]=buf[8];
    ch1[i]=buf[9];
    ch2[i]=buf[10];

    ch1[i]-=1; ch2[i]-=1;

    cout <<arm<<" "<<sect[i]<<" "<<side[i]<<" "<<plane[i]<<" "<<w1[i]<<" "<<w2[i]
         <<" "<<crate[i]<<" "<<slot[i]<<" "<<psadd[i]<<" "<<ch1[i]<<" "<<ch2[i]
         <<" "<< packetid[i]<<endl;

  } 

infile.close();

ofstream outfile;
outfile.open("tecmap_database_run5.dat");
for(int i=0; i<320; i++) {
  outfile<<
    arm<<" "<<sect[i]<<" "<<side[i]<<" "<<plane[i]<<" "<<w1[i]<<" "<<w2[i]<<" "<<
    crate[i]<<" "<<slot[i]<<" "<<psadd[i]<<" "<<ch1[i]<<" "<<ch2[i]<<" "<<
    packetid[i] <<endl;
}
outfile.close();


}

