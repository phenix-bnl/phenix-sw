void makeabsgains() {

// TEC Geomertry as defined in dTecGeom Staf table
int dTecGeom_NumWires[6] = { 415, 415, 436, 447, 446, 468 };
int dTecGeom_sectperarm = 4;
int dTecGeom_ActivePlane[6] = { 1, 1, 1, 1, 0, 0 };
float dTecGeom_WireSpacing[6] = { 0.405, 0.415, 0.405, 0.405, 0.415, 0.405 };
float dTecGeom_ywidth = 3.692;
float dTecGeom_phibote = 213.75;
float dTecGeom_phitope = 123.75;
float dTecGeom_phibotw = -33.75;
float dTecGeom_phitopw = 56.25;
float dTecGeom_Radius[6] = { 433.116, 443.883, 454.650, 465.417, 476.177, 486.937 };
float dTecGeom_Zin[6] = { 310.944, 318.654, 326.364, 334.074, 341.784, 349.494 };
int NARMS = 1;
int NSECTORS = 4;
int NPLANES = 6;
int NSIDES = 2;

float tmp,buff[48];
int i,index,totnent,ibuf=0;
int is,ia,ip,iz,iw;


totnent = NARMS*NSECTORS*NPLANES*NSIDES;

for(i=0; i<totnent; i++) { buff[ibuf]=1.0; ibuf++; }

cout << ibuf << " entries in buffer filled." << endl;

const char *tecdb = "tecabsgain_database.txt";
ofstream file;
file.open(tecdb);

  for(int i=0; i<totnent; i++)
  {
    tmp=buff[i];
    file << tmp << endl;
  } 

  cout
  << "makeabsgains:  TEC absolute gains written to " << tecdb << endl
  << "            Total " << i << " " << ibuf << " rows."
  << endl;

}

