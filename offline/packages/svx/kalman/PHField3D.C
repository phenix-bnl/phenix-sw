//
//    *************************************
//    *                                   *
//    *            PHField3D.C            *
//    *                                   *
//    *************************************
//
// ********************************************************************************
// GEANT Field Map, for use in converting ROOT maps of PHENIX magnetic field
// writting by Michael Stone, July 2011

#include "PHField3D.h"
#include <set>

using namespace std;

#ifndef USEG4
#define cm 1.
#define deg (M_PI/180.)
#define rad 1.
#define gauss 1.
#endif

PHField3D::PHField3D( const char* filename, int verb ) :
  scale_factor_(1.),
  maxz_(10000),
  minz_(-10000),
  verb_(verb)
{    
  cout << "\n================ Begin Construct Mag Field =====================" << endl;
  cout << "\n-----------------------------------------------------------"
       << "\n      Magnetic field Module - Verbosity:" << verb_ 
       << "\n-----------------------------------------------------------";
  cout << "\n ---> " "Reading the field grid from " << filename << " ... " << endl;
  
  // open file
  TFile *rootinput = TFile::Open(filename);
  rootinput->cd();
  
  //  get root NTuple objects
  TNtuple *field_map = (TNtuple*)gDirectory->Get("map");
  Float_t ROOT_Z,  ROOT_R,  ROOT_PHI;  
  Float_t ROOT_BZ, ROOT_BR, ROOT_BPHI;       
  field_map->SetBranchAddress("z",    &ROOT_Z);
  field_map->SetBranchAddress("r",    &ROOT_R);
  field_map->SetBranchAddress("phi",  &ROOT_PHI);
  field_map->SetBranchAddress("bz",   &ROOT_BZ);
  field_map->SetBranchAddress("br",   &ROOT_BR);
  field_map->SetBranchAddress("bphi", &ROOT_BPHI);
 
  // get the number of entries in the tree
  int nz, nr, nphi;
  nz = field_map->GetEntries("z>-1e6");
  nr = field_map->GetEntries("r>-1e6");
  nphi = field_map->GetEntries("phi>-1e6");
  int NENTRIES = field_map->GetEntries();

  // run checks on entries
  cout << " ---> The field grid contained " << NENTRIES << " entries" << endl;
  if( verb_>0 ){
    cout << "\n  NENTRIES should be the same as the following values:"
         << "\n  [ Number of values r,phi,z: "
         << nr << " " << nphi << " " << nz << " ]! " << endl;
  }

  if(nz!=nr || nz!=nphi || nr!=nphi){ 
    cout << "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" 
         << "\n The file you entered is not a \"table\" of values" 
         << "\n Something very likely went oh so wrong" 
         << "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" << endl;
  }

  // Keep track of the unique z, r, phi values in the grid using sets
  std::set<float> z_set, r_set, phi_set;

  // Sort the entries to get rid of any stupid ordering problems...

  // We copy the TNtuple into a std::map (which is always sorted)
  // using a 3-tuple of (z, r, phi) so it is sorted in z, then r, then
  // phi.
  if( verb_>0 ) { cout << "  --> Sorting Entries..." << endl; }
  std::map<trio,trio> sorted_map;
  for( int i=0; i<field_map->GetEntries(); i++){
    field_map->GetEntry(i);
    trio coord_key( ROOT_Z, ROOT_R, ROOT_PHI );
    trio field_val( ROOT_BZ, ROOT_BR, ROOT_BPHI );
    sorted_map[coord_key] = field_val;

    z_set.insert(ROOT_Z*cm);
    r_set.insert(ROOT_R*cm);
    phi_set.insert(ROOT_PHI*deg);
  }

  // couts for assurance
  if(verb_>4){
    map<trio,trio>::iterator it=sorted_map.begin();
    print_map(it);     
    float last_z=it->first.get<0>();
    for ( it=sorted_map.begin(); it!=sorted_map.end(); it++ ){
      if( it->first.get<0>() != last_z ) {
        last_z=it->first.get<0>();
        print_map( it );
      }
    }
  }
  
  if(verb_>0){ cout << "  --> Putting entries into containers... " <<  endl; }
  map<trio,trio>::iterator iter=sorted_map.begin();
  float z,r,phi;
  float Bz,Br,Bphi;

  // grab the minimum and maximum z values
  //--z = iter->first.get<0>();
  minz_ = *(z_set.begin());
  std::set<float>::iterator ziter = z_set.end();
  --ziter;
  maxz_ = *ziter;
  
  // initialize maps
  nz = z_set.size();
  nr = r_set.size();
  nphi = phi_set.size();
  r_map_.resize( nr, 0 );
  z_map_.resize( nz, 0 );
  phi_map_.resize( nphi, 0 );

  std::copy(z_set.begin(), z_set.end(), z_map_.begin());
  std::copy(phi_set.begin(), phi_set.end(), phi_map_.begin());
  std::copy(r_set.begin(), r_set.end(), r_map_.begin());
  
  // initialize the field map vectors to the correct sizes
  BFieldR_  .resize( nz, vector< vector<float> >(nr, vector<float>(nphi, 0)) );
  BFieldPHI_.resize( nz, vector< vector<float> >(nr, vector<float>(nphi, 0)) );
  BFieldZ_  .resize( nz, vector< vector<float> >(nr, vector<float>(nphi, 0)) );

  // all of this assumes that  z_prev < z , i.e. the table is ordered (as of right now)
  unsigned int ir=0, iphi=0, iz=0;  // useful indexes to keep track of
  iter = sorted_map.begin();
  for( ; iter!=sorted_map.end(); iter++ ) {
    
    // equivalent to ->GetEntry(iter)
    z =    iter-> first.get<0>() * cm;
    r =    iter-> first.get<1>() * cm;  
    phi =  iter-> first.get<2>() * deg; 
    Bz =   iter->second.get<0>() * gauss;
    Br =   iter->second.get<1>() * gauss;
    Bphi = iter->second.get<2>() * gauss;
    
    if( z>maxz_ ) maxz_ = z;
    if( z<minz_ ) minz_ = z;

    // check for change in z value, when z changes we have a ton of updates to do
    if( z != z_map_[iz] ){
      ++iz;
      ir=0;
      iphi=0; // reset indices

    } else if( r != r_map_[ir] ){ // check for change in r value
      ++ir;
      iphi=0;

    } else if( phi != phi_map_[iphi] ){ // change in phi value? (should be every time) 
      ++iphi;
    }

    // shouldn't happen
    if( iz>0 && z < z_map_[iz-1] ){
      cout << "!!!!!!!!! Your map isn't ordered.... z: " << z << " zprev: " << z_map_[iz-1]<< endl;
    }

    BFieldR_  [iz][ir][iphi] = Br;
    BFieldPHI_[iz][ir][iphi] = Bphi;
    BFieldZ_  [iz][ir][iphi] = Bz;

    // you can change this to check table values for correctness
    // print_map prints the values in the root table, and the
    // couts print the values entered into the vectors
    if( fabs(z)<10 && ir<10 /*&& iphi==2*/ && verb_>3){
      print_map(iter);

      cout << " B("
           << r_map_[ir] << ", " 
           << phi_map_[iphi] << ", "
           << z_map_[iz] << "):  (" 
           << BFieldR_[iz][ir][iphi] << ", "
           << BFieldPHI_[iz][ir][iphi] << ", "
           << BFieldZ_[iz][ir][iphi] << ")" << endl;
    }

  } // end loop over root field map file

  cout << "\n ---> ... read file successfully "
       << "\n ---> Z Boundaries ~ zlow, zhigh: " 
       << minz_/cm << "," << maxz_/cm << " cm " << endl;



  rootinput->Close();
  delete rootinput;
  cout << "\n================= End Construct Mag Field ======================\n" << endl;
}

void PHField3D::GetFieldValue(const double point[4], double *Bfield ) const
{
  if(verb_>2)
    cout << "\nPHField3D::GetFieldValue" << endl;
  double x = point[0];
  double y = point[1];
  double z = point[2];
  double r = sqrt(x*x+y*y);
  double phi;
  if(x==0){
    phi = atan2(y,0.00000000001);
  } else {
    phi = atan2(y,x);
  }
  if( phi<0 ) phi += 2*M_PI;  // normalize phi to be over the range [0,2*pi]
  
  // Check that the point is within the defined z region (check r in a second)
  if ( (z>=minz_) && (z<=maxz_) ) {
    
    double BFieldCyl[3];
    double cylpoint[4] = { z, r, phi, 0 };

    // take <z,r,phi> location and return a vector of <Bz, Br, Bphi>
    GetFieldCyl( cylpoint, BFieldCyl );  

    // X direction of B-field ( Bx = Br*cos(phi) - Bphi*sin(phi) 
    Bfield[0] = cos(phi)*BFieldCyl[1] - sin(phi)*BFieldCyl[2]; // unit vector transformations
    
    // Y direction of B-field ( By = Br*sin(phi) + Bphi*cos(phi)
    Bfield[1] = sin(phi)*BFieldCyl[1] + cos(phi)*BFieldCyl[2];
  
    // Z direction of B-field
    Bfield[2] = BFieldCyl[0];

    for(unsigned int i=0; i<3; ++i)
      Bfield[i] *= scale_factor_;

  } else {   // x,y,z is outside of z range of the field map

    Bfield[0] = 0.0;
    Bfield[1] = 0.0;
    Bfield[2] = 0.0;
    if( verb_>2 )
      cout << "!!!!!!!!!! Field point not in defined region (outside of z bounds)" << endl;
  }

  if(verb_>2){ 
    cout << "END PHField3D::GetFieldValue\n" 
         << "  --->  {Bx, By, Bz} : "
         << "< " << Bfield[0] << ", " << Bfield[1] << ", " << Bfield[2] << " >" << endl;
  }

  return;
}

void PHField3D::GetFieldCyl( const double CylPoint[4], double *BfieldCyl ) const
{
  double z = CylPoint[0]; 
  double r = CylPoint[1];
  double phi = CylPoint[2];

  // MGW - have to make sure the values are in the correct range before searching
  if(r<0.) {
    r = -r;
    phi += M_PI;
  }

  double twopi = 2.*M_PI;
  while(phi<0.) {
    phi += twopi;
  }

  while(phi>=twopi) {
    phi -= twopi;
  }

  BfieldCyl[0] = 0.0;
  BfieldCyl[1] = 0.0;
  BfieldCyl[2] = 0.0;

  if( verb_>2 )
    cout << "GetFieldCyl@ <z,r,phi>: {" << z << "," << r << "," << phi << "}" << endl;

  if(z<z_map_[0] || z>z_map_[z_map_.size()-1]) {
    if( verb_>2 ) 
      cout << "!!!! Point not in defined region (radius too large in specific z-plane)" << endl;
    return;
  }

  vector<float>::const_iterator ziter = upper_bound(z_map_.begin(), z_map_.end(), z);
  unsigned int z_index0 = distance(z_map_.begin(), ziter) - 1;
  unsigned int z_index1 = z_index0 + 1;

  vector<float>::const_iterator riter = upper_bound(r_map_.begin(), r_map_.end(), r);
  unsigned int r_index0 = distance(r_map_.begin(), riter) - 1;
  if(r_index0 >= r_map_.size()) {
    if( verb_>2 ) 
      cout << "!!!! Point not in defined region (radius too large in specific z-plane)" << endl;
    return;
  }

  unsigned int r_index1 = r_index0 + 1;
  if(r_index1 >= r_map_.size()) {
    if( verb_>2 ) 
      cout << "!!!! Point not in defined region (radius too large in specific z-plane)" << endl;
    return;
  }

  vector<float>::const_iterator phiiter = upper_bound(phi_map_.begin(), phi_map_.end(), phi);
  unsigned int phi_index0 = distance(phi_map_.begin(), phiiter) - 1;
  unsigned int phi_index1 = phi_index0+1;
  if(phi_index1 >= phi_map_.size())
    phi_index1 = 0;
    
  double Br000 = BFieldR_[z_index0][r_index0][phi_index0];
  double Br001 = BFieldR_[z_index0][r_index0][phi_index1];
  double Br010 = BFieldR_[z_index0][r_index1][phi_index0];
  double Br011 = BFieldR_[z_index0][r_index1][phi_index1];
  double Br100 = BFieldR_[z_index1][r_index0][phi_index0];
  double Br101 = BFieldR_[z_index1][r_index0][phi_index1];
  double Br110 = BFieldR_[z_index1][r_index1][phi_index0];
  double Br111 = BFieldR_[z_index1][r_index1][phi_index1];

  double Bphi000 = BFieldPHI_[z_index0][r_index0][phi_index0];
  double Bphi001 = BFieldPHI_[z_index0][r_index0][phi_index1];
  double Bphi010 = BFieldPHI_[z_index0][r_index1][phi_index0];
  double Bphi011 = BFieldPHI_[z_index0][r_index1][phi_index1];
  double Bphi100 = BFieldPHI_[z_index1][r_index0][phi_index0];
  double Bphi101 = BFieldPHI_[z_index1][r_index0][phi_index1];
  double Bphi110 = BFieldPHI_[z_index1][r_index1][phi_index0];
  double Bphi111 = BFieldPHI_[z_index1][r_index1][phi_index1];

  double Bz000 = BFieldZ_[z_index0][r_index0][phi_index0];
  double Bz001 = BFieldZ_[z_index0][r_index0][phi_index1];
  double Bz100 = BFieldZ_[z_index1][r_index0][phi_index0];
  double Bz101 = BFieldZ_[z_index1][r_index0][phi_index1];
  double Bz010 = BFieldZ_[z_index0][r_index1][phi_index0];
  double Bz110 = BFieldZ_[z_index1][r_index1][phi_index0];
  double Bz011 = BFieldZ_[z_index0][r_index1][phi_index1];
  double Bz111 = BFieldZ_[z_index1][r_index1][phi_index1];

  double zweight = z - z_map_[z_index0];
  double zspacing = z_map_[z_index1] - z_map_[z_index0];
  zweight /= zspacing;

  double rweight = r - r_map_[r_index0];
  double rspacing = r_map_[r_index1] - r_map_[r_index0];
  rweight /= rspacing;

  double phiweight = phi - phi_map_[phi_index0];
  double phispacing = phi_map_[phi_index1] - phi_map_[phi_index0];
  if(phi_index1==0)
    phispacing += 2*M_PI;
  phiweight /= phispacing;

  // Z direction of B-field 
  BfieldCyl[0] =
    (1-zweight)*((1-rweight)*((1-phiweight)*Bz000+phiweight*Bz001) +
                 rweight*((1-phiweight)*Bz010+phiweight*Bz011)) +
    zweight*((1-rweight)*((1-phiweight)*Bz100+phiweight*Bz101) +
             rweight*((1-phiweight)*Bz110+phiweight*Bz111));

  // R direction of B-field
  BfieldCyl[1] =
    (1-zweight)*((1-rweight)*((1-phiweight)*Br000+phiweight*Br001) +
                 rweight*((1-phiweight)*Br010+phiweight*Br011)) +
    zweight*((1-rweight)*((1-phiweight)*Br100+phiweight*Br101) +
             rweight*((1-phiweight)*Br110+phiweight*Br111));

  // PHI Direction of B-field
  BfieldCyl[2] =
    (1-zweight)*((1-rweight)*((1-phiweight)*Bphi000+phiweight*Bphi001) +
                 rweight*((1-phiweight)*Bphi010+phiweight*Bphi011)) +
    zweight*((1-rweight)*((1-phiweight)*Bphi100+phiweight*Bphi101) +
             rweight*((1-phiweight)*Bphi110+phiweight*Bphi111));


  //     cout << "wr: " << rweight << " wz: " << zweight << " wphi: " << phiweight << endl;
  //     cout << "Bz000: " << Bz000 << endl
  //          << "Bz001: " << Bz001 << endl
  //          << "Bz010: " << Bz010 << endl
  //          << "Bz011: " << Bz011 << endl
  //          << "Bz100: " << Bz100 << endl
  //          << "Bz101: " << Bz101 << endl
  //          << "Bz110: " << Bz110 << endl
  //          << "Bz111: " << Bz111 << endl
  //          << "Bz:    " << BfieldCyl[0] << endl << endl;

  if( verb_>2 ) { 
    cout << "End GFCyl Call: <bz,br,bphi> : {" 
         << BfieldCyl[0]/gauss << "," << BfieldCyl[1]/gauss << "," << BfieldCyl[2]/gauss << "}"
         << endl;
  }

  return;
}

// a binary search algorithm that puts the location that "key" would be, into index...
// it returns true if key was found, and false if not. 
bool PHField3D::bin_search( const vector<float>& vec, unsigned start, unsigned end, const float& key, unsigned& index ) const { 

  // Termination condition: start index greater than end index
  if(start > end) {
    index=start;
    return false;
  }
  
  // Find the middle element of the vector and use that for splitting
  // the array into two pieces.
  unsigned middle = start + ((end - start) / 2);
  
  if( vec[middle] == key ) {
    index=middle;
    return true;
  }
  else if( vec[middle] > key ){
    return bin_search(vec, start, middle - 1, key, index);
  }
  return bin_search(vec, middle + 1, end, key, index);  
}

// debug function to print key/value pairs in map
void PHField3D::print_map( map<trio,trio>::iterator& it ) const {

  cout << "    Key: <" 
       << it->first.get<0>()*cm << "," 
       << it->first.get<1>()*cm << "," 
       << it->first.get<2>()*deg << ">" 
       
       << " Value: <" 
       << it->second.get<0>()*gauss << "," 
       << it->second.get<1>()*gauss << "," 
       << it->second.get<2>()*gauss << ">\n";
  
}

//==============================================================
// Wrapper Functions, for debugging
//==============================================================

#ifndef  __CINT__
PHField3DWrapper::PHField3DWrapper(const char* filename) :
  field(filename)
{}

double PHField3DWrapper::GetBx(double x, double y, double z)
{
  double pos[4] = {x*cm, y*cm, z*cm, 0};
  double Bfield[6] = {0};
  field.GetFieldValue(pos,Bfield);
  return Bfield[0]/gauss;
}

double PHField3DWrapper::GetBy(double x, double y, double z)
{
  double pos[4] = {x*cm, y*cm, z*cm, 0};
  double Bfield[6] = {0};
  field.GetFieldValue(pos,Bfield);
  return Bfield[1]/gauss;
}

double PHField3DWrapper::GetBz(double x, double y, double z)
{
  double pos[4] = {x*cm, y*cm, z*cm, 0};
  double Bfield[6] = {0};
  field.GetFieldValue(pos,Bfield);
  return Bfield[2]/gauss;
}

double PHField3DWrapper::GetBCylZ(double z, double r, double phi) 
{
  double pos[4] = {z*cm, r*cm, phi*rad, 0};
  double Bfield[6] = {0};
  field.GetFieldCyl(pos,Bfield);
  return Bfield[0]/gauss;
}

double PHField3DWrapper::GetBCylR(double z, double r, double phi) 
{
  double pos[4] = {z*cm, r*cm, phi*rad, 0};
  double Bfield[6] = {0};
  field.GetFieldCyl(pos,Bfield);
  return Bfield[1]/gauss;
}

double PHField3DWrapper::GetBCylPHI(double z, double r, double phi) 
{
  double pos[4] = {z*cm, r*cm, phi*rad, 0};
  double Bfield[6] = {0};
  field.GetFieldCyl(pos,Bfield);
  return Bfield[2]/gauss;
}
#endif // __CINT__
