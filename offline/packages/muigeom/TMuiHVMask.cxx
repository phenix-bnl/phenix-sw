// $Id: TMuiHVMask.cxx,v 1.3 2009/08/22 13:58:52 hpereira Exp $

/*!
  \file    TMuiHVMask.cxx
  \brief   muid tube efficiencies
  \author  S.Kelly, H. Pereira
  \version $Revision: 1.3 $
  \date    $Date: 2009/08/22 13:58:52 $
*/

#include <boost/array.hpp>
#include <iomanip>
#include <fstream>
#include <sstream>

#include <PdbApplication.hh>
#include <PdbBankID.hh>
#include <PdbBankManager.hh>
#include <PdbCalBank.hh>
#include <PdbMuiTubeEff.hh>

#include "TMuiHVMask.h"
#include "MuiGeomClasses.hh"

using namespace std;

ClassImp(TMuiHVMask)

//________________________________________________________________
TMuiHVMask::EfficiencyPair TMuiHVMask::_effic_twopack( make_pair( 1.0, 0 ) );
TMuiHVMask::private_map TMuiHVMask::_map = TMuiHVMask::private_map(TMuiChannelId::kTwoPacksMaxTotal, &TwoPackHash, TMuiHVMask::_effic_twopack );
TMuiHVMask::Mode TMuiHVMask::_mode = TMuiHVMask::FROM_FILE;
PHTimeStamp TMuiHVMask::_time_stamp;
string TMuiHVMask::_filename_north("tube_eff_north_default.txt");
string TMuiHVMask::_filename_south("tube_eff_south_default.txt");			
MUIGEOM::Verbosity TMuiHVMask::_verbosity = MUIGEOM::NONE;
bool TMuiHVMask::_init_done = false;

//_____________________________________________________________
void TMuiHVMask::set_effic_twopack( const double& eff )
{ _effic_twopack = make_pair( eff, 0 ); }

//_____________________________________________________________
TMuiHVMask::EfficiencyPair TMuiHVMask::get_pair_twopack( const UShort_t& arm, const UShort_t& plane, const UShort_t& panel, const UShort_t& orient, const UShort_t& twopack)
{

  // initialize 
  if( !_init_done ) initialize();
  
  // returns single tube efficiency if use_override is true or initialization failed
  if( _mode == FIXED_VALUE ) return _effic_twopack;
  else return _map[TMuiChannelId( arm, plane, panel, (orient) ? kVERT:kHORIZ, twopack)];

}

//_____________________________________________________________
double TMuiHVMask::get_effic_twopack( const UShort_t& arm, const UShort_t& plane, const UShort_t& panel, const UShort_t& orient, const UShort_t& twopack)
{ return get_pair_twopack( arm, plane, panel, orient, twopack ).first; }
  
//_____________________________________________________________
double TMuiHVMask::get_effic_hvgroup( const UShort_t& arm, const UShort_t& plane, const UShort_t& panel, const UShort_t& orient,  const UShort_t& hvgroup)
{
  
  // returns single tube efficiency if use_override is true or initialization failed
  if( _mode == FIXED_VALUE ) return _effic_twopack.first;

  // retrieve hv group efficiency from hash map
  TMuiPanelGeo* panel_geo = TMuiGeometry::Geom()->getPanel(arm,plane,panel);    
  EOrient_t muid_orient = (orient) ? kVERT:kHORIZ;
  UShort_t ntwopack = panel_geo->getTwoPackCount(muid_orient);

  // Loop over two packs and return the effic for the first hv group
  for(int twopack = 0; twopack<ntwopack; ++twopack){
    TMuiChannelId id(arm,plane,panel,muid_orient,twopack);
    if(id.get_HV_chain_group() == hvgroup) return get_effic_twopack( arm, plane, panel, orient, id.TwoPack() );
  }
  
  return 0.0;
}

//________________________________________________________________
bool TMuiHVMask::initialize()
{  
  
  MUIGEOM::PRINT(cout,"TMuiHVMask::initialize");
  
  // fill map with defaut pair
  for( private_map::iterator iter = _map.begin(); iter != _map.end(); iter++ ) *iter = _effic_twopack;
  
  // check _mode
  if( _mode == FIXED_VALUE ) { cout << "_mode is FIXED_VALUE. Will use _effic_twopack = " << _effic_twopack.first << endl; } 
  else if( _mode == FROM_FILE ) { initialize_from_file(); }
  else if( _mode == FROM_DATABASE ) { initialize_from_database(); }
  else {
    cout << "_mode " << _mode << " is not implemented. will use _effic_twopack = " << _effic_twopack.first << endl; 
    _mode = FIXED_VALUE;
  } 

  _init_done = true;
  dump_average_efficiencies();
  
  MUIGEOM::PRINT(cout,"**");
  return true;
  
}

//________________________________________________________________
bool TMuiHVMask::update_database( PHTimeStamp start, PHTimeStamp stop, const char* comments)
{  
  static const std::string bank_name = "calib.mui.mui.tubeeff";
  static const std::string class_name = "PdbMuiTubeEffBank";
  
  PdbBankID bank_id = 0;
  
  // retrieve bank manager
  PdbBankManager *bank_manager = PdbBankManager::instance();
  if( !bank_manager ) 
  {
    cout << "TMuiHVMask::update_database - no bank_manager instance." << endl;
    return false;
  }
  
  // retrieve application
  PdbApplication *application = bank_manager->getApplication();  
  
  if(!application->startUpdate()) 
  {
    cout << "TMuiHVMask::update_database - Database not writable." << endl;
    application->abort();
    return false;
  }

  // create bank
  PdbCalBank *bank = bank_manager->createBank( class_name.c_str(),bank_id, comments, start, stop, bank_name.c_str() );
  if( !bank ) 
  {
    cout << "TMuiHVMask::update_database - unable to create bank named " << bank_name << "." << endl;
    return false;
  }
  
  // set bank length to its maximum size
  /* 
    the bank length is changed afterwards, when all efficiencies have been written, 
    so that its length match the exact number of twoPacks.
  */
  bank->setLength( TMuiChannelId::kTwoPacksMaxTotal );
  
  // get pointer to geometry
  TMuiGeometry *g = TMuiGeometry::Geom();
  
  // loop over arm, plane, panel and orientation
  unsigned int record( 0 );
  for( unsigned int arm = 0; arm < TMuiChannelId::kArmsTotal; arm++)
  for( unsigned int plane = 0; plane < TMuiChannelId::kPlanesPerArm; plane++ )
  for( unsigned int panel = 0; panel < TMuiChannelId::kPanelsPerPlane; panel++)
  {
    // retrieve pointer to orientation
    TMuiPanelGeo *p = g->getPanel (arm, plane, panel);
    if( !p ) 
    {
      cout << "TMuiHVMask::update_database - unable to get panel " << arm << "," << plane << "," << panel << endl;
      continue;
    }
    
    for( unsigned int orient = 0; orient < TMuiChannelId::kOrientations; orient++)
    {
      unsigned int ntwopack = p->getTwoPackCount ( orient ? kVERT:kHORIZ);
      for( unsigned int twopack = 0; twopack < ntwopack; twopack++)
      {

        // retrieve efficiency and assign
        EfficiencyPair eff_pair( get_pair_twopack( arm, plane, panel, orient, twopack ) );
        
        // retrieve Database record
        PdbMuiTubeEff* tube_eff = (PdbMuiTubeEff*) &bank->getEntry(record);
        if( !tube_eff ) {
          cout << "TMuiHVMask::update_database - unable to cast bank record " << record << endl;
          continue;
        }
        
        // assign location
        tube_eff->set_arm( arm );
        tube_eff->set_plane( plane );
        tube_eff->set_panel( panel );
        tube_eff->set_orientation( orient );
        tube_eff->set_twopack( twopack );
                
        // assign efficiency and entries
        tube_eff->set_eff( eff_pair.first );
        tube_eff->set_entries( eff_pair.second );                
        record++;
        
      }
    }
  }
    
  // resize bank
  bank->setLength( record );
  
  // output
  cout << "PisaPdbUtil::write - writing " << record << " entries to database." << endl;
  cout << "PisaPdbUtil::write - bankname: " << bank_name << " start:" << start << " stop: " << stop << endl;
  
  // try commit modifications
  application->commit( bank );
  delete bank;
  
  return true;
  
}

//________________________________________________________________
bool TMuiHVMask::update_files()
{  
  
  // get pointer to geometry
  TMuiGeometry *g = TMuiGeometry::Geom();
  
  // loop over arm
  for( unsigned int arm = 0; arm < TMuiChannelId::kArmsTotal; arm++)
  {
    
    // retrieve filename
    string file = ( arm == kSOUTH ) ? _filename_south:_filename_north;
    
    // open/check stream
    ofstream out( file.c_str() );
    if( !out ) 
    {
      cout << "TMuiHVMask::update_files - cannot write to " << file << endl;
      continue;
    }
    
    // keep track of number of written records
    unsigned int record( 0 );
    
    // plane, panel and orientation
    for( unsigned int plane = 0; plane < TMuiChannelId::kPlanesPerArm; plane++ )
    for( unsigned int panel = 0; panel < TMuiChannelId::kPanelsPerPlane; panel++)
    {
      // retrieve pointer to orientation
      TMuiPanelGeo *p = g->getPanel (arm, plane, panel);
      if( !p ) 
      {
        cout << "TMuiHVMask::update_database - unable to get panel " << arm << "," << plane << "," << panel << endl;
        continue;
      }
      
      for( unsigned int orient = 0; orient < TMuiChannelId::kOrientations; orient++)
      {
        unsigned int ntwopack = p->getTwoPackCount ( orient ? kVERT:kHORIZ);
        for( unsigned int twopack = 0; twopack < ntwopack; twopack++)
        {
          
          // retrieve efficiency
          EfficiencyPair eff_pair( get_pair_twopack( arm, plane, panel, orient, twopack ) );
          
          // write location
          out << arm << " " << plane << " " << panel << " " << orient << " " << twopack << "    ";
          
          // write efficiency and entries
          out << eff_pair.first << "    " << eff_pair.second << endl;
          record ++;
        }
        
      }
    }
    cout << "PisaPdbUtil::write - writing " << record << " entries to file " << file << endl;
    out.close();
  }
  
  return true;
  
}

//________________________________________________________________
void TMuiHVMask::initialize_from_file()
{  

  cout << "loading efficiencies from files" << endl;
  
  // put filenames into an array
  boost::array<const char*, TMuiChannelId::kArmsTotal> files = {{_filename_north.c_str(),_filename_south.c_str()}};  
  cout << "north filename: " << _filename_north.c_str() << endl;
  cout << "south filename: " << _filename_south.c_str() << endl;
  
  unsigned int records( 0 );
  for(size_t i=0; i<files.size(); ++i)
  {

    ifstream lookup_file;    

    // Input file
    lookup_file.open(files[i]);
    if(!lookup_file.is_open()) 
    {  
      ostringstream what;
      what << "can't find file " << files[i] << ". Will use _effic_twopack = " << _effic_twopack.first << endl;
      MUIGEOM::TRACE( what.str() );
      continue;
    }

    
    // Fill the map
    const int linesize = 512;
    char line[linesize];
    while ((lookup_file.rdstate() & ios::failbit) == 0 ) 
    {
      
      lookup_file.getline( line, linesize, '\n');
    
      if( !strlen( line ) ) continue; 
      istringstream in( line );

      // parse line
      UShort_t arm(0), plane(0), panel(0), orient(0), twopack(0);
      float effic(0);
      in >> arm >> plane  >> panel >> orient >> twopack;
      in >> effic;    
    
      if( in.rdstate() & ios::failbit ) {
        cout << "something wrong with in line \"" << line << "\".\n";
        continue;
      }  
      
      // try load the number of entries
      /*
        depending on the input file, the entries may or may not be found
        if they are not, assign a value of 1 (this to make the calculation of the average possible
      */
      unsigned int entries(1);
      in >> entries;
      if( in.rdstate() & ios::failbit ) entries = 1;
     
      // put into map
      _map[TMuiChannelId( arm, plane, panel, (orient) ? kVERT:kHORIZ, twopack )] = make_pair( effic, entries );
      records++;
    }
    
    lookup_file.close();
    
  }
  
  cout << "read " << records << " entries" << endl;
  
  return;
}     

//________________________________________________________________
void TMuiHVMask::initialize_from_database()
{  

  cout << "loading efficiencies from database" << endl;
  
  static const std::string bank_name = "calib.mui.mui.tubeeff";
  static const std::string class_name = "PdbMuiTubeEffBank";

  PdbBankID bank_id = 0;
  PdbBankManager *bank_manager = PdbBankManager::instance();
  
  if( !bank_manager ) 
  {
    cout << "no bank_manager instance." << endl;
    return;
  }

  PdbApplication *application = bank_manager->getApplication();  
  if( !application ) {
    cout << "no application." << endl;
    return;
  }  
   
  if(!application->startRead()) {
    cout << "PisaPdbUtil::read - Database not readable." << endl;
    application->abort();
    return;
  }  
    
  // read bank
  PdbCalBank *bank = bank_manager->fetchBank( class_name.c_str(), bank_id, bank_name.c_str(), _time_stamp );
  if( !bank ) {
    cout << "PisaPdbUtil::read - unable to fetch bank named " << bank_name << "." << endl;
    return;
  }
  
  cout << "reading bank named " << bank_name << " for timestamp: " << _time_stamp << endl;
  
  // loop over bank entries
  unsigned int records( 0 );
  for( unsigned int i=0; i < bank->getLength(); i++ )
  {
    
    PdbMuiTubeEff* tube_eff = (PdbMuiTubeEff*) &bank->getEntry(i);
    if( !tube_eff ) {
      cout << "unable to cast bank record " << i << endl;
      continue;
    }
    
    // assign efficiency to map
    TMuiChannelId id( 
      tube_eff->arm(),
      tube_eff->plane(),
      tube_eff->panel(),
      tube_eff->orientation() ? kVERT:kHORIZ,
      tube_eff->twopack() );
    _map[id] = make_pair( tube_eff->eff(), tube_eff->entries() );
    records++; 
  }
  
  cout << "read " << records << " entries" << endl;
  return;
  
}

//_____________________________________________________________________________________
void TMuiHVMask::dump_average_efficiencies( void )
{
   
  // get pointer to geometry
  TMuiGeometry *g = TMuiGeometry::Geom();
  
  // loop over arm, plane, panel and orientation
  for( unsigned int arm = 0; arm < TMuiChannelId::kArmsTotal; arm++)
  {
    
    double arm_ave_effic = 0;
    double arm_sum_weight = 0;
    boost::array< double, TMuiChannelId::kPlanesPerArm > ave_effic = {{0}};
    boost::array< double, TMuiChannelId::kPlanesPerArm > sum_weight = {{0}};
    
    for( unsigned int plane = 0; plane < TMuiChannelId::kPlanesPerArm; plane++ )
    {
      for( unsigned int panel = 0; panel < TMuiChannelId::kPanelsPerPlane; panel++)
      {
        // retrieve pointer to orientation
        TMuiPanelGeo *p = g->getPanel (arm, plane, panel);
        if( !p ) 
        {
          cout << "TMuiHVMask::update_database - unable to get panel " << arm << "," << plane << "," << panel << endl;
          continue;
        }
        
        for( unsigned int orient = 0; orient < TMuiChannelId::kOrientations; orient++)
        {
          unsigned int ntwopack = p->getTwoPackCount ( orient ? kVERT:kHORIZ);
          for( unsigned int twopack = 0; twopack < ntwopack; twopack++)
          {
            EfficiencyPair eff_pair( get_pair_twopack( arm, plane, panel, orient, twopack ) );
            double eff( eff_pair.first );
            double weight( eff_pair.second );
            ave_effic[ plane ] += weight*eff;
            sum_weight[ plane ] += weight;
          }
        }
      }
      
      arm_ave_effic += ave_effic[ plane ];
      arm_sum_weight += sum_weight[ plane ];
      if( sum_weight[ plane ] > 0 ) ave_effic[ plane ] /= sum_weight[ plane ];
    }
    
    // check weight
    if( arm_sum_weight > 0 )
    {
      
      // apply weight
      arm_ave_effic /= arm_sum_weight;
    
      // dump
      cout 
        << "average eff calculated from map - " << ( arm== kSOUTH ? "south":"north" ) 
        << setprecision(3) << setiosflags(ios::showpoint) << setiosflags(ios::fixed);
      
      for( unsigned int plane = 0; plane < TMuiChannelId::kPlanesPerArm; plane++ )
      { cout << " " << ave_effic[plane]; }
    
      cout << " average=" << arm_ave_effic << endl;
      
    }
    
  }
    
  return;
  
}
