void parse_hv_logs( 
  const char* input = "parse_hv_logs.log",
  const char* output = "parse_hv_logs.root" )
{
  
  int run = 0;
  int arm = 0;
  double trip_time = 0;
  double begin_time = 0;
  double end_time = 0;
  double first_time = 0;
  int channel_num = 0;
  
  int n_south = 0;
  int n_north = 0;
  
  const int BUFFER_SIZE=32000;
  const int AUTO_SAVE=16000;
  
  TFile *f = new TFile( output, "RECREATE" );
  TTree *tree = new TTree( "trips", "trips" );
  tree->Branch( "arm", &arm, "arm/I", BUFFER_SIZE );
  tree->Branch( "run", &run, "run/I", BUFFER_SIZE );
  tree->Branch( "channel", &channel_num, "channel/I", BUFFER_SIZE );
  tree->Branch( "trip_time", &trip_time, "trip_time/D", BUFFER_SIZE );
  tree->Branch( "begin_time", &begin_time, "begin_time/D", BUFFER_SIZE );
  tree->Branch( "end_time", &end_time, "end_time/D", BUFFER_SIZE );
  tree->Branch( "first_time", &first_time, "first_time/D", BUFFER_SIZE );

  // current run information, used to identify when new run is started,
  // notably to fill run statistics properly
  int current_run = 0;
  double current_begin_time = 0;
  double current_end_time = 0;

  TTree *run_tree = new TTree( "run_stat", "run trip statistics" );
  run_tree->Branch( "run", &current_run, "run/I", BUFFER_SIZE );
  run_tree->Branch( "begin_time", &current_begin_time, "begin_time/D", BUFFER_SIZE );
  run_tree->Branch( "end_time", &current_end_time, "end_time/D", BUFFER_SIZE );
  run_tree->Branch( "n_south", &n_south, "n_south/I", BUFFER_SIZE );
  run_tree->Branch( "n_north", &n_north, "n_north/I", BUFFER_SIZE );
  

  ifstream in( input );
  char line[512];
  while( in.getline( line, 512, '\n' ) )
  {
    istringstream line_stream( line );
    
    string tag;
    string channel;
    line_stream >> tag >> run >> channel >> trip_time >> begin_time >> end_time >> first_time;        
    if( line_stream.rdstate() & ios::failbit ) continue;
    

    // print out when a new run is found
    if( run != current_run )
    {
      
      // fill run tree if needed
      if( current_run != 0 ) run_tree->Fill();

      // update current run information
      current_run = run;
      current_begin_time = begin_time;
      current_end_time = end_time;
      
      // reset trips count
      n_south = 0;
      n_north = 0;
      
      // print out
      cout << "parse_hv_logs - run: " << run << endl;
      
    }

    // read channel number from string
    sscanf( channel.c_str()+1, "%i", &channel_num );

    // get arm from channel
    if( channel[0] == 'N' ) 
    { 
      arm = 1;
      n_north++;
    } else {
      arm = 0;
      n_south++;
    }

    tree->Fill();
    
  }

  //! fill last run
  if( current_run != 0 ) run_tree->Fill();
  
  cout << "parse_hv_logs - entries: " << tree->GetEntries() << endl;
  f->Write();
  f->Close();
  cout << "parse_hv_logs - completed." << endl;
}
