// $Id: get_channels.C,v 1.3 2008/06/24 13:05:43 hpereira Exp $
// list dead channels that match a given packet ID
void get_channels( void )
{
  
  gSystem->Load("libmutgeom.so");
  gSystem->Load("libfun4all.so");
  
  unsigned int packet_id = 11055;
  for( unsigned int arm=0; arm < 2; arm++ )
  {
    cout << "get_channels - arm: " << arm << endl;
    MutDCMChannelMap channel_map( new MutArm( arm ) );
    channel_map.setVerbosity( 2 );
    for( int channel = 0; channel < 1000 ; channel++ )
    { channel_map.getMutStrip( packet_id, channel ); }
    cout << endl;
  }
  
}
