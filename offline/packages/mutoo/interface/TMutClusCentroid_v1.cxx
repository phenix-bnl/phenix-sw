#include<TMutClusCentroid_v1.hh>
ClassImp(TMutClusCentroid_v1)

//__________________________________________________
TMutClusCentroid_v1::TMutClusCentroid_v1( const TMutClusCentroid& base_ref ):
  TMutClusCentroid( base_ref ),
  _q_tot_error( base_ref.get_q_tot_error() )
{}
  
//__________________________________________________
TMutClusCentroid_v1::TMutClusCentroid_v1( const TMutClusCentroid* base_ptr ):
  TMutClusCentroid( *base_ptr ),
  _q_tot_error( base_ptr->get_q_tot_error() )
{}
