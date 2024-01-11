
/*!
	\file TRpcClusCentroid_v1.cxx
	\brief Rpc cluster centroid
	\author H. Pereira Da Costa
  \version $Revision: 1.2 $
  \date    $Date: 2008/08/28 00:50:16 $
*/

#include "TRpcClusCentroid_v1.h"
ClassImp( TRpcClusCentroid_v1 )


using namespace std;

//___________________________________________
void TRpcClusCentroid_v1::print( ostream& os ) const
{
   os 
      << " TRpcClusCentroid_v1 - "
      << " peak strip: " << get_peak_strip()
      << " x: " << get_x() << " y: " << get_y()
			<< " cov: [" << get_covar(0,0) << "," << get_covar(0,1) << "," << get_covar( 1,0 ) << "," << get_covar(1,1) << "]" 
      << " q_peak: " << get_q_peak() 
      << " q_tot: " << get_q_tot()  << " q_tot_error: " << get_q_tot_error() 
			<< " t: " << get_t() << " t_error: " << get_t_error() 
      << std::endl;
}
