// $Id: PHKey.cxx,v 1.2 2006/05/04 15:36:13 hpereira Exp $

/*!
	 \file PHKey.cxx
	 \brief  associated object technology
	 \author S.Kelly H.Pereira
	 \version $Revision: 1.2 $
	 \date $Date: 2006/05/04 15:36:13 $
*/

#include<PHKey.hh>
#include<iostream>

ClassImp(Key)
ClassImp(PHKey)
ClassImp(PHKeyList)

void PHKeyList::print(std::ostream& os) const {
	typedef PHKeyList::key_list_type::const_iterator const_iterator;
	for(const_iterator iter = _key_list.begin(); iter != _key_list.end(); ++iter)
	iter->print(os);
}

