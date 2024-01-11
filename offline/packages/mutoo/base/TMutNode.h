// $Id: TMutNode.h,v 1.6 2010/02/25 16:48:56 winter Exp $

//////////////////////////////////////////////////////////////////
/*!
	 \file		TMutNode.h
	 \brief	 Utility class to access data object on the node tree
	 \author	S.Kelly/H.Pereira
	 \version $Revision: 1.6 $
	 \date		$Date: 2010/02/25 16:48:56 $
*/
//////////////////////////////////////////////////////////////////

#ifndef __TMUTNODE_H__
#define __TMUTNODE_H__

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <PHIOArray_v1.hh>
#include <PHException.h>

#include<iostream>
#include<string>

/*! \ingroup classes */
//! Node tree utility class
/*! Class that hides some of the uglies associated with node tree manipulations.	This
class handles unsatisfied request via exception. */

template<class Value> class TMutNode
{

  public:

	/*!	\brief
		Find the first instance of given Value type and given name on
		the node tree. Value object is assumed to live on a PHDataNode.
		If find fails throws runtime_error.
	*/
	static Value* find_node(PHCompositeNode* top_node, const std::string& name)
	{

		// iter to node tree
		//
		PHNodeIterator iter(top_node);

		// retrieve check node
		PHDataNode<Value> *node = static_cast< PHDataNode<Value>* >(iter.findFirst("PHDataNode",name.c_str()));
		if(!node) {
			std::string failure("find_node failed: " + name);
			throw std::runtime_error(DESCRIPTION(failure.c_str()));
		}

		// get a pointer to the requested object or throw
		Value* value_ptr = node->getData();
		if(value_ptr) return value_ptr;

    // node not found. throw an exception, returns 0
    /*
      note: technically, the "return 0" statement is useless,
      because an exception is thrown. However, "insure" seems to
      complain about it missing
    */
    std::string failure("find_node failed: " + name);
    throw std::runtime_error(DESCRIPTION(failure.c_str()));

    return 0;

	}

	/*!	\brief
		Instantiate an instance of Value and append to the node tree under provided
		node. This node created is non-persistent, that is it is a PHDataNode.
	*/
	static Value* new_node(PHCompositeNode* top_node, const std::string& name)
	{

    // See if a node named map_name already exist
    PHNodeIterator iter(top_node);
    PHDataNode<Value> *node = static_cast< PHDataNode<Value>* >(iter.findFirst("PHDataNode",name.c_str()));
    if( node )
    {

      std::cout << "TMutNode::new_node - node " << name.c_str() << " already exists. Using existing node" << std::endl;

    } else {

      node = new PHDataNode<Value>(new Value(), name.c_str());
      top_node->addNode(node);

    }

    return node->getData();

  }

	/*!	\brief
		Instantiate an instance of Value and append to the node tree under provided
		node. The IOCs input io array is coupled to the PHIOArray on the provided DST node.
	*/
	static Value* new_dst_input_node(
		PHCompositeNode* top_node,
		const std::string& map_name,
		PHCompositeNode* dst_node,
		const std::string& io_map_name)
	{

		// Find/check the PHIOArray on the dst node
		PHIOArray* io_array = TMutNode<PHIOArray>::find_io_node(dst_node,io_map_name.c_str());

		// Instantiate an IOC and couple to the PHIOArray on the DST node
		if( !io_array ){

			std::string failure("find_io_node failed: " + io_map_name);
			throw std::runtime_error(DESCRIPTION(failure.c_str()));

		}

		Value* ioc( 0 );

		// See if a node named map_name already exist
		PHNodeIterator iter(top_node);

		// retrieve check node
		PHDataNode<Value> *node = static_cast< PHDataNode<Value>* >(iter.findFirst("PHDataNode",map_name.c_str()));
		if( node )
		{

			std::cout << "TMutNode::new_dst_input_node - node " << map_name.c_str() << " already exists. Using existing node" << std::endl;
			ioc = node->getData();

		} else {

			std::ostringstream what;
			what << "TMutNode::new_dst_input_node - creating node " << map_name.c_str()
			     << " on " << top_node->getName();
			std::cout << what.str() << std::endl;

			// Instantiate IOC with correct key
			ioc = new Value(io_array->get_map_key());

			// Construct data node
			PHDataNode<Value>* data_node = new PHDataNode<Value>(ioc, map_name.c_str());

			// Append to the node tree under top_node
			top_node->addNode(data_node);
		}

		// Couple created ioc to io_array
		ioc->set_readable(true);
		ioc->set_io_map_name(io_map_name);

		// Return pointer to new ioc
		return ioc;
	}

	/*! \brief
		Find the first instance of given Value type and given name on
		the node tree. Value object is assumed to live on a PHIODataNode.
		If find fails throws runtime_error.
	*/
	static Value* find_io_node(PHCompositeNode* top_node, const std::string& name)
	{

		// iter to node tree
		PHNodeIterator iter(top_node);

		// retrieve/check node
		PHIODataNode<Value> *node = static_cast< PHIODataNode<Value>* >(iter.findFirst("PHIODataNode",name.c_str()));
		if( !node ) {
			std::string failure("find_io_node failed: " + name);
			throw std::runtime_error(DESCRIPTION(failure.c_str()));
		}

		// get a pointer to the requested object or throw
		Value* value_ptr = node->getData();
		if(value_ptr) return value_ptr;

    // node not found. Throw an exception, return 0
    /*
      note: technically, the "return 0" statement is useless,
      because an exception is thrown. However, "insure" seems to
      complain about it missing
    */
    std::string failure("find_io_node failed: " + name);
    throw std::runtime_error(DESCRIPTION(failure.c_str()));

    return 0;

	}

  
  // Search a vector of nodes for the one called "name".  Return immediately once it's
  // found, throw exception if not found, returns 0.
  //
  static Value* search_nodes(const std::vector<PHCompositeNode*>& nodes, std::string name)
  {
    for (unsigned int i=0; i<nodes.size(); i++)
      {
	try 
	  {
	    return TMutNode<Value>::find_node(nodes[i], name);
	  }
	catch (...)
	  {
	    continue;
	  }
      }
    throw std::runtime_error(std::string("TMutNode::search_nodes: Failed to find node ") + name);
    return 0;
  }

};

#endif



