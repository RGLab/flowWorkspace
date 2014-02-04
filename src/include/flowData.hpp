/*
 * flowData.hpp
 *
 *  Created on: Apr 13, 2012
 *      Author: wjiang2
 */

#ifndef FLOWDATA_HPP_
#define FLOWDATA_HPP_
#include <vector>
#include <iostream>
#include <string>
#include <valarray>
#include <stdexcept>
#include <Rcpp.h>
using namespace std;
using namespace Rcpp;

#include <boost/config.hpp>
#include <boost/archive/tmpdir.hpp>
#include <boost/archive/text_iarchive.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/xml_iarchive.hpp>
#include <boost/archive/xml_oarchive.hpp>
#include <boost/serialization/scoped_ptr.hpp>
#include <boost/serialization/base_object.hpp>
#include <boost/serialization/utility.hpp>
#include <boost/serialization/list.hpp>
#include <boost/serialization/map.hpp>
#include <boost/serialization/vector.hpp>
#include <boost/serialization/valarray.hpp>
#include <boost/graph/adj_list_serialize.hpp>
#include <boost/serialization/assume_abstract.hpp>
#include <boost/serialization/split_member.hpp>
#include <boost/serialization/version.hpp>
#include <boost/serialization/split_member.hpp>
#include <boost/algorithm/string.hpp>

/*
 * representing one FCS data
 * currently used as a transient copy of flow data (passed from R)
 * resource is released once the gating is done
 */
class flowData{
	friend std::ostream & operator<<(std::ostream &os, const flowData &fdata);
	friend class boost::serialization::access;
private:
	vector<string> params;
	unsigned sampleID;//it is only valid when access cdf version of flowdata, used as index for sample dimension
	valarray<double> data;
	unsigned nEvents;
	bool ignore_case; //whether channel-searching is case sensitive
	template<class Archive>
		    void serialize(Archive &ar, const unsigned int version)
		    {

					ar & BOOST_SERIALIZATION_NVP(params);
					ar & BOOST_SERIALIZATION_NVP(sampleID);
					ar & BOOST_SERIALIZATION_NVP(data);
					ar & BOOST_SERIALIZATION_NVP(nEvents);

		    }
public:
	flowData & operator=(const flowData& source);//explicitly define the copy assignment since the default one is compiler-specific
	flowData();
	flowData(const double* mat,vector<string>,unsigned _nEvents,unsigned _sampleID, bool _ignore_case = false);
	flowData(NumericMatrix mat,unsigned _sampleID, bool _ignore_case=false);
	slice getSlice(string channel);
	void updateSlice(string channel,valarray<double> x);
	valarray<double> subset(string channel);
	/*
	 * accessors
	 */
	void setParams(vector<string> _params);
	vector<string> getParams(){return params;};
	void setEventCount(unsigned _nEvents){nEvents=_nEvents;};
	unsigned getEventsCount(){return nEvents;};
	void setSampleID(unsigned _sampleID){sampleID=_sampleID;};
	unsigned getSampleID(){return sampleID;};

	void clear(){data.resize(0);};
	unsigned dataSize(){return data.size();};
	void getData(double * mat,unsigned nSize);
	valarray<double> getData();
};




#endif /* FLOWDATA_HPP_ */
