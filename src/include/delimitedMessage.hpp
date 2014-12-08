/*
 * delimitedMessage.hpp
 *
 *  Created on: Dec 5, 2014
 *      Author: wjiang2
 */

#ifndef DELIMITEDMESSAGE_HPP_
#define DELIMITEDMESSAGE_HPP_
#include <google/protobuf/message.h>
#include <google/protobuf/io/coded_stream.h>
#include <google/protobuf/io/zero_copy_stream_impl.h>

bool writeDelimitedTo(
    const google::protobuf::MessageLite& message,
    google::protobuf::io::ZeroCopyOutputStream & rawOutput);

bool readDelimitedFrom(
    google::protobuf::io::ZeroCopyInputStream & rawInput,
    google::protobuf::MessageLite & message);



#endif /* DELIMITEDMESSAGE_HPP_ */
