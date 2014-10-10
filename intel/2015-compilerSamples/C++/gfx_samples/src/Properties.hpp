/*
      Copyright 2005-2014 Intel Corporation.  All Rights Reserved.

    The source code contained or described herein and all
    documents related to the source code ("Material") are owned by 
    Intel Corporation or its suppliers or licensors. Title to the
    Material remains with Intel Corporation or its suppliers and
    licensors. The Material is protected by worldwide copyright
    laws and treaty provisions.  No part of the Material may be
    used, copied, reproduced, modified, published, uploaded,
    posted, transmitted, distributed,  or disclosed in any way
    except as expressly provided in the license provided with the
    Materials.  No license under any patent, copyright, trade
    secret or other intellectual property right is granted to or
    conferred upon you by disclosure or delivery of the Materials,
    either expressly, by implication, inducement, estoppel or
    otherwise, except as expressly provided in the license
    provided with the Materials. 
*/

#pragma once
#include <sstream>
#include <string>
#include <vector>


class Properties : public std::vector<std::pair<std::string, std::string> >
{
	typedef std::pair<std::string, std::string> elem;

	std::istringstream is;
	std::ostringstream os;

	bool look (const char* key, std::string*& value);

public:

	bool write (const char* fname) const;
	bool read  (const char* fname);
	bool readln (char* line);

	void put (const char* key, const char* value);
	bool get (const char* key, char*&      value);

	template <typename T> 
	void put (const char* key, T  value);

	template <typename T> 
	bool get (const char* key, T& value);

	template <typename T> 
	bool get (const char* key, T* ptr, int count);
};
