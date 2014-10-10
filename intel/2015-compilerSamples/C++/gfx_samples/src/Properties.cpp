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

#include "Properties.hpp"
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <iomanip>
#include <iostream>
#include <fstream>
using namespace std;


bool Properties::write (const char* fname) const
{
	ofstream ofs;
	ofs.open(fname);
	if (!ofs.good())
	{
		cerr << "Properties: failed to open file for write: " << fname << endl;
		return false;
	}

	ofs.precision(15);
	for (const_iterator it = begin(); it != end() && ofs.good(); it++)
		ofs << it->first << "=" << it->second << endl;

	ofs.close();
	if (!ofs.good())
	{
		cerr << "Properties: failed to write file: " << fname << endl;
		return false;
	}

	return true;
}


static void trim (char*&  ptr)
{
	char* end = ptr + strlen(ptr);

	while (isspace(*ptr))
		ptr++;

	while (--end >= ptr && isspace(*end))
		*end = 0;
}

/*
void Properties::read (const wchar_t* fname)
{
	size_t n = 2*wcslen(fname) + 1;
	char* buff = (char*)malloc(n);
	wcstombs(buff, fname, n);
	read(buff);
	free(buff);
}
*/

bool Properties::read (const char* fname)
{
	ifstream ifs;
	ifs.open(fname);
	if (!ifs.good())
	{
		cerr << "Properties: failed to open file for read: " << fname << endl;
		return false;
	}

	char buff[1024];
	while (ifs.good())
	{
		ifs.getline(buff, sizeof(buff));
		//cout << buff << endl;
		readln(buff);
	}

	ifs.close();
	return true;
}


bool Properties::readln (char* buff)
{
	char* ptr = strpbrk(buff, "#");
	if (ptr != 0)
		*ptr = 0;

	ptr = strpbrk(buff, "=");
	if (ptr != 0)
	{
		char* key = buff;
		char* value = ptr+1;
		*ptr = 0;
		trim(key);
		trim(value);
		//cout<< "***<" << key << ">=<" << value<< ">" << endl;
		put(key, (const char*)value);
		return true;
	}
	else
	{
		ptr = buff;
		trim(ptr);
		if (strlen(ptr) != 0)
			cerr << "Properties: line ignored <" << ptr << ">" << endl;
		return false;
	}
}


void Properties::put (const char* key, const char* value)
{
	string* v = 0;
	if (look(key, v))
		*v = value;
	else
	{
		elem e;
		e.first  = key,
		e.second = value;
		push_back(e);
	}
}


bool Properties::get (const char* key, char*& value)
{
	string* v;
	if (look(key, v))
	{
		value = strdup(v->c_str());
		return true;
	}
	return false;
}


bool Properties::look (const char* key, string*& value)
{
	if (key == 0 || strlen(key) == 0)
	{
		cerr << "Properties: empty key" << endl;
	}

	for (iterator it = begin(); it != end(); it++)
		if (it->first.compare(key) == 0)
		{
			value = &it->second;
			return true;
		}

	return false;
}

template <typename T> 
void Properties::put (const char* key, T value) 
{
	os.rdbuf()->pubseekpos(0);
	os << setprecision(15) << value;

	string* v;
	if (look(key, v))
		*v = os.str();
	else
	{
		elem e;
		e.first  = key,
		e.second = os.str();
		push_back(e);
	}
}

template <typename T> 
bool Properties::get (const char* key, T& value)
{
	string* v;
	if (!look(key, v))
		return false;
	
	string s(*v);
	s += (char)0;
	is.str(s);
	is >> value;
	return true;
}

template <typename T> 
bool Properties::get (const char* key, T* ptr, int count)
{
	string* v;
	if (!look(key, v))
		return false;
	
	string s(*v);
	s += (char)0;
	is.str(s);

	for (is.clear(); count > 0 && (is.rdstate() & ios::eofbit) == 0; --count)
	{
		is >> skipws >> *ptr++;
	}

	return true;
}


template 
void Properties::put<int> (const char* key, int value);

template 
bool Properties::get<int> (const char* key, int& value);


template 
void Properties::put<unsigned int> (const char* key, unsigned int value);

template 
bool Properties::get<unsigned int> (const char* key, unsigned int& value);


template  
void Properties::put<float> (const char* key, float value);

template 
bool Properties::get<float> (const char* key, float& value);


template 
void Properties::put<double> (const char* key, double value);

template 
bool Properties::get<double> (const char* key, double& value);


template 
void Properties::put<string> (const char* key, string value);

template <>
bool Properties::get<string> (const char* key, string& value)
{
	string* v;
	if (!look(key, v))
		return false;

	value = *v;
	return true;
}


template <>
void Properties::put<bool> (const char* key, bool value)
{
	put(key, value ? "true" : "false");
}

template <>
bool Properties::get<bool> (const char* key, bool& value)
{
	string* v;
	if (!look(key, v))
		return false;

	if (*v == "true" || *v == "on")
		value = true;
	else if (*v == "false" || *v == "off")
		value = false;
	else
		cerr << "invalid bool value " << key << " = " << *v << endl;

	return true;
}


template 
bool Properties::get<float> (const char* key, float* ptr, int count);


template 
bool Properties::get<double> (const char* key, double* ptr, int count);
