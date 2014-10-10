/*
    Copyright 2005-2014 Intel Corporation.  All Rights Reserved.

* The source code contained or described herein and all
* documents related to the source code ("Material") are owned by 
* Intel Corporation or its suppliers or licensors. Title to the
* Material remains with Intel Corporation or its suppliers and
* licensors. The Material is protected by worldwide copyright
* laws and treaty provisions.  No part of the Material may be
* used, copied, reproduced, modified, published, uploaded,
* posted, transmitted, distributed,  or disclosed in any way
* except as expressly provided in the license provided with the
* Materials.  No license under any patent, copyright, trade
* secret or other intellectual property right is granted to or
* conferred upon you by disclosure or delivery of the Materials,
* either expressly, by implication, inducement, estoppel or
* otherwise, except as expressly provided in the license
* provided with the Materials. 
*/

#pragma once

#undef _OPDEF
#define _OPDEF __declspec(target(gfx)) inline

template <class T> struct GFXVector4;

template <class T>
struct GFXVector3
{
    union{
        struct { T r, g, b; };
        struct { T x, y, z; };
    };

    _OPDEF GFXVector3(){}

    _OPDEF GFXVector3(T a_r, T a_g, T a_b)
        :r(a_r), g(a_g), b(a_b){}

    _OPDEF GFXVector3(T x)
        :r(x), g(x), b(x){}

    _OPDEF GFXVector3(const GFXVector3& v)
        :r(v.r){}

    _OPDEF GFXVector3(const GFXVector4<T>& v);

    template<class T2>
    _OPDEF GFXVector3(const GFXVector3<T2>& v)
        :r(T(v.r)), g(T(v.g)), b(T(v.b)) {}

    _OPDEF GFXVector3& operator=(const GFXVector3<T>& v)
    {
        r = v.r; g = v.g; b = v.b; 
        return *this;
    }

    _OPDEF GFXVector3& operator=(T x)
    {
        r = x; g = x; b = x; 
        return *this;
    }

    _OPDEF friend GFXVector3 operator+(const GFXVector3& v1, const GFXVector3& v2)
    {
        return GFXVector3(v1.r + v2.r, v1.g + v2.g, v1.b + v2.b);
    }

    _OPDEF friend GFXVector3 operator-(const GFXVector3& v1, const GFXVector3& v2)
    {
        return GFXVector3(v1.r - v2.r, v1.g - v2.g, v1.b - v2.b);
    }

    _OPDEF friend GFXVector3 operator*(const GFXVector3& v1, const GFXVector3& v2)
    {
        return GFXVector3(v1.r * v2.r, v1.g * v2.g, v1.b * v2.b);
    }

    _OPDEF friend GFXVector3 operator/(const GFXVector3& v1, const GFXVector3& v2)
    {
        return GFXVector3(v1.r / v2.r, v1.g / v2.g, v1.b / v2.b);
    }

    _OPDEF friend GFXVector3 operator^(const GFXVector3& v1, const GFXVector3& v2)
    {
        return GFXVector3(v1.r ^ v2.r, v1.g ^ v2.g, v1.b ^ v2.b);
    }

    _OPDEF friend GFXVector3 operator|(const GFXVector3& v1, const GFXVector3& v2)
    {
        return GFXVector3(v1.r | v2.r, v1.g | v2.g, v1.b | v2.b);
    }

    _OPDEF friend GFXVector3 operator&(const GFXVector3& v1, const GFXVector3& v2)
    {
        return GFXVector3(v1.r & v2.r, v1.g & v2.g, v1.b & v2.b);
    }

    _OPDEF friend GFXVector3 operator>>(const GFXVector3& v1, int count)
    {
        return GFXVector3(v1.r >> count, v1.g >> count, v1.b >> count);
    }

    _OPDEF friend GFXVector3 operator<<(const GFXVector3& v1, int count)
    {
        return GFXVector3(v1.r << count, v1.g << count, v1.b << count);
    }

    _OPDEF friend bool operator==(const GFXVector3& l, const GFXVector3& r)
    {
        return l.x == r.x && l.y == r.y && l.z == r.z;
    }

    _OPDEF friend bool operator!=(const GFXVector3& l, const GFXVector3& r)
    {
        return !(l == r);
    }

    _OPDEF GFXVector3& operator+=(const GFXVector3& v)
    {
        r += v.r; g += v.g; b += v.b; 
        return *this;
    }

    _OPDEF GFXVector3& operator-=(const GFXVector3& v)
    {
        r -= v.r; g -= v.g; b -= v.b; 
        return *this;
    }

    _OPDEF GFXVector3& operator*=(const GFXVector3& v)
    {
        r *= v.r; g *= v.g; b *= v.b; 
        return *this;
    }

    _OPDEF GFXVector3& operator/=(const GFXVector3& v)
    {
        r /= v.r; g /= v.g; b /= v.b; 
        return *this;
    }

    _OPDEF T sum()const
    {
        return r + g + b;
    }

	_OPDEF void unpack_uchar4(unsigned b4)
	{
		r = (T)( b4       & 0xff);
		g = (T)((b4 >> 8) & 0xff);
		b = (T)((b4 >> 16)& 0xff);
	}

	_OPDEF unsigned pack_uchar4()const
	{
		return ((int)r) 
			| (((int)g) << 8) 
			| (((int)b) << 16); 
	}

    _OPDEF friend GFXVector3 min(const GFXVector3& v1, const GFXVector3& v2)
    {
        return GFXVector3(
            v1.r < v2.r ? v1.r : v2.r, 
            v1.g < v2.g ? v1.g : v2.g, 
            v1.b < v2.b ? v1.b : v2.b 
            );
    }

    _OPDEF friend GFXVector3 max(const GFXVector3& v1, const GFXVector3& v2)
    {
        return GFXVector3(
            v1.r > v2.r ? v1.r : v2.r, 
            v1.g > v2.g ? v1.g : v2.g, 
            v1.b > v2.b ? v1.b : v2.b 
            );
    }

    _OPDEF friend GFXVector3 abs(const GFXVector3& v1)
    {
        return GFXVector3(
            v1.r >= T(0) ? v1.r : -v1.r, 
            v1.g >= T(0) ? v1.g : -v1.g, 
            v1.b >= T(0) ? v1.b : -v1.b 
            );
    }

    _OPDEF friend GFXVector3 sinf(const GFXVector3& v)
    {
        return GFXVector3(T(sinf(float(v.r))), T(sinf(float(v.g))), T(sinf(float(v.b)))); 
    }

    _OPDEF friend GFXVector3 cosf(const GFXVector3& v)
    {
        return GFXVector3(T(cosf(float(v.r))), T(cosf(float(v.g))), T(cosf(float(v.b)))); 
    }

    _OPDEF friend GFXVector3 logf(const GFXVector3& v)
    {
        return GFXVector3(T(logf(float(v.r))), T(logf(float(v.g))), T(logf(float(v.b)))); 
    }

    _OPDEF friend GFXVector3 expf(const GFXVector3& v)
    {
        return GFXVector3(T(expf(float(v.r))), T(expf(float(v.g))), T(expf(float(v.b)))); 
    }

    _OPDEF friend GFXVector3 sqrtf(const GFXVector3& v)
    {
        return GFXVector3(T(sqrtf(float(v.r))), T(sqrtf(float(v.g))), T(sqrtf(float(v.b)))); 
    }



};

template <class T>
struct GFXVector4
{
    union{
        struct { T r, g, b, a; };
        struct { T x, y, z, w; };
    };

    _OPDEF GFXVector4(){}

    _OPDEF GFXVector4(T a_r, T a_g, T a_b, T a_a)
        :r(a_r), g(a_g), b(a_b), a(a_a) {}

    _OPDEF GFXVector4(T x)
        :r(x), g(x), b(x), a(x){}

    _OPDEF GFXVector4(const GFXVector4& v)
        :r(v.r), g(v.g), b(v.b), a(v.a) {}

    template<class T2>
    _OPDEF GFXVector4(const GFXVector4<T2>& v)
        :r(T(v.r)), g(T(v.g)), b(T(v.b)), a(T(v.a)) {}

    _OPDEF GFXVector4& operator=(const GFXVector4<T>& v)
    {
        r = v.r; g = v.g; b = v.b; a = v.a;
        return *this;
    }

    _OPDEF GFXVector4& operator=(T x)
    {
        r = x; g = x; b = x; a = x;
        return *this;
    }

    _OPDEF friend GFXVector4 operator+(const GFXVector4& v1, const GFXVector4& v2)
    {
        return GFXVector4(v1.r + v2.r, v1.g + v2.g, v1.b + v2.b, v1.a + v2.a);
    }

    _OPDEF friend GFXVector4 operator-(const GFXVector4& v1, const GFXVector4& v2)
    {
        return GFXVector4(v1.r - v2.r, v1.g - v2.g, v1.b - v2.b, v1.a - v2.a);
    }

    _OPDEF friend GFXVector4 operator*(const GFXVector4& v1, const GFXVector4& v2)
    {
        return GFXVector4(v1.r * v2.r, v1.g * v2.g, v1.b * v2.b, v1.a * v2.a);
    }

    _OPDEF friend GFXVector4 operator/(const GFXVector4& v1, const GFXVector4& v2)
    {
        return GFXVector4(v1.r / v2.r, v1.g / v2.g, v1.b / v2.b, v1.a / v2.a);
    }

    _OPDEF friend GFXVector4 operator^(const GFXVector4& v1, const GFXVector4& v2)
    {
        return GFXVector4(v1.r ^ v2.r, v1.g ^ v2.g, v1.b ^ v2.b, v1.a ^ v2.a);
    }

    _OPDEF friend GFXVector4 operator|(const GFXVector4& v1, const GFXVector4& v2)
    {
        return GFXVector4(v1.r | v2.r, v1.g | v2.g, v1.b | v2.b, v1.a | v2.a);
    }

    _OPDEF friend GFXVector4 operator&(const GFXVector4& v1, const GFXVector4& v2)
    {
        return GFXVector4(v1.r & v2.r, v1.g & v2.g, v1.b & v2.b, v1.a & v2.a);
    }

    _OPDEF friend GFXVector4 operator>>(const GFXVector4& v1, int count)
    {
        return GFXVector4(v1.r >> count, v1.g >> count, v1.b >> count, v1.a >> count);
    }

    _OPDEF friend GFXVector4 operator<<(const GFXVector4& v1, int count)
    {
        return GFXVector4(v1.r << count, v1.g << count, v1.b << count, v1.a << count);
    }

    _OPDEF friend bool operator==(const GFXVector4& l, const GFXVector4& r)
    {
        return l.x == r.x && l.y == r.y && l.z == r.z && l.w == r.w;
    }

    _OPDEF friend bool operator!=(const GFXVector4& l, const GFXVector4& r)
    {
        return !(l == r);
    }

    _OPDEF GFXVector4& operator+=(const GFXVector4& v)
    {
        r += v.r; g += v.g; b += v.b; a += v.a;
        return *this;
    }

    _OPDEF GFXVector4& operator-=(const GFXVector4& v)
    {
        r -= v.r; g -= v.g; b -= v.b; a -= v.a;
        return *this;
    }

    _OPDEF GFXVector4& operator*=(const GFXVector4& v)
    {
        r *= v.r; g *= v.g; b *= v.b; a *= v.a;
        return *this;
    }

    _OPDEF GFXVector4& operator/=(const GFXVector4& v)
    {
        r /= v.r; g /= v.g; b /= v.b; a /= v.a;
        return *this;
    }

    _OPDEF T sum()const
    {
        return r + g + b + a;
    }

    _OPDEF friend GFXVector4 min(const GFXVector4& v1, const GFXVector4& v2)
    {
        return GFXVector4(
            v1.r < v2.r ? v1.r : v2.r, 
            v1.g < v2.g ? v1.g : v2.g, 
            v1.b < v2.b ? v1.b : v2.b, 
            v1.a < v2.a ? v1.a : v2.a 
            );
    }

    _OPDEF friend GFXVector4 max(const GFXVector4& v1, const GFXVector4& v2)
    {
        return GFXVector4(
            v1.r > v2.r ? v1.r : v2.r, 
            v1.g > v2.g ? v1.g : v2.g, 
            v1.b > v2.b ? v1.b : v2.b, 
            v1.a > v2.a ? v1.a : v2.a 
            );
    }

    _OPDEF friend GFXVector4 abs(const GFXVector4& v1)
    {
        return GFXVector4(
            v1.r >= T(0) ? v1.r : -v1.r, 
            v1.g >= T(0) ? v1.g : -v1.g, 
            v1.b >= T(0) ? v1.b : -v1.b, 
            v1.a >= T(0) ? v1.a : -v1.a 
            );
    }

    _OPDEF friend GFXVector4 sinf(const GFXVector4& v)
    {
        return GFXVector4(T(sinf(float(v.r))), T(sinf(float(v.g))), T(sinf(float(v.b))), T(sinf(float(v.a)))); 
    }

    _OPDEF friend GFXVector4 cosf(const GFXVector4& v)
    {
        return GFXVector4(T(cosf(float(v.r))), T(cosf(float(v.g))), T(cosf(float(v.b))), T(cosf(float(v.a)))); 
    }

    _OPDEF friend GFXVector4 logf(const GFXVector4& v)
    {
        return GFXVector4(T(logf(float(v.r))), T(logf(float(v.g))), T(logf(float(v.b))), T(logf(float(v.a)))); 
    }

    _OPDEF friend GFXVector4 expf(const GFXVector4& v)
    {
        return GFXVector4(T(expf(float(v.r))), T(expf(float(v.g))), T(expf(float(v.b))), T(expf(float(v.a)))); 
    }

    _OPDEF friend GFXVector4 sqrtf(const GFXVector4& v)
    {
        return GFXVector4(T(sqrtf(float(v.r))), T(sqrtf(float(v.g))), T(sqrtf(float(v.b))), T(sqrtf(float(v.a)))); 
    }

    _OPDEF friend void AOS2SOA(T * pr, T * pg, T * pb, T * pa, const GFXVector4 * src, int N)
    {
        #pragma unroll
        pr[0:N] = src[0:N].r;
        #pragma unroll
        pg[0:N] = src[0:N].g;
        #pragma unroll
        pb[0:N] = src[0:N].b;
        #pragma unroll
        pa[0:N] = src[0:N].a;
    }

    _OPDEF friend void SOA2AOS(GFXVector4 * dst, const T * pr, const T * pg, const T * pb, const T * pa, int N)
    {
        #pragma unroll
        dst[0:N].r = pr[0:N];
        #pragma unroll
        dst[0:N].g = pg[0:N];
        #pragma unroll
        dst[0:N].b = pb[0:N];
        #pragma unroll
        dst[0:N].a = pa[0:N];
    }

    _OPDEF friend void AOS2SOA(T * pr, T * pg, T * pb, const GFXVector4 * src, int N)
    {
        #pragma unroll
        pr[0:N] = src[0:N].r;
        #pragma unroll
        pg[0:N] = src[0:N].g;
        #pragma unroll
        pb[0:N] = src[0:N].b;
    }

    _OPDEF friend void SOA2AOS(GFXVector4 * dst, const T * pr, const T * pg, const T * pb, int N)
    {
        #pragma unroll
        dst[0:N].r = pr[0:N];
        #pragma unroll
        dst[0:N].g = pg[0:N];
        #pragma unroll
        dst[0:N].b = pb[0:N];
    }
};
     
template <class T>
_OPDEF GFXVector3<T>::GFXVector3(const GFXVector4<T>& v)
    :r(v.r), g(v.g), b(v.b) {}

#undef _OPDEF

typedef GFXVector4<float> GFXFloat4;
typedef GFXVector4<double> GFXDouble4;
typedef GFXVector4<int> GFXInt4;
typedef GFXVector4<unsigned> GFXUInt4;
typedef GFXVector4<short> GFXShort4;
typedef GFXVector4<unsigned short> GFXUShort4;
typedef GFXVector4<char> GFXChar4;
typedef GFXVector4<unsigned char> GFXUChar4;

typedef GFXVector3<float> GFXFloat3;
typedef GFXVector3<double> GFXDouble3;
typedef GFXVector3<int> GFXInt3;
typedef GFXVector3<unsigned> GFXUInt3;
typedef GFXVector3<short> GFXShort3;
typedef GFXVector3<unsigned short> GFXUShort3;
typedef GFXVector3<char> GFXChar3;
typedef GFXVector3<unsigned char> GFXUChar3;
