/* This file is part of RTags (https://github.com/Andersbakken/rtags).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <https://www.gnu.org/licenses/>. */

#ifndef Sandbox_h
#define Sandbox_h

#include <Source.h>
#include <type_traits>

#include "rct/String.h"
#include "rct/Path.h"
#include "rct/Map.h"
#include "rct/Hash.h"
#include "rct/List.h"

namespace Sandbox {
enum ReplaceMode {
    StartsWith,
    Everywhere
};
static const String encodedRoot("[[SBROOT]]");
const Path &root();
void setRoot(const Path &root);
inline bool hasRoot() { return !root().isEmpty(); }

template <typename T, typename std::enable_if<std::is_convertible<String, T>::value, T>::type * = nullptr>
bool encode(T &t, ReplaceMode mode = Everywhere)
{
    const Path &r = root();
    if (mode == Everywhere) {
        return t.replace(r, encodedRoot);
    }

    if (t.startsWith(r)) {
        t.replace(0, r.size(), encodedRoot);
        return true;
    }
    return false;
}

template <typename T,
          typename std::enable_if<std::is_convertible<typename T::mapped_type, String>::value>::type * = nullptr,
          typename std::enable_if<!std::is_convertible<typename T::key_type, String>::value>::type * = nullptr>
bool encode(T &t, ReplaceMode mode = Everywhere)
{
    bool ret = false;
    for (auto &val : t) {
        if (encode(val.second, mode))
            ret = true;
    }

    return ret;
}

template <typename T,
          typename std::enable_if<std::is_convertible<typename T::key_type, String>::value>::type * = nullptr,
          typename std::enable_if<!std::is_convertible<typename T::mapped_type, String>::value>::type * = nullptr>
bool encode(T &t, ReplaceMode mode = Everywhere)
{
    bool ret = false;
    typename T::iterator it = t.begin();
    while (it != t.end()) {
        typename T::key_type key = it->first;
        if (encode(key, mode)) {
            t[key] = it->second;
            t.erase(it++);
            ret = true;
        } else {
            ++it;
        }
    }
    return ret;
}

template <typename T,
          typename std::enable_if<std::is_convertible<typename T::key_type, String>::value>::type * = nullptr,
          typename std::enable_if<std::is_convertible<typename T::mapped_type, String>::value>::type * = nullptr>
bool encode(T &t, ReplaceMode mode = Everywhere)
{
    bool ret = false;
    for (auto &val : t) {
        ret = encode(val.second, mode) || ret;
    }
    return ret;
}

template <typename T,
          typename std::enable_if<std::is_convertible<typename T::value_type, String>::value>::type * = nullptr>
bool encode(T &t, ReplaceMode mode = Everywhere)
{
    bool ret = false;
    for (auto &val : t) {
        ret = encode(val, mode) || ret;
    }
    return ret;
}

template <typename T>
T encoded(const T &t, ReplaceMode mode = Everywhere)
{
    T copy = t;
    encode(copy, mode);
    return copy;
}

template <typename T>
T encoded(T &t, ReplaceMode mode = Everywhere)
{
    encode(t, mode);
    return t;
}

template <typename T, typename std::enable_if<std::is_convertible<String, T>::value, T>::type * = nullptr>
bool decode(T &t, ReplaceMode mode = Everywhere)
{
    const String &r = root();
    if (mode == Everywhere) {
        return t.replace(encodedRoot, r);
    }

    if (t.startsWith(encodedRoot)) {
        t.replace(0, encodedRoot.size(), r);
        return true;
    }
    return false;
}

template <typename T,
          typename std::enable_if<std::is_convertible<typename T::mapped_type, String>::value>::type * = nullptr,
          typename std::enable_if<!std::is_convertible<typename T::key_type, String>::value>::type * = nullptr>
bool decode(T &t, ReplaceMode mode = Everywhere)
{
    bool ret = false;
    for (auto &val : t) {
        if (decode(val.second, mode))
            ret = true;
    }

    return ret;
}

template <typename T,
          typename std::enable_if<std::is_convertible<typename T::key_type, String>::value>::type * = nullptr,
          typename std::enable_if<!std::is_convertible<typename T::mapped_type, String>::value>::type * = nullptr>
bool decode(T &t, ReplaceMode mode = Everywhere)
{
    bool ret = false;
    typename T::iterator it = t.begin();
    while (it != t.end()) {
        typename T::key_type key = it->first;
        if (decode(key, mode)) {
            t[key] = it->second;
            t.erase(it++);
            ret = true;
        } else {
            ++it;
        }
    }
    return ret;
}

template <typename T,
          typename std::enable_if<std::is_convertible<typename T::key_type, String>::value>::type * = nullptr,
          typename std::enable_if<std::is_convertible<typename T::mapped_type, String>::value>::type * = nullptr>
bool decode(T &t, ReplaceMode mode = Everywhere)
{
    bool ret = false;
    for (auto &val : t) {
        ret = decode(val.second, mode) || ret;
    }
    return ret;
}

template <typename T,
          typename std::enable_if<std::is_convertible<typename T::value_type, String>::value>::type * = nullptr>
bool decode(T &t, ReplaceMode mode = Everywhere)
{
    bool ret = false;
    for (auto &val : t) {
        ret = decode(val, mode) || ret;
    }
    return ret;
}

template <typename T>
T decoded(const T &t, ReplaceMode mode = Everywhere)
{
    T copy = t;
    decode(copy, mode);
    return copy;
}

template <typename T>
T decoded(T &t, ReplaceMode mode = Everywhere)
{
    decode(t, mode);
    return t;
}

}

#endif
