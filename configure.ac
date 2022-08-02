dnl Autoconf configure script for ada-lzma
dnl The MIT License (MIT)
dnl 
dnl Copyright (c) 2015, 2018, 2021, 2022 Stephane Carrez
dnl 
dnl Permission is hereby granted, free of charge, to any person obtaining a copy
dnl of this software and associated documentation files (the "Software"), to deal
dnl in the Software without restriction, including without limitation the rights
dnl to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
dnl copies of the Software, and to permit persons to whom the Software is
dnl furnished to do so, subject to the following conditions:
dnl 
dnl The above copyright notice and this permission notice shall be included in all
dnl copies or substantial portions of the Software.
dnl 
dnl THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
dnl IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
dnl FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
dnl AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
dnl LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
dnl OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
dnl SOFTWARE.

dnl Process this file with autoconf to produce a configure script.

AC_INIT([ada-lzma],[1.1])
AC_CONFIG_SRCDIR([src/lzma.ads])

# Current release settings
LZMA_MAJOR_VERSION=1
LZMA_MINOR_VERSION=1
LZMA_MICRO_VERSION=2
LZMA_VERSION=$LZMA_MAJOR_VERSION.$LZMA_MINOR_VERSION.$LZMA_MICRO_VERSION

# Set the version number of GtkAda
AC_SUBST(LZMA_VERSION)
AC_SUBST(LZMA_MAJOR_VERSION)
AC_SUBST(LZMA_MINOR_VERSION)
AC_SUBST(LZMA_MICRO_VERSION)

AM_GNAT_LIBRARY_PROJECT(lzmada)

AC_CHECK_HEADERS(lzma.h)

lzma_ok=0
AC_CHECK_LIB(lzma,lzma_code,[LIBS="$LIBS -lzma"
  lzma_ok=1])
if test $lzma_ok -eq 0; then
  AC_MSG_ERROR([The liblzma library was not found.  You should compile or install it.
  You can download the liblzma from http://tukaani.org/xz/
  Under Debian systems, you can install it using
    sudo apt-get install liblzma-dev])
fi

AC_CONFIG_FILES([Makefile.conf
lzmada.gpr
])
AC_OUTPUT
