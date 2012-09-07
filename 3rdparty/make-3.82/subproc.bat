@echo off
rem Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
rem 2005, 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
rem This file is part of GNU Make.
rem
rem GNU Make is free software; you can redistribute it and/or modify it under
rem the terms of the GNU General Public License as published by the Free
rem Software Foundation; either version 3 of the License, or (at your option)
rem any later version.
rem
rem GNU Make is distributed in the hope that it will be useful, but WITHOUT
rem ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
rem FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for.
rem more details.
rem
rem You should have received a copy of the GNU General Public License along
rem with this program.  If not, see <http://www.gnu.org/licenses/>.

cd w32\subproc
set MAKE=%2
set MAKEFILE=%1
if x%2 == x set MAKE=nmake
%MAKE% /f %MAKEFILE%
cd ..\..
