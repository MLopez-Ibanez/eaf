## Process this file with autoconf to produce a configure script.
##
## Copyright (C) 2010         Romain Francois and Dirk Eddelbuettel
## Copyright (C) 2014 - 2019  Dirk Eddelbuettel
##
## Licensed under GNU GPL 2 or later

# The version set here will propagate to other files from here
AC_INIT([eaf],[2.5])
# This doesn't work in MacOS
#AC_INIT([eaf],m4_esyscmd_s(awk -e '/^Version:/ {print $2}' DESCRIPTION))

## Do we need this?
# : ${R_HOME=`R RHOME`}
# if test -z "${R_HOME}"; then
#   echo "could not determine R_HOME"
#   exit 1
# fi
# CC=`"${R_HOME}/bin/R" CMD config CC`
# CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
# CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
# CXX=$(${R_HOME}/bin/R CMD config CXX)
# CXXFLAGS=$("${R_HOME}/bin/R" CMD config CXXFLAGS)
# # Checks for common programs using default macros
# AC_PROG_CC

PKG_CONFIG_NAME="gsl-config"
PKG_DEB_NAME="libgsl-dev"
PKG_RPM_NAME="libgsl-dev"
PKG_BREW_NAME="gsl"
## Use gsl-config to find arguments for compiler and linker flags
##
## Check for non-standard programs: gsl-config(1)
AC_PATH_PROG([GSL_CONFIG], [$PKG_CONFIG_NAME])
## If gsl-config was found, let's use it
if test "${GSL_CONFIG}" != ""; then
    # Use $PKG_CONFIG_NAME for header and linker arguments
    GSL_CFLAGS=`${GSL_CONFIG} --cflags`
    GSL_LIBS=`${GSL_CONFIG} --libs`
else
    # Customize the error
    AC_MSG_ERROR([
  --------------------------- [EAF PACKAGE] --------------------------------
  Configuration failed because $PKG_CONFIG_NAME was not found. Try installing:
  * Debian, Ubuntu, etc: sudo apt-get install $PKG_DEB_NAME
  * Fedora, CentOS, RHEL: sudo yum install $PKG_RPM_NAME
  * Mac OSX: brew install $PKG_BREW_NAME
  If $PKG_CONFIG_NAME is already installed, check that 'pkg-config' is in your
  PATH and PKG_CONFIG_PATH contains a $PKG_CONFIG_NAME.pc file. If pkg-config
  is unavailable you can set INCLUDE_DIR and LIB_DIR manually via:
  R CMD INSTALL --configure-vars='INCLUDE_DIR=... LIB_DIR=...'
  -------------------------- [ERROR MESSAGE] ---------------------------
  cat configure.log
  --------------------------------------------------------------------
  ])
fi

# Now substitute these variables in src/Makevars.in to create src/Makevars
AC_SUBST(GSL_CFLAGS)
AC_SUBST(GSL_LIBS)

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
