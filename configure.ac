dnl Process this file with autoconf to produce a configure script.
AC_PREREQ([2.69])
AC_INIT([mailcrypt],[3.5.9],[http://mailcrypt.sourceforge.net/bugs.html])
AC_CONFIG_SRCDIR([mailcrypt.el])

# Checks for programs.
AC_PROG_LN_S
AC_PROG_INSTALL

AC_PATH_PROG(INSTALL_INFO, install-info)
if test "$INSTALL_INFO x" = " x"
then
  INSTALL_INFO=":"
fi
AC_SUBST(INSTALL_INFO)

AC_PATH_PROG(BASENAME, basename)
if test "$BASENAME x" = " x"
then
  AC_MSG_ERROR(Cannot find program "basename" on PATH.
Hint: Try setting BASENAME environment variable or fixing PATH.)
fi

if test -n "$EMACS" && test "$EMACS" = "t"
then
    unset EMACS
fi

AC_CHECK_PROGS(EMACS, emacs xemacs)
if test "$EMACS x" = " x"
then
  AC_MSG_ERROR('Cannot find emacs or xemacs on PATH.
Hint: Try setting EMACS environment variable or fixing PATH.')
fi

RESULT=`$BASENAME $EMACS`
if test "$RESULT" = xemacs
then
  if $EMACS -batch -q --vanilla
  then
    EMACSFLAGS="--vanilla"         # We're running Xemacs 20.x or higher
    AC_SUBST(EMACSFLAGS)
  else
    EMACSFLAGS="-q -no-site-file"  # Still running Xemacs 19.x
  fi
else if test "$RESULT" = emacs
then
  EMACSFLAGS=""
  AC_SUBST(EMACSFLAGS)
fi
fi
EXTRA_SRCS=""
EXTRA_OBJS=""
echo -n checking for timer.el...
if $EMACS -batch -l timer 2>/dev/null
then
  echo timer.el
else
  echo no.  I will install it for you.
  EXTRA_SRCS="timer.el"
  AC_SUBST(EXTRA_SRCS)
  EXTRA_OBJS="timer.elc"
  AC_SUBST(EXTRA_OBJS)
fi

AC_CONFIG_FILES([Makefile
                 tests/Makefile
                 tests/remailer/Makefile])

AC_OUTPUT
