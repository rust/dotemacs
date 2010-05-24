#ifndef __UDCLIENT_H__
#define __UDCLIENT_H__

/* Posix.1g renames "Unix domain" as "local IPC".
   But not all systems define AF_LOCAL and AF_LOCAL.  */
#ifndef	AF_LOCAL
#define AF_LOCAL	AF_UNIX
#endif
#ifndef	PF_LOCAL
#define PF_LOCAL	PF_UNIX
#endif
/* Posix.1g defines SHUT_* constants. 
   But not all systems define SHUT_* constants. */
#ifndef SHUT_WR
#define SHUT_WR (1)
#endif

#if defined(USE_POLL) & !defined(INFTIM)
#define INFTIM (-1)
#endif

#define   max(a,b)        ((a) > (b) ? (a) : (b))

#endif
