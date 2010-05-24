/* 
 * AUTHOR: miura@da-cha.org
 * DATE: 2005.04.15
 *
 *  repeater to communicate with unix domain socket
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <stdio.h>
#include <sys/un.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>

#ifdef USE_POLL
#include <poll.h>
#endif

#include "udclient.h"

#define BUFSIZE (4096)

#define COMMAND "udclient"
#define VERSION "0.6"

/*
 * data transport routine
 */
static ssize_t ud_transport(int in, int out, char *buf, size_t bufsize)
{
	ssize_t len, wlen, llen;
	char *ptr = buf;

	if ((len = read(in, buf, bufsize)) < 0) {
		if (errno == EINTR)
			return 0;
		else
			return -1;
	}
	llen = len;
	while (llen > 0) {
		if ((wlen = write(out, ptr, llen)) < 0) {
			if (errno == EINTR)
				wlen = 0;
			else
				return -1;
		}
		llen -= wlen;
		ptr += wlen;
	}
	return len;
}

/* unix domain client routine for SOCK_STREAM */
#ifdef USE_POLL

int ud_cli(FILE *in, FILE *out, int sockfd)
{
	struct pollfd pollfd[2];
	int in_eof;
	char *buf;

	if ((buf = (char *)malloc(sizeof(char)*BUFSIZE)) == NULL)
		exit (1);

	pollfd[0].fd = 0; /* stdin */
	pollfd[0].events = POLLIN;
	pollfd[1].fd = sockfd;
	pollfd[1].events = POLLIN;

	in_eof = 0;
	/* transport request & response, until stdin or socket is closed */
	while (1) {
		int ret = poll(pollfd, 2, INFTIM);
		if (ret == -1) break;
		if (pollfd[0].revents == POLLIN) {
			/* transport request from stdin to iiim server */
			if (ud_transport(fileno(in), pollfd[1].fd, buf, BUFSIZE) == 0) {
				in_eof = 1;
				shutdown(sockfd, SHUT_WR); /* send FIN */
				break;
			}
		}
		if (pollfd[1].revents == POLLIN) {
			/* transport response from iiim server to stdout */
			if (ud_transport(pollfd[1].fd, fileno(out), buf, BUFSIZE) <= 0) {
				if (in_eof == 1) {
					break;
				}
				free(buf);
				return 1;
			}
		} 
		/* error check */
		if ((pollfd[1].revents & (POLLNVAL | POLLHUP | POLLERR))) {
			free(buf);
			return 1;
		}
	}
	free(buf);
	return 0;
}
#else /* USE_POLL */
int ud_cli(FILE *in, FILE *out, int sockfd)
{
	int maxfd, in_eof;
	fd_set rset;
	char *buf;

	if ((buf = (char *)malloc(sizeof(char)*BUFSIZE)) == NULL)
		exit (1);

	in_eof = 0;
	FD_ZERO(&rset);
	while (1) {
		if (in_eof == 0) {
			FD_SET(fileno(in), &rset);
		}
		FD_SET(sockfd, &rset);
		maxfd = max(fileno(in), sockfd) + 1;
		select(maxfd, &rset, NULL, NULL, NULL);

		if (FD_ISSET(sockfd, &rset)) { /*socket is readable */
			if (ud_transport(sockfd, fileno(out), buf, BUFSIZE) == 0) {
				if (in_eof == 1) { /* normal termination */
					break;
				} else {
					perror("ud_cli: server terminated prematurely"); 
					free(buf);
					return 1;
				}
			}
		}
		if (FD_ISSET(fileno(in), &rset)) { /* stdin  is readable */
			if (ud_transport(fileno(in), sockfd, buf, BUFSIZE) == 0) {
				in_eof = 1;
				shutdown(sockfd, SHUT_WR); /* send FIN */
				FD_CLR(fileno(in), &rset);
			}
		}
	}
	free(buf);
	return 0;
}
#endif /* USE_POLL */

static void print_help(void)
{
	fprintf(stderr, "usage: " COMMAND " [-v|-h] path\n"
		"\n"
		"-h\t show this message.\n"
		"-v\t print version.\n"
		"\n"
		"Communicate with server through unix domain socket\n"
		"specified by path argument.\n"
		"Written by Hiroshi Miura <miura@da-cha.org>, Jan. 2005\n"
		);
}

static int proc_opt(int argc, char *argv[])
{
	int i = 1;
	char c;

	if (argc < 2) {
		fprintf(stderr, "Error: socket pathname is needed.\n\n");
		return 0;
	}

	while (1) {
		if ((c = getopt(argc, argv, "hv")) == -1) 
			break;

		switch (c) {
		case 'h':
			print_help();
			return 0;
		case 'v':
			fprintf(stderr, COMMAND ": Version " VERSION "\n");
			return 0;
		default:
			fprintf(stderr, "Error: unknown option(s).\n\n");
			print_help();
			return 0;
		}
	}

	if ( argc == i + 1 )
		return i;
	
	if ( argc > i + 1 ) {
		fprintf(stderr, "Error: too many argument(s).\n\n");
	} else {
		fprintf(stderr, "Error: socket pathname is needed.\n\n");
	}
	print_help();
	return 0;
}

int main(int argc, char *argv[])
{
	int sockfd;
	int pathname_i;
	struct sockaddr_un svaddr;
	FILE *in, *out;
	int proto = SOCK_STREAM;

	if ((pathname_i = proc_opt(argc, argv)) <= 0) {
		exit(1);
	}

	in = stdin; out	= stdout; 

	/* create unix domain socket and connect server*/
	bzero(&svaddr, sizeof(svaddr));
	svaddr.sun_family = AF_LOCAL;
	strncpy(svaddr.sun_path, argv[pathname_i], sizeof(svaddr.sun_path) - 1);

	if ((sockfd = socket(PF_LOCAL, proto, 0)) < 0) {
		perror("socket");
		exit(1);
	}
	if (connect(sockfd, (struct sockaddr*)&svaddr, SUN_LEN(&svaddr)) < 0) {
		perror("connect");
		close(sockfd);
		exit(1);
	}

	/* transport data */
	if (!ud_cli(in, out, sockfd)){
		close(sockfd);	
		exit(1);
	}

	close(sockfd);
	exit(0);
}
