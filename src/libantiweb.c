// Antiweb (C) Doug Hoyte

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <time.h>
#include <limits.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/uio.h>
#include <unistd.h>
#include <errno.h>
#include <pwd.h>
#include <signal.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <dirent.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <inttypes.h>

#ifndef IOV_MAX
#define IOV_MAX UIO_MAXIOV // linux
#endif

#ifdef USE_EPOLL
#include <sys/epoll.h>
#endif

#ifdef USE_KQUEUE
#include <sys/event.h>
#endif

#include "libantiweb.h"

#define BACKLOG 2
#define AW_LARGE_FILE_BUFFER_SIZE (1<<20) // 1mb, will refill buffer at 1/2mb
#define AW_MAX_FILE_SIZE_TO_MMAP (1<<22) // 4mb
#define AW_MAX_WRITEV_SLOTS 8
#define AW_EVENT_BATCH_SIZE 16
#define AW_IN_SOCKET_BUFFER 8
#define AW_OUT_SOCKET_BUFFER 4
#define AW_NUM_ARTIFICIAL_READY_CONNS AW_IN_SOCKET_BUFFER
#define AW_HTTP_LINGER_SECONDS 2
#define AW_READ 1
#define AW_WRITE 2


// Globals:
struct conn *free_conns=NULL;
struct ioblock *free_ioblocks=NULL;
struct conn *conns_in_use=NULL;
int num_conns_in_use=0;
int keepalive_time_in_seconds=65; // just over keepalive default on most browsers - same as nginx
time_t recentish_time=0;
time_t next_timeout_scan=0;
int event_desc=-1;
struct conn *artificial_ready_conns[AW_NUM_ARTIFICIAL_READY_CONNS];
int num_artificial_ready_conns=0;
volatile sig_atomic_t time_for_a_reaping=0;
int fatal_has_already_been_called=0;

char *sep_single_newline = "\n";
char *sep_http = "\r\n\r\n";

struct conn *hub_conn = NULL;
struct conn *logger_conn = NULL;


struct conn *next_timeout_ptr=NULL;
int events_left=0;
#ifdef USE_EPOLL
struct epoll_event events[AW_EVENT_BATCH_SIZE];
#endif
#ifdef USE_KQUEUE
struct kevent events[AW_EVENT_BATCH_SIZE];
#endif


static void do_vectored_write_to_sd(struct conn *c);
static void make_socket_blocking(int sd);



void aw_log(char *file, char *prefix, char *log_msg) {
  struct ioblock *b;
  size_t len, plen, mlen;

  if (logger_conn == NULL) {
    printf("%s%s\n", prefix, log_msg);
    return;
  }

  plen = strlen(prefix);
  mlen = strlen(log_msg);

  if (plen > 50) _exit(-1);
  if (mlen > AW_MAX_MSG_LENGTH) _exit(-1);

  prealloc_ioblock();
  b = free_ioblocks;
  free_ioblocks = free_ioblocks->next;
  b->next = NULL;

  len = snprintf(b->data, AW_IOBLOCK_SIZE-1, "log %s %zd\n%s", file, mlen+plen, prefix);
  if (len == -1 || len >= AW_IOBLOCK_SIZE-1) _exit(-1);

  b->offset = 0;

  if (mlen <= AW_IOBLOCK_SIZE-len) {
    memcpy(b->data + len, log_msg, mlen);
    b->len = len + mlen;
    log_msg += mlen;
    mlen = 0;
  } else {
    memcpy(b->data + len, log_msg, AW_IOBLOCK_SIZE-len);
    b->len = AW_IOBLOCK_SIZE;
    log_msg += AW_IOBLOCK_SIZE-len;
    mlen -= AW_IOBLOCK_SIZE-len;
  }

  if (logger_conn->out == NULL) {
    logger_conn->out = logger_conn->outp = b;
  } else {
    logger_conn->outp->next = b;
    logger_conn->outp = b;
  }
  logger_conn->outlen += b->len;

  while (*log_msg) {
    int amt;

    if (mlen > AW_IOBLOCK_SIZE) amt = AW_IOBLOCK_SIZE;
    else amt = mlen;

    prealloc_ioblock();
    b = free_ioblocks;
    free_ioblocks = free_ioblocks->next;
    b->next = NULL;

    b->offset = 0;
    b->len = amt;
    memcpy(b->data, log_msg, amt);

    log_msg += amt;
    mlen -= amt;

    logger_conn->outp->next = b;
    logger_conn->outp = b;

    logger_conn->outlen += amt;
  }

  aw_event_update(logger_conn);
}


static void aw_logf(char *file, char *prefix, char *fmt, ...) {
  va_list ap;
  char buf[2048];

  va_start(ap, fmt);
  vsnprintf(buf, sizeof(buf), fmt, ap);
  va_end(ap);

  aw_log(file, prefix, buf);
}



static void fatal(const char *fmt, ...) {
  va_list ap;
  char buf[4096];
  int i,len;

  if (fatal_has_already_been_called) _exit(-1);
  fatal_has_already_been_called = 1;

  va_start(ap, fmt);
  vsnprintf(buf, sizeof(buf), fmt, ap);
  va_end(ap);

  fprintf(stderr, "FATAL: %s\n", buf);

  if (logger_conn == NULL) _exit(-1);

  len = strlen(buf);

  for(i=0; i<len; i++)
    if ((buf[i] & 0xff) < 32 || (buf[i] & 0xff) > 127) buf[i] = '?';

  aw_log("syslog", hub_conn ? "WORKER FATAL: " : "HUB FATAL: ", buf);

  make_socket_blocking(logger_conn->sd);

  while(logger_conn->outlen)
    do_vectored_write_to_sd(logger_conn);

  _exit(-1);
}

void aw_fatal(char *reason) {
  fatal("%s", reason);
}


static void my_close(int sd) {
  // No need to check the return value of close() because:
  //   1) We can't do anything about errors anyways.
  //   2) Although technically unspecified, all unixes that I am
  //      aware of will ensure the socket is closed even if close()
  //      returns an error.
  //   3) Trying to re-close a socket after an error will
  //      result in a race condition if we ever start using threads.
  close(sd);
}

static void make_socket_non_blocking(int sd) {
  //fcntl(sd, F_SETFL, fcntl(sd, F_GETFL) | O_NONBLOCK);
  int nb=1;
  if (ioctl(sd, FIONBIO, &nb) == -1)
    fatal("make_socket_non_blocking: ioctl: %s", strerror(errno));
}

static void make_socket_blocking(int sd) {
  //fcntl(sd, F_SETFL, fcntl(sd, F_GETFL) | ~O_NONBLOCK);
  int nb=0;
  if (ioctl(sd, FIONBIO, &nb) == -1)
    fatal("make_socket_blocking: ioctl: %s", strerror(errno));
}

static void make_socket_close_on_exec(int sd) {
  int flags;
  flags = fcntl(sd, F_GETFD);
  if (flags == -1)
    fatal("make_socket_close_on_exec: fcntl(F_GETFD): %s", strerror(errno));
  flags |= FD_CLOEXEC;
  if (fcntl(sd, F_SETFD, flags) == -1)
    fatal("make_socket_close_on_exec: fcntl(F_SETFD): %s", strerror(errno));
}

/*
static void make_socket_not_close_on_exec(int sd) {
  int flags;
  flags = fcntl(sd, F_GETFD);
  if (flags == -1)
    fatal("make_socket_not_close_on_exec: fcntl(F_GETFD): %s", strerror(errno));
  flags |= ~FD_CLOEXEC;
  if (fcntl(sd, F_SETFD, flags) == -1)
    fatal("make_socket_not_close_on_exec: fcntl(F_SETFD): %s", strerror(errno));
}

static void make_socket_no_nagle(int sd) {
  int flag=1;
  if (setsockopt(sd, IPPROTO_TCP, TCP_NODELAY, &flag, sizeof(int)) == -1)
    fatal("make_socket_no_nagle: setsockopt: %s", strerror(errno));
}
*/


int bytes_in_a_ioblock() {
  return sizeof(struct ioblock) + AW_IOBLOCK_SIZE;
}

void prealloc_ioblock() {
  if (free_ioblocks == NULL) {
    free_ioblocks = malloc(sizeof(struct ioblock));
    if (free_ioblocks == NULL)
      fatal("prealloc_ioblock: malloc: %s", strerror(errno));

    free_ioblocks->offset = free_ioblocks->len = 0;

    free_ioblocks->data = malloc(AW_IOBLOCK_SIZE);
    if (free_ioblocks->data == NULL)
      fatal("prealloc_ioblock: malloc: %s", strerror(errno));

    free_ioblocks->next = NULL;
  }
}


static void unalloc_ioblock(struct ioblock *b) {
  if (b == NULL)
    _exit(-1);
  b->next = free_ioblocks;
  free_ioblocks = b;
  b->offset = b->len = 0;
}



int bytes_in_a_conn() {
  return sizeof(struct conn) +
         (INET6_ADDRSTRLEN+1) +
         (sizeof(int) * AW_IN_SOCKET_BUFFER) +
         (sizeof(int) * AW_OUT_SOCKET_BUFFER);
}

// Allocates a new conn struct. Note that this increments
// num_conns_in_use but does not add the new conn to conns_in_use
// which must be done immediately by the caller
static struct conn *alloc_conn() {
  struct conn *c;

  num_conns_in_use++;

  if (free_conns==NULL) {
    c = malloc(sizeof(struct conn));
    if (c == NULL)
      fatal("alloc_conn: malloc: %s", strerror(errno));

    c->ip = malloc(INET6_ADDRSTRLEN+1);
    if (c->ip == NULL)
      fatal("alloc_conn: malloc: %s", strerror(errno));
    c->ip[INET6_ADDRSTRLEN] = '\0';

    c->in_sd_buffer = malloc(sizeof(int) * AW_IN_SOCKET_BUFFER);
    if (c->in_sd_buffer == NULL)
      fatal("alloc_conn: malloc: %s", strerror(errno));
    c->out_sd_buffer = malloc(sizeof(int) * AW_OUT_SOCKET_BUFFER);
    if (c->out_sd_buffer == NULL)
      fatal("alloc_conn: malloc: %s", strerror(errno));
  } else {
    c = free_conns;
    free_conns = free_conns->next;
  }

  c->mmap_start = c->mmap_curr = NULL;
  c->mmap_bytes = 0;
  c->in = c->inp = NULL;
  c->out = c->outp = NULL;
  c->inlen = c->outlen = c->ready = c->limit = 0;
  c->num_in_sd_buffer = c->num_out_sd_buffer = 0;
  c->conntype = 0;
  c->kernel_state = 0;
  c->expiry = 0;
  c->sd = c->fd = -1;
  c->fd_bytes_left = 0;
  c->next = NULL;
  c->sep = c->currsep = NULL;

  c->proxy_mate = NULL;
  c->proxy_len = c->proxy_bufsize = c->proxy_buftrig = 0;

  return c;
}



// unallocates a conn, decrements num_conns_in_use, and removes the conn from conns_in_use
void aw_unalloc_conn(struct conn *c) {
  struct ioblock *b;
  struct conn *tpc;
  int i;

  if (c == hub_conn || c == logger_conn || c == NULL)
    _exit(-1);

  if (c->conntype == 0)
    fatal("aw_unalloc_conn: already unallocated");

  c->conntype = 0;

  if (next_timeout_ptr == c)
    next_timeout_ptr = next_timeout_ptr->next;

  remove_from_artificial_ready_conns(c);

  num_conns_in_use--;

  while(c->in) {
    b = c->in;
    c->in = c->in->next;
    unalloc_ioblock(b);
  }
  c->in = c->inp = NULL;

  while(c->out) {
    b = c->out;
    c->out = c->out->next;
    unalloc_ioblock(b);
  }
  c->out = c->outp = NULL;

  c->inlen = c->outlen = 0;

  if (c->sd != -1) {
    aw_event_remove_all(c);
    my_close(c->sd);
    c->sd = -1;
  }
  c->kernel_state = 0;

  if (c->mmap_start != NULL) {
    if (munmap(c->mmap_start, c->mmap_bytes))
      fatal("aw_unalloc_conn: munmap (%s)", strerror(errno));
    c->mmap_start = c->mmap_curr = NULL;
    c->mmap_bytes = 0;
  }

  if (c->fd != -1) {
    my_close(c->fd);
    c->fd = -1;
  }
  c->fd_bytes_left = 0;

  if (c == conns_in_use) {
    conns_in_use = c->next;
  } else {
    tpc = conns_in_use;
    while (tpc->next && tpc->next != c)
      tpc = tpc->next;

    if (tpc->next == NULL)
      fatal("aw_unalloc_conn: connection not found in conns_in_use");

    tpc->next = c->next;
  }

  for (i=0; i<c->num_in_sd_buffer; i++)
    my_close(c->in_sd_buffer[i]);

  for (i=0; i<c->num_out_sd_buffer; i++)
    my_close(c->out_sd_buffer[i]);

  c->num_in_sd_buffer = c->num_out_sd_buffer = 0;

  c->ip[0] = '\0';
  c->expiry = 0;
  c->sep = c->currsep = NULL;
  c->limit = c->ready = 0;

  if (c->proxy_mate) {
    if (c->proxy_mate->out) aw_touch_conn(c->proxy_mate, 10); // Give it time to flush its buffers, if any. FIXME: #define
    else aw_touch_conn(c->proxy_mate, 0);
    c->proxy_mate->proxy_mate = NULL;
    c->proxy_mate = NULL;
  }
  c->proxy_len = c->proxy_bufsize = c->proxy_buftrig = 0;

  c->next = free_conns;
  free_conns = c;

}



static void add_to_artificial_ready_conns(struct conn *c) {
  int i;

  if (num_artificial_ready_conns >= AW_NUM_ARTIFICIAL_READY_CONNS)
    fatal("add_to_artificial_ready_conns: queue full");

  for (i=0; i<num_artificial_ready_conns; i++)
    if (artificial_ready_conns[i] == c)
      fatal("add_to_artificial_ready_conns: connection already in queue");

  artificial_ready_conns[num_artificial_ready_conns] = c;
  num_artificial_ready_conns++;
}

void remove_from_artificial_ready_conns(struct conn *c) {
  int i;

  for (i=0; i<num_artificial_ready_conns; i++) {
    if (artificial_ready_conns[i] == c) {
      for (; i<num_artificial_ready_conns-1; i++)
        artificial_ready_conns[i] = artificial_ready_conns[i+1];
      num_artificial_ready_conns--;
      return;
    }
  }
}

static struct conn *get_next_artificial_ready_conn() {
  struct conn *c;
  int i;

  if (num_artificial_ready_conns == 0)
    return NULL;

  c = artificial_ready_conns[0];

  for (i=1; i<num_artificial_ready_conns; i++)
    artificial_ready_conns[i-1] = artificial_ready_conns[i];

  num_artificial_ready_conns--;

  return c;
}



void aw_touch_conn(struct conn *c, int seconds) {
  if (seconds == -1) {
    c->expiry = 0;
  } else {
    c->expiry = recentish_time + seconds;
    if (next_timeout_scan) {
      if (c->expiry < next_timeout_scan)
        next_timeout_scan = c->expiry;
    } else {
      next_timeout_scan = c->expiry;
    }
  }
}



// Only call this if conntype==AW_CONNTYPE_UNIX and there are sds on
// out_sd_buffer waiting to be sent. Otherwise, use writev.

static ssize_t writev_to_unix_socket(struct conn *c, struct iovec *vec, int count) {
  struct msghdr msg;
  char ccmsg[CMSG_SPACE(sizeof(int)*AW_OUT_SOCKET_BUFFER)];
  struct cmsghdr *cmsg;
  int rv, i, num_to_transfer;

  if (c->num_out_sd_buffer > AW_OUT_SOCKET_BUFFER)
    fatal("writev_to_unix_socket: num_out_sd_buffer is too large (%d)", c->num_out_sd_buffer);

  if (c->num_out_sd_buffer <= 0)
    fatal("writev_to_unix_socket: num_out_sd_buffer is 0");

  num_to_transfer = c->num_out_sd_buffer;

  msg.msg_name = NULL;
  msg.msg_namelen = 0;
  msg.msg_iov = vec;
  msg.msg_iovlen = count;
  msg.msg_control = ccmsg;
  msg.msg_controllen = CMSG_SPACE(sizeof(int) * num_to_transfer);

  cmsg = CMSG_FIRSTHDR(&msg);
  cmsg->cmsg_level = SOL_SOCKET;
  cmsg->cmsg_type = SCM_RIGHTS;
  cmsg->cmsg_len = CMSG_LEN(sizeof(int) * num_to_transfer);

  memcpy((int *)CMSG_DATA(cmsg), c->out_sd_buffer, sizeof(int) * num_to_transfer);
  msg.msg_controllen = cmsg->cmsg_len;
  msg.msg_flags = 0;

  again:

  rv = sendmsg(c->sd, &msg, 0);

  if (rv == -1 || rv == 0) {
    if (rv == -1) {
      if (errno == EINTR) goto again;
      if (errno == EAGAIN) return -1; // will be logged by caller
    }
    fatal("writev_to_unix_socket: sendmsg: %d (%s)", num_to_transfer, strerror(errno));
  }

  if ((msg.msg_flags & MSG_TRUNC) || (msg.msg_flags & MSG_CTRUNC))
    fatal("writev_to_unix_socket: CMSG got truncated: %d fds, flags = %d", num_to_transfer, msg.msg_flags);

  for (i=0; i<num_to_transfer; i++)
    my_close(c->out_sd_buffer[i]);

  for (i=1; i<c->num_out_sd_buffer; i++)
    c->out_sd_buffer[i-1] = c->out_sd_buffer[i];

  c->num_out_sd_buffer -= num_to_transfer;

  return rv;
}


static ssize_t readv_from_unix_socket(struct conn *c, struct iovec *vec, int count) {
  struct msghdr msg;
  char ccmsg[CMSG_SPACE(sizeof(int)*AW_IN_SOCKET_BUFFER)];
  struct cmsghdr *cmsg;
  int rv, i, num;

  msg.msg_name = 0;
  msg.msg_namelen = 0;
  msg.msg_iov = vec;
  msg.msg_iovlen = count;
  msg.msg_control = ccmsg;
  msg.msg_controllen = CMSG_SPACE(sizeof(int) * AW_IN_SOCKET_BUFFER);

  rv = recvmsg(c->sd, &msg, 0);

  if (rv == -1)
    return -1;

  if ((msg.msg_flags & MSG_TRUNC) || (msg.msg_flags & MSG_CTRUNC))
    fatal("readv_to_unix_socket: not enough CMSG space");

  for (cmsg = CMSG_FIRSTHDR(&msg); cmsg != NULL; cmsg = CMSG_NXTHDR(&msg, cmsg)) {
    if (!cmsg->cmsg_type == SCM_RIGHTS)
      fatal("readv_from_unix_socket: Unknown control message %d", cmsg->cmsg_type);

    num = (cmsg->cmsg_len - CMSG_LEN(0))/sizeof(int);

    for(i=0; i<num; i++) {
      if (c->num_in_sd_buffer >= AW_IN_SOCKET_BUFFER)
        fatal("readv_from_unix_socket: Not enough room in in_sd_buffer");

      c->in_sd_buffer[c->num_in_sd_buffer] = ((int*)CMSG_DATA(cmsg))[i];
      c->num_in_sd_buffer++;
    }
  }

  return rv;
}






// ip and port are only used when sending inet listener conns
void aw_send_conn(struct conn *c, struct conn *to, char *ip, int port) {
  struct ioblock *b;

  if (to->num_out_sd_buffer >= AW_OUT_SOCKET_BUFFER) {
    if (to->num_out_sd_buffer >= AW_OUT_SOCKET_BUFFER) {
      c->conntype = AW_CONNTYPE_ZOMBIE;
      if (c->sd != -1) {
        aw_event_remove_all(c);
        my_close(c->sd);
        c->sd = -1;
      }
      aw_touch_conn(c, 0);
      return;
    }
  }

  prealloc_ioblock();
  b = free_ioblocks;
  free_ioblocks = free_ioblocks->next;
  b->next = NULL;

  if (c->conntype == AW_CONNTYPE_HTTP)
    b->len = snprintf(b->data, AW_IOBLOCK_SIZE-1, "http %" PRId64 " %s\n", c->inlen, c->ip);
  else if (c->conntype == AW_CONNTYPE_UNIX)
    b->len = snprintf(b->data, AW_IOBLOCK_SIZE-1, "supervise %" PRId64 "\n", c->inlen);
  else if (c->conntype == AW_CONNTYPE_INETLISTENER)
    b->len = snprintf(b->data, AW_IOBLOCK_SIZE-1, "add-listener %s %d\n", ip, port);
  else
    fatal("aw_send_conn: unknown conntype (%d)", c->conntype);

  b->offset = 0;

  if (to->out == NULL) {
    to->out = to->outp = b;
  } else {
    to->outp->next = b;
    to->outp = b;
  }

  to->outlen += b->len;

  if (c->in) {
    to->outp->next = c->in;
    to->outp = c->inp;
    to->outlen += c->inlen;
  }

  to->out_sd_buffer[to->num_out_sd_buffer] = c->sd;
  to->num_out_sd_buffer++;

  aw_event_remove_all(c);

  c->sd = -1;
  c->in = c->inp = NULL; // Now on to's output queue
  c->inlen = 0;
  c->conntype = AW_CONNTYPE_ZOMBIE;
  aw_touch_conn(c, 0);

  aw_event_update(to);

}





// This will only ever be called when sep/limit is NOT satisfied
// because otherwise we would process the data first.
// So if readv() errors or EOFs, we know we have an
// incomplete message in the input queue so it can be discarded.
// If there is no data on the output queue, the connection is
// immediately zombified. If there IS data, the connection then becomes
// a write-out zombie: a zombie with a valid sd AND data on the
// output queue. Once that data is written, or once the writev()
// fails and the sd becomes invalid, the conn will be fully zombified.

static void do_vectored_read_from_sd(struct conn *c) {
  struct iovec v[2];
  ssize_t rv;
  int room=0;
  struct ioblock *startblock;
  int startoffset;
  int originlen;

  prealloc_ioblock();

  originlen = c->inlen;

  again:

  if (c->in == NULL) {
    startblock = free_ioblocks;
    startoffset = 0;
    v[0].iov_base = free_ioblocks->data;
    v[0].iov_len = AW_IOBLOCK_SIZE;
    if (c->conntype == AW_CONNTYPE_UNIX) rv = readv_from_unix_socket(c, v, 1);
    else rv = readv(c->sd, v, 1);
  } else {
    room = AW_IOBLOCK_SIZE - (c->inp->offset + c->inp->len);

    if (room == 0) {
      startblock = free_ioblocks;
      startoffset = 0;
      v[0].iov_base = free_ioblocks->data;
      v[0].iov_len = AW_IOBLOCK_SIZE;
      if (c->conntype == AW_CONNTYPE_UNIX) rv = readv_from_unix_socket(c, v, 1);
      else rv = readv(c->sd, v, 1);
    } else {
      startblock = c->inp;
      startoffset = c->inp->offset + c->inp->len;
      v[0].iov_base = c->inp->data + (c->inp->offset + c->inp->len);
      v[0].iov_len = room;
      v[1].iov_base = free_ioblocks->data;
      v[1].iov_len = AW_IOBLOCK_SIZE;
      if (c->conntype == AW_CONNTYPE_UNIX) rv = readv_from_unix_socket(c, v, 2);
      else rv = readv(c->sd, v, 2);
    }
  }

  if (rv == -1 || rv == 0) {
    if (rv == -1 && errno == EINTR) goto again;

    if (rv == -1 && errno == EAGAIN) {
      aw_logf("syslog", "", "do_vectored_read_from_sd: EAGAIN (%d)", c->conntype);
      return;
    }

    // Read failed. Make it a write-out zombie
    c->conntype = AW_CONNTYPE_ZOMBIE;

    if (c->out == NULL) {
      aw_event_remove_all(c);
      my_close(c->sd);
      c->sd = -1;
    } else {
      aw_event_update(c);
    }
    return;
  }

  if (c->in == NULL) {
    c->in = c->inp = free_ioblocks;
    free_ioblocks = free_ioblocks->next;
    c->in->next = NULL;
    c->in->offset = 0;
    c->in->len = rv;
  } else if (room == 0) {
    c->inp->next = free_ioblocks;
    free_ioblocks = free_ioblocks->next;
    c->inp = c->inp->next;
    c->inp->next = NULL;
    c->inp->offset = 0;
    c->inp->len = rv;
  } else if (rv <= room) {
    c->inp->len += rv;
  } else {
    c->inp->len += room;
    c->inp->next = free_ioblocks;
    free_ioblocks = free_ioblocks->next;
    c->inp = c->inp->next;
    c->inp->next = NULL;
    c->inp->offset = 0;
    c->inp->len = rv-room;
  }

  c->inlen += rv;

  if (c->conntype == AW_CONNTYPE_HTTP_LINGER) {
    aw_drop_n_input_bytes(c, rv);
    return;
  }

  // c->ready must be 0 here otherwise we wouldn't have called this function.
  if (c->ready) _exit(-1);

  if (c->sep == NULL) {
    if (c->inlen >= c->limit)
      c->ready = c->limit;
    return;
  } else if (*c->currsep == '\0') {
    fatal("do_vectored_read_from_sd: read before dropping");
  } else {
    int count=0, i;

    for (i=startoffset; (startblock->data + i) < (startblock->data + startblock->offset + startblock->len); i++) {
      if (startblock->data[startblock->offset + i] == *c->currsep) c->currsep++;
      else c->currsep = c->sep;

      count++;

      if (*c->currsep == '\0') {
        c->ready = count+originlen;
        if (c->ready >= c->limit) {
          c->ready = 0;
          c->conntype = AW_CONNTYPE_ZOMBIE;
          aw_event_update(c);
        }
        return;
      }
    }

    startblock = startblock->next;

    while (startblock != NULL) {
      for (i=0; i<(startblock->len); i++) {
        if (startblock->data[startblock->offset + i] == *c->currsep) c->currsep++;
        else c->currsep = c->sep;

        count++;

        if (*c->currsep == '\0') {
          c->ready = count+originlen;
          if (c->ready >= c->limit) {
            c->ready = 0;
            c->conntype = AW_CONNTYPE_ZOMBIE;
            aw_event_update(c);
          }
          return;
        }

      }

      startblock = startblock->next;
    }

    if (c->inlen >= c->limit) {
      c->conntype = AW_CONNTYPE_ZOMBIE;
      aw_event_update(c);
      return;
    }
  }

  // c->ready must be 0 here otherwise we would have returned
  if (c->ready) _exit(-1);

}


static void readv_into_ioblocks_and_add_to_conn(struct ioblock *b, int blocks, off_t bytes_to_read, struct conn *c) {
  struct iovec v[blocks];
  struct ioblock *tp=b, *lastblock=NULL;
  off_t i=0, bytes_left;

  if (b == NULL || blocks <= 0 || blocks > IOV_MAX || bytes_to_read <= 0)
    fatal("readv_into_ioblocks_and_add_to_conn: bad args");

  bytes_left = bytes_to_read;

  while (tp) {
    v[i].iov_base = tp->data;

    if (bytes_left > AW_IOBLOCK_SIZE) {
      v[i].iov_len = tp->len = AW_IOBLOCK_SIZE;
      bytes_left -= AW_IOBLOCK_SIZE;
    } else {
      v[i].iov_len = tp->len = bytes_left;
      bytes_left = 0;
    }

    if (tp->next == NULL) lastblock = tp;

    i++;
    tp = tp->next;
  }

  if (bytes_left) fatal("readv_into_ioblocks_and_add_to_conn: bytes left over");
  if (i != blocks) fatal("readv_into_ioblocks_and_add_to_conn: caller lied about number of blocks");

  again:

  i = readv(c->fd, v, blocks);

  if (i == -1) {
    if (errno == EINTR) goto again;
    fatal("readv_into_ioblocks_and_add_to_conn: readv: %s", strerror(errno));
  }

  if (i != bytes_to_read) fatal("readv_into_ioblocks_and_add_to_conn: readv returned %d not %d", i, bytes_to_read);

  if (c->out == NULL) {
    c->out = b;
    c->outp = lastblock;
  } else {
    c->outp->next = b;
    c->outp = lastblock;
  }

  c->outlen += bytes_to_read;
  c->fd_bytes_left -= bytes_to_read;

}


static void large_file_read_ahead(struct conn *c) {
  off_t bytes_to_transfer_to_out_chain, bytes_to_readv=0;
  int blocks=0;
  struct ioblock *b=NULL, *tpb;

  if (c->fd_bytes_left <= (off_t)AW_LARGE_FILE_BUFFER_SIZE-(c->outlen))
    bytes_to_transfer_to_out_chain = c->fd_bytes_left;
  else
    bytes_to_transfer_to_out_chain = (off_t)AW_LARGE_FILE_BUFFER_SIZE-(c->outlen);

  if (bytes_to_transfer_to_out_chain <= 0)
    fatal("large_file_read_ahead: don't call when buffer already full");

  while (bytes_to_transfer_to_out_chain > 0) {
    blocks++;
    prealloc_ioblock();
    tpb = free_ioblocks;
    free_ioblocks = free_ioblocks->next;
    tpb->next = b;
    b = tpb;

    if (bytes_to_transfer_to_out_chain < (off_t) AW_IOBLOCK_SIZE) {
      bytes_to_readv += bytes_to_transfer_to_out_chain;
      bytes_to_transfer_to_out_chain = 0;
    } else {
      bytes_to_readv += AW_IOBLOCK_SIZE;
      bytes_to_transfer_to_out_chain -= AW_IOBLOCK_SIZE;
    }

    if (blocks == IOV_MAX) {
      readv_into_ioblocks_and_add_to_conn(b, IOV_MAX, bytes_to_readv, c);
      b = NULL;
      blocks = 0;
      bytes_to_readv = 0;
    }
  }

  if (bytes_to_transfer_to_out_chain < 0)
    fatal("large_file_read_ahead: bytes_to_transfer_to_out_chain went negative");

  if (blocks) {
    if (bytes_to_readv <= 0)
      fatal("large_file_read_ahead: final transfer has blocks (%d) but no bytes", blocks);
    readv_into_ioblocks_and_add_to_conn(b, blocks, bytes_to_readv, c);
  }

  if (c->fd_bytes_left == 0) {
    my_close(c->fd);
    c->fd = -1;

    c->conntype = AW_CONNTYPE_HTTP;
    aw_event_update(c);

    aw_update_conn_ready_status(c);
    if (c->ready)
      add_to_artificial_ready_conns(c);
  }
}



static void do_vectored_write_to_sd(struct conn *c) {
  struct iovec v[AW_MAX_WRITEV_SLOTS];
  ssize_t rv;
  int n=0;
  struct ioblock *b;

  if (c->fd != -1) {
    if (c->mmap_curr == NULL && c->outlen < (AW_LARGE_FILE_BUFFER_SIZE/2))
      large_file_read_ahead(c);
  }

  if (!c->out && c->mmap_curr == NULL)
    fatal("do_vectored_write_to_sd: tried to write from empty buffer (conntype=%d; fd=%d; fd_bytes_left=%" PRId64 ")", c->conntype, c->fd, c->fd_bytes_left);

  b = c->out;

  while(b) {
    v[n].iov_base = b->data + b->offset;
    v[n].iov_len = b->len;

    n++;

    if (n == AW_MAX_WRITEV_SLOTS) break;

    b = b->next;
  }

  if (c->mmap_curr != NULL && n <= AW_MAX_WRITEV_SLOTS-1) {
    v[n].iov_base = c->mmap_curr;
    v[n].iov_len = c->fd_bytes_left;

    n++;
  }

  again:

  if (c->num_out_sd_buffer && c->conntype == AW_CONNTYPE_UNIX) rv = writev_to_unix_socket(c, v, n);
  else rv = writev(c->sd, v, n);

  if (rv == 0 || rv == -1) {
    if (rv == -1 && errno == EINTR) goto again;

    if (rv == -1 && errno == EAGAIN) {
      aw_logf("syslog", "", "do_vectored_write_to_sd: EAGAIN (%d)", c->conntype);
      return;
    }

    c->conntype = AW_CONNTYPE_ZOMBIE;
    if (c->sd != -1) {
      aw_event_remove_all(c);
      my_close(c->sd);
      c->sd = -1;
    }
    return;
  }

  if (c->outlen) {
    c->outlen -= rv;
    if (c->outlen < 0) { // We must have written some of an mmaped file.
      if (c->mmap_curr == NULL)
        fatal("do_vectored_write_to_sd: wrote more than our buffer and no mmaped file");
      c->outlen = 0;
    }
  }

  while(c->out != NULL && rv > 0) {
    if (c->out->len <= rv) {
      rv -= c->out->len;
      b = c->out;
      c->out = c->out->next;
      unalloc_ioblock(b);
      if (c->out == NULL) c->outp = NULL;
    } else {
      c->out->offset += rv;
      c->out->len -= rv;
      rv = 0;
    }
  }

  if (c->mmap_curr) {
    // rv now holds the amount of mmaped file data that was sent
    c->fd_bytes_left -= rv;
    c->mmap_curr += rv;
  }

  // An HTTP connection times-out keepalive_time_in_seconds seconds after the last time we sent data to it:
  if (c->conntype == AW_CONNTYPE_HTTP ||
      c->conntype == AW_CONNTYPE_HTTP_SEND_FILE ||
      c->conntype == AW_CONNTYPE_PROXY_SINK)
    aw_touch_conn(c, keepalive_time_in_seconds);

}



void aw_update_conn_ready_status(struct conn *c) {
  struct ioblock *b;

  c->currsep = c->sep;

  if (c->sep == NULL && c->inlen >= c->limit) {
    c->ready = c->limit;
  } else if (c->sep == NULL) {
    c->ready = 0;
  } else {
    int count=0, i;

    b = c->in;
    while (b != NULL) {
      for (i=0; i<(b->len); i++) {
        if (b->data[b->offset + i] == *c->currsep) c->currsep++;
        else c->currsep = c->sep;

        count++;

        if (*c->currsep == '\0') {
          c->ready = count;
          return;
        }
      }

      b = b->next;
    }

    c->ready = 0;
  }

}



void aw_drop_n_input_bytes(struct conn *c, int n) {
  struct ioblock *b;

  if (n < 1 || n > c->inlen)
    fatal("aw_drop_n_input_bytes: bad value for n: %d", n);

  again:

  if (c->in->len > n) {
    c->in->offset += n;
    c->in->len -= n;
    c->inlen -= n;
    n = 0;
  } else {
    c->inlen -= c->in->len;
    n -= c->in->len;
    b = c->in;
    c->in = c->in->next;
    if (c->in == NULL) c->inp = NULL;
    unalloc_ioblock(b);
  }

  if (n > 0) goto again;

  aw_update_conn_ready_status(c);

}



// If you have a stat for this file handy, pass it as user_stat otherwise fstat() will be called after open().

void aw_send_file_to_http_conn(struct conn *c, char *file, struct stat *user_stat, off_t offset) {
  struct stat local_stat;
  struct stat *s;

  if (c->fd != -1)
    fatal("aw_send_file_to_http_conn: send already in progress");

  if (user_stat && user_stat->st_size == 0) return; // don't bother open()ing empty files

  open_again:

  c->fd = open(file, O_RDONLY);

  if (c->fd == -1) {
    if (errno == EINTR) goto open_again;
    fatal("aw_send_file_to_http_conn: Unable to open file '%s' (%s)", file, strerror(errno));
  }

  if (user_stat != NULL) {
    s = user_stat;
  } else {
    s = &local_stat;
    if (fstat(c->fd, s) == -1)
      fatal("aw_send_file_to_http_conn: fstat failed on '%s' (%s)", file, strerror(errno));
  }

  c->fd_bytes_left = s->st_size;

  if (c->fd_bytes_left == 0) {
    my_close(c->fd);
    c->fd = -1;
    return;
  }

  if (c->fd_bytes_left <= AW_IOBLOCK_SIZE) {
    // For small files, don't bother with mmap(). Just read it into the conn's
    // output buffer and close the file. This is so that for a series
    // of pipelined small files we can send them all (along with their http
    // headers) to the kernel with a single writev(). Also, mmap()+munmap()
    // usually means more overhead than a read() for a small file.

    struct ioblock *b;
    int rv;

    prealloc_ioblock();
    b = free_ioblocks;
    free_ioblocks = free_ioblocks->next;
    b->next = NULL;
    b->offset = 0;

    c->fd_bytes_left -= offset;
    if (lseek(c->fd, offset, SEEK_SET) != offset)
      fatal("aw_send_file_to_http_conn: lseek small: %s", strerror(errno));

    while (c->fd_bytes_left > 0) {
      read_again:
      rv = read(c->fd, b->data + b->len, c->fd_bytes_left);
      if (rv == -1 || rv == 0) {
        if (rv == -1 && rv == EINTR) goto read_again;
        fatal("aw_send_file_to_http_conn: read() '%s' (%s)", file, strerror(errno));
      }

      c->fd_bytes_left -= rv;
      b->len += rv;
    }

    my_close(c->fd);
    c->fd = -1;

    if (c->out == NULL) {
      c->out = c->outp = b;
    } else {
      c->outp->next = b;
      c->outp = b;
    }

    c->outlen += b->len;

  } else if (c->fd_bytes_left <= AW_MAX_FILE_SIZE_TO_MMAP) {
    // mmap() these files to eliminate copying data into userspace iff
    // AW_IOBLOCK_SIZE < the file length <= AW_MAX_FILE_SIZE_TO_MMAP.

    c->mmap_start = c->mmap_curr = mmap(NULL, c->fd_bytes_left, PROT_READ, MAP_SHARED, c->fd, 0);

    if (c->mmap_start == MAP_FAILED)
      fatal("aw_send_file_to_http_conn: mmap: '%s' %s", file, strerror(errno));
    if (c->mmap_start == NULL)
      fatal("aw_send_file_to_http_conn: mmap: '%s' returned NULL", file);

    c->mmap_curr += offset;
    c->mmap_bytes = c->fd_bytes_left - offset;

    c->conntype = AW_CONNTYPE_HTTP_SEND_FILE;
    c->ready = 0; // don't handle more requests until we've finished sending the mmaped file

  } else {
    // We don't mmap() "large" (larger than AW_MAX_FILE_SIZE_TO_MMAP bytes) files for two reasons:
    //   1) To avoid running out of address space on 32 bit machines when serving
    //      many files at once (or a small number of super-large files).
    //   2) So we can use a user-space buffer to reduce disk thrashing 
    //      when concurrently sending multiple large files (idea shamelessly stolen from lighttpd):
    //      http://blog.lighttpd.net/articles/2005/11/11/optimizing-lighty-for-high-concurrent-large-file-downloads

    c->fd_bytes_left -= offset;
    if (lseek(c->fd, offset, SEEK_SET) != offset)
      fatal("aw_send_file_to_http_conn: lseek large: %s", strerror(errno));

    if (c->outlen < (AW_LARGE_FILE_BUFFER_SIZE/2))
      large_file_read_ahead(c);
  }

  aw_event_update(c);

}







struct conn *aw_listen_unix(char *path) {
  struct conn *c;
  int sd;
  struct sockaddr_un address;
  size_t address_len;

  c = alloc_conn();
  c->conntype = AW_CONNTYPE_UNIXLISTENER;

  sd = socket(AF_UNIX, SOCK_STREAM, 0);
  if (sd < 0)
    fatal("aw_listen_unix: socket(): %s", strerror(errno));

  unlink(path);

  address.sun_family = AF_UNIX;
  strcpy(address.sun_path, path);
  address_len = sizeof(address.sun_family) + strlen(path) + 1;

  if (bind(sd, (struct sockaddr *) &address, address_len) != 0)
    fatal("aw_listen_unix: bind(): %s", strerror(errno));

  if (listen(sd, BACKLOG) != 0)
    fatal("aw_listen_unix: listen(): %s", strerror(errno));

  c->sd = sd;

  c->next = conns_in_use;
  conns_in_use = c;

  aw_event_update(c);

  make_socket_close_on_exec(c->sd);

  return c;
}



struct conn *aw_start_timer(int seconds) {
  struct conn *c;

  c = alloc_conn();
  c->conntype = AW_CONNTYPE_TIMER;
  c->ready = 0;
  aw_touch_conn(c, seconds);

  c->next = conns_in_use;
  conns_in_use = c;

  aw_event_update(c);

  return c;
}


struct conn *aw_conn_unix(char *path) {
  struct conn *c;
  int sd;
  struct sockaddr_un address;
  size_t address_len;

  c = alloc_conn();
  c->conntype = AW_CONNTYPE_UNIX;
  c->sep = c->currsep = sep_single_newline;
  c->limit = AW_MAX_UNIX_MSG_LEN;
  c->expiry = 0;

  sd = socket(PF_UNIX, SOCK_STREAM, 0);
  if (sd < 0)
    fatal("aw_conn_unix: socket(): %s", strerror(errno));

  address.sun_family = AF_UNIX;
  strcpy(address.sun_path, path);
  address_len = sizeof(address.sun_family) + strlen(path) + 1;

  if(connect(sd, (struct sockaddr *) &address, address_len) != 0)
    fatal("aw_conn_unix: connect(%s): %s", path, strerror(errno));

  c->sd = sd;

  c->next = conns_in_use;
  conns_in_use = c;

  aw_event_update(c);

  make_socket_close_on_exec(c->sd);

  return c;
}



struct conn *aw_listen_inet(char *bind_addr, int port) {
  struct conn *c;
  int tp=1,sd=-1,rv;
  int ip_ver=0;

  if (strchr(bind_addr, ':')) ip_ver = 6;
  else if (strchr(bind_addr, '.')) ip_ver = 4;
  else fatal("aw_listen_inet: Bad bind_addr: %s", bind_addr);

  c = alloc_conn();
  c->conntype = AW_CONNTYPE_INETLISTENER;

  if (ip_ver == 4) {
    struct sockaddr_in my_addr;

    if ((sd = socket(AF_INET, SOCK_STREAM, 0)) == -1)
      fatal("aw_listen_inet(%s, %d): socket(): %s", bind_addr, port, strerror(errno));

    setsockopt(sd, SOL_SOCKET, SO_REUSEADDR, &tp, sizeof(tp));

    memset(&my_addr, 0, sizeof(my_addr));
    my_addr.sin_family = AF_INET;
    my_addr.sin_port = htons(port);

    rv = inet_pton(AF_INET, bind_addr, &my_addr.sin_addr);
    if (rv <= 0) fatal("aw_listen_inet: Bad bind_addr: %s", bind_addr);

    if (bind(sd, (struct sockaddr *)&my_addr, sizeof(struct sockaddr)) == -1)
      fatal("aw_listen_inet(%s, %d): bind(): %s", bind_addr, port, strerror(errno));

    if (listen(sd, BACKLOG) == -1)
      fatal("aw_listen_inet(%s, %d): listen(): %s", bind_addr, port, strerror(errno));

  } else if (ip_ver == 6) {
    struct sockaddr_in6 my_addr;

    if ((sd = socket(AF_INET6, SOCK_STREAM, 0)) == -1)
      fatal("aw_listen_inet(%s, %d): socket(): %s", bind_addr, port, strerror(errno));

    setsockopt(sd, SOL_SOCKET, SO_REUSEADDR, &tp, sizeof(tp));

    memset(&my_addr, 0, sizeof(my_addr));
    my_addr.sin6_family = AF_INET6;
    my_addr.sin6_port = htons(port);

    rv = inet_pton(AF_INET6, bind_addr, &my_addr.sin6_addr);
    if (rv <= 0) fatal("aw_listen_inet: Bad bind_addr: %s", bind_addr);

    if (bind(sd, (struct sockaddr *)&my_addr, sizeof(my_addr)) == -1)
      fatal("aw_listen_inet(%s, %d): bind(): %s", bind_addr, port, strerror(errno));

    if (listen(sd, BACKLOG) == -1)
      fatal("aw_listen_inet(%s, %d): listen(): %s", bind_addr, port, strerror(errno));

  }

  c->sd = sd;

  c->next = conns_in_use;
  conns_in_use = c;

  aw_event_update(c);

  make_socket_close_on_exec(c->sd);

  return c;
}





// Will only return NULL on INET sockets. In other words, may return NULL iff both of these are true:
//   1) from->conntype == AW_CONNTYPE_INETLISTENER
//   2) accept_conntype == AW_CONNTYPE_HTTP

struct conn *aw_accept_conn(struct conn *from, int pre_read, int accept_conntype) {
  struct conn *c;

  if (pre_read < 0 || pre_read > (1<<15))
    fatal("aw_accept_conn: bad value for pre_read: %d", pre_read);

  if (accept_conntype != AW_CONNTYPE_HTTP &&
      accept_conntype != AW_CONNTYPE_UNIX &&
      accept_conntype != AW_CONNTYPE_INETLISTENER)
    fatal("aw_accept_conn: unknown conntype (%d)", accept_conntype);

  c = alloc_conn();

  if (accept_conntype == AW_CONNTYPE_HTTP) {
    c->conntype = AW_CONNTYPE_HTTP;
    c->sep = c->currsep = sep_http;
    c->limit = AW_MAX_HTTP_HEADER;
  } else if (accept_conntype == AW_CONNTYPE_UNIX) {
    c->conntype = AW_CONNTYPE_UNIX;
    c->sep = c->currsep = sep_single_newline;
    c->limit = AW_MAX_UNIX_MSG_LEN;
  } else if (accept_conntype == AW_CONNTYPE_INETLISTENER) {
    c->conntype = AW_CONNTYPE_INETLISTENER;
  }

  if (from->conntype == AW_CONNTYPE_INETLISTENER) {
    struct sockaddr_in6 their_addr;
    socklen_t tp = sizeof(their_addr);

    if (accept_conntype != AW_CONNTYPE_HTTP)
      fatal("aw_accept_conn: can only accept http connections from an inet listener");

    again:

    c->sd = accept(from->sd, (struct sockaddr *)&their_addr, &tp);

    if (c->sd == -1) {
      if (errno == EINTR) goto again;

      if (errno == ECONNABORTED || errno == EAGAIN || errno == EWOULDBLOCK || errno == EMFILE) {
        c->next = conns_in_use;
        conns_in_use = c;
        aw_unalloc_conn(c);
        return NULL;
      }

      fatal("aw_accept_conn: accept(): %s", strerror(errno));
      // IMPORTANT if above fatal is removed you must put the conn in conns_in_use before calling aw_unalloc_conn
    }

    if (tp == sizeof(struct sockaddr_in6)) {
      if (inet_ntop(AF_INET6, &their_addr.sin6_addr, c->ip, INET6_ADDRSTRLEN) == NULL)
        fatal("aw_accept_conn: inet_ntop: 6 %s", strerror(errno));
    } else if (tp == sizeof(struct sockaddr_in)) {
      if (inet_ntop(AF_INET, &(((struct sockaddr_in *)&their_addr)->sin_addr), c->ip, INET6_ADDRSTRLEN) == NULL)
        fatal("aw_accept_conn: inet_ntop: 4 %s", strerror(errno));
    } else {
      c->ip[0] = '\0';
    }

  } else if (from->conntype == AW_CONNTYPE_UNIX) {
    int i;

    if (from->num_in_sd_buffer == 0) {
      fatal("aw_accept_conn: No sockets received from unix socket");
    }

    c->sd = from->in_sd_buffer[0];

    for (i=0; i<from->num_in_sd_buffer-1; i++)
      from->in_sd_buffer[i] = from->in_sd_buffer[i+1];

    from->num_in_sd_buffer--;

    if (pre_read) {
      from->limit = pre_read;
      from->ready = 0;
      from->sep = from->currsep = NULL;

      while (from->inlen < pre_read)
        do_vectored_read_from_sd(from);

      while (pre_read) {
        if (pre_read < from->in->len) {
          struct ioblock *b;

          prealloc_ioblock();
          memcpy(free_ioblocks->data, from->in->data + from->in->offset, pre_read);
          free_ioblocks->offset = 0;
          free_ioblocks->len = pre_read;

          b = free_ioblocks;
          free_ioblocks = free_ioblocks->next;

          b->next = NULL;

          if (c->in == NULL) {
            c->in = c->inp = b;
          } else {
            c->inp->next = b;
            c->inp = b;
          }

          c->inlen += pre_read;
          from->in->offset += pre_read;
          from->in->len -= pre_read;
          from->inlen -= pre_read;
          pre_read = 0;
        } else {
          struct ioblock *b;

          b = from->in;
          from->in = from->in->next;
          if (from->in == NULL) from->inp = NULL;
          from->inlen -= b->len;

          b->next = NULL;

          if (c->in == NULL) {
            c->in = c->inp = b;
          } else {
            c->inp->next = b;
            c->inp = b;
          }

          c->inlen += b->len;
          pre_read -= b->len;
        }
      }

      aw_update_conn_ready_status(c);

      if (c->ready)
        add_to_artificial_ready_conns(c);

      from->limit = AW_MAX_UNIX_MSG_LEN;
      from->sep = from->currsep = sep_single_newline;

    }

  } else fatal("aw_accept_conn: Unknown listener type: %d", from->conntype);

  make_socket_non_blocking(c->sd);
  make_socket_close_on_exec(c->sd);

  c->next = from->next;
  from->next = c;

  aw_update_conn_ready_status(from);

  if (accept_conntype == AW_CONNTYPE_HTTP)
    aw_touch_conn(c, keepalive_time_in_seconds);

  aw_event_update(c);

  return c;
}



struct conn *aw_accept_unix_conn(struct conn *from) {
  struct conn *c;
  struct sockaddr_un address;
  socklen_t address_len=sizeof(struct sockaddr_un);

  c = alloc_conn();
  c->conntype = AW_CONNTYPE_UNIX;
  c->sep = c->currsep = sep_single_newline;
  c->limit = AW_MAX_UNIX_MSG_LEN;
  c->ip[0] = '\0';

  if (from->conntype == AW_CONNTYPE_UNIXLISTENER) {
    again:
    c->sd = accept(from->sd, (struct sockaddr *) &address, &address_len);

    if (c->sd == -1) {
      if (errno == EINTR) goto again;
      fatal("aw_accept_unix_conn(): accept(): %s", strerror(errno));
      // IMPORTANT if above fatal is removed you must put the conn in conns_in_use before calling aw_unalloc_conn
    }
  } else {
    fatal("aw_accept_unix_conn(): tried to accept from a %d", from->conntype);
  }

  c->next = from->next;
  from->next = c;

  from->ready = 0;

  aw_event_update(c);
  return c;
}



void transfer_proxy_data(struct conn *from, struct conn *to) {
  struct ioblock *b;
  struct ioblock *tpb;

  if (to->conntype == AW_CONNTYPE_ZOMBIE || from->conntype == AW_CONNTYPE_ZOMBIE) {
    if (to->sd != -1 && to->outlen == 0) {
      aw_event_remove_all(to);
      my_close(to->sd);
      to->sd = -1;
    }
    if (from->sd != -1) {
      aw_event_remove_all(from);
      my_close(from->sd);
      from->sd = -1;
    }
    to->conntype = AW_CONNTYPE_ZOMBIE;
    from->conntype = AW_CONNTYPE_ZOMBIE;
    aw_touch_conn(to, 0);
    aw_touch_conn(from, 0);
    aw_event_update(to);
    aw_event_update(from);
    return;
  }

  if (to->conntype != AW_CONNTYPE_PROXY_SINK)
    fatal("transfer_proxy_data: must transfer to proxy sink (was %d)", to->conntype);

  if (from->conntype != AW_CONNTYPE_PROXY_SOURCE)
    fatal("transfer_proxy_data: must transfer from proxy source (was %d)", from->conntype);

  if (to->proxy_mate != from || from->proxy_mate != to)
    fatal("transfer_proxy_data: connections aren't mated");

  while(from->inlen > 0 && to->proxy_len > 0) {
    b = from->in;

    if (b->len <= to->proxy_len) {
      from->in = from->in->next;
      if (from->in == NULL) from->inp = NULL;
      from->inlen -= b->len;

      if (to->out == NULL) {
        to->out = to->outp = b;
      } else {
        to->outp->next = b;
        to->outp = b;
      }
      b->next = NULL;
      to->outlen += b->len;

      to->proxy_len -= b->len;
    } else {
      prealloc_ioblock();
      tpb = free_ioblocks;
      free_ioblocks = free_ioblocks->next;

      memcpy(tpb->data, b->data + b->offset, to->proxy_len);
      tpb->len = to->proxy_len;

      b->len -= to->proxy_len;
      b->offset += to->proxy_len;
      from->inlen -= to->proxy_len;

      if (to->out == NULL) {
        to->out = to->outp = tpb;
      } else {
        to->outp->next = tpb;
        to->outp = tpb;
      }
      tpb->next = NULL;
      to->outlen += tpb->len;

      to->proxy_len = 0;
    }
  }

  aw_event_update(to);

  if (to->proxy_len == 0) { // HOOK for when we really do proxying: let the lisp closure decide what to do next
    from->ready = 1;
    add_to_artificial_ready_conns(from);
  }
}




void aw_event_update(struct conn *c) {
  int aw_evs=0;

  if (c->expiry) {
    if (next_timeout_scan == 0 || c->expiry < next_timeout_scan)
      next_timeout_scan = c->expiry;
  }

  if (c->sd == -1) return;

  switch(c->conntype) {
    case AW_CONNTYPE_HTTP:
    case AW_CONNTYPE_UNIX:
      aw_evs = AW_READ;
      if (c->outlen)
        aw_evs |= AW_WRITE;
      break;

    case AW_CONNTYPE_INETLISTENER:
    case AW_CONNTYPE_UNIXLISTENER:
    case AW_CONNTYPE_PROXY_SOURCE:
      aw_evs = AW_READ;
      break;

    case AW_CONNTYPE_ZOMBIE:
      if (c->outlen && c->sd != -1)
        aw_evs = AW_WRITE;
      break;

    case AW_CONNTYPE_HTTP_SEND_FILE:
      aw_evs = AW_WRITE;
      break;

    case AW_CONNTYPE_PROXY_SINK:
      if (c->outlen && c->sd != -1)
        aw_evs = AW_WRITE;
      break;

    case AW_CONNTYPE_HTTP_LINGER:
      aw_evs = AW_READ;
      if (c->out || c->fd_bytes_left) aw_evs |= AW_WRITE;
      break;

    case AW_CONNTYPE_TIMER:
    case AW_CONNTYPE_PROXY_IDLE:
      break;

    default:
      fatal("aw_event_update: unknown conntype (%d)", c->conntype);
  }

  if (aw_evs == c->kernel_state)
    return;

  #ifdef USE_EPOLL

  if (aw_evs == 0) {
    aw_event_remove_all(c);
    return;
  }

  {
    struct epoll_event e;

    e.events = 0;

    if (aw_evs & AW_READ)
      e.events = EPOLLIN;
    if (aw_evs & AW_WRITE)
      e.events |= EPOLLOUT;

    e.data.ptr = c;

    if (c->kernel_state) {
      if (epoll_ctl(event_desc, EPOLL_CTL_MOD, c->sd, &e) == -1)
        fatal("aw_event_update: epoll_ctl MOD: %s", strerror(errno));
    } else {
      if (epoll_ctl(event_desc, EPOLL_CTL_ADD, c->sd, &e) == -1)
        fatal("aw_event_update: epoll_ctl ADD: %s", strerror(errno));
    }
  }
  #endif

  #ifdef USE_KQUEUE

  {
    struct kevent e[4]; // will only use 2
    int curr=0;

    if ((aw_evs & AW_READ) && !(c->kernel_state & AW_READ)) {
      EV_SET(&e[curr], c->sd, EVFILT_READ, EV_ADD, 0, 0, c);
      curr++;
    } else if (!(aw_evs & AW_READ) && (c->kernel_state & AW_READ)) {
      EV_SET(&e[curr], c->sd, EVFILT_READ, EV_DELETE, 0, 0, NULL);
      curr++;
    }

    if ((aw_evs & AW_WRITE) && !(c->kernel_state & AW_WRITE)) {
      EV_SET(&e[curr], c->sd, EVFILT_WRITE, EV_ADD, 0, 0, c);
      curr++;
    } else if (!(aw_evs & AW_WRITE) && (c->kernel_state & AW_WRITE)) {
      EV_SET(&e[curr], c->sd, EVFILT_WRITE, EV_DELETE, 0, 0, NULL);
      curr++;
    }

    kq_again:

    if (kevent(event_desc, &e[0], curr, NULL, 0, NULL) == -1) {
      if (errno == EINTR) goto kq_again;
      fatal("aw_event_update: kevent: %s", strerror(errno));
    }

  }
  #endif

  c->kernel_state = aw_evs;
}


void aw_event_remove_all(struct conn *c) {
  if (c->kernel_state == 0)
    return;

  #ifdef USE_EPOLL
  if (epoll_ctl(event_desc, EPOLL_CTL_DEL, c->sd, NULL) == -1) {
    fatal("aw_event_remove_all: epoll_ctl: %s", strerror(errno));
  }
  #endif

  #ifdef USE_KQUEUE
  {
    struct kevent e[2];
    int curr=0;

    if (c->kernel_state & AW_READ) {
      EV_SET(&e[curr], c->sd, EVFILT_READ, EV_DELETE, 0, 0, NULL);
      curr++;
    }

    if (c->kernel_state & AW_WRITE) {
      EV_SET(&e[curr], c->sd, EVFILT_WRITE, EV_DELETE, 0, 0, NULL);
      curr++;
    }

    kq_again:

    if (kevent(event_desc, &e[0], curr, NULL, 0, NULL) == -1) {
      if (errno == EINTR) goto kq_again;
      fatal("aw_event_remove_all: kevent: %s", strerror(errno));
    }

  }
  #endif

  c->kernel_state = 0;
}


static void aw_process_event(struct conn *c, int aw_evs) {
  if (c->sd == -1)
    return; // Must have been closed since we got the event

  switch(c->conntype) {
    case AW_CONNTYPE_HTTP:
    case AW_CONNTYPE_UNIX:
      if (aw_evs & AW_READ) {
        do_vectored_read_from_sd(c);
      }
      if (aw_evs & AW_WRITE) {
        do_vectored_write_to_sd(c);
        if (c->out == NULL) aw_event_update(c);
      }
      break;

      case AW_CONNTYPE_INETLISTENER:
      case AW_CONNTYPE_UNIXLISTENER:
        if (aw_evs & AW_READ)
          c->ready = 1;
        break;

      case AW_CONNTYPE_ZOMBIE:
        if (c->outlen && c->sd != -1) {
          if (aw_evs & AW_WRITE) {
            do_vectored_write_to_sd(c);
            if (c->out == NULL) aw_event_update(c);
          } else {
            aw_event_remove_all(c);
            my_close(c->sd);
            c->sd = -1;
            aw_touch_conn(c, 0);
          }
        }
        break;

      case AW_CONNTYPE_HTTP_LINGER:
        if (aw_evs & AW_WRITE) {
          if (c->out || c->fd_bytes_left)
            do_vectored_write_to_sd(c);
          if (c->out == NULL && c->fd_bytes_left == 0) {
            aw_touch_conn(c, AW_HTTP_LINGER_SECONDS);
            aw_event_update(c);
            if (shutdown(c->sd, SHUT_WR) == -1) {
              if (errno == ENOTCONN || errno == ECONNRESET) {
                aw_event_remove_all(c);
                my_close(c->sd);
                c->sd = -1;
                c->conntype = AW_CONNTYPE_ZOMBIE;
                aw_touch_conn(c, 0);
                return;
              }
              else fatal("aw_process_event: shutdown: %s", strerror(errno));
            }
          }
        }
        if (aw_evs & AW_READ)
          do_vectored_read_from_sd(c);
        break;

      case AW_CONNTYPE_HTTP_SEND_FILE:
        if (c->fd == -1)
          fatal("aw_process_event: tried to send from file without fd");

        if (aw_evs & AW_WRITE)
          do_vectored_write_to_sd(c);

        if (c->fd_bytes_left == 0) {
          if (c->mmap_start != NULL) {
            if (munmap(c->mmap_start, c->mmap_bytes))
              fatal("aw_process_event: munmap (%s)", strerror(errno));

            c->mmap_start = c->mmap_curr = NULL;
            c->mmap_bytes = 0;
          }

          my_close(c->fd);
          c->fd = -1;

          c->conntype = AW_CONNTYPE_HTTP;
          aw_event_update(c);

          aw_update_conn_ready_status(c);
          if (c->ready)
            add_to_artificial_ready_conns(c);
        }
        break;

      case AW_CONNTYPE_PROXY_SOURCE:
        if (aw_evs & AW_READ) {
          do_vectored_read_from_sd(c);
          transfer_proxy_data(c, c->proxy_mate);
        }
        break;

      case AW_CONNTYPE_PROXY_SINK:
        if (aw_evs & AW_WRITE) {
          do_vectored_write_to_sd(c);
          aw_event_update(c);
        }
        break;

    default:
      fatal("aw_process_event: unknown conntype (%d)", c->conntype);
  }
}



struct conn *aw_get_event() {
  struct conn *tpc;
  int i, aw_evs;

  restart:

  tpc = get_next_artificial_ready_conn();

  if (tpc) {
    if (tpc->ready)
      return tpc;
    else
      goto restart;
  }

  if (events_left) {
    events_left--;

    #ifdef USE_EPOLL
    return events[events_left].data.ptr;
    #endif

    #ifdef USE_KQUEUE
    return events[events_left].udata;
    #endif
  }

  while (next_timeout_ptr) {
    if (next_timeout_ptr->expiry && next_timeout_ptr->expiry <= recentish_time) {
      if (next_timeout_ptr->conntype == AW_CONNTYPE_TIMER) {
        tpc = next_timeout_ptr;
        next_timeout_ptr = next_timeout_ptr->next;
        tpc->ready = 1;
        return tpc;
      } else {
        tpc = next_timeout_ptr;
        next_timeout_ptr = next_timeout_ptr->next;
        tpc->conntype = AW_CONNTYPE_ZOMBIE;
        if (tpc->sd != -1) {
          aw_event_remove_all(tpc);
          my_close(tpc->sd);
          tpc->sd = -1;
        }
        aw_event_update(tpc);
        return tpc;
      }
    } else if (next_timeout_ptr->expiry) {
      if (next_timeout_scan == 0 || next_timeout_ptr->expiry < next_timeout_scan)
        next_timeout_scan = next_timeout_ptr->expiry;
    }

    next_timeout_ptr = next_timeout_ptr->next;
  }

  again:

  while (time_for_a_reaping) {
    int rv;
    reap_again:
    rv = wait3(NULL,WNOHANG,NULL);
    if (rv == -1) {
      if (errno == EINTR) goto reap_again;
      else if (errno == ECHILD) time_for_a_reaping=0;
      else fatal("aw_get_event: wait3: %s", strerror(errno));
    }
    if (rv == 0) time_for_a_reaping=0;
  }

  if (num_conns_in_use == 0) _exit(-1); // Hub: Lost main unix socket listener  Worker: Lost the hub unix socket

  if (next_timeout_scan && recentish_time >= next_timeout_scan) {
    next_timeout_scan = recentish_time;
    goto skip_event_syscall;
  }

  #ifdef USE_EPOLL
  events_left = epoll_wait(event_desc, events, AW_EVENT_BATCH_SIZE,
                           next_timeout_scan ? ((next_timeout_scan-recentish_time)*1000) : -1);
  if (events_left==-1) {
    if (errno == EINTR) {
      events_left = 0;
      time(&recentish_time);
      goto again;
    }
    fatal("aw_get_event: epoll: %s", strerror(errno));
  }
  #endif

  #ifdef USE_KQUEUE
  {
    struct timespec ts;

    if (next_timeout_scan) {
      ts.tv_sec = next_timeout_scan-recentish_time;
      ts.tv_nsec = 0;
    }

    events_left = kevent(event_desc, NULL, 0, events, AW_EVENT_BATCH_SIZE, next_timeout_scan ? &ts : NULL);
    if (events_left==-1) {
      if (errno == EINTR) {
        events_left = 0;
        time(&recentish_time);
        goto again;
      }
      fatal("aw_get_event: kevent: %s", strerror(errno));
    }

  }
  #endif

  time(&recentish_time);

  skip_event_syscall:

  if (next_timeout_scan && recentish_time >= next_timeout_scan) {
    next_timeout_scan = 0;
    next_timeout_ptr = conns_in_use;
  }

  for (i=0; i<events_left; i++) {
    aw_evs = 0;

    #ifdef USE_EPOLL
    if (events[i].events & EPOLLIN) aw_evs = AW_READ;
    if (events[i].events & EPOLLOUT) aw_evs |= AW_WRITE;
    aw_process_event(events[i].data.ptr, aw_evs);
    #endif

    #ifdef USE_KQUEUE
    if (events[i].filter == EVFILT_READ) aw_evs = AW_READ;
    else if (events[i].filter == EVFILT_WRITE) aw_evs = AW_WRITE;
    else fatal("aw_get_event: unknown event %d  flags=%d", events[i].filter, events[i].flags);
    aw_process_event(events[i].udata, aw_evs);
    #endif
  }

  goto restart;
}



void aw_send_dir_listings(struct conn *c, char *dirpath) {
  char buf[AW_IOBLOCK_SIZE];
  struct ioblock *b;
  DIR *dirp;
  struct dirent *dp;
  size_t space_left, len;
  int needs_slash;

  if (c->outp == NULL) fatal("aw_send_dir_listings: called with empty output buffer"); // must put http headers on first

  dirp = opendir(dirpath);
  if (dirp == NULL)
    fatal("aw_send_dir_listings: opendir(%s) (%s)", dirpath, strerror(errno));

  prealloc_ioblock();
  b = free_ioblocks;
  free_ioblocks = free_ioblocks->next;
  b->len = b->offset = 0;
  b->next = NULL;
  space_left = AW_IOBLOCK_SIZE;

  while ((dp = readdir(dirp)) != NULL) {

    if (strcmp(".", dp->d_name) == 0) continue;

    needs_slash = (dp->d_type == DT_DIR);

    len = snprintf(buf, sizeof(buf), "<a href=\"%s%s\">%s%s</a><br>",
                   dp->d_name, needs_slash ? "/" : "",
                   dp->d_name, needs_slash ? "/" : "");

    if (len >= sizeof(buf)) continue;

    if (len > space_left) {
      c->outp->next = b;
      c->outp = b;
      c->outlen += b->len;

      prealloc_ioblock();
      b = free_ioblocks;
      free_ioblocks = free_ioblocks->next;
      b->next = NULL;
      b->len = b->offset = 0;
      space_left = AW_IOBLOCK_SIZE; 
    }

    memcpy(b->data + b->len, buf, len); // b->offset is always 0 in this function
    b->len += len;
    space_left -= len;

  }

  if (b->len) {
    c->outp->next = b;
    c->outp = b;
    b->next = NULL;
    c->outlen += b->len;
  } else {
    b->next = free_ioblocks;
    free_ioblocks = b;
  }

  if (closedir(dirp) == -1)
    fatal("aw_send_dir_listings: closedir: (%s)", strerror(errno));

  aw_event_update(c);

}





struct conn *aw_build_cgi_conn(struct conn *c, char *path, char *pathinfo, char *urlargs, char *method, char *cookie, char *content_type, off_t postlen, off_t bufsize, off_t buftrig, int single_process_cgis_only, int maxfiles, int naked) {
  struct conn *cgi_conn;
  int tpipe[2], fv;
  char *myargs[3];

  if (pipe(tpipe) != 0)
    fatal("aw_build_cgi_conn: pipe: %s", strerror(errno));

  fv = fork();

  if (fv == -1)
    fatal("aw_build_cgi_conn: fork: %s", strerror(errno));

  if (fv == 0) { // Child
    char buf[1024];

    #ifdef USE_EPOLL
    if (close(event_desc) == -1)
      _exit(1);
    #endif
    // kqueues never make it across fork()
    event_desc = -1;

    make_socket_blocking(c->sd);

    if (!naked) {
      snprintf(buf, sizeof(buf), "HTTP/1.1 200 OK\r\nServer: Antiweb/%s\r\nConnection: close\r\n", AW_VERSION);
      if (write(c->sd, buf, strlen(buf)) != (int) strlen(buf))
        _exit(1); // Can't log from the CGI process
    }

    if (dup2(tpipe[0], 0) == -1)
      _exit(1);

    if (close(tpipe[0]) == -1)
      _exit(1);

    if (close(tpipe[1]) == -1)
      _exit(1);

    if (dup2(c->sd, 1) == -1)
      _exit(1);

    if (aw_get_nofile() > maxfiles)
      aw_set_nofile(maxfiles);

    if (single_process_cgis_only)
      aw_set_nproc(1);

    setenv("REMOTE_ADDR", c->ip, 1);
    if (*pathinfo == '\0') setenv("PATH_INFO", "/", 1);
    else setenv("PATH_INFO", pathinfo, 1);
    setenv("QUERY_STRING", urlargs, 1);
    setenv("REQUEST_METHOD", method, 1);
    setenv("HTTP_COOKIE", cookie, 1);
    setenv("CONTENT_TYPE", content_type, 1);
    snprintf(buf, sizeof(buf), "%" PRId64, postlen);
    setenv("CONTENT_LENGTH", buf, 1);

    myargs[0] = path;
    myargs[1] = urlargs;
    myargs[2] = NULL;

    execv(path, myargs);
    _exit(1); // exec failed
  }

  // Parent

  my_close(tpipe[0]);

  c->conntype = AW_CONNTYPE_PROXY_SOURCE;
  aw_touch_conn(c, -1); // Let it time out at the sink
  c->ready = 0;
  c->limit = postlen;
  c->currsep = c->sep = NULL;

  cgi_conn = alloc_conn();

  cgi_conn->conntype = AW_CONNTYPE_PROXY_SINK;
  aw_touch_conn(cgi_conn, keepalive_time_in_seconds);

  cgi_conn->proxy_mate = c;
  c->proxy_mate = cgi_conn;
  cgi_conn->sd = tpipe[1];

  cgi_conn->proxy_len = postlen;
  cgi_conn->proxy_bufsize = bufsize;
  cgi_conn->proxy_buftrig = buftrig;

  cgi_conn->next = conns_in_use;
  conns_in_use = cgi_conn;

  if (postlen == 0) cgi_conn->conntype = AW_CONNTYPE_ZOMBIE;

  transfer_proxy_data(c, cgi_conn);

  aw_event_update(c);
  aw_event_update(cgi_conn);

  return cgi_conn;
}




struct stat aw_static_stat_struct;

void *aw_stat_returning_a_static_struct(char *path) {
  int rv;

  if (*path != '/')
    fatal("aw_stat_returning_a_static_struct: path must be absolute, not %s", path);

  if (strlen(path) > PATH_MAX)
    return NULL;

  rv = stat(path, &aw_static_stat_struct);

  if (rv) return NULL;
  return &aw_static_stat_struct;
}

void *aw_lstat_returning_a_static_struct(char *path) {
  int rv;

  if (*path != '/')
    fatal("aw_lstat_returning_a_static_struct: path must be absolute, not %s", path);

  if (strlen(path) > PATH_MAX)
    return NULL;

  rv = lstat(path, &aw_static_stat_struct);

  if (rv) return NULL;
  return &aw_static_stat_struct;
}

int aw_stat_is_dir(struct stat *stat) {
  return S_ISDIR(stat->st_mode);
}

int aw_stat_is_reg_file(struct stat *stat) {
  return S_ISREG(stat->st_mode);
}

int aw_stat_is_world_readable(struct stat *stat) {
  return stat->st_mode & S_IROTH;
}

int aw_stat_is_world_executable(struct stat *stat) {
  return stat->st_mode & S_IXOTH;
}

int aw_stat_is_sym_link(struct stat *stat) {
  return S_ISLNK(stat->st_mode);
}

off_t aw_stat_get_file_size(struct stat *stat) {
  return stat->st_size;
}

off_t aw_stat_get_inode(struct stat *stat) {
  return (off_t) stat->st_ino;
}

time_t aw_stat_get_mtime(struct stat *stat) {
  return stat->st_mtime;
}

int aw_stat_get_uid(struct stat *stat) {
  return (int) stat->st_uid;
}

int aw_stat_get_gid(struct stat *stat) {
  return (int) stat->st_gid;
}

void aw_chmod(char *path, int mode) {
  if (chmod(path, mode) == -1)
    fatal("aw_chmod: couldn't set mode on %s to %o (%s)", path, mode, strerror(errno));
}




static void nada() {
}

void reaper() {
  time_for_a_reaping=1;
}


void aw_init() {
  time(&recentish_time);

  signal(SIGPIPE, nada);
  signal(SIGHUP, nada);
  signal(SIGCHLD, reaper);

  #ifdef USE_EPOLL
  event_desc = epoll_create(100);
  #endif

  #ifdef USE_KQUEUE
  event_desc = kqueue();
  #endif
}



void aw_dropto_uid_gid(int id) {
  if (id == 0)
    fatal("aw_dropto_uid_gid: can't drop to root");
  if (setgid(id) == -1)
    fatal("aw_dropto_uid_gid: setgid: %s", strerror(errno));
  if (setuid(id) == -1)
    fatal("aw_dropto_uid_gid: setuid: %s", strerror(errno));
}


int aw_lookup_user_name_with_getpwnam(char *name) {
  struct passwd *p;

  p = getpwnam(name);

  if (p == NULL)
    fatal("aw_lookup_user_name_with_getpwnam: couldn't lookup username %s", name);

  if (p->pw_uid == 0)
    fatal("aw_lookup_user_name_with_getpwnam: won't run with UID 0 of user %s", name);

  if (p->pw_uid != p->pw_gid)
    fatal("aw_lookup_user_name_with_getpwnam: user %s has different UID from GID", name);

  return (int) p->pw_uid;
}


void aw_chroot(char *path) {
  if (chroot(path) == -1)
    fatal("aw_chroot: %s", strerror(errno));
  if (chdir("/") == -1)
    fatal("aw_chroot: chdir: %s", strerror(errno));
}


void aw_set_nofile(int n) {
  struct rlimit rl;

  rl.rlim_cur = rl.rlim_max = n;

  if (setrlimit(RLIMIT_NOFILE, &rl))
    fatal("aw_set_nofile: %s", strerror(errno));
}

int aw_get_nofile() {
  struct rlimit rl;

  if (getrlimit(RLIMIT_NOFILE, &rl))
    fatal("aw_get_nofile: %s", strerror(errno));

  return rl.rlim_max;
}

void aw_set_nproc(int n) {
  struct rlimit rl;

  rl.rlim_cur = rl.rlim_max = n;

  if (setrlimit(RLIMIT_NPROC, &rl))
    fatal("aw_set_nproc: %s", strerror(errno));
}


void aw_daemonise_fork() {
  int rv;

  rv = fork();
  if (rv == -1)
    fatal("aw_daemonise: fork: %s", strerror(errno));
  if (rv) _exit(0);
}

void aw_daemonise_drop_terminal() {
  if (setsid() == -1)
    fatal("aw_daemonise: setsid: %s", strerror(errno));

  umask(0);

  if (freopen("/dev/null", "r", stdin) == NULL ||
      freopen("/dev/null", "w", stdout) == NULL ||
      freopen("/dev/null", "w", stderr) == NULL)
    fatal("aw_daemonise: freopen: %s", strerror(errno));

  if (chdir("/") == -1)
    fatal("aw_daemonise: chdir: %s", strerror(errno));
}

void *aw_open_log_file(char *filename) {
  FILE *fp = fopen(filename, "a");
  if (fp == NULL) _exit(-1);
  return (void *) fp;
}

void aw_close_log_file(void *v_fp) {
  if (fclose((FILE *) v_fp)) _exit(-1);
}

void aw_write_log_message(void *v_fp, char *msg) {
  size_t i,len;
  FILE *fp = (FILE *) v_fp;

  if (fp == NULL) _exit(-1);

  len = strlen(msg);

  for(i=0; i<len; i++)
    if ((msg[i] & 0xff) < 32 || (msg[i] & 0xff) > 127) msg[i] = '?';

  if (fprintf(fp, "%ld %s\n", (long) recentish_time, msg) == -1)
    _exit(-1);

  fflush(fp);
}




// def was adapted from http://www.zlib.net/zpipe.c

// zpipe.c: example of proper use of zlib's inflate() and deflate()
// Not copyrighted -- provided to the public domain
// Version 1.4  11 December 2005  Mark Adler

// FIXME: asserts should be fatal()s so we see log msgs (though I have never seen this routine fail)

#include <assert.h>
#include <zlib.h>

#define CHUNK 16384

static int def(FILE *source, FILE *dest, int level) {
    int ret, flush;
    unsigned have;
    z_stream strm;
    unsigned char in[CHUNK];
    unsigned char out[CHUNK];

    /* allocate deflate state */
    strm.zalloc = Z_NULL;
    strm.zfree = Z_NULL;
    strm.opaque = Z_NULL;
    ret = deflateInit2(&strm, level, Z_DEFLATED, 15+16, 8, Z_DEFAULT_STRATEGY);
    if (ret != Z_OK)
        return ret;

    /* compress until end of file */
    do {
        strm.avail_in = fread(in, 1, CHUNK, source);
        if (ferror(source)) {
            (void)deflateEnd(&strm);
            return Z_ERRNO;
        }
        flush = feof(source) ? Z_FINISH : Z_NO_FLUSH;
        strm.next_in = in;

        /* run deflate() on input until output buffer not full, finish
           compression if all of source has been read in */
        do {
            strm.avail_out = CHUNK;
            strm.next_out = out;
            ret = deflate(&strm, flush);    /* no bad return value */
            assert(ret != Z_STREAM_ERROR);  /* state not clobbered */
            have = CHUNK - strm.avail_out;
            if (fwrite(out, 1, have, dest) != have || ferror(dest)) {
                (void)deflateEnd(&strm);
                return Z_ERRNO;
            }
        } while (strm.avail_out == 0);
        assert(strm.avail_in == 0);     /* all input will be used */

        /* done when last data in file processed */
    } while (flush != Z_FINISH);
    assert(ret == Z_STREAM_END);        /* stream will be complete */

    /* clean up and return */
    (void)deflateEnd(&strm);
    return Z_OK;
}


// Note: if there is no trailing slash, it will assume the last component is a file
// that should live in the created directoy. "/path/to/file" only creates "/path/to/"
// but "/path/to/dir/" creates "/path/to/dir/". Don't pass it read-only strings.

void aw_mkdir_dash_p(char *path) {
  char *tp, *nextslash;

  if (*path != '/') fatal("aw_mkdir_dash_p: needs absolute path, not %s", path);
  tp=path+1;
  while ((nextslash = strchr(tp, '/'))) {
    *nextslash = '\0';
    if (mkdir(path, 0755) == -1) {
      if (errno != EEXIST) fatal("aw_mkdir_dash_p: couldn't mkdir(%s) (%s)", path, strerror(errno));
    }
    *nextslash = '/';
    tp = nextslash+1;
  }
}


void aw_gzip_file(char *src, char *dst) {
  FILE *srcf;
  FILE *dstf;

  srcf = fopen(src, "r");
  if (srcf == NULL) fatal("aw_gzip_file: Couldn't read from %s (%s)", src, strerror(errno));

  dstf = fopen(dst, "w");
  if (dstf == NULL) fatal("aw_gzip_file: Couldn't write to %s (%s)", dst, strerror(errno));

  if (def(srcf, dstf, 9) != Z_OK)
    fatal("aw_gzip_file: deflation failed");

  if (fclose(srcf)) fatal("aw_gzip_file: fclosing src %s (%s)", src, strerror(errno));
  if (fclose(dstf)) fatal("aw_gzip_file: fclosing dst %s (%s)", dst, strerror(errno));
}



typedef struct {
    unsigned long state[5];
    unsigned long count[2];
    unsigned char buffer[64];
} SHA1_CTX;

void SHA1Transform(unsigned long state[5], unsigned char buffer[64]);
void SHA1Init(SHA1_CTX* context);
void SHA1Update(SHA1_CTX* context, unsigned char* data, unsigned int len);
void SHA1Final(unsigned char digest[20], SHA1_CTX* context);

void aw_sha1(char *input, int len, char *output, int iters) {
  SHA1_CTX ctx;
  int i,j;
  char tp[20];

  if (len < 0) fatal("aw_sha1: bad len value %d", len);
  if (iters < 1) fatal("aw_sha1: bad iters value %d", iters);

  SHA1Init(&ctx);
  SHA1Update(&ctx, (unsigned char *) input, (unsigned int) len);
  SHA1Final((unsigned char *) output, &ctx);

  for (i=1; i<iters; i++) {
    for (j=0; j<20; j++) tp[j] = output[j];

    SHA1Init(&ctx);
    SHA1Update(&ctx, (unsigned char *) tp, (unsigned int) 20);
    SHA1Final((unsigned char *) output, &ctx);
  }
}




#ifdef USE_BDB
#include <db.h>

DB_ENV *berkeleydb_env = NULL;

static void aw_bdb_ensure_bdb_has_been_inited() {
  if (berkeleydb_env == NULL)
    fatal("BerkeleyDB environment hasn't been initialised");
}
#endif

void aw_bdb_init_environment(char *dir) {
#ifndef USE_BDB
  fatal("BerkeleyDB support not compiled");
#else
  int ret;

  db_version(&ret, NULL, NULL);
  if (ret != 4) fatal("aw_bdb_init_environment: BerkeleyDB version 4 is required");

  ret = db_env_create(&berkeleydb_env, 0);
  if (ret) fatal("aw_init_db: db_env_create: %s", db_strerror(ret));

  ret = berkeleydb_env->set_lk_detect(berkeleydb_env, DB_LOCK_DEFAULT);
  if (ret) fatal("aw_init_db: env->set_lk_detect: %s", db_strerror(ret));

  ret = berkeleydb_env->open(berkeleydb_env, dir, DB_CREATE|DB_INIT_TXN|DB_INIT_LOCK|DB_INIT_LOG|DB_INIT_MPOOL, 0);
  if (ret) fatal("aw_init_db: env->open: %s", db_strerror(ret));
#endif
}


void *aw_bdb_open(char *filename) {
#ifndef USE_BDB
  fatal("BerkeleyDB support not compiled");
  return NULL;
#else
  int ret;
  DB *mydb = NULL;

  aw_bdb_ensure_bdb_has_been_inited();

  ret = db_create(&mydb, berkeleydb_env, 0);
  if (ret) fatal("aw_bdb_open: db_create: %s (%s)", filename, db_strerror(ret));

  ret = mydb->open(mydb, NULL, filename, NULL, DB_BTREE, DB_CREATE | DB_AUTO_COMMIT, 0600);
  if (ret) fatal("aw_bdb_open: unable to open %s (%s)", filename, db_strerror(ret));

  return mydb;
#endif
}


void *aw_bdb_begin_transaction(void *parent) {
#ifndef USE_BDB
  fatal("BerkeleyDB support not compiled");
  return NULL;
#else
  DB_TXN *txn=NULL;
  DB_TXN *parent_txn=(DB_TXN *)parent;
  int ret;

  aw_bdb_ensure_bdb_has_been_inited();

  ret = berkeleydb_env->txn_begin(berkeleydb_env, parent_txn, &txn, 0);
  if (ret) fatal("aw_bdb_begin_transaction: txn_begin: %s", db_strerror(ret));

  return txn;
#endif
}


void aw_bdb_commit_transaction(void *v_txn) {
#ifndef USE_BDB
  fatal("BerkeleyDB support not compiled");
#else
  int ret;
  DB_TXN *txn=(DB_TXN *)v_txn;

  aw_bdb_ensure_bdb_has_been_inited();

  ret = txn->commit(txn, 0);
  if (ret) fatal("aw_bdb_commit_transaction: commit: %s", db_strerror(ret));
#endif
}


void aw_bdb_abort_transaction(void *v_txn) {
#ifndef USE_BDB
  fatal("BerkeleyDB support not compiled");
#else
  int ret;
  DB_TXN *txn=(DB_TXN *)v_txn;

  aw_bdb_ensure_bdb_has_been_inited();

  ret = txn->abort(txn);
  if (ret) fatal("aw_bdb_abort_transaction: commit: %s", db_strerror(ret));
#endif
}


void aw_bdb_checkpoint() {
#ifndef USE_BDB
  fatal("BerkeleyDB support not compiled");
#else
  int ret;

  aw_bdb_ensure_bdb_has_been_inited();

  ret = berkeleydb_env->txn_checkpoint(berkeleydb_env, 0, 0, 0);
  if (ret) fatal("aw_bdb_checkpoint: txn_checkpoint: %s", db_strerror(ret));
#endif
}


// returns 1 if item not found
// returns 2 if deadlock detected
int aw_bdb_get(void *v_mydb, void *v_txn, int db_rmw, char *keystr, int keylen, void *v_ptr_datastr, int *datalen) {
#ifndef USE_BDB
  fatal("BerkeleyDB support not compiled");
  return 0;
#else
  DB *mydb = (DB *)v_mydb;
  DB_TXN *txn = (DB_TXN *)v_txn;
  char **datastr = (char **)v_ptr_datastr;
  DBT key, data;
  int ret;

  aw_bdb_ensure_bdb_has_been_inited();

  memset(&key, 0, sizeof(DBT));
  memset(&data, 0, sizeof(DBT));

  key.data = keystr;
  key.size = keylen;

  ret = mydb->get(mydb, txn, &key, &data, db_rmw ? DB_RMW : 0);
  if (ret) {
    if (ret == DB_NOTFOUND) return 1;
    if (ret == DB_LOCK_DEADLOCK) return 2;
    fatal("aw_bdb_get: get: %s", db_strerror(ret));
  }

  *datastr = data.data;
  *datalen = data.size;

  return 0;
#endif
}


// returns 2 if deadlock detected
int aw_bdb_put(void *v_mydb, void *v_txn, char *keystr, int keylen, char *datastr, int datalen) {
#ifndef USE_BDB
  fatal("BerkeleyDB support not compiled");
  return 0;
#else
  DB *mydb = (DB *)v_mydb;
  DB_TXN *txn = (DB_TXN *)v_txn;
  DBT key, data;
  int ret;

  aw_bdb_ensure_bdb_has_been_inited();

  memset(&key, 0, sizeof(DBT));
  memset(&data, 0, sizeof(DBT));

  key.data = keystr;
  key.size = keylen;
  data.data = datastr;
  data.size = datalen;

  ret = mydb->put(mydb, txn, &key, &data, 0);
  if (ret) {
    if (ret == DB_LOCK_DEADLOCK) return 2;
    fatal("aw_bdb_put: put: (%s)", db_strerror(ret));
  }

  return 0;
#endif
}

// returns 1 if item not found
// returns 2 if deadlock detected
int aw_bdb_del(void *v_mydb, void *v_txn, char *keystr, int keylen) {
#ifndef USE_BDB
  fatal("BerkeleyDB support not compiled");
  return 0;
#else
  DB *mydb = (DB *)v_mydb;
  DB_TXN *txn = (DB_TXN *)v_txn;
  DBT key;
  int ret;

  aw_bdb_ensure_bdb_has_been_inited();

  memset(&key, 0, sizeof(DBT));

  key.data = keystr;
  key.size = keylen;

  ret = mydb->del(mydb, txn, &key, 0);
  if (ret) {
    if (ret == DB_NOTFOUND) return 1;
    if (ret == DB_LOCK_DEADLOCK) return 2;
    fatal("aw_bdb_del: del: %s", db_strerror(ret));
  }

  return 0;
#endif
}


char *aw_bdb_version() {
#ifndef USE_BDB
  fatal("BerkeleyDB support not compiled");
  return NULL;
#else
  return db_version(NULL, NULL, NULL);
#endif
}


void *aw_bdb_open_cursor(void *v_mydb, void *v_txn) {
#ifndef USE_BDB
  fatal("BerkeleyDB support not compiled");
  return NULL;
#else
  DB *mydb = (DB *)v_mydb;
  DB_TXN *txn = (DB_TXN *)v_txn;
  DBC *mydbc = NULL;
  int ret;

  aw_bdb_ensure_bdb_has_been_inited();

  ret = mydb->cursor(mydb, txn, &mydbc, 0);
  if (ret) fatal("aw_bdb_open_cursor: cursor: %s", db_strerror(ret));

  return mydbc;
#endif
}


void aw_bdb_close_cursor(void *v_mydbc) {
#ifndef USE_BDB
  fatal("BerkeleyDB support not compiled");
#else
  DBC *mydbc = (DBC *)v_mydbc;
  int ret;

  aw_bdb_ensure_bdb_has_been_inited();

  ret = mydbc->close(mydbc);
  if (ret) fatal("aw_bdb_close_cursor: close: %s", db_strerror(ret));
#endif
}


// returns 1 if at end of list
// returns 2 if deadlock detected
int aw_bdb_cursor_next(void *v_mydbc, void *v_ptr_keystr, int *keylen, void *v_ptr_datastr, int *datalen) {
#ifndef USE_BDB
  fatal("BerkeleyDB support not compiled");
  return 0;
#else
  DBC *mydbc = (DBC *)v_mydbc;
  char **keystr = (char **)v_ptr_keystr;
  char **datastr = (char **)v_ptr_datastr;
  DBT key, data;
  int ret;

  aw_bdb_ensure_bdb_has_been_inited();

  memset(&key, 0, sizeof(DBT));
  memset(&data, 0, sizeof(DBT));

  ret = mydbc->get(mydbc, &key, &data, DB_NEXT);
  if (ret) {
    if (ret == DB_NOTFOUND) return 1;
    if (ret == DB_LOCK_DEADLOCK) return 2;
    fatal("aw_bdb_cursor_next: get: %s", db_strerror(ret));
  }

  *keystr = key.data;
  *keylen = key.size;

  *datastr = data.data;
  *datalen = data.size;

  return 0;
#endif
}
