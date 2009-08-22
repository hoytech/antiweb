// Antiweb (C) Doug Hoyte

// *** THIS FILE IS NOT A REGULAR C HEADER FILE ***
// It follows a very strict format for parsing by both the C compiler
// and a special lisp parser that lives in build.lisp.

#define AW_VERSION "4.0BETA14"

#define AW_MAX_MSG_LENGTH 100000
#define AW_IOBLOCK_SIZE 4096 // ideally page size
#define AW_MAX_UNIX_MSG_LEN 2048 // can be > AW_IOBLOCK_SIZE if needed, but must be below AW_MAX_MSG_LENGTH
#define AW_MAX_HTTP_HEADER 4096 // ditto
#define AW_MAX_CGI_POST_LEN 100000000 // returns 413 to client if post body is larger than this (in bytes)

// connection types stored in conntype slot of the conn struct:
#define AW_CONNTYPE_ZOMBIE -1
#define AW_CONNTYPE_HTTP 1
#define AW_CONNTYPE_HTTP_LINGER 2
#define AW_CONNTYPE_HTTP_SEND_FILE 3
#define AW_CONNTYPE_UNIX 4
#define AW_CONNTYPE_INETLISTENER 5
#define AW_CONNTYPE_UNIXLISTENER 6
#define AW_CONNTYPE_PROXY_SOURCE 7
#define AW_CONNTYPE_PROXY_SINK 8
#define AW_CONNTYPE_PROXY_IDLE 9
#define AW_CONNTYPE_TIMER 10

#define INET6_ADDRSTRLEN 46

struct ioblock {
  int offset;
  int len;
  char *data;
  struct ioblock *next;
};


struct conn {
  int sd; // -1=NA
  char *ip; // Must always point to buffer of size (1+ INET6_ADDRSTRLEN)

  int fd; // -1=NA
  off_t fd_bytes_left;
  char *mmap_start; // NULL if not using mmap
  char *mmap_curr; // ditto
  size_t mmap_bytes; // number of mmaped bytes

  off_t inlen; // total len in input ioblock chain
  struct ioblock *in; // first in chain, NULL if empty
  struct ioblock *inp; // last in chain, NULL if empty

  off_t outlen; // ditto. output chain. must keep kernel state in sync
  struct ioblock *out;
  struct ioblock *outp;

  int *in_sd_buffer; // socket descriptor buffers. only for unix sockets
  int *out_sd_buffer;
  int num_in_sd_buffer;
  int num_out_sd_buffer;

  int conntype; // connection type. see #defines above
  int kernel_state; // state the kernel has registered
  time_t expiry; // absolute seconds from time(). 0 means never
  char *sep; // message separator (ie "\r\n\r\n" or "\n") or NULL for fixed size messages
  char *currsep; // where are we so far, points to '\0' if sep found
  int limit; // if sep==NULL, limit is desired message length. otherwise, limit is ceiling where conn is zombied
  int ready; // if non-zero, how many bytes are ready

  struct conn *proxy_mate; // proxy connections are always paired
  off_t proxy_len; // bytes left to proxy. stored in the proxy sink
  off_t proxy_bufsize; // not used atm
  off_t proxy_buftrig; // ditto

  struct conn *next;
};


void aw_fatal(char *reason);
struct conn *aw_listen_unix(char *path);
struct conn *aw_start_timer(int seconds);
struct conn *aw_conn_unix(char *path);
struct conn *aw_listen_inet(char *bind_addr, int port);
struct conn *aw_accept_conn(struct conn *from, int pre_read, int accept_conntype);
struct conn *aw_accept_unix_conn(struct conn *from);
void aw_update_conn_ready_status(struct conn *c);
void prealloc_ioblock();
void aw_drop_n_input_bytes(struct conn *c, int n);
void aw_send_conn(struct conn *c, struct conn *to, char *ip, int port);
void aw_unalloc_conn(struct conn *c);
void aw_touch_conn(struct conn *c, int seconds); // -1 seconds means set to no timeout
void aw_event_update(struct conn *c);
void aw_event_remove_all(struct conn *c);
void remove_from_artificial_ready_conns(struct conn *c);
struct conn *aw_get_event();
void aw_init();
void aw_dropto_uid_gid(int id);
int aw_lookup_user_name_with_getpwnam(char *name);
void aw_chroot(char *path);
void aw_set_nofile(int n);
int aw_get_nofile();
void aw_set_nproc(int n);
void aw_daemonise_fork();
void aw_daemonise_drop_terminal();
void aw_send_file_to_http_conn(struct conn *c, char *file, struct stat *user_stat, off_t offset); // Assumes file exists
void *aw_stat_returning_a_static_struct(char *path);
void *aw_lstat_returning_a_static_struct(char *path);
int aw_stat_is_dir(struct stat *stat);
int aw_stat_is_reg_file(struct stat *stat);
int aw_stat_is_world_readable(struct stat *stat);
int aw_stat_is_world_executable(struct stat *stat);
int aw_stat_is_sym_link(struct stat *stat);
off_t aw_stat_get_file_size(struct stat *stat);
off_t aw_stat_get_inode(struct stat *stat);
time_t aw_stat_get_mtime(struct stat *stat);
int aw_stat_get_uid(struct stat *stat);
int aw_stat_get_gid(struct stat *stat);
void aw_chmod(char *path, int mode);
void aw_send_dir_listings(struct conn *c, char *dirpath);
struct conn *aw_build_cgi_conn(struct conn *c, char *path, char *pathinfo, char *urlargs, char *method, char *cookie, char *content_type, off_t postlen, off_t bufsize, off_t buftrig, int single_process_cgis_only, int maxfiles, int naked);
void aw_log(char *file, char *prefix, char *log_msg);
void aw_mkdir_dash_p(char *path); // see source code
void aw_gzip_file(char *src, char *dst);
int bytes_in_a_ioblock();
int bytes_in_a_conn();
void aw_sha1(char *input, int len, char *output, int iters);

void aw_bdb_init_environment(char *dir);
void *aw_bdb_open(char *filename);
void *aw_bdb_begin_transaction(void *parent);
void aw_bdb_commit_transaction(void *txn);
void aw_bdb_abort_transaction(void *txn);
void aw_bdb_checkpoint();
int aw_bdb_get(void *v_mydb, void *v_txn, int db_rmw, char *keystr, int keylen, void *v_ptr_datastr, int *datalen);
int aw_bdb_put(void *v_mydb, void *v_txn, char *keystr, int keylen, char *datastr, int datalen);
int aw_bdb_del(void *v_mydb, void *v_txn, char *keystr, int keylen);
char *aw_bdb_version();
void *aw_bdb_open_cursor(void *v_mydb, void *v_txn);
void aw_bdb_close_cursor(void *v_mydbc);
int aw_bdb_cursor_next(void *v_mydbc, void *v_ptr_keystr, int *keylen, void *v_ptr_datastr, int *datalen);


extern struct conn *free_conns;
extern struct ioblock *free_ioblocks;
extern struct conn *conns_in_use;
extern int num_conns_in_use;
extern int keepalive_time_in_seconds;
extern time_t recentish_time;
extern char *sep_single_newline;
extern char *sep_http;
extern struct conn *hub_conn;
extern struct conn *logger_conn;
