#include <fcntl.h>
#include <poll.h>
#include <errno.h>
#include <sys/wait.h>

#ifndef O_CLOEXEC
/* There is no such constant on RHES 5 and earlier */
#define O_CLOEXEC 0x80000
#endif

int SPAWN_O_NONBLOCK = O_NONBLOCK;
int SPAWN_O_CLOEXEC = O_CLOEXEC;
int SPAWN_O_RDWR = O_RDWR;
int SPAWN_O_NOCTTY = O_NOCTTY;
int SPAWN_WNOHANG = WNOHANG;
int SPAWN_FD_CLOEXEC = FD_CLOEXEC;
int SPAWN_F_SETFD = F_SETFD;
int SPAWN_F_SETFL = F_SETFL;
int SPAWN_EINTR = EINTR;
int SPAWN_EAGAIN = EAGAIN;
unsigned short SPAWN_POLLIN = POLLIN;
unsigned short SPAWN_POLLOUT = POLLOUT;

extern int SPAWN_NONBLOCK;
extern int SPAWN_CLOEXEC;
extern int SPAWN_WNOHANG;
extern int SPAWN_F_SETFL;
extern int SPAWN_EINTR;
extern int SPAWN_EAGAIN;
extern unsigned short SPAWN_POLLIN;
extern unsigned short SPAWN_POLLOUT;

/* Macros */

extern int __spawn_WIFEXITED(int status)
{
    return (int)WIFEXITED(status);
}

extern unsigned __spawn_WEXITSTATUS(int status)
{
    return WEXITSTATUS(status);
}

extern int __spawn_WIFSIGNALED(int status)
{
    return (int)WIFSIGNALED(status);
}

extern unsigned __spawn_WTERMSIG(int status)
{
    return WTERMSIG(status);
}
