/* -*- indent-tabs-mode: nil;c-basic-offset: 4 -*-              
 * ex: ts=4 sw=4 sts=4 et                                      
 *
 * Copyright 2008-2011 Steve Vinoski. All Rights Reserved.      
 * Copyright 2010-2011 Tuncer Ayaz. All Rights Reserved.        
 * Copyright 2012 Joaquim Pedro França Simão. All Rights Reserved.
 * Use of this source code is governed by a BSD-style           
 * license that can be found in the LICENSE file.               
 *
 * Based on original code from yaws                             
 *                                                              
 * Interface to sendfile system call for Yaws                   
 * author: vinoski@ieee.org                                     
 * Created : 09 Nov 2008 by Steve Vinoski <vinoski@ieee.org>    
 *
 * Renamed to sendfile_drv.c and modified: Tuncer Ayaz in May 2010 
 * Renamed to splice_drv.c and modified: Joaquim Pedro in Jan 2012
 */

#define _GNU_SOURCE
#include <errno.h>
#include <stdint.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>

#include "erl_driver.h"
#ifndef ERL_DRV_NIL
#include "erl_driver_compat.h"
#endif

/*
 * previously drv_output(..., int len)
 * since R15B drv_output(..., ErlDrvSizeT len)
 * use int if OTP < R15B
 */
#if ERL_DRV_EXTENDED_MAJOR_VERSION < 2
typedef int ErlDrvSizeT;
#endif

#include "hashtable.h"

#define lshift_index(s, i, shift, t) (((t)((unsigned char*)(s))[i]) << (shift))
#define lshift32(s, i, shift) lshift_index(s, i, shift, uint32_t)
#define lshift64(s, i, shift) lshift_index(s, i, shift, uint64_t)
#define get_int32(s) (lshift32(s,0,24) | lshift32(s,1,16) | lshift32(s,2,8) \
                      | lshift32(s,3,0))
#define get_int64(s) (lshift64(s,0,56) | lshift64(s,1,48) | lshift64(s,2,40) |\
                      lshift64(s,3,32) | lshift64(s,4,24) | lshift64(s,5,16) |\
                      lshift64(s,6,8) | lshift64(s,7,0))

#define put_shift(i, s, idx, shift) (((unsigned char*)(s))[idx] = \
                                     ((unsigned char)((i) >> (shift)) & 0XFF))
#define put_int32(i, s) do { \
    put_shift(i, s, 0, 24); \
    put_shift(i, s, 1, 16); \
    put_shift(i, s, 2,  8); \
    put_shift(i, s, 3,  0); \
    } while(0)
#define put_int64(i, s) do { \
    put_shift(i, s, 0, 56); \
    put_shift(i, s, 1, 48); \
    put_shift(i, s, 2, 40); \
    put_shift(i, s, 3, 32); \
    put_shift(i, s, 4, 24); \
    put_shift(i, s, 5, 16); \
    put_shift(i, s, 6,  8); \
    put_shift(i, s, 7,  0); \
    } while(0)

typedef union {
    void* hashkey;
    ErlDrvEvent ev_data;
#ifdef _LP64
    uint64_t socket_fd;
#else
    uint32_t socket_fd;
#endif
} SocketFd;

typedef struct {
    off_t offset;
    size_t count;
    ssize_t total;
    int file_fd;
} Transfer;

typedef struct hashtable* Transfers;

typedef struct {
    ErlDrvPort port;
    Transfers xfer_table;
} Desc;


static unsigned int fdhash(void* k)
{
    return ((SocketFd*)&k)->socket_fd;
}

static int fdeq(void* k1, void* k2)
{
    return k1 == k2;
}

static ErlDrvData splice_drv_start(ErlDrvPort port, char* buf)
{
    Desc* d = (Desc*)driver_alloc(sizeof(Desc));
    if (d == NULL) return (ErlDrvData) -1;
    d->port = port;
    d->xfer_table = create_hashtable(8192, fdhash, fdeq);
    if (d->xfer_table == NULL) {
        driver_free(d);
        return (ErlDrvData) -1;
    }
    return (ErlDrvData)d;
}

static void splice_drv_stop(ErlDrvData handle)
{
    Desc* d = (Desc*)handle;
    hashtable_destroy(d->xfer_table, 1);
    driver_free(d);
}

typedef union {
    off_t offset;
    size_t size;
    ssize_t count;
    uint64_t bits;
    unsigned char bytes[8];
} U64_t;

typedef union {
    char* buffer;
    struct {
        U64_t    offset;
        U64_t    count;
        uint32_t in_fd;
        uint32_t out_fd;
    }* args;
    struct {
        U64_t         count;
        uint32_t      in_fd;
        uint32_t      out_fd;
        unsigned char success;
        char          errno_string[1];
    }* result;
} Buffer;

static ErlDrvSizeT set_error_buffer(Buffer* b, int socket_fd, int err)
{
    char* s, *t;
    ErlDrvSizeT result_size = sizeof *(b->result);
    memset(b->result, 0, result_size);
    put_int32(socket_fd, &(b->result->in_fd));
    s = erl_errno_id(err);
    if (strcmp(s, "unknown") == 0 && err == EOVERFLOW) {
        s = "EOVERFLOW";
    }
    for (t = b->result->errno_string; *s; s++, t++) {
        *t = tolower(*s);
    }
    *t = '\0';
    return result_size - 1 + t - b->result->errno_string;
}

static ssize_t splice_call(int in_fd, int out_fd, off_t* in_offset, size_t count)
{
#if defined(__linux__)
    int mpipe[2], saved_errno = 0;
    if(pipe(mpipe) < 0) {
        return -1;
    }
    
    off_t cur = *in_offset;
    ssize_t received = splice(in_fd, NULL, mpipe[1], NULL, count, SPLICE_F_MORE | SPLICE_F_MOVE);

   if (received >= 0 && received != count) {            
       if (*in_offset == cur) {                
           *in_offset += received;           
           saved_errno = EAGAIN;
       }            
   }
   else if(received < 0 && errno == EINTR) {
        errno = EAGAIN;
        return -1;
   } else if (received < 0) {
        return -1;
   }
 
   ssize_t sent = splice(mpipe[0], NULL, out_fd, NULL, received, SPLICE_F_MORE | SPLICE_F_MOVE);
    
   if(sent < 0 && sent == EINTR) { /* Should I return EAGAIN from *HERE*? */
        errno = EAGAIN;
        return -1;
   } else if (sent < 0) {
        return -1;
   }

  
    errno = saved_errno;
    return received;
#else
    errno = ENOSYS;
    return -1;
#endif
}

static void splice_drv_output(ErlDrvData handle, char* buf,
                                ErlDrvSizeT buflen)
{
    int socket_fd, out_fd;
    Desc* d = (Desc*)handle;
    Buffer b;
    b.buffer = buf;
    socket_fd = get_int32(&(b.args->in_fd));
    out_fd = get_int32(&(b.args->out_fd));
    if (out_fd < 0) {
        ErlDrvSizeT out_buflen = set_error_buffer(&b, socket_fd, errno);
        driver_output(d->port, buf, out_buflen);
    } else {
        Transfer* xfer;
        SocketFd sfd;
        sfd.socket_fd = socket_fd;
        xfer = (Transfer*)hashtable_search(d->xfer_table, sfd.hashkey);
        if (xfer == NULL) {
            /* Transfer objects are intentionally not freed until the
               driver stops, or if an insertion error occurs below. */
            xfer = (Transfer*)malloc(sizeof(Transfer));
            if (xfer == NULL) {
                ErlDrvSizeT out_buflen = set_error_buffer(&b, socket_fd,
                                                          ENOMEM);
                driver_output(d->port, buf, out_buflen);
                return;
            }
            if (!hashtable_insert(d->xfer_table, sfd.hashkey, xfer)) {
                ErlDrvSizeT out_buflen = set_error_buffer(&b, socket_fd,
                                                          ENOMEM);
                driver_output(d->port, buf, out_buflen);
                free(xfer);
                return;
            }
        }
        xfer->file_fd = out_fd;
        xfer->offset = get_int64(&(b.args->offset.offset));
        xfer->count = get_int64(&(b.args->count.size));
        xfer->total = 0;
#if defined(ERL_DRV_USE) && defined(ERL_DRV_WRITE)
        driver_select(d->port, sfd.ev_data, ERL_DRV_USE|ERL_DRV_WRITE, 1);
#else
        driver_select(d->port, sfd.ev_data, DO_WRITE, 1);
#endif
    }
}


static void splice_drv_ready_output(ErlDrvData handle, ErlDrvEvent ev)
{
    Desc* d = (Desc*)handle;
    ssize_t result;
    off_t cur_offset;
    Transfer* xfer;
    SocketFd* sfd = (SocketFd*)&ev;
    xfer = (Transfer*)hashtable_search(d->xfer_table, sfd->hashkey);
    if (xfer == NULL) {
        /* fatal error, something is very wrong */
        driver_failure_atom(d->port, "socket_fd_not_found");
        return;
    }
    cur_offset = xfer->offset;
    result = splice_call(sfd->socket_fd, xfer->file_fd,
                           &xfer->offset, xfer->count);
    if (result < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
        if (xfer->offset != cur_offset) {
            off_t written = xfer->offset - cur_offset;
            xfer->count -= written;
            xfer->total += written;
        }
    } else {
        int save_errno = errno;
        ErlDrvSizeT out_buflen;
        char buf[36]; /* TODO: wtf is this? */
        Buffer b;
        b.buffer = buf;
        memset(buf, 0, sizeof buf);
#ifdef ERL_DRV_WRITE
        driver_select(d->port, ev, ERL_DRV_WRITE, 0);
#else
        driver_select(d->port, ev, DO_WRITE, 0);
#endif
        if (result < 0) {
            out_buflen = set_error_buffer(&b, sfd->socket_fd, save_errno);
        } else {
            uint64_t total = xfer->total + result;
            put_int64(total, &(b.result->count.count));
            put_int32(sfd->socket_fd, &(b.result->in_fd));
            b.result->success = 1;
            b.result->errno_string[0] = '\0';
            out_buflen = sizeof(*b.result);
        }
        xfer->offset = xfer->count = xfer->total = 0;
        driver_output(d->port, buf, out_buflen);
    }
}

static ErlDrvEntry splice_driver_entry = {
    NULL,
    splice_drv_start,
    splice_drv_stop,
    splice_drv_output,
    NULL,
    splice_drv_ready_output,
    "splice_drv",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,
    NULL,
    NULL,
    NULL
};

DRIVER_INIT(splice_drv)
{
    return &splice_driver_entry;
}
