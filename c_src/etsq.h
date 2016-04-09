/*
 * etsq.h
 *
 *  Created on: Mar 21, 2016
 *      Author: Vinod
 */

#ifndef ETSQ_H_
#define ETSQ_H_

#include <iostream>       // std::cin, std::cout
#include <map>          // std::map
#include <queue>          // std::queue
#include "erl_nif.h"

#define enif_make_error(env, error) enif_make_tuple2(env, \
	enif_make_atom(env, "error"), enif_make_atom(env, error))

struct cmp_str
{
   bool operator()(char *a, char *b) const
   {
      return std::strcmp(a, b) < 0;
   }
};

class ErlTerm
{
public:
	ErlNifEnv *term_env;
	ERL_NIF_TERM term;
public:
	ErlTerm(ERL_NIF_TERM erl_nif_term)
	{
		term_env = enif_alloc_env();
		this->term = enif_make_copy(term_env, erl_nif_term);
	}
	ErlTerm(ErlTerm *erl_term)
	{
		term_env = enif_alloc_env();
		this->term = enif_make_copy(term_env, erl_term->term);
	}
	ErlTerm(int value)
	{
		term_env = enif_alloc_env();
		this->term = enif_make_int(term_env, value);
	}
	ErlTerm(const char *error)
	{
		term_env = enif_alloc_env();
		this->term = enif_make_error(term_env, error);
	}
	~ErlTerm()
	{
		enif_free_env(term_env);
		term_env = NULL;
	}
};

typedef std::queue<ErlTerm*> ErlQueue;

class QueueInfo
{
public:
	ErlNifMutex* pmutex;
	ErlQueue queue;
public:
	QueueInfo(char* name)
	{
		pmutex = enif_mutex_create(name);
	}
	~QueueInfo()
	{
		enif_mutex_destroy(pmutex);
	}
};

typedef std::map<char *, QueueInfo*, cmp_str> QInfoMap;
typedef std::pair<char *, QueueInfo*> QInfoMapPair;

// Class to handle Read lock
class ReadLock
{
	ErlNifRWLock *pread_lock;
public:
	ReadLock(ErlNifRWLock *pread_lock)
	{
		this->pread_lock = pread_lock;
		enif_rwlock_rlock(this->pread_lock);
	};
	~ReadLock()
	{
		enif_rwlock_runlock(pread_lock);
	};
};

// Class to handle Write lock
class WriteLock
{
	ErlNifRWLock *pwrite_lock;
public:
	WriteLock(ErlNifRWLock *pwrite_lock)
	{
		this->pwrite_lock = pwrite_lock;
		enif_rwlock_rwlock(this->pwrite_lock);
	};
	~WriteLock()
	{
		enif_rwlock_rwunlock(pwrite_lock);
	};
};

// Class to handle Mutex lock and unlock
class Mutex
{
	ErlNifMutex *pmtx;
public:
	Mutex(ErlNifMutex *pmtx)
	{
		this->pmtx = pmtx;
		enif_mutex_lock(this->pmtx);
	};
	~Mutex()
	{
		enif_mutex_unlock(pmtx);
	};
};

#endif /* ETSQ_H_ */
